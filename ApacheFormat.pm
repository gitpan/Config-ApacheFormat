package Config::ApacheFormat;
use 5.006001;
use strict;
use warnings;
our $VERSION = '1.0';

=head1 NAME

Config::ApacheFormat - use Apache format config files

=head1 SYNOPSIS

Config files used with this module are in Apache's format:

  # comment here
  RootDir /path/foo
  LogDir  /path/foo/log
  Colors red green orange blue \
         black teal

  <Directory /path/foo>
     # override Colors inside block
     Colors red blue black
  </Directory>
  
Code to use this config file might look like:

  use Config::ApacheFormat;

  # load a conf file
  my $config = Config::ApacheFormat->new();
  $config->read("my.conf");

  # access some parameters
  $root_dir = $config->get("RootDir");
  $log_dir  = $config->get("LogDir");
  @colors   = $config->get("colors");

  # using the autoloaded methods
  $config->autoload_support(1);
  $root_dir = $config->RootDir;
  $log_dir  = $config->logdir;

  # access parameters inside a block
  my $block = $config->block(Directory => "/path/foo");
  @colors = $block->get("colors");
  $root_dir = $block->get("root_dir");

=head1 DESCRIPTION

This module is designed to parse a configuration file in the same
syntax used by the Apache web server (see http://httpd.apache.org for
details).  This allows you to build applications which can be easily
managed by experienced Apache admins.  Also, by using this module,
you'll benefit from the support for nested blocks with built-in
parameter inheritence.  This can greatly reduce the amount or repeated
information in your configuration files.

A good reference to the Apache configuration file format can be found
here:

  http://httpd.apache.org/docs-2.0/configuring.html

To quote from that document, concerning directive syntax:

 Apache configuration files contain one directive per line. The
 back-slash "\" may be used as the last character on a line to
 indicate that the directive continues onto the next line. There must
 be no other characters or white space between the back-slash and the
 end of the line.

 Directives in the configuration files are case-insensitive, but
 arguments to directives are often case sensitive. Lines that begin
 with the hash character "#" are considered comments, and are
 ignored. Comments may not be included on a line after a configuration
 directive. Blank lines and white space occurring before a directive
 are ignored, so you may indent directives for clarity.

And block notation:

 Directives placed in the main configuration files apply to the entire
 server. If you wish to change the configuration for only a part of the
 server, you can scope your directives by placing them in <Directory>,
 <DirectoryMatch>, <Files>, <FilesMatch>, <Location>, and
 <LocationMatch> sections. These sections limit the application of the
 directives which they enclose to particular filesystem locations or
 URLs. They can also be nested, allowing for very fine grained
 configuration.

=head1 RATIONALE

There are at least two other modules on CPAN that perform a similar
function to this one, L<Apache::ConfigFile|Apache::ConfigFile> and
L<Apache::ConfigParser|Apache::ConfigParser>.  Although both are close
to what I need, neither is totally satisfactory.

Apache::ConfigFile suffers from a complete lack of tests and a rather
clumsy API.  Also, it doesn't support quoted strings correctly.

Apache::ConfigParser comes closer to my needs, but contains code
specific to parsing actual Apache configuration files.  As such it is
unsuitable to parsing an application configuration file in Apache
format.  Unlike Apache::ConfigFile, Apache::ConfigParser lacks support
for Include.

Additionally, neither module supports directive inheritence within
blocks.  As this is the main benefit of Apache's block syntax I
decided I couldn't live without it.  

In general, I see no problem with reinventing the wheel as long as
you're sure your version will really be better.  I believe this is, at
least for my purposes.

=head1 METHODS

=item $config = Config::ApacheFormat->new(opt => "value")

This method parses a config file and returns an object that may be
used to access the data contained within.  The object supports the
following attributes, all of which may be set through new():

=over 4

=item inheritence_support

Set this to 0 to turn off the inheritence feature, Block inheritence
means that variables declared outside a block are available from
inside the block unless overriden.  Defaults to 1.

=item include_support

When this is set to 1, the directive "Include" will be treated
specially by the parser.  It will cause the value to be treated as a
filename and that filename will be read in.  This matches Apache's
behavior and allows users to break up configuration files into
multiple, possibly shared, pieces.  Defaults to 1.

=item autoload_support

Set this to 1 and all your directives will be available as object
methods.  So instead of:

  $config->get("foo");

You can write:

  $config->foo;

Defaults to 0.

=item case_sensitive

Set this to 1 to preserve the case of directive names.  Otherwise, all
names will be lc()ed and matched case-insensitively.  Defaults to 0.

=back

All of these attributes are also available as accessor methods.  Thus,
this:

 $config = Config::ApacheFormat->new(inheritence_support => 0,
                                     include_support => 1);

Is equivalent to:

 $config = Config::ApacheFormat->new();
 $config->inheritence_support(0);
 $config->include_support(1);

=over 4

=cut

use Scalar::Util   qw(weaken);
use Carp           qw(croak);
use Text::Balanced qw(extract_delimited);

# declare generated methods
use Class::MethodMaker
  new_with_init => "new",
  new_hash_init => "hash_init",
  get_set => [ -noclear => 
               "inheritence_support", 
               "include_support",
               "autoload_support",
               "case_sensitive", 
               "_parent",
               "_data",
               "_block_vals",
             ];

# setup defaults
sub init {
    my $self = shift;
    my %args = (
                inheritence_support => 1,
                include_support     => 1,
                autoload_support    => 0,
                case_sensitive      => 0,
                _data               => {},
                @_);
    return $self->hash_init(%args);
}

=item $config->read("my.conf");

=item $config->read(\*FILE);

Reads a configuration file into the config object.  You must pass
either the path of the file to be read or a reference to an open
filehandle.  If an error is encountered while reading the file, this
method will die().

Calling read() more than once will add the new configuration values
from another source, overwriting any conflicting values.  Call clear()
first if you want to read a new set from scratch.

=cut

# read the configuration file, optionally ending at block_name
sub read {
    my ($self, $file) = @_;

    # open the file if needed 
    my $fh;
    if (ref $file) {
        $fh = $file;
    } else {
        open($fh, "<", $file) or croak("Unable to open file '$file': $!");
    }

    # read in data
    my @lines = <$fh>;
    
    return $self->_read($file, \@lines);
}

# underlying _read, called recursively an block name for
# nested block objects
sub _read {
    my ($self, $file, $lines, $block_name) = @_;

    # pre-fetch for loop
    my $case_sensitive = $self->{case_sensitive};
    my $data           = $self->{_data};

    # parse through the file, line by line
    my $line_num = 0;
    my ($name, $values, $line);
  LINE: 
    while(@$lines) {
        # accumulate a full line, dealing with line-continuation
        $line = "";
        do {
            $_ = shift @$lines;
            $line_num++;
            s/^\s+//;            # strip leading space
            next LINE if /^#/;   # skip comments
            s/\s+$//;            # strip trailing space            
            $line .= $_;
        } while ($line =~ s/\\$// and @$lines);
        
        # skip blank lines
        next LINE unless length $line;

        # parse line
        if ($line =~ /^<\/(\w+)>$/) {
            # end block            
            $name = $1;
            $name = lc $name unless $case_sensitive; # lc($1) breaks on 5.6.1!

            croak("Syntax error at line $line_num: " .
                  "Unexpected end to block '$name' found." .
                  (defined $block_name ? 
                   "\nI was waiting for </$block_name>\n" : ""))
              unless defined $block_name and $block_name eq $name;

            # this is our cue to return
            last LINE;

        } elsif ($line =~ /^<(\w+)\s*(.*)>$/) {
            # open block
            $name   = $1;
            $values = $2;
            $name   = lc $name unless $case_sensitive;

            my @vals;
            @vals = _parse_value_list($values) if $values;

            # create new object for block, inheriting options from
            # this object, with this object set as parent (using
            # weaken() to avoid creating a circular reference that
            # would leak memory)
            my $parent = $self;
            weaken($parent);
            my $block = ref($self)->new(
                  inheritence_support => $self->{inheritence_support},
                  include_support     => $self->{include_support},
                  autoload_support    => $self->{autoload_support},
                  case_sensitive      => $case_sensitive,
                  _parent             => $parent,
                  _block_vals         => \@vals,
                                       );
            
            # tell the block to read from $fh up to the closing tag
            # for this block
            $block->_read($file, $lines, $name);

            # store block for get() and block()
            push @{$data->{$name}}, $block;

        } elsif ($line =~ /^(\w+)\s+(.+)$/) {
            # directive
            $name = $1;
            $values = $2;
            $name = lc $name unless $case_sensitive;

            # include processing
            if ($name =~ /^include$/i) {
                # try just opening as-is
                if (open(my $include_fh, "<", $values)) {
                    unshift(@$lines, <$include_fh>);
                } elsif (not ref $file) {
                    # try opening it relative to the enclosing file
                    # using File::Spec
                    require File::Spec;
                    my @parts = File::Spec->splitpath($file);
                    $parts[-1] = $values;
                    open($include_fh, "<", File::Spec->catpath(@parts)) or 
                      croak("Unable to open include file '$values', ".
                            "found on line $line_num.");
                    unshift(@$lines, <$include_fh>);
                } else {
                    croak("Unable to open include file '$values', ".
                          "found on line $line_num.");
                }
                
                next LINE;
            }

            if ($values =~ /^\w+$/) {
                # handle the common case of a single unquoted scalar
                # value fast
                $data->{$name} = $values;
            } else {
                # parse out values
                eval {
                    $data->{$name} = _parse_value_list($values);
                };
                croak("Syntax error on line $line_num parsing '$name': $@")
                  if $@;
            }
        } else {
            croak("Syntax error on line $line_num: unable to parse line.");
        }
    }

    return $self;
}

# given a string returns a list of tokens, allowing for quoted strings
# and otherwise splitting on whitespace
sub _parse_value_list {
    my $values = shift;
    # break apart line, allowing for quoted strings with
    # escaping
    my @val;
    while($values =~ /\S/) {
        my $val;
        if ($values =~ /^\s*["']/) {
            $val = extract_delimited($values, q{"'});
            die("value string not properly formatted.\n")
              unless length $val;
            
            # remove quotes and fixup escaped characters
            $val = substr($val, 1, length($val) - 2);
            $val =~ s/\\(.)/$1/g;
        } else {
            ($val, $values) = $values =~ /^\s*(\S+)\s*(.*)$/;
        }
        push(@val, $val);
    }
    die("no value found for directive.\n")
      unless @val;

    return @val == 1 ? $val[0] : \@val;
}


=item $value = $config->get("var_name")

Returns a value from the configuration file.  If the directive
contains a single value, it will be returned.  If the directive
contains a list of values then they will be returned as a list.  If
the directive does not exist in the configuration file then nothing
will be returned (undef in scalar context, empty list in list context).

For example, given this confiuration file:

  Foo 1
  Bar bif baz bop

The following code would work as expected:

  my $foo = $config->get("Foo");   # $foo = 1
  my @bar = $config->get("Bar");   # @bar = ("bif", "baz", "bop")

If the name is the name of a block tag in the configuration file then
a list of available block specifiers will be returned.  For example,
given this configuration file:

  <Site big>
     Size 10
  </Site>

  <Site small>
     Size 1
  </Site>

This call:

  @sites = $config->get("Site");

Will return C<([ Site => "big"], [ Site => "small" ])>.  These arrays
can then be used with the block() method described below.

Calling get() with no arguments will return the names of all available
directives.

=cut

# get a value from the config file.  Called recursively for nested
# blocks.
sub get {
    my ($self, $name) = @_;
    return keys %{$self->{_data}} if @_ == 1;
    $name = lc $name unless $self->{case_sensitive};
    my $val = $self->{_data}{$name};
    if (not defined $val) {
        if ($self->{_parent} and $self->{inheritence_support}) {
            $val = $self->{_parent}->get($name);
        } else {
            return;
        }
    }
    
    # for blocks, return a list of valid block identifiers
    my $type = ref $val;
    if ($type and $type eq 'ARRAY' and 
        ref($val->[0]) eq ref($self)) {
        return map { [ $name, @{$_->{_block_vals}} ] } @$val;
    }
    
    # normal directives are either lists or single values
    return $type ? @$val : $val;
}

=item $block = $config->block("BlockName")

=item $block = $config->block(Directory => "/foo/bar")

=item $block = $config->block(Directory => "~" => "^.*/bar")

This method returns a Config::ApacheFormat object used to access the
values inside a block.  Parameters specified within the block will be
available.  Also, if inheritence is turned on (the default), values
set outside the block that are not overwritten inside the block will
also be available.  For example, given this file:

  MaxSize 100

  <Site "big">
     Size 10
  </Site>

  <Site "small">
     Size 1
  </Site>

this code:

  print "Max: ", $config->get("MaxSize"), "\n";

  $block = $config->block(Site => "big");
  print "Big: ", $block->get("Size"), " / ", 
                 $block->get("MaxSize"), "\n";

  $block = $config->block(Site => "small");
  print "Small: ", $block->get("Size"), " / ", 
                   $block->get("MaxSize"), "\n";

will print:

  Max: 100
  Big: 10 / 100
  Small: 1 / 100

Note that C<block()> does not require any particular number of
parameters.  Any number will work, as long as they uniquely identify a
block in the configuration file.  To get a list of available blocks,
use get() with the name of the block tag.

This method will die() if no block can be found matching the specifier
passed in.

=cut

# get object for a given block specifier
sub block {
    my ($self, $name, @vals) = @_;
    $name = lc $name unless $self->{case_sensitive};
    my $data = $self->_data;

    # make sure we have at least one block named $name
    my $block_array;
    croak("No such block named '$name' in config file.")
      unless ($block_array = $data->{$name} and 
              ref($block_array) eq 'ARRAY' and
              ref($block_array->[0]) eq ref($self));

    # find a block matching @vals.  If Perl supported arbitrary
    # structures as hash keys this could be more efficient.
  BLOCK: 
    foreach my $block (@{$block_array}) {
        if (@vals == @{$block->{_block_vals}}) {
            for (local $_ = 0; $_ < @vals; $_++) {
                next BLOCK unless $vals[$_] eq $block->{_block_vals}[$_];
            }
            return $block;
        }
    }

    croak("No such block named '$name' with values ", 
          join(', ', map { "'$_'" } @vals), " in config file.");
}   

=item $config->clear()

Clears out all data in $config.  Call before re-calling
$config->read() for a fresh read.

=cut

sub clear { shift->_data({}); }

# handle autoload_support feature
sub AUTOLOAD {
    our $AUTOLOAD;
    return if $AUTOLOAD =~ /DESTROY$/;

    my $self = shift;
    my ($name) = $AUTOLOAD =~ /([^:]+)$/;
    croak(qq(Can't locate object method "$name" via package ") . 
          ref($self) . '"')
      unless $self->{autoload_support};

    return $self->get($name);
}


1;
__END__

=back

=head1 TODO

Some possible ideas for future development:

=over 4

=item *

Unroll the recursion in get() for faster access to inherited values.

=item *

Add a set() method.  (useless?)

=item *

Add a write() method to create a new configuration file.  (useless?)

=back

=head1 BUGS

I know of no bugs in this software.  If you find one, please create a
bug report at:

  http://rt.cpan.org/

Include the version of the module you're using and a small piece of
code that I can run which demonstrates the problem.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2002 Sam Tregar

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl 5 itself.

=head1 AUTHOR

Sam Tregar <sam@tregar.com>

=head1 SEE ALSO

L<Apache::ConfigFile|Apache::ConfigFile>

L<Apache::ConfigParser|Apache::ConfigParser>

=cut
