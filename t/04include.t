use Test::More qw(no_plan);
use Config::ApacheFormat;

my $config = Config::ApacheFormat->new();
$config->read("t/includer.conf");

is($config->get('foo'), 1);
is($config->get('bar'), 2);


