use Test::More qw(no_plan);
BEGIN { use_ok('Config::ApacheFormat'); }

#my $config = Config::ApacheFormat->new();
#isa_ok($config, 'Config::ApacheFormat');

#use Benchmark qw(timethis);
#timethis(-10, sub { 

my $config = Config::ApacheFormat->new();
$config->read("t/large.conf");
is($config->get("User"), 'nobody');
is($config->get("Group"), 'nobody');

#$config->clear();
#});
