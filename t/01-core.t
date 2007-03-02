#! perl
use Test::More 'no_plan';

# use File::Find;
# use Data::Dumper;
# $Data::Dumper::Deparse = 1;

BEGIN { use_ok('File::Finder') }

isa_ok(my $f = File::Finder->new, "File::Finder");
isa_ok($f->as_wanted, "CODE");
isa_ok($f->as_options, "HASH");
