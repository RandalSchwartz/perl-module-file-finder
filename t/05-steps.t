#! perl
use Test::More 'no_plan';

BEGIN { use_ok('File::Finder') }

isa_ok(my $f = File::Finder->new, "File::Finder");

use File::Find;
sub fin {
  my $wanted = shift;

  my @results;
  find(sub {$wanted->() and push @results, $File::Find::name}, @_);
  @results;
}

eq_set([File::Finder->in(qw(.))], [fin(sub { 1 }, '.')], 'all names');

eq_set([File::Finder->name(qr/\t$/)->in(qw(.))],
       [fin(sub { /\.t$/ }, '.')],
       'all files named *.t');

eq_set([File::Finder->perm('+444')->in(qw(.))],
       [fin(sub { (stat($_))[2] & 0444 }, '.')],
       'readable by someone');

eq_set([File::Finder->perm('-444')->in(qw(.))],
       [fin(sub { (stat($_))[2] & 0444 == 0444 }, '.')],
       'readable by everyone');

eq_set([File::Finder->perm('+222')->in(qw(.))],
       [fin(sub { (stat($_))[2] & 0222 }, '.')],
       'writeable by someone');

eq_set([File::Finder->perm('+111')->in(qw(.))],
       [fin(sub { (stat($_))[2] & 0111 }, '.')],
       'executable by someone');

eq_set([File::Finder->perm('644')->in(qw(.))],
       [fin(sub { (stat($_))[2] & 0777 == 0644 }, '.')],
       'mode 644');

eq_set([File::Finder->perm('755')->in(qw(.))],
       [fin(sub { (stat($_))[2] & 0777 == 0755 }, '.')],
       'mode 755');

eq_set([File::Finder->type('f')->in(qw(.))],
       [fin(sub { -f }, '.')],
       'all files');

eq_set([File::Finder->user($<)->in(qw(.))],
       [fin(sub { -o }, '.')],
       'owned');

eq_set([File::Finder->not->user($<)->in(qw(.))],
       [fin(sub { not -o }, '.')],
       'not owned');

eq_set([File::Finder->group(0+$()->in(qw(.))],
       [fin(sub { $( == (stat)[5] }, '.')],
       'group');

eq_set([File::Finder->not->group(0+$()->in(qw(.))],
       [fin(sub { $( != (stat)[5] }, '.')],
       'not group');

eq_set([File::Finder->nouser->in(qw(.))],
       [fin(sub { not getpwuid((stat)[4]) }, '.')],
       'nouser');

eq_set([File::Finder->not->nouser->in(qw(.))],
       [fin(sub { getpwuid((stat)[4]) }, '.')],
       'not nouser');

eq_set([File::Finder->nogroup->in(qw(.))],
       [fin(sub { not getgrgid((stat)[5]) }, '.')],
       'nogroup');

eq_set([File::Finder->not->group(0+$()->in(qw(.))],
       [fin(sub { getgrgid((stat)[5]) }, '.')],
       'not nogroup');

eq_set([File::Finder->links('-2')->in(qw(.))],
       [fin(sub { (stat)[3] < 2 }, '.')],
       'less than 2 links');

eq_set([File::Finder->links('+1')->in(qw(.))],
       [fin(sub { (stat)[3] > 1 }, '.')],
       'more than 1 link');

eq_set([File::Finder->size('-10c')->in(qw(.))],
       [fin(sub { -s $_ < 10 }, '.')],
       'less than 10 bytes');

eq_set([File::Finder->size('+10c')->in(qw(.))],
       [fin(sub { -s $_ > 10 }, '.')],
       'more than 10 bytes');
