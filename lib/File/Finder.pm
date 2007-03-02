package File::Finder;

use 5.006;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
	
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.03';

use Carp qw(croak);

## public methods:

sub new {
  my $class = shift;
  bless {
	 options => {},
	 steps => [],
	}, $class;
}

sub as_wanted {
  my $self = shift;
  return sub { $self->_run };
}

use overload
  '&{}' => 'as_wanted',
  '""' => sub { overload::StrVal(shift) },
  ;

sub as_options {
  my $self = shift;
  return { %{$self->{options}}, wanted => sub { $self->_run } };
}

sub in {
  my $self = _force_object(shift);

  ## this must return count in a scalar context
  $self->collect(sub { $File::Find::name }, @_);
}

sub collect {
  my $self = _force_object(shift);
  my $code = shift;

  my @result;
  my $self_store = $self->eval( sub { push @result, $code->() } );

  require File::Find;
  File::Find::find($self_store->as_options, @_);

  ## this must return count in a scalar context
  return @result;
}

## private methods

sub _force_object {
  my $self_or_class = shift;
  ref $self_or_class ? $self_or_class : $self_or_class->new;
}

sub _clone {
  my $self = _force_object(shift);
  bless {
	 options => {%{$self->{options}}},
	 steps => [@{$self->{steps}}],
	}, ref $self;
}

sub _run {
  my $self = shift;

  my @state = (1);
  ## $state[-1]:
  ## if 2: we're in a true state, but we've just seen a NOT
  ## if 1: we're in a true state
  ## if 0: we're in a false state
  ## if -1: we're in a "skipping" state (true OR ...[here]...)

  ## I thought File::Find promised thus, but apparently not:
  defined $_ and lstat($_);	# ignore results
  ## this sets the "_" filehandle properly for our tests

  for my $step(@{$self->{steps}}) {
    if (ref $step) {		# coderef
      if ($state[-1] >= 1) {	# true state
	if ($self->$step) {	# coderef ran returning true
	  if ($state[-1] == 2) {
	    $state[-1] = 0;
	  }
	} else {
	  $state[-1]--;		# 2 => 1, 1 => 0
	}
      }
    } elsif ($step eq "or") {
      # -1 => -1, 0 => 1, 1 => -1, 2 is error
      croak "not before or?" if $state[-1] > 1;
      if ($state[-1] == 0) {
	$state[-1] = 1;
      } elsif ($state[-1] == 1) {
	$state[-1] = -1;
      }
    } elsif ($step eq "left") {
      ## start subrule
      ## -1 => -1, 0 => -1, 1 => 1, 2 => 1
      push @state, ($state[-1] >= 1) ? 1 : -1;
    } elsif ($step eq "right") {
      ## end subrule
      croak "right without left" unless @state > 1;
      croak "not before right" if $state[-1] > 1;
      my $result = pop @state;
      if ($state[-1] >= 1) {
	if ($result) { # 1 or -1, so counts as true
	  if ($state[-1] == 2) {
	    $state[-1] = 0;
	  }
	} else {
	  $state[-1]--;		# 2 => 1, 1 => 0
	}
      }
    } elsif ($step eq "comma") {
      croak "not before comma" if $state[-1] > 1;
      if (@state < 2) {		# not in parens
	$state[-1] = 1;		# reset to true
      } else {			# in parens, reset as if start of parens
	$state[-1] = (($state[-2] >= 1) ? 1 : -1);
      }
    } elsif ($step eq "not") {
      # -1 => -1, 0 => 0, 1 => 2, 2 => 1
      if ($state[-1] >= 1) {
	$state[-1] = $state[-1] > 1 ? 1 : 2;
      }
    } else {
      die "internal error at $step";
    }
  }
  croak "left without right" unless @state == 1;
  croak "trailing not" if $state[-1] > 1;
  return $state[-1] != 0;	# true and skipping are both true
}

sub AUTOLOAD {
  my $self = _force_object(shift);

  my ($method) = our $AUTOLOAD =~ /(?:.*::)?(.*)/;
  return if $method eq "DESTROY";

  my $clone = $self->_clone;
  my $sub_method = $clone->_steps_class->can($method)
    or croak "Cannot add step $method";

  push @{$clone->{steps}}, $sub_method->($clone, @_);
  $clone;
}

sub _steps_class { "File::Finder::Steps" }

package File::Finder::Steps;

use Carp qw(croak);

## meta methods return literal strings

sub or { return "or" }
sub left { return "left" }
BEGIN { *begin = \&left; }
sub right { return "right" }
BEGIN { *end = \&right; }
sub not { return "not" }
sub comma { return "comma" }	# gnu extension

## The remaining methods get called with $self as the first parameter,
## and can perform compile-time operations (like computing closure
## vars or accessing $self->{options}). They must return a coderef
## that will be called as method at runtime (passing the same $self
## again) and must return true/false to indicate success/failure.

## constants

sub true { return sub { 1 } }
sub false { return sub { 0 } }

## these mimic the order in find2perl

sub follow {
  my $self = shift;
  $self->{options}{follow} = 1;
  return sub { 1 };
}

sub name {
  my $self = shift;
  my $name = shift;

  unless (UNIVERSAL::isa($name, "Regexp")) {
    require Text::Glob;
    $name = Text::Glob::glob_to_regex($name);
  }

  return sub {
    /$name/;
  };
}

sub perm {
  my $self = shift;
  my $perm = shift;
  $perm =~ /^(\+|-)?\d+\z/ or croak "bad permissions $perm";
  if ($perm =~ s/^-//) {
    $perm = oct($perm) if $perm =~ /^0/;
    return sub {
      ((stat _)[2] & $perm) == $perm;
    };
  } elsif ($perm =~ s/^\+//) {	# gnu extension
    $perm = oct($perm) if $perm =~ /^0/;
    return sub {
      ((stat _)[2] & $perm);
    };
  } else {
    $perm = oct($perm) if $perm =~ /^0/;
    return sub {
      ((stat _)[2] & 0777) == $perm;
    };
  }
}

BEGIN {
  my %typecast;

  sub type {
    my $self = shift;
    my $type = shift;

    $type =~ /^[a-z]\z/i or croak "bad type $type";
    $type =~ s/s/S/;

    $typecast{$type} ||= eval "sub { -$type _ }";
  }
}

sub print {
  return sub {
    print $File::Find::name, "\n";
    1;
  };
}

sub print0 {
  return sub {
    print $File::Find::name, "\0";
    1;
  };
}

# sub fstype

sub user {
  my $self = shift;
  my $user = shift;

  my $uid = ($user =~ /^\d+\z/) ? $user : _user_to_uid($user);
  die "bad user $user" unless defined $uid;

  return sub {
    (stat _)[4] == $uid;
  };
}

sub group {
  my $self = shift;
  my $group = shift;

  my $gid = ($group =~ /^\d+\z/) ? $group : _group_to_gid($group);
  die "bad group $group" unless defined $gid;

  return sub {
    (stat _)[5] == $gid;
  };
}

sub nouser {
  return sub {
    CORE::not defined _uid_to_user((stat _)[4]);
  }
}

sub nogroup {
  return sub {
    CORE::not defined _gid_to_group((stat _)[5]);
  }
}

## from ovid:
#  my %status = (
#  	      links => sub { (stat(_))[3] },
#  	      inum => sub { (stat(_))[1] },
#  	      atime => sub { int(-A _) },
#  	      mtime => sub { int(-M _) },
#  	      ctime => sub { int(-C _) },
#  	     );
#  while (my ($function,$op) = each %status) {
#    no strict 'refs';
#    *$function = sub {
#      my $self = shift;
#      my ($prefix, $n) = shift =~ qr/^(\+|-|)(.*)/;
#      return sub {
#        _n($prefix, $n, $op->());
#      };
#    }
#  } 

sub links {
  my $self = shift;
  my ($prefix, $n) = shift =~ /^(\+|-|)(.*)/;

  return sub {
    _n($prefix, $n, (stat(_))[3]);
  };
}

sub inum {
  my $self = shift;
  my ($prefix, $n) = shift =~ /^(\+|-|)(.*)/;

  return sub {
    _n($prefix, $n, (stat(_))[1]);
  };
}

sub size {
  my $self = shift;
  my ($prefix, $n) = shift =~ /^(\+|-|)(.*)/;

  if ($n =~ s/c\z//) {
    return sub {
      _n($prefix, $n, int(-s _));
    };
  }
  if ($n =~ s/k\z//) {
    return sub {
      _n($prefix, $n, int(((-s _)+1023) / 1024));
    };
  }
  return sub {
    _n($prefix, $n, int(((-s _)+511) / 512));
  };
}

sub atime {
  my $self = shift;
  my ($prefix, $n) = shift =~ /^(\+|-|)(.*)/;

  return sub {
    _n($prefix, $n, int(-A _));
  };
}

sub mtime {
  my $self = shift;
  my ($prefix, $n) = shift =~ /^(\+|-|)(.*)/;

  return sub {
    _n($prefix, $n, int(-M _));
  };
}

sub ctime {
  my $self = shift;
  my ($prefix, $n) = shift =~ /^(\+|-|)(.*)/;

  return sub {
    _n($prefix, $n, int(-C _));
  };
}

sub exec {
  my $self = shift;
  my @command = @_;

  return sub {
    my @mapped = @command;
    for my $one (@mapped) {
      $one =~ s/{}/$_/g;
    }
    system @mapped;
    return !$?;
  };
}

sub ok {
  my $self = shift;
  my @command = @_;

  return sub {
    my @mapped = @command;
    for my $one (@mapped) {
      $one =~ s/{}/$_/g;
    }
    my $old = select(STDOUT);
    $|++;
    print "@mapped? ";
    select $old;
    return 0 unless <STDIN> =~ /^y/i;
    system @mapped;
    return !$?;
  };
}

sub prune {
  return sub { $File::Find::prune = 1 };
}

# sub xdev
# sub newer

sub eval {
  my $self = shift;
  my $eval = shift;

  ## if this is another File::Finder object... then cheat:
  $eval = $eval->as_wanted if UNIVERSAL::can($eval, "as_wanted");

  return $eval;			# just reuse the coderef
}

sub depth {
  my $self = shift;
  $self->{options}{bydepth} = 1;
  return sub { 1 };
}

sub ls {
  return \&_ls;
}

## sub tar
## sub cpio / ncpio

## utility subroutines

sub _n {
  my ($prefix, $arg, $value) = @_;
  if ($prefix eq "+") {
    $value > $arg;
  } elsif ($prefix eq "-") {
    $value < $arg;
  } else {
    $value == $arg;
  }
}

BEGIN {

  my %user_to_uid;
  my %uid_to_user;

  my $initialize = sub {
    while (my ($user, $pw, $uid) = getpwent) {
      $user_to_uid{$user} = $uid;
      $uid_to_user{$uid} = $user;
    }
  };

  sub _user_to_uid {
    my $user = shift;

    %user_to_uid or $initialize->();
    $user_to_uid{$user};
  }

  sub _uid_to_user {
    my $uid = shift;

    %uid_to_user or $initialize->();
    $uid_to_user{$uid};
  }

}

BEGIN {

  my %group_to_gid;
  my %gid_to_group;

  my $initialize = sub {
    while (my ($group, $pw, $gid) = getgrent) {
      $group_to_gid{$group} = $gid;
      $gid_to_group{$gid} = $group;
    }
  };

  sub _group_to_gid {
    my $group = shift;

    %group_to_gid or $initialize->();
    $group_to_gid{$group};
  }

  sub _gid_to_group {
    my $gid = shift;

    %gid_to_group or $initialize->();
    $gid_to_group{$gid};
  }

}

BEGIN {
  ## from find2perl

  my @rwx = qw(--- --x -w- -wx r-- r-x rw- rwx);
  my @moname = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);

  sub _sizemm {
    my $rdev = shift;
    sprintf("%3d, %3d", ($rdev >> 8) & 0xff, $rdev & 0xff);
  }

  sub _ls {
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
        $atime,$mtime,$ctime,$blksize,$blocks) = stat(_);
    my $pname = $File::Find::name;

    $blocks
      or $blocks = int(($size + 1023) / 1024);

    my $perms = $rwx[$mode & 7];
    $mode >>= 3;
    $perms = $rwx[$mode & 7] . $perms;
    $mode >>= 3;
    $perms = $rwx[$mode & 7] . $perms;
    substr($perms, 2, 1) =~ tr/-x/Ss/ if -u _;
    substr($perms, 5, 1) =~ tr/-x/Ss/ if -g _;
    substr($perms, 8, 1) =~ tr/-x/Tt/ if -k _;
    if    (-f _) { $perms = '-' . $perms; }
    elsif (-d _) { $perms = 'd' . $perms; }
    elsif (-l _) { $perms = 'l' . $perms; $pname .= ' -> ' . readlink($_); }
    elsif (-c _) { $perms = 'c' . $perms; $size = _sizemm($rdev); }
    elsif (-b _) { $perms = 'b' . $perms; $size = _sizemm($rdev); }
    elsif (-p _) { $perms = 'p' . $perms; }
    elsif (-S _) { $perms = 's' . $perms; }
    else         { $perms = '?' . $perms; }

    my $user = _uid_to_user($uid) || $uid;
    my $group = _gid_to_group($gid) || $gid;

    my ($sec,$min,$hour,$mday,$mon,$timeyear) = localtime($mtime);
    if (-M _ > 365.25 / 2) {
      $timeyear += 1900;
    } else {
      $timeyear = sprintf("%02d:%02d", $hour, $min);
    }

    printf "%5lu %4ld %-10s %3d %-8s %-8s %8s %s %2d %5s %s\n",
      $ino, $blocks, $perms, $nlink, $user, $group, $size,
	$moname[$mon], $mday, $timeyear, $pname;
    1;
  }
}

## heh heh

sub ffr {
  my $self = shift;
  my $ffr_object = shift;

  my $their_wanted;

  no warnings;
  local *File::Find::find = sub {
    my ($options) = @_;
    for (my ($k, $v) = each %$options) {
      if ($k eq "wanted") {
  	$their_wanted = $v;
      } else {
  	$self->{options}->{$k} = $v;
      }
    }
  };
  $ffr_object->in("/DUMMY");	# boom!
  croak "no wanted defined" unless defined $their_wanted;
  return $their_wanted;
}

1;
__END__

=head1 NAME

File::Finder - nice wrapper for File::Find ala find(1)

=head1 SYNOPSIS

  use File::Finder;
  ## simulate "-type f"
  my $all_files = File::Finder->type('f');

  ## any rule can be extended:
  my $all_files_printer = $all_files->print;

  ## traditional use: generating "wanted" subroutines:
  use File::Find;
  find($all_files_printer, @starting_points);  

  ## or, we can gather up the results immediately:
  my @results = $all_files->in(@starting_points);

  ## -depth and -follow are noted, but need a bit of help for find:
  my $deep_dirs = File::Finder->depth->type('d')->ls->exec('rmdir','{}');
  find($deep_dirs->as_options, @places);

=head1 DESCRIPTION

C<File::Find> is great, but constructing the C<wanted> routine can
sometimes be a pain.  This module provides a C<wanted>-writer, using
syntax that is directly mappable to the I<find> command's syntax.

Also, I find myself (heh) frequently just wanting the list of names
that match.  With C<File::Find>, I have to write a little accumulator,
and then access that from a closure.  But with C<File::Finder>, I can
turn the problem inside out.

A C<File::Finder> object contains a hash of C<File::Find> options, and
a series of steps that mimic I<find>'s predicates.  Initially, a
C<File::Finder> object has no steps.  Each step method clones the
previous object's options and steps, and then adds the new step,
returning the new object.  In this manner, an object can be grown,
step by step, by chaining method calls.  Furthermore, a partial
sequence can be created and held, and used as the head of many
different sequences.

For example, a step sequence that finds only files looks like:

  my $files = File::Finder->type('f');

Here, C<type> is acting as a class method and thus a constructor.  An
instance of C<File::Finder> is returned, containing the one step to
verify that only files are selected.  We could use this immediately
as a C<File::Find::find> wanted routine, although it'd be uninteresting:

  use File::Find;
  find($files, "/tmp");

Calling a step method on an existing object adds the step, returning
the new object:

  my $files_print = $files->print;

And now if we use this with C<find>, we get a nice display:

  find($files_print, "/tmp");

Of course, we didn't really need that second object: we could
have generated it on the fly:

  find($files->print, "/tmp");

C<File::Find> supports options to modify behavior, such as depth-first
searching.  The C<depth> step flags this in the options as well:

  my $files_depth_print = $files->depth->print;

However, the C<File::Finder> object needs to be told explictly to
generate an options hash for C<File::Find::find> to pass this
information along:

  find($files_depth_print->as_options, "/tmp");

A C<File::Finder> object, like the I<find> command, supports AND, OR,
NOT, and parenthesized sub-expressions.  AND binds tighter than OR,
and is also implied everywhere that it makes sense.  Like I<find>, the
predicates are computed in a "short-circuit" fashion, so that a false
to the left of the (implied) AND keeps the right side from being
evaluated, including entire parenthesized subexpressions.  Similarly,
if the left side of an OR is false, the right side is evaluated, and
if the left side of the OR is true, the right side is skipped.  Nested
parens are handled properly.  Parens are indicated with the rather
ugly C<left> and C<right> methods:

  my $big_or_old_files = $files->left->size("+50")->or->atime("+30")->right;

The parens here correspond directly to the parens in:

  find somewhere -type f '(' -size +50 -o -atime +30 ')'

and are needed so that the OR and the implied ANDs have the right
nesting.

Besides passing the constructed C<File::Finder> object to
C<File::Finder::find> directly as a C<wanted> routine or an options
hash, you can also call C<find> implictly, with C<in>.  C<in> provides
a list of starting points, and returns all filenames that match the
criteria.

For example, a list of all names in /tmp can be generated simply with:

 my @names = File::Finder->in("/tmp");

For more flexibility, use C<collect> to execute an arbitrary block
in a list context, concatenating all the results (similar to C<map>):

  my %sizes = File::Finder
    ->collect(sub { $File::Find::name => -s _ }, "/tmp");

That's all I can think of for now.  The rest is in the detailed
reference below.

=head2 META METHODS

All of these methods can be used as class or instance methods, except
C<new>, which is usually not needed and is class only.

=over

=item new

Not strictly needed, because any instance method called on a class
will create a new object anyway.

=item as_wanted

Returns a subroutine suitable for passing to C<File::Find::find> or
C<File::Find::finddepth> as the I<wanted> routine.  If the object is
used in a place that wants a coderef, this happens automatically
through overloading.

=item as_options

Returns a hashref suitable for passing to C<File::Find::find> or
C<File::Find::finddepth> as the I<options> hash. This is necessary if
you want the meta-information to carry forward properly.

=item in(@starting_points)

Calls C<< File::Find::find($self->as_options, @starting_points) >>,
gathering the results, and returns the results as a list.  At the
moment, it also returns the count of those items in a scalar context.
If that's useful, I'll maintain that.

=item collect($coderef, @starting_points)

Calls C<$coderef> in a list context for each of the matching items,
gathering and concatenating the results, and returning the results as
a list.

  my $f = File::Finder->type('f');
  my %sizes = $f->collect(sub { $File::Find::name, -s _ }, "/tmp");

In fact, C<in> is implemented by calling C<collect> with a coderef
of just C<sub { $File::Find::name }>.

=back

=head2 STEPS METHODS

These methods are called on a class or instance to add a "step".  Each
step adds itself to a list of steps, returning the new object.  This
allows you to chain steps together to form a formula.

As in I<find>, the default operator is "and", and short-circuiting is
performed.

=over

=item or

Like I<find>'s C<or>.

=item left

Like a left parenthesis.  Used in nesting pairs with C<right>.

=item right

Like a right parenthesis.  Used in nesting pairs with C<left>.
For example:

  my $big_or_old = File::Finder
    ->type('f')
    ->left
      ->size("+100")->or->mtime("+90")
    ->right;
  find($big_or_old->ls, "/tmp");

You need parens because the "or" operator is lower precedence than
the implied "and", for the same reason you need them here:

  find /tmp -type f '(' -size +100 -o -mtime +90 ')' -print

Without the parens, the -type would bind to -size, and not to the
choice of -size or -mtime.

Mismatched parens will not be found until the formula is used, causing
a fatal error.

=item not

Like I<find>'s C<!>.  Prefix operator, can be placed in front of
individual terms or open parens.  Can be nested, but what's the point?

  # list all non-files in /tmp
  File::Finder->not->type('f')->ls->in("/tmp");

=item true

Always returns true.  Useful when a subexpression might fail, but
you don't want the overall code to fail:

  ... ->left-> ...[might return false]... ->or->true->right-> ...

Of course, this is the I<find> command's idiom of:

   find .... '(' .... -o -true ')' ...

=item false

Always returns false.

=item comma

Like GNU I<find>'s ",".  The result of the expression (or
subexpression if in parens) up to this point is discarded, and
execution continues afresh.  Useful when a part of the expression is
needed for its side effects, but shouldn't affect the rest of the
"and"-ed chain.

  # list all files and dirs, but don't descend into CVS dir contents:
  File::Finder->type('d')->name('CVS')->prune->comma->ls->in('.');

=item follow

Enables symlink following, and returns true.

=item name(NAME)

True if basename matches NAME, which can be given as a glob
pattern or a regular expression object:

  my $pm_files = File::Finder->name('*.pm')->in('.');
  my $pm_files_too = File::Finder->name(qr/pm$/)->in('.');

=item perm(PERMISSION)

Like I<find>'s C<-perm>.  Leading "-" means "all of these bits".
Leading "+" means "any of these bits".  Value is de-octalized if a
leading 0 is present, which is likely only if it's being passed as a
string.

  my $files = File::Finder->type('f');
  # find files that are exactly mode 644
  my $files_644 = $files->perm(0644);
  # find files that are at least world executable:
  my $files_world_exec = $files->perm("-1");
  # find files that have some executable bit set:
  my $files_exec = $files->perm("+0111");

=item type(TYPE)

Like I<find>'s C<-type>.  All native Perl types are supported.  Note
that C<s> is a socket, mapping to Perl's C<-S>, to be consistent with
I<find>.  Returns true or false, as appropriate.

=item print

Prints the fullname to C<STDOUT>, followed by a newline.  Returns true.

=item print0

Prints the fullname to C<STDOUT>, followed by a NUL.  Returns true.

=item fstype

Not implemented yet.

=item user(USERNAME|UID)

True if the owner is USERNAME or UID.

=item group(GROUPNAME|GID)

True if the group is GROUPNAME or GID.

=item nouser

True if the entry doesn't belong to any known user.

=item nogroup

True if the entry doesn't belong to any known group.

=item links( +/- N )

Like I<find>'s C<-links N>.  Leading plus means "more than", minus
means "less than".

=item inum( +/- N )

True if the inode number meets the qualification.

=item size( +/- N [c/k])

True if the file size meets the qualification.  By default, N is
in half-K blocks.  Append a trailing "k" to the number to indicate
1K blocks, or "c" to indicate characters (bytes).

=item atime( +/- N )

True if access time (in days) meets the qualification.

=item mtime( +/- N )

True if modification time (in days) meets the qualification.

=item ctime( +/- N )

True if inode change time (in days) meets the qualification.

=item exec(@COMMAND)

Forks the child process via C<system()>.  Any appearance of C<{}> in
any argument is replaced by the current filename.  Returns true if the
child exit status is 0.  The list is passed directly to C<system>,
so if it's a single arg, it can contain C</bin/sh> syntax.  Otherwise,
it's a pre-parsed command that must be found on the PATH.

Note that I couldn't figure out how to horse around with the current
directory very well, so I'm using C<$_> here instead of the more
traditional C<File::Find::name>.  It still works, because we're still
chdir'ed down into the directory, but it looks weird on a trace.
Trigger C<no_chdir> in C<find> if you want a traditional I<find> full
path.

  my $f = File::Finder->exec('ls', '-ldg', '{}');
  find({ no_chdir => 1, wanted => $f }, @starting_dirs);

Yeah, it'd be trivial for me to add a no_chdir method.  Soon.

=item ok(@COMMAND)

Like C<exec>, but displays the command line first, and waits for a
response.  If the response begins with C<y> or C<Y>, runs the command.
If the command fails, or the response wasn't yes, returns false,
otherwise true.

=item prune

Sets C<$File::Find::prune>, and returns true.

=item xdev

Not yet implemented.

=item newer

Not yet implemented.

=item eval(CODEREF)

Ah yes, the master escape, with extra benefits.  Give it a coderef,
and it evaluates that code at the proper time.  The return value is noted
for true/false and used accordingly.

  my $blaster = File::Finder->atime("+30")->eval(sub { unlink });

But wait, there's more.  If the parameter is an object that responds
to C<as_wanted>, that method is automatically called, hoping for a
coderef return. This neat feature allows subroutines to be created and
nested:

  my $old = File::Finder->atime("+30");
  my $big = File::Finder->size("+100");
  my $old_or_big = File::Finder->eval($old)->or->eval($big);
  my $killer = File::Finder->eval(sub { unlink });
  my $kill_old_or_big = File::Finder->eval($old_or_big)->ls->eval($killer);
  $kill_old_or_big->in('/tmp');

Almost too cool for words.

=item depth

Like I<find>'s C<-depth>.  Sets a flag for C<as_options>, and returns true.

=item ls

Like I<find>'s C<-ls>.  Performs a C<ls -dils> on the entry to
C<STDOUT> (without forking), and returns true.

=item tar

Not yet implemented.

=item [n]cpio

Not yet implemented.

=item ffr($ffr_object)

Incorporate a C<File::Find::Rule> object as a step. Note that this
must be a rule object, and not a result, so don't call or pass C<in>.
For example, using C<File::Find::Rule::ImageSize> to define a
predicate for image files that are bigger than a megapixel in my
friends folder, I get:

  require File::Finder;
  require File::Find::Rule;
  require File::Find::Rule::ImageSize;
  my $ffr = File::Find::Rule->file->image_x('>1000')->image_y('>1000');
  my @big_friends = File::Finder->ffr($ffr)
    ->in("/Users/merlyn/Pictures/Sorted/Friends");

=back

=head2 EXTENDING

The steps methods are actually in the C<File::Finder::Steps> class.
You can add more subroutines to that package directly, or subclass
that class.  If you subclass that class, you should subclass
C<File::Finder> and override the C<_steps_class> method to return your
new subclass name.

The exact protocol of a step generator is in the source code, and
won't be repeated here.  If you're smart enough to want to extend
this, you're smart enough to find the source. {grin}

=head2 SPEED

All the steps can have a compile-time and run-time component.  As
much work is done during compile-time as possible.  Runtime consists
of a simple linear pass executing a series of closures representing
the individual steps (not method calls).  It is hoped that this will
produce a speed that is within a factor of 2 or 3 of a handcrafted
monolithic C<wanted> routine.

=head1 SEE ALSO

C<File::Find>, I<find2perl>, C<File::Find::Rule>

=head1 BUGS

None known yet.

=head1 AUTHOR

Randal L. Schwartz, E<lt>merlyn@stonehenge.comtE<gt>, with a tip
of the hat to Richard Clamp for C<File::Find::Rule>.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2003 by Randal L. Schwartz, Stonehenge Consulting Services, Inc.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=cut
