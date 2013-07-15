#!/usr/bin/perl

if ('' cmp 'DOC SYNOPSIS') {

=pod

=head1 NAME

compile-latex - compile files until reaching a fixed point

=head1 SYNOPSIS

=head2 ACTIONS

Actions are things that C<compile-latex> does if asked.  If no action is
specified, then B<--build> is used instead.

Basic actions are:

compile-latex B<--help>|B<--man>|B<--nroff>|B<--usage>

compile-latex B<[--action build|--build]> I<goals>

compile-latex B<--action [in|out|junk|ignore]> I<goals>

compile-latex B<[--action clean|--clean]> I<goals>

=cut

}

use strict;use warnings;
# Unicode safety
use utf8;use Encode;
use JSON::PP;use Data::Dumper;
use Digest::MD5;
use Term::ANSIColor;
use File::Path qw(make_path remove_tree);use Cwd qw(getcwd realpath);
# command execution
use Symbol 'gensym';use IPC::Open3;
use POSIX qw(:sys_wait_h dup dup2);use IO::Select;use IO::Handle;

my ($sourceContext,$optionContext,$execContext,$outContext,$progContext);

$progContext={'tmpfiles' => {}, 'in'=> {}, 'out' => {} , 'junk' => {}, 'ignore' => {}};

# Convenience

sub clone {
  my $obj=shift @_;
  my $t=ref($obj);
  return $obj unless ($t);
  return [map {&clone($_)} @$obj] if ($t eq 'ARRAY');
  my $n={};
  foreach my $key (keys %$obj) {
    $n->{$key}=&clone($obj->{$key});
  }
  return $n;
}

sub addHash {
  my $hash={};
  foreach my $h (@_) {
    foreach my $k (keys %$h) {
      $hash->{$k}=1;
    }
  }
  return $hash;
}
sub addHashInPlace {
  my $hash=shift @_;
  foreach my $h (@_) {
    foreach my $k (keys %$h) {
      $hash->{$k}=$h->{$k};
    }
  }
}
sub subHash {
  my $hash={};    # TODO optimize if first hash much larger than second?
  my $init=shift @_;
  foreach my $k (keys %$init) {
    $hash->{$k}=1;
  }
  foreach my $h (@_) {
    foreach my $k (keys %$h) {
      delete ($hash->{$k}) if (exists($hash->{$k}));
    }
  }
  return $hash;
}

sub main {
  &initLogging();
  &parseARGV();
  my $actions=$optionContext->{'actions'};
  if (defined($actions->{'help'})) {
    &usage($optionContext->{'help'});
  }
  &setupCommand();
  foreach my $sourcefile (sort keys %{$optionContext->{'includedFiles'}}) {
    &processSourcefile($sourcefile);
  }
  foreach my $i ('in','out','junk','ignore') {
    if (defined($optionContext->{'actions'}->{$i})) {
      foreach my $key (sort keys %{$progContext->{$i}}) {
        &out(1,'result',&encodeGoalLine($key));
      }
    }
  }
  foreach my $i ('cli') {
    if (defined($optionContext->{'actions'}->{$i})) {
      foreach my $key (keys %{$progContext->{$i}}) {
        &out(1,'result',$key.";".$progContext->{$i}->{$key});
      }
    }
  }
  &finish(0);
}

&main();

# Logging
sub initLogging {

=pod

=head2 OUTPUT OPTIONS

B<--verbosity> I<category> I<level> Change the verbosity to I<level> in I<category>.

B<--I<category>> I<level> Shortcut, as above (see L</"VERBOSITY">).

If verbosity can be mistaken for a target name, use the B<--verbosity>
form, because the target will take precedence.

The verbosity level is a positive integer (including 0).


The various categories are :

=over

=item * B<init> initialization information (not useful)

=item * B<plan> informations about the building plan (can be very verbose)

=item * B<exec> informations about the commands launched by the plan

=item * B<err> the error output of the various commands

=item * B<out> the console output of the various commands

=item * B<display> the summary of the execution of the plan (1: only
source files, 2: also jobnames, 3: each command (default))

=item * B<result> the result of actions such as B<out>, B<in> or B<junk>.

=back

Each rule can define a class and a subclass, and each item built will be
output in a synthetic manner. The default setting is 3 for B<display>, 1
for B<what> and 1 for B<out> and B<err>. A quiet effect can be obtained
by setting everything to 0.

The B<--filter> option allows intelligent filtering on the TeX output
(which is very difficult to parse). If some possible error message is
detected (possibly leading to user input), the output will be fully
printed. Else, nothing will be printed.

=cut

  binmode(STDOUT,':utf8');
  binmode(STDERR,':utf8');
  binmode(STDIN,':utf8');
  my $verbosity={'exec'=>0,
                 'init'=>0,
                 'plan'=>0,
                 'result'=>1,
                 'out'=>1,
                 'err'=>1,
                 'dev'=>1,
                 'display'=>3};
  my $jsonEncoder=JSON::PP->new->utf8->pretty->canonical;
  $outContext={
               'color' => 1,
               'class' => '',
               'subclass' => '',
               'currentcol' => 0,
               'nomove' => 0,
               'countphase' => '',
               'lastcountcol' => 0,
               'verbosity' => $verbosity,
               'specialprefix' => qr/^PARTIAL PLAN:(.*)$/,
               'json' => $jsonEncoder,
               'outString' => ''
              };
  &columns();
  $SIG{WINCH} = sub { &columns(); };
  $ENV{'SPECIALPREFIX'}="PARTIAL PLAN:";
  $outContext->{'color'}=0 unless -t STDOUT;
  &out(1,'init','Logging system ready');
}                               # DOC INSIDE
sub columns {
  require 'sys/ioctl.ph';
  my $winsize;
  die 'no TIOCGWINSZ' unless defined &TIOCGWINSZ;
  open(TTY, "+</dev/tty") or die "No tty: $!";
  unless (ioctl(TTY, &TIOCGWINSZ, $winsize='')) {
    die sprintf "$0: ioctl TIOCGWINSZ (%08x: $!)\n", &TIOCGWINSZ;
  }
  my ($row, $col, $xpixel, $ypixel) = unpack('S4', $winsize);
  $outContext->{'columns'}=$col;
}
sub finish {
  my $status=shift @_;
  &newline();
  if ($status) {
    print "Dying: ",@_,"\n";
  }
  foreach my $key (keys %{$progContext->{'tmpfiles'}}) {
    unlink($key);
  }
  exit($status);
}
sub newline {
  print "\n" if ($outContext->{'currentcol'});
  $outContext->{'currentcol'}=0;
  $outContext->{'nomove'}=0;
}
sub class {
  my $class=$_[0];
  if ($class ne $outContext->{'class'}) {
    &newline();
    $outContext->{'subclass'}='';
    print color 'bold' if ($outContext->{'color'});
    print $class,"\n";
    print color 'reset' if ($outContext->{'color'});
    $outContext->{'class'}=$class;
  }
}
sub subclass {
  my $subclass=$_[0];
  if ($subclass ne $outContext->{'subclass'}) {
    $outContext->{'subclass'}=$subclass;
    print color 'bold' if ($outContext->{'color'});
    &outDisplay($subclass);
    print color 'reset' if ($outContext->{'color'});
  }
}
sub outDisplay {
  my $str=join('',@_);
  my $storepos=0;
  if ($str =~ /^subclass:(.*)$/) {
    &subclass($1);
    return;
  }
  if ($str =~ /^count:(.*):([0-9\/]+)$/) {
    $str="$1:$2";
    my $oldcountphase=$outContext->{'countphase'};
    my $countphase="$1";
    $outContext->{'countphase'}=$countphase;
    if ($outContext->{'color'} == 0) {
      my $lastcountcol=$outContext->{'lastcountcol'}+1;
      if ($lastcountcol == 500) {
        $lastcountcol=0;
      }
      if ($countphase ne $oldcountphase) {
        $lastcountcol=0;
      }
      $outContext->{'lastcountcol'}=$lastcountcol;
      return if ($lastcountcol != 0);
    } else {
      if ($outContext->{'nomove'} == 1 and $countphase eq $oldcountphase) {
        $storepos=2;
      } else {
        $storepos=1;
      }
    }
  } else {
    $outContext->{'countphase'}='';
  }
  if ($storepos == 2) {
    $outContext->{'nomove'}=1;
    $outContext->{'currentcol'}=$outContext->{'lastcountcol'};
    print "\x1b[u";
  }
  my $len=length($str);
  my $margin=2;                 # Two chars required for []
  if ($outContext->{'currentcol'}+$margin+$len>$outContext->{'columns'}) {
    &newline();
  }
  if ($outContext->{'currentcol'}==0) {
    print '    ';
    $outContext->{'currentcol'}=4;
  }
  if ($storepos == 1) {
    $outContext->{'nomove'}=1;
    $outContext->{'lastcountcol'}=$outContext->{'currentcol'};
    print "\x1b[s";
  } elsif ($storepos != 2) {
    $outContext->{'nomove'}=0;
  }
  $outContext->{'currentcol'}+=2+$len;
  print '[',$str,']';
}
sub out {
  my $v=shift @_;
  my $n=shift @_;
  if ($outContext->{'verbosity'}->{$n}>=$v) {
    if (ref($_[0])) {
      $_[1] = Data::Dumper->Dump([$_[0]],[$_[1]]);
      shift @_;
    }
    if ($n eq 'display') {
      my $storepos=0;
      if ($v==1) {
        &class(@_);
      } elsif ($v==2) {
        &subclass(@_);
      } else {
        &outDisplay(@_);
      }
    } elsif ($n eq 'result') {
      &newline();
      print @_,"\n";
    } elsif ($n eq 'out') {
      $outContext->{'outString'}.=join('',@_);
    } elsif ($n eq 'err' or $n eq 'dev') {
      &newline();
      print color 'red' if ($outContext->{'color'});
      print @_,"\n";
      print color 'reset' if ($outContext->{'color'});
    } else {
      &newline();
      print $n,':',@_,"\n";
    }
  }
}
sub outIdle() {
  my $out=$outContext->{'outString'};
  my @fulllines=();
  while ($out=~m|^([^\n]*\n)(.*)$|gs) {
    push @fulllines,$1;
    $out=$2;
  }
  my $alert=0;
  if (length($out) and
      defined($outContext->{'outIdle'}) and
      (time-$outContext->{'outIdle'})>2) {
    push @fulllines,$out;
    $out='';
  }
  goto END unless (scalar @fulllines);
  if ($outContext->{'filter'}) {
    $outContext->{'outStringPast'}=[] unless defined($outContext->{'outStringPast'});
    my $past=$outContext->{'outStringPast'};
    my $alert=0;
    foreach my $line (@fulllines) {
      push $past,$line;
      if ($line=~/^([^:]*):[0-9]+: (.*)\.$/) {
        $alert="Error \"$2\" in file $1";
      }
    }
    if ($alert) {
      $outContext->{'filter'}=0;
      @fulllines=(@$past);
    } else {
      goto END;
    }
  }
  &newline();
  print color 'blue' if ($outContext->{'color'});
  foreach my $line (@fulllines) {
    print $line;
  }
  print color 'reset' if ($outContext->{'color'});
 END:
  if (length($out)) {
    $outContext->{'outIdle'}=time unless defined($outContext->{'outIdle'});
  } else {
    delete $outContext->{'outIdle'};
  }
  $outContext->{'outString'}=$out;
}
sub outFinish() {
  $outContext->{'outString'}.="\n" if length $outContext->{'outString'}>0;
  &outIdle();
}

# ARGV
sub initOptions {
  $optionContext={};
  $optionContext->{'includedFiles'}={};
  $optionContext->{'verbosity'}=$outContext->{'verbosity'};
  $optionContext->{'availableHelps'}={
                                      'help' => 1,
                                      'nroff' => 1,
                                      'man' => 1,
                                      'usage' => 1,
                                     };
  $optionContext->{'availableActions'}={
                                        'in' => 1,
                                        'out' => 1,
                                        'junk' => 1,
                                        'ignore' => 1,
                                        'clean' => 1,
                                        'build' => 1,
                                        'cli' => 1,
                                       };
  $optionContext->{'availableVariants'}={
                                         'pdflatex' => 1,
                                         'pdftex' => 1,
                                         'dvips' => 1,
                                         'xelatex' => 1,
                                        };
  $optionContext->{'initialOptions'}={
                                      '--help-action' => 1,
                                      '--file' => 1,
                                      '--action' => 1,
                                      '--filter' => 1,
                                      '--global' => 1,
                                     };
  $optionContext->{'standardTargets'}
    = {
       'all' => [ '--all' ],
       '--help' => [ '--help-action', 'help' ],
       '-h' => [ '--help-action', 'help' ],
       '--usage' => [ '--help-action', 'usage' ],
       'help' => [ '--help-action', 'man' ],
       '--man' => [ '--help-action', 'man' ],
       '--nroff' => [ '--help-action', 'nroff' ],
       '--clean' => [ '--action','clean' ],
       '--build' => [ '--action','build' ],
       '--index' => [  '--index-input-suffix', '.idx' , '--index-output-suffix', '.ind' ],
      };
  $optionContext->{'optionAliases'}={
                                     '--index-file-suffix' => '--index-input-suffix',
                                    };
  $optionContext->{'targets'}={};
  $optionContext->{'variant'}='pdflatex';
  $optionContext->{'chdir'}=1;
  $optionContext->{'filter'}=0;
  $optionContext->{'manual'}=0;
  $optionContext->{'ignoreglobal'}=1;
  $progContext->{'indexCount'}=0;
}
sub checkOptionArg {
  my ($oc,$x,$option,$available,$where)=@_;
  my $optname=$option->[$x];
  &finish(1,"Wrong option: $optname must be followed by one argument") unless defined($option->[$x+1]);
  my $cat=$option->[$x+1];
  if ( $available and !defined($optionContext->{$available}->{$cat}) ) {
    &finish(1,"Wrong option: $optname must be followed by one of \"".join('" or "',sort keys %{$optionContext->{$available}}).'"');
  }
  $oc->{$where}->{$cat}=1 if $where;
  return $cat;
}
sub mergeOptions {
  my ($in,$out)=@_;
  foreach my $key (keys %$out) {
    if (ref($out->{$key}) eq 'HASH') {
      $in->{$key}={} unless defined($in->{$key});
      &mergeOptions($in->{$key},$out->{$key});
    } else {
      $in->{$key}=$out->{$key};
    }
  }
}
sub finishIndex {               # close index group
  my ($localoptions,$index)=@_;
  my $indexcount='index'.$progContext->{'indexCount'};
  return {} unless (scalar keys %$index > 0);
  $localoptions->{'index'}->{$indexcount}=$index;
  $progContext->{'indexCount'}++;
  return {};
}
sub parseOptions {
  my ($optionContext,$desc,$options)=@_;
  my $localOptions={};
  my $index={};
  my @option=@$options;
  &out(1,'init',"Treating $desc options");
  &out(2,'init',$options);
  my $x=0;
  while ($x < scalar @option) {
    my $arg=$option[$x];
    if (defined($optionContext->{'optionAliases'}->{$arg})) {
      $arg=$optionContext->{'optionAliases'}->{$arg};
    }
    if (defined($optionContext->{'nofiles'}) and defined($optionContext->{'initialOptions'}->{$arg}) ) {
      &finish(1,"Wrong option: $arg is not allowed in $desc");
    }
    if ($arg eq '--') {
      $x++;
      while (defined($option[$x])) {
        $optionContext->{'includedFiles'}->{$option[$x]}=1;
        $x++;
      }
    } elsif ($arg eq '--help-action') {
      &checkOptionArg($optionContext,$x++,$options,'availableHelps','help');
      $optionContext->{'actions'}->{'help'}=1;
    } elsif ($arg eq '--action') {
      &checkOptionArg($optionContext,$x++,$options,'availableActions','actions');
    } elsif ($arg =~ '--(\w+)' and defined($optionContext->{'verbosity'}->{$1})) {
      my $cat=$1;
      my $level=$option[++$x];
      &finish(1,"Wrong option: --$cat must be followed by an integer") unless $level =~ /^[0-9]+$/;
      $optionContext->{'verbosity'}->{$cat}=$level;
    } elsif ($arg eq '--verbosity') {
      &finish(1,"Wrong option: --verbosity must be followed by two arguments") unless defined($option[$x+2]);
      my $cat=&checkOptionArg($optionContext,$x++,$options,'verbosity');
      my $level=$option[++$x];
      &finish(1,"Wrong option: --verbosity must be followed by a category and an integer") unless $level =~ /^[0-9]+$/;
      $optionContext->{'verbosity'}->{$cat}=$level;
    } elsif ($arg eq '--variant') {
      &checkOptionArg($localOptions,$x++,$options,'availableVariants');
      $localOptions->{'variant'}=$option[$x];
    } elsif ($arg eq '--no-chdir') {
      $localOptions->{'chdir'}=0;
    } elsif ($arg eq '--chdir') {
      $localOptions->{'chdir'}=1;
    } elsif ($arg eq '--no-filter') {
      $localOptions->{'filter'}=0;
    } elsif ($arg eq '--filter') {
      $localOptions->{'filter'}=1;
    } elsif ($arg eq '--manual') {
      $localOptions->{'manual'}=1;
    } elsif ($arg eq '--no-global') {
      $localOptions->{'ignoreglobal'}=1;
    } elsif ($arg eq '--global') {
      $localOptions->{'ignoreglobal'}=0;
    } elsif ($arg eq '--bibtex-file') {
      &checkOptionArg($localOptions,$x++,$options,undef,'bibfile');
    } elsif ($arg eq '--bibtex-file-suffix') {
      &checkOptionArg($localOptions,$x++,$options,undef,'bibfileSuffix');
    } elsif ($arg eq '--bibtex') {
      $localOptions->{'bibfileSuffix'}->{'.aux'}=1;
    } elsif ($arg eq '--bibtex-program') {
      my $a=&checkOptionArg(undef,$x++,$options);
      $localOptions->{'bibtexname'}=$a;
    } elsif ($arg eq '--index-style') {
      my $a=&checkOptionArg($index,$x++,$options,undef,'styleCandidate');
      $index->{'style'}={$a=>1};
      $index->{'styleIsSuffix'}=0;
    } elsif ($arg eq '--index-style-suffix') {
      my $a=&checkOptionArg($index,$x++,$options,undef,'styleCandidate');
      $index->{'style'}={$a=>1};
      $index->{'styleIsSuffix'}=1;
    } elsif ($arg eq '--index-options') {
      &checkOptionArg($index,$x++,$options,undef,'options');
    } elsif ($arg eq '--index-log') {
      my $a=&checkOptionArg($index,$x++,$options,undef,'logCandidate');
      $index->{'log'}={$a=>1};
      $index->{'logIsSuffix'}=0;
    } elsif ($arg eq '--index-log-suffix') {
      my $a=&checkOptionArg($index,$x++,$options,undef,'logCandidate');
      $index->{'log'}={$a=>1};
      $index->{'logIsSuffix'}=1;
    } elsif ($arg eq '--index-output') {
      my $a=&checkOptionArg($index,$x++,$options,undef,'outputCandidate');
      $index->{'output'}={$a=>1};
      $index->{'outputIsSuffix'}=0;
    } elsif ($arg eq '--index-output-suffix') {
      my $a=&checkOptionArg($index,$x++,$options,undef,'outputCandidate');
      $index->{'output'}={$a=>1};
      $index->{'outputIsSuffix'}=1;
    } elsif ($arg eq '--index-input') {
      $index=&finishIndex($localOptions,$index);
      &checkOptionArg($index,$x++,$options,undef,'input');
      $index->{'inputIsSuffix'}=0;
    } elsif ($arg eq '--index-input-suffix') {
      $index=&finishIndex($localOptions,$index);
      &checkOptionArg($index,$x++,$options,undef,'input');
      $index->{'inputIsSuffix'}=1;
    } elsif ($arg eq '--assume-ignore') {
      &checkOptionArg($localOptions,$x++,$options,undef,'assume-ignore');
    } elsif ($arg eq '--assume-in') {
      &checkOptionArg($localOptions,$x++,$options,undef,'assume-in');
    } elsif ($arg eq '--assume-junk') {
      &checkOptionArg($localOptions,$x++,$options,undef,'assume-junk');
    } elsif ($arg eq '--assume-out') {
      &checkOptionArg($localOptions,$x++,$options,undef,'assume-out');
    } elsif ($arg eq '--jobname-only') {
      &checkOptionArg($localOptions,$x++,$options,undef,'jobnameOnly');
    } elsif ($arg eq '--jobname') {
      my $jobname=&checkOptionArg(undef,$x++,$options);
      $index=&finishIndex($localOptions,$index);
      my $lo=$localOptions;
      $localOptions={};
      $localOptions->{'jobname'}->{$jobname}=1;
      if (defined($lo->{'jobnameOnly'})) {
        foreach my $k (keys %{$lo->{'jobnameOnly'}}) {
          $localOptions->{'jobname'}->{$k}=1;
        }
        delete $lo->{'jobnameOnly'};
      }
      if (defined($lo->{'jobname'})) {
        foreach my $k (keys %{$lo->{'jobname'}}) {
          $localOptions->{'jobname'}->{$k}=1;
        }
        delete $lo->{'jobname'};
      }
      if (defined($lo->{'jobnameLocal'})) {
        foreach my $k (keys %{$lo->{'jobnameLocal'}}) {
          $localOptions->{'jobnameLocal'}->{$k}=&clone($lo->{'jobnameLocal'}->{$k});
          delete $lo->{$k};
        }
        delete $lo->{'jobnameLocal'};
      }
      $localOptions->{'jobnameLocal'}->{$jobname}=$lo;
    } elsif (
             defined($optionContext->{'targets'}->{$arg}) or
             defined($optionContext->{'standardTargets'}->{$arg}) or
             $arg eq '--target'
            ) {
      if ($arg eq '--target') {
        $arg=&checkOptionArg($optionContext,$x++,$options,'targets');
      }
      &parseTarget($optionContext,$arg);
    } else {
      if (defined($optionContext->{'nofiles'})) {
        &finish(1,"Wrong option: unknown option $arg in $desc");
      }
      if ($arg eq '--file') {
        $arg=&checkOptionArg($optionContext,$x++,$options,undef,'includedFiles');
      }
      $index=&finishIndex($localOptions,$index);
      $optionContext->{'includedFiles'}->{$arg}=1;
      $optionContext->{'sourceLocal'}->{$arg}=&clone($localOptions);
      $localOptions={};
    }
    $x++;
  }
  $index=&finishIndex($localOptions,$index);
  if (scalar keys $localOptions > 0) {
    &out(2,'init',"Merging remaining local options");
    &mergeOptions($optionContext,$localOptions);
  }
}
sub parseTarget {
  my $optionContext=shift @_;
  my $target=shift @_;
  if (ref($optionContext->{'targets'}->{$target}) ne 'ARRAY') {
    my $targetOptions=[];
    if (defined($optionContext->{'standardTargets'}->{$target})) {
      $targetOptions=$optionContext->{'standardTargets'}->{$target};
    } else {
      my $targetName='plan/targets/'.$target;
      open TARGET,$targetName or &finish(1,"Unable to open $targetName");
      while (my $l=<TARGET>) {
        chomp $l;
        push $targetOptions,map {&decodeGoalLine($_)} split(/ /,$l);
      }
      close TARGET;
      $optionContext->{'targets'}->{$target}=$targetOptions;
    }
    &parseOptions($optionContext,'target '.$target,$targetOptions);
  } else {
    &out(2,'init',"Skipping $target because it was already invoked");
  }
}
sub parseARGV {
  &initOptions();
  &parseOptions($optionContext,'command line',[@ARGV]);
  if (scalar keys %{$optionContext->{'actions'}} == 0) {
    $optionContext->{'actions'}->{'build'}=1;
  }
  &out(3,'init','At the end of the command line');
  &out(3,'init',$optionContext);
  if (scalar keys %{$optionContext->{'includedFiles'}} == 0 and
      !defined($optionContext->{'actions'}->{'help'})) {
    &finish(3,"compile-latex needs a source file");
  }
}

# Help
sub usage {

=pod

=head2 HELP OPTIONS

B<--help>|B<-h> Generate this help (long version).

B<--usage> Generate this help (short version).

B<--man> Generate man page.

B<--nroff> Generate man page in NROFF format.

B<--help-action usage|help|man|nroff> One of the above.

The built-in target C<help> is equivalent to option B<--man>.

=cut

  ### usage ($option)
  ### Provides all kinds of textual help and stops
  my ($options)=@_;
  my $option=(sort keys %$options)[0];
  &out(1,'init',"Entering help with option $option");
  if (!defined($option)) {
    $option='usage';
  }
  my $cmd='pod2txt';
  my $release="0.6";
  my ($a,$out,$b);
  if ($option eq 'man'||$option eq 'nroff') {
    ($a,$out,$b)=@{&executeCommand('','pod2man',"$0",'--center','User commands','--release',$release)};
    if ($a==0 && $option eq 'man') {
      ($a,$out,$b)=@{&executeCommand($out,'nroff','-man','-')};
    }
  } else {
    ($a,$out,$b)=@{&executeCommand('','pod2text',"$0")};
  }
  if ($option eq 'usage') {
    $out =~ s/\nPRINCIPLE.*$//s;
  }
  if ($option ne 'man') {
    print $out;
  } else {
    open FILE,"|pager";
    print FILE $out;
    close FILE;
  }
  &finish(0);
}

# Command execution with stderr filtering
sub REAPER {
  my $child;
  while (($child = waitpid(-1, WNOHANG)) > 0) {
    &out(1,'exec',"Caught $child dying with status $?");
    $execContext->{'status'}->{$child} = $?;
    if (!defined($execContext->{'children'}->{$child})) {
      $execContext->{'unborn'}->{$child} = 1;
    } else {
      delete $execContext->{'children'}->{$child};
    }
  }
  $SIG{CHLD} = \&REAPER;        # still loathe SysV
}
sub setupCommand {
  &out(1,'init','Initialize harness for command executions');
  select(STDERR); $| = 1;
  select(STDOUT); $| = 1;
  $execContext={
                'status' => {},
                'children' => {},
                'unborn' => {},
               };
  $SIG{CHLD} = \&REAPER;
  $SIG{INT} = sub {
    foreach my $pid (keys %{$execContext->{'children'}}) {
      kill 2,$pid;
    }
  }
}
sub errorCommand {
  my $lines=$_[0];
  foreach my $line (split /\n/m,$lines) {
    chomp $line;
    if ($line =~ $outContext->{'specialprefix'}) {
      my $part=$1;
      if ($part =~ /^done:(.*)$/) {
        $part=$1;
        push $execContext->{'partiallyDone'},$part;
        next;
      }
      &out(3,'display',$part);
    } else {
      &out(1,'err',$line);
    }
  }
}
sub execCommand {
  my $line;
  &out(1,'exec','Executing '.join(' ',@_));
  my $out = gensym;
  my $err = gensym;
  $execContext->{'partiallyDone'}=[];
  my $savedstdin=dup(0);
  my $pid = open3("<&".$savedstdin,$out, $err, @_);
  die "Could not fork: $!" unless defined($pid);
  return 1 if (!$pid);
  &out(1,'exec',"Forked pid $pid");
  $execContext->{'children'}->{$pid}=1;
  my $sel= new IO::Select;
  $sel->add($err,$out);
  if (defined($execContext->{'unborn'}->{$pid})) {
    delete $execContext->{'unborn'}->{$pid};
    delete $execContext->{'children'}->{$pid};
  }
  while (scalar keys %{$execContext->{'children'}} > 0) {
    foreach my $readhandle ($sel->can_read(.5)) {
      my $length=sysread $readhandle,$line,4096;
      if ($length) {
        if (fileno($readhandle)==fileno($err)) {
          &errorCommand($line);
        } else {
          &out(1,'out',$line);
        }
      }
    }
    &outIdle();
  }
  &outFinish();
  &out(3,'exec',$execContext);
  my $status=0+$execContext->{'status'}->{$pid};
  delete $execContext->{'status'}->{$pid};
  &out(1,'exec',"Command finished with status $status");
  delete $execContext->{'outerr'};
  return $status;
}
sub executeCommand {
  # a much simpler version for the manual page
  my $in=shift @_;
  &out(1,'exec',join(' ',@_));
  my $outputtext='';
  my $errtext='';
  my $handler=undef;
  $handler=$SIG{CHLD} if defined($SIG{CHLD});
  $SIG{CHLD} = sub {} ;
  my $pid=open3(\*INPUT, \*OUTPUT, \*ERR,@_);
  print INPUT $in if $in;
  close(INPUT);
  my $done=0;
  do {
    while (<OUTPUT>) {
      $outputtext.=$_;
    }
    while (<ERR>) {
      $errtext.=$_;
    }
    $done=waitpid($pid,WNOHANG);
  } until ($done>0);
  $SIG{CHLD}=$handler if $handler;
  return [ $?, $outputtext, $errtext ];
}

# Disk Input/Output
sub fingerprintDir {
  my ($f)=@_;
  opendir (my $dh, $f) || return "$f:unreadable";
  my $hashing = Digest::MD5->new;
  my @files=();
  while (my $file=readdir $dh) {
    next if $file =~ /^\./;
    push @files,$f.'/'.$file;
  }
  closedir $dh;
  @files=sort @files;
  my $data;
  foreach my $f (@files) {
    if (-d $f) {
      $data=&fingerprintDir($f);
    } else {
      my ($d,$i,$m,$nl,$u,$g,$rd,$s,$at,$mt,$ct,$bs,$bl) = stat($f);
      $s='unk' unless defined($s);
      $data="$f:$s";
    }
    $hashing->add($data);
  }
  return $hashing->hexdigest;
}
sub fingerprint {
  my ($f)=@_;
  my $hash='0';
  return '0' unless (-e $f);
  return &fingerprintDir($f) if (-d $f);
  open FILE,"$f" or return "$f:0";
  my $hashing = Digest::MD5->new;
  $hashing->addfile(*FILE);
  $hash=$hashing->hexdigest;
  close FILE;
  return $hash;
}
sub fingerprintArray {
  # example: $rule, 'in'
  my $context=shift @_;
  my $key=shift @_;
  my @a=sort keys %{$context->{$key}};
  return 'none' unless (scalar @a);
  my $hash='';
  my $hashing = Digest::MD5->new;
  my $fp;
  foreach my $file (@a) {
    $fp=&fingerprint($file);
    $hashing->add($file.$fp);
    $hash.=$file.':'.$fp;
  }
  foreach my $supp (@_) {
    $hashing->add(join('|',@$supp));
    $hash.=join('|',@$supp);
  }
  return $hash;
  return $hashing->hexdigest;
}

sub encodeGoalLine {
  join('',
       map { ($_ > 127 || $_ < 32 || $_ == 37 || $_ == 59 || $_ == 58 )?
               sprintf("%%(%d)", $_) :
                 chr($_)
               } unpack("W*", &decode_utf8($_[0]))); # unpack Unicode characters
}
sub decodeGoalLine {
  my $a=$_[0];
  $a =~ s/%\(([0-9]+)\)/chr($1)/eg;
  return $a;
}

# latex-specific
sub collapseArrays {
  my $arrays=shift @_;
  my $in=$arrays->{'in'};
  my $out=$arrays->{'out'};
  my $junk=$arrays->{'junk'};
  my $pwd=$arrays->{'pwd'};
  my $home=$arrays->{'home'};
  # something that is both in and out is junk
  my $skey;
  my $waslocal={};
  my $wasnotlocal={};
  my $realhome=realpath($home);
  my $eliminateglobal=$optionContext->{'ignoreglobal'};
  foreach my $array ($in,$out,$junk) {
    foreach my $key (keys %$array) {
      $skey=$key;
      if ($skey=~m|^/|) {
        $skey=realpath($skey);
      } else {
        $skey=realpath($pwd.'/'.$skey);
        $waslocal->{$skey}=1;
      }
      if ($key ne $skey) {
        delete $array->{$key};
        $array->{$skey}=1;
      }
    }
  }
  foreach my $key (keys %$in) {
    if (defined($out->{$key})) {
      $junk->{$key}=1;
    }
  }
  foreach my $key (keys %$junk) {
    delete $in->{$key};
    delete $out->{$key};
  }
  foreach my $array ($in,$out,$junk) {
    foreach my $key (keys %$array) {
      if (defined $waslocal->{$key}) {
        delete $array->{$key};
        $skey=File::Spec->abs2rel($key,$realhome);
        $array->{$skey}=1;
      } elsif ($eliminateglobal) {
        delete $array->{$key};
      }
    }
  }
}

sub processSourcefile {
  my ($filename)=@_;
  $filename.='.tex' unless $filename =~ /\.tex$/;
  goto FILENAME_FOUND if -r $filename;
  $filename=~ s/\.tex$//;
  goto FILENAME_FOUND if -r $filename;
  &finish(1,"Could not read neither $filename nor $filename.tex");
 FILENAME_FOUND:
  &out(1,'plan',"$_[0] is sourcefile $filename");
  &out(2,'init',"Searching $filename for options");
  $sourceContext=&clone($optionContext);
  open FILE,$filename or die "Could not open $filename";
  my @infile=();
  while (<FILE>) {
    chomp;
    next unless /^%+\s*compile-latex\s+option(s?)\s*:\s*(.*)\s*$/;
    if ($1) {
      push @infile,map {&decodeGoalLine($_)} split(/ /,$2)
    } else {
      push @infile,&decodeGoalLine($2);
    }
  }
  close FILE;
  $sourceContext->{'nofiles'}=1;
  $sourceContext->{'texsource'}=$filename;
  $sourceContext->{'fulltexsource'}=$filename;
  if (defined($sourceContext->{'sourceLocal'})) {
    my $lo=&clone($sourceContext->{'sourceLocal'});
    delete($sourceContext->{'sourceLocal'});
    if (defined($lo->{$_[0]})) {
      &mergeOptions($sourceContext,$lo->{$_[0]});
    }
  }
  &parseOptions($sourceContext,'sourcefile '.$filename,\@infile);
  &out(3,'init','At the end of the source file');
  &out(3,'init',$sourceContext);
  my $defaultjobname=$filename;
  my $homedir = realpath(getcwd);
  $sourceContext->{'pwd'}=$homedir;
  $sourceContext->{'home'}=$homedir;
  if ($sourceContext->{'chdir'}) {
    my $targetdir = realpath($filename);
    my ($volume,$directories,$file) = File::Spec->splitpath( $targetdir );
    my $dir = File::Spec->catdir($volume,$directories);
    &out(1,'exec',"chdir to $dir");
    chdir $dir;
    $sourceContext->{'texsource'}=$file;
    $sourceContext->{'pwd'}=$dir;
    $defaultjobname=$file;
  }
  $defaultjobname=~s/\.tex$//g;
  my $metadir='.compile-latex';
  if ( (-e $metadir or -l $metadir) and ! -d $metadir) {
    &finish(1,"Could not create metadata directory $metadir: something exists");
  } elsif (! (-e $metadir and -l $metadir)) {
    make_path($metadir);
    &finish(1,"Could not create metadata directory $metadir") unless -d $metadir;
  }
  $sourceContext->{'metadir'}=$metadir;
  $sourceContext->{'defaultjobname'}=$defaultjobname;
  if (scalar keys %{$sourceContext->{'jobname'}} == 0) {
    $sourceContext->{'jobname'}->{$defaultjobname}=1;
  }
  $sourceContext->{'class'}="Compiling $filename";
  foreach my $jobname (sort keys %{$sourceContext->{'jobname'}}) {
    processJob($jobname);
  }
  if ($sourceContext->{'chdir'}) {
    &out(1,'exec',"chdir to $homedir");
    chdir $homedir;
  }
}

sub processJob {
  my ($jobname)=@_;
  my $plans={};
  foreach my $plankey ('pre','run','post') {
    $plans->{$plankey}={'order'=>[]};
  }
  my $env=&clone($sourceContext);
  if (defined($env->{'jobnameLocal'})) {
    my $lo=&clone($env->{'jobnameLocal'});
    delete($env->{'jobnameLocal'});
    if (defined($lo->{$_[0]})) {
      &mergeOptions($env,$lo->{$_[0]});
    }
  }
  &out(3,'init','At the end of the job selection');
  &out(3,'init',$env);
  if (scalar keys %{$env->{'jobnameOnly'}}) {
    if (!defined($env->{'jobnameOnly'}->{$jobname})) {
      &out(1,'init',"Skipping $jobname");
      return;
    }
  }
  &parseOptions($env,'non-manual',['--index']) unless ($env->{'manual'});
  $env->{'meta'}=File::Spec->catdir($env->{'metadir'},$jobname);
  make_path($env->{'meta'});
  # remove dirpart
  $env->{'stem'}=$jobname;
  # create metadatadir
  &buildLatex($env,$plans);
  &buildIndices($env,$plans);
  &buildBibs($env,$plans);
  foreach my $plankey ('pre','run','post') {
    foreach my $elem (@{$plans->{$plankey}->{'order'}}) {
      &loadPlanPart($env,$plans->{$plankey},$elem);
    }
  }
  if (defined($env->{'actions'}->{'build'})) {
    my $done=0;
    my $class='Processing '.$env->{'texsource'};
    $class.=" as $jobname" if $jobname ne $env->{'defaultjobname'};
    &out(1,'display',$class);
    my $counter=1;
    foreach my $plankey ('pre','run','post') {
      $done=0;
      while (!$done) {
        ($done,$counter)=&runPlanParts($env,$plans->{$plankey},"cycle $counter",$counter);
      }
    }
    &out(3,'display',"done");
  }
  if (
      defined($env->{'actions'}->{'out'}) or
      defined($env->{'actions'}->{'in'}) or
      defined($env->{'actions'}->{'junk'}) or
      defined($env->{'actions'}->{'ignore'})
     ) {
    my $array={'in'=>{},'out'=>{},'junk'=>{},'ignore'=>{}};
    foreach my $plankey ('pre','run','post') {
      foreach my $elem (@{$plans->{$plankey}->{'order'}}) {
        foreach my $i ('in','out','junk','ignore') {
          &addHashInPlace($array->{$i},$plans->{$plankey}->{$elem}->{$i});
        }
      }
    }
    $array->{'home'}=$env->{'home'};
    $array->{'pwd'}=$env->{'pwd'};
    &collapseArrays($array);
    foreach my $i ('in','out','junk','ignore') {
      if (defined($env->{'actions'}->{$i})) {
        &addHashInPlace($progContext->{$i},$array->{$i});
      }
    }
  }
  if (defined($env->{'actions'}->{'cli'})) {
    my @args=('PROGNAME');
    foreach my $from ('in','junk','out','ignore') {
      foreach my $key (sort keys %{$plans->{'run'}->{'latex'}->{$from}}) {
        push @args,"--assume-$from",&encodeGoalLine($key) if ($from ne 'in' or $key ne $env->{'texsource'}) and ($from ne 'out' or $key ne $env->{'outputLatex'});
      }
    }
    if ($env->{'manual'}) {
      my $any=1;
      my $stem=$env->{'stem'};
      foreach my $key (keys %{$plans->{'run'}->{'updateBibs'}->{'allbibs'}}) {
        if ($any) {$any=0;push @args,'--manual';};
        if ($key eq "$stem.aux") {
          push @args,'--bibtex';
        } elsif ($key=~/^$stem(.*)$/) {
          push @args,'--bibtex-file-suffix',&encodeGoalLine($1);
        } else {
          push @args,'--bibtex-file',&encodeGoalLine($key);
        }
      }
    }
    foreach my $key (keys %{$plans->{'run'}->{'allIndexes'}}) {
      my @indexargs=@{$plans->{'run'}->{$key}->{'cli'}};
      push @args,@indexargs if scalar @indexargs>1;
      push @args,@indexargs if scalar @indexargs == 1 and $env->{'manual'};
    }
    push @args,'--jobname',&encodeGoalLine($jobname) if ($jobname ne $env->{'defaultjobname'});
    push @args,'--jobname-only',&encodeGoalLine($jobname) if ($jobname ne $env->{'defaultjobname'});
    push @args,$env->{'fulltexsource'};
    $progContext->{'cli'}->{&encodeGoalLine($env->{'fulltexsource'}).';'.$jobname.';'.&encodeGoalLine($jobname)}=join(' ',@args);
  }
}

sub runPlanParts {
  my ($env,$plan,$subclass,$counter)=@_;
  my $alldone=1;
  my $order=$plan->{'order'};
  $counter++ if scalar @$order;
  foreach my $elem (@$order) {
    my $part=$plan->{$elem};
    if (!exists($part->{'always'})) {
      my $fingerprintIn=&fingerprintArray($part,'in');
      my $fingerprintOut=&fingerprintArray($part,'out');
      my $fingerprintJunk=&fingerprintArray($part,'junk');
      my $fp="$fingerprintIn $fingerprintOut $fingerprintJunk";
      my $oldfp=join(' ',$part->{'fingerprintIn'},$part->{'fingerprintOut'},$part->{'fingerprintJunk'});
      if ($oldfp eq $fp) {
        &out(1,'plan',"Skipping $elem");
        next;
      } else {
        &out(2,'plan',"Not skipping $elem because\n$fp is not\n$oldfp");
      }
      $part->{'fingerprintIn'}=$fingerprintIn;
      $part->{'fingerprintJunk'}=$fingerprintJunk;
    }
    &out(1,'plan',"Trying $elem");
    my $return; # 1 should be returned if nothing was done, 0 if something was done
    {
      no strict 'refs';
      $return=&{$part->{'callback'}}($env,$plan,$part,$subclass);
    }
    &savePlanPart($env,$plan,$elem);
    $alldone=0 unless $return;
    $subclass=undef unless $alldone;
  }
  return ($alldone,$counter);
}

sub buildLatex {
  my ($env,$plans)=@_;
  my $runPlan=$plans->{'run'};
  my $p=$env->{'variant'};
  if ($p eq 'pdflatex') {
    $env->{'latexname'}='pdflatex';
    $env->{'dviname'}='';
    $env->{'outputFile'}=$env->{'stem'}.'.pdf';
  } elsif ($p eq 'dvips' or $p eq 'dvips+latex') {
    $env->{'latexname'}='latex';
    $env->{'dviname'}='dvips';
    $env->{'inputDviFile'}=$env->{'stem'}.'.dvi';
    $env->{'outputFile'}=$env->{'stem'}.'.ps';
  } elsif ($p eq 'dvipdf' or $p eq 'dvipdf+latex') {
    $env->{'latexname'}='latex';
    $env->{'dviname'}='dvipdf';
    $env->{'inputDviFile'}=$env->{'stem'}.'.dvi';
    $env->{'outputFile'}=$env->{'stem'}.'.pdf';
  } elsif ($p eq 'dvipdfm' or $p eq 'dvipdfm+latex') {
    $env->{'latexname'}='latex';
    $env->{'dviname'}='dvipdfm';
    $env->{'inputDviFile'}=$env->{'stem'}.'.dvi';
    $env->{'outputFile'}=$env->{'stem'}.'.pdf';
  } elsif ($p eq 'xelatex') {
    $env->{'latexname'}='xelatex --no-pdf';
    $env->{'dviname'}='xdvipdfmx';
    $env->{'inputDviFile'}=$env->{'stem'}.'.xdv';
    $env->{'outputFile'}=$env->{'stem'}.'.pdf';
  } elsif ($p eq 'lualatex') {
    $env->{'latexname'}='lualatex';
    $env->{'dviname'}='';
    $env->{'outputFile'}=$env->{'stem'}.'.pdf';
  } else {
    # unknown chain
    &finish(1,"Unknown latex chain: $p");
  }
  my $latex={
             'in' => { $env->{'texsource'} => 1},
             'callback' => 'latexCallback',
             'command' => [split(/ /,$env->{'latexname'}),
                           '--file-line-error',
                           '--recorder',
                           '--jobname',$env->{'stem'},
                           $env->{'texsource'}
                          ],
            };
  if (defined $env->{'inputDviFile'}) {
    $latex->{'out'}={$env->{'inputDviFile'}=>1};
    $env->{'outputLatex'}=$env->{'inputDviFile'};
  } else {
    $latex->{'out'}={$env->{'outputFile'}=>1};
    $env->{'outputLatex'}=$env->{'outputFile'};
  }
  $latex->{'ignore'}->{$env->{'stem'}.".log"}=1;
  $latex->{'ignore'}->{$env->{'stem'}.".fls"}=1;
  foreach my $from ('in','out','junk','ignore') {
    if (defined $env->{"assume-$from"}) {
      my $fromH=$env->{"assume-$from"};
      foreach my $key (keys %$fromH) {
        $latex->{$from}->{$key}=1;
      }
    }
  }
  foreach my $key (keys %{$latex->{'ignore'}}) {
    foreach my $xx ('in','out','junk') {
      delete $latex->{$xx}->{$key} if exists $latex->{$xx}->{$key};
    }
  }
  $runPlan->{'latex'}=$latex;
  push $runPlan->{'order'},'latex';
}
sub buildIndices {
  my ($env,$plans)=@_;
  my $runPlan=$plans->{'run'};
  foreach my $key (keys %{$env->{'index'}}) {
    &buildIndex($env,$runPlan,$env->{'index'}->{$key});
  }
}
sub buildIndex {
  my ($env,$runPlan,$index)=@_;
  my $style=0;
  my ($in,$out,$sty,$log);
  my @args=();
  if (!defined($index->{'input'})) {
    $index->{'input'}={'.idx'=>1};
    $index->{'inputIsSuffix'}=1;
  }
  if (!defined($index->{'output'})) {
    $index->{'output'}={'.ind'=>1};
    $index->{'outputIsSuffix'}=1;
  }
  if (!defined($index->{'log'})) {
    $index->{'log'}={'.ilg'=>1};
    $index->{'logIsSuffix'}=1;
  }
  if (defined($index->{'style'})) {
    $style=1;
  } else {
    $index->{'style'}={'.mst'=>1};
    $index->{'styleIsSuffix'}=1;
  }
  if ($index->{'inputIsSuffix'}) {
    my $key=(keys $index->{'input'})[0];
    $in=$env->{'stem'}.$key;
    push @args,'--index-input-suffix',$key;
  } else {
    my $key=(keys $index->{'input'})[0];
    $in=&decodeGoalLine($key);
    push @args,'--index-input',$key;
  }
  if ($index->{'logIsSuffix'}) {
    my $key=(keys $index->{'log'})[0];
    $log=$env->{'stem'}.$key;
    push @args,'--index-log-suffix',$key unless $key eq '.ilg';
  } else {
    my $key=(keys $index->{'log'})[0];
    $log=&decodeGoalLine($key);
    push @args,'--index-log',$key;
  }
  if ($index->{'outputIsSuffix'}) {
    my $key=(keys $index->{'output'})[0];
    $out=$env->{'stem'}.$key;
    push @args,'--index-output-suffix',$key unless $key eq '.ind';
  } else {
    my $key=(keys $index->{'output'})[0];
    $out=&decodeGoalLine($key);
    push @args,'--index-output',$key;
  }
  if ($index->{'styleIsSuffix'}) {
    my $key=(keys $index->{'style'})[0];
    $sty=$env->{'stem'}.$key;
    push @args,'--index-style-suffix',$key unless $key eq '.mst';
  } else {
    my $key=(keys $index->{'style'})[0];
    $sty=&decodeGoalLine($key);
    push @args,'--index-style',$key;
  }
  if (join(' ',@args) eq '--index-input-suffix .idx') {
    @args=('--index');
  }
  my $action={
              'in' => { $in=>1,$sty=>1 },
              'out' => { $out=>1 },
              'ignore' => {$log => 1},
              'junk' => {},
              'callback' => 'indexCallback',
              'style' => $style,
              'styleFile'=> $sty,
              'sourceFile'=> $in,
              'command' => ['makeindex','-o',$out],
              'cmdname' => 'makeindex '.(keys $index->{'input'})[0].'=>'.(keys $index->{'output'})[0],
              'cli' => \@args,
             };
  push $action->{'command'},'-s',$sty if ($style);
  push $action->{'command'},$in;
  my $xkey="makeindex $out";
  $runPlan->{$xkey}=$action;
  push $runPlan->{'order'},$xkey;
  $runPlan->{'allIndexes'}->{$xkey}=1;
}
sub buildBibs {
  my ($env,$plans)=@_;
  my $runPlan=$plans->{'run'};
  if ($env->{'manual'} != 1) {
    push $runPlan->{'order'},'autoBib';
    $runPlan->{'autoBib'}={
                           'command'=> ['autoBib'],
                           'always' => 1,
                           'callback' => 'autoBibCallback'
                          };
  }
  push $runPlan->{'order'},'updateBibs';
  $runPlan->{'updateBibs'}={
                            'command'=> ['updateBibs'],
                            'always' => 1,
                            'callback' => 'updateBibsCallback',
                            'allbibs' => {},
                           };
  foreach my $key (keys %{$env->{'bibfile'}}) {
    &buildBib($env,$runPlan,$key);
  }
  foreach my $key (keys %{$env->{'bibfileSuffix'}}) {
    &buildBib($env,$runPlan,$env->{'stem'}.$key);
  }
}
sub buildBib {
  my ($env,$runPlan,$bib)=@_;
  my $stem=$bib;
  $stem=~s/\.aux$//g;
  $bib="$stem.aux";
  my $key='bib'.$bib;
  my $fakebib=$bib;
  if ($stem eq $env->{'stem'}) {
    $fakebib=File::Spec->catfile($env->{'meta'},"auxbib");
  }
  my $bibpart={
               'in' => {$fakebib => 1},
               'out'=> {"$stem.bbl"=>1},
               'ignore' => {"$stem.blg"=>1},
               'junk' => {},
               'callback' => 'bibCallback',
              };
  $env->{'bibtexname'}="bibtex" unless (exists $env->{'bibtexname'});
  my $command=[map {&decodeGoalLine($_)} split(/ /,$env->{'bibtexname'})];
  push $command,'-terse' if ($env->{'filter'});
  push $command,$stem;
  $bibpart->{'command'}=$command;
  $bibpart->{'cmdname'}="bibtex $stem";
  if (-f $bib) {
    my $copy=0;
    if ($fakebib ne $bib) {
      $copy=1;
      open FINGERFILE,'>',$fakebib;
    }
    if (!open FILE,$bib) {
      &out(1,'err',"Could not open $bib for reading");
      goto skip;
    }
    my $bibdata={};
    my $bibstyle=undef;
    while (<FILE>) {
      if ($copy and /^\\([a-z]+)/) {
        print FINGERFILE $_ if ($1 eq 'bibdata' or $1 eq 'citation' or $1 eq 'bibstyle');
      }
      if (/^\\bibstyle{(.*)}$/) {
        my $bibdataf=$1;
        $bibdataf=~s/\.bst$//g;
        $bibstyle=$bibdataf;
      } elsif (/^\\bibdata{(.*)}$/) {
        my $bibdataf=$1;
        $bibdataf=~s/\.bib$//g;
        $bibdata->{$bibdataf}=1;
      }
    }
    close FINGERFILE if ($copy);
    goto skip unless defined($bibstyle);
    goto skip unless scalar keys %$bibdata;
    my $bibstylef=&kpsewhich('bibtex',$bibstyle.'.bst');
    $bibpart->{'in'}->{$bibstylef}=1 if defined $bibstylef;
    foreach my $bkey (keys %$bibdata) {
      my $bibdataf=&kpsewhich('bibtex',$bkey.'.bib');
      $bibpart->{'in'}->{$bibdataf}=1 if $bibdataf;
    }
    $bibpart->{'home'}=$bibpart->{'pwd'}=getcwd();
    &collapseArrays($bibpart);
  }
 skip:
  push $runPlan->{'order'},$key unless exists $runPlan->{$key};
  $runPlan->{$key}=$bibpart;
  $runPlan->{'updateBibs'}->{'allbibs'}->{$bib}=1;
  &loadPlanPart($env,$runPlan,$key);
}
sub kpsewhich {
  my ($progname,$file)=@_;
  my ($a,$out,$b)=@{&executeCommand('','kpsewhich','-progname',$progname,$file)};
  chomp $out;
  return $out if ($a == 0);
  return undef;
}
sub updateBibsCallback {
  my ($env,$plan,$part,$subclass)=@_;
  foreach my $key (keys %{$part->{'allbibs'}}) {
    &buildBib($env,$plan,$key,1);
  }
  return 1;
}
sub autoBibCallback {
  my ($env,$plan,$part,$subclass)=@_;
  my @outlatexfiles=(keys %{$plan->{'latex'}->{'out'}},keys %{$plan->{'latex'}->{'junk'}});
  my $bibfiles={};
  foreach my $file (@outlatexfiles) {
    open FILE,$file or next;
    my $status=0;
    while (<FILE>) {
      if (/^\\bibstyle/ or /^\\bibcite/ or /^\\bibdata/ or /^\\citation/) {
        $status=$status|2;
      } else {
        $status=$status|1;
      }
    }
    close FILE;
    $bibfiles->{$file}=1 if $status == 2;
    $bibfiles->{$file}=1 if $file eq $env->{'stem'}.'.aux' and $status == 3;
  }
  foreach my $key (keys %$bibfiles) {
    $plan->{'updateBibs'}->{'allbibs'}->{$key}=1;
  }
  return 1;
}
sub bibCallback {
  my ($env,$plan,$part,$subclass)=@_;
  &out(2,'display',$subclass) if defined($subclass);
  &out(3,'display',$part->{'cmdname'});
  my @command=@{$part->{'command'}};
  my $status=&execCommand(@command);
  if ($status) {
    &finish(1,$env->{'cmdname'}." failed with status $status");
  }
  $part->{'fingerprintOut'}=&fingerprintArray($part,'out');
  return 0;
}
sub indexCallback {
  my ($env,$plan,$part,$subclass)=@_;
  return 1 unless -f ($part->{'sourceFile'});
  &out(2,'display',$subclass) if defined($subclass);
  &out(3,'display',$part->{'cmdname'});
  if ($part->{'style'} and ! -f $part->{'styleFile'}) {
    &finish(1,"Mandatory style file for makeindex $part->{'styleFile'} not found");
  }
  my @command=@{$part->{'command'}};
  if ($env->{'filter'}) {
    splice @command,-1,0,'-q';
  }
  my $status=&execCommand(@command);
  if ($status) {
    &finish(1,$env->{'cmdname'}." failed with status $status");
  }
  $part->{'fingerprintOut'}=&fingerprintArray($part,'out');
  return 0;
}
sub latexCallback {
  my ($env,$plan,$part,$subclass)=@_;
  &out(2,'display',$subclass) if defined($subclass);
  &out(3,'display',$env->{'latexname'});
  $outContext->{'filter'}=$env->{'filter'};
  my $status=&execCommand(@{$part->{'command'}});
  if ($status) {
    &finish(1,"TeX failed with status $status");
  }
  # Reevaluate inputs and outputs
  my ($out,$in,$junk,$pwd)=({},{},{},undef);
  open FILE,$env->{'stem'}.".fls" or die 'Input/output list was not generated!';
  my $line;
  while ($line=<FILE>) {
    chomp $line; 
    if ($line=~/^INPUT (.*)$/) {
      if (defined $out->{$1}) {
        $junk->{$1}=1;
      } else {
        $in->{$1}=1;
      }
    } elsif ($line=~/^OUTPUT (.*)$/) {
      if (defined $in->{$1}) {
        $junk->{$1}=1;
      } else {
        $out->{$1}=1;
      }
    } elsif ($line=~/^PWD (.*)$/) {
      $pwd=$1;
    } else {
      &out(1,'err',"Unexpected line \"$line\" in input/output list\n");
    }
  }
  close FILE;
  $out->{$env->{'stem'}.".fls"}=1;
  my $arrays={'in'=>$in,'out'=>$out,'junk'=>$junk,'pwd'=>$pwd,'home'=>$pwd};
  &collapseArrays($arrays);
  foreach my $key (keys %{$part->{'ignore'}}) {
    foreach my $x ($in,$out,$junk) {
      delete $x->{$key} if exists $x->{$key};
    }
  }
  $part->{'in'}=$in;
  $part->{'out'}=$out;
  $part->{'junk'}=$junk;
  $part->{'fingerprintOut'}=&fingerprintArray($part,'out');
  # $part->{'fingerprintIn'}=&fingerprintArray($part,'in');
  # $part->{'fingerprintJunk'}=&fingerprintArray($part,'junk');
  return 0;
}

sub savePlanPart {
  my ($env,$plan,$elem)=@_;
  my $part=$plan->{$elem};
  my $filename=File::Spec->catfile($env->{'meta'},$elem);
  open FILE,">$filename" or die "Could not save metadata to $filename";
  do {
    local $Data::Dumper::Indent=0;
    print FILE Data::Dumper->Dump([$part],['part']);
  };
  close FILE;
}
sub loadPlanPart {
  my ($env,$plan,$elem)=@_;
  my $xpart=$plan->{$elem};
  my $hashing = Digest::MD5->new;
  $hashing->add(join(' ',@{$xpart->{'command'}}));
  $xpart->{'fingerprintCmd'}=$hashing->hexdigest;
  my $filename=File::Spec->catfile($env->{'meta'},$elem);
  $xpart->{'fingerprintOut'}='new';
  $xpart->{'fingerprintJunk'}='new';
  $xpart->{'fingerprintIn'}='new';
  if (-f $filename) {
    open FILE,"$filename" or die "Could not save metadata to $filename";
    my $string=<FILE>;
    close FILE;
    my $part;
    do {
      eval $string;
    };
    foreach my $i ('out','in','junk') {
      $xpart->{$i}={} unless defined($xpart->{$i});
      &addHashInPlace($xpart->{$i},$part->{$i});
    }
    if ($part->{'fingerprintCmd'} eq $xpart->{'fingerprintCmd'}) {
      &out(1,'plan','Integrating saved fingerprints');
      $xpart->{'fingerprintOut'}=$part->{'fingerprintOut'};
      $xpart->{'fingerprintJunk'}=$part->{'fingerprintJunk'};
      $xpart->{'fingerprintIn'}=$part->{'fingerprintIn'};
    }
  }
}

# TODO
# shorthands for options
# shorthands for certain options
# autodoc
# retrofix fingerprintIn if one input was added/deleted, and we have a working cache of it?
# bibtex management
# .aux => .auxbib management

# About .auxbib
# Filter out .aux => $metadir/auxbib
# if cmdline of bibtex is bibtex $env->{'stem'}.".aux", then modify it as bibtex File::Spec->catfile($env->{'meta'},"auxbib");
