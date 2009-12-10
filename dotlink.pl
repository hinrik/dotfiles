#!/usr/bin/env perl

use strict;
use warnings;

use Cwd qw<getcwd>;
use File::Path qw<rmtree>;
use Getopt::Long qw<:config auto_help bundling>;
use Pod::Usage;

our $VERSION = '0.08';

GetOptions(
    'n|dry-run' => \my $dry_run,
    'f|force'   => \my $force,
    'v|version' => sub { print "dotlink.pl version $VERSION\n"; exit },
) or pod2usage();

my $dir = defined $ARGV[0] ? $ARGV[0] : getcwd();
my $home = $ENV{HOME};

opendir my $dir_handle, $dir or die "Can't open the dir $dir: $!; aborted";

while (defined (my $file = readdir $dir_handle)) {
    next if $file =~ /^\.{1,2}$/;
    next if $file !~ /^\./;
    next if $file =~ /^\.git$/;

    if (-l "$home/$file" && stat "$home/$file") {
        next if (stat "$home/$file")[1] == (stat "$dir/$file")[1];
    }
    
    if (-e "$home/$file" || -l "$home/$file") {
        if (!$force) {
            $dry_run
                ? print "--force is off, will not overwrite '$home/$file'\n"
                : print "--force is off, not overwriting '$home/$file'\n"
            ;
            next;
        }
        
        $dry_run
            ? print "Will overwrite '$home/$file'\n"
            : print "Overwriting '$home/$file'\n"
        ;

        -d "$home/$file"
            ? do { rmtree "$home/$file" if !$dry_run }
            : do { unlink "$home/$file" if !$dry_run }
        ;
    }
    else {
        $dry_run
            ? print "Will create '$home/$file'\n"
            : print "Creating '$home/$file'\n"
        ;
    }

    symlink "$dir/$file", "$home/$file" if !$dry_run;
}

=head1 NAME

dotlink.pl - Creates symlinks in your home dir for all the dotfiles in
a specified directory.

=head1 SYNOPSIS

B<dotlink.pl> [options] [DIR]

 Options:
  -n, --dry-run    Don't actually do anything
  -f, --force      Removes already existing files if found
  -h, --help       Display this help message
  -v, --version    Display version information

DIR is the directory containing the files you want to link. Defaults to
the current directory.

=head1 AUTHOR

Hinrik E<Ouml>rn SigurE<eth>sson, hinrik.sig@gmail.com

=head1 LICENSE AND COPYRIGHT

Copyright 2008 Hinrik E<Ouml>rn SigurE<eth>sson

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
