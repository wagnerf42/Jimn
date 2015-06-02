#!/usr/bin/env perl

#this script is here to enable me to quickly check
#which file has already been reviewed and which file is not checked

use strict;
use warnings;
use File::Find;

my %statuses;
load_statuses(\%statuses);

if (@ARGV) {
	if ($ARGV[0] eq '-d') {
		my $file = $ARGV[1];
		$file = $1 if $file=~/^\.\/(\S+)/;
		if (exists $statuses{$file}) {
			system("git diff $statuses{$file}:./$file ./$file");
		} else {
			die "unknown file $ARGV[1]";
		}
	} else {
		#if args are given we mark corresponding files as ok at their last revision
		my @files = map {($_=~/^\.\/(\S+)/)?$1:$_} @ARGV;
		$statuses{$_} = revision($_) for @files;
		save_statuses(\%statuses);
	}
} else {
	display_stats();
}

sub display_stats {
	my @python_files;
	find(sub {
			if (/\.py$/) {
				my $filename = $File::Find::name;
				$filename = $1 if $filename =~/^\.\/(\S+)/;
				my $revision = revision($_);
				return unless defined $revision;
				if ((not defined $statuses{$filename}) or ($statuses{$filename} ne $revision)) {
					push @python_files, $filename;
				}
			}
		},
		'.');

	print "the following files lack reviews : \n";
	my @new_files;
	my @old_files;
	for my $file (@python_files) {
		if (exists $statuses{$file}) {
			push @old_files, $file;
		} else {
			push @new_files, $file;
		}
	}

	my %old_sizes;
	for my $file (@old_files) {
		$old_sizes{$file} = `git diff $statuses{$file}:./$file ./$file | wc -l`;
		chomp($old_sizes{$file});
	}
	my @sorted_old_files = sort {$old_sizes{$a} <=> $old_sizes{$b}} @old_files;
	print "new files:\n";
	print "\t$_\n" for @new_files;
	print "\nold files:\n";
	print "\t$_ ($old_sizes{$_})\n" for @sorted_old_files;
	return;
}

sub revision {
	my $file = shift;
	my $revision = `git log -- $file | head -n 1`;
	return if not defined $revision or $revision eq '';
	die "file is $file ; error parsing $revision" unless $revision=~/commit\s+(\S+)/;
	return $1;
}

sub load_statuses {
	my $statuses = shift;
	open(my $file, '<', 'status.txt') or die 'are you in root directory ?';
	while(my $line = <$file>) {
		die 'wrong status file' unless $line=~/^(\S+)\s=>\s*(\S+)\s*$/;
		$statuses->{$1} = $2;
	}
	close($file);
	return;
}

sub save_statuses {
	my $statuses = shift;
	open(my $file, '>', 'status.txt') or die 'are you in root directory ?';
	for my $key (keys %{$statuses}) {
		print $file "$key => $statuses->{$key}\n";
	}
	close($file);
}
