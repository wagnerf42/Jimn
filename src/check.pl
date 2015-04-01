#!/usr/bin/env perl

#this script is here to enable me to quickly check
#which file has already been reviewed and which file is not checked

use strict;
use warnings;
use File::Find;

my %statuses;
load_statuses(\%statuses);

if (@ARGV) {
	#if args are given we mark corresponding files as ok at their last revision
	$statuses{$_} = revision($_) for @ARGV;
	save_statuses(\%statuses);
} else {
	display_stats();
}

sub display_stats {
	my @python_files;
	find(sub {
			if (/\.py$/) {
				my $revision = revision($_);
				return if $revision eq '';
				if ((not defined $statuses{$File::Find::name}) or ($statuses{$File::Find::name} ne $revision)) {
					push @python_files, $File::Find::name;
				}
			}
		},
		'.');

	print "the following files lack reviews : \n";
	for my $file (@python_files) {
		print "$file --> last changed on ".revision($file);
		if (exists $statuses{$file}) {
			print " ; last review on $statuses{$file}\n";
		} else {
			print " ; never reviewed \n";
		}
	}
	return;
}

sub revision {
	my $file = shift;
	my $revision = `git log $file | head -n 1`;
	chomp($revision);
	return $revision;
}

sub load_statuses {
	my $statuses = shift;
	open(my $file, '<', 'status.txt') or die 'are you in root directory ?';
	while(my $line = <$file>) {
		die 'wrong status file' unless $line=~/^(\S+)\s=>\s(commit \S+)\s*$/;
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
