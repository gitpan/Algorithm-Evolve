#!/usr/bin/perl

use Algorithm::Evolve;
use StringEvolver;

sub callback {
	my $pop = shift;
	
	unless ($pop->generations % 50) {
		print "generations:",  $pop->generations,
		      " best_fit:",    $pop->best_fit->fitness,
		      " avg_fitness:", $pop->avg_fitness, $/;
	}
	
	$pop->suspend if $pop->generations >= 500;
}

my $pop = Algorithm::Evolve->new(
	critter_class    => StringEvolver,
	selection        => rank,
	replacement      => rank,
	parents_per_gen  => 8,
	children_per_gen => 8,
	size             => 40,
	callback         => \&callback,
#	random_seed      => shift
);

$pop->start;
