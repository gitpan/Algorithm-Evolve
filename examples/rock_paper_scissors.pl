#!/usr/bin/perl

## this example script has POD -- check it out!

use StringEvolver alphabet => [qw/R P S/], mutation_rate => 0.05;
our @ISA = ('StringEvolver');

use lib '../lib';
use Algorithm::Evolve;

sub compare {
	my ($class, $c1, $c2)   = @_;
	my ($string1, $string2) = ($c1->value, $c2->value);
	my ($score1, $score2)   = (0, 0);

	my $offset1 = int rand length $string1;
	my $offset2 = int rand length $string1;

	$string1 =~ s/^(.{$offset1})(.+)$/$2$1/;
	$string2 =~ s/^(.{$offset2})(.+)$/$2$1/;

	for (0 .. length($string1) - 1) {
		my $char1 = substr($string1, $_, 1);
		my $char2 = substr($string2, $_, 1);

		next if $char1 eq $char2; ## tie

		if (($char1 eq 'R' && $char2 eq 'S')
		 or ($char1 eq 'S' && $char2 eq 'P')
		 or ($char1 eq 'P' && $char2 eq 'R')) {
			$score1++;
		} else {
			$score2++;
		}
	}

	return $score1 <=> $score2;
}


sub callback {
	my $pop = shift;

	my %occurences;

	for my $critter (@{$pop->critters}) {
		my $str = $critter->value;
		$occurences{$_} += $str =~ s/$_//g for qw/R P S/;
	}

	print "$occurences{R} $occurences{P} $occurences{S}\n";
	
	$pop->suspend if $pop->generations >= 1000;
}

my $pop = Algorithm::Evolve->new(
	critter_class    => main,
	selection        => gladitorial,
	replacement      => gladitorial,
	parents_per_gen  => 10,
	children_per_gen => 10,
	size             => 80,
	callback         => \&callback,
#	random_seed      => shift
);

$pop->start;

__END__

=head1 NAME

rock_paper_scissors.pl - Rock Paper Scissors co-evolution example for
Algorithm::Evolve

=head1 DESCRIPTION

This simulation uses StringEvolver.pm as a base class for crossover,
random initialization, and mutation. Unlike examples/string_evolver.pl,
this is a co-evolving system where fitness is not absolute, but based
on a critter's ability to play Rock, Paper, Scissors against other
members of the population.

We override the compare method -- this is the used by gladitorial selection.
It uses each string gene as a sequence of Rock, Paper, and Scissors moves. The
gene who wins the most turns is the winner. Notice how we pick a random spot
in the string genes to start at, and wrap back to the beginning (this is the 
C<$offset1>, C<$offset2> part of the compare method)

At each generation, the total number of Rock, Paper, and Scissors
encodings in the population are tallied and printed. If you graph the
output in something like gnuplot, you will probably notice that the
population cycles between Rock, Paper, and Scissors being the most
prevalent move. This is the command in gnuplot I use to view the output
from this script:

   gnuplot> plot 'output' using :1 title 'Rock' with lines, \
   >             'output' using :2 title 'Paper' with lines, \
   >             'output' using :3 title 'Scissors' with lines

Notice how (in general) Scissors overtakes Paper which overtakes Rock which
overtakes scissors, etc.

