package Algorithm::Evolve::Util;

use strict;
use base 'Exporter';
use List::Util qw/shuffle/;
use Carp qw/croak carp/;

our $VERSION = '0.01';

our %EXPORT_TAGS = (
	str => [qw/str_crossover str_mutate str_agreement str_random/],
);
our @EXPORT_OK = map { @$_ } values %EXPORT_TAGS;

sub str_crossover {
	my ($s1, $s2, $n_point) = @_;
	$n_point ||= 2;

	my $len = length($s1);

	croak "Can't do ${n_point}-point crossover on length $len string"
		if $n_point >= $len;

	## this allows for duplication of indices. maybe a fixme

	my @points = sort { $a <=> $b } map { int(rand $len) } 1 .. $n_point;
	push @points, $len if $n_point % 2;

	for (0 .. @points/2 - 1) {
		my ($x, $y) = @points[2*$_, 2*$_+1];
		(substr($s1, $x, $y-$x), substr($s2, $x, $y-$x)) =
			(substr($s2, $x, $y-$x), substr($s1, $x, $y-$x));
	}
	
	return ($s1, $s2);
}

sub str_agreement {
	my ($s1, $s2) = @_;

	my $tally = 0;
	for (0 .. length($s1)-1) {
		$tally++ if substr($s1, $_, 1) eq substr($s2, $_, 1);
	}

	return $tally;
}

sub str_mutate {
	my ($string, $n, $alphabet) = @_;
	$n        ||= 1;
	$alphabet ||= [0,1];

	croak "Invalid alphabet"
		unless ref $alphabet eq 'ARRAY' and @$alphabet > 1;

	my $len            = length($string);
	my @mutate_indices = ();
	
	if ($n < 1) {  ## probabalistic mutation
		@mutate_indices = map { rand() < $n ? $_ : () } 0 .. $len-1;
	} else {
		## could just roll my own algorithm to randomly choose $n items, but
		## List::Util is XS and much faster even to shuffle the whole array
		## as long as the alphabet isn't large (in the thousands)
		
		@mutate_indices = (shuffle 0 .. $len-1 )[ 0 .. $n-1 ];
	}
	
	for my $idx (@mutate_indices) {
		my $char = substr($string, $idx, 1);
		my @different = grep { $char ne $_ } @$alphabet;
		
		substr($string, $idx, 1) = $different[ int(rand @different) ];
	}
	
	return $string;
}

sub str_random {
	my ($length, $alphabet) = @_;
	$alphabet ||= [0,1];

	return join '', map { $alphabet->[ rand @$alphabet ] } 1 .. $length;
}


##########################################
##########################################
##########################################
1;
__END__

=head1 NAME

Algorithm::Evolve::Util - Some useful utility functions for use in evolutionary
algorithms.

=head1 SYNOPSIS

    use Algorithm::Evolve::Util qw/:str/;

=head1 SYNTAX

At the moment, this module only provides string-mangling utilities. They can 
all be imported with the use argument ':str'.

=over 4

=item C< str_crossover( $string1, $string2 [, $n ] ) >

Performs a random $n-point crossover between two strings, and returns the two
resulting children. $n defaults to 2. The two inputs should be the same length,
although this is not enforced. $n must be also less than the length of the
strings.

If you are unfamiliar with the string crossover operation, try examining sample
outputs for input strings of C<'a' x 20> and C<'b' x 20>.

=item C< str_agreement( $string1, $string2 ) >

Returns the number of characters in which the two strings agree. Does not
enforce that the strings have the same length, even though the result is
somewhat meaningless in that case.

=item C< str_mutate( $string1 [, $num [, \@alphabet ]] ) >

Mutates the string according to the given alphabet (defaulting to {0,1}). If
$num is less than one, it performs I<probabilistic> mutation, with each
character having a $num probability of being mutated. If $num is greater than
or equal to 1, it performs I<N-point mutation>: exactly $num characters are
chosen at random from the string and mutated. $num defaults to 1. Returns the
modified string.

A mutation will always change the character in question, i.e., an
'a' will never be chosen to replace an existing 'a' during a mutation.

=item C< str_random( $length [, \@alphabet ] ) >

Returns a random string of the specified length over the specified alphabet,
defaulting to {0,1}.

=back

=head1 SEE ALSO

L<Algorithm::Evolve>

=head1 AUTHOR

Algorithm::Evolve is written by Mike Rosulek E<lt>mike@mikero.comE<gt>. Feel 
free to contact me with comments, questions, patches, or whatever.

=head1 COPYRIGHT

Copyright (c) 2003 Mike Rosulek. All rights reserved. This module is free 
software; you can redistribute it and/or modify it under the same terms as Perl 
itself.
