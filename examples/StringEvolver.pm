package StringEvolver;
use strict;
use Algorithm::Evolve::Util ':str';

our %configs;

sub import {
	my $class = shift;
	
	%configs = (
		string_length    => 20,
		alphabet         => [0,1],
		reference_string => '1' x 20,
		mutation_rate    => 0.05,
		crossover_pts    => 2,
		@_
	);
}

sub new {
	my $pkg = shift;
	my $string = shift
		|| str_random($configs{string_length}, $configs{alphabet});
	return bless \$string, $pkg;
}

sub crossover {
	my ($pkg, $s1, $s2) = @_;
	return map { $pkg->new($_) } 
	       str_crossover($$s1, $$s2, $configs{crossover_pts});
}

sub fitness {
	my $self = shift;
	return str_agreement($configs{reference_string}, $$self);
}

sub mutate {
	my $self = shift;
	$$self = str_mutate($$self, $configs{mutation_rate}, $configs{alphabet});
}

sub value {
	my $self = shift;
	return $$self;
}

1;
__END__

=head1 NAME

StringEvolver - A generic base critter class for use with Algorithm::Evolve

=head1 SYNOPSIS

  package StringCritters;
  use StringEvolver string_length => 50,
                    alphabet => [qw/A B C D/],
                    ...;
  our @ISA = ('StringEvolver');
  
  # override methods you want to change
  
  # StringCritters is now a valid critter class

You can use this class as a base class any time your representation is a
string gene.

=head1 USE ARGUMENTS

=over

=item string_length

The length of strings to evolve. Defaults to 20.

=item alphabet

A reference to an array of characters. Defaults to [0,1].

=item reference_string

By default, fitness is measured as the number of character agreements to a
reference string. However, if you are implementing a non-trivial string
evolver, you will probably override the fitness method and this argument
won't make a difference. It defaults to C<'1'x20>.

=item mutation_rate

If this number is less than one, then it is the probablistic mutation rate
for each character. If it is greater than or equal to one, then exactly that
many mutations will be performed per child (so it must be an integer).
Defaults to 0.05. 

=item crossover_pts

The number of crossover points when performing crossover. See
L<Algorithm::Evolve::Util|Algorithm::Evolve::Util> for more information on
crossover.

=back

=head1 INHERITED METHODS

When used as a base class, the calling class inherit the following methods:

=over

=item C<< Class->new() >>

When used with an argument, the new critter is initialized with the argument
as its string value. Otherwise, this method creates a random string within
the alphabet.

=item C<< $obj->mutate() >>

Mutates the critter's string gene according to the given mutation rate.

=item C<< Class->crossover($obj1, $obj2) >>

Takes two critters and returns a random crossover of the two,
according to the given number of crossover points.

=item C<< $obj->fitness() >>

Returns the fitness of the critter, measured as the number of character
agreements with a reference string. You will probably override this method.

=item C<< $obj->value() >>

Returns the value of the critter's string gene.

=back

=head1 SEE ALSO

L<Algorithm::Evolve|Algorithm::Evolve>, 
L<Algorithm::Evolve::Util|Algorithm::Evolve::Util>, the rest of the
F<examples/> directory.

=head1 AUTHOR

Algorithm::Evolve is written by Mike Rosulek E<lt>mike@mikero.comE<gt>. Feel 
free to contact me with comments, questions, patches, or whatever.

=head1 COPYRIGHT

Copyright (c) 2003 Mike Rosulek. All rights reserved. This module is free 
software; you can redistribute it and/or modify it under the same terms as Perl 
itself.
