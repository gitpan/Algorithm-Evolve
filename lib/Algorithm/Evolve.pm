package Algorithm::Evolve;

use strict;
use Carp qw/croak carp/;
use List::Util qw/shuffle/;

our (%SELECTION, %REPLACEMENT);
our $VERSION = '0.01';

my $rand_max = (1 << 31); ## feh, that's big enough!

###########################

sub new {
	my $pkg = shift;

	my $pop = bless {
		random_seed      => int(rand $rand_max),
		generations      => 0,
		size             => 1000,
		parents_per_gen  => 2,
		children_per_gen => 2,
		@_
	}, $pkg;
	
	srand( $pop->random_seed );

	$pop->_validate_args;

	return $pop;
}

sub _validate_args {
	my $pop = shift;
	
	croak "Invalid selection/replacement criteria"
		unless exists $REPLACEMENT{ $pop->replacement }
		   and exists $SELECTION{ $pop->selection };

	croak "parents_per_gen must be even" if $pop->parents_per_gen % 2;
	croak "parents_per_gen must divide children_per_gen"
		if $pop->children_per_gen % $pop->parents_per_gen;
	croak "parents_per_gen and children_per_gen must be no larger than size"
		if $pop->children_per_gen > $pop->size
		or $pop->parents_per_gen > $pop->size;
		
	$pop->{children_per_parent} =
		$pop->children_per_gen / $pop->parents_per_gen;

}

############################

sub start {
	my $pop = shift;
	$pop->_initialize;
	
	my $select = $SELECTION{ $pop->selection };
	my $replace = $REPLACEMENT{ $pop->replacement };
	
	until ($pop->is_suspended) {
	
		my @parent_indices = $select->($pop, $pop->parents_per_gen);		
		my @parents = @{$pop->critters}[ @parent_indices ];

		## take parents two at a time, thus the array slice
		## @parents[ 2*$_ , 1 + 2*$_ ]
		
		my @children = 
			map { my @p = @parents[ 2*$_ , 2*$_+1 ];
			      map { $pop->critter_class->crossover(@p) }
			          1 .. $pop->children_per_parent;
			    }
			0 .. (@parents/2)-1;
	
		$_->mutate for @children;
	
		my @replace_indices = $replace->($pop, $pop->children_per_gen);

		## place the new critters first, then sort. maybe fixme:
		
		@{$pop->critters}[ @replace_indices ] = @children;
		@{$pop->fitnesses}[ @replace_indices ] = ()
			if $pop->sort_method eq 'fitness';
		
		$pop->_sort_critters;

		$pop->{generations}++;	
		$pop->callback->($pop) if (ref $pop->callback);
	}
}

###################

sub suspend {
	my $pop = shift;
	$pop->{is_suspended} = 1;
}

sub resume {
	my $pop = shift;
	$pop->{is_suspended} = 0;
	$pop->start;
}

sub best_fit {
	my $pop = shift;
	$pop->critters->[-1];
}

sub avg_fitness {
	my $pop = shift;
	my $sum = 0;
	$sum += $_ for @{$pop->fitnesses};
	return $sum / @{$pop->fitnesses};
}

####################

sub _initialize {
	my $pop = shift;
	return if defined $pop->critters;
	
	$pop->{critters} = [ map { $pop->critter_class->new } 1 .. $pop->{size} ];

	$pop->{sort_method} = 'fitness';
	eval { $pop->critters->[0]->fitness };
	$pop->{sort_method} = 'compare' if $@;

	if ($pop->{sort_method} eq 'fitness') {
		$pop->{fitnesses} = [
			map { $pop->critters->[$_]->fitness }
			0 .. $#{$pop->critters}
		];
	}

	$pop->_sort_critters;
}


## memoize the fitness values to optimize re-sorting
my $sort_warn = 0;

sub _sort_critters {
	my $pop = shift;

	if ($pop->sort_method eq 'fitness') {

		## don't want to be calling $pop->fitnesses from within the
		## sort comparison
		
		my $fitnesses = $pop->fitnesses;
		my $critters = $pop->critters;

		for (0 .. $#{$pop->critters}) {
			$fitnesses->[$_] = $critters->[$_]->fitness
				unless defined $fitnesses->[$_];
		}
		
		my @sorted_indices =
			sort { $fitnesses->[$a] <=> $fitnesses->[$b] }
			0 .. $#{$critters};
		@{$critters}  = @{$critters}[ @sorted_indices ];
		@{$fitnesses} = @{$fitnesses}[ @sorted_indices ];

	} else {

		@{$pop->critters} = 
			sort { $pop->critter_class->compare($a, $b) } @{$pop->critters};
		carp "Proceed at your own risk -- using comparison sorting"
			unless $sort_warn++;

	}

}

############################
## picks N indices randomly, using the given weights

sub _pick_n_indices_weighted {
	my $num = shift;
	my $relative_prob = shift;
	
	my $sum = 0;
	$sum += $_ for @$relative_prob;

	my @indices = ();
	
	while ($num--) {
		die "Total probability isn't positive -- perhaps we are choosing too many indices"
			if $sum <= 0;
		
		my $dart = $sum * rand;
		my $index = -1;
	
		$dart -= $relative_prob->[++$index] while ($dart > 0);
		
		$sum -= $relative_prob->[$index];
		$relative_prob->[$index] = 0;
		push @indices, $index;
	}
	
	return @indices;
}

#############################
## Selection / replacement routines: these take a population object and a 
## number, and return a list of indices. Keep in mind that the critter
## array is already sorted by fitness.

#############################

## these two go crazy with negative fitness values. fixme later maybe

$SELECTION{roulette} = sub {
	my ($pop, $num) = @_;
	_pick_n_indices_weighted( $num, [ @{$pop->fitnesses} ] );
};

$REPLACEMENT{roulette} = sub {
	my ($pop, $num) = @_;
	_pick_n_indices_weighted( $num, [ map { 1/($_+1) } @{$pop->fitnesses} ] );
};

###############
	
$SELECTION{rank} = sub {
	my ($pop, $num) = @_;
	_pick_n_indices_weighted( $num, [ 1 .. @{$pop->critters} ] );
};
	
$REPLACEMENT{rank} = sub {
	my ($pop, $num) = @_;
	_pick_n_indices_weighted( $num, [ reverse(1 .. @{$pop->critters}) ] );
};

###############

$REPLACEMENT{random} = $SELECTION{random} = sub {
	my ($pop, $num) = @_;
	_pick_n_indices_weighted( $num, [ (1) x @{$pop->critters} ] );
};

################

$SELECTION{absolute} = sub {
	my ($pop, $num) = @_;
	return ( $pop->size - $num .. $pop->size - 1 );
};

$REPLACEMENT{absolute} = sub {
	my ($pop, $num) = @_;
	return ( 0 .. $num-1 );
};

################

my @tournament_replace_indices;
my $tournament_warn = 0;
	
$SELECTION{tournament} = sub {
	my ($pop, $num) = @_;
	my $t_size = $pop->{tournament_size};
	
	croak "Invalid (or no) tournament size specified" 
		if not defined $t_size or $t_size < 2 or $t_size > $pop->size;
	croak "Tournament size * #tournaments must be no greater than population size" 
		if ($num/2) * $t_size > $pop->size;
	carp "Tournament selection without tournament replacement is insane"
		unless ($pop->replacement eq 'tournament' or $tournament_warn++);
		
	my $tournament_groups = $num / 2;
	
	my @indices = shuffle(0 .. $#{$pop->critters});
	my @tournament_choose_indices = @tournament_replace_indices = ();
	
	for my $i (0 .. $tournament_groups-1) {
		my $beg = $t_size * $i;
		my $end = $beg + $t_size - 1;
		
		## the critters are already sorted by fitness within $pop->critters -- 
		## so we can sort them by their index number, without having to call the
		## fitness function again.

		my @sorted_group_indices = sort { $b <=> $a } @indices[ $beg .. $end ];
		push @tournament_choose_indices,  @sorted_group_indices[0,1];
		push @tournament_replace_indices, @sorted_group_indices[-2,-1];
	}

	return @tournament_choose_indices;		
};

$REPLACEMENT{tournament} = sub {
	my ($pop, $num) = @_;
	croak "parents_per_gen must equal children_per_gen with tournament selection"
		if @tournament_replace_indices != $num;
	croak "Can't use tournament replacement without tournament selection"
		unless ($pop->selection eq 'tournament');
				
	return @tournament_replace_indices;
};

#######################################

BEGIN {
	## creates very basic readonly accessors - very loosely based on an
	## idea by Juerd in http://perlmonks.org/index.pl?node_id=222941

	my @fields = qw/critters replacement selection size generations
	                callback critter_class random_seed is_suspended
	                sort_method fitnesses parents_per_gen 
	                children_per_gen children_per_parent/;

	no strict 'refs';
	for my $f (@fields) { 
		*$f = sub { carp "$f method is readonly" if $#_; $_[0]->{$f} };
	}
}

##########################################
##########################################
##########################################
1;
__END__

=head1 NAME

Algorithm::Evolve - An extensible and generic framework for executing 
evolutionary algorithms

=head1 SYNOPSIS

    #!/usr/bin/perl -w
    use Algorithm::Evolve;
    use MyCritters;  ## class providing appropriate methods
    
    sub callback {
        my $pop = shift;

        ## some stats every 10 generations   
        print $pop->avg_fitness, $/ unless $pop->generations % 10;
        
        $pop->suspend if $pop->generations >= 2000;
    }
    
    my $pop = Algorithm::Evolve->new(
        critter_class    => MyCritters,
        selection        => rank,
        replacement      => random,
        parents_per_gen  => 2,
        children_per_gen => 4,
        size             => 400,
        callback         => \&callback,
    );
    
    $pop->start;
    
    ## print out final population statistics

=cut

=head1 DESCRIPTION

This module contains a class-based framework for executing evolutionary 
algorithms. The goal is to allow evolution of any type of object conceivable 
with relative ease, be it a simple string, an array, a hash of arrays, a 
directed graph object using a CPAN module, or anything else.

The configurable aspects of an evolutionary algorithm can be split up into two 
categories: those aspects closely tied to the object representation (fitness 
function, crossover and mutation operators), and those that do not depend at 
all on the object representation (selection and replacement methods, population 
size, number of mating events). The former group of options is implemented by 
the user and abstracted to class of evolvable objects, while the latter is 
handled by Algorithm::Evolve using simple configuration switches.




=head1 USAGE

=head2 Designing a class of critter objects

Algorithm::Evolve maintains a population of critter objects to be evolved. You 
may evolve any type of objects you want, provided the class supplies the 
following methods:

=over 4

=item C<< Class->new() >>

This method will be called as a class method with no arguments. It must return
a blessed critter object. It is recommended that the returned critter's genes
be randomly initialized.

=item C<< Class->crossover( $obj1, $obj2 ) >>

This method will also be called as a class method, with two critter objects as 
arguments. It should return a list of two new objects based on the genes of the 
passed objects. 

=item C<< $critter->mutate() >>

This method will be called as an instance method, with no arguments. It should
randomly modify the genes of the critter. Its return value is ignored.

=item C<< $critter->fitness() >>

This method will also be called as an instance method, with no arguments. It 
should return the critter's fitness measure within the problem space. This 
should always be a  nonnegative number. This method need not be memo-ized, as 
it is only called once per critter by Algorithm::Evolve.

=back

You may also want to use the C<DESTROY> method as a hook for when critters are 
removed from the population.

See the F<examples> directory for an example of a unimodal string evolver that 
uses a very lightweight blessed string-ref implementation. Also, take a look at 
L<Algorithm::Evolve::Util> which provides some useful utilities for 
implementing a critter class.




=head2 Algorithm::Evolve population interface

=over 4

=item C<< $pop = Algorithm::Evolve->new( option => value, ... ) >>

Takes a hash of arguments and returns a population object. The relevant options 
are:

B<critter_class>, the name of the critter class whose objects are to be 
evolved. This class should already be C<use>'d or C<require>'d by your code.

B<selection> and B<replacement>, the type of selection and replacement methods 
to use. Available methods for both currently include: 
  C<tournament>,
  C<random>,
  C<rank>,
  C<roulette>, and
  C<absolute>.
You may mix and match selection and replacement methods, the only exception 
being C<tournament> which can only be used as both selection and replacement 
method.

B<tournament_size>, only required if you choose tournament selection/
replacement. Algorithm::Evolve only currently performs single-tournament 
selection/replacement.

B<parents_per_gen> and B<children_per_gen> control the number of breedings per 
generation. children_per_gen must be a multiple of parents_per_gen. 
parents_per_gen must also be an even number. Each pair of parents selected in a 
generation will produce the same number of children, calling the crossover 
method in the critter class as many times as necessary. Basically, each 
selected parent gets a gene copy count of children_per_gen/parents_per_gen. The
exception is in tournament selection, where children_per_gen must be equal to
parents_per_gen. The number of tournaments each generation is then equal to
parents_per_gen/2.

B<size>, the number of critters to have in the population.

B<callback>, an optional (but highly recommended) reference to a function. It 
should expect one argument, the population object. It is called after each 
generation. You may find it useful for printing out current statistical 
information. You must also use it if you intend to stop the algorithm after a 
certain number of generations (or some other criteria).

B<random_seed>, an optional number that will be fed to C<srand> before the 
algorithm starts. Use this to reproduce previous results. If this is not given, 
Algorithm::Evolve will generate a random seed that you can retrieve.




=item C<< $pop->run >>

Begins execution of the algorithm, and returns when the population has been 
C<suspend>'ed.

=item C<< $pop->suspend >>

Call this method from within the callback function to stop the algorithm's 
iterations and return from the C<run> method.

=item C<< $pop->resume >>

Start up the algorithm again after being C<suspend>'ed.

=item C<< $pop->generations, $pop->avg_fitness, $pop->best_fit >>

These return basic information about the current state of the population. You 
probably will use these methods from within the callback sub. The best_fit 
method returns the most-fit critter in the population.

=item C<< $pop->critters >>

Returns a reference to an array containing all the critters in the population, 
sorted by fitness. You can use this to iterate over the entire population, but 
please don't modify the array.

=item C<< $pop->random_seed >>

Returns the random seed that was used for this execution.

=back




=head1 SEE ALSO

L<Algorithm::Evolve::Util>

=head1 AUTHOR

Algorithm::Evolve is written by Mike Rosulek E<lt>mike@mikero.comE<gt>. Feel 
free to contact me with comments, questions, patches, or whatever.

=head1 COPYRIGHT

Copyright (c) 2003 Mike Rosulek. All rights reserved. This module is free 
software; you can redistribute it and/or modify it under the same terms as Perl 
itself.
