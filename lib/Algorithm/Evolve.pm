package Algorithm::Evolve;

use strict;
use Carp qw/croak carp/;
use List::Util qw/shuffle/;

our (%SELECTION, %REPLACEMENT);
our $VERSION = '0.02';

my $rand_max = (1 << 31); ## close enough

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
		
	until ($pop->is_suspended) {
		my $select = $SELECTION{ $pop->selection };
		my $replace = $REPLACEMENT{ $pop->replacement };
	
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
	carp "It's hard to pick the most fit when fitness is relative!"
		unless ($pop->sort_method eq 'fitness');
	$pop->critters->[-1];
}

sub avg_fitness {
	my $pop = shift;
	my $sum = 0;
	$sum += $_ for @{$pop->fitnesses};
	return $sum / @{$pop->fitnesses};
}

sub selection {
	my ($pop, $method) = @_;
	return $pop->{selection} unless defined $method;
	$pop->{selection} = $method;
	$pop->_validate_args;
	return $pop->{selection};
}

sub replacement {
	my ($pop, $method) = @_;
	return $pop->{replacement} unless defined $method;
	$pop->{replacement} = $method;
	$pop->_validate_args;
	return $pop->{replacement};
}

sub parents_children_per_gen {
	my ($pop, $p, $c) = @_;
	return unless defined $p and defined $c;
	$pop->{parents_per_gen} = $p;
	$pop->{children_per_gen} = $c;
	$pop->_validate_args;
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

#		@{$pop->critters} = 
#			sort { $pop->critter_class->compare($a, $b) } @{$pop->critters};
#		carp "Proceed at your own risk -- using comparison sorting"
#			unless $sort_warn++;

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
	croak "Can't use roulette selection/replacement without a fitness function"
		unless ($pop->sort_method eq 'fitness');
	_pick_n_indices_weighted( $num, [ @{$pop->fitnesses} ] );
};

$REPLACEMENT{roulette} = sub {
	my ($pop, $num) = @_;
	croak "Can't use roulette selection/replacement without a fitness function"
		unless ($pop->sort_method eq 'fitness');
	_pick_n_indices_weighted( $num, [ map { 1/($_+1) } @{$pop->fitnesses} ] );
};

###############
	
$SELECTION{rank} = sub {
	my ($pop, $num) = @_;
	croak "Can't use rank selection/replacement without a fitness function"
		unless ($pop->sort_method eq 'fitness');
	_pick_n_indices_weighted( $num, [ 1 .. $pop->size ] );
};
	
$REPLACEMENT{rank} = sub {
	my ($pop, $num) = @_;
	croak "Can't use rank selection/replacement without a fitness function"
		unless ($pop->sort_method eq 'fitness');
	_pick_n_indices_weighted( $num, [ reverse(1 .. $pop->size) ] );
};

###############

$REPLACEMENT{random} = $SELECTION{random} = sub {
	my ($pop, $num) = @_;
	_pick_n_indices_weighted( $num, [ (1) x $pop->size ] );
};

################

$SELECTION{absolute} = sub {
	my ($pop, $num) = @_;
	croak "Can't use absolute selection/replacement without a fitness function"
		unless ($pop->sort_method eq 'fitness');
	return ( $pop->size - $num .. $pop->size - 1 );
};

$REPLACEMENT{absolute} = sub {
	my ($pop, $num) = @_;
	croak "Can't use absolute selection/replacement without a fitness function"
		unless ($pop->sort_method eq 'fitness');
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
		## so we can sort them by their index number, without having to call
		## the fitness function again.

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

my @gladitorial_replace_indices;
my $gladitorial_warn = 0;
my $gladitorial_attempts_warn = 0;

$SELECTION{gladitorial} = sub {
	my ($pop, $num) = @_;
	
	carp "Gladitorial selection without gladitorial replacement is insane"
		unless ($pop->replacement eq 'gladitorial' or $gladitorial_warn++);

	my $max_attempts = $pop->{max_gladitorial_attempts} || 100;
	my $fetched = 0;
	my $attempts = 0;
	my @available_indices = 0 .. $#{$pop->critters};

	@gladitorial_replace_indices = ();
	my @select_indices;
	
	while ($fetched != $pop->parents_per_gen) {
		my ($i1, $i2) = (shuffle @available_indices)[0,1];

		if ($attempts++ > $max_attempts) {
			carp "Max gladitorial attempts exceeded -- choosing at random"
				unless $gladitorial_attempts_warn++;
			my $remaining = $pop->parents_per_gen - @select_indices;

			push @gladitorial_replace_indices, 
				(shuffle @available_indices)[0 .. $remaining-1];
			push @select_indices,
				(shuffle @available_indices)[0 .. $remaining-1];

			last;							
		}
	
		my $cmp = $pop->critter_class->compare(
			@{$pop->critters}[$i1, $i2]
		);
		
		next if $cmp == 0;
			
		my ($select, $remove) = $cmp > 0 ? ($i1,$i2) : ($i2,$i1);
		
		@available_indices = 
			grep { $_ != $remove } @available_indices;
		push @gladitorial_replace_indices, $remove;
		push @select_indices, $select;
		$fetched++;	
	}

	return @select_indices;
};

$REPLACEMENT{gladitorial} = sub {
	my ($pop, $num) = @_;
	
	croak "parents_per_gen must equal children_per_gen with gladitorial selection"
		if @gladitorial_replace_indices != $num;
	croak "Can't use gladitorial replacement without gladitorial selection"
		unless ($pop->selection eq 'gladitorial');
				
	return @gladitorial_replace_indices;
};

#######################################

BEGIN {
	## creates very basic readonly accessors - very loosely based on an
	## idea by Juerd in http://perlmonks.org/index.pl?node_id=222941

	my @fields = qw/critters size generations callback critter_class
	                random_seed is_suspended sort_method fitnesses
	                parents_per_gen children_per_gen children_per_parent/;

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

        ## Output some stats every 10 generations   
        print $pop->avg_fitness, $/ unless $pop->generations % 10;
        
        ## Stop after 2000 generations
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
    
    ## Print out final population statistics, cleanup, etc..

=cut

=head1 DESCRIPTION

This module is intended to be a useful tool for quick and easy implementation
of evolutionary algorithms. It aims to be flexible, yet simple. For this
reason, it is not a comprehensive implementation of all possible evolutionary
algorithm configurations. The flexibility of Perl allows the evolution of
any type of object conceivable: a simple string or array, a deeper structure
like a hash of arrays, or even a complex object like graph object from another
CPAN module, etc. 

It's also worth mentioning that evolutionary algorithms are generally very
CPU-intensive. There are a great deal of calls to C<rand()> and a lot of
associated floating-point math. If you want a lightning-fast framework, then
searching CPAN at all is probably a bad place to start. However, this doesn't
mean that I've ignored efficiency. The fitness function is often the biggest
bottleneck.

=head2 Framework Overview

The configurable parts of an evolutionary algorithm can be split up into two 
categories:

=over

=item Dependent on the internal representation of genes to evolve:

These include fitness function, crossover and mutation operators. For example,
evolving string genes requires a different mutation operator than evolving
array genes.

=item Independent of representation:

These include selection and replacement methods, population size, number of
mating events, etc.

=back

In Algorithm::Evolve, the first group of options is implemented by the user 
for maximum flexibility. These functions are abstracted to class of evolvable
objects (a B<critter class> in this document). The module itself handles the
representation-independent parts of the algorithm using simple configuration
switches and methods.

=head1 USAGE

=head2 Designing a class of critter objects (interface specification)

Algorithm::Evolve maintains a population of critter objects to be evolved. You 
may evolve any type of objects you want, provided the class supplies the 
following methods:

=over

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
should return the critter's fitness measure within the problem space, which
should always be a nonnegative number. This method need not be memo-ized, as 
it is only called once per critter by Algorithm::Evolve.

This method may be omitted only if using gladitorial selection/replacement
(see below).

=item C<< Class->compare( $obj1, $obj2 ) >>

This method is used for L</co-evolution> with the gladitorial selection method.
It should return a number less than zero if $obj1 is "better," 0 if the two
are equal, or a number greater than zero if $obj2 is "better." 

=back

You may also want to use the C<DESTROY> method as a hook for detecting when
critters are removed from the population.

See the F<examples> directory for an example of a unimodal string evolver that 
uses a very lightweight blessed string-ref implementation. Also, take a look at 
L<Algorithm::Evolve::Util> which provides some useful utilities for 
implementing a critter class.




=head2 Algorithm::Evolve population interface

=over

=item C<< $pop = Algorithm::Evolve->new( option => value, ... ) >>

Takes a hash of arguments and returns a population object. The relevant options 
are:

B<critter_class>, the name of the critter class whose objects are to be 
evolved. This class should already be C<use>'d or C<require>'d by your code.

B<selection> and B<replacement>, the type of selection and replacement methods 
to use. Available methods for both currently include: 

=over

=item *

C<tournament>: Create tournaments groups of the desired size (see below).
The two highest-fitness group members get to breed, and the two lowest-fitness
members get replaced (This is also called single-tournament selection). Must be
specified for both selection and replacement.

=item *

C<gladitorial>: See below under L</co-evolution>. Must be used for both
selection and replacement.

=item *

C<random>: Choose critters completely at random.

=item *

C<roulette>: Choose critters with weighted probability based on their
fitness. For selection, each critter's weight is its fitness. For replacement,
each critter's weight is 1/(f+1).

=item *

C<rank>: Choose critters with weighted probability based on their rank. For
selection, the most-fit critter's weight is C<< $pop->size >>, while the 
least-fit critter's weight is 1. For replacement, the weights are in reverse
order.

=item *

C<absolute>: Choose the N most-fit critters for selection, or the N least-fit
for replacement.

=back

You may mix and match different kinds of selection and replacement methods. The
only exceptions are C<tournament> and C<gladitorial>, which must be used as
both selection and replacement method.



B<tournament_size>, only required if you choose tournament 
selection/replacement. Should be at least 4 unless you know what you're doing.

B<max_gladitorial_attempts>: Because comparisons in gladitorial selection may
result in a tie, this is the number of ties permitted before giving up and
picking critters at random instead during that breeding event. The first time
this occurs, the module will C<carp> a message.

B<parents_per_gen> and B<children_per_gen> control the number of breedings per 
generation. children_per_gen must be a multiple of parents_per_gen. 
parents_per_gen must also be an even number. Each pair of parents selected in a 
generation will produce the same number of children, calling the crossover 
method in the critter class as many times as necessary. Basically, each 
selected parent gets a gene copy count of children_per_gen/parents_per_gen. 

In tournament and gladitorial selection, children_per_gen must be equal to
parents_per_gen. The number of tournaments each generation is equal to
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




=item C<< $pop->run() >>

Begins execution of the algorithm, and returns when the population has been 
C<suspend>'ed.

=item C<< $pop->suspend() >>

Call this method from within the callback function to stop the algorithm's 
iterations and return from the C<run> method.

=item C<< $pop->resume() >>

Start up the algorithm again after being C<suspend>'ed.

=item C<< $pop->generations() >>

=item C<< $pop->avg_fitness() >>

=item C<< $pop->best_fit() >>

These return basic information about the current state of the population. You 
probably will use these methods from within the callback sub. The best_fit 
method returns the most-fit critter in the population.

=item C<< $pop->critters() >>

Returns a reference to an array containing all the critters in the population, 
sorted by fitness. You can use this to iterate over the entire population, but 
please don't modify the array.

=item C<< $pop->random_seed() >>

Returns the random seed that was used for this execution.

=item C<< $pop->selection( [ $new_method ] ) >>

=item C<< $pop->replacement( [ $new_method ] ) >>

Fetch or change the selection/replacement method while the algorithm is
running.

=item C<< $pop->parents_children_per_gen($parents, $children) >>

Changes the parents_per_gen and children_per_gen attributes of the population
while the algorithm is running. Both are changed at once because the latter
must always be a multiple of the former.

=back


=head2 Co-Evolution

When there is no absolute measure of fitness for a problem, and a critter's
fitness depends on the other memebers of the population, this is called
B<co-evolution>. A good example of such a problem is rock-paper-scissors. If
we were to evolve strategies for this game, any strategy's success would be
dependent on what the rest of the population is doing.

To perform such an evolutionary algorithm, implement the C<compare> method
in your critter class and choose gladitorial selection and replacement. 
Gladitorial selection/replacement chooses random pairs of critters and
C<compare>s them. If the result is not a tie, the winner receives reproduction
rights, and the loser is chosen for replacement. This happens until the
desired number of parents have been selected, or until a timeout occurs.

=head1 SEE ALSO

L<Algorithm::Evolve::Util|Algorithm::Evolve::Util>, the F<examples/>
directory.

=head1 AUTHOR

Algorithm::Evolve is written by Mike Rosulek E<lt>mike@mikero.comE<gt>. Feel 
free to contact me with comments, questions, patches, or whatever.

=head1 COPYRIGHT

Copyright (c) 2003 Mike Rosulek. All rights reserved. This module is free 
software; you can redistribute it and/or modify it under the same terms as Perl 
itself.
