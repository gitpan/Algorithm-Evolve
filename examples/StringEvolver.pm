package StringEvolver;
use strict;
use Algorithm::Evolve::Util qw/:str/;

my $string_length    = 50;
my @alphabet         = qw/0 1/;
my $reference_string = '1' x $string_length;
my $mutation_rate    = 0.05;
my $crossover_pts    = 2;

sub new {
	my $pkg = shift;
	my $string = shift || str_random($string_length, \@alphabet);
	return bless \$string, $pkg;
}

sub crossover {
	my ($pkg, $s1, $s2) = @_;
	return map { $pkg->new($_) } str_crossover($$s1, $$s2, $crossover_pts);
}

sub fitness {
	my $self = shift;
	return str_agreement($reference_string, $$self);
}

sub mutate {
	my $self = shift;
	$$self = str_mutate($$self, $mutation_rate, \@alphabet);
}

sub print {
	my $self = shift;
	return $$self;
}

1;
