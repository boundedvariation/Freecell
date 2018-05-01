package App::FC_Solve::ToHaskellFreecell;

use strict;
use warnings;
use autodie;

use parent 'Games::Solitaire::Verify::Base';

use Games::Solitaire::Verify::State::LaxParser ();
use List::MoreUtils qw(firstidx);

use Getopt::Long qw(GetOptionsFromArray);

__PACKAGE__->mk_acc_ref(
    [
        qw(
            _st
            _filename
            _variant_params
            _buffer_ref
            )
    ]
);

sub _init
{
    my ( $self, $args ) = @_;

    my $argv = $args->{'argv'};

    my $variant_map = Games::Solitaire::Verify::VariantsMap->new();

    my $variant_params = $variant_map->get_variant_by_id("freecell");

    GetOptionsFromArray(
        $argv,
        'g|game|variant=s' => sub {
            my ( undef, $game ) = @_;

            $variant_params = $variant_map->get_variant_by_id($game);

            if ( !defined($variant_params) )
            {
                die "Unknown variant '$game'!\n";
            }
        },
        'freecells-num=i' => sub {
            my ( undef, $n ) = @_;
            $variant_params->num_freecells($n);
        },
        'stacks-num=i' => sub {
            my ( undef, $n ) = @_;
            $variant_params->num_columns($n);
        },
        'decks-num=i' => sub {
            my ( undef, $n ) = @_;

            if ( !( ( $n == 1 ) || ( $n == 2 ) ) )
            {
                die "Decks should be 1 or 2.";
            }

            $variant_params->num_decks($n);
        },
        'sequences-are-built-by=s' => sub {
            my ( undef, $val ) = @_;

            my %seqs_build_by = (
                ( map { $_ => $_ } (qw(alt_color suit rank)) ),
                "alternate_color" => "alt_color",
            );

            my $proc_val = $seqs_build_by{$val};

            if ( !defined($proc_val) )
            {
                die "Unknown sequences-are-built-by '$val'!";
            }

            $variant_params->seqs_build_by($proc_val);
        },
        'empty-stacks-filled-by=s' => sub {
            my ( undef, $val ) = @_;

            my %empty_stacks_filled_by_map =
                ( map { $_ => 1 } (qw(kings any none)) );

            if ( !exists( $empty_stacks_filled_by_map{$val} ) )
            {
                die "Unknown empty stacks filled by '$val'!";
            }

            $variant_params->empty_stacks_filled_by($val);
        },
        'sequence-move=s' => sub {
            my ( undef, $val ) = @_;

            my %seq_moves = ( map { $_ => 1 } (qw(limited unlimited)) );

            if ( !exists( $seq_moves{$val} ) )
            {
                die "Unknown sequence move '$val'!";
            }

            $variant_params->sequence_move($val);
        },
    ) or die "Cannot process command line arguments";

    my $filename = shift(@$argv);

    if ( !defined($filename) )
    {
        $filename = "-";
    }

    $self->_variant_params($variant_params);
    $self->_filename($filename);

    my $s = '';
    $self->_buffer_ref( \$s );

    return;
}

sub _append
{
    my ( $self, $text ) = @_;

    ${ $self->_buffer_ref } .= $text;

    return;
}

sub _get_buffer
{
    my ($self) = @_;

    return ${ $self->_buffer_ref };
}

sub _slurp
{
    my $filename = shift;

    my $in;

    if ( $filename eq '-' )
    {
        $in = \*STDIN;
    }
    else
    {
        open $in, '<', $filename
            or die "Cannot open '$filename' for slurping - $!";
    }

    local $/;
    my $contents = <$in>;

    close($in);

    return $contents;
}

sub _read_initial_state
{
    my $self = shift;

    $self->_st(
        Games::Solitaire::Verify::State::LaxParser->new(
            {
                string           => scalar( _slurp( $self->_filename ) ),
                variant          => 'custom',
                'variant_params' => $self->_variant_params(),
            }
        )
    );

    return;
}

sub _process_main
{
    my $self = shift;

    $self->_read_initial_state;
    my $fc = join( ' ',
        map { $_ ? $_->to_string : () }
            map { $self->_st->get_freecell($_) }
            ( 0 .. $self->_st->num_freecells - 1 ) );
    if ($fc)
    {
        $self->_append("FC $fc\n");
    }
    foreach my $i ( 0 .. $self->_st->num_columns - 1 )
    {
        my $col = $self->_st->get_column($i);
        $self->_append(
            join( ' ',
                'C',
                reverse map { $col->pos($_)->to_string } 0 .. $col->len - 1 )
                . "\n"
        );
    }

    return;
}

sub run
{
    my ($self) = @_;

    $self->_process_main;

    print $self->_get_buffer;

    return;
}

1;

=head1 NAME

App::FC_Solve::ToHaskellFreecell - a modulino for
converting from fc-solve boards to solve.hs ones.

=head1 SYNOPSIS

    $ perl -M App::FC_Solve::ToHaskellFreecell -e 'Games::Solitaire::Verify::App::CmdLine::From_Patsolve->new({argv => \@ARGV})->run()' -- [ARGS]

=head1 METHODS

=head2 run()

Actually execute the command-line application.

=cut
