# This is the beginning of bootstrap_trailer.pl

$new_start_symbol //= '(undefined start symbol)';
$new_semantics //= 'not defined';
$new_version //= 'not defined';

Marpa::exception('Version requested is ', $new_version, "\nVersion must match ", $Marpa::VERSION, ' exactly.')
   unless $new_version eq $Marpa::VERSION;

Marpa::exception('Semantics are ', $new_semantics, "\nThe only semantics currently available are perl5.")
   unless $new_semantics eq 'perl5';

my $g = new Marpa::Grammar({
    start => $new_start_symbol,
    rules => $new_rules,
    terminals => $new_terminals,
    warnings => 1,
    precompute => 0,
});

$g->set({
    default_lex_prefix => $new_default_lex_prefix,
    precompute => 0,
}) if defined $new_default_lex_prefix;

$g->set({
    default_action => $new_default_action,
    precompute => 0,
}) if defined $new_default_action;

$g->set({
    default_null_value => $new_default_null_value,
    precompute => 0,
}) if defined $new_default_null_value;

$g->precompute();

my $recce = new Marpa::Recognizer({
   grammar=> $g,
   preamble => $new_preamble,
   lex_preamble => $new_lex_preamble,
});

sub locator {
    my $earleme = shift;
    my $string = shift;

    state $lines;
    $lines = [0];
    my $pos = pos ${$string} = 0;
    NL: while (${$string} =~ /\n/gxms) {
	$pos = pos ${$string};
	push @{$lines}, $pos;
	last NL if $pos > $earleme;
    }
    my $line = (@{$lines}) - ($pos > $earleme ? 2 : 1);
    my $line_start = $lines->[$line];
    return ($line, $line_start);
}

my $spec;

{
    local($RS) = undef;
    open my $grammar, '<', $grammar_file_name or Marpa::exception("Cannot open $grammar_file_name: $ERRNO");
    $spec = <$grammar>;
    close $grammar;
    if ((my $earleme = $recce->text(\$spec)) >= 0) {
	# for the editors, line numbering starts at 1
	# do something about this?
	my ($line, $line_start) = locator($earleme, \$spec);
	say STDERR 'Parsing exhausted at line ', $line+1, ", earleme $earleme";
	given (index $spec, "\n", $line_start) {
	    when (undef) { say STDERR substr $spec, $line_start }
	    default { say STDERR substr $spec, $line_start, $_-$line_start }
	}
	say STDERR +(q{ } x ($earleme-$line_start)), q{^};
	exit 1;
    }
}

$recce->end_input();

my $evaler = new Marpa::Evaluator( { recce => $recce } );
Marpa::exception('No parse') unless $evaler;

sub slurp {
    open my $fh, '<', shift;
    local($RS)=undef;
    my $file = <$fh>;
    close $fh;
    return $file;
}

say '# This file was automatically generated using Marpa ', $Marpa::VERSION;

if ($header_file_name)
{
    my $header = slurp($header_file_name);
    if (defined $header)
    {
        # explicit STDOUT is workaround for perlcritic bug
        print {*STDOUT} $header
            or Marpa::exception("print failed: $ERRNO");
    }
}

my $value = $evaler->old_value();
say ${$value};

if ($trailer_file_name)
{
    my $trailer = slurp($trailer_file_name);
    if (defined $trailer)
    {
        # explicit STDOUT is workaround for perlcritic bug
        print {*STDOUT} $trailer
            or Marpa::exception("print failed: $ERRNO");
    }
}

# This is the end of bootstrap_trailer.pl
