#!perl

use 5.010;
use strict;
use warnings;
use Marpa;
use Marpa::MDL;
use Marpa::MDL::Internal::Actions;
use Fatal qw(open);
use English qw( -no_match_vars );
use Carp;

package Bootstrap_Grammar;

## no critic (ValuesAndExpressions::ProhibitNoisyQuotes)
## no critic (TestingAndDebugging::ProhibitNoStrict)
no strict 'vars';
#<<< no perltidy

# To update, copy self.mdl.raw between these two lines
$data = {
  'marpa_options' => [
    {
      'rules' => [
        {
          'action' => 'grammar',
          'lhs' => 'grammar',
          'rhs' => [
            'paragraphs',
            'trailing-matter'
          ]
        },
        {
          'lhs' => 'paragraphs',
          'min' => 1,
          'rhs' => [
            'paragraph'
          ],
          'separator' => 'empty-line'
        },
        {
          'lhs' => 'paragraph',
          'rhs' => [
            'definition-paragraph'
          ]
        },
        {
          'lhs' => 'paragraph',
          'rhs' => [
            'production-paragraph'
          ]
        },
        {
          'lhs' => 'paragraph',
          'rhs' => [
            'terminal-paragraph'
          ]
        },
        {
          'lhs' => 'definition-paragraph',
          'min' => 1,
          'rhs' => [
            'definition'
          ]
        },
        {
          'action' => 'Marpa::MDL::Internal::Actions::first_arg',
          'lhs' => 'action-sentence:optional',
          'rhs' => [
            'action-sentence'
          ]
        },
        {
          'lhs' => 'action-sentence:optional',
          'rhs' => []
        },
        {
          'action' => 'production_paragraph',
          'lhs' => 'production-paragraph',
          'rhs' => [
            'non-structural-production-sentences',
            'production-sentence',
            'non-structural-production-sentences',
            'action-sentence:optional',
            'non-structural-production-sentences'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'non-structural-production-sentences',
          'min' => 0,
          'rhs' => [
            'non-structural-production-sentence'
          ]
        },
        {
          'action' => 'non_structural_production_sentence',
          'lhs' => 'non-structural-production-sentence',
          'rhs' => [
            'priority:k0',
            'integer',
            'period'
          ]
        },
        {
          'action' => 'Marpa::MDL::Internal::Actions::first_arg',
          'lhs' => 'the:k1:optional',
          'rhs' => [
            'the:k1'
          ]
        },
        {
          'lhs' => 'the:k1:optional',
          'rhs' => []
        },
        {
          'action' => 'long_action_sentence',
          'lhs' => 'action-sentence',
          'rhs' => [
            'the:k1:optional',
            'action:k2',
            'is:k3',
            'action-specifier',
            'period'
          ]
        },
        {
          'action' => 'short_action_sentence',
          'lhs' => 'action-sentence',
          'rhs' => [
            'action-specifier',
            'period'
          ]
        },
        {
          'action' => 'first_arg',
          'lhs' => 'action-specifier',
          'rhs' => [
            'string-specifier'
          ]
        },
        {
          'lhs' => 'non-structural-production-sentence',
          'rhs' => [
            'comment-sentence'
          ]
        },
        {
          'lhs' => 'non-structural-terminal-sentence',
          'rhs' => [
            'comment-sentence'
          ]
        },
        {
          'action' => 'definition_of_predefined',
          'lhs' => 'definition',
          'priority' => '1000',
          'rhs' => [
            'predefined-setting',
            'period'
          ]
        },
        {
          'lhs' => 'definition',
          'rhs' => [
            'comment-sentence'
          ]
        },
        {
          'lhs' => 'definition',
          'rhs' => [
            'bracketed-comment'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'definition',
          'rhs' => [
            'string-definition'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'predefined-setting',
          'rhs' => [
            'default-action-setting'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'predefined-setting',
          'rhs' => [
            'default-null-value-setting'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'predefined-setting',
          'rhs' => [
            'semantics-setting'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'predefined-setting',
          'rhs' => [
            'version-setting'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'predefined-setting',
          'rhs' => [
            'start-symbol-setting'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'predefined-setting',
          'rhs' => [
            'default-lex-prefix-setting'
          ]
        },
        {
          'action' => 'semantics_predicate',
          'lhs' => 'semantics-setting',
          'rhs' => [
            'the:k1:optional',
            'semantics:k4',
            'copula',
            'perl5:k5'
          ]
        },
        {
          'action' => 'semantics_subject',
          'lhs' => 'semantics-setting',
          'rhs' => [
            'perl5:k5',
            'copula',
            'the:k1:optional',
            'semantics:k4'
          ]
        },
        {
          'action' => 'version_predicate',
          'lhs' => 'version-setting',
          'rhs' => [
            'the:k1:optional',
            'version:k6',
            'copula',
            'version-number'
          ]
        },
        {
          'action' => 'version_subject',
          'lhs' => 'version-setting',
          'rhs' => [
            'version%5c%20number:k7',
            'copula',
            'the:k1:optional',
            'version:k6'
          ]
        },
        {
          'action' => 'start_symbol_predicate',
          'lhs' => 'start-symbol-setting',
          'rhs' => [
            'the:k1:optional',
            'start:k8',
            'symbol:k9',
            'copula',
            'symbol-phrase'
          ]
        },
        {
          'action' => 'start_symbol_subject',
          'lhs' => 'start-symbol-setting',
          'rhs' => [
            'symbol-phrase',
            'copula',
            'the:k1:optional',
            'start:k8',
            'symbol:k9'
          ]
        },
        {
          'action' => 'default_lex_prefix_subject',
          'lhs' => 'default-lex-prefix-setting',
          'rhs' => [
            'regex',
            'copula',
            'the:k1:optional',
            'default:ka',
            'lex:kb',
            'prefix:kc'
          ]
        },
        {
          'action' => 'default_lex_prefix_predicate',
          'lhs' => 'default-lex-prefix-setting',
          'rhs' => [
            'the:k1:optional',
            'default:ka',
            'lex:kb',
            'prefix:kc',
            'copula',
            'regex'
          ]
        },
        {
          'action' => 'default_null_value_subject',
          'lhs' => 'default-null-value-setting',
          'rhs' => [
            'string-specifier',
            'copula',
            'the:k1:optional',
            'default:ka',
            'null:kd',
            'value:ke'
          ]
        },
        {
          'action' => 'default_null_value_predicate',
          'lhs' => 'default-null-value-setting',
          'rhs' => [
            'the:k1:optional',
            'default:ka',
            'null:kd',
            'value:ke',
            'copula',
            'string-specifier'
          ]
        },
        {
          'lhs' => 'copula',
          'rhs' => [
            'is:k3'
          ]
        },
        {
          'lhs' => 'copula',
          'rhs' => [
            'are:kf'
          ]
        },
        {
          'action' => 'string_definition',
          'lhs' => 'string-definition',
          'rhs' => [
            'symbol-phrase',
            'is:k3',
            'string-specifier',
            'period'
          ]
        },
        {
          'action' => 'default_action_subject',
          'lhs' => 'default-action-setting',
          'rhs' => [
            'action-specifier',
            'is:k3',
            'the:k1:optional',
            'default:ka',
            'action:k2'
          ]
        },
        {
          'action' => 'default_action_predicate',
          'lhs' => 'default-action-setting',
          'rhs' => [
            'the:k1:optional',
            'default:ka',
            'action:k2',
            'is:k3',
            'action-specifier'
          ]
        },
        {
          'lhs' => 'comment-sentence',
          'rhs' => [
            'comment-tag',
            '%5c%3a:k10',
            'comment-words',
            'period'
          ]
        },
        {
          'lhs' => 'comment-words',
          'min' => 1,
          'rhs' => [
            'comment-word'
          ]
        },
        {
          'action' => 'q_string',
          'lhs' => 'literal-string',
          'rhs' => [
            'q-string'
          ]
        },
        {
          'action' => 'literal_string',
          'lhs' => 'literal-string',
          'rhs' => [
            'double-quoted-string'
          ]
        },
        {
          'action' => 'literal_string',
          'lhs' => 'literal-string',
          'rhs' => [
            'single-quoted-string'
          ]
        },
        {
          'action' => 'production_sentence',
          'lhs' => 'production-sentence',
          'rhs' => [
            'lhs',
            'production-copula',
            'rhs',
            'period'
          ]
        },
        {
          'lhs' => 'production-copula',
          'rhs' => [
            '%5c%3a:k10'
          ]
        },
        {
          'lhs' => 'production-copula',
          'rhs' => [
            '%5c%3a%5c%3a%5c%3d:k11'
          ]
        },
        {
          'action' => 'symbol_phrase',
          'lhs' => 'symbol-phrase',
          'min' => 1,
          'rhs' => [
            'symbol-word'
          ]
        },
        {
          'action' => 'lhs',
          'lhs' => 'lhs',
          'rhs' => [
            'symbol-phrase'
          ]
        },
        {
          'action' => 'empty_rhs',
          'lhs' => 'rhs',
          'rhs' => []
        },
        {
          'action' => 'comma_separated_rhs',
          'lhs' => 'rhs',
          'min' => 1,
          'rhs' => [
            'rhs-element'
          ],
          'separator' => 'comma'
        },
        {
          'action' => 'sequence_rhs',
          'lhs' => 'rhs',
          'priority' => '1000',
          'rhs' => [
            'symbol-phrase',
            'sequence:k12'
          ]
        },
        {
          'action' => 'optional_sequence_rhs',
          'lhs' => 'rhs',
          'priority' => '2000',
          'rhs' => [
            'optional:k13',
            'symbol-phrase',
            'sequence:k12'
          ]
        },
        {
          'action' => 'separated_sequence_rhs',
          'lhs' => 'rhs',
          'priority' => '2000',
          'rhs' => [
            'symbol-phrase',
            'separated:k14',
            'symbol-phrase',
            'sequence:k12'
          ]
        },
        {
          'action' => 'optional_separated_sequence_rhs',
          'lhs' => 'rhs',
          'priority' => '3000',
          'rhs' => [
            'optional:k13',
            'symbol-phrase',
            'separated:k14',
            'symbol-phrase',
            'sequence:k12'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'rhs-element',
          'rhs' => [
            'mandatory-rhs-element'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'rhs-element',
          'priority' => '1000',
          'rhs' => [
            'optional-rhs-element'
          ]
        },
        {
          'action' => 'mandatory_rhs_element',
          'lhs' => 'mandatory-rhs-element',
          'rhs' => [
            'rhs-symbol-specifier'
          ]
        },
        {
          'action' => 'optional_rhs_element',
          'lhs' => 'optional-rhs-element',
          'rhs' => [
            'optional:k13',
            'rhs-symbol-specifier'
          ]
        },
        {
          'action' => 'rhs_symbol_phrase_specifier',
          'lhs' => 'rhs-symbol-specifier',
          'rhs' => [
            'symbol-phrase'
          ]
        },
        {
          'action' => 'rhs_regex_specifier',
          'lhs' => 'rhs-symbol-specifier',
          'rhs' => [
            'regex'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'terminal-paragraph',
          'rhs' => [
            'non-structural-terminal-sentences',
            'terminal-sentence',
            'non-structural-terminal-sentences'
          ]
        },
        {
          'action' => 'concatenate_lines',
          'lhs' => 'non-structural-terminal-sentences',
          'min' => 0,
          'rhs' => [
            'non-structural-terminal-sentence'
          ]
        },
        {
          'action' => 'regex_terminal_sentence',
          'lhs' => 'terminal-sentence',
          'rhs' => [
            'symbol-phrase',
            'matches:k15',
            'regex',
            'period'
          ]
        },
        {
          'action' => 'string_terminal_sentence',
          'lhs' => 'terminal-sentence',
          'rhs' => [
            'match:k16',
            'symbol-phrase',
            'using:k17',
            'string-specifier',
            'period'
          ]
        },
        {
          'action' => 'first_arg',
          'lhs' => 'string-specifier',
          'rhs' => [
            'literal-string'
          ]
        },
        {
          'action' => 'string_name_specifier',
          'lhs' => 'string-specifier',
          'rhs' => [
            'symbol-phrase'
          ]
        },
        {
          'lhs' => 'trailing-matter',
          'rhs' => [
            'final-comment'
          ]
        },
        {
          'lhs' => 'trailing-matter',
          'rhs' => [
            'final-whitespace'
          ]
        },
        {
          'lhs' => 'trailing-matter',
          'rhs' => []
        }
      ],
      'start' => 'grammar',
      'terminals' => [
        'priority:k0',
        'the:k1',
        'action:k2',
        'is:k3',
        'semantics:k4',
        'perl5:k5',
        'version:k6',
        'version%5c%20number:k7',
        'start:k8',
        'symbol:k9',
        'default:ka',
        'lex:kb',
        'prefix:kc',
        'null:kd',
        'value:ke',
        'are:kf',
        '%5c%3a:k10',
        '%5c%3a%5c%3a%5c%3d:k11',
        'sequence:k12',
        'optional:k13',
        'separated:k14',
        'matches:k15',
        'match:k16',
        'using:k17',
        'q-string',
        'regex',
        'empty-line',
        'final-comment',
        'final-whitespace',
        'bracketed-comment',
        'single-quoted-string',
        'double-quoted-string',
        'version-number',
        'symbol-word',
        'period',
        'integer',
        'comment-tag',
        'comment-word',
        'comma'
      ],
    }
  ],
  'mdlex_options' => [
    {
      'default_prefix' => '(?:[ \\t]*(?:\\n|(?:\\#[^\\n]*\\n)))*[ \\t]*',
      'terminals' => [
        {
          'name' => 'priority:k0',
          'regex' => 'priority'
        },
        {
          'name' => 'the:k1',
          'regex' => 'the'
        },
        {
          'name' => 'action:k2',
          'regex' => 'action'
        },
        {
          'name' => 'is:k3',
          'regex' => 'is'
        },
        {
          'name' => 'semantics:k4',
          'regex' => 'semantics'
        },
        {
          'name' => 'perl5:k5',
          'regex' => 'perl5'
        },
        {
          'name' => 'version:k6',
          'regex' => 'version'
        },
        {
          'name' => 'version%5c%20number:k7',
          'regex' => 'version number'
        },
        {
          'name' => 'start:k8',
          'regex' => 'start'
        },
        {
          'name' => 'symbol:k9',
          'regex' => 'symbol'
        },
        {
          'name' => 'default:ka',
          'regex' => 'default'
        },
        {
          'name' => 'lex:kb',
          'regex' => 'lex'
        },
        {
          'name' => 'prefix:kc',
          'regex' => 'prefix'
        },
        {
          'name' => 'null:kd',
          'regex' => 'null'
        },
        {
          'name' => 'value:ke',
          'regex' => 'value'
        },
        {
          'name' => 'are:kf',
          'regex' => 'are'
        },
        {
          'name' => '%5c%3a:k10',
          'regex' => ':'
        },
        {
          'name' => '%5c%3a%5c%3a%5c%3d:k11',
          'regex' => '::='
        },
        {
          'name' => 'sequence:k12',
          'regex' => 'sequence'
        },
        {
          'name' => 'optional:k13',
          'regex' => 'optional'
        },
        {
          'name' => 'separated:k14',
          'regex' => 'separated'
        },
        {
          'name' => 'matches:k15',
          'regex' => 'matches'
        },
        {
          'name' => 'match:k16',
          'regex' => 'match'
        },
        {
          'name' => 'using:k17',
          'regex' => 'using'
        },
        {
          'builtin' => 'q_quote',
          'name' => 'q-string'
        },
        {
          'builtin' => 'regex',
          'name' => 'regex'
        },
        {
          'name' => 'empty-line',
          'regex' => '^\\h*\\n'
        },
        {
          'name' => 'final-comment',
          'regex' => '\\#[^\\n]*\\Z'
        },
        {
          'name' => 'final-whitespace',
          'regex' => '\\s\\z'
        },
        {
          'name' => 'bracketed-comment',
          'regex' => '\\x{5b}[^\\x{5d}]*\\x{5d}'
        },
        {
          'builtin' => 'single_quote',
          'name' => 'single-quoted-string'
        },
        {
          'builtin' => 'double_quote',
          'name' => 'double-quoted-string'
        },
        {
          'name' => 'version-number',
          'regex' => '\\d+\\.[\\d_.]+\\d'
        },
        {
          'name' => 'symbol-word',
          'regex' => '[a-zA-Z_][a-zA-Z0-9_-]*'
        },
        {
          'name' => 'period',
          'regex' => '\\.'
        },
        {
          'name' => 'integer',
          'regex' => '\\d+'
        },
        {
          'name' => 'comment-tag',
          'regex' => '(to\\s+do|note|comment)'
        },
        {
          'name' => 'comment-word',
          'regex' => '[\\x{21}-\\x{2d}\\x{2f}-\\x{7e}]+'
        },
        {
          'name' => 'comma',
          'regex' => '\\,'
        }
      ]
    }
  ]
};

# To update, copy self.mdl.raw between these two lines

#>>>
## use critic
use strict 'vars';

package main;

my $source = do { local $RS = undef; <> };

## no critic (Variables::ProhibitPackageVars)
my $value = Marpa::MDLex::mdlex(
    [   { action_object => 'Marpa::MDL::Internal::Actions', },
        @{ $Bootstrap_Grammar::data->{marpa_options} }
    ],
    $Bootstrap_Grammar::data->{mdlex_options},
    $source
);
## use critic

my $d = Data::Dumper->new( [ ${$value} ], [qw(data)] );
$d->Sortkeys(1);
$d->Purity(1);
$d->Deepcopy(1);
$d->Indent(1);
say $d->Dump() or Carp::croak("Cannot print: $ERRNO");

