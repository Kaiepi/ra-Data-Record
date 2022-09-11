use v6.d;
use Data::Record;
use Test;

plan 2;

subtest 'exports', {
    plan 13;

    ok Data::Record.WHO<Instance>:exists,
      'Data::Record::Instance gets exported';
    ok Data::Record.WHO<Tuple>:exists,
      'Data::Record::Tuple gets exported';
    ok Data::Record.WHO<List>:exists,
      'Data::Record::List gets exported';
    ok Data::Record.WHO<Map>:exists,
      'Data::Record::Map gets exported';

    ok ::{'&circumfix:«<@ @>»'}:exists,
      '&circumfix:«<@ @>» gets exported';
    ok ::{'&circumfix:<[@ @]>'}:exists,
      '&circumfix:<[@ @]> gets exported';
    ok ::{'&circumfix:<{@ @}>'}:exists,
      '&circumfix:<{@ @}> gets exported';
    ok ::{'&infix:«(><)»'}:exists,
      '&infix:«(><)» gets exported';
    ok ::{'&infix:«(<<)»'}:exists,
      '&infix:«(<<)» gets exported';
    ok ::{'&infix:«(>>)»'}:exists,
      '&infix:«(>>)» gets exported';
    ok ::{'&infix:«(<>)»'}:exists,
      '&infix:«(<>)» gets exported';

    ok (::X.WHO<Data>:exists) && (::X.WHO<Data>.WHO<Record>:exists),
      'X::Data::Record gets exported';
    ok ::X.WHO<Data>.WHO<Record>.WHO<TypeCheck>:exists,
      'X::Data::Record::TypeCheck, and thus all exceptions, get exported';
};

subtest 'records', {
    plan 10;

    my constant Schema = {@
        name  => Str:D,
        items => [@ <@ Int:D, Str:D @> @]
    @} :name('Schema');

    my %data is Map =
        name  => 'Kaiepi',
        items => ((1,'First!!!111!1!one'),);

    lives-ok {
        my % := {
           name  => %data<name>,
           items => %data<items>,
        } (<<) Schema;
    }, 'can instantiate a record type with a consumed record';
    lives-ok {
        my % := {
            name => %data<name>,
        } (>>) Schema;
    }, 'can instantiate a record type with a subsumed record';
    lives-ok {
        my % := {
            name => %data<name>,
        } (<>) Schema;
    }, 'can instantiate a record type with a coerced record';

    my %coerced := %data (><) Schema;
    cmp-ok %coerced, &[~~], Schema,
      'record coercions smartmatch against their types';
    cmp-ok Schema, &[~~], %coerced,
      'record types smartmatch against their coercions';
    cmp-ok Schema.fields.<items>, &[!~~], %coerced,
      'other record types do not smartmatch against record type coercions';
    cmp-ok %data, &[eqv], %coerced,
      'data are equivalent to their record coercions';
    cmp-ok %coerced, &[eqv], %data,
      'record coercions are equivalent to their data';
    cmp-ok %coerced, &[eqv], %coerced,
      'record coercions are equivalent to themselves';

    proto sub is-valid(Mu --> Bool:D)   {*}
    multi sub is-valid(Schema --> True) { }
    multi sub is-valid(Mu --> False)    { }

    ok is-valid(%coerced),
      'record coercions typecheck against their original record type in signatures';
};

# vim: ft=perl6 sw=4 ts=4 sts=4 et
