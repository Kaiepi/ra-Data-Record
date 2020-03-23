use v6.e.PREVIEW;
use Data::Record;
use Test;

plan 9;

my constant Schema = {@
    name  => Str:D,
    items => [@ <@ Int:D, Str:D @> @]
@} :name('Schema');

my %data =
    name => 'Kaiepi',
    items => ((1,'First!!!111!1!one'),);

lives-ok {
    my % := {
       name  => %data<name>,
       items => (|%data<items>, (2,))
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

# vim: ft=perl6 sw=4 ts=4 sts=4 et
