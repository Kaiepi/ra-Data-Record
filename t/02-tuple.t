use v6.d;
use Data::Record::Exceptions;
use Data::Record::Operators;
use Data::Record::Tuple;
use Test;

plan 4;

subtest 'basic', {
    plan 45;

    my Str:D $name = 'IntTuple';

    only term:<IntTuple> {
        once <@ Int:D @>:$name
    }

    lives-ok { IntTuple }, 'can create tuple record types';
    is IntTuple.^name, $name,
      'names get passed around when creating tuple record types OK';
    is IntTuple.raku, "<@ Int:D @>:name('$name')",
      'tuple record types have the correct .raku';
    cmp-ok IntTuple.for, &[=:=], List:D,
      'tuple record types are for lists';
    cmp-ok IntTuple.fields, &[eqv], (Int:D,),
      'tuple record types have the correct fields';
    cmp-ok IntTuple.fields[0].VAR, &[!~~], Scalar,
      'tuple record types handle the containers of its fields OK';

    cmp-ok (1,2,), &[!~~], IntTuple,
      'cannot typecheck tuples with the wrong arity';
    cmp-ok ('1',), &[!~~], IntTuple,
      'cannot typecheck tuples with the correct arity, but wrong typing';
    cmp-ok (1,), &[~~], IntTuple,
      'can typecheck tuples with the correct arity and typing';

    lives-ok {
        (1,) (<<) IntTuple
    }, '(<<) lives for acceptable lists';
    throws-like {
        () (<<) IntTuple
    }, X::Data::Record::Missing,
      '(<<) throws for underfilled lists';
    lives-ok {
        (1,2,) (<<) IntTuple
    }, '(<<) lives for overfilled lists';
    throws-like {
        ('1',) (<<) IntTuple
    }, X::Data::Record::TypeCheck,
      '(<<) throws for unacceptable lists';

    lives-ok {
        (1,) (>>) IntTuple
    }, '(>>) lives for acceptable lists';
    throws-like {
        () (>>) IntTuple
    }, X::Data::Record::Definite,
      '(>>) throws for underfilled lists if missing values must be definite...';
    lives-ok {
        () (>>) <@ Int:_ @>
    }, '...but lives otherwise';
    throws-like {
        (1,2,) (>>) IntTuple
    }, X::Data::Record::Extraneous,
      '(>>) throws for overfilled lists';
    throws-like {
        ('1',) (>>) IntTuple
    }, X::Data::Record::TypeCheck,
      '(>>) throws for unacceptable lists';

    lives-ok {
        (1,) (<>) IntTuple
    }, '(<>) lives for acceptable lists';
    throws-like {
        () (<>) IntTuple
    }, X::Data::Record::Definite,
      '(<>) throws for underfilled lists if missing values must be definite...';
    lives-ok {
        () (<>) <@ Int:_ @>
    }, '...but lives otherwise';
    lives-ok {
        (1,2,) (<>) IntTuple
    }, '(<>) lives for overfilled lists';
    throws-like {
        ('1',) (<>) IntTuple
    }, X::Data::Record::TypeCheck,
      '(<>) throws for unacceptable lists';

    lives-ok {
        (1,) (><) IntTuple
    }, '(><) lives for acceptable lists';
    throws-like {
        () (><) IntTuple
    }, X::Data::Record::Missing,
      '(><) throws for underfilled lists';
    throws-like {
        (1,2,) (><) IntTuple
    }, X::Data::Record::Extraneous,
      '(><) throws for overfilled lists';
    throws-like {
        ('1',) (><) IntTuple
    }, X::Data::Record::TypeCheck,
      '(><) throws for unacceptable lists';

    my @record := [1];
    my @tuple  := @record (><) IntTuple;
    is @tuple.raku, "$name\.new(@tuple.record().raku())",
      'tuples have the correct .raku';
    is @tuple.gist, @tuple.record.gist,
      'the .gist of tuples is that of their record';

    ok @tuple[0]:exists,
      'can check if positions exist in a tuple';
    nok @tuple[1]:exists,
      'can check if positions do not exist in a tuple';

    cmp-ok (@tuple[0] = 2), &[===], 2,
      'can assign to positions in a tuple';
    throws-like {
        @tuple[0] = 'ayy lmao'
    }, X::Data::Record::TypeCheck,
      'cannot assign to positions in a tuple if the value does not typecheck';
    throws-like {
        @tuple[1] = 3
    }, X::Data::Record::OutOfBounds,
      'cannot assign to positions that are out of bounds for a tuple';

    cmp-ok (@tuple[0] := 3), &[===], 3,
      'can bind positions in a tuple';
    throws-like {
        @tuple[0] := 'ayy lmao'
    }, X::Data::Record::TypeCheck,
      'cannot bind to positions in a tuple if the value does not typecheck';
    throws-like {
        @tuple[1] := 4
    }, X::Data::Record::OutOfBounds,
      'cannot bind to positions that are out of bounds for a tuple';

    cmp-ok @tuple[0], &[===], 3,
      'can get values for positions in a tuple';
    throws-like {
        @tuple[0]:delete
    }, X::Data::Record::Immutable,
      'cannot delete positions of a tuple';

    throws-like {
        @tuple.push: 4
    }, X::Data::Record::Immutable,
      'cannot push to a tuple';
    throws-like {
        @tuple.pop
    }, X::Data::Record::Immutable,
      'cannot pop a tuple';
    throws-like {
        @tuple.shift
    }, X::Data::Record::Immutable,
      'cannot shift a tuple';
    throws-like {
        @tuple.unshift: 2
    }, X::Data::Record::Immutable,
      'cannot unshift a tuple';
    throws-like {
        @tuple.append: 4, 5, 6
    }, X::Data::Record::Immutable,
      'cannot append to a tuple';
    throws-like {
        @tuple.prepend: 1, 2
    }, X::Data::Record::Immutable,
      'cannot prepend to a tuple';
};

subtest 'generic', {
    plan 4;

    only term:<PTuple> {
        once <@{ $^a }@>:name<PTuple>
    }
    only term:<PIntTuple> {
        once PTuple.^parameterize: Int:D
    }

    lives-ok { PTuple }, 'can create a generic tuple';
    lives-ok { PIntTuple }, 'can parameterize generic tuples';

    cmp-ok (1,), &[~~], PIntTuple,
      'can typecheck against generic tuples';
    lives-ok {
        (1,) (><) PIntTuple;
    }, 'can instantiate generic tuples';
};

subtest 'nested', {
    plan 8;

    only term:<NIntTuple> {
        once <@ <@ Int:D @> @>:name<NIntTuple>
    }

    lives-ok { NIntTuple }, 'can create nested tuples';
    cmp-ok ((1,),), &[~~], NIntTuple,
      'can typecheck against nested tuples';

    my @instance;
    lives-ok {
        @instance := ((1,),) (><) NIntTuple;
    }, 'can instantiate nested tuples with lists of lists';
    lives-ok {
        @instance.record (><) NIntTuple
    }, 'can instantiate nested tuples with lists of records';
    throws-like {
        @instance.fields (><) NIntTuple
    }, X::Data::Record::TypeCheck,
      'cannot instantiate nested tuples with lists of record type objecs';

    my @unrecord;
    lives-ok {
        @unrecord := @instance.unrecord;
    }, 'can unrecord nested tuples..';
    cmp-ok @unrecord, &[~~], List:D,
      '...yielding a list...';
    cmp-ok @unrecord[0], &[~~], List:D,
      '...of lists';
};

subtest 'lazy', {
    plan 3;

    only term:<IntTuple> {
        once <@ Int:D @>:name<IntTuple>
    }

    my $reified := Promise.new;
    my @instance;
    lives-ok {
        @instance := (lazy gather {
            $reified.keep;
            take 1;
        }).list (><) IntTuple;
    }, 'can create lazy tuples...';
    nok $reified, '...which are not reified...';
    @instance[0];
    ok $reified, '...until they should be';
};

# vim: ft=perl6 sw=4 ts=4 sts=4 et
