use v6.e.PREVIEW;
use Data::Record::Exceptions;
use Data::Record::Operators;
use Data::Record::List;
use Test;

plan 4;

subtest 'basic', {
    plan 36;

    my Mu    \IntList = Mu;
    my Str:D $name    = 'IntList';
    lives-ok {
        IntList := [@ Int:D @] :$name;
    }, 'can create record list types';
    throws-like {
        [@{ foo => Int:D }@]
    }, X::Data::Record::Block,
      'cannot create record list types with hashes';

    is IntList.^name, $name,
      'names get passed around when creating record list types OK';
    is IntList.raku, "[@ Int:D @]:name('$name')",
      'record list types have the correct .raku';
    cmp-ok IntList.for, &[=:=], List,
      'record list types are for lists';
    cmp-ok IntList.fields, &[eqv], (Int:D,),
      'record list types have the correct parameters';
    cmp-ok IntList.fields[0].VAR, &[!~~], Scalar,
      'record list types handle the containers of its fields OK';

    cmp-ok (1,2,3,), &[~~], IntList,
      'can typecheck acceptable lists';
    cmp-ok ('4','5','6',), &[!~~], IntList,
      'cannot typecheck unacceptable lists';

    lives-ok {
        (1,) (<<) IntList
    }, '(<<) lives for acceptable lists';
    lives-ok {
        (1,'2',) (<<) IntList
    }, '(<<) consumes unacceptable values in lists';

    lives-ok {
        (1,) (>>) IntList
    }, '(>>) lives for acceptable lists';
    throws-like {
        (1,'2',) (>>) IntList
    }, X::Data::Record::TypeCheck,
        '(>>) throws for unacceptable lists';

    lives-ok {
        (1,) (<>) IntList
    }, '(<>) lives for acceptable lists';
    lives-ok {
        (1,'2',) (<>) IntList
    }, '(<>) coerces lists';

    lives-ok {
        (1,) (><) IntList
    }, '(><) lives for acceptable lists';
    throws-like {
        (1,'2',) (><) IntList
    }, X::Data::Record::TypeCheck,
      '(><) throws for unacceptable lists';

    my @record := [1];
    my @list   := @record (><) IntList;
    ok @list[0]:exists,
      'can check if positions exist in a list';
    cmp-ok @list[0], &[===], 1,
      'can check values for positions of a list';
    cmp-ok (@list[0] = 2), &[===], 2,
      'can assign to positions in a list';
    throws-like {
        @list[0] = 'ayy lmao'
    }, X::Data::Record::TypeCheck,
      'cannot assign to positions in a list if the value does not typecheck';
    cmp-ok (@list[0] := 3), &[===], 3,
      'can bind to positions in a list';
    throws-like {
        @list[0] := 'ayy lmao';
    }, X::Data::Record::TypeCheck,
      'cannot bind to positions in a list if the value does not typecheck';
    cmp-ok @list[0]:delete, &[===], 3,
      'can delete positions in a list';

    @list.push: 1;
    cmp-ok @list, &[eqv], [1],
      'can push a value to a list';
    @list.push: 2, 3;
    cmp-ok @list, &[eqv], [1, 2, 3],
      'can push values to a list';
    throws-like {
        @list.push: '4'
    }, X::Data::Record::TypeCheck,
      'cannot push a value to a list if it does not typecheck';
    throws-like {
        @list.push: '4', '5', '6'
    }, X::Data::Record::TypeCheck,
      'cannot push values to a list if they do not typecheck';

    @list.pop;
    cmp-ok @list, &[eqv], [1, 2],
      'can pop a value from a list';
    @list.pop for ^2;

    @list.unshift: 3;
    cmp-ok @list, &[eqv], [3],
      'can unshift a value to a list';
    @list.unshift: 1, 2;
    cmp-ok @list, &[eqv], [1,2,3],
      'can unshift values to a list';
    throws-like {
        @list.unshift: '0'
    }, X::Data::Record::TypeCheck,
      'cannot unshift a value to a list if it does not typecheck';
    throws-like {
        @list.unshift: '-2', '-1', '0'
    }, X::Data::Record::TypeCheck,
      'cannot unshift values to a list if they do not typecheck';

    @list.shift;
    cmp-ok @list, &[eqv], [2, 3],
      'can unshift a value from a list';
    @list.shift for ^2;

    @list.prepend: 1, 2, 3;
    cmp-ok @list, &[eqv], [1, 2, 3],
      'can prepend values to a list';

    @list.append: 4, 5, 6;
    cmp-ok @list, &[eqv], [1, 2, 3, 4, 5, 6],
      'can append values to a list';
};

subtest 'generic', {
    plan 4;

    my Mu \PList    = Mu;
    my Mu \PIntList = Mu;
    lives-ok {
        PList := [@{ $^a }@] :name('PList');
    }, 'can create a generic list';
    lives-ok {
        PIntList := PList.^parameterize: Int:D;
    }, 'can parameterize generic lists';

    cmp-ok (1,), &[~~], PIntList,
      'can typecheck against generic lists';
    lives-ok {
        () (<>) PIntList
    }, 'can instantiate generic lists';
};

subtest 'nested', {
    plan 8;

    my Mu \NIntList = Mu;
    lives-ok {
        NIntList := [@ [@ Int:D @] @] :name('NIntList');
    }, 'can create nested lists';
    cmp-ok ((1,),), &[~~], NIntList,
      'can typecheck against nested lists';

    my @instance;
    lives-ok {
        @instance := ((1,),) (><) NIntList
    }, 'can instantiate nested lists with lists of lists';
    lives-ok {
        @instance.record (><) NIntList
    }, 'can instantiate nested lists with lists of records';
    throws-like {
        @instance.fields (><) NIntList
    }, X::Data::Record::TypeCheck,
      'cannot instantiate nested lists with lists of record type objects';

    my @unrecord;
    lives-ok {
        @unrecord := @instance.unrecord;
    }, 'can unrecord a nested list...';
    cmp-ok @unrecord, &[~~], List:D,
      '...yielding a list...';
    cmp-ok @unrecord[0], &[~~], List:D,
      '...of lists';
};

subtest 'lazy', {
    plan 3;

    my Mu        \IntList    = [@ Int:D @] :name('IntList');
    my Promise:D $reified   .= new;
    my           @instance;
    lives-ok {
        @instance := (lazy gather {
            $reified.keep;
            take 1;
        }).list (><) IntList;
    }, 'can create lazy lists...';
    nok $reified, '...which are not reified...';
    @instance[0];
    ok $reified, '...until they should be';
};

# vim: ft=perl6 sw=4 ts=4 sts=4 et
