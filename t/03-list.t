use v6.d;
use Data::Record::Exceptions;
use Data::Record::Operators;
use Data::Record::List;
use Test;

plan 4;

subtest 'basic', {
    plan 51;

    my Mu    \IntList = Mu;
    my Str:D $name    = 'IntList';
    lives-ok {
        IntList := [@ Int:D @] :$name;
    }, 'can create record list types';

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
    is @list.raku, "$name\.new(@list.record().raku())",
      'lists have the correct .raku';
    is @list.gist, @list.record.gist,
      'the .gist of lists is that of their record';

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

    my Mu \IntStrList = Nil;
    lives-ok {
        IntStrList := [@ Int:D, Str:D @] :name('IntStrList');
    }, 'can create multi-field record list types';

    nok (1,'a',2) ~~ IntStrList,
      'cannot typecheck lists with the wrong arity for a multi-field list type';

    my @stream;
    throws-like {
        [1] (>>) IntStrList
    }, X::Data::Record::Definite,
      'subsuming or coercing a list with the wrong arity for a multi-field list type throws for missing, definite fields...';
    lives-ok {
        [1] (>>) [@ Int:D, Str:_ @]
    }, '...but lives for any other type of missing field';
    throws-like {
        [1] (<<) IntStrList
    }, X::Data::Record::Missing,
      'wrapping or consuming a list with the wrong arity for a multi-field list type throws for missing fields';
    lives-ok {
        @stream := [1,'2'] (><) IntStrList
    }, 'can wrap a list with the correct arity for a multi-field list type';

    throws-like {
        @stream.push: 3
    }, X::Data::Record::Missing,
      'pushing one value to a multi-field list throws';
    throws-like {
        @stream.push: 3, '4', 5
    }, X::Data::Record::Missing,
      'pushing valuess with the wrong arity for a multi-field list throws';
    lives-ok {
        @stream.push: 3, '4'
    }, 'can push valuess with the correct arity for a multi-field list';

    throws-like {
        @stream.pop
    }, X::Data::Record::Missing,
      'cannot pop from a multi-field list';

    throws-like {
        @stream.shift
    }, X::Data::Record::Missing,
      'cannot shift from a multi-field list';

    throws-like {
        @stream.unshift: 0
    }, X::Data::Record::Missing,
      'unshifting one value to a multi-field list throws';
    throws-like {
        @stream.unshift: -2, '1', 0
    }, X::Data::Record::Missing,
      'unshifting values with the wrong arity for a multi-field list throws';
    lives-ok {
        @stream.unshift: -1, '0'
    }, 'can unshift values with the correct arity for a multi-field list';
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
    plan 4;

    my Mu        \IntList    = [@ Int:D @] :name('IntList');
    my Promise:D $reified   .= new;
    my           @instance;
    lives-ok {
        @instance := (lazy gather {
            $reified.keep;
            take 1;
        }).Array (><) IntList;
    }, 'can create lazy arrays...';
    nok $reified, '...which are not reified...';
    @instance[0];
    ok $reified, '...until they should be';
    throws-like {
        @instance.push: 2
    }, X::Cannot::Lazy,
      'pushing to lazy arrays does not throw until Array decides it should';
};

# vim: ft=perl6 sw=4 ts=4 sts=4 et
