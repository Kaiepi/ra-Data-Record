use v6.d;
use Data::Record::Exceptions;
use Data::Record::Operators;
use Data::Record::Map;
use Test;

plan 3;

subtest 'basic', {
    plan 43;

    my Mu    \NameMap = Mu;
    my Str:D $name    = 'NameMap';
    lives-ok {
        NameMap := {@ name => Str:D @} :$name;
    }, 'can create record map types';
    throws-like {
        {@{ foo => Int:D }@}
    }, X::Data::Record::Block,
      'cannot create record map types with hashes';

    is NameMap.^name, $name,
      'names get passed around when creating record map types OK';
    is NameMap.raku, qs[{@ :name(Str:D) @}:name('$name')],
      'record map types have the correct .raku';
    cmp-ok NameMap.for, &[=:=], Map,
      'record map types are for maps';
    cmp-ok NameMap.fields, &[eqv], Map.new((name => Str:D,)),
      'record map types have the correct parameters';
    cmp-ok NameMap.fields.<name>.VAR, &[!~~], Scalar,
      'record map types handle the containers of its fields OK';

    cmp-ok { name => 'Kaiepi' }, &[~~], NameMap,
      'can typecheck acceptable maps';
    cmp-ok { name => 42 }, &[!~~], NameMap,
      'cannot typecheck unacceptable maps';

    lives-ok {
        { name => 'Kaiepi' } (<<) NameMap
    }, '(<<) lives for acceptable maps';
    throws-like {
        { } (<<) NameMap
    }, X::Data::Record::Missing,
      '(<<) throws for maps missing fields';
    lives-ok {
        { name => 'Kaiepi', ayy => 'lmao' } (<<) NameMap
    }, '(<<) lives for maps with extraneous fields';
    throws-like {
        { name => 42 } (<<) NameMap
    }, X::Data::Record::TypeCheck,
      '(<<) throws for unacceptable maps';

    lives-ok {
        { name => 'Kaiepi' } (>>) NameMap
    }, '(>>) lives for acceptable maps';
    throws-like {
        { } (>>) NameMap
    }, X::Data::Record::Definite,
      '(>>) throws if a field does not exist in a map and it is a definite type...';
    lives-ok {
        { } (>>) {@ name => Str:_ @}
    }, '...but lives otherwise';
    throws-like {
        { name => 'Kaiepi', ayy => 'lmao' } (>>) NameMap
    }, X::Data::Record::Extraneous,
      '(>>) throws with extraneous fields';
    throws-like {
        { name => 42 } (>>) NameMap
    }, X::Data::Record::TypeCheck,
      '(>>) throws for unacceptable maps';

    lives-ok {
        { name => 'Kaiepi' } (<>) NameMap
    }, '(<>) lives for acceptable maps';
    throws-like {
        { } (>>) NameMap
    }, X::Data::Record::Definite,
      '(<>) throws if a field does not exist in a map and it is a definite type...';
    lives-ok {
        { } (<>) {@ name => Str:_ @}
    }, '...but lives otherwise';
    lives-ok {
        { name => 'Kaiepi', ayy => 'lmao' } (<>) NameMap
    }, '(<>) lives with extraneous fields';
    throws-like {
        { name => 42 } (<>) NameMap
    }, X::Data::Record::TypeCheck,
      '(<>) throws for unacceptable maps';

    lives-ok {
        { name => 'Kaiepi' } (><) NameMap
    }, '(><) lives with acceptable maps';
    throws-like {
        { } (><) NameMap
    }, X::Data::Record::Missing,
      '(><) throws with missing fields';
    throws-like {
        { name => 'Kaiepi', ayy => 'lmao' } (><) NameMap
    }, X::Data::Record::Extraneous,
      '(><) throws with extraneous fields';
    throws-like {
        { name => 42 } (<>) NameMap
    }, X::Data::Record::TypeCheck,
      '(><) throws for unacceptable maps';

    my %map := { name => 'Kaiepi' } (><) NameMap;
    ok %map<name>:exists,
      'can check if keys exist in a map';

    cmp-ok %map<name>, &[===], 'Kaiepi',
      'can get values of keys in a map...';
    throws-like {
        %map<ayy>
    }, X::Data::Record::OutOfBounds,
      '...but only if they exist';

    cmp-ok (%map<name> = 'Ben Davies'), &[===], 'Ben Davies',
      'can assign to keys in a map...';
    throws-like {
        %map<foo> = 'foo'
    }, X::Data::Record::OutOfBounds,
      '...but only if they exist in the record type...';
    throws-like {
        %map<name> = 42
    }, X::Data::Record::TypeCheck,
      '...and only if they typecheck against their corresponding field';

    cmp-ok (%map<name> := 'Ben Dover'), &[===], 'Ben Dover',
      'can bind to keys in a map...';
    throws-like {
        %map<foo> := 'foo'
    }, X::Data::Record::OutOfBounds,
      '...but only if they exist in the record type...';
    throws-like {
        %map<name> := 42
    }, X::Data::Record::TypeCheck,
      '...and only if they typecheck against their corresponding field';

    throws-like {
        %map<name>:delete
    }, X::Data::Record::Immutable,
      'cannot delete keys from a map';

    throws-like {
        %map.push: 'foo', 'bar'
    }, X::Data::Record::Immutable,
      'cannot push to a map';
    throws-like {
        %map.pop
    }, X::Data::Record::Immutable,
      'cannot pop from a map';
    throws-like {
        %map.shift
    }, X::Data::Record::Immutable,
      'cannot shift from a map';
    throws-like {
        %map.unshift: 'foo', 'bar'
    }, X::Data::Record::Immutable,
      'cannot unshift to a map';
    throws-like {
        %map.append: { foo => 'bar' }
    }, X::Data::Record::Immutable,
      'cannot append to a map';
    throws-like {
        %map.prepend: { foo => 'bar' }
    }, X::Data::Record::Immutable,
      'cannot prepend to a map';
};

subtest 'generic', {
    plan 4;

    my Mu \PValueMap    = Mu;
    my Mu \PStrValueMap = Mu;
    lives-ok {
        PValueMap := {@{ value => $^a }@} :name('PValueMap');
    }, 'can create a generic map';
    lives-ok {
        PStrValueMap := PValueMap.^parameterize: Str:D;
    }, 'can parameterize generic maps';

    cmp-ok {value => 'ppopcorm.......'}, &[~~], PStrValueMap,
      'can typecheck against generic maps';
    lives-ok {
        {value => 'ლ(´ڡ`ლ)'} (<>) PStrValueMap
    }, 'can instantiate generic maps';
};

subtest 'nested', {
    plan 8;

    my Mu \NNameMap = Mu;
    lives-ok {
        NNameMap := {@ name => {@ value => Str:D @} @} :name('NNameMap');
    }, 'can create nested maps';
    cmp-ok {name => {value => 'ok'}}, &[~~], NNameMap,
      'can typecheck against nested maps';

    my %instance;
    lives-ok {
        %instance := {name => {value => 'ok'}} (><) NNameMap
    }, 'can instantiate nested maps with maps containing maps';
    lives-ok {
        %instance.record (><) NNameMap
    }, 'can instantiate nested maps with maps containing records';
    throws-like {
        %instance.fields (><) NNameMap
    }, X::Data::Record::TypeCheck,
      'cannot instantiate nested maps with maps containing record type objects';

    my %unrecord;
    lives-ok {
        %unrecord := %instance.unrecord;
    }, 'can unrecord nested maps...';
    isa-ok %unrecord, Map,
      '...yielding a map...';
    isa-ok %unrecord<name>, Map,
      '...of maps';
};

# vim: ft=perl6 sw=4 ts=4 sts=4 et
