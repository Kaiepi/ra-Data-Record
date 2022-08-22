use v6.d;
use Data::Record::Exceptions;
use Data::Record::Operators;
use Data::Record::Map;
use Test;

plan 2;

subtest 'non-structural', {
    plan 3;

    subtest 'basic', {
        plan 48;

        my Str:D $name = 'NameMap';
        sub term:<NameMap> { once ({@ name => Str:D @}:$name) }

        lives-ok { NameMap }, 'can create record map types';
        throws-like {
            {@{ foo => Int:D }@}
        }, X::Data::Record::Block,
          'cannot create record map types with hashes';

        is NameMap.^name, $name,
          'names get passed around when creating record map types OK';
        is NameMap.raku, qs[{@ :name(Str:D) @}:name('$name')],
          'record map types have the correct .raku';
        cmp-ok NameMap.for, &[=:=], Map:D,
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
            { } (<>) NameMap
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
        is %map.raku, "$name\.new(%map.record().raku())",
          'maps have the correct .raku';
        is %map.gist, %map.record.gist,
          'the .gist of maps is that of their record';

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

        %map := %(
            name  => 'Kaiepi',
            items => []
        ) (<>) ({@
            name  => Str:D,
            items => Array:D[Str:D]
        @} :name('NameItemsMap'));

        lives-ok {
            %map.push: 'items', 'ok'
        }, 'can push to array fields in a record map using a key/value pair';
        lives-ok {
            %map.push: (items => 'ok 2 electric boogaloo');
        }, 'can push to array fields in a record map using a pair';
        throws-like {
            %map.push: 'name', 'Mrofnet'
        }, X::Data::Record::TypeCheck,
          'cannot push to existing non-array keys in a record map using a key/value pair';
        throws-like {
            %map.push: (name => 'Mrofnet');
        }, X::Data::Record::TypeCheck,
          'cannot push to existing non-array keys in a record map using a pair';
        throws-like {
            %map.push: 'foo', 'bar'
        }, X::Data::Record::OutOfBounds,
          'cannot push a key/value pair whose key does not exist in the record type';
        throws-like {
            %map.push: (foo => 'bar')
        }, X::Data::Record::OutOfBounds,
          'cannot push a pair whose key does not exist in the record type';

        lives-ok {
            %map.append: 'items', <wew lad>
        }, 'can append to array fields in a record map';
        throws-like {
            %map.append: 'name', 'Mrofnet'
        }, X::Data::Record::TypeCheck,
          'cannot append to existing keys non-array keys in a record map';
        throws-like {
            %map.append: 'foo', 'bar'
        }, X::Data::Record::OutOfBounds,
          'cannot append a value for a key that does not exist in the record type';
    };

    subtest 'generic', {
        plan 4;

        sub term:<PValueMap>    { once ({@{ value => $^a }@}:name<PValueMap>) }
        sub term:<PStrValueMap> { once (PValueMap.^parameterize: Str:D) }

        lives-ok { PValueMap }, 'can create a generic map';
        lives-ok { PStrValueMap }, 'can parameterize generic maps';

        cmp-ok {value => 'ppopcorm.......'}, &[~~], PStrValueMap,
          'can typecheck against generic maps';
        lives-ok {
            {value => 'ლ(´ڡ`ლ)'} (<>) PStrValueMap
        }, 'can instantiate generic maps';
    };

    subtest 'nested', {
        plan 8;

        sub term:<NNameMap> { once ({@ name => {@ value => Str:D @} @}:name<NNameMap>) }

        lives-ok { NNameMap }, 'can create nested maps';
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
};

subtest 'structural', {
    plan 3;

    subtest 'basic', {
        plan 49;

        my Str:D $name = 'NameMap';
        sub term:<NameMap> { once ({@ name => Str:D @}:structural:$name) }

        lives-ok { NameMap }, 'can create structural record map types';
        throws-like {
            {@{ foo => Int:D }@} :structural
        }, X::Data::Record::Block,
          'cannot create structural record map types with hashes';

        is NameMap.^name, $name,
          'names get passed around when creating structural record map types OK';
        is NameMap.raku, qs[{@ :name(Str:D) @}:structural:name('$name')],
          'structural record map types have the correct .raku';
        cmp-ok NameMap.for, &[=:=], Map:D,
          'structural record map types are for maps';
        cmp-ok NameMap.fields, &[eqv], Map.new((name => Str:D,)),
          'structural record map types have the correct parameters';
        cmp-ok NameMap.fields.<name>.VAR, &[!~~], Scalar,
          'structural record map types handle the containers of its fields OK';

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
        lives-ok {
            { name => 'Kaiepi', ayy => 'lmao' } (>>) NameMap
        }, '(>>) lives for maps with extraneous fields';
        throws-like {
            { name => 42 } (>>) NameMap
        }, X::Data::Record::TypeCheck,
          '(>>) throws for unacceptable maps';

        lives-ok {
            { name => 'Kaiepi' } (<>) NameMap
        }, '(<>) lives for acceptable maps';
        throws-like {
            { } (<>) NameMap
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
        lives-ok {
            { name => 'Kaiepi', ayy => 'lmao' } (><) NameMap
        }, '(><) lives for maps with extraneous fields';
        throws-like {
            { name => 42 } (<>) NameMap
        }, X::Data::Record::TypeCheck,
          '(><) throws for unacceptable maps';

        my %map := { name => 'Kaiepi' } (><) NameMap;
        is %map.raku, "$name\.new(%map.record().raku())",
          'maps have the correct .raku';
        is %map.gist, %map.record.gist,
          'the .gist of maps is that of their record';

        ok %map<name>:exists,
          'can check if keys exist in a map';

        cmp-ok %map<name>, &[===], 'Kaiepi',
          'can get values of keys in a map';
        lives-ok {
            %map<ayy>
        }, 'can get values of keys in a map that are not in its fields';

        cmp-ok (%map<name> = 'Ben Davies'), &[===], 'Ben Davies',
          'can assign to keys in a map...';
        throws-like {
            %map<name> = 42
        }, X::Data::Record::TypeCheck,
          '...but only if they typecheck against their corresponding field';
        lives-ok {
            %map<foo> = 'foo'
        }, 'can assign anything to keys in a map that are not in its fields';

        cmp-ok (%map<name> := 'Ben Dover'), &[===], 'Ben Dover',
          'can bind to keys in a map...';
        throws-like {
            %map<name> := 42
        }, X::Data::Record::TypeCheck,
          '...but only if they typecheck against their corresponding field';
        lives-ok {
            %map<foo> := 'foo'
        }, 'can bind anything to keys in a map that are not in its fields';

        throws-like {
            %map<name>:delete
        }, X::Data::Record::Immutable,
          'cannot delete keys from a map if they are in its fields';
        lives-ok {
            %map<foo>:delete
        }, 'can delete keys from a map if they are not in its fields';

        %map := %(
            name  => 'Kaiepi',
            items => []
        ) (<>) ({@
            name  => Str:D,
            items => Array:D[Str:D]
        @} :structural :name('NameItemsMap'));

        lives-ok {
            %map.push: 'items', 'ok'
        }, 'can push to array fields in a structural record map using a key/value pair';
        lives-ok {
            %map.push: (items => 'ok 2 electric boogaloo');
        }, 'can push to array fields in a structural record map using a pair';
        throws-like {
            %map.push: 'name', 'Mrofnet'
        }, X::Data::Record::TypeCheck,
          'cannot push to existing non-array keys in a structural record map using a key/value pair';
        throws-like {
            %map.push: (name => 'Mrofnet');
        }, X::Data::Record::TypeCheck,
          'cannot push to existing non-array keys in a structural record map using a pair';
        lives-ok {
            %map.push: 'foo', 'bar'
        }, 'can push a key/value pair whose key does not exist in the structural record type';
        lives-ok {
            %map.push: (foo => 'bar')
        }, 'can push a pair whose key does not exist in the structural record type';
        %map<foo>:delete;

        lives-ok {
            %map.append: 'items', <wew lad>
        }, 'can append to array fields in a structural record map';
        throws-like {
            %map.append: 'name', 'Mrofnet'
        }, X::Data::Record::TypeCheck,
          'cannot append to existing keys non-array keys in a structural record map';
        lives-ok {
            %map.append: 'foo', 'bar'
        }, 'can append a value for a key that does not exist in the structural record type';
        %map<foo>:delete;
    };

    subtest 'generic', {
        plan 4;

        sub term:<PValueMap>    { once ({@{ value => $^a }@}:structural:name<PValueMap>) }
        sub term:<PStrValueMap> { once (PValueMap.^parameterize: Str:D) }

        lives-ok { PValueMap }, 'can create a generic structural map';
        lives-ok { PStrValueMap }, 'can parameterize generic maps';

        cmp-ok {value => 'ppopcorm.......'}, &[~~], PStrValueMap,
          'can typecheck against generic maps';
        lives-ok {
            {value => 'ლ(´ڡ`ლ)'} (<>) PStrValueMap
        }, 'can instantiate generic maps';
    };

    subtest 'nested', {
        plan 8;

        sub term:<NNameMap> { once ({@ name => {@ value => Str:D @}:structural @}:structural:name<NNameMap>) }

        lives-ok { NNameMap }, 'can create nested structural maps';
        cmp-ok {name => {value => 'ok'}}, &[~~], NNameMap,
          'can typecheck against nested structural maps';

        my %instance;
        lives-ok {
            %instance := {name => {value => 'ok'}} (><) NNameMap
        }, 'can instantiate nested maps with maps containing maps';
        lives-ok {
            %instance.record (><) NNameMap
        }, 'can instantiate nested maps with maps containing structural records';
        throws-like {
            %instance.fields (><) NNameMap
        }, X::Data::Record::TypeCheck,
          'cannot instantiate nested maps with maps containing structural record type objects';

        my %unrecord;
        lives-ok {
            %unrecord := %instance.unrecord;
        }, 'can unrecord nested maps...';
        isa-ok %unrecord, Map,
          '...yielding a map...';
        isa-ok %unrecord<name>, Map,
          '...of maps';
    };
};


# vim: ft=perl6 sw=4 ts=4 sts=4 et
