use v6.d;
use Data::Record::Instance;
use Data::Record::Exceptions;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
unit role Data::Record::Map[Bool:D :$structural! where !*]
     does Data::Record::Instance[Map]
     does Iterable
     does Associative;

has %!record;

submethod BUILD(::?CLASS:D: :%record! --> Nil) {
    %!record := %record;
}

multi method new(::?CLASS:_: Map:D $original is raw) {
    my %record := self.wrap: $original;
    self.bless: :%record
}
multi method new(::?CLASS:_: Map:D $original is raw, Bool:D :$consume! where ?*) {
    my %record := self.consume: $original;
    self.bless: :%record
}
multi method new(::?CLASS:_: Map:D $original is raw, Bool:D :$subsume! where ?*) {
    my %record := self.subsume: $original;
    self.bless: :%record
}
multi method new(::?CLASS:_: Map:D $original is raw, Bool:D :$coerce! where ?*) {
    my %record := self.coerce: $original;
    self.bless: :%record
}

sub take-record(Mu $field is raw, Mu $key is raw, Mu $value is raw, *%named-args --> Nil) {
    if $value ~~ Data::Record::Instance {
        if $value.DEFINITE {
            take-rw ($key => $value ~~ $field
                          ?? $value
                          !! $field.new: $value.record, |%named-args);
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => 'map reification',
                expected  => $field,
                got       => $value;
        }
    } elsif $value ~~ $field.for {
        take-rw ($key => $field.new: $value, |%named-args);
    } else {
        die X::Data::Record::TypeCheck.new:
            operation => 'map reification',
            expected  => $field,
            got       => $value;
    }
}

method wrap(::THIS ::?CLASS:_: Map:D $original is raw --> Map:D) {
    $original.new: gather for ($original.keys ∪ %.fields.keys).keys -> Mu $key is raw {
        die X::Data::Record::Extraneous.new(
            operation => 'map reification',
            type      => THIS,
            what      => 'key',
            key       => $key,
        ) unless %.fields{$key}:exists;

        die X::Data::Record::Missing.new(
            operation => 'map reification',
            type      => THIS,
            what      => 'key',
            key       => $key,
        ) unless $original{$key}:exists;

        my Mu $field := %.fields{$key};
        my Mu $value := $original{$key};
        if $field ~~ Data::Record::Instance {
            take-record $field, $key, $value;
        } elsif $value ~~ $field {
            take-rw ($key => $value);
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => 'map reification',
                expected  => $field,
                got       => $value;
        }
    }
}

method consume(::THIS ::?CLASS:_: Map:D $original is raw --> Map:D) {
    $original.new: gather for ($original.keys ∪ %.fields.keys).keys -> Mu $key is raw {
        next unless %.fields{$key}:exists;

        die X::Data::Record::Missing.new(
            operation => 'map reification',
            type      => THIS,
            what      => 'key',
            key       => $key,
        ) unless $original{$key}:exists;

        my Mu $field := %.fields{$key};
        my Mu $value := $original{$key};
        if $field ~~ Data::Record::Instance {
            take-record $field, $key, $value, :consume;
        } elsif $value ~~ $field {
            take-rw ($key => $value);
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => 'map reification',
                expected  => $field,
                got       => $value;
        }
    }
}

method subsume(::THIS ::?CLASS:_: Map:D $original is raw --> Map:D) {
    $original.new: gather for ($original.keys ∪ %.fields.keys).keys -> Mu $key is raw {
        die X::Data::Record::Extraneous.new(
            operation => 'map reification',
            type      => THIS,
            what      => 'key',
            key       => $key,
        ) unless %.fields{$key}:exists;

        my Mu $field := %.fields{$key};
        if $original{$key}:exists {
            my Mu $value := $original{$key};
            if $field ~~ Data::Record::Instance {
                take-record $field, $key, $value, :subsume;
            } elsif $value ~~ $field {
                take-rw ($key => $value);
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => 'map reification',
                    expected  => $field,
                    got       => $value;
            }
        } elsif $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite {
            die X::Data::Record::Definite.new:
                type  => THIS,
                what  => 'key',
                key   => $key,
                value => $field;
        } else {
            take-rw ($key => $field);
        }
    }
}

method coerce(::THIS ::?CLASS:_: Map:D $original is raw --> Map:D) {
    $original.new: gather for ($original.keys ∪ %.fields.keys).keys -> Mu $key is raw {
        next unless %.fields{$key}:exists;

        my Mu $field := %.fields{$key};
        if $original{$key}:exists {
            my Mu $value := $original{$key};
            if $field ~~ Data::Record::Instance {
                take-record $field, $key, $value, :coerce;
            } elsif $value ~~ $field {
                take-rw ($key => $value);
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => 'map reification',
                    expected  => $field,
                    got       => $value;
            }
        } elsif $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite {
            die X::Data::Record::Definite.new:
                type  => THIS,
                what  => 'key',
                key   => $key,
                value => $field;
        } else {
            take-rw ($key => $field);
        }
    }
}

method fields(::?CLASS:_: --> Map:D) {
    state $fields;
    $fields := self.^fields.Map without $fields;
    $fields
}

method record(::?CLASS:D: --> Map:D) { %!record }

method unrecord(::?CLASS:D: --> Map:D) {
    %!record.new: %!record.kv.map: &unrecord
}
proto sub unrecord(Mu, Mu --> Pair:D) {*}
multi sub unrecord(Mu \key, Data::Record::Instance:D \recorded --> Pair:D) is default {
    (key) => recorded.unrecord
}
multi sub unrecord(Mu \key, Mu \value --> Pair:D) {
    (key) => value
}

multi method gist(::?CLASS:D: --> Str:D) {
    %!record.gist
}

multi method raku(::?CLASS:U: --> Str:D) {
    my Str:D $raku = '{@ ' ~ %.fields.map(*.raku).join(', ') ~ ' @}';
    my Str:D $name = self.^name;
    $raku ~= ":name('$name')" unless $name eq MetamodelX::RecordHOW::ANON_NAME;
    $raku
}

multi method ACCEPTS(::?CLASS:U: Map:D $map is raw --> Bool:D) {
    for ($map.keys ∪ %.fields.keys).keys -> Mu $key is raw {
        return False unless (%.fields{$key}:exists) && ($map{$key}:exists);
        return False unless $map{$key} ~~ %.fields{$key};
    }
    True
}
multi method ACCEPTS(::?CLASS:D: |args --> Bool:D) {
    %!record.ACCEPTS: |args
}

method !field-op-for-value(::?CLASS:D: Mu $field is raw, Mu $value is raw, &op, Str:D :$operation! --> Mu) {
    if $field ~~ Data::Record::Instance {
        if $value ~~ Data::Record::Instance {
            if $value.DEFINITE {
                op $value ~~ $field
                ?? $value
                !! $field.new: $value.record
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => $operation,
                    expected  => $field,
                    got       => $value
            }
        } elsif $value ~~ $field.for {
            op $field.new: $value
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => $operation,
                expected  => $field,
                got       => $value
        }
    } elsif $value ~~ $field {
        op $value
    } else {
        die X::Data::Record::TypeCheck.new:
            operation => $operation,
            expected  => $field,
            got       => $value
    }
}

method EXISTS-KEY(::?CLASS:D: Mu $key is raw --> Bool:D) {
    %!record{$key}:exists
}

method AT-KEY(::THIS ::?CLASS:D: Mu $key is raw --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'key',
        key  => $key
    ) unless %.fields{$key}:exists;

    %!record{$key}
}

method BIND-KEY(::THIS ::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'key',
        key  => $key
    ) unless %.fields{$key}:exists;

    self!field-op-for-value:
        %.fields{$key}, $value, { %!record{$key} := $value },
        :operation<binding>
}

method ASSIGN-KEY(::THIS ::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'key',
        key  => $key
    ) unless %.fields{$key}:exists;

    self!field-op-for-value:
        %.fields{$key}, $value, { %!record{$key} = $value },
        :operation<assignment>
}

method DELETE-KEY(::THIS ::?CLASS:D: Mu --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'deletion',
        type      => THIS
}

method push(::THIS ::?CLASS:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'push',
        type      => THIS
}

method pop(::THIS ::?CLASS:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'pop',
        type      => THIS
}

method shift(::THIS ::?CLASS:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'shift',
        type      => THIS
}

method unshift(::THIS ::?CLASS:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'unshift',
        type      => THIS
}

method append(::THIS ::?CLASS:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'append',
        type      => THIS
}

method prepend(::THIS ::?CLASS:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'prepend',
        type      => THIS
}

method iterator(::?CLASS:D: --> Mu)  { %!record.iterator }
method is-lazy(::?CLASS:D: --> Mu)   { %!record.is-lazy }
method list(::?CLASS:D: --> Mu)      { %!record.list }
method elems(::?CLASS:D: --> Mu)     { %!record.elems }
method cache(::?CLASS:D: --> Mu)     { %!record.cache }
method eager(::?CLASS:D: --> Mu)     { %!record.eager }
method lazy(::?CLASS:D: --> Mu)      { %!record.lazy }
method hash(::?CLASS:D: --> Mu)      { self }
method keys(::?CLASS:D: --> Mu)      { %!record.keys }
method values(::?CLASS:D: --> Mu)    { %!record.values }
method kv(::?CLASS:D: --> Mu)        { %!record.kv }
method pairs(::?CLASS:D: --> Mu)     { %!record.pairs }
method antipairs(::?CLASS:D: --> Mu) { %!record.antipairs }

multi sub circumfix:<{@ @}>(Pair:D $pair is raw, Str:_ :$name, Bool:D :$structural = False --> Mu) is export {
    my Mu $record   := MetamodelX::RecordHOW.new_type: :$name;
    my Mu $delegate := Data::Record::Map.^parameterize: :$structural;
    $record.^set_language_version;
    $record.^set_delegate: $delegate;
    $record.^set_fields: $pair;
    $record.^set_parameters: :$structural;
    $record.^add_role: $delegate;
    $record.^compose
}
multi sub circumfix:<{@ @}>(+pairs where pairs.all ~~ Pair:D, Str:_ :$name, Bool:D :$structural = False --> Mu) is export {
    my Mu $record   := MetamodelX::RecordHOW.new_type: :$name;
    my Mu $delegate := Data::Record::Map.^parameterize: :$structural;
    $record.^set_language_version;
    $record.^set_delegate: $delegate;
    $record.^set_fields: pairs;
    $record.^set_parameters: :$structural;
    $record.^add_role: $delegate;
    $record.^compose
}
multi sub circumfix:<{@ @}>(Block:D $block is raw, Str:_ :$name, Bool:D :$structural = False --> Mu) is export {
    MetamodelX::RecordTemplateHOW.new_type:
        Data::Record::Map, $block, :$name, :$structural
}
multi sub circumfix:<{@ @}>(%not-a-block-wtf, Str:_ :$name, Bool:_ :$structural --> Mu) is export {
    die X::Data::Record::Block.new: type => Data::Record::Map
}

multi sub infix:«(><)»(Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs
}
multi sub infix:«(><)»(Data::Record::Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs.record
}
multi sub infix:«(><)»(Data::Record::Map:U $lhs is raw, Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs
}
multi sub infix:«(><)»(Data::Record::Map:U $lhs is raw, Data::Record::Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs.record
}

multi sub infix:«(<<)»(Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs, :consume
}
multi sub infix:«(<<)»(Data::Record::Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs.record, :consume
}
multi sub infix:«(<<)»(Data::Record::Map:U $lhs is raw, Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs, :subsume
}
multi sub infix:«(<<)»(Data::Record::Map:U $lhs is raw, Data::Record::Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs.record, :subsume
}

multi sub infix:«(>>)»(Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs, :subsume
}
multi sub infix:«(>>)»(Data::Record::Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs.record, :subsume
}
multi sub infix:«(>>)»(Data::Record::Map:U $lhs is raw, Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs, :consume
}
multi sub infix:«(>>)»(Data::Record::Map:U $lhs is raw, Data::Record::Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs.record, :consume
}

multi sub infix:«(<>)»(Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs, :coerce
}
multi sub infix:«(<>)»(Data::Record::Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs.record, :coerce
}
multi sub infix:«(<>)»(Data::Record::Map:U $lhs is raw, Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs, :coerce
}
multi sub infix:«(<>)»(Data::Record::Map:U $lhs is raw, Data::Record::Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs.record, :coerce
}

multi sub infix:<eqv>(Map:D $lhs is raw, Data::Record::Map:D $rhs is raw --> Bool:D) is export {
    $lhs eqv $rhs.unrecord
}
multi sub infix:<eqv>(Data::Record::Map:D $lhs is raw, Map:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs
}
multi sub infix:<eqv>(Data::Record::Map:D $lhs is raw, Data::Record::Map:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs.unrecord
}
