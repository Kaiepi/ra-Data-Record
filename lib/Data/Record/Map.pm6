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

submethod BUILD(::?ROLE:D: :%record! --> Nil) {
    %!record := %record;
}

multi method new(::?ROLE:_: Map:D $original is raw --> ::?ROLE:D) {
    my %record := self.wrap: $original;
    self.bless: :%record
}
multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :consume($)! where ?* --> ::?ROLE:D) {
    my %record := self.consume: $original;
    self.bless: :%record
}
multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :subsume($)! where ?* --> ::?ROLE:D) {
    my %record := self.subsume: $original;
    self.bless: :%record
}
multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :coerce($)! where ?* --> ::?ROLE:D) {
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

method wrap(::THIS ::?ROLE:_: Map:D $original is raw --> Map:D) {
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

method consume(::THIS ::?ROLE:_: Map:D $original is raw --> Map:D) {
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

method subsume(::THIS ::?ROLE:_: Map:D $original is raw --> Map:D) {
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

method coerce(::THIS ::?ROLE:_: Map:D $original is raw --> Map:D) {
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

method fields(::?ROLE:_: --> Map:D) {
    state $fields;
    $fields := self.^fields.Map without $fields;
    $fields
}

method record(::?ROLE:D: --> Map:D) { %!record }

method unrecord(::?ROLE:D: --> Map:D) {
    %!record.new: %!record.kv.map: &unrecord
}
proto sub unrecord(Mu, Mu --> Pair:D) {*}
multi sub unrecord(Mu \key, Data::Record::Instance:D \recorded --> Pair:D) is default {
    (key) => recorded.unrecord
}
multi sub unrecord(Mu \key, Mu \value --> Pair:D) {
    (key) => value
}

multi method raku(::?ROLE:U: --> Str:D) {
    my Str:D $raku = '{@ ' ~ %.fields.map(*.raku).join(', ') ~ ' @}';
    my Str:D $name = self.^name;
    $raku ~= ":name('$name')" unless $name eq MetamodelX::RecordHOW::ANON_NAME;
    $raku
}

multi method ACCEPTS(::?ROLE:U: Map:D $map is raw --> Bool:D) {
    for ($map.keys ∪ %.fields.keys).keys -> Mu $key is raw {
        return False unless (%.fields{$key}:exists) && ($map{$key}:exists);
        return False unless $map{$key} ~~ %.fields{$key};
    }
    True
}
multi method ACCEPTS(::?ROLE:D: |args --> Bool:D) {
    %!record.ACCEPTS: |args
}

method !field-op-for-value(::?ROLE:D: Mu $field is raw, Mu $value is raw, &op, Str:D :$operation! --> Mu) is raw {
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

method EXISTS-KEY(::?ROLE:D: Mu $key is raw --> Bool:D) {
    %!record{$key}:exists
}

method AT-KEY(::THIS ::?ROLE:D: Mu $key is raw --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'key',
        key  => $key
    ) unless %.fields{$key}:exists;

    %!record{$key}
}

method BIND-KEY(::THIS ::?ROLE:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'key',
        key  => $key
    ) unless %.fields{$key}:exists;

    self!field-op-for-value:
        %.fields{$key}, $value, { %!record{$key} := $value },
        :operation<binding>
}

method ASSIGN-KEY(::THIS ::?ROLE:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'key',
        key  => $key
    ) unless %.fields{$key}:exists;

    self!field-op-for-value:
        %.fields{$key}, $value, { %!record{$key} = $value },
        :operation<assignment>
}

method DELETE-KEY(::THIS ::?ROLE:D: Mu --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'deletion',
        type      => THIS
}

# Performs an array op (push/append) for a list of values in a gather.
#
# "But this will never take any values! What's the point of this?"
#
# The point of this is to defer typechecking errors until the array op iterates
# over the Seq created with this, since Hash array ops have exceptions of their
# own that are best left to their corresponding methods to handle.
method !array-op-for-values(::?ROLE:D: @values, &op, Str:D :$operation! --> Map:D) {
    my Mu     $key;
    my Bool:D $has-key = False;
    for @values -> Mu $value is raw {
        if $has-key {
            # We have a key from the last iteration.
            $has-key = False;
            self!array-op-for-pair: $key, $value, &op, :$operation;
        } elsif $value ~~ Pair:D {
            # We have a pair; no need to buffer.
            self!array-op-for-pair: $value.key, $value.value, &op, :$operation;
        } else {
            # We have a key; hope there's a value to go with it!
            $key     := $value;
            $has-key  = True;
        }
    }
}
method !array-op-for-pair(::THIS ::?ROLE:D: Mu $key is raw, Mu $value is raw, &op, Str:D :$operation! --> Nil) {
    my %fields := %.fields;
    if %fields{$key}:exists {
        my Mu $field := %fields{$key};
        if $field ~~ Data::Record::Instance[List] | Array {
            # Perform the array op now, since this value doesn't
            # necessarily typecheck against Array, which the corresponding
            # Hash method expects.
            op $key, $value
        } else {
            # A field for this key already exists, but isn't an array of
            # some sort; this can't possibly typecheck.
            die X::Data::Record::TypeCheck.new:
                operation => $operation,
                expected  => $field,
                got       => $value
        }
    } else {
        die X::Data::Record::OutOfBounds.new:
            type => THIS,
            what => 'key',
            key  => $key
    }
}

method push(::THIS ::?ROLE:D: +values --> ::?ROLE:D) {
    %!record.push: gather self!array-op-for-values: values, -> Mu \key, Mu \value {
        %!record{key}.push: value
    }, :operation<push>;
    self
}

method append(::THIS ::?ROLE:D: +values --> ::?ROLE:D) {
    %!record.append: gather self!array-op-for-values: values, -> Mu \key, Mu \value {
        %!record{key}.append: |value
    }, :operation<append>;
    self
}

method iterator(::?ROLE:D: --> Mu)  { %!record.iterator }
method is-lazy(::?ROLE:D: --> Mu)   { %!record.is-lazy }
method list(::?ROLE:D: --> Mu)      { %!record.list }
method elems(::?ROLE:D: --> Mu)     { %!record.elems }
method cache(::?ROLE:D: --> Mu)     { %!record.cache }
method eager(::?ROLE:D: --> Mu)     { %!record.eager }
method lazy(::?ROLE:D: --> Mu)      { %!record.lazy }
method hash(::?ROLE:D: --> Mu)      { self }
method keys(::?ROLE:D: --> Mu)      { %!record.keys }
method values(::?ROLE:D: --> Mu)    { %!record.values }
method kv(::?ROLE:D: --> Mu)        { %!record.kv }
method pairs(::?ROLE:D: --> Mu)     { %!record.pairs }
method antipairs(::?ROLE:D: --> Mu) { %!record.antipairs }

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
