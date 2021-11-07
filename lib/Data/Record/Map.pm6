use v6.d;
use MetamodelX::RecordHOW;
use MetamodelX::RecorderHOW;
use MetamodelX::RecordTemplateHOW;
use Data::Record::Instance;
use Data::Record::Exceptions;

class Data::Record::Map { ... }

role MetamodelX::RecordHOW[Data::Record::Map ::D] does MetamodelX::RecorderHOW[Map, D] {
    has Bool() $!structural   is built = False;
    has Str:D  $!drop_default = $!structural ?? 'none' !! 'unbounded'; # FIXME: Inverted??? Come on.

    method structural(::?CLASS:D: $type is raw) { $!structural }

    method get_field(::?CLASS:D: $type is raw, Mu $key is raw) {
        self.fields($type).AT-KEY($key)
    }

    method type_check_field($type is raw, Mu $key is raw, Mu $field is raw, Mu $value is raw) is raw {
        Metamodel::Primitives.is_type($value, Data::Record::Instance:U) || !$field.ACCEPTS($value)
          ?? self.fail_type_check($type, $key, $field, $value)
          !! $value
    }

    method fail_type_check($type is raw, Mu $key is raw, Mu $field is raw, Mu $value is raw) is raw {
        X::Data::Record::TypeCheck.new(:$*operation, :expected($field), :got($value)).throw;
        $value
    }

    method map_field($type is raw, Mu $key is raw, Mu $value is raw, :$drop = $!drop_default) {
        my $fields := self.fields: $type;
        $fields.EXISTS-KEY($key)
          ?? self.type_check_field($type, $key, $fields.AT-KEY($key), $value)
          !! self."drop_$drop"($type, $key, $value)
    }

    method map_to_field($type is raw, Mu $key is raw, Map:D $values is raw, :$drop = $!drop_default, :$keep!) is raw {
        my $fields := self.fields: $type;
        $fields.EXISTS-KEY($key)
          ?? $values.EXISTS-KEY($key)
            ?? self.type_check_field($type, $key, $fields.AT-KEY($key), $values.AT-KEY($key))
            !! self."keep_$keep"($type, $key, $fields.AT-KEY($key))
          !! self."drop_$drop"($type, $key, $values.AT-KEY($key))
    }

    method drop_none($type is raw, Mu $key is raw, Mu $value is raw) is raw {
        $value
    }

    method drop_unbounded($type is raw, Mu $key is raw, Mu $value is raw) {
        X::Data::Record::OutOfBounds.new(:$type, :what<key>, :$key).throw;
        $value
    }

    method drop_more($type is raw, Mu $key is raw, Mu $value is raw) is raw {
        X::Data::Record::Extraneous.new(:$*operation, :$type, :what<key>, :$key, :$value).throw;
        $value
    }

    method drop_again($type is raw, Mu $key is raw, Mu $value is raw) is raw {
        next;
        $value
    }

    method keep_none($type is raw, Mu $key is raw, Mu $field is raw --> Empty) { }

    method keep_missing($type is raw, Mu $key is raw, Mu $field is raw) {
        X::Data::Record::Missing.new(:$*operation, :$type, :what<key>, :$key, :$field).throw;
        self.keep_coercing: $type, $key, $field
    }

    method keep_coercing($type is raw, Mu $key is raw, Mu $field is raw) {
        X::Data::Record::Definite.new(:$type, :what<index>, :$key, :value($field)).throw
            if Metamodel::Primitives.is_type($field.HOW, Metamodel::DefiniteHOW) && $field.^definite;
        $field
    }
}

my class MapIterator does PredictiveIterator {
    has Mu         $.type      is required;
    has Str:D      $.mode      is required;
    has Str:D      $.operation is required;
    has Map:D      $.values    is required;
    has Iterator:D $!keys      is required;
    has Int:D      $.arity     is required;

    submethod BUILD(::?CLASS:D: :$type! is raw, Str:D :$meta!, Str:D :$mode!, :$!operation!, :$values! is raw --> Nil) {
        my $keys := $type.^fields.keys ∪ $values.keys;
        $!type   := $type;
        $!values := $values;
        $!mode   := "$mode\-$meta";
        $!keys   := $keys.keys.iterator;
        $!arity  := $keys.elems;
    }

    method new(::?CLASS:_: $type is raw, $meta, $mode, $operation, $values is raw --> ::?CLASS:D) {
        self.bless: :$type, :$meta, :$mode, :$operation, :$values
    }

    method pull-one(::?CLASS:D:) is raw {
        my $*operation = $!operation;
        self."$!mode"()
    }

    method is-lazy(::?CLASS:D: --> False) { }

    method count-only(::?CLASS:D: --> Int:D) { $!arity }

    method map-one-pair(::?CLASS:D: *%named) is raw {
        my $key := $!keys.pull-one;
        $key =:= IterationEnd ?? $key !! ($key => $!type.^map_to_field: $key, $!values, |%named)
    }

    method wrap-structured(::?CLASS:D:) is raw {
        self.map-one-pair: :drop<more>, :keep<missing>
    }
    method consume-structured(::?CLASS:D:) is raw {
        loop { return-rw self.map-one-pair: :drop<again>, :keep<missing> }
    }
    method subsume-structured(::?CLASS:D:) is raw {
        self.map-one-pair: :drop<more>, :keep<coercing>
    }
    method coerce-structured(::?CLASS:D:) is raw {
        loop { return-rw self.map-one-pair: :drop<again>, :keep<coercing> }
    }

    method wrap-unstructured(::?CLASS:D:) is raw {
        self.map-one-pair: :drop<none>, :keep<missing>
    }
    method consume-unstructured(::?CLASS:D:) is raw {
        self.map-one-pair: :drop<none>, :keep<missing>
    }
    method subsume-unstructured(::?CLASS:D:) is raw {
        self.map-one-pair: :drop<none>, :keep<coercing>
    }
    method coerce-unstructured(::?CLASS:D:) is raw {
        self.map-one-pair: :drop<none>, :keep<coercing>
    }

    method wrap-bounded(::?CLASS:D:) is raw {
        self.map-one-pair: :keep<none>
    }
    method consume-bounded(::?CLASS:D:) is raw {
        self.map-one-pair: :keep<none>
    }
    method subsume-bounded(::?CLASS:D:) is raw {
        self.map-one-pair: :keep<none>
    }
    method coerce-bounded(::?CLASS:D:) is raw {
        self.map-one-pair: :keep<none>
    }
}

class Data::Record::Map does Data::Record::Instance[Map] does Iterable does Associative {
    has %.record is required is built(:bind);

    multi method new(::?CLASS:_: Map:D $original is raw --> ::?CLASS:D) {
        my %record := self.wrap: $original;
        self.bless: :%record
    }
    multi method new(::?CLASS:_: Map:D $original is raw, Bool:D :consume($)! where ?* --> ::?CLASS:D) {
        my %record := self.consume: $original;
        self.bless: :%record
    }
    multi method new(::?CLASS:_: Map:D $original is raw, Bool:D :subsume($)! where ?* --> ::?CLASS:D) {
        my %record := self.subsume: $original;
        self.bless: :%record
    }
    multi method new(::?CLASS:_: Map:D $original is raw, Bool:D :coerce($)! where ?* --> ::?CLASS:D) {
        my %record := self.coerce: $original;
        self.bless: :%record
    }

    method wrap(::THIS ::?CLASS:_: Map:D $original is raw --> Map:D) {
        my $meta := self.^structural ?? 'unstructured' !! 'structured';
        $original.new: List.from-iterator: MapIterator.new: THIS, $meta, 'wrap', 'map reification', $original
    }

    method consume(::THIS ::?CLASS:_: Map:D $original is raw --> Map:D) {
        my $meta := self.^structural ?? 'unstructured' !! 'structured';
        $original.new: List.from-iterator: MapIterator.new: THIS, $meta, 'consume', 'map reification', $original
    }

    method subsume(::THIS ::?CLASS:_: Map:D $original is raw --> Map:D) {
        my $meta := self.^structural ?? 'unstructured' !! 'structured';
        $original.new: List.from-iterator: MapIterator.new: THIS, $meta, 'subsume', 'map reification', $original
    }

    method coerce(::THIS ::?CLASS:_: Map:D $original is raw --> Map:D) {
        my $meta := self.^structural ?? 'unstructured' !! 'structured';
        $original.new: List.from-iterator: MapIterator.new: THIS, $meta, 'coerce', 'map reification', $original
    }

    method fields(::?CLASS:_: --> Map:D) { self.^fields }

    method structural(::?CLASS:_: --> Bool:D) { self.^structural }

    do { # hide this sub
        proto sub unrecord(Mu \key, Mu --> Pair:D)                 { (key) => {*} }
        multi sub unrecord(Mu, Data::Record::Instance:D \recorded) { recorded.unrecord }
        multi sub unrecord(Mu, Mu \value)                          { value }

        method unrecord(::?CLASS:D: --> Map:D) {
            %!record.new: %!record.kv.map: &unrecord
        }
    }

    multi method raku(::?CLASS:U: --> Str:D) {
        my Str:D $raku = '{@ ' ~ %.fields.map(*.raku).join(', ') ~ ' @}';
        my Str:D $name = self.^name;
        $raku ~= ':structural' if self.^structural;
        $raku ~= ":name('$name')" unless self.^is_anonymous;
        $raku
    }

    multi method ACCEPTS(::?CLASS:U: Map:D $map is raw --> Bool:D) {
        my %fields:= %.fields;
        for ($map.keys ∪ %fields.keys).keys -> Mu $key is raw {
            return False unless %fields{$key}:exists;
            return False unless $map{$key}:exists && $map{$key} ~~ %.fields{$key};
        }
        True
    }

    method EXISTS-KEY(::?CLASS:D: Mu $key is raw --> Bool:D) {
        %!record.EXISTS-KEY: $key
    }

    method AT-KEY(::THIS ::?CLASS:D: Mu $key is raw) is raw {
        state $*operation = 'lookup';
        self.^map_field: $key, %!record.AT-KEY: $key
    }

    method BIND-KEY(::THIS ::?CLASS:D: Mu $key is raw, Mu $value is raw) is raw {
        state $*operation = 'binding';
        %!record.BIND-KEY: $key, self.^map_field: $key, $value
    }

    method ASSIGN-KEY(::THIS ::?CLASS:D: Mu $key is raw, Mu $value is raw) is raw {
        state $*operation = 'assignment';
        %!record.ASSIGN-KEY: $key, self.^map_field: $key, $value
    }

    method DELETE-KEY(::THIS ::?CLASS:D: Mu $key is raw) {
        # TODO: Maybe it's OK to delete a field if indefinite.
        X::Data::Record::Immutable.new(
            operation => 'deletion',
            type      => THIS,
        ).throw if self.^fields.EXISTS-KEY: $key;
        let %!record;
        state $*operation = 'deletion';
        self.^map_field: $key, %!record.DELETE-KEY: $key
    }

    method push(::THIS ::?CLASS:D: +@values is raw --> ::?CLASS:D) {
        MapIterator.new(THIS, 'bounded', 'wrap', 'push', let %!record .= push: @values).sink-all;
        self
    }

    method append(::THIS ::?CLASS:D: +@values is raw --> ::?CLASS:D) {
        MapIterator.new(THIS, 'bounded', 'wrap', 'append', let %!record .= append: @values).sink-all;
        self
    }

    method iterator(::?CLASS:D:)  { %!record.iterator }
    method is-lazy(::?CLASS:D:)   { %!record.is-lazy }
    method list(::?CLASS:D:)      { %!record.list }
    method elems(::?CLASS:D:)     { %!record.elems }
    method cache(::?CLASS:D:)     { %!record.cache }
    method eager(::?CLASS:D:)     { %!record.eager }
    method lazy(::?CLASS:D:)      { %!record.lazy }
    method hash(::?CLASS:D:)      { self }
    method keys(::?CLASS:D:)      { %!record.keys }
    method values(::?CLASS:D:)    { %!record.values }
    method kv(::?CLASS:D:)        { %!record.kv }
    method pairs(::?CLASS:D:)     { %!record.pairs }
    method antipairs(::?CLASS:D:) { %!record.antipairs }
}

multi sub circumfix:<{@ @}>(Map:D $fields, Str:_ :$name, Bool:D :$structural = False) is export {
    MetamodelX::RecordHOW[Data::Record::Map].new_type($fields<>, :$name, :$structural).^compose
}
multi sub circumfix:<{@ @}>(*@pairs, Str:_ :$name, Bool:D :$structural = False) is export {
    MetamodelX::RecordHOW[Data::Record::Map].new_type(Map.new(@pairs), :$name, :$structural).^compose
}
multi sub circumfix:<{@ @}>(Block:D $block, Str:_ :$name, Bool:D :$structural = False) is export {
    MetamodelX::RecordTemplateHOW[MetamodelX::RecordHOW[Data::Record::Map]].new_type: $block, :$name, :$structural
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
