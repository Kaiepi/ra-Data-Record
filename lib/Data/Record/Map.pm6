use v6.d;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
use Data::Record::Mode;
use Data::Record::Instance;
use Data::Record::Lifter;
use Data::Record::Exceptions;
unit class Data::Record::Map does Data::Record::Instance[Map:D] does Iterable does Associative;

my class MapIterator { ... }

has %.record is required;

submethod BUILD(::?CLASS:D: :%record is raw --> Nil) {
    %!record := %record;
}

multi method new(::?CLASS:_: Map:D $original is raw, Bool:D :wrap($)! where ?* --> ::?CLASS:D) {
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
multi method new(::?CLASS:_ ::THIS: Map:D $original is raw, Data::Record::Mode:D :$mode = WRAP --> ::?CLASS:D) {
    my %record := $original.new: List.from-iterator:
        MapIterator.new: THIS, THIS.^metamode, $mode, 'map reification', $original;
    self.bless: :%record
}

method wrap(::?CLASS:_ ::THIS: Map:D $original is raw --> Map:D) {
    $original.new: List.from-iterator:
        MapIterator.new: THIS, THIS.^metamode, WRAP, 'map reification', $original
}

method consume(::?CLASS:_ ::THIS: Map:D $original is raw --> Map:D) {
    $original.new: List.from-iterator:
        MapIterator.new: THIS, THIS.^metamode, CONSUME, 'map reification', $original
}

method subsume(::?CLASS:_ ::THIS: Map:D $original is raw --> Map:D) {
    $original.new: List.from-iterator:
        MapIterator.new: THIS, THIS.^metamode, SUBSUME, 'map reification', $original
}

method coerce(::?CLASS:_ ::THIS: Map:D $original is raw --> Map:D) {
    $original.new: List.from-iterator:
        MapIterator.new: THIS, THIS.^metamode, COERCE, 'map reification', $original
}

method fields(::?CLASS:_: --> Map:D) { self.^fields }

method structural(::?CLASS:_: --> Bool:D) { self.^structural }

do { # hide this sub
    proto sub unrecord(Mu \key, Mu --> Pair:D)                 { (key) => {*} }
    multi sub unrecord(Mu, Data::Record::Instance:D \recorded) { recorded.unrecord }
    multi sub unrecord(Mu, Mu \value) is raw                   { value }

    method unrecord(::?CLASS:D: --> Map:D) {
        %!record.new: %!record.kv.map: &unrecord
    }
}

multi method raku(::?CLASS:U: --> Str:D) {
    my Str:D $raku = '{@ ' ~ self.^fields.map(*.raku).join(', ') ~ ' @}';
    my Str:D $name = self.^name;
    $raku ~= ':structural' if self.^structural;
    $raku ~= ":name('$name')" unless self.^is_anonymous;
    $raku
}

multi method ACCEPTS(::?CLASS:U: Map:D \topic) {
    my $fields := self.^fields;
    [&&] lazy for keys $fields.keys ∪ topic.keys -> $key is raw {
        $fields.EXISTS-KEY($key) && topic.EXISTS-KEY($key) &&
            $fields.AT-KEY($key).ACCEPTS(topic.AT-KEY: $key)
    }
}

method EXISTS-KEY(::?CLASS:D: $key is raw --> Bool:D) {
    %!record.EXISTS-KEY: $key
}

method AT-KEY(::THIS ::?CLASS:D: $key is raw) is raw {
    CONTROL { .flunk: 'lookup' when CX::Rest }
    self.^map_field: $key, %!record.AT-KEY: $key
}

method BIND-KEY(::THIS ::?CLASS:D: $key is raw, Mu $value is raw) is raw {
    CONTROL { .flunk: 'binding' when CX::Rest }
    %!record.BIND-KEY: $key, self.^map_field: $key, $value
}

method ASSIGN-KEY(::THIS ::?CLASS:D: $key is raw, Mu $value is raw) is raw {
    CONTROL { .flunk: 'assignment' when CX::Rest }
    %!record.ASSIGN-KEY: $key, self.^map_field: $key, $value
}

method DELETE-KEY(::THIS ::?CLASS:D: $key is raw) {
    # TODO: Maybe it's OK to delete a field if indefinite.
    X::Data::Record::Immutable.new(
        operation => 'deletion',
        type      => THIS,
    ).throw if self.^fields.EXISTS-KEY: $key;
    CONTROL { .flunk: 'deletion' when CX::Rest }
    self.^map_field: $key, (let %!record).DELETE-KEY: $key
}

method push(::THIS ::?CLASS:D: +@values is raw --> ::?CLASS:D) {
    MapIterator.new(THIS, 'bounded', WRAP, 'push', let %!record .= push: @values).sink-all;
    self
}

method append(::THIS ::?CLASS:D: +@values is raw --> ::?CLASS:D) {
    MapIterator.new(THIS, 'bounded', WRAP, 'append', let %!record .= append: @values).sink-all;
    self
}

multi method iterator(::?CLASS:D:) { %!record.iterator }

multi method hash(::?CLASS:D:) is raw { self }

multi method list(::?CLASS:D:) is raw { %!record.list }

method is-lazy(::?CLASS:_:) {
    self.DEFINITE && %!record.is-lazy
}

method cache(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? %!record.is-lazy
        ?? self.new(%!record.cache)
        !! self
      !! %(self).cache
}

method eager(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? %!record.is-lazy
        ?? self.new(%!record.eager)
        !! self
      !! %(self).eager
}

method lazy(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? %!record.is-lazy
        ?? self
        !! self.new(%!record.lazy)
      !! %(self).lazy
}

multi method elems(::?CLASS:D:) { %!record.elems }

multi method keys(::?CLASS:D:) { %!record.keys }

multi method values(::?CLASS:D:) { %!record.values }

multi method kv(::?CLASS:D:) { %!record.kv }

multi method pairs(::?CLASS:D:) { %!record.pairs }

multi method antipairs(::?CLASS:D:) { %!record.antipairs }

method ^annotations($? --> 1) { }

method ^structural($type is raw --> Bool:D) {
    self.yield_annotations($type)[once self.annotation_offset($type)]<>
}

method ^metamode($type is raw) {
    # XXX: Inverted?? Come on.
    self.structural($type) ?? 'unstructured' !! 'structured'
}

method ^get_field($type is raw, $key is raw) {
    my %fields := self.fields: $type;
    %fields.AT-KEY($key)
}

method ^declares_field($type is raw, $key is raw) {
    my %fields := self.fields: $type;
    %fields.EXISTS-KEY($key)
}

method ^map_field(
    $type is raw, $key is raw, Mu $value is raw,
    Data::Record::Mode:D :$mode = WRAP,
    :$drop = self.structural($type) ?? 'none' !! 'unbounded',
) {
    my %fields := self.fields: $type;
    %fields.EXISTS-KEY($key)
      ?? ($value @~~ %fields.AT-KEY($key) :$mode)
      !! self."drop_$drop"($type, $key, $value)
}

method ^map_to_field(
    $type is raw, $key is raw, Map:D $values is raw,
    :$mode = WRAP,
    :$drop = self.structural($type) ?? 'none' !! 'unbounded',
    :$keep!,
) is raw {
    my %fields := self.fields: $type;
    %fields.EXISTS-KEY($key)
      ?? $values.EXISTS-KEY($key)
        ?? ($values.AT-KEY($key) @~~ %fields.AT-KEY($key) :$mode)
        !! self."keep_$keep"($type, $key, %fields.AT-KEY($key))
      !! self."drop_$drop"($type, $key, $values.AT-KEY($key))
}

method ^drop_none($type is raw, $key is raw, Mu $value is raw) is raw {
    $value
}

method ^drop_unbounded($type is raw, $key is raw, Mu $value is raw) {
    X::Data::Record::OutOfBounds.new(:$type, :what<key>, :$key).throw;
    $value
}

method ^drop_more($type is raw, $key is raw, Mu $value is raw) is raw {
    X::Data::Record::Extraneous.new(:$*operation, :$type, :what<key>, :$key, :$value).throw;
    $value
}

method ^drop_again($type is raw, $key is raw, Mu $value is raw) is raw {
    next;
    $value
}

method ^keep_none($type is raw, $key is raw, Mu $field is raw --> Empty) { }

method ^keep_missing($type is raw, $key is raw, Mu $field is raw) {
    X::Data::Record::Missing.new(:$*operation, :$type, :what<key>, :$key, :$field).throw;
    self.keep_coercing: $type, $key, $field
}

method ^keep_coercing($type is raw, $key is raw, Mu $field is raw) {
    X::Data::Record::Definite.new(:$type, :what<index>, :$key, :value($field)).throw
        if Metamodel::Primitives.is_type($field.HOW, Metamodel::DefiniteHOW) && $field.^definite;
    $field
}

my class MapIterator does PredictiveIterator {
    has Data::Record::Map:U  $.type      is required;
    has Data::Record::Mode:D $.mode      is required;
    has Str:D                $.meta      is required;
    has Str:D                $.operation is required;
    has Map:D                $.values    is required;
    has Iterator:D           $!keys      is required;
    has Int:D                $.arity     is required;

    submethod BUILD(::?CLASS:D: :$type! is raw, :$!mode!, :$!meta, :$!operation!, :$values! is raw --> Nil) {
        my $keys := $type.^fields.keys ∪ $values.keys;
        $!type   := $type;
        $!values := $values;
        $!keys   := $keys.keys.iterator;
        $!arity  := $keys.elems;
    }

    method new(::?CLASS:_: $type is raw, $meta, $mode, $operation, $values is raw --> ::?CLASS:D) {
        self.bless: :$type, :$meta, :$mode, :$operation, :$values
    }

    method pull-one(::?CLASS:D:) is raw {
        my $*operation = $!operation;
        self."$!mode\-$!meta"()
    }

    method is-lazy(::?CLASS:D: --> False) { }

    method count-only(::?CLASS:D: --> Int:D) { $!arity }

    method map-one-pair(::?CLASS:D: *%named) is raw {
        CONTROL { .flunk: $!operation when CX::Rest }
        (my $key := $!keys.pull-one) =:= IterationEnd
          ?? IterationEnd
          !! ($key => $!type.^map_to_field: $key, $!values, |%named, :$!mode)
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

multi sub circumfix:<{@ @}>(Map:D $fields, Str:_ :$name, Bool:D :$structural = False) is export {
    my $obj := MetamodelX::RecordHOW[
        Map:D, Data::Record::Map
    ].new_type: $fields<>, :$name;
    my $how := $obj.HOW;
    $how.yield_annotations($obj) = $structural;
    $how.compose: $obj
}
multi sub circumfix:<{@ @}>(*@pairs, Str:_ :$name, Bool:D :$structural = False) is export {
    my $obj := MetamodelX::RecordHOW[
        Map:D, Data::Record::Map
    ].new_type: Map.new(@pairs), :$name;
    my $how := $obj.HOW;
    $how.yield_annotations($obj) = $structural;
    $how.compose: $obj
}
multi sub circumfix:<{@ @}>(Block:D $block, Str:_ :$name, Bool:D :$structural = False) is export {
    my $obj := MetamodelX::RecordTemplateHOW[
        MetamodelX::RecordHOW[Map:D, Data::Record::Map]
    ].new_type: $block, :$name;
    my $how := $obj.HOW;
    $how.yield_annotations($obj) = $structural;
    $how.compose: $obj
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
