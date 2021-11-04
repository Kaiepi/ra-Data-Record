use v6.d;
use MetamodelX::RecordHOW;
use MetamodelX::RecorderHOW;
use MetamodelX::RecordTemplateHOW;
use Data::Record::Instance;
use Data::Record::Exceptions;

class Data::Record::Tuple { ... }

role MetamodelX::RecordHOW[Data::Record::Tuple ::D] does MetamodelX::RecorderHOW[List, D] {
    method get_field($type is raw, Mu $key is raw) {
        self.fields($type).AT-POS: $key
    }

    method enforce_bounds(::THIS $type is raw, Mu $key is raw --> Nil) {
        X::Data::Record::OutOfBounds.new(
            :type(THIS), :what<index>, :$key
        ).throw unless self.fields($type).EXISTS-POS: $key
    }

    method enforce_immutability(::THIS, $operation --> Nil) {
        X::Data::Record::Immutable.new(:$operation, :type(THIS)).throw
    }

    method map_field($type is raw, Mu $key is raw, Mu $field is raw, Mu $value is raw) is raw {
        Metamodel::Primitives.is_type($value, Data::Record::Instance:U) || !$field.ACCEPTS($value)
          ?? X::Data::Record::TypeCheck.new(:$*operation, :expected($field), :got($value)).throw
          !! $value
    }

    method map_it_field($type is raw, Mu $key is raw, Mu $field is raw, Mu $value is raw, :$drop!, :$keep!) is raw {
        $field =:= IterationEnd
          ?? self."drop_it_$drop"($type, $key, $field, $value)
          !! $value =:= IterationEnd
            ?? self."keep_it_$keep"($type, $key, $field, $value)
            !! self.map_field($type, $key, $field, $value)
    }

    method drop_it_more($type is raw, Mu $key is raw, Mu $field is raw, Mu $value is raw --> IterationEnd) {
        X::Data::Record::Extraneous.new(
            :$*operation, :$type, :what<index>, :$key, :$value
        ).throw unless $value =:= IterationEnd;
    }

    method drop_it_less(Mu, Mu, Mu, Mu --> IterationEnd) { }

    method keep_it_missing($type is raw, Mu $key is raw, Mu $field is raw, Mu $value is raw --> IterationEnd) {
        X::Data::Record::Missing.new(:$*operation, :$type, :what<index>, :$key, :$field).throw;
    }

    method keep_it_coercing($type is raw, Mu $key is raw, Mu $field is raw, Mu $value is raw) is raw {
        X::Data::Record::Definite.new(
            :$type, :what<index>, :$key, :value($field)
        ).throw if $field.HOW.archetypes.definite && $field.^definite;
        $field
    }
}

#|[ Iterator for tuples (lists of fixed length) that are to become records.
    Typechecks the list's values and coerces any record fields along the way. ]
my class TupleIterator does Iterator {
    has Mu         $.type      is required;
    has Str:D      $.mode      is required;
    has Str:D      $.operation is required;
    has Iterator:D $.fields    is required;
    has Iterator:D $.values    is required;
    has Int:D      $.arity     is required;
    has Int:D      $.count     = 0;

    submethod BUILD(::?CLASS:D: Mu :$!type! is raw, :$!mode!, :$!operation!, :$fields! is raw, :$values! is raw --> Nil) {
        $!fields := $fields.iterator;
        $!values := $values.iterator;
        $!arity  := $fields.elems;
    }

    method new(::?CLASS:_: Mu $type is raw, $mode, $operation, $fields is raw, $values is raw --> ::?CLASS:D) {
        self.bless: :$type, :$mode, :$operation, :$fields, :$values
    }

    method pull-one(::?CLASS:D:) is raw {
        my $*operation := $!operation;
        self."$!mode"($!fields.pull-one)
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }

    #|[ The list must have an arity equal to the tuple type's and all values
        must typecheck as their corresponding fields, otherwise an exception
        will be thrown. ]
    method wrap(::?CLASS:D: Mu $field is raw) is raw {
        LEAVE $!count++;
        $!type.^map_it_field: $!count, $field, $!values.pull-one, :drop<more>, :keep<missing>
    }

    #|[ The list must have an arity greater than or equal to the tuple type; if
        it's greater, extraneous values will be stripped. If any values are
        missing or values corresponding to fields don't typecheck, an exception
        will be thrown. ]
    method consume(::?CLASS:D: Mu $field is raw) is raw {
        LEAVE $!count++;
        $!type.^map_it_field: $!count, $field, $!values.pull-one, :drop<less>, :keep<missing>
    }

    #|[ The list must have an arity lesser than or equal to the tuple type's;
        if it's lesser, missing values will be stubbed (if possible).  If any
        values don't typecheck as their corresponding fields, an exception will
        be thrown. ]
    method subsume(::?CLASS:D: Mu $field is raw) is raw {
        LEAVE $!count++;
        $!type.^map_it_field: $!count, $field, $!values.pull-one, :drop<more>, :keep<coercing>
    }

    #|[ Coerces a list. Arity does not matter; missing values are stubbed (if
        possible) and extraneous values are stripped. If any values don't
        typecheck as their corresponding fields, an exception will be thrown. ]
    method coerce(::?CLASS:D: Mu $field is raw) is raw {
        LEAVE $!count++;
        $!type.^map_it_field: $!count, $field, $!values.pull-one, :drop<less>, :keep<coercing>
    }
}

class Data::Record::Tuple does Data::Record::Instance[List] does Iterable does Positional {
    has @!record;

    submethod BUILD(::?CLASS:D: :@record --> Nil) {
        @!record := @record;
    }

    multi method new(::?CLASS:_: List:D $original is raw --> ::?CLASS:D) {
        my @record := self.wrap: $original;
        @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
        self.bless: :@record
    }
    multi method new(::?CLASS:_: List:D $original is raw, Bool:D :consume($)! where ?* --> ::?CLASS:D) {
        my @record := self.consume: $original;
        @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
        self.bless: :@record
    }
    multi method new(::?CLASS:_: List:D $original is raw, Bool:D :subsume($)! where ?* --> ::?CLASS:D) {
        my @record := self.subsume: $original;
        @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
        self.bless: :@record
    }
    multi method new(::?CLASS:_: List:D $original is raw, Bool:D :coerce($)! where ?* --> ::?CLASS:D) {
        my @record := self.coerce: $original;
        @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
        self.bless: :@record
    }

    method wrap(::THIS ::?CLASS:_: ::T List:D $original is raw --> List:D) {
        T.from-iterator: TupleIterator.new: THIS, 'wrap', 'tuple reification', @.fields, $original
    }

    method consume(::THIS ::?CLASS:_: ::T List:D $original is raw --> List:D) {
        T.from-iterator: TupleIterator.new: THIS, 'consume', 'tuple reification', @.fields, $original
    }

    method subsume(::THIS ::?CLASS:_: ::T List:D $original is raw --> List:D) {
        T.from-iterator: TupleIterator.new: THIS, 'subsume', 'tuple reification', @.fields, $original
    }

    method coerce(::THIS ::?CLASS:_: ::T List:D $original is raw --> List:D) {
        T.from-iterator: TupleIterator.new: THIS, 'coerce', 'tuple reification', @.fields, $original
    }

    method fields(::?CLASS:_: --> List:D) { self.^fields }

    method record(::?CLASS:D: --> List:D) { @!record }

    do { # hide this sub
        proto sub unrecord(Mu)                                 {*}
        multi sub unrecord(Data::Record::Instance:D \recorded) { recorded.unrecord }
        multi sub unrecord(Mu \value) is raw                   { value }

        method unrecord(::?CLASS:D: --> List:D) {
            @!record.WHAT.from-iterator: @!record.map(&unrecord).iterator
        }
    }

    multi method raku(::?CLASS:U: --> Str:D) {
        my Str:D $raku = '<@ ' ~ @.fields.map(*.raku).join(', ') ~ ' @>';
        my Str:D $name = self.^name;
        $raku ~= ":name('$name')" unless $name eq MetamodelX::RecordHOW::ANON_NAME;
        $raku
    }

    multi method ACCEPTS(::?CLASS:U: List:D $list is raw --> Bool:D) {
        # $list could be lazy, so we can't just .elems it to find out if it has
        # the correct arity. Instead, ensure the index for each of our fields
        # exists in $list and typechecks, then check if any extraneous values
        # exist.
        my Int:D $count = 0;
        for @.fields.kv -> Int:D $idx, Mu $field is raw {
            return False unless $list[$idx]:exists && $list[$idx] ~~ $field;
            $count++;
        }
        $list[$count]:!exists
    }

    method EXISTS-POS(::?CLASS:D: Int:D $pos --> Bool:D) {
        @!record[$pos]:exists
    }

    method AT-POS(::?CLASS:D: Mu $pos is raw --> Mu) is raw {
        self.^enforce_bounds: $pos;
        @!record.AT-POS: $pos
    }

    method BIND-POS(::?CLASS:D: Mu $pos is raw, Mu $value is raw --> Mu) is raw {
        self.^enforce_bounds: $pos;
        state $*operation = 'binding';
        @!record.BIND-POS: $pos, self.^map_field: $pos, self.^get_field($pos), $value
    }

    method ASSIGN-POS(::?CLASS:D: Mu $pos is raw, Mu $value is raw --> Mu) is raw {
        self.^enforce_bounds: $pos;
        state $*operation = 'assignment';
        @!record.ASSIGN-POS: $pos, self.^map_field: $pos, self.^get_field($pos), $value
    }

    method DELETE-POS(::?CLASS:D: Mu $pos is raw --> Mu) is raw {
        self.^enforce_immutability: 'deletion'
    }

    method push(::?CLASS:D: | --> Nil)    { self.^enforce_immutability: 'push' }
    method pop(::?CLASS:D: | --> Nil)     { self.^enforce_immutability: 'pop' }
    method shift(::?CLASS:D: | --> Nil)   { self.^enforce_immutability: 'shift' }
    method unshift(::?CLASS:D: | --> Nil) { self.^enforce_immutability: 'unshift' }
    method append(::?CLASS:D: | --> Nil)  { self.^enforce_immutability: 'append' }
    method prepend(::?CLASS:D: | --> Nil) { self.^enforce_immutability: 'prepend' }

    method eager(::?CLASS:D: --> ::?CLASS:D) { @!record.is-lazy ?? self.new(@.record.eager) !! self }
    method lazy(::?CLASS:D: --> ::?CLASS:D)  { @!record.is-lazy ?? self !! self.new(@.record.lazy) }

    method iterator(::?CLASS:D: --> Mu)  { @!record.iterator }
    method is-lazy(::?CLASS:D: --> Mu)   { @!record.is-lazy }
    method cache(::?CLASS:D: --> Mu)     { @!record.cache }
    method list(::?CLASS:D: --> Mu)      { self }
    method elems(::?CLASS:D: --> Mu)     { @!record.elems }
    method hash(::?CLASS:D: --> Mu)      { @!record.hash }
    method keys(::?CLASS:D: --> Mu)      { @!record.keys }
    method values(::?CLASS:D: --> Mu)    { @!record.values }
    method kv(::?CLASS:D: --> Mu)        { @!record.kv }
    method pairs(::?CLASS:D: --> Mu)     { @!record.pairs }
    method antipairs(::?CLASS:D: --> Mu) { @!record.antipairs }
}

multi sub circumfix:«<@ @>»(+@fields is raw, Str:_ :$name --> Mu) is export {
    MetamodelX::RecordHOW[Data::Record::Tuple].new_type(@fields, :$name).^compose
}
multi sub circumfix:«<@ @>»(Block:D $block is raw, Str:_ :$name --> Mu) is export {
    MetamodelX::RecordTemplateHOW[MetamodelX::RecordHOW[Data::Record::Tuple]].new_type: $block, :$name
}

multi sub infix:«(><)»(List:D $lhs is raw, Data::Record::Tuple:U $rhs is raw --> Data::Record::Tuple:D) is export {
    $rhs.new: $lhs
}
multi sub infix:«(><)»(Data::Record::Tuple:D $lhs is raw, Data::Record::Tuple:U $rhs is raw --> Data::Record::Tuple:D) is export {
    $rhs.new: $lhs.record
}
multi sub infix:«(><)»(Data::Record::Tuple:U $lhs is raw, List:D $rhs is raw --> Data::Record::Tuple:D) is export {
    $lhs.new: $rhs
}
multi sub infix:«(><)»(Data::Record::Tuple:U $lhs is raw, Data::Record::Tuple:D $rhs is raw --> Data::Record::Tuple:D) is export {
    $lhs.new: $rhs.record
}

multi sub infix:«(<<)»(List:D $lhs is raw, Data::Record::Tuple:U $rhs is raw --> Data::Record::Tuple:D) is export {
    $rhs.new: $lhs, :consume
}
multi sub infix:«(<<)»(Data::Record::Tuple:D $lhs is raw, Data::Record::Tuple:U $rhs is raw --> Data::Record::Tuple:D) is export {
    $rhs.new: $lhs.record, :consume
}
multi sub infix:«(<<)»(Data::Record::Tuple:U $lhs is raw, List:D $rhs is raw --> Data::Record::Tuple:D) is export {
    $lhs.new: $rhs, :subsume
}
multi sub infix:«(<<)»(Data::Record::Tuple:U $lhs is raw, Data::Record::Tuple:D $rhs is raw --> Data::Record::Tuple:D) is export {
    $lhs.new: $rhs.record, :subsume
}

multi sub infix:«(>>)»(List:D $lhs is raw, Data::Record::Tuple:U $rhs is raw --> Data::Record::Tuple:D) is export {
    $rhs.new: $lhs, :subsume
}
multi sub infix:«(>>)»(Data::Record::Tuple:D $lhs is raw, Data::Record::Tuple:U $rhs is raw --> Data::Record::Tuple:D) is export {
    $rhs.new: $lhs.record, :subsume
}
multi sub infix:«(>>)»(Data::Record::Tuple:U $lhs is raw, List:D $rhs is raw --> Data::Record::Tuple:D) is export {
    $lhs.new: $rhs, :consume
}
multi sub infix:«(>>)»(Data::Record::Tuple:U $lhs is raw, Data::Record::Tuple:D $rhs is raw --> Data::Record::Tuple:D) is export {
    $lhs.new: $rhs.record, :consume
}

multi sub infix:«(<>)»(List:D $lhs is raw, Data::Record::Tuple:U $rhs is raw --> Data::Record::Tuple:D) is export {
    $rhs.new: $lhs, :coerce
}
multi sub infix:«(<>)»(Data::Record::Tuple:D $lhs is raw, Data::Record::Tuple:U $rhs is raw --> Data::Record::Tuple:D) is export {
    $rhs.new: $lhs.record, :coerce
}
multi sub infix:«(<>)»(Data::Record::Tuple:U $lhs is raw, List:D $rhs is raw --> Data::Record::Tuple:D) is export {
    $lhs.new: $rhs, :coerce
}
multi sub infix:«(<>)»(Data::Record::Tuple:U $lhs is raw, Data::Record::Tuple:D $rhs is raw --> Data::Record::Tuple:D) is export {
    $lhs.new: $rhs.record, :coerce
}

multi sub infix:<eqv>(List:D $lhs is raw, Data::Record::Tuple:D $rhs is raw --> Bool:D) is export {
    $lhs eqv $rhs.unrecord
}
multi sub infix:<eqv>(Data::Record::Tuple:D $lhs is raw, List:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs
}
multi sub infix:<eqv>(Data::Record::Tuple:D $lhs is raw, Data::Record::Tuple:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs.unrecord
}
