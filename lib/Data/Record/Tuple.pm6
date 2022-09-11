use v6.d;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
use Data::Record::Mode;
use Data::Record::Exceptions;
use Data::Record::Instance;
use Data::Record::Lifter;
unit class Data::Record::Tuple does Data::Record::Instance[List:D] does Iterable does Positional;

my class TupleIterator { ... }

has @!record is required;

submethod BUILD(::?CLASS:D: :@record is raw --> Nil) {
    @!record := @record;
}

multi method new(::?CLASS:_: List:D $original is raw, Bool:D :wrap($)! where ?* --> ::?CLASS:D) {
    my @record := self.wrap: $original;
    sink @record unless @record.is-lazy;
    self.bless: :@record
}
multi method new(::?CLASS:_: List:D $original is raw, Bool:D :consume($)! where ?* --> ::?CLASS:D) {
    my @record := self.consume: $original;
    sink @record unless @record.is-lazy;
    self.bless: :@record
}
multi method new(::?CLASS:_: List:D $original is raw, Bool:D :subsume($)! where ?* --> ::?CLASS:D) {
    my @record := self.subsume: $original;
    sink @record unless @record.is-lazy;
    self.bless: :@record
}
multi method new(::?CLASS:_: List:D $original is raw, Bool:D :coerce($)! where ?* --> ::?CLASS:D) {
    my @record := self.coerce: $original;
    sink @record unless @record.is-lazy;
    self.bless: :@record
}
multi method new(::?CLASS:_ ::THIS: List:D ::T $original is raw, Data::Record::Mode:D :$mode = WRAP --> ::?CLASS:D) {
    my @record := T.from-iterator: TupleIterator.new: THIS, $mode, 'tuple reification', $original;
    sink @record unless @record.is-lazy; # Reify eager lists for eager typechecking.
    self.bless: :@record
}

method wrap(::?CLASS:_ ::THIS: ::T List:D $original is raw --> List:D) {
    T.from-iterator: TupleIterator.new: THIS, WRAP, 'tuple reification', $original
}

method consume(::?CLASS:_ ::THIS: ::T List:D $original is raw --> List:D) {
    T.from-iterator: TupleIterator.new: THIS, CONSUME, 'tuple reification', $original
}

method subsume(::?CLASS:_ ::THIS: ::T List:D $original is raw --> List:D) {
    T.from-iterator: TupleIterator.new: THIS, SUBSUME, 'tuple reification', $original
}

method coerce(::?CLASS:_ ::THIS: ::T List:D $original is raw --> List:D) {
    T.from-iterator: TupleIterator.new: THIS, COERCE, 'tuple reification', $original
}

method fields(::?CLASS:_: --> List:D) { self.^fields }

method record(::?CLASS:D: --> List:D) { @!record }

do { # hide this sub
    proto sub unrecord(Mu) is raw                          {*}
    multi sub unrecord(Data::Record::Instance:D \recorded) { recorded.unrecord }
    multi sub unrecord(Mu \value)                          { value }

    method unrecord(::?CLASS:D: --> List:D) {
        @!record.WHAT.from-iterator: @!record.map(&unrecord).iterator
    }
}

multi method raku(::?CLASS:U: --> Str:D) {
    my Str:D $raku = '<@ ' ~ self.^fields.map(*.raku).join(', ') ~ ' @>';
    my Str:D $name = self.^name;
    $raku ~= ":name('$name')" unless self.^is_anonymous;
    $raku
}

do {
    sub ACCEPTS(Mu \a, Mu \b) is raw is hidden-from-backtrace { a.ACCEPTS: b }

    multi method ACCEPTS(::?CLASS:U: List:D \topic) {
        my @fields := self.^fields;
        (my $matches := [&&] @fields Z[[&ACCEPTS]] topic)
            & !topic.is-lazy
            & topic.elems == $matches.elems
    }
}

method EXISTS-POS(::?CLASS:D: Int:D $pos --> Bool:D) {
    @!record.EXISTS-POS: $pos
}

method AT-POS(::?CLASS:D: Mu $pos) is raw {
    die self.^suggest_bounds: $pos unless self.^declares_field: $pos;
    @!record.AT-POS: $pos
}

method BIND-POS(::?CLASS:D: Mu $pos, Mu $value is raw) is raw {
    CONTROL { .flunk: 'binding' when CX::Rest }
    @!record.BIND-POS: $pos, self.^map_field: $pos, $value
}

method ASSIGN-POS(::?CLASS:D: Mu $pos, Mu $value is raw) is raw {
    CONTROL { .flunk: 'assignment' when CX::Rest }
    @!record.ASSIGN-POS: $pos, self.^map_field: $pos, $value
}

method DELETE-POS(::?CLASS:D: Mu $pos --> Nil) is raw {
    self.^enforce_immutability: 'deletion'
}

method push(::?CLASS:D: | --> Nil)    { self.^enforce_immutability: 'push' }
method pop(::?CLASS:D: | --> Nil)     { self.^enforce_immutability: 'pop' }
method shift(::?CLASS:D: | --> Nil)   { self.^enforce_immutability: 'shift' }
method unshift(::?CLASS:D: | --> Nil) { self.^enforce_immutability: 'unshift' }
method append(::?CLASS:D: | --> Nil)  { self.^enforce_immutability: 'append' }
method prepend(::?CLASS:D: | --> Nil) { self.^enforce_immutability: 'prepend' }

multi method iterator(::?CLASS:D:) { @!record.iterator }

multi method list(::?CLASS:D:) is raw { self }

multi method hash(::?CLASS:D:) is raw { @!record.hash }

method is-lazy(::?CLASS:_:) {
    self.DEFINITE && @!record.is-lazy
}

method cache(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? @!record.is-lazy
        ?? self.new(@!record.cache)
        !! self
      !! @(self).cache
}

method eager(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? @!record.is-lazy
        ?? self.new(@!record.eager)
        !! self
      !! @(self).eager
}

method lazy(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? @!record.is-lazy
        ?? self
        !! self.new(@!record.lazy)
      !! @(self).lazy
}

multi method elems(::?CLASS:D:) { @!record.elems }

multi method keys(::?CLASS:D:) { @!record.keys }

multi method values(::?CLASS:D:) { @!record.values }

multi method kv(::?CLASS:D:) { @!record.kv }

multi method pairs(::?CLASS:D:) { @!record.pairs }

multi method antipairs(::?CLASS:D:) { @!record.antipairs }

method ^get_field(Mu $type is raw, $key) {
    @.fields($type).AT-POS($key)
}

method ^declares_field(Mu $type is raw, $key) {
    @.fields($type).EXISTS-POS($key)
}

method ^enforce_immutability(::THIS, $operation --> Nil) {
    X::Data::Record::Immutable.new(:$operation, :type(THIS)).throw
}

method ^suggest_bounds(::THIS, $key --> Exception:D) {
    X::Data::Record::OutOfBounds.new: :type(THIS), :what<index>, :$key
}

method ^map_field(
    Mu $type is raw, $key, Mu $value is raw,
    Data::Record::Mode:D :$mode = WRAP,
    :$drop = 'unbounded'
) is raw {
    my @fields := self.fields: $type;
    @fields.EXISTS-POS($key)
      ?? ($value @~~ @fields.AT-POS($key) :$mode)
      !! self."drop_$drop"($type, $key, $value)
}

method ^drop_unbounded(::THIS Mu $type is raw, $key, Mu $value is raw --> Nil) {
    self.suggest_bounds($type, $key).throw
}

method ^map_it_field(
    Mu $type is raw, $key, Mu $field is raw, Mu $value is raw,
    :$mode!,
    :$drop!,
    :$keep!,
) is raw {
    $field =:= IterationEnd
      ?? self."drop_it_$drop"($type, $key, $value)
      !! $value =:= IterationEnd
        ?? self."keep_it_$keep"($type, $key, $field)
        !! ($value @~~ $field :$mode)
}

method ^drop_it_more(Mu $type is raw, $key, Mu $value is raw --> IterationEnd) {
    X::Data::Record::Extraneous.new(
        :$*operation, :$type, :what<index>, :$key, :$value
    ).throw unless $value =:= IterationEnd;
}

method ^drop_it_less(Mu, Mu, Mu --> IterationEnd) {
    # Follow through on a return.
}

method ^keep_it_missing(Mu $type is raw, $key, Mu $field is raw --> IterationEnd) {
    X::Data::Record::Missing.new(:$*operation, :$type, :what<index>, :$key, :$field).throw;
}

method ^keep_it_coercing(Mu $type is raw, $key, Mu $field is raw) is raw {
    X::Data::Record::Definite.new(
        :$type, :what<index>, :$key, :value($field)
    ).throw if $field.HOW.archetypes.definite && $field.^definite;
    $field
}

#|[ Iterator for tuples (lists of fixed length) that are to become records.
    Typechecks the list's values and coerces any record fields along the way. ]
my class TupleIterator does Iterator {
    has Data::Record::Tuple:U $.type      is required;
    has Data::Record::Mode:D  $.mode      is required;
    has Str:D                 $.operation is required;
    has Iterator:D            $.keys      is required;
    has Iterator:D            $.fields    is required;
    has Iterator:D            $.values    is required;

    submethod BUILD(::?CLASS:D: Mu :$type! is raw, :$!mode!, :$!operation!, :$values! is raw --> Nil) {
        $!type   := $type;
        $!keys   := (0..*).iterator;
        $!fields := $!type.^fields.iterator;
        $!values := $values.iterator;
    }

    method new(::?CLASS:_: Mu $type is raw, $mode, $operation, $values is raw --> ::?CLASS:D) {
        self.bless: :$type, :$mode, :$operation, :$values
    }

    method pull-one(::?CLASS:D:) is raw {
        my $*operation := $!operation;
        self."$!mode"()
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }

    #|[ The list must have an arity equal to the tuple type's and all values
        must typecheck as their corresponding fields, otherwise an exception
        will be thrown. ]
    method wrap(::?CLASS:D:) is raw {
        CONTROL { .flunk: $!operation when CX::Rest }
        $!type.^map_it_field:
            $!keys.pull-one, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<more>, :keep<missing>
    }

    #|[ The list must have an arity greater than or equal to the tuple type; if
        it's greater, extraneous values will be stripped. If any values are
        missing or values corresponding to fields don't typecheck, an exception
        will be thrown. ]
    method consume(::?CLASS:D:) is raw {
        CONTROL { .flunk: $!operation when CX::Rest }
        $!type.^map_it_field:
            $!keys.pull-one, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<less>, :keep<missing>
    }

    #|[ The list must have an arity lesser than or equal to the tuple type's;
        if it's lesser, missing values will be stubbed (if possible).  If any
        values don't typecheck as their corresponding fields, an exception will
        be thrown. ]
    method subsume(::?CLASS:D:) is raw {
        CONTROL { .flunk: $!operation when CX::Rest }
        $!type.^map_it_field:
            $!keys.pull-one, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<more>, :keep<coercing>
    }

    #|[ Coerces a list. Arity does not matter; missing values are stubbed (if
        possible) and extraneous values are stripped. If any values don't
        typecheck as their corresponding fields, an exception will be thrown. ]
    method coerce(::?CLASS:D:) is raw {
        CONTROL { .flunk: $!operation when CX::Rest }
        $!type.^map_it_field:
            $!keys.pull-one, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<less>, :keep<coercing>
    }
}

multi sub circumfix:«<@ @>»(+@fields is raw, Str:_ :$name --> Mu) is export {
    my $obj := MetamodelX::RecordHOW[
        List:D, Data::Record::Tuple
    ].new_type: @fields, :$name;
    my $how := $obj.HOW;
    $how.compose: $obj
}
multi sub circumfix:«<@ @>»(Block:D $block is raw, Str:_ :$name --> Mu) is export {
    my $obj := MetamodelX::RecordTemplateHOW[
        MetamodelX::RecordHOW[List:D, Data::Record::Tuple]
    ].new_type: $block, :$name;
    my $how := $obj.HOW;
    $how.compose: $obj
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
