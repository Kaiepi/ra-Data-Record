use v6.e.PREVIEW;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
use Data::Record::Mode;
use Data::Record::Exceptions;
use Data::Record::Instance;
use Data::Record::Lifter;
unit class Data::Record::Tuple does Data::Record::Instance[List:D] does Iterable does Positional;

my class TupleIterator { ... }

has $.record is built(:bind) is default(anon class Phantom is Nil {
    submethod FALLBACK(|) {
        fail new X::Attribute::Required:
            :name<$!record>,
            :why('a record must enforce its type')
    }
});

multi method new(::?CLASS:_:) {
    self.CREATE
}
multi method new(::?CLASS:_: List:D $original is raw, Data::Record::Mode:D :$mode = WRAP) {
    self.CREATE."$mode"($original)
}

multi method CALL-ME(::?CLASS:_: List:D $original is raw, Bool:D :wrap($)! where ?* --> ::?CLASS:D) {
    self.CREATE.wrap: $original
}
multi method CALL-ME(::?CLASS:_: List:D $original is raw, Bool:D :consume($)! where ?* --> ::?CLASS:D) {
    self.CREATE.consume: $original
}
multi method CALL-ME(::?CLASS:_: List:D $original is raw, Bool:D :subsume($)! where ?* --> ::?CLASS:D) {
    self.CREATE.subsume: $original
}
multi method CALL-ME(::?CLASS:_: List:D $original is raw, Bool:D :coerce($)! where ?* --> ::?CLASS:D) {
    self.CREATE.coerce: $original
}

method wrap(::?CLASS:D ::THIS: List:D ::T $original is raw) {
    $!record := T.from-iterator: TupleIterator.new: THIS, WRAP, 'list reification', $original;
    sink $!record unless $!record.is-lazy;
    self
}

method consume(::?CLASS:D ::THIS: List:D ::T $original is raw) {
    $!record := T.from-iterator: TupleIterator.new: THIS, CONSUME, 'list reification', $original;
    sink $!record unless $!record.is-lazy;
    self
}

method subsume(::?CLASS:D ::THIS: List:D ::T $original is raw) {
    $!record := T.from-iterator: TupleIterator.new: THIS, SUBSUME, 'list reification', $original;
    sink $!record unless $!record.is-lazy;
    self
}

method coerce(::?CLASS:_ ::THIS: List:D ::T $original is raw) {
    $!record := T.from-iterator: TupleIterator.new: THIS, COERCE, 'list reification', $original;
    sink $!record unless $!record.is-lazy;
    self
}

method fields(::?CLASS:_: --> List:D) { self.^fields }

do { # hide this sub
    proto unrecord(Mu) is raw                          {*}
    multi unrecord(Data::Record::Instance:D \recorded) { recorded.unrecord }
    multi unrecord(Mu \value)                          { value }

    method unrecord(::?CLASS:D: --> List:D) {
        $!record.WHAT.from-iterator: $!record.map(&unrecord).iterator
    }
}

multi method raku(::?CLASS:U: --> Str:D) {
    my Str:D $raku = '<@ ' ~ self.^fields.map(*.raku).join(', ') ~ ' @>';
    my Str:D $name = self.^name;
    $raku ~= ":name('$name')" unless self.^is_anonymous;
    $raku
}

do {
    only ACCEPTS(Mu \a, Mu \b) is raw is hidden-from-backtrace { a.ACCEPTS: b }

    multi method ACCEPTS(::?CLASS:U: List:D \topic) {
        my @fields := self.^fields;
        (my $matches := [&&] @fields Z[[&ACCEPTS]] topic)
            & !topic.is-lazy
            & topic.elems == $matches.elems
    }
}

method EXISTS-POS(::?CLASS:D: Int:D $pos --> Bool:D) {
    $!record.EXISTS-POS: $pos
}

method AT-POS(::?CLASS:D: Mu $pos) is raw {
    $!record.AT-POS: $pos
}

method BIND-POS(::?CLASS:D: Mu $pos, Mu $value is raw) is raw {
    CONTROL { .flunk: 'binding' when CX::Rest }
    $!record.BIND-POS: $pos, self.^map_field: $pos, $value
}

method ASSIGN-POS(::?CLASS:D: Mu $pos, Mu $value is raw) is raw {
    CONTROL { .flunk: 'assignment' when CX::Rest }
    $!record.ASSIGN-POS: $pos, self.^map_field: $pos, $value
}

method DELETE-POS(::?CLASS:D: Mu $pos --> Nil) is raw {
    CONTROL { .flunk: 'deletion' when CX::Rest }
    (let $!record).DELETE-POS: $pos;
    self.^map_field: $pos, $!record.AT-POS: $pos
}

method push(::?CLASS:D: | --> Nil)    { self.^enforce_immutability: 'push' }
method pop(::?CLASS:D: | --> Nil)     { self.^enforce_immutability: 'pop' }
method shift(::?CLASS:D: | --> Nil)   { self.^enforce_immutability: 'shift' }
method unshift(::?CLASS:D: | --> Nil) { self.^enforce_immutability: 'unshift' }
method append(::?CLASS:D: | --> Nil)  { self.^enforce_immutability: 'append' }
method prepend(::?CLASS:D: | --> Nil) { self.^enforce_immutability: 'prepend' }

multi method iterator(::?CLASS:D:) { $!record.iterator }

multi method list(::?CLASS:D:) is raw { self }

multi method hash(::?CLASS:D:) is raw { $!record.hash }

method is-lazy(::?CLASS:_:) {
    self.DEFINITE && $!record.is-lazy
}

method cache(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? $!record.is-lazy
        ?? self.new($!record.cache)
        !! self
      !! @(self).cache
}

method eager(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? $!record.is-lazy
        ?? self.new($!record.eager)
        !! self
      !! @(self).eager
}

method lazy(::?CLASS:_: --> ::?CLASS:D) {
    self.DEFINITE
      ?? $!record.is-lazy
        ?? self
        !! self.new($!record.lazy)
      !! @(self).lazy
}

multi method elems(::?CLASS:D:) { $!record.elems }

multi method keys(::?CLASS:D:) { $!record.keys }

multi method values(::?CLASS:D:) { $!record.values }

multi method kv(::?CLASS:D:) { $!record.kv }

multi method pairs(::?CLASS:D:) { $!record.pairs }

multi method antipairs(::?CLASS:D:) { $!record.antipairs }

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
    has Int:D                 $.key       is default(0);
    has Iterator:D            $.fields    is required;
    has Iterator:D            $.values    is required;

    submethod BUILD(::?CLASS:D: Mu :$type! is raw, :$!mode!, :$!operation!, :$values! is raw --> Nil) {
        $!type   := $type;
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
            $!key++, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<more>, :keep<missing>
    }

    #|[ The list must have an arity greater than or equal to the tuple type; if
        it's greater, extraneous values will be stripped. If any values are
        missing or values corresponding to fields don't typecheck, an exception
        will be thrown. ]
    method consume(::?CLASS:D:) is raw {
        CONTROL { .flunk: $!operation when CX::Rest }
        $!type.^map_it_field:
            $!key++, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<less>, :keep<missing>
    }

    #|[ The list must have an arity lesser than or equal to the tuple type's;
        if it's lesser, missing values will be stubbed (if possible).  If any
        values don't typecheck as their corresponding fields, an exception will
        be thrown. ]
    method subsume(::?CLASS:D:) is raw {
        CONTROL { .flunk: $!operation when CX::Rest }
        $!type.^map_it_field:
            $!key++, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<more>, :keep<coercing>
    }

    #|[ Coerces a list. Arity does not matter; missing values are stubbed (if
        possible) and extraneous values are stripped. If any values don't
        typecheck as their corresponding fields, an exception will be thrown. ]
    method coerce(::?CLASS:D:) is raw {
        CONTROL { .flunk: $!operation when CX::Rest }
        $!type.^map_it_field:
            $!key++, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<less>, :keep<coercing>
    }
}

multi method beget(::?CLASS:_: +@fields is raw, Str:_ :$name) is raw {
    my $obj := MetamodelX::RecordHOW[
        List:D, Data::Record::Tuple
    ].new_type: @fields, :$name;
    my $how := $obj.HOW;
    $how.compose: $obj
}
multi method beget(::?CLASS:_: Block:D $block is raw, Str:_ :$name) is raw {
    my $obj := MetamodelX::RecordTemplateHOW[
        MetamodelX::RecordHOW[List:D, Data::Record::Tuple]
    ].new_type: $block, :$name;
    my $how := $obj.HOW;
    $how.compose: $obj
}

multi infix:<eqv>(List:D $lhs is raw, Data::Record::Tuple:D $rhs is raw) is raw is export {
    samewith $lhs, $rhs.unrecord
}
multi infix:<eqv>(Data::Record::Tuple:D $lhs is raw, List:D $rhs is raw) is raw is export {
    samewith $lhs.unrecord, $rhs
}
