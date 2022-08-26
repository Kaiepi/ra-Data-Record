use v6.e.PREVIEW;
use MetamodelX::RecorderHOW;
use MetamodelX::RecordTemplateHOW;
use MetamodelX::RecordLifter;
use Data::Record::Mode;
use Data::Record::Instance;
use Data::Record::Exceptions;

my constant &infix:<@~~> = MetamodelX::RecordLifter[Data::Record::Instance].^pun;

my class ListIterator { ... }

my class ArrayIterator { ... }

class Data::Record::List does Data::Record::Instance[List:D] does Iterable does Positional {
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
        my @record := T.from-iterator: ListIterator.new: THIS, $mode, 'list reification', $original;
        sink @record unless @record.is-lazy;
        self.bless: :@record
    }

    method wrap(::?CLASS:_ ::THIS: ::T List:D $original is raw --> List:D) {
        T.from-iterator: ListIterator.new: THIS, WRAP, 'list reification', $original
    }

    method consume(::?CLASS:_ ::THIS: ::T List:D $original is raw --> List:D) {
        T.from-iterator: ListIterator.new: THIS, CONSUME, 'list reification', $original
    }

    method subsume(::?CLASS:_ ::THIS: ::T List:D $original is raw --> List:D) {
        T.from-iterator: ListIterator.new: THIS, SUBSUME, 'list reification', $original
    }

    method coerce(::?CLASS:_ ::THIS: ::T List:D $original is raw --> List:D) {
        T.from-iterator: ListIterator.new: THIS, COERCE, 'list reification', $original
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
        my Str:D $raku = '[@ ' ~ self.^fields.map(*.raku).join(', ') ~ ' @]';
        my Str:D $name = self.^name;
        $raku ~= ":name('$name')" unless $name eq MetamodelX::RecordHOW::ANON_NAME;
        $raku
    }

    do {
        sub ACCEPTS(Mu \a, Mu \b --> Bool:D) is hidden-from-backtrace { a.ACCEPTS: b }

        multi method ACCEPTS(::?CLASS:U: List:D \topic --> Bool:D) {
            my @fields  := self.^fields;
            my @matches := eager (|@fields xx *) Z[[&ACCEPTS]] topic;
            ? [&&] |@matches, !topic.is-lazy, (@fields.elems andthen @matches %% * orelse True)
        }
    }

    method EXISTS-POS(::?CLASS:D: Mu $pos is raw --> Bool:D) {
        @!record.EXISTS-POS: $pos
    }

    method AT-POS(::?CLASS:D: Mu $pos is raw --> Mu) is raw {
        @!record.AT-POS: $pos
    }

    method BIND-POS(::?CLASS:D: Mu $pos is raw, Mu $value is raw --> Mu) is raw {
        state $*operation = 'binding';
        @!record.BIND-POS: $pos, self.^map_field: $pos, $value
    }

    method ASSIGN-POS(::?CLASS:D: Mu $pos is raw, Mu $value is raw --> Mu) is raw {
        state $*operation = 'assignment';
        @!record.ASSIGN-POS: $pos, self.^map_field: $pos, $value
    }

    method DELETE-POS(::?CLASS:D: Mu $pos is raw --> Mu) is raw {
        # XXX: This should be typechecking for the definiteness of the field
        # this position corresponds to and ensuring that, if this will leave
        # an empty space in the record, the field is not definite; however,
        # array slices complicate things.
        @!record.DELETE-POS: $pos
    }

    method push(::THIS ::?CLASS:D: **@values is raw --> ::?CLASS:D) {
        @!record.push: Slip.from-iterator: ArrayIterator.new: THIS, WRAP, 'push', @values;
        self
    }

    method pop(::?CLASS:D ::THIS:) is raw {
        self.^enforce_singleton: 'pop'; # XXX: "enforce"
        @!record.pop
    }

    method shift(::?CLASS:D ::THIS: --> Mu) is raw {
        self.^enforce_singleton: 'pop'; # XXX: "enforce"
        @!record.shift
    }

    method unshift(::THIS ::?CLASS:D: **@values is raw --> ::?CLASS:D) {
        @!record.unshift: Slip.from-iterator: ArrayIterator.new: THIS, WRAP, 'unshift', @values;
        self
    }

    proto method prepend(|) {*}
    multi method prepend(::THIS ::?CLASS:D: Iterable:D $values is raw --> ::?CLASS:D) {
        @!record.prepend: Seq.new: ArrayIterator.new: THIS, WRAP, 'prepend', $values;
        self
    }
    multi method prepend(::THIS ::?CLASS:D: **@values is raw --> ::?CLASS:D) {
        @!record.prepend: Slip.from-iterator: ArrayIterator.new: THIS, WRAP, 'prepend', @values;
        self
    }

    proto method append(|) {*}
    multi method append(::THIS ::?CLASS:D: Iterable:D $values is raw --> ::?CLASS:D) {
        @!record.append: Seq.new: ArrayIterator.new: THIS, WRAP, 'append', $values;
        self
    }
    multi method append(::THIS ::?CLASS:D: **@values is raw --> ::?CLASS:D) {
        @!record.append: Slip.from-iterator: ArrayIterator.new:  THIS, WRAP, 'append', @values;
        self
    }

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

    method ^arity($type is raw --> Int:_) {
        my @fields := self.fields: $type;
        @fields.elems
    }

    method ^get_field($type is raw, $key is raw) {
        my @fields := self.fields: $type;
        @fields.AT-POS: (@fields.elems andthen $key % $_ orelse $key)
    }

    method ^declares_field($type is raw, $key is raw) {
        my @fields := self.fields: $type;
        @fields.EXISTS-POS: (@fields.elems andthen $key % $_ orelse $key)
    }

    method ^enforce_singleton($type is raw, Str:D $operation --> Nil) {
        my @fields := self.fields: $type;
        X::Data::Record::Missing.new(
            :$operation, :$type, :what<index>, :key(0), :field(@fields[0])
        ).throw unless (@fields.elems andthen $_ == 1);
    }

    method ^coerce_void($type is raw, $key is raw) {
        my @fields := self.fields: $type;
        my $field  := @fields.AT-POS: $key % @fields.elems;
        X::Data::Record::Definite.new(
           :$type, :what<index>, :$key, :value($field)
        ).throw if $field.HOW.archetypes.definite && $field.^definite;
        $field
    }

    method ^map_field($type is raw, $key is raw is copy, Mu $value is raw, :$mode = WRAP) is raw {
        my @fields := self.fields: $type;
        @fields.elems andthen $key := $key % $_;
        @fields.EXISTS-POS($key)
          ?? ($value @~~ @fields.AT-POS($key) :$mode)
          !! $value
    }

    method ^map_it_field(
        $type is raw, $key is raw, Mu $field is raw, Mu $value is raw,
        :$mode!,
        :$keep!,
        :$drop!,
    ) is raw {
        my @fields := self.fields: $type;
        $value =:= IterationEnd
          ?? self."keep_it_$keep"($type, $key, $field)
          !! @fields.EXISTS-POS((@fields.elems andthen $key % $_ orelse $key))
            ?? ($value @~~ $field :$mode)
            !! self."drop_it_$drop"($type, $key, $field)
    }

    method ^keep_it_missing($type is raw, $key is raw, Mu $field is raw --> IterationEnd) {
        my @fields := self.fields: $type;
        X::Data::Record::Missing.new(
            :$*operation, :$type, :what<index>, :$key, :$field
        ).throw if (@fields.elems andthen not $key %% $_);
    }

    method ^keep_it_coercing($type is raw, $key is raw, Mu $field is raw) {
        my @fields := self.fields: $type;
        (@fields.elems andthen $key %% $_)
          ?? IterationEnd
          !! self.coerce_void($type, $key)
    }

    method ^drop_it_now($type is raw, $key is raw, Mu $field is raw --> IterationEnd) {
        # Follow through on a return.
    }

    method ^drop_it_again($type is raw, $key is raw, Mu $field is raw --> IterationEnd) {
        next
    }
}

#|[ Iterator for lists that are to become records. Classes that do this role
    typecheck the list's values and coerce any of them that correspond to fields
    that are records in some manner. ]
my class ListIterator does Iterator {
    has Data::Record::List:U $.type      is required;
    has Data::Record::Mode:D $.mode      is required;
    has Str:D                $.operation is required;
    has Iterator:D           $!keys      is required;
    has Iterator:D           $!fields    is required;
    has Iterator:D           $!values    is required;

    submethod BUILD(::?CLASS:D: :$type! is raw, :$!mode!, :$!operation!, :$values! --> Nil) {
        $!type   := $type;
        $!keys   := (0..*).iterator;
        $!fields := (|$type.^fields xx *).iterator;
        $!values := $values.iterator;
    }

    method new(::?CLASS:_: $type is raw, $mode, $operation, $values --> ::?CLASS:D) {
        self.bless: :$type, :$mode, :$operation, :$values
    }

    method pull-one(::?CLASS:D:) is raw {
        my $*operation := $!operation;
        self."$!mode"()
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }

    #|[ If any value in the given list cannot typecheck, an exception will be
        thrown; if the arity of the list does not match that of the record, it
        will be considered to have missing fields and thus an exception will be
        thrown. ]
    method wrap(::?CLASS:D:) is raw {
        $!type.^map_it_field:
            $!keys.pull-one, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<now>, :keep<missing>
    }

    #|[ Any fields that do not typecheck will be stripped from the list, but if
        the arity of the list does not match that of the record, it will be
        considered to have missing fields and thus an exception will be thrown. ]
    method consume(::?CLASS:D:) is raw {
        loop { return-rw $!type.^map_it_field:
            $!keys.pull-one, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<again>, :keep<missing> }
    }

    #|[ If any fields are missing from the list, they will be stubbed (if
        possible), but if any fields do not typecheck, then an exception will
        be thrown. Note that it's impossible for extraneous fields to exist in
        a list. ]
    method subsume(::?CLASS:D:) is raw {
        $!type.^map_it_field:
            $!keys.pull-one, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<now>, :keep<coercing>
    }

    #|[ If any values in the given list cannot typecheck, they will be stripped
        from the list; if any fields are missing from the given list, they will
        be stubbed (if possible).  This should only throw if a definite field
        is missing. ]
    method coerce(::?CLASS:D:) is raw {
        loop { return-rw $!type.^map_it_field:
            $!keys.pull-one, $!fields.pull-one, $!values.pull-one,
            :$!mode, :drop<now>, :keep<coercing> }
    }

}

#|[ Iterator for array ops taking lists of values (push/unshift/append/prepend).
    This is mostly identical to ListIterator, but adds
    behaviour to handle checking the arity of the list, which is handled in
    such a way as to support lazy lists. ]
my class ArrayIterator is ListIterator {
    method push-all(::?CLASS:D: \target --> IterationEnd) {
        my IterationBuffer \buffer .= new;
        for 0..* -> $count is raw {
            (my $result := self.pull-one) =:= IterationEnd
              ?? last(target.append: buffer)
              !! buffer.push($result)
        }
    }
}
#=[ Array ops can get passed lazy lists, though Array does not support
    this. We can't throw X::Cannot::Lazy ourselves; what if someone defines
    their own List subtype with methods that support them? Instead, we can
    check the arity whenever this iterator's values get pushed onto the
    relevant iterator of our record, so we have some way to check the
    list's arity without using the elems method. ]

multi sub circumfix:<[@ @]>(+@fields is raw, Str :$name --> Mu) is export {
    my $obj := MetamodelX::RecorderHOW[
        List:D, Data::Record::List
    ].new_type: @fields, :$name;
    my $how := $obj.HOW;
    $how.compose: $obj
}
multi sub circumfix:<[@ @]>(Block:D $block is raw, Str :$name --> Mu) is export {
    my $obj := MetamodelX::RecordTemplateHOW[
        MetamodelX::RecorderHOW[List:D, Data::Record::List]
    ].new_type: $block, :$name;
    my $how := $obj.HOW;
    $how.compose: $obj
}

multi sub infix:«(><)»(List:D $lhs is raw, Data::Record::List:U $rhs is raw --> Data::Record::List:D) is export {
    $rhs.new: $lhs
}
multi sub infix:«(><)»(Data::Record::List:D $lhs is raw, Data::Record::List:U $rhs is raw --> Data::Record::List:D) is export {
    $rhs.new: $lhs.record
}
multi sub infix:«(><)»(Data::Record::List:U $lhs is raw, List:D $rhs is raw --> Data::Record::List:D) is export {
    $lhs.new: $rhs
}
multi sub infix:«(><)»(Data::Record::List:U $lhs is raw, Data::Record::List:D $rhs is raw --> Data::Record::List:D) is export {
    $lhs.new: $rhs.record
}

multi sub infix:«(<<)»(List:D $lhs is raw, Data::Record::List:U $rhs is raw --> Data::Record::List:D) is export {
    $rhs.new: $lhs, :consume
}
multi sub infix:«(<<)»(Data::Record::List:D $lhs is raw, Data::Record::List:U $rhs is raw --> Data::Record::List:D) is export {
    $rhs.new: $lhs.record, :consume
}
multi sub infix:«(<<)»(Data::Record::List:U $lhs is raw, List:D $rhs is raw --> Data::Record::List:D) is export {
    $lhs.new: $rhs, :subsume
}
multi sub infix:«(<<)»(Data::Record::List:U $lhs is raw, Data::Record::List:D $rhs is raw --> Data::Record::List:D) is export {
    $lhs.new: $rhs.record, :subsume
}

multi sub infix:«(>>)»(List:D $lhs is raw, Data::Record::List:U $rhs is raw --> Data::Record::List:D) is export {
    $rhs.new: $lhs, :subsume
}
multi sub infix:«(>>)»(Data::Record::List:D $lhs is raw, Data::Record::List:U $rhs is raw --> Data::Record::List:D) is export {
    $rhs.new: $lhs.record, :subsume
}
multi sub infix:«(>>)»(Data::Record::List:U $lhs is raw, List:D $rhs is raw --> Data::Record::List:D) is export {
    $lhs.new: $rhs, :consume
}
multi sub infix:«(>>)»(Data::Record::List:U $lhs is raw, Data::Record::List:D $rhs is raw --> Data::Record::List:D) is export {
    $lhs.new: $rhs.record, :consume
}

multi sub infix:«(<>)»(List:D $lhs is raw, Data::Record::List:U $rhs is raw --> Data::Record::List:D) is export {
    $rhs.new: $lhs, :coerce
}
multi sub infix:«(<>)»(Data::Record::List:D $lhs is raw, Data::Record::List:U $rhs is raw --> Data::Record::List:D) is export {
    $rhs.new: $lhs.record, :coerce
}
multi sub infix:«(<>)»(Data::Record::List:U $lhs is raw, List:D $rhs is raw --> Data::Record::List:D) is export {
    $lhs.new: $rhs, :coerce
}
multi sub infix:«(<>)»(Data::Record::List:U $lhs is raw, Data::Record::List:D $rhs is raw --> Data::Record::List:D) is export {
    $lhs.new: $rhs.record, :coerce
}

multi sub infix:<eqv>(List:D $lhs is raw, Data::Record::List:D $rhs is raw --> Bool:D) is export {
    $lhs eqv $rhs.unrecord
}
multi sub infix:<eqv>(Data::Record::List:D $lhs is raw, List:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs
}
multi sub infix:<eqv>(Data::Record::List:D $lhs is raw, Data::Record::List:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs.unrecord
}
