use v6.d;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
use Data::Record::Instance;
use Data::Record::Exceptions;

#|[ Iterator for lists that are to become records. Classes that do this role
    typecheck the list's values and coerce any of them that correspond to fields
    that are records in some manner. ]
my class ListIterator does Iterator {
    has Mu         $.type      is required;
    has Str:D      $.mode      is required;
    has Str:D      $.operation is required;
    has Iterator:D $.fields    is required;
    has Iterator:D $.values    is required;
    has Int:D      $.arity     is required;
    has Int:D      $.count     = 0;

    submethod BUILD(::?CLASS:D: :$!type! is raw, :$!mode!, :$!operation!, :$fields!, :$values! --> Nil) {
        $!fields := (|$fields xx *).iterator;
        $!values := $values.iterator;
        $!arity  := $fields.elems;
    }

    method new(::?CLASS:_: $type is raw, $mode, $operation, $fields, $values --> ::?CLASS:D) {
        self.bless: :$type, :$mode, :$operation, :$fields, :$values
    }

    method pull-one(::?CLASS:D:) is raw {
        self."$!mode"($!fields.pull-one)
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }

    #|[ If any value in the given list cannot typecheck, an exception will be
        thrown; if the arity of the list does not match that of the record, it
        will be considered to have missing fields and thus an exception will be
        thrown. ]
    method wrap(::?CLASS:D: Mu $field is raw) is raw {
        LEAVE $!count++;
        self.map-one: $field, $!values.pull-one, :end<required>, :fail<now>
    }

    #|[ Any fields that do not typecheck will be stripped from the list, but if
        the arity of the list does not match that of the record, it will be
        considered to have missing fields and thus an exception will be thrown. ]
    method consume(::?CLASS:D: Mu $field is raw) is raw {
        LEAVE $!count++;
        loop { return-rw self.map-one: $field, $!values.pull-one, :end<required>, :fail<again> }
    }

    #|[ If any fields are missing from the list, they will be stubbed (if
        possible), but if any fields do not typecheck, then an exception will
        be thrown. Note that it's impossible for extraneous fields to exist in
        a list. ]
    method subsume(::?CLASS:D: Mu $field is raw) is raw {
        LEAVE $!count++;
        self.map-one: $field, $!values.pull-one, :end<optional>, :fail<now>
    }

    #|[ If any values in the given list cannot typecheck, they will be stripped
        from the list; if any fields are missing from the given list, they will
        be stubbed (if possible).  This should only throw if a definite field
        is missing. ]
    method coerce(::?CLASS:D: Mu $field is raw) is raw {
        LEAVE $!count++;
        loop { return-rw self.map-one: $field, $!values.pull-one, :end<optional>, :fail<again> }
    }

    method map-one(::?CLASS:D: Mu $field is raw, Mu $value is raw, :$end!, :$fail!) {
        $value =:= IterationEnd
          ?? self."end-$end"($field, $value)
          !! Metamodel::Primitives.is_type($value, Data::Record::Instance:U) || !$field.ACCEPTS($value)
            ?? self."fail-$fail"($field, $value)
            !! $value
    }

    method end-required(::?CLASS:D: Mu $field is raw, Mu $value is raw --> IterationEnd) {
        X::Data::Record::Missing.new(
            :$!operation, :$!type, :what<index>, :key($!count % $!arity), :$field
        ).throw unless $!count %% $!arity;
    }

    method end-optional(::?CLASS:D: Mu $field is raw, Mu $value is raw --> Mu) is raw {
        $!count %% $!arity
          ?? IterationEnd
          !! Metamodel::Primitives.is_type($field.HOW, Metamodel::DefiniteHOW) && $field.^definite
            ?? X::Data::Record::Definite.new(:$!type, :what<index>, :key($!count), :value($field)).throw
            !! $field
    }

    method fail-now(::?CLASS:D: Mu $field is raw, Mu $value is raw --> IterationEnd) {
        X::Data::Record::TypeCheck.new(:$!operation, :expected($field), :got($value)).throw
    }

    method fail-again(::?CLASS:D: Mu $field is raw, Mu $value is raw --> IterationEnd) {
        next
    }
}

#|[ Iterator for array ops taking lists of values (push/unshift/append/prepend).
    This is mostly identical to ListIterator, but adds
    behaviour to handle checking the arity of the list, which is handled in
    such a way as to support lazy lists. ]
my class ArrayIterator is ListIterator {
    method push-all(::?CLASS:D: \target --> IterationEnd) {
        my IterationBuffer:D \buffer .= new;
        loop {
            if (my $result := self.pull-one) =:= IterationEnd {
                last if $.count %% $.arity;
                return IterationEnd;
            } else {
                buffer.push: $result;
            }
        }
        target.append: buffer;
    }
}
#=[ Array ops can get passed lazy lists, though Array does not support
    this. We can't throw X::Cannot::Lazy ourselves; what if someone defines
    their own List subtype with methods that support them? Instead, we can
    check the arity whenever this iterator's values get pushed onto the
    relevant iterator of our record, so we have some way to check the
    list's arity without using the elems method. ]

role Data::Record::List does Data::Record::Instance[List] does Iterable does Positional {
    has @!record;

    submethod BUILD(::?ROLE:D: :@record! --> Nil) {
        @!record := @record;
    }

    multi method new(::?ROLE:_: List:D $original is raw --> ::?ROLE:D) {
        my @record := self.wrap: $original;
        @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
        self.bless: :@record
    }
    multi method new(::?ROLE:_: List:D $original is raw, Bool:D :consume($)! where ?* --> ::?ROLE:D) {
        my @record := self.consume: $original;
        @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
        self.bless: :@record
    }
    multi method new(::?ROLE:_: List:D $original is raw, Bool:D :subsume($)! where ?* --> ::?ROLE:D) {
        my @record := self.subsume: $original;
        @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
        self.bless: :@record
    }
    multi method new(::?ROLE:_: List:D $original is raw, Bool:D :coerce($)! where ?* --> ::?ROLE:D) {
        my @record := self.coerce: $original;
        @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
        self.bless: :@record
    }
    method wrap(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
        T.from-iterator: ListIterator.new: THIS, 'wrap', 'list reification', @.fields, $original
    }

    method consume(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
        T.from-iterator: ListIterator.new: THIS, 'consume', 'list reification', @.fields, $original
    }

    method subsume(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
        T.from-iterator: ListIterator.new: THIS, 'subsume', 'list reification', @.fields, $original
    }

    method coerce(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
        T.from-iterator: ListIterator.new: THIS, 'coerce', 'list reification', @.fields, $original
    }

    method fields(::?ROLE:_: --> List:D) { self.^fields }

    method record(::?ROLE:D: --> List:D) { @!record }

    method unrecord(::?ROLE:D: --> List:D) {
        @!record.WHAT.from-iterator: @!record.map(&unrecord).iterator
    }
    proto sub unrecord(Mu --> Mu) {*}
    multi sub unrecord(Data::Record::Instance:D \recorded --> Mu) {
        recorded.unrecord
    }
    multi sub unrecord(Mu \value --> Mu) is raw {
        value
    }

    multi method raku(::?CLASS:U: --> Str:D) {
        my Str:D $raku = '[@ ' ~ @.fields[0].raku ~ ' @]';
        my Str:D $name = self.^name;
        $raku ~= ":name('$name')" unless $name eq MetamodelX::RecordHOW::ANON_NAME;
        $raku
    }

    multi method ACCEPTS(::?CLASS:U: List:D $list is raw --> Bool:D) {
        my @fields := @.fields;
        for (|@fields xx *) Z $list -> (Mu $field is raw, Mu $value is raw) {
            state Int:D $count = 0;
            NEXT $count++;
            LAST return False unless $count %% +@fields;
            return False unless $value ~~ $field;
        }
        True
    }

    method EXISTS-POS(::?ROLE:D: Int:D $pos --> Bool:D) {
        @!record[$pos]:exists
    }

    method AT-POS(::?ROLE:D: Int:D $pos --> Mu) is raw {
        @!record[$pos]
    }

    method BIND-POS(::?ROLE:D: Int:D $pos, Mu $value is raw --> Mu) is raw {
        my @fields := @.fields;
        self!field-op: 'binding', {
            @!record[$pos] := $_
        }, @fields[$pos % +@fields], $value
    }

    method ASSIGN-POS(::?ROLE:D: Int:D $pos, Mu $value is raw --> Mu) is raw {
        my @fields := @.fields;
        self!field-op: 'assignment', {
            @!record[$pos] = $_
        }, @fields[$pos % +@fields], $value
    }

    method DELETE-POS(::?ROLE:D: Int:D $pos --> Mu) is raw {
        # XXX: This should be typechecking for the definiteness of the field
        # this position corresponds to and ensuring that, if this will leave
        # an empty space in the record, the field is not definite; however,
        # array slices complicate things.
        @!record[$pos]:delete
    }

    proto method push(|) {*}
    multi method push(::THIS ::?ROLE:D: Mu $value is raw --> ::?ROLE:D) {
        my @fields := @.fields;
        X::Data::Record::Missing.new(
            operation => 'push',
            type      => THIS,
            what      => 'index',
            key       => 1,
            field     => @fields[1],
        ).throw unless +@fields == 1;
        self!field-op: 'push', {
            @!record.push: $_;
            self
        }, @fields[0], $value;
    }
    multi method push(::THIS ::?ROLE:D: **@values --> ::?ROLE:D) {
        @!record.push: Slip.from-iterator: ArrayIterator.new: THIS, 'wrap', 'push', @.fields, @values;
        self
    }

    method pop(::THIS ::?ROLE:D: --> Mu) is raw {
        my @fields := @.fields;
        X::Data::Record::Missing.new(
            operation => 'pop',
            type      => THIS,
            what      => 'index',
            key       => (my Int:D $idx = +@fields - 1),
            field     => @fields[$idx],
        ).throw unless +@fields == 1;
        @!record.pop
    }

    method shift(::THIS ::?ROLE:D: --> Mu) is raw {
        my @fields := @.fields;
        X::Data::Record::Missing.new(
            operation => 'shift',
            type      => THIS,
            what      => 'index',
            key       => 0,
            field     => @fields[0],
        ).throw unless +@fields == 1;
        @!record.shift
    }

    proto method unshift(|) {*}
    multi method unshift(::THIS ::?ROLE:D: $value is raw --> ::?ROLE:D) {
        my @fields := @.fields;
        X::Data::Record::Missing.new(
            operation => 'unshift',
            type      => THIS,
            what      => 'index',
            key       => (my Int:D $idx = +@fields - 2),
            field     => @fields[$idx],
        ).throw unless +@fields == 1;
        self!field-op: 'unshift', {
           @!record.unshift: $_;
           self
        }, @fields[0], $value
    }
    multi method unshift(::THIS ::?ROLE:D: **@values --> ::?ROLE:D) {
        @!record.unshift: Slip.from-iterator: ArrayIterator.new: THIS, 'wrap', 'unshift', @.fields, @values;
        self
    }

    proto method prepend(|) {*}
    multi method prepend(::THIS ::?ROLE:D: Iterable:D $values is raw --> ::?ROLE:D) {
        @!record.prepend: Seq.new: ArrayIterator.new: THIS, 'wrap', 'prepend', @.fields, $values;
        self
    }
    multi method prepend(::THIS ::?ROLE:D: **@values --> ::?ROLE:D) {
        @!record.prepend: Slip.from-iterator: ArrayIterator.new: THIS, 'wrap', 'prepend', @.fields, @values;
        self
    }

    proto method append(|) {*}
    multi method append(::THIS ::?ROLE:D: Iterable:D $values is raw --> ::?ROLE:D) {
        @!record.append: Seq.new: ArrayIterator.new: THIS, 'wrap', 'append', @.fields, $values;
        self
    }
    multi method append(::THIS ::?ROLE:D: **@values --> ::?ROLE:D) {
        @!record.append: Slip.from-iterator: ArrayIterator.new:  THIS, 'wrap', 'append', @.fields, @values;
        self
    }

    method eager(::?ROLE:D: --> ::?ROLE:D) {
        @!record.is-lazy ?? self.new(@.record.eager) !! self
    }

    method lazy(::?ROLE:D: --> ::?ROLE:D) {
        @!record.is-lazy ?? self !! self.new(@.record.lazy)
    }

    method iterator(::?ROLE:D: --> Mu)  { @!record.iterator }
    method is-lazy(::?ROLE:D: --> Mu)   { @!record.is-lazy }
    method cache(::?ROLE:D: --> Mu)     { @!record.cache }
    method list(::?ROLE:D: --> Mu)      { self }
    method hash(::?ROLE:D: --> Mu)      { @!record.hash }
    method elems(::?ROLE:D: --> Mu)     { @!record.elems }
    method keys(::?ROLE:D: --> Mu)      { @!record.keys }
    method values(::?ROLE:D: --> Mu)    { @!record.values }
    method kv(::?ROLE:D: --> Mu)        { @!record.kv }
    method pairs(::?ROLE:D: --> Mu)     { @!record.pairs }
    method antipairs(::?ROLE:D: --> Mu) { @!record.antipairs }
}

multi sub circumfix:<[@ @]>(+values, Str:_ :$name --> Mu) is export {
    my Mu $record := MetamodelX::RecordHOW.new_type: :$name;
    $record.^set_language_version;
    $record.^set_delegate: Data::Record::List;
    $record.^set_fields: values;
    $record.^set_parameters;
    $record.^add_role: Data::Record::List;
    $record.^compose
}
multi sub circumfix:<[@ @]>(Block:D $block is raw, Str:_ :$name --> Mu) is export {
    MetamodelX::RecordTemplateHOW.new_type:
        Data::Record::List, $block, :$name
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
