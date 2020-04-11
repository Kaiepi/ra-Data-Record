use v6.d;
use Data::Record::Instance;
use Data::Record::Exceptions;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
unit role Data::Record::Tuple
     does Data::Record::Instance[List]
     does Iterable
     does Positional;

has @!record;

submethod BUILD(::?ROLE:D: :@record --> Nil) {
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

# Iterator for tuples (lists of fixed length) that are to become records.
# Classes that do this role typecheck the list's values and coerce any of them
# that correspond to fields that are records in some manner.
my role TupleIterator does Iterator {
    has Mu         $!type   is required;
    has Iterator:D $!fields is required;
    has Iterator:D $!values is required;
    has Int:D      $!idx    = 0;
    has Bool:D     $!done   = False;

    submethod BUILD(::?CLASS:D: Mu :$type is raw, List:D :$fields is raw, List:D :$record is raw --> Nil) {
        $!type   := $type;
        $!fields := $fields.iterator;
        $!values := $record.iterator;
    }

    method new(::?CLASS:_: Mu $type is raw, List:D $fields is raw, List:D $record is raw --> ::?ROLE:D) {
        self.bless: :$type, :$fields, :$record
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }
}

# Wraps a list. The list must have an arity equal to the tuple type's and all
# values must typecheck as their corresponding fields, otherwise an exception
# will be thrown.
my class WrappedTupleIterator does TupleIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        return IterationEnd if $!done;

        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $field =:= IterationEnd && $value =:= IterationEnd {
            $!done = True;
            return IterationEnd;
        }
        if $field =:= IterationEnd {
            X::Data::Record::Extraneous.new(
                operation => 'tuple reification',
                type      => $!type,
                what      => 'index',
                key       => $!idx,
                value     => $value,
            ).throw;
        }
        if $value =:= IterationEnd {
            X::Data::Record::Missing.new(
                operation => 'tuple reification',
                type      => $!type,
                what      => 'index',
                key       => $!idx,
                field     => $field,
            ).throw;
        }
        KEEP $!idx++;
        if $field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                X::Data::Record::TypeCheck.new(
                    operation => 'tuple reification',
                    expected  => $field,
                    got       => $value,
                ).throw without $value;
                $value ~~ $field
                    ?? $value
                    !! $field.new: $value.record
            } elsif $value ~~ $field.for {
                $field.new: $value
            } else {
                X::Data::Record::TypeCheck.new(
                    operation => 'tuple reification',
                    expected  => $field,
                    got       => $value,
                ).throw;
            }
        } elsif $value ~~ $field {
            $value
        } else {
            X::Data::Record::TypeCheck.new(
                operation => 'tuple reification',
                expected  => $field,
                got       => $value,
            ).throw;
        }
    }
}

method wrap(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: WrappedTupleIterator.new: THIS, @.fields, $original
}

# Consumes a list. The list must have an arity greater than or equal to the
# tuple type; if it's greater, extraneous values will be stripped. If any
# values are missing or values corresponding to fields don't typecheck, an
# exception will be thrown.
my class ConsumedTupleIterator does TupleIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        return IterationEnd if $!done;

        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $field =:= IterationEnd {
            $!done = True;
            return IterationEnd;
        }
        if $value =:= IterationEnd {
            X::Data::Record::Missing.new(
                operation => 'tuple reification',
                type      => $!type,
                what      => 'index',
                key       => $!idx,
                field     => $field,
            ).throw;
        }
        KEEP $!idx++;
        if $field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                X::Data::Record::TypeCheck.new(
                    operation => 'tuple reification',
                    expected  => $field,
                    got       => $value,
                ).throw without $value;
                CATCH { default { return self.pull-one } }
                $value ~~ $field
                    ?? $value
                    !! $field.new: $value.record, :consume
            } elsif $value ~~ $field.for {
                CATCH { default { return self.pull-one } }
                $field.new: $value, :consume
            } else {
                self.pull-one
            }
        } elsif $value ~~ $field {
            $value
        } else {
            X::Data::Record::TypeCheck.new(
                operation => 'tuple reification',
                expected  => $field,
                got       => $value,
            ).throw;
        }
    }
}

method consume(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: ConsumedTupleIterator.new: THIS, @.fields, $original
}

# Subsumes a list. The list must have an arity lesser than or equal to the
# tuple type's; if it's lesser, missing values will be stubbed (if possible).
# If any values don't typecheck as their corresponding fields, an exception
# will be thrown.
my class SubsumedTupleIterator does TupleIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        return IterationEnd if $!done;

        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $field =:= IterationEnd && $value =:= IterationEnd {
            $!done = True;
            return IterationEnd;
        }
        if $field =:= IterationEnd {
            X::Data::Record::Extraneous.new(
                operation => 'tuple reification',
                type      => $!type,
                what      => 'index',
                key       => $!idx,
                value     => $value,
            ).throw;
        }
        KEEP $!idx++;
        if $value =:= IterationEnd {
            X::Data::Record::Definite.new(
                type  => $!type,
                what  => 'index',
                key   => $!idx,
                value => $field,
            ).throw if $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite;
            return $field;
        }
        if $field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                X::Data::Record::TypeCheck.new(
                    operation => 'tuple reification',
                    expected  => $field,
                    got       => $value,
                ).throw without $value;
                $value ~~ $field
                    ?? $value
                    !! $field.new: $value.record, :subsume
            } elsif $value ~~ $field.for {
                $field.new: $value, :subsume
            } else {
                X::Data::Record::TypeCheck.new(
                    operation => 'tuple reification',
                    expected  => $field,
                    got       => $value,
                ).throw;
            }
        } elsif $value ~~ $field {
            $value
        } else {
            X::Data::Record::TypeCheck.new(
                operation => 'tuple reification',
                expected  => $field,
                got       => $value,
            ).throw;
        }
    }
}

method subsume(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: SubsumedTupleIterator.new: THIS, @.fields, $original
}

# Coerces a list. Arity does not matter; missing values are stubbed (if
# possible) and extraneous values are stripped. If any values don't typecheck
# as their corresponding fields, an exception will be thrown.
my class CoercedTupleIterator does TupleIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        return IterationEnd if $!done;

        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $field =:= IterationEnd {
            $!done = True;
            return IterationEnd;
        }
        KEEP $!idx++;
        if $value =:= IterationEnd {
            X::Data::Record::Definite.new(
                type  => $!type,
                what  => 'index',
                key   => $!idx,
                value => $field,
            ).throw if $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite;
            return $field;
        }
        if $field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                X::Data::Record::TypeCheck.new(
                    operation => 'tuple reification',
                    expected  => $field,
                    got       => $value,
                ).throw without $value;
                CATCH { default { return self.pull-one } }
                $value ~~ $field
                    ?? $value
                    !! $field.new: $value.record, :coerce
            } elsif $value ~~ $field.for {
                CATCH { default { return self.pull-one } }
                $field.new: $value, :coerce
            } else {
                self.pull-one
            }
        } elsif $value ~~ $field {
            $value
        } else {
            X::Data::Record::TypeCheck.new(
                operation => 'tuple reification',
                expected  => $field,
                got       => $value,
            ).throw;
        }
    }
}

method coerce(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: CoercedTupleIterator.new: THIS, @.fields, $original
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

method EXISTS-POS(::?ROLE:D: Int:D $pos --> Bool:D) {
    @!record[$pos]:exists
}

method AT-POS(::THIS ::?ROLE:D: Int:D $pos --> Mu) is raw {
    if @.fields[$pos]:!exists {
        die X::Data::Record::OutOfBounds.new:
            type => THIS,
            what => 'index',
            key  => $pos
    } else {
        @!record[$pos]
    }
}

method BIND-POS(::THIS ::?ROLE:D: Int:D $pos, Mu $value is raw --> Mu) is raw {
    my @fields := @.fields;
    if @fields[$pos]:!exists {
        die X::Data::Record::OutOfBounds.new:
            type => THIS,
            what => 'index',
            key  => $pos
    } else {
        self!field-op: 'binding', {
            @!record[$pos] := $_
        }, @fields[$pos], $value
    }
}

method ASSIGN-POS(::THIS ::?ROLE:D: Int:D $pos, Mu $value is raw --> Mu) is raw {
    my @fields := @.fields;
    if @fields[$pos]:!exists {
        die X::Data::Record::OutOfBounds.new:
            type => THIS,
            what => 'index',
            key  => $pos
    } else {
        self!field-op: 'assignment', {
           @!record[$pos] = $_
        }, @fields[$pos], $value
    }
}

method DELETE-POS(::THIS ::?ROLE:D: Int:D $pos --> Mu) is raw {
    die X::Data::Record::Immutable.new:
        operation => 'deletion',
        type      => THIS
}

method push(::THIS ::?ROLE:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'push',
        type      => THIS
}

method pop(::THIS ::?ROLE:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'pop',
        type      => THIS
}

method shift(::THIS ::?ROLE:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'shift',
        type      => THIS
}

method unshift(::THIS ::?ROLE:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'unshift',
        type      => THIS
}

method append(::THIS ::?ROLE:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'append',
        type      => THIS
}

method prepend(::THIS ::?ROLE:D: | --> Mu) {
    die X::Data::Record::Immutable.new:
        operation => 'append',
        type      => THIS
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
method elems(::?ROLE:D: --> Mu)     { @!record.elems }
method hash(::?ROLE:D: --> Mu)      { @!record.hash }
method keys(::?ROLE:D: --> Mu)      { @!record.keys }
method values(::?ROLE:D: --> Mu)    { @!record.values }
method kv(::?ROLE:D: --> Mu)        { @!record.kv }
method pairs(::?ROLE:D: --> Mu)     { @!record.pairs }
method antipairs(::?ROLE:D: --> Mu) { @!record.antipairs }

multi sub circumfix:«<@ @>»(+values, Str:_ :$name --> Mu) is export {
    my Mu $record = MetamodelX::RecordHOW.new_type: :$name;
    $record.^set_language_version;
    $record.^set_delegate: Data::Record::Tuple;
    $record.^set_fields: values;
    $record.^set_parameters;
    $record.^add_role: Data::Record::Tuple;
    $record.^compose
}
multi sub circumfix:«<@ @>»(Block:D $block is raw, Str:_ :$name --> Mu) is export {
    MetamodelX::RecordTemplateHOW.new_type:
        Data::Record::Tuple, $block, :$name
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
