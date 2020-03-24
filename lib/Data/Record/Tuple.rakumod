use v6.d;
use Data::Record::Instance;
use Data::Record::Exceptions;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
unit role Data::Record::Tuple
     does Data::Record::Instance[List]
     does Iterable
     does Positional;

has @!record is built(:bind);

multi method new(::?CLASS:_: List:D $original is raw) {
    my @record := self.wrap: $original;
    @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
    self.bless: :@record
}
multi method new(::?CLASS:_: List:D $original is raw, Bool:D :$consume! where ?*) {
    my @record := self.consume: $original;
    @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
    self.bless: :@record
}
multi method new(::?CLASS:_: List:D $original is raw, Bool:D :$subsume! where ?*) {
    my @record := self.subsume: $original;
    @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
    self.bless: :@record
}
multi method new(::?CLASS:_: List:D $original is raw, Bool:D :$coerce! where ?*) {
    my @record := self.coerce: $original;
    @record.elems unless @record.is-lazy; # Reify eager lists for eager typechecking.
    self.bless: :@record
}

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

    method new(::?CLASS:_: Mu $type is raw, List:D $fields is raw, List:D $record is raw) {
        self.bless: :$type, :$fields, :$record
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }
}

my class WrappedTupleIterator does TupleIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        return IterationEnd if $!done;

        LEAVE $!idx++;
        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $field =:= IterationEnd && $value =:= IterationEnd {
            $!done = True;
            IterationEnd
        } elsif $field =:= IterationEnd {
            die X::Data::Record::Extraneous.new:
                operation => 'tuple reification',
                type      => $!type,
                what      => 'index',
                key       => $!idx
        } elsif $value =:= IterationEnd {
            die X::Data::Record::Missing.new:
                operation => 'tuple reification',
                type      => $!type,
                what      => 'index',
                key       => $!idx
        } elsif $field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    $value ~~ $field
                        ?? $value
                        !! $field.new: $value.record
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => 'tuple reification',
                        expected  => $field,
                        got       => $value
                }
            } elsif $value ~~ $field.for {
                $field.new: $value
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => 'tuple reification',
                    expected  => $field,
                    got       => $value
            }
        } elsif $value ~~ $field {
            $value
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => 'tuple reification',
                expected  => $field,
                got       => $value
        }
    }
}

method wrap(::THIS ::?CLASS:_ \: ::T List:D $original is raw --> List:D) {
    T.from-iterator: WrappedTupleIterator.new: THIS, @.fields, $original
}

my class ConsumedTupleIterator does TupleIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        return IterationEnd if $!done;

        LEAVE $!idx++;
        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $field =:= IterationEnd {
            $!done = True;
            IterationEnd
        } elsif $value =:= IterationEnd {
            die X::Data::Record::Missing.new:
                operation => 'tuple reification',
                type      => $!type,
                what      => 'index',
                key       => $!idx
        } elsif $field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    $value ~~ $field
                        ?? $value
                        !! $field.new: $value.record, :consume
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => 'tuple reification',
                        expected  => $field,
                        got       => $value
                }
            } elsif $value ~~ $field.for {
                CATCH { default { return self.pull-one } }
                $field.new: $value, :consume
            } else {
                self.pull-one
            }
        } elsif $value ~~ $field {
            $value
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => 'tuple reification',
                expected  => $field,
                got       => $value
        }
    }
}

method consume(::THIS ::?CLASS:_ \: ::T List:D $original is raw --> List:D) {
    T.from-iterator: ConsumedTupleIterator.new: THIS, @.fields, $original
}

my class SubsumedTupleIterator does TupleIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        return IterationEnd if $!done;

        LEAVE $!idx++;
        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $value =:= IterationEnd {
            if $field =:= IterationEnd {
                $!done = True;
                IterationEnd
            } elsif $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite {
                die X::Data::Record::Definite.new:
                    type  => $!type,
                    what  => 'index',
                    key   => $!idx,
                    value => $field
            } else {
                $field
            }
        } elsif $field =:= IterationEnd {
            die X::Data::Record::Extraneous.new:
                operation => 'tuple reification',
                type      => $!type,
                what      => 'index',
                key       => $!idx
        } elsif $field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    $value ~~ $field
                        ?? $value
                        !! $field.new: $value.record, :subsume
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => 'tuple reification',
                        expected  => $field,
                        got       => $value
                }
            } elsif $value ~~ $field.for {
                $field.new: $value, :subsume
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => 'tuple reification',
                    expected  => $field,
                    got       => $value
            }
        } elsif $value ~~ $field {
            $value
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => 'tuple reification',
                expected  => $field,
                got       => $value
        }
    }
}

method subsume(::THIS ::?CLASS:_ \: ::T List:D $original is raw --> List:D) {
    T.from-iterator: SubsumedTupleIterator.new: THIS, @.fields, $original
}

my class CoercedTupleIterator does TupleIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        return IterationEnd if $!done;

        LEAVE $!idx++;
        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $field =:= IterationEnd {
            $!done = True;
            IterationEnd
        } elsif $value =:= IterationEnd {
            if $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite {
                die X::Data::Record::Definite.new:
                    type  => $!type,
                    what  => 'index',
                    key   => $!idx,
                    value => $field
            } else {
                $field
            }
        } elsif $field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    $value ~~ $field
                        ?? $value
                        !! $field.new: $value.record, :coerce
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => 'tuple reification',
                        expected  => $field,
                        got       => $value
                }
            } elsif $value ~~ $field.for {
                CATCH { default { return self.pull-one } }
                $field.new: $value, :coerce
            } else {
                self.pull-one
            }
        } elsif $value ~~ $field {
            $value
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => 'tuple reification',
                expected  => $field,
                got       => $value
        }
    }
}

method coerce(::THIS ::?CLASS:_ \: ::T List:D $original is raw --> List:D) {
    T.from-iterator: CoercedTupleIterator.new: THIS, @.fields, $original
}

method fields(::?CLASS:_: --> List:D) { self.^fields }

method record(::?CLASS:D: --> List:D) { @!record }

method unrecord(::?CLASS:D: --> List:D) {
    @!record.WHAT.from-iterator: @!record.map(&unrecord).iterator
}
proto sub unrecord(Mu --> Mu) {*}
multi sub unrecord(Data::Record::Instance:D \recorded --> Mu) {
    recorded.unrecord
}
multi sub unrecord(Mu \value --> Mu) is raw {
    value
}

multi method gist(::?CLASS:D: --> Str:D) {
    @!record.gist
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
multi method ACCEPTS(::?CLASS:D: |args --> Bool:D) {
    @!record.ACCEPTS: |args
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

method EXISTS-POS(::?CLASS:D: Int:D $pos --> Bool:D) {
    @!record[$pos]:exists
}

method AT-POS(::THIS ::?CLASS:D \: Int:D $pos --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'index',
        key  => $pos,
    ) unless @.fields[$pos]:exists;

    @!record[$pos]
}

method BIND-POS(::THIS ::?CLASS:D \: Int:D $pos, Mu $value is raw --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'index',
        key  => $pos,
    ) unless @.fields[$pos]:exists;

    self!field-op-for-value:
        @.fields[$pos], $value, { @!record[$pos] := $_ },
        :operation<binding>
}

method ASSIGN-POS(::THIS ::?CLASS:D \: Int:D $pos, Mu $value is raw --> Mu) is raw {
    die X::Data::Record::OutOfBounds.new(
        type => THIS,
        what => 'index',
        key  => $pos,
    ) unless @.fields[$pos]:exists;

    self!field-op-for-value:
        @.fields[$pos], $value, { @!record[$pos] = $_ },
        :operation<assignment>
}

method DELETE-POS(::THIS ::?CLASS:D: Int:D $pos --> Mu) is raw {
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
        operation => 'append',
        type      => THIS
}

method eager(::?CLASS:D: --> ::?CLASS:D) {
    @!record.is-lazy ?? self.new(@.record.eager) !! self
}

method lazy(::?CLASS:D: --> ::?CLASS:D) {
    @!record.is-lazy ?? self !! self.new(@.record.lazy)
}

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

multi sub infix:<eqv>(List:D $lhs is raw, Data::Record::Tuple:D $rhs is raw --> Bool:D) is export {
    $lhs eqv $rhs.unrecord
}
multi sub infix:<eqv>(Data::Record::Tuple:D $lhs is raw, List:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs
}
multi sub infix:<eqv>(Data::Record::Tuple:D $lhs is raw, Data::Record::Tuple:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs.unrecord
}
