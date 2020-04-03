use v6.d;
use Data::Record::Instance;
use Data::Record::Exceptions;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
unit role Data::Record::List
     does Data::Record::Instance[List]
     does Iterable
     does Positional;

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

# Iterator for lists that are to become records. Classes that do this role
# typecheck the list's values and coerce any of them that correspond to fields
# that are records in some manner.
my role ListIterator does Iterator {
    has Str:D $.operation is required;
    has Mu    $.type      is required;
    has Int:D $.arity     is required;
    has Int:D $.count     = 0;

    has Iterator:D $!fields     is required;
    has Iterator:D $!values     is required;
    has Mu         %!named-args is required;

    submethod BUILD(::?CLASS:D: Str:D :$!operation!, Mu :$type! is raw, :@fields!, Iterable:D :$values!, :%!named-args! --> Nil) {
        $!type   := $type;
        $!fields := (|@fields xx *).iterator;
        $!values := $values.iterator;
        $!arity  := @fields.elems;
    }

    method new(::?CLASS:_: Str:D $operation, $type is raw, @fields, Iterable:D $values, *%named-args --> ::?ROLE:D) {
        self.bless: :$operation, :$type, :@fields, :$values, :%named-args
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }
}

# This wraps a list with a record. If any value in the given list cannot
# typecheck, an exception will be thrown; if the arity of the list does not
# match that of the record, it will be considered to have missing fields and
# thus an exception will be thrown.
my class WrapListIterator does ListIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $value =:= IterationEnd {
            die X::Data::Record::Missing.new(
                operation => $!operation,
                type      => $!type,
                what      => 'index',
                key       => $!count % $!arity,
                field     => $field
            ) unless $!count %% $!arity;
            IterationEnd
        } else {
            $!count++;
            if $field ~~ Data::Record::Instance {
                if $value ~~ Data::Record::Instance {
                    if $value.DEFINITE {
                        $value ~~ $field
                            ?? $value
                            !! $field.new: $value.record, |%!named-args
                    } else {
                        die X::Data::Record::TypeCheck.new:
                            operation => $!operation,
                            expected  => $field,
                            got       => $value
                    }
                } elsif $value ~~ $field.for {
                    $field.new: $value, |%!named-args
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => $!operation,
                        expected  => $field,
                        got       => $value
                }
            } elsif $value ~~ $field {
                $value
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => $!operation,
                    expected  => $field,
                    got       => $value
            }
        }
    }
}

method wrap(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: WrapListIterator.new: 'list reification', THIS, @.fields, $original
}

# This consumes a list with a record. Any fields that do not typecheck will be
# stripped from the list, but if the arity of the list does not match that of
# the record, it will be considered to have missing fields and thus an
# exception will be thrown.
my class ConsumeListIterator does ListIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        my Mu     $field     := $!fields.pull-one;
        my Mu     $value;
        my Bool:D $is-record  = $field ~~ Data::Record::Instance;
        my Bool:D $ended      = False;
        my Bool:D $matches    = False;
        repeat {
           $value   := $!values.pull-one;
           $ended    = $value =:= IterationEnd;
           $matches  = !$ended && !$is-record && $value ~~ $field;
        } until $ended || $matches;
        if $ended {
            die X::Data::Record::Missing.new(
                operation => $!operation,
                type      => $!type,
                what      => 'index',
                key       => $!count % $!arity,
                field     => $field,
            ) unless $!count %% $!arity;
            IterationEnd
        } elsif $is-record {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    if $value ~~ $field {
                        $!count++;
                        $value
                    } else {
                        CATCH { default { return self.pull-one } }
                        KEEP  $!count++;
                        $field.new: $value.record, |%!named-args
                    }
                } else {
                    self.pull-one
                }
            } elsif $value ~~ $field.for {
                CATCH { default { return self.pull-one } }
                KEEP  $!count++;
                $field.new: $value, |%!named-args
            } else {
                self.pull-one
            }
        } elsif $matches {
            $!count++;
            $value
        } else {
            self.pull-one
        }
    }
}

method consume(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: ConsumeListIterator.new: 'list reification', THIS, @.fields, $original, :consume
}

# This subsumes a list with a record. If any fields are missing from the list,
# they will be stubbed (if possible), but if any fields do not typecheck, then
# an exception will be thrown. Note that it's impossible for extraneous fields
# to exist in a list.
my class SubsumeListIterator does ListIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        my Mu $field := $!fields.pull-one;
        my Mu $value := $!values.pull-one;
        if $value =:= IterationEnd {
            if $!count %% $!arity {
                IterationEnd
            } else {
                $!count++;
                if $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite {
                    die X::Data::Record::Definite.new:
                        type  => $!type,
                        what  => 'index',
                        key   => $!count,
                        value => $field
                } else {
                    $field
                }
            }
        } else {
            $!count++;
            if $field ~~ Data::Record::Instance {
                if $value ~~ Data::Record::Instance {
                    if $value.DEFINITE {
                        $value ~~ $field
                            ?? $value
                            !! $field.new: $value.record, |%!named-args
                    } else {
                        die X::Data::Record::TypeCheck.new:
                            operation => $!operation,
                            expected  => $field,
                            got       => $value
                    }
                } elsif $value ~~ $field.for {
                    $field.new: $value, |%!named-args
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => $!operation,
                        expected  => $field,
                        got       => $value
                }
            } elsif $value ~~ $field {
                $value
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => $!operation,
                    expected  => $field,
                    got       => $value
            }
        }
    }
}

method subsume(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: SubsumeListIterator.new: 'list reification', THIS, @.fields, $original, :subsume
}

# This coerces a list to a record. If any values in the given list cannot
# typecheck, they will be stripped from the list; if any fields are missing
# from the given list, they will be stubbed (if possible). This should only
# throw if a definite field is missing.
my class CoerceListIterator does ListIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        my Mu     $field     := $!fields.pull-one;
        my Bool:D $is-record  = $field ~~ Data::Record::Instance;
        my Mu     $value;
        my Bool:D $ended      = False;
        my Bool:D $matches    = False;
        repeat {
           $value   := $!values.pull-one;
           $ended    = $value =:= IterationEnd;
           $matches  = !$ended && !$is-record && $value ~~ $field;
        } until $ended || $matches;
        if $ended {
            if $!count %% $!arity {
                IterationEnd
            } else {
                $!count++;
                if $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite {
                    die X::Data::Record::Definite.new:
                        type  => $!type,
                        what  => 'index',
                        key   => $!count,
                        value => $field
                } else {
                    $field
                }
            }
        } elsif $is-record {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    if $value ~~ $field {
                        $!count++;
                        $value
                    } else {
                        CATCH { default { return self.pull-one } }
                        KEEP  $!count++;
                        $field.new: $value.record, |%!named-args
                    }
                } else {
                    self.pull-one
                }
            } elsif $value ~~ $field.for {
                CATCH { default { return self.pull-one } }
                KEEP  $!count++;
                $field.new: $value, |%!named-args
            } else {
                self.pull-one
            }
        } elsif $matches {
            $!count++;
            $value
        } else {
            self.pull-one
        }
    }
}

method coerce(::THIS ::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: CoerceListIterator.new: 'list reification', THIS, @.fields, $original, :coerce
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

# Iterator for array ops taking lists of values (push/unshift/append/prepend).
# This is mostly identical to WrapListIterator, but adds behaviour to handle
# checking the arity of the list, which is handled in such a way as to support
# lazy lists.
#
# Array ops can get passed lazy lists, though Array does not support this. We
# can't throw X::Cannot::Lazy ourselves; what if someone defines their own List
# subtype with methods that support them? Instead, we can check the arity
# whenever this iterator's values get pushed onto the relevant iterator of our
# record, so we have some way to check the list's arity without using the elems
# method.
my class ArrayIterator is WrapListIterator {
    method push-all(::?CLASS:D: \target --> IterationEnd) {
        my IterationBuffer:D \buffer .= new;
        loop {
            if (my Mu $result := self.pull-one) =:= IterationEnd {
                my Int:D $count = $.count;
                my Int:D $arity = $.arity;
                last if $count %% $arity;
                return IterationEnd;
            } else {
                buffer.push: $result;
            }
        }
        target.append: buffer;
    }
}

proto method push(|) {*}
multi method push(::THIS ::?ROLE:D: Mu $value is raw --> ::?ROLE:D) {
    my @fields := @.fields;
    if +@fields == 1 {
        self!field-op: 'push', {
            @!record.push: $_;
            self
        }, @fields[0], $value;
    } else {
        die X::Data::Record::Missing.new:
            operation => 'push',
            type      => THIS,
            what      => 'index',
            key       => 1,
            field     => @fields[1];
        self
    }
}
multi method push(::THIS ::?ROLE:D: **@values --> ::?ROLE:D) {
    @!record.push: Slip.from-iterator: ArrayIterator.new: 'push', THIS, @.fields, @values;
    self
}

method pop(::THIS ::?ROLE:D: --> Mu) is raw {
    my @fields := @.fields;
    if +@fields == 1 {
        @!record.pop
    } else {
        my Int:D $idx = +@fields - 1;
        die X::Data::Record::Missing.new:
            operation => 'pop',
            type      => THIS,
            what      => 'index',
            key       => $idx,
            field     => @fields[$idx]
    }
}

method shift(::THIS ::?ROLE:D: --> Mu) is raw {
    my @fields := @.fields;
    if +@fields == 1 {
        @!record.shift
    } else {
        die X::Data::Record::Missing.new:
            operation => 'shift',
            type      => THIS,
            what      => 'index',
            key       => 0,
            field     => @fields[0]
    }
}

proto method unshift(|) {*}
multi method unshift(::THIS ::?ROLE:D: $value is raw --> ::?ROLE:D) {
    my @fields := @.fields;
    if +@fields == 1 {
        self!field-op: 'unshift', {
           @!record.unshift: $_;
           self
        }, @fields[0], $value
    } else {
        my Int:D $idx = +@fields - 2;
        die X::Data::Record::Missing.new:
            operation => 'unshift',
            type      => THIS,
            what      => 'index',
            key       => $idx,
            field     => @fields[$idx];
        self
    }
}
multi method unshift(::THIS ::?ROLE:D: **@values --> ::?ROLE:D) {
    @!record.unshift: Slip.from-iterator: ArrayIterator.new: 'unshift', THIS, @.fields, @values;
    self
}

proto method prepend(|) {*}
multi method prepend(::THIS ::?ROLE:D: Iterable:D $values is raw --> ::?ROLE:D) {
    @!record.prepend: Seq.new: ArrayIterator.new: 'prepend', THIS, @.fields, $values;
    self
}
multi method prepend(::THIS ::?ROLE:D: **@values --> ::?ROLE:D) {
    @!record.prepend: Slip.from-iterator: ArrayIterator.new: 'prepend', THIS, @.fields, @values;
    self
}

proto method append(|) {*}
multi method append(::THIS ::?ROLE:D: Iterable:D $values is raw --> ::?ROLE:D) {
    @!record.append: Seq.new: ArrayIterator.new: 'append', THIS, @.fields, $values;
    self
}
multi method append(::THIS ::?ROLE:D: **@values --> ::?ROLE:D) {
    @!record.append: Slip.from-iterator: ArrayIterator.new: 'append', THIS, @.fields, @values;
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
