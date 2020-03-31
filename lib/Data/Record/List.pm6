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

my role ListIterator does Iterator {
    has Mu         $!field       is required;
    has Iterator:D $!values      is required;
    has Mu         %!named-args;
    has Int:D      $!idx         = 0;

    submethod BUILD(::?CLASS:D: Mu :$field! is raw, List:D :$record! is raw, :%!named-args! --> Nil) {
        $!field  := $field;
        $!values := $record.iterator;
    }

    method new(::?CLASS:_: Mu $field is raw, List:D $record is raw, *%named-args --> ::?ROLE:D) {
        self.bless: :$field, :$record, :%named-args
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }
}

my class StrictListIterator does ListIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        my Mu $value := $!values.pull-one;
        if $value =:= IterationEnd {
            IterationEnd
        } elsif $!field ~~ Data::Record::Instance {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    $value ~~ $!field
                        ?? $value
                        !! $!field.new: $value.record, |%!named-args
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => 'list reification',
                        expected  => $!field,
                        got       => $value;
                }
            } elsif $value ~~ $!field.for {
                $!field.new: $value, |%!named-args
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => 'list reification',
                    expected  => $!field,
                    got       => $value
            }
        } elsif $value ~~ $!field {
            $value
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => 'list reification',
                expected  => $!field,
                got       => $value;
        }
    }
}

my class LaxListIterator does ListIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        my Mu     $value;
        my Bool:D $is-record = $!field ~~ Data::Record::Instance;
        my Bool:D $ended     = False;
        my Bool:D $matches   = False;
        repeat {
           $value   := $!values.pull-one;
           $ended    = $value =:= IterationEnd;
           $matches  = !$ended && !$is-record && $value ~~ $!field;
        } until $ended || $matches;
        if $ended {
            IterationEnd
        } elsif $is-record {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    $value ~~ $!field
                        ?? $value
                        !! $!field.new: $value.record, |%!named-args
                } else {
                    self.pull-one
                }
            } elsif $value ~~ $!field.for {
                CATCH { default { return self.pull-one } }
                $!field.new: $value, |%!named-args
            } else {
                self.pull-one
            }
        } elsif $matches {
            $value
        } else {
            self.pull-one
        }
    }
}

method wrap(::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: StrictListIterator.new: @.fields[0], $original
}

method consume(::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: LaxListIterator.new: @.fields[0], $original, :consume
}

method subsume(::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: StrictListIterator.new: @.fields[0], $original, :subsume
}

method coerce(::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: LaxListIterator.new: @.fields[0], $original, :coerce
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
    so $list.all ~~ @.fields[0]
}
multi method ACCEPTS(::?CLASS:D: |args --> Bool:D) {
    @!record.ACCEPTS: |args
}

method !op-for-value(::?ROLE:D: Mu $value is raw, &op, Str:D :$operation! --> Mu) is raw {
    my Mu $field := @.fields[0];
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

method !op-for-values(::?ROLE:D: Iterable:D $values is raw, &op, Str:D :$operation! --> Mu) is raw {
    op gather {
        my Mu $field := @.fields[0];
        if $field ~~ Data::Record::Instance {
            for $values -> Mu $value is raw {
                if $value ~~ Data::Record::Instance {
                    if $value.DEFINITE {
                        take-rw $value ~~ $field
                             ?? $value
                             !! $field.new: $value.record;
                    } else {
                        die X::Data::Record::TypeCheck.new:
                            operation => $operation,
                            expected  => $field,
                            got       => $value;
                    }
                } elsif $value ~~ $field.for {
                    take-rw $field.new: $value;
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => $operation,
                        expected  => $field,
                        got       => $value;
                }
            }
        } else {
            for $values -> Mu $value is raw {
                if $value ~~ $field {
                    take-rw $value;
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => $operation,
                        expected  => $field,
                        got       => $value
                }
            }
        }
    }
}

method EXISTS-POS(::?ROLE:D: Int:D $pos --> Bool:D) {
    @!record[$pos]:exists
}

method AT-POS(::?ROLE:D: Int:D $pos --> Mu) is raw {
    @!record[$pos]
}

method BIND-POS(::?ROLE:D: Int:D $pos, Mu $value is raw --> Mu) is raw {
    self!op-for-value: $value, { @!record[$pos] := $_ }, :operation<binding>
}

method ASSIGN-POS(::?ROLE:D: Int:D $pos, Mu $value is raw --> Mu) is raw {
    self!op-for-value: $value, { @!record[$pos] = $_ }, :operation<assignment>
}

method DELETE-POS(::?ROLE:D: Int:D $pos --> Mu) is raw is default {
    @!record[$pos]:delete
}

proto method push(|) {*}
multi method push(::?ROLE:D: Mu $value is raw --> ::?ROLE:D) {
    self!op-for-value: $value, { @!record.push: $_ }, :operation<push>;
    self
}
multi method push(::?ROLE:D: **@values --> ::?ROLE:D) {
    self!op-for-values: @values, { @!record.push: |$_ }, :operation<push>;
    self
}

method pop(::?ROLE:D: --> Mu) is raw {
    @!record.pop
}

method shift(::?ROLE:D: --> Mu) is raw {
    @!record.shift
}

proto method unshift(|) {*}
multi method unshift(::?ROLE:D: $value is raw --> ::?ROLE:D) {
    self!op-for-value: $value, { @!record.unshift: $_ }, :operation<unshift>;
    self
}
multi method unshift(::?ROLE:D: **@values --> ::?ROLE:D) {
    self!op-for-values: @values, { @!record.unshift: |$_ }, :operation<unshift>;
    self
}

proto method prepend(|) {*}
multi method prepend(::?ROLE:D: Iterable:D $values is raw --> ::?ROLE:D) {
    self!op-for-values: $values, { @!record.prepend: $_ }, :operation<prepend>;
    self
}
multi method prepend(::?ROLE:D: **@values --> ::?ROLE:D) {
    self!op-for-values: @values, { @!record.prepend: |$_ }, :operation<prepend>;
    self
}

proto method append(|) {*}
multi method append(::?ROLE:D: Iterable:D $values is raw --> ::?ROLE:D) {
    self!op-for-values: $values, { @!record.append: $_ }, :operation<append>;
    self
}
multi method append(::?ROLE:D: **@values --> ::?ROLE:D) {
    self!op-for-values: @values, { @!record.append: |$_ }, :operation<append>;
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
    die 'Multi-parameter record lists NYI' if +values > 1;
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
