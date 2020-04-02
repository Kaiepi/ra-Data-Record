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
    has Str:D      $!operation  is required;
    has Mu         $!field      is required;
    has Bool:D     $!is-record  is required;
    has Iterator:D $!values     is required;
    has Mu         %!named-args is required;

    submethod BUILD(::?CLASS:D: Str:D :$!operation, Mu :$field! is raw, Iterable:D :$values!, :%!named-args! --> Nil) {
        $!field     := $field;
        $!is-record  = $field ~~ Data::Record::Instance;
        $!values    := $values.iterator;
    }

    method new(::?CLASS:_: Str:D $operation, Mu $field is raw, Iterable:D $values, *%named-args --> ::?ROLE:D) {
        self.bless: :$operation, :$field, :$values, :%named-args
    }

    method is-lazy(::?CLASS:D: --> Bool:D) {
        $!values.is-lazy
    }
}

# This coerces lists to records with strict typechecking. If a value in the
# given list cannot typecheck, an exception will be thrown.
my class StrictListIterator does ListIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        my Mu $value := $!values.pull-one;
        if $value =:= IterationEnd {
            IterationEnd
        } elsif $!is-record {
            if $value ~~ Data::Record::Instance {
                if $value.DEFINITE {
                    $value ~~ $!field
                        ?? $value
                        !! $!field.new: $value.record, |%!named-args
                } else {
                    die X::Data::Record::TypeCheck.new:
                        operation => $!operation,
                        expected  => $!field,
                        got       => $value;
                }
            } elsif $value ~~ $!field.for {
                $!field.new: $value, |%!named-args
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => $!operation,
                    expected  => $!field,
                    got       => $value
            }
        } elsif $value ~~ $!field {
            $value
        } else {
            die X::Data::Record::TypeCheck.new:
                operation => $!operation,
                expected  => $!field,
                got       => $value;
        }
    }
}

# This coerces lists to records with lax typechecking. If a value in the given
# list cannot typecheck, it will be stripped from the list.
my class LaxListIterator does ListIterator {
    method pull-one(::?CLASS:D: --> Mu) is raw {
        my Mu     $value;
        my Bool:D $ended   = False;
        my Bool:D $matches = False;
        repeat {
           $value   := $!values.pull-one;
           $ended    = $value =:= IterationEnd;
           $matches  = !$ended && !$!is-record && $value ~~ $!field;
        } until $ended || $matches;
        if $ended {
            IterationEnd
        } elsif $!is-record {
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
    T.from-iterator: StrictListIterator.new: 'list reification', @.fields[0], $original
}

method consume(::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: LaxListIterator.new: 'list reification', @.fields[0], $original, :consume
}

method subsume(::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: StrictListIterator.new: 'list reification', @.fields[0], $original, :subsume
}

method coerce(::?ROLE:_: ::T List:D $original is raw --> List:D) {
    T.from-iterator: LaxListIterator.new: 'list reification', @.fields[0], $original, :coerce
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

method EXISTS-POS(::?ROLE:D: Int:D $pos --> Bool:D) {
    @!record[$pos]:exists
}

method AT-POS(::?ROLE:D: Int:D $pos --> Mu) is raw {
    @!record[$pos]
}

method BIND-POS(::?ROLE:D: Int:D $pos, Mu $value is raw --> Mu) is raw {
    self!field-op: 'binding', {
        @!record[$pos] := $_
    }, @.fields[0], $value
}

method ASSIGN-POS(::?ROLE:D: Int:D $pos, Mu $value is raw --> Mu) is raw {
    self!field-op: 'assignment', {
        @!record[$pos] = $_
    }, @.fields[0], $value
}

method DELETE-POS(::?ROLE:D: Int:D $pos --> Mu) is raw is default {
    @!record[$pos]:delete
}

proto method push(|) {*}
multi method push(::?ROLE:D: Mu $value is raw --> ::?ROLE:D) {
    self!field-op: 'push', {
        @!record.push: $_;
        self
    }, @.fields[0], $value;
}
multi method push(::?ROLE:D: **@values --> ::?ROLE:D) {
    @!record.push: Slip.from-iterator: StrictListIterator.new: 'push', @.fields[0], @values;
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
    self!field-op: 'unshift', {
       @!record.unshift: $_;
       self
    }, @.fields[0], $value
}
multi method unshift(::?ROLE:D: **@values --> ::?ROLE:D) {
    @!record.unshift: Slip.from-iterator: StrictListIterator.new: 'unshift', @.fields[0], @values;
    self
}

proto method prepend(|) {*}
multi method prepend(::?ROLE:D: Iterable:D $values is raw --> ::?ROLE:D) {
    @!record.prepend: Seq.new: StrictListIterator.new: 'prepend', @.fields[0], $values;
    self
}
multi method prepend(::?ROLE:D: **@values --> ::?ROLE:D) {
    @!record.prepend: Slip.from-iterator: StrictListIterator.new: 'prepend', @.fields[0], @values;
    self
}

proto method append(|) {*}
multi method append(::?ROLE:D: Iterable:D $values is raw --> ::?ROLE:D) {
    @!record.append: Seq.new: StrictListIterator.new: 'append', @.fields[0], $values;
    self
}
multi method append(::?ROLE:D: **@values --> ::?ROLE:D) {
    @!record.append: Slip.from-iterator: StrictListIterator.new: 'append', @.fields[0], @values;
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
