use v6.e.PREVIEW;
use Data::Record::Exceptions;
only EXPORT(--> Map:D) { Map.new: '&infix:<eqv>' => ::{'&infix:<eqv>'} }
unit role Data::Record::Instance[::T];

#|[ Programmatic constructor. ]
proto method new(::?CLASS:_: +) {*}
#=[ Takes :wrap, :consume, :subsume, :coerce adverbs individually. ]
multi method new(::?CLASS:_ $self:
    $original is raw,
    *%adverbs where (.{<wrap consume subsume coerce>.any}:exists)
) is DEPRECATED<CALL-ME> {
    $self($original, |%adverbs)
}

#|[ Verbal constructor. ]
proto method CALL-ME(::?CLASS:_: $) {*}
#=[ Takes :wrap, :consume, :subsume, :coerce adverbs individually. ]
multi method CALL-ME(::?CLASS:_: ::?ROLE:U $other is raw) {
    $?CLASS
}
multi method CALL-ME(::?CLASS:_: ::?ROLE:D $other is raw) {
    samewith $other.record
}
multi method CALL-ME(::?CLASS:_: ::?CLASS:_ $self is raw) {
    $self
}

#|[ Wraps a data structure, typechecking it to ensure it matches this record
    type. This is done recursively for record type fields. ]
method wrap(::?ROLE:_: T --> T) { ... }
#=[ This will die with X::Data::Record::Missing if any fields are missing from
    the data structure, and X::Data::Record::Extraneous if any extraneous fields
    exist within it. ]

#|[ Consumes a data structure, stripping any extraneous fields so it matches
    this record type. This is done recursively for record type fields. ]
method consume(::?ROLE:_: T --> T) { ... }
#=[ This will die with X::Data::Record::Missing if any fields are missing from
    the data structure. ]

#|[ Subsumes a data structure, filling out any missing fields if possible so
    it matches this record type. This is done recursively for record type
    fields. ]
method subsume(::?ROLE:_: T --> T) { ... }
#=[ This will die with X::Data::Record::Extraneous if any extraneous fields
    exist within the data structure. ]

#|[ Coerces a data structure, both consuming any extraneous fields and filling
    out any missing fields if possible. This is done recursively for record
    type fields. ]
method coerce(::?ROLE:_: T --> T) { ... }

#|[ The data structure type that this record wraps. ]
method for(::?ROLE:_: --> Mu) { T }
#|[ The fields of this record. ]
method fields(::?ROLE:_: --> T) { ... }

#|[ Returns this record's wrapped data as is. ]
method record(::?ROLE:D: --> T)   { ... }
#|[ Returns this record's wrapped data, recursively unwrapping any records found within it. ]
method unrecord(::?ROLE:D: --> T) { ... }

multi method gist(::?CLASS:D: --> Str:D) { self.record.gist }

# multi method raku(::?CLASS:U: --> Str:D) { ... }
multi method raku(::?CLASS:D: --> Str:D) { self.^name ~ '.new(' ~ self.record.raku ~ ')' }

proto method ACCEPTS(Mu: Mu) {*}
multi method ACCEPTS(::?ROLE:U: T --> True) { }
multi method ACCEPTS(::?ROLE:_: Mu --> False) { }
multi method ACCEPTS(::?CLASS:_: ::?CLASS:_ \topic) { self.WHAT =:= topic.WHAT }

#|[ Seals this record instance with its fields as a subtype. ]
proto method beget(::?CLASS:U: |) {*}

#|[ Handles an operation on a field of the record given a callback accepting a
    value to perform the operation with. Typechecking and coercion of data
    structures to records is handled before passing the value to the callback. ]
method !field-op(
    ::?ROLE:_:
    Str:D  $operation,
           &op         is raw,
    Mu     $field      is raw,
    Mu     $value      is raw,
          *%named-args
    --> Mu
) is raw #`[is DEPRECATED] {
    if $field ~~ Data::Record::Instance {
        if $value ~~ Data::Record::Instance {
            if $value.DEFINITE {
                op $value ~~ $field
                ?? $value
                !! $field.new: $value.record, |%named-args
            } else {
                X::Data::Record::TypeCheck.new(
                    operation => $operation,
                    expected  => $field,
                    got       => $value,
                ).throw;
            }
        } elsif $value ~~ $field.for {
            op $field.new: $value, |%named-args
        } else {
            X::Data::Record::TypeCheck.new(
                operation => $operation,
                expected  => $field,
                got       => $value,
            ).throw;
        }
    } elsif $value ~~ $field {
        op $value
    } else {
        X::Data::Record::TypeCheck.new(
            operation => $operation,
            expected  => $field,
            got       => $value,
        ).throw;
    }
}
