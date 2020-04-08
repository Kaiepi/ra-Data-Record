use v6.d;
use Data::Record::Exceptions;
unit role Data::Record::Instance[::T];

proto method new(::?ROLE:_: |)                               {*}
# multi method new(::?ROLE:_: T:D)                             { ... }
# multi method new(::?ROLE:_: T:D, Bool:D :$consume! where ?*) { ... }
# multi method new(::?ROLE:_: T:D, Bool:D :$subsume! where ?*) { ... }
# multi method new(::?ROLE:_: T:D, Bool:D :$coerce! where ?*)  { ... }

#|[ Wraps a data structure, typechecking it to ensure it matches this record
    type. This is done recursively for record type fields. ]
method wrap(::?ROLE:_: T:D --> T:D) { ... }
#=[ This will die with X::Data::Record::Missing if any fields are missing from
    the data structure, and X::Data::Record::Extraneous if any extraneous fields
    exist within it. ]

#|[ Consumes a data structure, stripping any extraneous fields so it matches
    this record type. This is done recursively for record type fields. ]
method consume(::?ROLE:_: T:D --> T:D) { ... }
#=[ This will die with X::Data::Record::Missing if any fields are missing from
    the data structure. ]

#|[ Subsumes a data structure, filling out any missing fields if possible so
    it matches this record type. This is done recursively for record type
    fields. ]
method subsume(::?ROLE:_: T:D --> T:D) { ... }
#=[ This will die with X::Data::Record::Extraneous if any extraneous fields
    exist within the data structure. ]

#|[ Coerces a data structure, both consuming any extraneous fields and filling
    out any missing fields if possible. This is done recursively for record
    type fields. ]
method coerce(::?ROLE:_: T:D --> T:D) { ... }

#|[ The data structure type that this record wraps. ]
method for(::?ROLE:_: --> Mu) { T }
#|[ The fields of this record. ]
method fields(::?ROLE:_: --> T:D) { ... }

#|[ Returns this record's wrapped data as is. ]
method record(::?ROLE:D: --> T:D)   { ... }
#|[ Returns this record's wrapped data, recursively unwrapping any records found within it. ]
method unrecord(::?ROLE:D: --> T:D) { ... }

multi method gist(::?CLASS:D: --> Str:D) { self.record.gist }

# multi method raku(::?CLASS:U: --> Str:D) { ... }
multi method raku(::?CLASS:D: --> Str:D) { self.^name ~ '.new(' ~ self.record.raku ~ ')' }

multi method ACCEPTS(::?CLASS:_: ::?CLASS:U --> True) { }
# multi method ACCEPTS(::?CLASS:U: T:D --> Bool:D)      { ... }
multi method ACCEPTS(::?CLASS:D: |args --> Bool:D)    { self.record.ACCEPTS: |args }

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
) is raw {
    if $field ~~ Data::Record::Instance {
        if $value ~~ Data::Record::Instance {
            if $value.DEFINITE {
                op $value ~~ $field
                ?? $value
                !! $field.new: $value.record, |%named-args
            } else {
                die X::Data::Record::TypeCheck.new:
                    operation => $operation,
                    expected  => $field,
                    got       => $value
            }
        } elsif $value ~~ $field.for {
            op $field.new: $value, |%named-args
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
