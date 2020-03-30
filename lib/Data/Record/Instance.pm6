use v6.d;
unit role Data::Record::Instance[::T];

proto method new(::?ROLE:_: |)                                 {*}
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

#|[ Returns this record's wrapped data as-is. ]
method record(::?ROLE:D: --> T:D)   { ... }
#|[ Returns this record's wrapped data, recursively unwrapping any records found within it. ]
method unrecord(::?ROLE:D: --> T:D) { ... }

multi method gist(::?ROLE:D: --> Str:D) { self.record.gist }

# multi method raku(::?ROLE:U: --> Str:D) { ... }

multi method ACCEPTS(::?ROLE:_: ::?ROLE:U --> True) { }
# multi method ACCEPTS(::?ROLE:U: T:D --> Bool:D)    { ... }
# multi method ACCEPTS(::?ROLE:D: | --> Bool:D)      { ... }
