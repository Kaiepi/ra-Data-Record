use v6.e.PREVIEW;
unit role Data::Record::Instance[::T];

proto method new(::?CLASS:_: |)                                 {*}
# multi method new(::?CLASS:_: T:D)                             { ... }
# multi method new(::?CLASS:_: T:D, Bool:D :$consume! where ?*) { ... }
# multi method new(::?CLASS:_: T:D, Bool:D :$subsume! where ?*) { ... }
# multi method new(::?CLASS:_: T:D, Bool:D :$coerce! where ?*)  { ... }

#|[ The type that this record wraps. ]
method for(::?CLASS:_: --> Mu) { T }
#|[ The fields of this record. ]
method fields(::?CLASS:_: --> T:D) { ... }

#|[ Returns this record's wrapped data as-is. ]
method record(::?CLASS:D: --> T:D)   { ... }
#|[ Returns this record's wrapped data, recursively unwrapping any records found within it. ]
method unrecord(::?CLASS:D: --> T:D) { ... }

multi method gist(::?CLASS:D: --> Str:D) { self.record.gist }

# multi method raku(::?CLASS:U: --> Str:D) { ... }

multi method ACCEPTS(::?CLASS:_: ::?CLASS:U --> True) { }
# multi method ACCEPTS(::?CLASS:U: T:D --> Bool:D)    { ... }
# multi method ACCEPTS(::?CLASS:D: | --> Bool:D)      { ... }
