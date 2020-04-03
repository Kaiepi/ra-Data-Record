use v6.d;

my role X::Data::Record is export { }

my role X::Data::Record::Arity does X::Data::Record {
    has Str:D $.operation is required;
    has Mu    $.type      is required;
    has Str:D $.what      is required;
    has Mu    $.key       is required;
    submethod BUILD(::?CLASS:D: Str:D :$!operation!, Mu :$type! is raw, Str:D :$!what!, Mu :$key! is raw --> Nil) {
        $!type  := $type;
        $!key   := $key;
    }
}

my class X::Data::Record::Missing is Exception does X::Data::Record::Arity {
    has Mu $.field;
    submethod TWEAK(::?CLASS:D: Mu :$field! is raw --> Nil) {
        $!field := $field;
    }
    method message(::?CLASS:D: --> Str:D) {
        "Missing required field at $!what '$!key.gist()' ($!field.raku()) during $!operation for record of type $!type.^name()"
    }
}

my class X::Data::Record::Extraneous is Exception does X::Data::Record::Arity {
    has Mu $.value;
    submethod TWEAK(::?CLASS:D: Mu :$value! is raw --> Nil) {
        $!value := $value;
    }
    method message(::?CLASS:D: --> Str:D) {
        "Forbidden extraneous value at $!what '$!key.gist()' ($!value.raku()) found during $!operation for record of type $!type.^name()"
    }
}

my class X::Data::Record::TypeCheck is X::TypeCheck does X::Data::Record { }

my class X::Data::Record::Definite is Exception does X::Data::Record {
    has Mu    $.type  is required;
    has Str:D $.what  is required;
    has Mu    $.key   is required;
    has Mu    $.value is required;
    submethod BUILD(::?CLASS:D: Mu :$type! is raw, Str:D :$!what!, Mu :$key! is raw, Mu :$value! is raw --> Nil) {
        $!type  := $type;
        $!key   := $key;
        $!value := $value;
    }
    method message(::?CLASS:D: --> Str:D) {
        "$!what.tc() '$!key.gist()' ($!value.raku()) in record of type $!type.^name() must be defined"
    }
}

my class X::Data::Record::OutOfBounds is Exception does X::Data::Record {
    has Mu    $.type is required;
    has Str:D $.what is required;
    has Mu    $.key  is required;
    submethod BUILD(::?CLASS:D: Mu :$type! is raw, Str:D :$!what!, Mu :$key! is raw --> Nil) {
        $!type := $type;
        $!key  := $key;
    }
    method message(::?CLASS:D: --> Str:D) {
        "$!what.tc() '$!key.gist()' is out of bounds for record of type $!type.^name()"
    }
}

my class X::Data::Record::Immutable is Exception does X::Data::Record {
    has Mu    $.type      is required;
    has Str:D $.operation is required;
    submethod BUILD(::?CLASS:D: Mu :$type! is raw, Str:D :$!operation! --> Nil) {
        $!type := $type;
    }
    method message(::?CLASS:D: --> Str:D) {
        "Record of type $!type.^name() is immutable, therefore $!operation cannot be done"
    }
}

my class X::Data::Record::Block is Exception does X::Data::Record {
    has Mu $.type is required;
    submethod BUILD(::?CLASS:D: Mu :$type! is raw --> Nil) {
        $!type := $type;
    }
    method message(::?CLASS:D: --> Str:D) {
        "Expected a block when creating a new $!type.^name() type, but got a hash instead"
    }
}

my class X::Data::Record::Composed is Exception does X::Data::Record {
    has Mu    $.type      is required;
    has Str:D $.operation is required;
    submethod BUILD(::?CLASS:D: Mu :$type! is raw, Str:D :$!operation --> Nil) {
        $!type := $type;
    }
    method message(::?CLASS:D: --> Str:D) {
        "Cannot $!operation a record of type $!type.^name() after it has been composed"
    }
}
