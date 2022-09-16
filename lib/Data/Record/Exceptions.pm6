use v6.e.PREVIEW;

role X::Data::Record { }

role X::Data::Record::Arity does X::Data::Record {
    has Str:D $.operation is required;
    has Mu    $.type      is built(:bind) is required;
    has Str:D $.what      is required;
    has Mu    $.key       is built(:bind) is required;
}

class X::Data::Record::Missing is Exception does X::Data::Record::Arity {
    has Mu $.field is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "Missing required field at $!what '$!key.gist()' ($!field.raku()) during $!operation for record of type $!type.^name()"
    }
}

class X::Data::Record::Extraneous is Exception does X::Data::Record::Arity {
    has Mu $.value is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "Forbidden extraneous value at $!what '$!key.gist()' ($!value.raku()) found during $!operation for record of type $!type.^name()"
    }
}

class X::Data::Record::TypeCheck is X::TypeCheck does X::Data::Record { }

class X::Data::Record::Definite is Exception does X::Data::Record {
    has Mu    $.type  is required is built(:bind);
    has Str:D $.what  is required;
    has Mu    $.key   is required is built(:bind);
    has Mu    $.value is required is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "$!what.tc() '$!key.gist()' ($!value.raku()) in record of type $!type.^name() must be defined"
    }
}

class X::Data::Record::OutOfBounds is Exception does X::Data::Record {
    has Mu    $.type is required is built(:bind);
    has Str:D $.what is required;
    has Mu    $.key  is required is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "$!what.tc() '$!key.gist()' is out of bounds for record of type $!type.^name()"
    }
}

class X::Data::Record::Immutable is Exception does X::Data::Record {
    has Mu    $.type      is required is built(:bind);
    has Str:D $.operation is required;
    method message(::?CLASS:D: --> Str:D) {
        "Record of type $!type.^name() is immutable, therefore $!operation cannot be done"
    }
}

class X::Data::Record::Block is Exception does X::Data::Record {
    has Mu $.type is required is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "Expected a block when creating a new $!type.^name() type, but got a hash instead"
    }
}

class X::Data::Record::Composed is Exception does X::Data::Record {
    has Mu    $.type      is built(:bind) is required;
    has Str:D $.operation is required;
    method message(::?CLASS:D: --> Str:D) {
        "Cannot $!operation a record of type $!type.^name() after it has been composed"
    }
}
