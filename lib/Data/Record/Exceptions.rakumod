use v6.d;

my role X::Data::Record is export { }

my class X::Data::Record::TypeCheck is X::TypeCheck does X::Data::Record { }

my role X::Data::Record::Arity does X::Data::Record {
    has Str:D $.operation is required;
    has Mu    $.type      is required is built(:bind);
    has Str:D $.what      is required;
    has Mu    $.key       is required is built(:bind);
}

my class X::Data::Record::Missing is Exception does X::Data::Record::Arity {
    method message(::?CLASS:D: --> Str:D) {
        "Missing required $!what '$!key.gist()' during $!operation for record of type $!type.^name()"
    }
}

my class X::Data::Record::Extraneous is Exception does X::Data::Record::Arity {
    method message(::?CLASS:D: --> Str:D) {
        "Forbidden extraneous $!what '$!key.gist()' found during $!operation for record of type $!type.^name()"
    }
}

my class X::Data::Record::Definite is Exception does X::Data::Record {
    has Mu    $.type  is required is built(:bind);
    has Str:D $.what  is required;
    has Mu    $.key   is required is built(:bind);
    has Mu    $.value is required is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "$!what.tc() '$!key.gist()' ($!value.raku()) in record of type $!type.^name() must be defined"
    }
}

my class X::Data::Record::OutOfBounds is Exception does X::Data::Record {
    has Mu    $.type is required is built(:bind);
    has Str:D $.what is required;
    has Mu    $.key  is required is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "$!what.tc() '$!key.gist()' is out of bounds for record of type $!type.^name()"
    }
}

my class X::Data::Record::Immutable is Exception does X::Data::Record {
    has Str:D $.operation is required;
    has Mu    $.type      is required is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "Record of type $!type.^name() is immutable, therefore $!operation cannot be done"
    }
}

my class X::Data::Record::Block is Exception does X::Data::Record {
    has Mu $.type is required is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "Expected a block when creating a new $!type.^name() type, but got a hash instead"
    }
}

my class X::Data::Record::Merge is Exception does X::Data::Record {
    has Mu $.type is required is built(:bind);
    method message(::?CLASS:D: --> Str:D) {
        "It does not make sense to merge two $!type.^name() types"
    }
}

my class X::Data::Record::Composed is Exception does X::Data::Record {
    has Mu    $.type      is required is built(:bind);
    has Str:D $.operation is required;
    method message(::?CLASS:D: --> Str:D) {
        "Cannot $!operation a record of type $!type.^name() after it has been composed"
    }
}
