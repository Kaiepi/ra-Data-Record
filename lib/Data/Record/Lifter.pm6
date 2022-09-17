use v6.e.PREVIEW;
use Data::Record::Mode;
use Data::Record::Exceptions;
use Data::Record::Instance;

my constant I = Data::Record::Instance;

#|[ A control exception containing the operands of a failed record typecheck. ]
class CX::Rest does X::Control {
    #|[ The value in a record typecheck. ]
    has $.value is built(:bind) is required;
    #|[ The record field in a record typecheck. ]
    has $.field is built(:bind) is required;

    my @POOL := cache $?CLASS.CREATE xx *;
    submethod new(::?CLASS:_: Mu $value is raw, Mu $field is raw) {
        use nqp;
        my $self := @POOL.AT-POS: nqp::threadid(nqp::currentthread());
        nqp::bindattr($self, $?CLASS, '$!value', $value);
        nqp::bindattr($self, $?CLASS, '$!field', $field);
        $self.reset-backtrace;
        $self
    }

    #|[ Fails a record typecheck by throwing X::Data::Record::TypeCheck. ]
    method flunk(::?CLASS:D: Str:D $operation --> Nil) {
        X::Data::Record::TypeCheck.new(
            :$operation, :expected($!field), :got($!value)
        ).throw;
    }

    method message(::?CLASS:D: --> '<rest control exception>') { }
}
#=[ This is thrown primarily by Data::Record::Lifter, which is how it can be
    decoupled from record instances' value mapping metamethods. ]

#|[ Lifts the type of a value to that of an arbitrary record field. That will
    smartmatch given the value, throwing CX::Rest should that fail. A nested
    record type will wrap any matching value in order to cache ACCEPTS calls. ]
class Data::Record::Lifter does Callable is repr<Uninstantiable> {
    proto method CALL-ME(Mu, Mu) is raw {*}
    multi method CALL-ME(I \a, I \b;; :$mode!) {
        CX::Rest.new(a, b).throw unless a.DEFINITE;
        Metamodel::Primitives.is_type(a, b) ?? a !! b.new(a.record, :$mode)
    }
    multi method CALL-ME(Mu \a, I \b;; :$mode!) {
        CX::Rest.new(a, b).throw unless Metamodel::Primitives.is_type(a, b.^for);
        b.new: a, :$mode
    }
    multi method CALL-ME(Mu \a, Mu \b) {
        CX::Rest.new(a, b).throw unless b.ACCEPTS: a;
        a
    }
}

my package EXPORT {
    package DEFAULT {
        OUR::«'&infix:<@~~>'» := Data::Record::Lifter;
    }
}
