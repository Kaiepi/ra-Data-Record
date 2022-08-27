use v6.d;
use Data::Record::Mode;
use Data::Record::Exceptions;
use Data::Record::Instance;

class Data::Record::Lifter does Callable is repr<Uninstantiable> {
    my constant I = Data::Record::Instance;

    #|[ Lifts the type of a value to that of an arbitrary record field. In other
        rwords, typecheck; if the field is a record as well, wrap the value we toss
        at it so we can predict its future typechecks. ]
    proto method CALL-ME(Mu, Mu) is raw is hidden-from-backtrace {*}
    #=[ For nicer errors, set a Str:D $*operation. ]
    multi method CALL-ME(I \a, I \b;; :$mode!) {
        X::Data::Record::TypeCheck.new(
            :operation($*operation // 'unknown'), :expected(b), :got(a)
        ).throw unless a.DEFINITE;
        Metamodel::Primitives.is_type(a, b) ?? a !! b.new(a.unrecord, :$mode)
    }
    multi method CALL-ME(Mu \a, I \b;; :$mode!) {
        X::Data::Record::TypeCheck.new(
            :operation($*operation // 'unknown'), :expected(b), :got(a)
        ).throw unless Metamodel::Primitives.is_type(a, b.^for);
        b.new: a, :$mode
    }
    multi method CALL-ME(Mu \a, Mu \b;; :mode($)!) {
        X::Data::Record::TypeCheck.new(
            :operation($*operation // 'unknown'), :expected(b), :got(a)
        ).throw unless b.ACCEPTS: a;
        a
    }
}

my package EXPORT {
    package DEFAULT {
        OUR::«'&infix:<@~~>'» := Data::Record::Lifter;
    }
}
