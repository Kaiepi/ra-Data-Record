use v6.d;
use Data::Record::Mode;
use Data::Record::Exceptions;
unit role MetamodelX::RecordLifter[::I];

#|[ Lifts the type of a value to that of an arbitrary record field. In other
    rwords, typecheck; if the field is a record as well, wrap the value we toss
    at it so we can predict its future typechecks. ]
proto method lift(Mu, Mu, *%) is raw is hidden-from-backtrace {*}
#=[ For nicer errors, set a Str:D $*operation. ]
multi method lift(I \b, I \a;; :$mode!) {
    X::Data::Record::TypeCheck.new(
        :operation($*operation // 'unknown'), :expected(b), :got(a)
    ).throw unless a.DEFINITE;
    Metamodel::Primitives.is_type(a, b) ?? a !! b.new(a.unrecord, :$mode)
}
multi method lift(I \b, Mu \a;; :$mode!) {
    X::Data::Record::TypeCheck.new(
        :operation($*operation // 'unknown'), :expected(b), :got(a)
    ).throw unless Metamodel::Primitives.is_type(a, b.^for);
    b.new: a, :$mode
}
multi method lift(Mu \b, Mu \a;; :mode($)!) {
    X::Data::Record::TypeCheck.new(
        :operation($*operation // 'unknown'), :expected(b), :got(a)
    ).throw unless b.ACCEPTS: a;
    a
}
