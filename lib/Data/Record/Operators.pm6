use v6.e.PREVIEW;
use Data::Record::Instance;
unit module Data::Record::Operators;

my constant I = Data::Record::Instance;

#|[ Creates a new record type for a tuple. ]
only circumfix:«<@ @>»(|args) is export {
    require ::('Data::Record::Tuple');
    Data::Record::<Tuple>.beget: |args
}

#|[ Creates a new record type for a list. ]
only circumfix:<[@ @]>(|args) is export {
    require ::('Data::Record::List');
    Data::Record::<List>.beget: |args
}

#|[ Creates a new record type for a map. ]
only circumfix:<{@ @}>(|args) is export {
    require ::('Data::Record::Map');
    Data::Record::<Map>.beget: |args
}

#|[ Given a record type and a data structure, wraps the data structure so that
    it can be an instance of the record type without changing its contents. ]
proto infix:«(><)»($, $) is tighter<=> is export {*}
multi infix:«(><)»(I:_ $lhs is raw, Mu:D $rhs is raw) {
    $lhs($rhs):wrap
}
multi infix:«(><)»(Mu:D $lhs is raw, I:_ $rhs is raw) {
    $rhs($lhs):wrap
}

#|[ When a record type is on the blunt end, consumes the data structure on the
    blunt end, stripping any extraneous values. When a record type is on the
    sharp end, subsumes the data structure on the blunt end, filling in any
    missing values if possible. In both cases, the data structure will be
    coerced to an instance of the record type. ]
proto infix:«(<<)»($, $) is tighter<=> is export {*}
multi infix:«(<<)»(I:_ $lhs is raw, Mu:D $rhs is raw) {
    $lhs($rhs):subsume
}
multi infix:«(<<)»(Mu:D $lhs is raw, I:_ $rhs is raw) {
    $rhs($lhs):consume
}

#|[ When a record type is on the blunt end, consumes the data structure on the
    blunt end, stripping any extraneous values. When a record type is on the
    sharp end, subsumes the data structure on the blunt end, filling in any
    missing values if possible. In both cases, the data structure will be
    coerced to an instance of the record type. ]
proto infix:«(>>)»($, $) is tighter<=> is export {*}
multi infix:«(>>)»(I:_ $lhs is raw, Mu:D $rhs is raw) {
    $lhs($rhs):consume
}
multi infix:«(>>)»(Mu:D $lhs is raw, I:_ $rhs is raw) {
    $rhs($lhs):subsume
}

#|[ Given a record type and a data structure, coerces the data structure to the
    record type by any means possible. ]
proto infix:«(<>)»($, $) is tighter<=> is export {*}
multi infix:«(<>)»(I:_ $lhs is raw, Mu:D $rhs is raw) {
    $lhs($rhs):coerce
}
multi infix:«(<>)»(Mu:D $lhs is raw, I:_ $rhs is raw) {
    $rhs($lhs):coerce
}
