use v6.d;
unit module Data::Record::Operators;

#|[ Creates a new record type for a tuple. ]
proto sub circumfix:«<@ @>»(|) is export {*}

#|[ Creates a new record type for a list. ]
proto sub circumfix:«[@ @]»(|) is export {*}

#|[ Creates a new record type for a map. ]
proto sub circumfix:<{@ @}>(|) is export {*}

#|[ When a record type is on the blunt end, consumes the data structure on the
    blunt end, stripping any extraneous values. When a record type is on the
    sharp end, subsumes the data structure on the blunt end, filling in any
    missing values if possible. In both cases, the data structure will be
    coerced to an instance of the record type. ]
proto sub infix:«(<<)»(|) is export {*}

#|[ When a record type is on the blunt end, consumes the data structure on the
    blunt end, stripping any extraneous values. When a record type is on the
    sharp end, subsumes the data structure on the blunt end, filling in any
    missing values if possible. In both cases, the data structure will be
    coerced to an instance of the record type. ]
proto sub infix:«(>>)»(|) is export {*}

#|[ Given a record type and a data structure, coerces the data structure to the
    record type by any means possible. ]
proto sub infix:«(<>)»(|) is export {*}

#|[ Given a record type and a data structure, wraps the data structure so that
    it can be an instance of the record type without changing its contents. ]
proto sub infix:«(><)»(|) is export {*}
