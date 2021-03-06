use v6.d;
use Data::Record::Operators;
use Data::Record::Exceptions;
use Data::Record::Instance;
use Data::Record::Tuple;
use Data::Record::List;
use Data::Record::Map;
sub EXPORT(--> Map:D) {
    Map.new:
        '&infix:<eqv>' => &infix:<eqv>,
        |Data::Record::Operators::EXPORT::DEFAULT.WHO.keys.map({ $_ => ::($_) })
}
unit module Data::Record:auth<github:Kaiepi>:ver<0.2.3>:api<0>;

constant Instance = Data::Record::Instance;
constant Tuple    = Data::Record::Tuple;
constant List     = Data::Record::List;
constant Map      = Data::Record::Map;

my package EXPORT::DEFAULT {
    package X::Data {
        constant Record = X::Data::Record;
    }
}
