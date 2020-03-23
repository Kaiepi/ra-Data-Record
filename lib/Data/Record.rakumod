use v6.e.PREVIEW;
use Data::Record::Operators;
use Data::Record::Tuple;
use Data::Record::List;
use Data::Record::Map;
sub EXPORT(--> Map:D) {
    Map.new:
        '&infix:<eqv>' => &infix:<eqv>,
        |Data::Record::Operators::EXPORT::DEFAULT.WHO.keys.map({ $_ => ::($_) })
}
unit module Data::Record:auth<github:Kaiepi>:ver<0.0.1>:api<0>;

constant Tuple = Data::Record::Tuple;
constant List  = Data::Record::List;
constant Map   = Data::Record::Map;
