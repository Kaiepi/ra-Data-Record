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
unit module Data::Record:auth<zef:Kaiepi>:ver<1.0.0>:api<2>;
