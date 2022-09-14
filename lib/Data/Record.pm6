use v6.d;
use Data::Record::Exceptions;
use Data::Record::Instance;
use Data::Record::Operators;
use Data::Record::Tuple;
use Data::Record::List;
use Data::Record::Map;
only EXPORT(--> Map:D) {
    Map.new:
        '&circumfix:«<@ @>»' => &circumfix:«<@ @>»,
        '&circumfix:<{@ @}>' => &circumfix:<{@ @}>,
        '&circumfix:<[@ @]>' => &circumfix:<[@ @]>,
        '&infix:«(><)»' => &infix:«(><)»,
        '&infix:«(<<)»' => &infix:«(<<)»,
        '&infix:«(>>)»' => &infix:«(>>)»,
        '&infix:«(<>)»' => &infix:«(<>)»,
        '&infix:<eqv>' => &infix:<eqv>,
}
unit module Data::Record:auth<zef:Kaiepi>:ver<1.0.2>:api<2>;
