use v6.e.PREVIEW;
#|[ A mode of coercion. ]
unit enum Data::Record::Mode <wrap consume subsume coerce>;

my package EXPORT::DEFAULT {
    constant WRAP    = wrap;
    constant CONSUME = consume;
    constant SUBSUME = subsume;
    constant COERCE  = coerce;
}
