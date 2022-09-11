![Build Status](https://github.com/Kaiepi/ra-Data-Record/actions/workflows/test.yml/badge.svg)

NAME
====

Data::Record - Record types!

SYNOPSIS
========

```perl6
use Data::Record;

# Data::Record introduces record types for maps, lists, and tuples:
my constant Schema = {@
    name  => Str:D,
    items => [@ <@ Int:D, Str:D @> @]
@} :name('Schema');

# With the type we just made, we can typecheck data structures to ensure they
# match it:
my %valid =
    name  => 'Kaiepi',
    items => [(1, 'Item 1'), (2, 'Item 2')];
my %invalid =
    name        => 'Mrofnet',
    items       => [],
    constructor => 'Thanks, JavaScript.';
say %valid ~~ Schema;   # OUTPUT: True
say %invalid ~~ Schema; # OUTPUT: False

# ...but typechecking this way is inefficient, and is always done eagerly!
# Using the (<<), (>>), (<>), and (><) operators provided, data can be coerced
# to our record type by various means:
%valid   := %valid (><) Schema;
%invalid := %invalid (<<) Schema;
say %invalid; # OUTPUT: {items => [], name => Mrofnet}

# For the most part, coerced data can be used the same way as the original
# data, with the bonus of extra typechecking:
{
    CATCH { default { say .^name } }
    %invalid<items>.push: (3, 'Item 3');        # OK!
    %invalid<items>.push: "OOPSIE WOOPSIE OwO"; # OUTPUT: X::Data::Record::TypeCheck
}

# Finally, to restore the data's original typing, simply call the unrecord
# method on it:
my %now-valid := %invalid.unrecord;
say %now-valid.^name;           # OUTPUT: Hash
say %now-valid<items>.^name;    # OUTPUT: Array
say %now-valid<items>[0].^name; # OUTPUT: List
```

DESCRIPTION
===========

`Data::Record` is a library that adds support for record types to Raku. Operators for creating record types for maps, lists, and tuples are included. Data structures can then be coerced to these record types using the coercion operators provided, some of which will sanitize them, all of which allow for efficient typechecking for common operations you can do with them.

For documentation on how this library can be used, refer to the [wiki](https://github.com/Kaiepi/p6-Data-Record/wiki/).

AUTHOR
======

Ben Davies (Kaiepi)

COPYRIGHT AND LICENSE
=====================

Copyright 2022 Ben Davies

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

