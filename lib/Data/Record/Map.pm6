use v6.d;
use Data::Record::Instance;
use Data::Record::Exceptions;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;

my role Data::Record::Map[Bool:D :$structural! where !*]
   does Data::Record::Instance[Map]
   does Iterable
   does Associative
{
    has %!record;

    submethod BUILD(::?ROLE:D: :%record! --> Nil) {
        %!record := %record;
    }

    multi method new(::?ROLE:_: Map:D $original is raw --> ::?ROLE:D) {
        my %record := self.wrap: $original;
        self.bless: :%record
    }
    multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :consume($)! where ?* --> ::?ROLE:D) {
        my %record := self.consume: $original;
        self.bless: :%record
    }
    multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :subsume($)! where ?* --> ::?ROLE:D) {
        my %record := self.subsume: $original;
        self.bless: :%record
    }
    multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :coerce($)! where ?* --> ::?ROLE:D) {
        my %record := self.coerce: $original;
        self.bless: :%record
    }

    method wrap(::THIS ::?ROLE:_: Map:D $original is raw --> Map:D) {
        my %fields := %.fields;
        $original.new: gather for ($original.keys ∪ %fields.keys).keys -> Mu $key is raw {
            X::Data::Record::Extraneous.new(
                operation => 'map reification',
                type      => THIS,
                what      => 'key',
                key       => $key,
                value     => $original{$key},
            ).throw unless %fields{$key}:exists;
            X::Data::Record::Missing.new(
                operation => 'map reification',
                type      => THIS,
                what      => 'key',
                key       => $key,
                field     => %fields{$key},
            ).throw unless $original{$key}:exists;
            self!field-op: 'map reification', {
                take-rw $key => $_
            }, %fields{$key}, $original{$key};
        }
    }

    method consume(::THIS ::?ROLE:_: Map:D $original is raw --> Map:D) {
        my %fields := %.fields;
        $original.new: gather for ($original.keys ∪ %fields.keys).keys -> Mu $key is raw {
            next unless %fields{$key}:exists;
            X::Data::Record::Missing.new(
                operation => 'map reification',
                type      => THIS,
                what      => 'key',
                key       => $key,
                field     => %fields{$key},
            ).throw unless $original{$key}:exists;
            self!field-op: 'map reification', {
                take-rw ($key => $_)
            }, %fields{$key}, $original{$key}, :consume;
        }
    }

    method subsume(::THIS ::?ROLE:_: Map:D $original is raw --> Map:D) {
        my %fields := %.fields;
        $original.new: gather for ($original.keys ∪ %fields.keys).keys -> Mu $key is raw {
            X::Data::Record::Extraneous.new(
                operation => 'map reification',
                type      => THIS,
                what      => 'key',
                key       => $key,
                value     => $original{$key},
            ).throw unless %fields{$key}:exists;

            my Mu $field := %fields{$key};
            if $original{$key}:exists {
                my Mu $value := $original{$key};
                self!field-op: 'map reification', {
                    take-rw ($key => $_)
                }, $field, $value, :subsume;
            } elsif $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite {
                X::Data::Record::Definite.new(
                    type  => THIS,
                    what  => 'key',
                    key   => $key,
                    value => $field,
                ).throw;
            } else {
                take-rw ($key => $field);
            }
        }
    }

    method coerce(::THIS ::?ROLE:_: Map:D $original is raw --> Map:D) {
        my %fields := %.fields;
        $original.new: gather for ($original.keys ∪ %fields.keys).keys -> Mu $key is raw {
            next unless %fields{$key}:exists;

            my Mu $field := %fields{$key};
            if $original{$key}:exists {
                my Mu $value := $original{$key};
                self!field-op: 'map reification', {
                    take-rw ($key => $_)
                }, $field, $value, :coerce;
            } elsif $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite {
                X::Data::Record::Definite.new(
                    type  => THIS,
                    what  => 'key',
                    key   => $key,
                    value => $field,
                ).throw;
            } else {
                take-rw ($key => $field);
            }
        }
    }

    method fields(::?ROLE:_: --> Map:D) {
        state Map:_ $fields;
        $fields := self.^fields.Map without $fields;
        $fields
    }

    method record(::?ROLE:D: --> Map:D) { %!record }

    method unrecord(::?ROLE:D: --> Map:D) {
        %!record.new: %!record.kv.map: &unrecord
    }
    proto sub unrecord(Mu, Mu --> Pair:D) {*}
    multi sub unrecord(Mu \key, Data::Record::Instance:D \recorded --> Pair:D) is default {
        (key) => recorded.unrecord
    }
    multi sub unrecord(Mu \key, Mu \value --> Pair:D) {
        (key) => value
    }

    multi method raku(::?CLASS:U: --> Str:D) {
        my Str:D $raku = '{@ ' ~ %.fields.map(*.raku).join(', ') ~ ' @}';
        my Str:D $name = self.^name;
        $raku ~= ":name('$name')" unless $name eq MetamodelX::RecordHOW::ANON_NAME;
        $raku
    }

    multi method ACCEPTS(::?CLASS:U: Map:D $map is raw --> Bool:D) {
        my %fields:= %.fields;
        for ($map.keys ∪ %fields.keys).keys -> Mu $key is raw {
            return False unless %fields{$key}:exists;
            return False unless $map{$key}:exists && $map{$key} ~~ %.fields{$key};
        }
        True
    }

    method EXISTS-KEY(::?ROLE:D: Mu $key is raw --> Bool:D) {
        %!record{$key}:exists
    }

    method AT-KEY(::THIS ::?ROLE:D: Mu $key is raw --> Mu) is raw {
        if %.fields{$key}:!exists {
            die X::Data::Record::OutOfBounds.new:
                type => THIS,
                what => 'key',
                key  => $key
        } else {
            %!record{$key}
        }
    }

    method BIND-KEY(::THIS ::?ROLE:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
        my %fields := %.fields;
        if %fields{$key}:!exists {
            die X::Data::Record::OutOfBounds.new:
                type => THIS,
                what => 'key',
                key  => $key
        } else {
            self!field-op: 'binding', {
                %!record{$key} := $_
            }, %fields{$key}, $value
        }
    }

    method ASSIGN-KEY(::THIS ::?ROLE:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
        my %fields := %.fields;
        if %fields{$key}:!exists {
            die X::Data::Record::OutOfBounds.new:
                type => THIS,
                what => 'key',
                key  => $key
        } else {
            self!field-op: 'assignment', {
                %!record{$key} = $_
            }, %fields{$key}, $value
        }
    }

    method DELETE-KEY(::THIS ::?ROLE:D: Mu --> Mu) {
        X::Data::Record::Immutable.new(
            operation => 'deletion',
            type      => THIS
        ).throw;
    }

    # An iterator that wraps the list of values passed to Hash array ops
    # (push/append).
    #
    # "But this never returns anything besides IterationEnd on pull? What's the
    # point of this?"
    #
    # Hash array methods have exceptions of their own that they can fail with.
    # The point of this is to ensure typechecking gets deferred until Hash's
    # methods iterate over this so those get handled properly.
    my role ArrayIterator does Iterator {
        has Mu         $!type   is required;
        has            %!record is required;
        has            %!fields is required;
        has Iterator:D $!values is required;

        submethod BUILD(::?ROLE:D: Mu :$type! is raw, :%record!, :%fields!, :@values --> Nil) {
            $!type   := $type;
            %!record := %record;
            %!fields := %fields;
            $!values := @values.iterator;
        }

        method new(::?ROLE:_: Mu $type is raw, %record, %fields, @values --> ::?ROLE:D) {
            self.bless: :$type, :%record, :%fields, :@values
        }

        method pull-one(::?CLASS:D: --> Mu) is raw {
            my Mu     $key;
            my Bool:D $has-key = False;
            until (my Mu $value := $!values.pull-one) =:= IterationEnd {
                if $has-key {
                    $has-key = False;
                    self!pull-one-pair: $key, $value;
                } elsif $value ~~ Pair:D {
                    self!pull-one-pair: $value.key, $value.value;
                } else {
                    $key     := $value;
                    $has-key  = True;
                }
            }
            IterationEnd
        }

        method !pull-one-pair(::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
            if %!fields{$key}:exists {
                my Mu $field := %!fields{$key};
                X::Data::Record::TypeCheck.new(
                    operation => $.operation,
                    expected  => $field,
                    got       => $value,
                ).throw unless $field ~~ Data::Record::Instance[List] | Array;
                self!perform-array-op: $key, $value
            } else {
                die X::Data::Record::OutOfBounds.new:
                    type => $!type,
                    what => 'key',
                    key  => $key
            }
        }

        method operation(::?CLASS:D: --> Str:D) { ... }

        method !perform-array-op(::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) { ... }

        method is-lazy(::?CLASS:D: --> Bool:D) { $!values.is-lazy }
    }

    my class PushIterator does ArrayIterator {
        method operation(::?CLASS:D: --> 'push') { }

        method !perform-array-op(::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) {
            %!record{$key}.push: $value
        }
    }

    method push(::THIS ::?ROLE:D: +values --> ::?ROLE:D) {
        %!record.push: Seq.new: PushIterator.new: THIS, %!record, %.fields, values;
        self
    }

    my class AppendIterator does ArrayIterator {
        method operation(::?CLASS:D: --> 'append') { }

        method !perform-array-op(::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) {
            %!record{$key}.append: |$value
        }
    }

    method append(::THIS ::?ROLE:D: +values --> ::?ROLE:D) {
        %!record.append: Seq.new: AppendIterator.new: THIS, %!record, %.fields, values;
        self
    }

    method iterator(::?ROLE:D: --> Mu)  { %!record.iterator }
    method is-lazy(::?ROLE:D: --> Mu)   { %!record.is-lazy }
    method list(::?ROLE:D: --> Mu)      { %!record.list }
    method elems(::?ROLE:D: --> Mu)     { %!record.elems }
    method cache(::?ROLE:D: --> Mu)     { %!record.cache }
    method eager(::?ROLE:D: --> Mu)     { %!record.eager }
    method lazy(::?ROLE:D: --> Mu)      { %!record.lazy }
    method hash(::?ROLE:D: --> Mu)      { self }
    method keys(::?ROLE:D: --> Mu)      { %!record.keys }
    method values(::?ROLE:D: --> Mu)    { %!record.values }
    method kv(::?ROLE:D: --> Mu)        { %!record.kv }
    method pairs(::?ROLE:D: --> Mu)     { %!record.pairs }
    method antipairs(::?ROLE:D: --> Mu) { %!record.antipairs }
}

my role Data::Record::Map[Bool:D :$structural! where ?*]
   does Data::Record::Instance[Map]
   does Iterable
   does Associative
{
    has %!record;

    submethod BUILD(::?ROLE:D: :%record! --> Nil) {
        %!record := %record;
    }

    multi method new(::?ROLE:_: Map:D $original is raw --> ::?ROLE:D) {
        my %record := self.wrap: $original;
        self.bless: :%record
    }
    multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :consume($)! where ?* --> ::?ROLE:D) {
        my %record := self.consume: $original;
        self.bless: :%record
    }
    multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :subsume($)! where ?* --> ::?ROLE:D) {
        my %record := self.subsume: $original;
        self.bless: :%record
    }
    multi method new(::?ROLE:_: Map:D $original is raw, Bool:D :coerce($)! where ?* --> ::?ROLE:D) {
        my %record := self.coerce: $original;
        self.bless: :%record
    }

    method !strict-coerce(::THIS ::?ROLE:_: Map:D $original is raw, *%named-args --> Map:D) {
        my %fields := %.fields;
        $original.new: gather for ($original.keys ∪ %fields.keys).keys -> Mu $key is raw {
            X::Data::Record::Missing.new(
                operation => 'map reification',
                type      => THIS,
                what      => 'key',
                key       => $key,
                field     => %fields{$key},
            ).throw unless $original{$key}:exists;

            my Mu $value := $original{$key};
            if %fields{$key}:exists {
                my Mu $field := %fields{$key};
                self!field-op: 'map reification', {
                    take-rw ($key => $_)
                }, $field, $value, |%named-args;
            } else {
                take-rw ($key => $value);
            }
        }
    }

    method wrap(::?ROLE:_: Map:D $original is raw --> Map:D) {
        self!strict-coerce: $original
    }

    method consume(::?ROLE:_: Map:D $original is raw --> Map:D) {
        self!strict-coerce: $original, :consume
    }

    method !lax-coerce(::THIS ::?ROLE:_: Map:D $original is raw, *%named-args --> Map:D) {
        my %fields := %.fields;
        $original.new: gather for ($original.keys ∪ %fields.keys).keys -> Mu $key is raw {
            if $original{$key}:exists {
                my Mu $value := $original{$key};
                if %fields{$key}:exists {
                    my Mu $field := %fields{$key};
                    self!field-op: 'map reification', {
                        take-rw ($key => $_)
                    }, $field, $value, |%named-args;
                } else {
                    take-rw ($key => $value);
                }
            } else {
                my Mu $field := %fields{$key};
                X::Data::Record::Definite.new(
                    type  => THIS,
                    what  => 'key',
                    key   => $key,
                    value => $field,
                ).throw if $field.HOW ~~ Metamodel::DefiniteHOW && $field.^definite;
                take-rw ($key => $field);
            }
        }
    }

    method subsume(::?ROLE:_: Map:D $original is raw --> Map:D) {
        self!lax-coerce: $original, :subsume
    }

    method coerce(::?ROLE:_: Map:D $original is raw --> Map:D) {
        self!lax-coerce: $original, :coerce
    }

    method fields(::?ROLE:_: --> Map:D) {
        state Map:_ $fields;
        $fields := self.^fields.Map without $fields;
        $fields
    }

    method record(::?ROLE:D: --> Map:D) { %!record }

    method unrecord(::?ROLE:D: --> Map:D) {
        %!record.new: %!record.kv.map: &unrecord
    }
    proto sub unrecord(Mu, Mu --> Pair:D) {*}
    multi sub unrecord(Mu \key, Data::Record::Instance:D \recorded --> Pair:D) is default {
        (key) => recorded.unrecord
    }
    multi sub unrecord(Mu \key, Mu \value --> Pair:D) {
        (key) => value
    }

    multi method raku(::?CLASS:U: --> Str:D) {
        my Str:D $raku = '{@ ' ~ %.fields.map(*.raku).join(', ') ~ ' @}:structural';
        my Str:D $name = self.^name;
        $raku ~= ":name('$name')" unless $name eq MetamodelX::RecordHOW::ANON_NAME;
        $raku
    }

    multi method ACCEPTS(::?CLASS:U: Map:D $map is raw --> Bool:D) {
        for %.fields.kv -> Mu $key is raw, Mu $field is raw {
            return False unless $map{$key}:exists && $map{$key} ~~ $field;
        }
        True
    }

    method EXISTS-KEY(::?ROLE:D: Mu $key is raw --> Bool:D) {
        %!record{$key}:exists
    }

    method AT-KEY(::THIS ::?ROLE:D: Mu $key is raw --> Mu) is raw {
        %!record{$key}
    }

    method BIND-KEY(::THIS ::?ROLE:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
        my %fields := %.fields;
        if %fields{$key}:exists {
            self!field-op: 'binding', {
                %!record{$key} := $_
            }, %fields{$key}, $value
        } else {
            %!record{$key} := $value
        }
    }

    method ASSIGN-KEY(::THIS ::?ROLE:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
        my %fields := %.fields;
        if %fields{$key}:exists {
            self!field-op: 'assignment', {
                %!record{$key} = $_
            }, %fields{$key}, $value
        } else {
            %!record{$key} = $value
        }
    }

    method DELETE-KEY(::THIS ::?ROLE:D: Mu $key is raw --> Mu) is raw {
        # XXX: Not quite the right exception
        X::Data::Record::Immutable.new(
            operation => 'deletion',
            type      => THIS,
        ).throw if %.fields{$key}:exists;
        %!record{$key}:delete
    }

    # Similar deal to non-structural maps' ArrayIterator, only now we
    # actually do return values from .pull-one! These are to be
    # pushed/appended by the record itself, not us.
    my role ArrayIterator does Iterator {
        has            %!record is required;
        has            %!fields is required;
        has Iterator:D $!values is required;

        submethod BUILD(::?ROLE:D: :%record!, :%fields!, :@values! --> Nil) {
            %!record := %record;
            %!fields := %fields;
            $!values := @values.iterator;
        }

        method new(::?ROLE:_: %record, %fields, @values --> ::?ROLE:D) {
            self.bless: :%record, :%fields, :@values
        }

        method pull-one(::?CLASS:D: --> Mu) is raw {
            my Mu     $key;
            my Bool:D $has-key = False;
            until (my Mu $value := $!values.pull-one) =:= IterationEnd {
                if $has-key {
                    $has-key = False;
                    return self!pull-one-pair: $key, $value;
                } elsif $value ~~ Pair:D {
                    return self!pull-one-pair: $value.key, $value.value;
                } else {
                    $key     := $value;
                    $has-key  = True;
                }
            }
            IterationEnd
        }

        method !pull-one-pair(::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) is raw {
            if %!fields{$key}:exists {
                my Mu $field := %!fields{$key};
                X::Data::Record::TypeCheck.new(
                    operation => $.operation,
                    expected  => $field,
                    got       => $value,
                ).throw unless $field ~~ Data::Record::Instance[List] | Array;
                self!perform-array-op: $key, $value;
                next; # Not so fast, &return!
            } else {
                ($key => $value);
            }
        }

        method operation(::?CLASS:D: --> Str:D) { ... }

        method !perform-array-op(::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) { ... }
    }

    my class PushIterator does ArrayIterator {
        method operation(::?CLASS:D: --> 'push') { }

        method !perform-array-op(::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) {
            %!record{$key}.push: $value
        }
    }

    method push(::?ROLE:D: +values --> ::?ROLE:D) {
        %!record.push: Seq.new: PushIterator.new: %!record, %.fields, values;
        self
    }

    my class AppendIterator does ArrayIterator {
        method operation(::?CLASS:D: --> 'append') { }

        method !perform-array-op(::?CLASS:D: Mu $key is raw, Mu $value is raw --> Mu) {
            %!record{$key}.append: |$value
        }
    }

    method append(::?ROLE:D: +values --> ::?ROLE:D) {
        %!record.push: Seq.new: AppendIterator.new: %!record, %.fields, values;
        self
    }
}

my package EXPORT::DEFAULT {
    package Data::Record {
        constant Map = Data::Record::Map;
    }
}

multi sub circumfix:<{@ @}>(Pair:D $pair is raw, Str:_ :$name, Bool:D :$structural = False --> Mu) is export {
    my Mu $record   := MetamodelX::RecordHOW.new_type: :$name;
    my Mu $delegate := Data::Record::Map.^parameterize: :$structural;
    $record.^set_language_version;
    $record.^set_delegate: $delegate;
    $record.^set_fields: $pair;
    $record.^set_parameters: :$structural;
    $record.^add_role: $delegate;
    $record.^compose
}
multi sub circumfix:<{@ @}>(+pairs where pairs.all ~~ Pair:D, Str:_ :$name, Bool:D :$structural = False --> Mu) is export {
    my Mu $record   := MetamodelX::RecordHOW.new_type: :$name;
    my Mu $delegate := Data::Record::Map.^parameterize: :$structural;
    $record.^set_language_version;
    $record.^set_delegate: $delegate;
    $record.^set_fields: pairs;
    $record.^set_parameters: :$structural;
    $record.^add_role: $delegate;
    $record.^compose
}
multi sub circumfix:<{@ @}>(Block:D $block is raw, Str:_ :$name, Bool:D :$structural = False --> Mu) is export {
    MetamodelX::RecordTemplateHOW.new_type:
        Data::Record::Map, $block, :$name, :$structural
}
multi sub circumfix:<{@ @}>(%not-a-block-wtf, Str:_ :$name, Bool:_ :$structural --> Mu) is export {
    X::Data::Record::Block.new(type => Data::Record::Map).throw
}

multi sub infix:«(><)»(Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs
}
multi sub infix:«(><)»(Data::Record::Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs.record
}
multi sub infix:«(><)»(Data::Record::Map:U $lhs is raw, Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs
}
multi sub infix:«(><)»(Data::Record::Map:U $lhs is raw, Data::Record::Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs.record
}

multi sub infix:«(<<)»(Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs, :consume
}
multi sub infix:«(<<)»(Data::Record::Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs.record, :consume
}
multi sub infix:«(<<)»(Data::Record::Map:U $lhs is raw, Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs, :subsume
}
multi sub infix:«(<<)»(Data::Record::Map:U $lhs is raw, Data::Record::Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs.record, :subsume
}

multi sub infix:«(>>)»(Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs, :subsume
}
multi sub infix:«(>>)»(Data::Record::Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs.record, :subsume
}
multi sub infix:«(>>)»(Data::Record::Map:U $lhs is raw, Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs, :consume
}
multi sub infix:«(>>)»(Data::Record::Map:U $lhs is raw, Data::Record::Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs.record, :consume
}

multi sub infix:«(<>)»(Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs, :coerce
}
multi sub infix:«(<>)»(Data::Record::Map:D $lhs is raw, Data::Record::Map:U $rhs is raw --> Data::Record::Map:D) is export {
    $rhs.new: $lhs.record, :coerce
}
multi sub infix:«(<>)»(Data::Record::Map:U $lhs is raw, Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs, :coerce
}
multi sub infix:«(<>)»(Data::Record::Map:U $lhs is raw, Data::Record::Map:D $rhs is raw --> Data::Record::Map:D) is export {
    $lhs.new: $rhs.record, :coerce
}

multi sub infix:<eqv>(Map:D $lhs is raw, Data::Record::Map:D $rhs is raw --> Bool:D) is export {
    $lhs eqv $rhs.unrecord
}
multi sub infix:<eqv>(Data::Record::Map:D $lhs is raw, Map:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs
}
multi sub infix:<eqv>(Data::Record::Map:D $lhs is raw, Data::Record::Map:D $rhs is raw --> Bool:D) is export {
    $lhs.unrecord eqv $rhs.unrecord
}
