use v6;
unit role MetamodelX::RecorderHOW[::T]
     does Metamodel::Naming
     does Metamodel::REPRComposeProtocol;

has int    $!id          is built;
has Mu     $!delegate    is built(:bind) is required;
has Mu     $!fields      is built(:bind) is required;
has Mu     $!template    is built(:bind);
has Bool:D $!is_composed = False;

my constant ARCHETYPES = Metamodel::Archetypes.new: :nominal;
method archetypes(::?ROLE:_: --> ARCHETYPES) { }

my atomicint $next_id = 1;
method new_type(::?ROLE:_:
    Mu $delegate is raw, Mu $fields is raw,
    Str :$name, Str :$repr, Mu :$template is raw
) {
    my int $id   = !$name.DEFINITE && $next_id⚛++;
    my Mu  $how := self.new: :$id, :$delegate, :fields($fields), :$template;
    my Mu  $obj := Metamodel::Primitives.create_type: $how, $repr // 'P6opaque';
    $how.set_name: $obj, $id ?? "<anon record $id>" !! $name<>;
    $obj
}

method get_attribute_for_usage(::?ROLE:D: $, |meta) { $!delegate.^get_attribute_for_usage: |meta }

method attributes(::?ROLE:D: $, |meta) { $!delegate.^attributes: |meta }

method find_method(::?ROLE:D: $, |meta) { $!delegate.^find_method: |meta }

method lookup(::?ROLE:D: $, |meta) { $!delegate.^lookup: |meta }

method methods(::?ROLE:D: $, |meta) { $!delegate.^methods: |meta }

method roles_to_compose(::?ROLE:D: $, |meta) { $!delegate.^roles_to_compose: |meta }

method roles(::?ROLE:D: $, |meta) { $!delegate.^roles: |meta }

method parents(::?ROLE:D: $, |meta) { $!delegate.^parents: |meta }

method mro(::?ROLE:D: $, |meta) is raw {
    my $mro := IterationBuffer.new;
    $mro.push: $_ for $!delegate.^mro: |meta;
    $mro
}

method compose(::?ROLE:D: Mu $obj is raw) {
    my $is_composed := cas $!is_composed, False, True;
    self.compose_repr: $obj unless $is_composed;
    $obj
}

#|[ An ID given to anonymous records. ]
method id(::?ROLE:D: $ --> int) { $!id }

#|[ A class to which to delegate method calls. ]
method delegate(::?ROLE:D: $) { $!delegate }

#|[ A collection of fields for the record. ]
method fields(::?ROLE:D: $) { $!fields }
#=[ Its type is coupled to the delegate; if a Data::Record::Instance, that's also its type parameter. ]

#|[ An optional record template. ]
method template(::?ROLE:D: $) { $!template }

#|[ Whether or not this is an anonymous record. ]
method is_anonymous(::?ROLE:D: $ --> Bool:D) { ?$!id }

#|[ Whether or not this record has been composed. ]
method is_composed(::?ROLE:D: $) { $!is_composed }

method type_check(::?ROLE:D: Mu $obj is raw, Mu $checkee is raw is copy --> int) {
    use nqp;
    nqp::eqaddr(($checkee := nqp::decont($checkee)), nqp::decont($obj)) # Is it our identity?
      || nqp::istype_nd($!delegate, $checkee) # Is it like our delegate?
}
