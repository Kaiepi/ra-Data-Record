use v6.d;
unit role MetamodelX::RecorderHOW[::F, ::D]
     does Metamodel::Naming
     does Metamodel::REPRComposeProtocol
     does Metamodel::BaseType;

has int    $!id          is built;
has Mu     $!template    is built(:bind);
has Bool:D $!is_composed = False;

my constant ARCHETYPES = Metamodel::Archetypes.new: :nominal;
method archetypes(::?ROLE:_: --> ARCHETYPES) { }

#|[ The type of field. ]
method for(::?ROLE:_: Mu $?) { F }

#|[ A class to which to delegate method calls. ]
method delegate(::?ROLE:_: Mu $?) { D }

# XXX TODO: We can't define $!fields here. In fact, outside of the individual
# collection HOWs, we shouldn't care about them at all in the MOP.
my atomicint $next_id = 1;
method new_type(::?ROLE:_: Mu $fields is raw, Str :$name, Mu :$template is raw, *%rest) {
    my int $id   = !$name.DEFINITE && $next_id⚛++;
    my Mu  $how := self.new: |%rest, :$fields, :$id, :$template;
    my Mu  $obj := Metamodel::Primitives.create_type: $how, D.REPR;
    Metamodel::Primitives.configure_type_checking: $obj, ($obj,), :!authoritative, :call_accepts;
    $how.set_name: $obj, $id ?? "<anon record $id>" !! $name<>;
    $obj
}

method get_attribute_for_usage(::?ROLE:D: Mu, |meta) { D.^get_attribute_for_usage: |meta }

method attributes(::?ROLE:D: Mu, |meta) { D.^attributes: |meta }

method find_method(::?ROLE:D: Mu, |meta) { D.^find_method: |meta }

method lookup(::?ROLE:D: Mu, |meta) { D.^lookup: |meta }

method methods(::?ROLE:D: Mu, |meta) { D.^methods: |meta }

method roles_to_compose(::?ROLE:D: Mu, |meta) { D.^roles_to_compose: |meta }

method roles(::?ROLE:D: Mu, |meta) { D.^roles: |meta }

method parents(::?ROLE:D: Mu, |meta) { D.^parents: |meta }

method mro(::?ROLE:D: Mu, |meta) is raw {
    my $mro := IterationBuffer.new;
    $mro.push: $_ for D.^mro: |meta;
    $mro
}

method compose(::?ROLE:D: Mu $obj is raw) {
    my $is_composed := cas $!is_composed, False, True;
    self.compose_repr: $obj unless $is_composed;
    $obj
}

#|[ An ID given to anonymous records. ]
method id(::?ROLE:D: Mu --> int) { $!id }

#|[ An optional record template. ]
method template(::?ROLE:D: Mu) { $!template }

#|[ Whether or not this is an anonymous record. ]
method is_anonymous(::?ROLE:D: Mu --> Bool:D) { ?$!id }

#|[ Whether or not this record has been composed. ]
method is_composed(::?ROLE:D: Mu) { $!is_composed }

method type_check(::?ROLE:D: Mu $obj is raw, Mu $checkee is raw is copy --> int) {
    use nqp;
    nqp::istype_nd(D, $checkee)
}

method accepts_type(::?ROLE:D: Mu $obj is raw, Mu $checkee is raw is copy --> int) {
    use nqp;
    nqp::istype_nd($checkee, D)
}
