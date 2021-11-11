use v6.d;
use MetamodelX::RecorderHOW;
my atomicint $next_id = 1;
unit role MetamodelX::RecordTemplateHOW[MetamodelX::RecorderHOW ::P]
     does Metamodel::Naming;

my \F = P.for;
my \D = P.delegate;

my $coercer := Metamodel::CoercionHOW.new_type: F, Mu;

has Metamodel::Archetypes:D $!archetypes is required;
has int                     $!id;
has Block:D                 $!body_block is required;

my Metamodel::Archetypes:D constant N-ARCHETYPES .= new: :nominal, :parametric;
my Metamodel::Archetypes:D constant G-ARCHETYPES .= new: :nominal, :parametric, :generic;
submethod BUILD(::?ROLE:D: int :$id, Block:D :$body_block! is raw --> Nil) {
    $!id          = $id;
    $!body_block := $body_block;
    $!archetypes := P.archetypes.generic || $body_block.is_generic ?? G-ARCHETYPES !! N-ARCHETYPES;
}

proto method archetypes(--> Metamodel::Archetypes:D) {*}
multi method archetypes(::?ROLE:U: --> N-ARCHETYPES) { }
multi method archetypes(::?ROLE:D:)                  { $!archetypes }

method for(::?ROLE:_: Mu $?) { F }

method delegate(::?ROLE:_: Mu $?) { D }

method recorder(::?ROLE:_: Mu $?) { P }

method new_type(::?ROLE:_: Block:D $body_block is raw, Str :$name, *%rest) {
    my int $id   = !$name.DEFINITE && $next_id⚛++;
    my Mu  $how := self.bless: |%rest, :$id, :$body_block;
    my Mu  $obj := Metamodel::Primitives.create_type: $how, 'Uninstantiable';
    $how.set_name: $obj, $id && "<anon record template $id>" || $name;
    Metamodel::Primitives.configure_type_checking: $obj, (), :!authoritative, :call_accepts;
    Metamodel::Primitives.set_parameterizer($obj, &RECORD-PARAMETERIZER);
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

method mro(::?ROLE:D: Mu, |meta) {
    my $mro := IterationBuffer.new;
    $mro.push: $_ for D.^mro: |meta;
    $mro
}

method body_block(::?ROLE:D: Mu --> Block:D) { $!body_block }

method is_anonymous(::?ROLE:D: Mu --> Bool:D) { ?$!id }

method parameterize(::?ROLE:D: Mu $obj is raw, |args) {
    my $encoded := IterationBuffer.new;
    $encoded.push: %(args) || Nil;
    $encoded.push: $_ for @(args);
    Metamodel::Primitives.parameterize_type: $obj, $encoded.Slip
}
sub RECORD-PARAMETERIZER(Mu $obj is raw, @encoded) {
    $obj.HOW!do_parameterization: $obj, @encoded
}
method !do_parameterization(Mu $template is raw, @encoded) {
    my @args   := (@encoded.head andthen |*), |@encoded.skip;
    my $fields := $coercer.^coerce: $!body_block(|@args);
    my $name   := self.name($template) ~ '[' ~ @args.map(*.raku).join(', ') ~ ']';
    P.new_type($fields, :$name, :$template).^compose
}

method is_generic(::?ROLE:D: Mu --> int) {
    P.archetypes.generic || $!body_block.is_generic
}

method instantiate_generic(::?ROLE:D: Mu $obj is raw, Mu $type_env is raw) {
    my Str:D    $name        = self.name: $obj;
    my Mu       $delegate   := D;
    my Block:D  $body_block := $!body_block;
    $delegate   := $delegate.^instantiate_generic: $type_env if $delegate.^is_generic;
    $body_block := $body_block.instantiate_generic: $type_env if $body_block.is_generic;
    self.new_type: $delegate, $body_block, :$name
}

method type_check(::?ROLE:D: Mu $obj is raw is copy, Mu $checkee is raw is copy --> int) {
    use nqp;
    nqp::eqaddr(($checkee := nqp::decont($checkee)), ($obj := nqp::decont($obj))) # Is it our identity?
      || nqp::istype_nd(D, $checkee) # Is it like our delegate?
      || nqp::istype_nd($checkee.HOW, P) # Is it a parameterization of ours?
        && nqp::istype($obj, $checkee.^template)
}

method accepts_type(::?ROLE:D: Mu $obj is raw, Mu $checkee is raw --> int) {
    use nqp;
    nqp::istype_nd($checkee.HOW, P)
      && nqp::istype_nd($checkee.^template, $obj)
}
