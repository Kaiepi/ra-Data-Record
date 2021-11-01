use v6.d;
use MetamodelX::RecorderHOW;
my atomicint $next_id = 1;
unit role MetamodelX::RecordTemplateHOW[::T]
     does Metamodel::Naming;

my $coercer := Metamodel::CoercionHOW.new_type: T, Mu;

has Metamodel::Archetypes:D $!archetypes is required;
has int                     $!id;
has Mu                      $!delegate   is required;
has Block:D                 $!body_block is required;

my Metamodel::Archetypes:D constant N-ARCHETYPES .= new: :nominal, :parametric;
my Metamodel::Archetypes:D constant G-ARCHETYPES .= new: :nominal, :parametric, :generic;
submethod BUILD(::?CLASS:D: int :$id, Mu :$delegate! is raw, Block:D :$body_block! is raw --> Nil) {
    $!id          = $id;
    $!delegate   := $delegate;
    $!body_block := $body_block;
    $!archetypes :=
        $delegate.HOW.archetypes.generic || $body_block.is_generic
          ?? G-ARCHETYPES
          !! N-ARCHETYPES;
}

proto method archetypes(--> Metamodel::Archetypes:D)  {*}
multi method archetypes(::?CLASS:U: --> N-ARCHETYPES) { }
multi method archetypes(::?CLASS:D:)                  { $!archetypes }

method new_type(::?CLASS:_: Mu $delegate is raw, Block:D $body_block is raw, Str :$name --> Mu) {
    my int $id   = !$name.DEFINITE && $next_id⚛++;
    my Mu  $how := self.bless: :$id, :$delegate, :$body_block;
    my Mu  $obj := Metamodel::Primitives.create_type: $how, 'Uninstantiable';
    $how.set_name: $obj, $id && "<anon record template $id>" || $name;
    Metamodel::Primitives.configure_type_checking: $obj, (), :!authoritative, :call_accepts;
    Metamodel::Primitives.set_parameterizer($obj, &RECORD-PARAMETERIZER);
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

method delegate(::?CLASS:D: Mu) { $!delegate }

method body_block(::?CLASS:D: Mu --> Block:D) { $!body_block }

method is_anonymous(::?CLASS:D: Mu --> Bool:D) { ?$!id }

method parameterize(::?CLASS:D: Mu $obj is raw, |args) {
    my $encoded := IterationBuffer.new;
    $encoded.push: %(args) || Nil;
    $encoded.push: $_ for @(args);
    Metamodel::Primitives.parameterize_type: $obj, $encoded.Slip
}
sub RECORD-PARAMETERIZER(Mu $obj is raw, @encoded --> Mu) {
    $obj.HOW!do_parameterization: $obj, @encoded
}
method !do_parameterization(Mu $template is raw, @encoded --> Mu) {
    my @args   := (@encoded.head andthen |*), |@encoded.skip;
    my $fields := $coercer.^coerce: $!body_block(|@args);
    my $name   := self.name($template) ~ '[' ~ @args.map(*.raku).join(', ') ~ ']';
    MetamodelX::RecorderHOW[T].new_type($!delegate, $fields, :$name, :$template).^compose
}

method is_generic(::?CLASS:D: Mu --> int) {
    $!delegate.HOW.archetypes.generic || $!body_block.is_generic
}

method instantiate_generic(::?CLASS:D: Mu $obj is raw, Mu $type_env is raw --> Mu) {
    my Str:D    $name        = self.name: $obj;
    my Mu       $delegate   := $!delegate;
    my Block:D  $body_block := $!body_block;
    $delegate   := $delegate.^instantiate_generic: $type_env if $delegate.^is_generic;
    $body_block := $body_block.instantiate_generic: $type_env if $body_block.is_generic;
    self.new_type: $delegate, $body_block, :$name
}

method type_check(::?CLASS:D: Mu $obj is raw is copy, Mu $checkee is raw is copy --> int) {
    use nqp;
    nqp::eqaddr(($checkee := nqp::decont($checkee)), ($obj := nqp::decont($obj))) # Is it our identity?
      || nqp::istype_nd($!delegate, $checkee) # Is it like our delegate?
      || nqp::istype_nd($checkee.HOW, MetamodelX::RecorderHOW[T]) # Is it a parameterization of ours?
        && nqp::eqaddr($checkee.^template, $obj)
}

method accepts_type(::?CLASS:D: Mu $obj is raw, Mu $chcekee is raw --> int) {
    use nqp;
    nqp::istype_nd($chcekee.HOW, MetamodelX::RecorderHOW[T])
      && nqp::eqaddr($chcekee.^template, nqp::decont($obj))
}
