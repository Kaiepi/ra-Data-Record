use v6.d;
use MetamodelX::RecordHOW;
unit class MetamodelX::RecordTemplateHOW does Metamodel::Naming;

has Mu      $!delegate   is required;
has Block:D $!body_block is required;
has         %!parameters is required;

has Metamodel::Archetypes:D $!archetypes is required;

submethod BUILD(::?CLASS:D: Mu :$delegate! is raw, Block:D :$body_block! is raw, :%parameters! --> Nil) {
    $!delegate   := $delegate.^parameterize: |%parameters;
    $!body_block := $body_block;
    %!parameters := %parameters.Map;

    my Metamodel::Archetypes:D constant N-ARCHETYPES .= new: :1nominal, :1parametric;
    my Metamodel::Archetypes:D constant G-ARCHETYPES .= new: :1nominal, :1parametric, :1generic;
    $!archetypes := $delegate.HOW.archetypes.generic || $body_block.is_generic
                 ?? G-ARCHETYPES
                 !! N-ARCHETYPES;
}

method new_type(::?CLASS:_: Mu $delegate is raw, Block:D $body_block is raw, Str:_ :$name, *%parameters --> Mu) {
    use nqp;
    our Str:D constant ANON_NAME = '<anon record>';

    my ::?CLASS:D $meta := self.bless: :$delegate, :$body_block, :%parameters;
    my Mu         $obj  := Metamodel::Primitives.create_type: $meta, 'Uninstantiable';
    $meta.set_name: $obj, $name // ANON_NAME;
    Metamodel::Primitives.configure_type_checking: $obj, (), :!authoritative, :call_accepts;
    nqp::setparameterizer($obj, &RECORD-PARAMETERIZER);
    $obj
}
sub RECORD-PARAMETERIZER(Mu $obj is raw, @args --> Mu) {
    $obj.HOW!do_parameterization: $obj, @args
}
method !do_parameterization(Mu $obj is raw, (@pos, %named) --> Mu) {
    my       $fields := $!body_block.(|@pos, |%named);
    my Str:D $name    = self.name: $obj;
    $name ~= [~] '[',
                 @pos.map(*.raku).join(', '),
                 (do ', ' if ?@pos && ?%named),
                 %named.map(*.raku).join(', '),
                 ']';

    my Mu $record := MetamodelX::RecordHOW.new_type: :$name;
    $record.^set_language_version;
    $record.^set_template: $obj;
    $record.^set_delegate: $!delegate;
    $record.^set_fields: $fields;
    $record.^set_parameters: |%!parameters;
    $record.^add_role: $!delegate;
    $record.^compose
}

method archetypes(::?CLASS:D: --> Metamodel::Archetypes:D) { $!archetypes }

method delegate(::?CLASS:D: Mu --> Mu) { $!delegate }

method body_block(::?CLASS:D: Mu --> Block:D) { $!body_block }

method parameters(::?CLASS:D: Mu --> Map:D) { %!parameters }

method parameterize(::?CLASS:D: Mu $obj is raw, |args --> Mu) {
    use nqp;
    my Mu $pos := nqp::list();
    nqp::push($pos, $_) for @(args);
    my Mu $named := nqp::hash();
    nqp::bindkey($named, nqp::unbox_s(.key), .value) for %(args);
    nqp::parameterizetype($obj, nqp::list($pos, $named))
}

method is_generic(::?CLASS:D: Mu --> int) {
    return 1 if $!delegate.HOW.archetypes.generic;
    return 1 if $!body_block.is_generic;
    0
}

method instantiate_generic(::?CLASS:D: Mu $obj is raw, Mu $type_env is raw --> Mu) {
    my Str:D    $name        = self.name: $obj;
    my Mu       $delegate   := $!delegate;
    my Block:D  $body_block := $!body_block;
    $delegate   := $delegate.^instantiate_generic: $type_env if $delegate.^is_generic;
    $body_block := $body_block.instantiate_generic: $type_env if $body_block.is_generic;
    self.new_type: $delegate, $body_block, :$name, |%!parameters
}

method find_method(::?CLASS:D: Mu, |args --> Mu) { $!delegate.^find_method: |args }

method methods(::?CLASS:D: Mu, |args --> Mu) { $!delegate.^methods: |args }

method accepts_type(::?CLASS:D: Mu $checkee is raw, Mu $checker is raw --> int) {
    my Mu $checkee-d := $checkee<>;
    my Mu $checker-d := $checker<>;
    # Are the LHS and RHS identical?
    return 1 if $checker-d =:= $checkee-d;
    # Is the RHS a parameterization of the LHS?
    return 1 if Metamodel::Primitives.is_type($checker-d.HOW, MetamodelX::RecordHOW)
             && $checker-d.^template =:= $checkee-d;
    # Fail the typecheck.
    0
}
