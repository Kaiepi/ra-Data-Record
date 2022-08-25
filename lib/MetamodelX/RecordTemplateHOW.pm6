use v6.d;
use annotations::containers;
use annotations::how;
use MetamodelX::RecorderHOW;
my atomicint $ID = 1;
unit role MetamodelX::RecordTemplateHOW[MetamodelX::RecorderHOW ::P]
     does MetamodelX::AnnotationHOW[Buffer, Metamodel::ClassHOW];

my constant N-ARCHETYPES = Metamodel::Archetypes.new: :nominal, :parametric;
my constant G-ARCHETYPES = Metamodel::Archetypes.new: :nominal, :parametric, :generic;

my \F = P.for;
my \D = P.delegate;

proto method archetypes(::?ROLE:_: $?, $? --> Metamodel::Archetypes:D) {*}
multi method archetypes(::?ROLE:U: --> N-ARCHETYPES) { }
multi method archetypes(::?ROLE:D: Mu $?, Metamodel::Archetypes:_ $archetypes?) { once $archetypes }

method for(::?ROLE:_: Mu $?) { F }

method delegate(::?ROLE:_: Mu $?) { D }

method recorder(::?ROLE:_: Mu $?) { P }

method new_type(::?ROLE:_: Block:D $body_block is raw, Str :$name, *%rest) {
    my uint $id = !$name.DEFINITE && $ID⚛++;
    my $obj := callwith :name($id ?? "<anon record template $id>" !! $name), |%rest;
    my $how := $obj.HOW;
    $how.archetypes: $obj, P.archetypes.generic || $body_block.is_generic ?? G-ARCHETYPES !! N-ARCHETYPES;
    $how.yield_annotations($obj) = $id, $body_block;
    $how.add_parent: $obj, D;
    Metamodel::Primitives.set_parameterizer($obj, &RECORD-PARAMETERIZER);
    $obj
}

method publish_type_cache(::?ROLE:D: Mu $obj is raw) is raw {
    Metamodel::Primitives.configure_type_checking: $obj, self.mro($obj).map({ slip $_, |$_.^role_typecheck_list })
}

method body_block(::?ROLE:D: Mu $obj is raw --> Block:D) { self.yield_annotations($obj)[1]<> }

method anonymous_id(::?ROLE:D: Mu $obj is raw --> int) { self.yield_annotations($obj)[0]<> }

method is_anonymous(::?ROLE:D: Mu $obj is raw --> Bool:D) { ?self.anonymous_id: $obj }

method parameterize(::?ROLE:D: Mu $obj is raw, |args) is raw {
    my $encoded := IterationBuffer.new;
    $encoded.push: %(args) || my constant EMPTY = Map.new;
    $encoded.push: $_ for @(args);
    Metamodel::Primitives.parameterize_type: $obj, $encoded.List
}
sub RECORD-PARAMETERIZER(Mu $obj is raw, @encoded) is raw {
    $obj.HOW!do_parameterization: $obj, @encoded
}
method !do_parameterization(Mu $template is raw, @encoded) is raw {
    my $args := Capture.new: :list(@encoded.skip), :hash(@encoded.head);
    my $name := self.name($template) ~ '[' ~ @$args.map(*.raku).join(', ') ~ ']';
    my $obj  := P.new_type: self.body_block($template)(|$args), $template, :$name;
    my $how  := $obj.HOW;
    $how.yield_annotations($obj) = self.yield_annotations($template).skip(2);
    $how.compose: $obj
}

method is_generic(::?ROLE:D: Mu $obj is raw --> int) {
    P.archetypes.generic || self.body_block($obj).is_generic
}

method instantiate_generic(::?ROLE:D: Mu $obj is raw, Mu $type_env is raw) {
    my $name       := self.name: $obj;
    my $delegate   := D;
    my $body_block := self.body_block: $obj;
    $delegate   := $delegate.^instantiate_generic: $type_env if $delegate.^is_generic;
    $body_block := $body_block.instantiate_generic: $type_env if $body_block.is_generic;
    self.new_type: $delegate, $body_block, :$name
}
