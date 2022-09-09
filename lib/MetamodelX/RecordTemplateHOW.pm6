use v6.d;
use annotations::containers;
use annotations::how;
use MetamodelX::RecordHOW;
my atomicint $ID = 1;
unit role MetamodelX::RecordTemplateHOW[MetamodelX::RecordHOW ::P]
     does MetamodelX::AnnotationHOW[Buffer, Metamodel::ClassHOW];

my \F = P.for;
my \D = P.delegate;

#|[ The type of field. ]
method for(::?ROLE:_: Mu $?) { F }

#|[ A class to which to delegate method calls. ]
method delegate(::?ROLE:_: Mu $?) { D }

#|[ A HOW with which to produce a record type. ]
method recorder(::?ROLE:_: Mu $?) { P }

my constant N-ARCHETYPES = Metamodel::Archetypes.new: :nominal, :parametric, :inheritable, :augmentable;
my constant G-ARCHETYPES = Metamodel::Archetypes.new: :nominal, :parametric, :generic, :inheritable, :augmentable;

proto method archetypes(::?ROLE:_: $? --> Metamodel::Archetypes:D) {*}
multi method archetypes(::?ROLE:U: $? --> N-ARCHETYPES) { }
multi method archetypes(::?ROLE:D: $?) {
    self.ANN[0]<>
}

method new_type(::?ROLE:_: Block:D $body_block is raw, Str :$name, *%rest) {
    my uint $id = !$name.DEFINITE && $ID⚛++;
    my $obj := callwith :name($id ?? "<anon record template $id>" !! $name), |%rest;
    my $how := $obj.HOW;
    my $archetypes := P.archetypes.generic || $body_block.is_generic ?? G-ARCHETYPES !! N-ARCHETYPES;
    $how.ANN = $archetypes, $id, $body_block;
    $how.add_parent: $obj, D;
    Metamodel::Primitives.set_parameterizer($obj, &RECORD-PARAMETERIZER);
    $obj
}

#|[ A number of annotations we promise to keep via this specific parametric HOW. ]
method higher_annotations(::?ROLE:_: $? --> 3) { }
#=[ This is separate from MetamodelX::RecordHOW's annotations because those
    types have the delegate as a parent ordinarily. ]

#|[ A position for a list of higher annotations for this metaobject. ]
method higher_annotation_offset(::?ROLE:_: Mu $obj? is raw --> Int:D) {
    self.*higher_annotations($obj).skip.sum
}
#=[ Depending on this in a subtype requires a higher annotations offset to
    skip. ]

#|[ A block accepting type arguments, returning a record type's fields. ]
method body_block(::?ROLE:D: Mu $obj is raw --> Block:D) {
    self.ANN[2]<>
}
#=[ Completes the record template, allowing it to produce a true record type. ] 

#|[ An ID given to anonymous record templates. ]
method anonymous_id(::?ROLE:D: Mu $obj is raw --> uint) {
    self.ANN[1]<>
}

#|[ Whether or not this is an anonymous record template. ]
method is_anonymous(::?ROLE:D: Mu $obj is raw --> Bool:D) {
    ?self.anonymous_id: $obj
}

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
    $how.ANN = self.ANN.skip: self.*higher_annotations($template).sum;
    $how.compose: $obj
}

method instantiate_generic(::?ROLE:D: Mu $obj is raw, Mu $type_env is raw) {
    my $name       := self.name: $obj;
    my $delegate   := D;
    my $body_block := self.body_block: $obj;
    $delegate   := $delegate.^instantiate_generic: $type_env if $delegate.HOW.archetypes.generic;
    $body_block := $body_block.instantiate_generic: $type_env if $body_block.is_generic;
    self.new_type: $delegate, $body_block, :$name
}
