use v6.d;
use annotations::containers;
use annotations::how;
my atomicint $ID = 1;
unit role MetamodelX::RecorderHOW[::F, ::D]
     does MetamodelX::AnnotationHOW[Buffer, Metamodel::ClassHOW];

#|[ The type of field. ]
method for(::?ROLE:_: Mu $?) is raw { F }

#|[ A class to which to delegate method calls. ]
method delegate(::?ROLE:_: Mu $?) is raw { D }

method new_type(::?ROLE:_: F(Mu) $fields is raw, D $template? is raw, :$name, *%rest) is raw {
    use nqp;
    my uint $id = !$name.DEFINITE && $ID⚛++;
    my $obj := callwith :name($id ?? "<anon record $id>" !! $name), |%rest;
    my $how := $obj.HOW;
    $how.yield_annotations($obj) = $id, $template, $fields;
    $how.add_parent: $obj, $template;
    nqp::settypecheckmode($obj, nqp::const::TYPE_CHECK_NEEDS_ACCEPTS)
}

#|[ A number of annotations we promise to keep via this specific HOW. ]
method annotations(::?ROLE:_: $? --> 3) { }
#=[ MROish; typically called with .* dispatch. ]

#|[ A position for a list of annotations for this metaobject. ]
method annotation_offset(::?ROLE:_: Mu $obj? is raw --> Int:D) {
    self.*annotations($obj).skip.sum
}
#=[ Depending on this in a subtype requires an annotations count to skip. ]

#|[ The fields defining this record type. ]
method fields(::?ROLE:D: Mu $obj is raw) is raw {
    self.yield_annotations($obj)[2]<>
}

#|[ An origin for this record; should be the delegate by default. ]
method template(::?ROLE:D: Mu $obj is raw) is raw {
    self.yield_annotations($obj)[1]<>
}

#|[ An ID given to anonymous records. ]
method anonymous_id(::?ROLE:D: Mu $obj is raw --> uint) {
    self.yield_annotations($obj)[0]<>
}

#|[ Whether or not this is an anonymous record. ]
method is_anonymous(::?ROLE:D: Mu $obj? is raw --> Bool:D) {
    ?self.anonymous_id($obj)
}

method accepts_type(::?ROLE:D: Mu $obj is raw, Mu $checkee is raw --> int) {
    Metamodel::Primitives.is_type($checkee, D) # Is it like our delegate?
}
