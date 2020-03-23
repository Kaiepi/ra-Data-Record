use v6.e.PREVIEW;
unit class MetamodelX::RecordHOW is Metamodel::ClassHOW;

has Mu $!template;
has Mu $!delegate;
has    @!fields;
has    %!parameters;

method new_type(::?CLASS:_: Str:_ :$name, |args) {
    our Str:D constant ANON_NAME = '<anon record>';
    callwith name => $name // ANON_NAME, |args
}

method template(::?CLASS:D: Mu --> Mu) { $!template }

method set_template(::?CLASS:D: Mu $obj is raw, Mu $template is raw --> Mu) {
    die if self.is_composed: $obj;
    $!template := $template
}

method delegate(::?CLASS:D: Mu --> Mu) { $!delegate }

method set_delegate(::?CLASS:D: Mu $obj is raw, Mu $delegate is raw --> Mu) {
    die if self.is_composed: $obj;
    $!delegate := $delegate
}

method fields(::?CLASS:D: Mu --> List:D) { @!fields }

method set_fields(::?CLASS:D: Mu $obj is raw, +fields --> List:D) {
    die if self.is_composed: $obj;
    @!fields := fields
}

method parameters(::?CLASS:D: Mu --> Map:D) { %!parameters }

method set_parameters(::?CLASS:D: Mu $obj is raw, *%parameters --> Map:D) {
    die if self.is_composed: $obj;;
    %!parameters := %parameters.Map
}
