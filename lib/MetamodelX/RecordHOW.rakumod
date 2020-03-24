use v6.d;
use Data::Record::Exceptions;
unit class MetamodelX::RecordHOW is Metamodel::ClassHOW;

has Mu $!template;
has Mu $!delegate;
has    @!fields;
has    %!parameters;

method new_type(::?CLASS:_: Str:_ :$name, |args --> Mu) {
    our Str:D constant ANON_NAME = '<anon record>';
    callwith name => $name // ANON_NAME, |args
}

method template(::?CLASS:D: Mu --> Mu) { $!template }

method set_template(::?CLASS:D: Mu $obj is raw, Mu $template is raw --> Mu) {
    if self.is_composed: $obj {
        die X::Data::Record::Composed.new:
            type      => $obj,
            operation => 'set the template for'
    } else {
        $!template := $template
    }
}

method delegate(::?CLASS:D: Mu --> Mu) { $!delegate }

method set_delegate(::?CLASS:D: Mu $obj is raw, Mu $delegate is raw --> Mu) {
    if self.is_composed: $obj {
        die X::Data::Record::Composed.new:
            type      => $obj,
            operation => 'set the delegate for'
    } else {
        $!delegate := $delegate
    }
}

method fields(::?CLASS:D: Mu --> List:D) { @!fields }

method set_fields(::?CLASS:D: Mu $obj is raw, +fields --> List:D) {
    if self.is_composed: $obj {
        die X::Data::Record::Composed.new:
            type      => $obj,
            operation => 'set the fields for'
    } else {
        @!fields := fields
    }
}

method parameters(::?CLASS:D: Mu --> Map:D) { %!parameters }

method set_parameters(::?CLASS:D: Mu $obj is raw, *%parameters --> Map:D) {
    if self.is_composed: $obj {
        die X::Data::Record::Composed.new:
            type      => $obj,
            operation => 'set the parameters for'
    } else {
        %!parameters := %parameters.Map
    }
}
