use v6.d;
use Data::Record::Exceptions;
use MetamodelX::RecordHOW;
use MetamodelX::RecorderHOW;
use MetamodelX::RecordTemplateHOW;
use Test;

plan 2;

subtest 'MetamodelX::RecorderHOW', {
    plan 10;

    my class Instance { method fields { self.^fields } }

    my Mu \Named := Mu;
    my Mu \Anon  := Mu;
    lives-ok {
        Anon := MetamodelX::RecorderHOW[List, Instance].new_type: Empty;
    }, 'can create a recorder';
    lives-ok {
        Named := MetamodelX::RecorderHOW[List, Instance].new_type: Empty, :name<Named>;
    }, 'can create a named recorder';

    is Anon.^name, '<anon record 1>', 'anonymous records generate a name';
    is Named.^name, 'Named', 'named records have theirs';

    ok Anon.^is_anonymous, 'anonymous records are anonymous';
    nok Named.^is_anonymous, 'named records are not anonymous';

    lives-ok {
        Named ~~ -> ::Named $named { $named ~~ -> Named { $named } }
    }, 'records know their identity';
    lives-ok {
        Named ~~ -> Instance { }
    }, 'records can delegate typechecks';
    lives-ok {
        cmp-ok Named.fields, &[=:=], Named.^fields, 'can invoke metamethods on self from a method';
    }, 'can invoke methods on a record via its delegate';
};

subtest 'MetamodelX::RecordTemplateHOW', {
    plan 8;

    my class Instance { method method() { } }

    my class Protocol does MetamodelX::RecorderHOW[List, Instance] { }

    my Mu \RecordTemplate  = Mu;
    my    &body_block     := { $_ };
    lives-ok {
        RecordTemplate :=
            MetamodelX::RecordTemplateHOW[Protocol].new_type: &body_block, :name<Record>;
    }, 'can create new record template types';

    ok RecordTemplate.HOW.find_method(RecordTemplate, 'method'),
      'can find methods on record templates, which are those of their delegate';

    my Mu \Record := Mu;
    lives-ok {
        Record := RecordTemplate.^parameterize: 1;
    }, 'can parameterize record template types';

    cmp-ok Record.^template, &[=:=], RecordTemplate,
      'parameterizations have the correct template';
    cmp-ok Record.^delegate, &[=:=], RecordTemplate.^delegate,
      'parameterizations have the correct delegate';
    cmp-ok Record.^fields, &[eqv], (1,),
      'parameterizations have the correct fields';

    ok Metamodel::Primitives.is_type(Record, RecordTemplate),
      'parameterizations typecheck as their template';
    ok Metamodel::Primitives.is_type(Record, RecordTemplate.^delegate),
      'parameterizations typecheck as their delegate';
};

# vim: ft=raku sw=4 ts=4 sts=4 et
