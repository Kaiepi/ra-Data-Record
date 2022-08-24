use v6.d;
use Data::Record::Exceptions;
use MetamodelX::RecordHOW;
use MetamodelX::RecorderHOW;
use MetamodelX::RecordTemplateHOW;
use Test;

plan 2;

my class Instance { method method() { } }

my class Recorder does MetamodelX::RecorderHOW[List:D, Instance] { }

subtest 'MetamodelX::RecorderHOW', {
    plan 9;

    only term:<Anon> {
        once MetamodelX::RecorderHOW[List, Instance].new_type(Empty).^compose
    }
    only term:<Named> {
        once MetamodelX::RecorderHOW[List, Instance].new_type(Empty, :name<Named>).^compose
    }

    lives-ok { Anon }, 'can create a recorder';
    lives-ok { Named }, 'can create a named recorder';
    lives-ok { Named.method }, 'can invoke methods';

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
};

subtest 'MetamodelX::RecordTemplateHOW', {
    plan 8;

    my &body_block := { $_ };

    only term:<RecordTemplate> {
        once MetamodelX::RecordTemplateHOW[Recorder].new_type(&body_block, :name<Record>).^compose
    }
    only term:<Record> {
        once RecordTemplate.^parameterize(1)
    }

    lives-ok { RecordTemplate }, 'can create new record template types';
    lives-ok { RecordTemplate.method }, 'can invoke methods on record template types';
    lives-ok { Record }, 'can parameterize record template types';

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
