use v6.e.PREVIEW;
use Data::Record::Exceptions;
use MetamodelX::RecordHOW;
use MetamodelX::RecordTemplateHOW;
use Test;

plan 2;

subtest 'MetamodelX::RecordHOW', {
    plan 10;

    my Mu \Record = Mu;
    lives-ok {
        Record := MetamodelX::RecordHOW.new_type: :name<Record>
    }, 'can create new record types';
    lives-ok {
        Record.HOW.set_template: Record, Mu
    }, 'can set record templates before composition';
    lives-ok {
        Record.HOW.set_delegate: Record, role { }
    }, 'can set record delegates before composition';
    lives-ok {
        Record.HOW.set_fields: Record
    }, 'can set record fields before composition';
    lives-ok {
        Record.HOW.set_parameters: Record
    }, 'can set record parameters before composition';
    lives-ok {
        Record.HOW.compose: Record
    }, 'can compose record types';
    throws-like {
        Record.HOW.set_template: Record, Mu
    }, X::Data::Record::Composed,
      'cannot set record templates after composition';
    throws-like {
        Record.HOW.set_delegate: Record, role { }
    }, X::Data::Record::Composed,
      'cannot set record delegates after composition';
    throws-like {
        Record.HOW.set_fields: Record
    }, X::Data::Record::Composed,
      'cannot set record fields after composition';
    throws-like {
        Record.HOW.set_parameters: Record
    }, X::Data::Record::Composed,
      'cannot set record parameters after composition';
};

subtest 'MetamodelX::RecordTemplateHOW', {
    plan 9;

    my role Data::Record::Mock[+] {
        method method() { }
    }
    my role Data::Record::Mock[+, Bool:D :$param!] {
        method method() { }
    }

    my Mu \RecordTemplate  = Mu;
    my    &body_block     := { $_ };
    lives-ok {
        RecordTemplate := MetamodelX::RecordTemplateHOW.new_type:
            Data::Record::Mock, &body_block, :name<Record>, :param;
    }, 'can create new record template types';

    ok RecordTemplate.HOW.find_method(RecordTemplate, 'method'),
      'can find methods on record templates, which are those of their delegate';

    my Mu \Record = Mu;
    lives-ok {
        Record := RecordTemplate.^parameterize: 1;
    }, 'can parameterize record template types';

    cmp-ok Record.^template, &[=:=], RecordTemplate,
      'parameterizations have the correct template';
    cmp-ok Record.^delegate, &[=:=], RecordTemplate.^delegate,
      'parameterizations have the correct delegate';
    cmp-ok Record.^fields, &[eqv], (1,),
      'parameterizations have the correct fields';
    cmp-ok Record.^parameters, &[eqv], Map.new((:param)),
      'parameterizations have the correct parameters';

    ok Metamodel::Primitives.is_type(Record, RecordTemplate),
      'parameterizations typecheck as their template';
    ok Metamodel::Primitives.is_type(Record, RecordTemplate.^delegate),
      'parameterizations typecheck as their delegate';
};

# vim: ft=perl6 sw=4 ts=4 sts=4 et
