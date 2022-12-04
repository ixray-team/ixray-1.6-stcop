{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2001, EldoS                   }
{                                                    }
{====================================================}

{$I 'ElPack.inc'}
{$R 'Design\ElDBCtrls.dcr'}
{$R 'Design\ElDBLookupCtrls.dcr'}
{.$R 'Design\ElDBTree.dcr'}

unit ElDBReg;

interface

uses TypInfo,
{$ifdef VCL_6_USED}
     DesignEditors, 
     DesignWindows, 
     DsnConst, 
     DesignIntf,
{$else}
     DsgnIntf,
{$endif}
     Classes,
     Db,
     ElDBNavDsgn,
     // ElDBTree,
{$ifdef VCL_4_USED}
     ElDBLookupCtrls,
{$ifdef ELPACK_UNICODE}
     ElDBWideLookupControls,
{$endif}
{$endif}
     ElPropTools,
     ElDBBoxes,
     ElDBBtnEdit,
     ElDBCtrls,
     ElDBCurrEdit,
     ElDBDTPick,
     ElDBHTMLView,
     ElDBLbl,
     ElDBSpin;

procedure Register;

implementation

type

  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    procedure GetValueList(List : TStrings); virtual; abstract;
    procedure GetValues(Proc : TGetStrProc); override;
  end;

  TDataFieldProperty = class(TDBStringProperty)
  public
    function GetDataSourcePropName : string; virtual;
    procedure GetValueList(List : TStrings); override;
  end;

function TDBStringProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValues(Proc : TGetStrProc);
var
  I : Integer;
  Values : TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

function TDataFieldProperty.GetDataSourcePropName : string;
begin
  Result := 'DataSource';
end;

procedure TDataFieldProperty.GetValueList(List : TStrings);
var
  PPI : PPropInfo;
  DataSource : TDataSource;

  function GetObjectProp(Instance: TObject; PropInfo: PPropInfo): TObject;
  begin
    Result := TObject(GetOrdProp(Instance, PropInfo));
  end;

begin
  PPI := GetPropertyRecord(GetComponent(0), GetDataSourcePropName);
  if PPI <> nil then
  begin
    DataSource := GetObjectProp(GetComponent(0), PPI) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
    Dispose(PPI);
  end;
end;

procedure Register;
begin
  RegisterComponents('EldoS DB',
  [{TElDataTree, }TElDBListBox, TElDBComboBox, TElDBEdit, TElDBMemo, TElDBCheckBox, TElDBRadioGroup,
   TElDBCurrencyEdit, TElDBDateTimePicker, TElDBSpinEdit, TElDBFloatSpinEdit, TElDBLabel, TElDBHTMLView,
   TElDBButtonEdit, TElDBNavigator
   {$ifdef VCL_4_USED}
{$ifdef ELPACK_UNICODE}
   , TElWideDBEdit, TElWideDBMemo, TElWideDBLookupListBox, TElWideDBLookupComboBox
{$endif}
   , TElDBLookupListBox, TElDBLookupComboBox
   {$endif}
  ]);

   RegisterNoIcon([TElDBNavButton]);
   Classes.RegisterClass(TElDBNavButton);

  RegisterPropertyEditor(TypeInfo(string), TElDBEdit,    'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBMemo,    'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBLabel,   'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBListBox, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBSpinEdit, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBComboBox, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBHTMLView, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBCheckBox, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBButtonEdit, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBRadioGroup, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBCurrencyEdit, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBFloatSpinEdit, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBDateTimePicker, 'DataField', TDataFieldProperty);
{$ifdef VCL_4_USED}
{$ifdef ELPACK_UNICODE}
  RegisterPropertyEditor(TypeInfo(string), TElWideDBEdit, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElWideDBMemo, 'DataField', TDataFieldProperty);
{$endif}
  RegisterPropertyEditor(TypeInfo(string), TElDBLookupListBox, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBLookupListBox, 'ListField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBLookupListBox, 'KeyField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBLookupComboBox, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBLookupComboBox, 'ListField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElDBLookupComboBox, 'KeyField', TDataFieldProperty);

{$ifdef ELPACK_UNICODE}
  RegisterPropertyEditor(TypeInfo(string), TElWideDBLookupListBox, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElWideDBLookupListBox, 'ListField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElWideDBLookupListBox, 'KeyField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElWideDBLookupComboBox, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElWideDBLookupComboBox, 'ListField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TElWideDBLookupComboBox, 'KeyField', TDataFieldProperty);
{$endif}
{$endif}
  RegisterComponentEditor(TElDBNavigator, TElDBNavigatorEditor);
end;

end.
