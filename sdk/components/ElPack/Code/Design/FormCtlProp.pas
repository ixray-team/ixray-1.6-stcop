
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit FormCtlProp;

interface

uses

{$ifdef VCL_6_USED}
  DesignIntf, DesignEditors, DesignWindows, DsnConst,  
{$else}
  DsgnIntf, 
{$endif}
  ToolIntf, EditIntf, ExptIntf, Windows, Dialogs, TypInfo, Classes, SysUtils,
  Consts, Forms;

type
  TFormCtlProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc : TGetStrProc); override;
    procedure SetValue(const Value : string); override;
  end;

type
  TFormProperty = class(TEnumProperty)
  private
    List : TStringList;
    FormName,
    FileName : String;
{$IFDEF VCL_4_USED}
    procedure EnumProc(const FileName, UnitName, FormName, DesignClass: string; CoClasses: TStrings);
    procedure FNProc(const FileName, UnitName, FormName, DesignClass: string; CoClasses: TStrings);
{$ENDIF}
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

implementation

procedure TFormCtlProperty.GetValues(Proc : TGetStrProc);
begin
  inherited;
  if (Designer.Form is GetTypeData(GetPropType)^.ClassType) and
    (Designer.Form.Name <> '') then Proc(Designer.Form.Name);
end;

procedure TFormCtlProperty.SetValue(const Value : string);
var
  Comp : TComponent;
begin
  Comp := Designer.GetComponent(Value);
  if ((Comp = nil) or not (Comp is GetTypeData(GetPropType)^.ClassType))
    and (CompareText(Designer.Form.Name, Value) = 0) then
  begin
    if not (Designer.Form is GetTypeData(GetPropType)^.ClassType) then
    begin
      MessageDlg(Format('Invalid property value: %s expected, %s found',
                        [Designer.Form.ClassName, GetTypeData(GetPropType)^.ClassType.ClassName]),
                 mtError, [mbOk], 0);
      raise
        EPropertyError.Create(SInvalidPropertyValue);
    end;
    SetOrdValue(Longint(Designer.Form));
  end
  else
    inherited;
end;

function TFormProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TFormProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

{$IFDEF VCL_4_USED}
procedure TFormProperty.EnumProc(const FileName, UnitName, FormName, DesignClass: string; CoClasses: TStrings);
begin
  if FormName <> '' then
     List.Add(FormName);
end;

procedure TFormProperty.FNProc(const FileName, UnitName, FormName, DesignClass: string; CoClasses: TStrings);
begin
  if Self.FormName = FormName then
  begin
    Self.FileName := FormName;
  end;
end;
{$ENDIF}

procedure TFormProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  List := TStringList.Create;
  try
    {$IFDEF VCL_4_USED}
    Designer.GetProjectModules(EnumProc);
    {$ENDIF}
    for i := 0 to List.Count - 1 do Proc(List[i]);
  finally
    List.Free;
  end;
end;

procedure TFormProperty.SetValue(const Value: string);
begin
  if Value = '' then SetStrValue('') else
  begin
    FormName := Value;
    FileName := '';
    {$IFDEF VCL_4_USED}
    Designer.GetProjectModules(FNProc);
    SetStrValue(FileName);
    {$ELSE}
    SetStrValue(FormName);
    {$ENDIF}
  end;
end;

end.
