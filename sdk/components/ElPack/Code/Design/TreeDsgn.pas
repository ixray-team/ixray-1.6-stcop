{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit TreeDsgn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElTree, ElHeader, frmItemsProp, frmSectProp, 
{$ifdef VCL_6_USED}
  DesignIntf, DesignEditors, DesignWindows, DsnConst
{$else}
  DsgnIntf
{$endif}
  ;

type
  TElTreeEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

  TElHeaderEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;
 
  
implementation

procedure TElTreeEditor.ExecuteVerb(Index : Integer);
var Form : TCustomForm;

begin
  if Index = 0 then
  begin
    with TItemsPropDlg.Create(Application) do
    begin
      try
        { Set dialog's caption. }
        Caption := Component.Owner.Name + '.' + Component.Name + ' - ' + Caption;
        AComp := Component;
        DTreeItems := TCustomElTree(AComp).Items;
        Tree.Items := DTreeItems;
        if (ShowModal = mrOk) then
        begin
          Form := GetParentForm(Component as TControl);
          if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
        end;
      finally
        Free;  { Free dialog. }
      end;  { try/finally }
    end;  { with }
  end else
  if Index = 1 then
  begin
    with TElSectionsPropDlg.Create(Application) do
    begin
      ASect := TElTree(Component).HeaderSections;
      SetData;
      Caption := Format('Editing %s.Sections', [Component.Name]);
      if ShowModal = mrOk then
      begin
        GetData;
        Form := GetParentForm(Component as TControl);
        if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
      end;
    end;
  end;
end;

function TElTreeEditor.GetVerb(Index : Integer) : string;
begin
  if Index = 0 then
    Result := 'Items ...'
  else
  if Index = 1 then
    Result := 'Header Sections ...';
end;

function TElTreeEditor.GetVerbCount : Integer;
begin
  Result := 2;
end;


procedure TElHeaderEditor.ExecuteVerb(Index : Integer);
var Form : TCustomForm;
begin
  if Index = 0 then
  begin
    with TElSectionsPropDlg.Create(Application) do
    begin
      ASect := TElHeader(Component).Sections;
      SetData;
      Caption := Format('Editing %s.Sections', [Component.Name]);
      if ShowModal = mrOk then
      begin
        GetData;
        Form := GetParentForm(Component as TControl);
        if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
      end;
    end;
  end;
end;

function TElHeaderEditor.GetVerb(Index : Integer) : string;
begin
  if Index = 0 then Result := 'Sections ...';
end;

function TElHeaderEditor.GetVerbCount : Integer;
begin
  Result := 1;
end;

end.

