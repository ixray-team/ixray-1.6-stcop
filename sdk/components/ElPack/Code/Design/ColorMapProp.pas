
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit ColorMapProp;

interface

uses Classes, SysUtils, 
  DesignIntf, DesignEditors, DesignWindows, DsnConst,  

Dialogs, Controls, Forms, TypInfo, frmColorMapItems, ElColorMap;

type
  TColorMapItemsProperty = class(TPropertyEditor)
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
  end;

implementation

procedure TColorMapItemsProperty.Edit;
{ Called when the '...' button is pressed or the property is double-clicked. }
var
  i : integer;
  L : TElColorMap;
  E : TColorEntry;
  ColorMapItemsForm : TColorMapItemsForm;
{$IFNDEF VER90}
  Form : TCustomForm;
{$ELSE}
  Form : TForm;
{$ENDIF}

begin
  ColorMapItemsForm := nil;
  try
    ColorMapItemsForm := TColorMapItemsForm.Create(Application);
    with ColorMapItemsForm do
    begin
      L := (GetComponent(0) as TElColorMap);
      for i := 0 to L.Count - 1 do
      begin
        E := L.Items[i];
        Map.AddItem(E);
      end;
      Map.CustomCols.Assign(L.CustomCols);
      if (ShowModal = mrOK) then
      begin
        L.ClearItems;
        L.BeginUpdate;
        for i := 0 to Map.Count - 1 do
        begin
          E := Map.Items[i];
          L.AddItem(E);
        end;
        L.EndUpdate;
        L.CustomCols.Assign(Map.CustomCols);
        try
          Form := GetParentForm(TComponent(L).Owner as TControl);
        except
          Form := nil;
        end;
        if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
      end; { if }
    end; { with }
  finally
    ColorMapItemsForm.Free;
  end; { try/finally }
end; { Edit }

function TColorMapItemsProperty.GetAttributes : TPropertyAttributes;
{ Returns information used by the Object Inspector to show the approprate adornments (e.g., "..." button, drop-down button). }
begin
  GetAttributes := [paDialog];
end; { GetAttributes }

function TColorMapItemsProperty.GetValue : string;
{ Returns a string representation for the property's current value, so that the Object Inspector can display it. The default is "unknown". }
begin
  result := '(Color Entries)';
end; { GetValue }

end.
