
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
{$include elpack2.inc}
{$ifdef ELPACK_SINGLECOMP}
{$I ElPack.inc}
{$else}
{$ifdef LINUX}
{$I ../ElPack.inc}
{$else}
{$I ..\ElPack.inc}
{$endif}
{$endif}

unit frmTbrStp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElToolBar, ExtCtrls, StdCtrls, ElACtrls, ElBtnCtl, ElPopBtn, ElList,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElXPThemedControl, ElListBox, ElCLabel, ElLabel, ElEdits, ElCombos;

type
  TfrmToolbarSetup = class(TForm)
    pnlSections: TPanel;
    btnAdd: TElPopupButton;
    btnDelete: TElPopupButton;
    btnUp: TElPopupButton;
    btnDown: TElPopupButton;
    btnOk: TElPopupButton;
    btnCancel: TElPopupButton;
    lbxAvailable: TElListBox;
    lbxVisible: TElListBox;
    lblAvailable: TElLabel;
    lblVisible: TElLabel;
    TextOptionsLabel: TElLabel;
    IconOptionsLabel: TElLabel;
    TextOptionsCombo: TElComboBox;
    IconOptionsCombo: TElComboBox;
    procedure FormShow(Sender: TObject);
    procedure lbxVisibleEnter(Sender: TObject);
    procedure lbxAvailableEnter(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure lbxVisibleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbxVisibleDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxVisibleDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxAvailableDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxAvailableMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbxAvailableDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    procedure UpdateButtons;
  public
    procedure LoadToolbarControls(Toolbar : TElToolbar);
    procedure SaveToolbarControls(Toolbar : TElToolbar);
  end;

implementation

{$ifndef CLX_USED}
{$R *.DFM}
{$else}
{$R *.XFM}
{$endif}

type THackControl = class(TControl);

procedure TfrmToolbarSetup.LoadToolbarControls(Toolbar : TElToolbar);
var
  I : integer;
  S : string;
  AControl : TControl;
  List2    : TElList;
begin
  lbxAvailable.Items.BeginUpdate;
  lbxAvailable.Items.Clear;
  lbxVisible.Items.BeginUpdate;
  lbxVisible.Items.Clear;
  lbxAvailable.Items.AddObject('Separator', nil);

  List2 := TElList.Create;

  Toolbar.OrderedControls(List2);

  for I := 0 to List2.Count - 1 do
  begin
    AControl := TControl(List2[i]);
    S := THackControl(AControl).Caption;
    if S = '' then
    begin
      S := '(Untitled)';
      if AControl is TElToolButton then
      begin
        if TElToolButton(AControl).ButtonType in [ebtDivider, ebtSeparator] then
          S := 'Separator';
      end;
    end;
    if ((AControl is TElToolButton) and TElToolButton(AControl).RealVisible) or 
	    ((not (AControl is TElToolButton) ) and AControl.Visible) then
   // if AControl.Visible then 
      lbxVisible.Items.AddObject(S, AControl)
    else
      lbxAvailable.Items.AddObject(S, AControl);
  end;
  List2.Free;
  
  lbxVisible.Items.EndUpdate;
  lbxAvailable.Items.EndUpdate;
  if Toolbar.ShowCaption then
    TextOptionsCombo.ItemIndex := 0
  else
    TextOptionsCombo.ItemIndex := 1;
  if Toolbar.LargeSize then
    IconOptionsCombo.ItemIndex := 1
  else
    IconOptionsCombo.ItemIndex := 0;
end;

procedure TfrmToolbarSetup.UpdateButtons;
begin
  btnAdd.Enabled := lbxAvailable.Focused and (lbxAvailable.ItemIndex > -1);
  btnDelete.Enabled := lbxVisible.Focused and (lbxVisible.ItemIndex > -1);
  btnUp.Enabled := lbxVisible.Focused and (lbxVisible.ItemIndex > 0);
  btnDown.Enabled := lbxVisible.Focused and (lbxVisible.ItemIndex > -1) and
    (lbxVisible.ItemIndex < lbxVisible.Items.Count - 1);
end;

procedure TfrmToolbarSetup.FormShow(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmToolbarSetup.lbxVisibleEnter(Sender: TObject);
begin
  if (lbxVisible.ItemIndex = -1) and (lbxVisible.Items.Count > 0) then
    lbxVisible.ItemIndex := 0;
  UpdateButtons;
end;

procedure TfrmToolbarSetup.lbxAvailableEnter(Sender: TObject);
begin
  if (lbxAvailable.ItemIndex = -1) and (lbxAvailable.Items.Count > 0) then
    lbxAvailable.ItemIndex := 0;
  UpdateButtons;
end;

procedure TfrmToolbarSetup.btnAddClick(Sender: TObject);
var
  Index: integer;
begin
  if lbxAvailable.ItemIndex = -1 then exit;
  Index := lbxAvailable.ItemIndex;
  lbxVisible.ItemIndex := lbxVisible.Items.AddObject(lbxAvailable.Items[Index],
    lbxAvailable.Items.Objects[Index]);
  if lbxAvailable.Items.Objects[Index] <> nil then
    lbxAvailable.Items.Delete(Index);
  if lbxAvailable.Items.Count > 0 then
    if lbxAvailable.Items.Count <= Index then
      lbxAvailable.ItemIndex := lbxAvailable.Items.Count - 1
    else
      lbxAvailable.ItemIndex := Index;
  lbxAvailable.SetFocus;
  UpdateButtons;
end;

procedure TfrmToolbarSetup.btnDeleteClick(Sender: TObject);
var
  Index: integer;
begin
  if lbxVisible.ItemIndex = -1 then exit;
  Index := lbxVisible.ItemIndex;
  if lbxVisible.Items.Objects[Index] <> nil then
    lbxAvailable.ItemIndex := lbxAvailable.Items.AddObject(lbxVisible.Items[Index],
      lbxVisible.Items.Objects[Index]);

  lbxVisible.Items.Delete(Index);  
  if lbxVisible.Items.Count > 0 then
    if lbxVisible.Items.Count <= Index then
      lbxVisible.ItemIndex := lbxVisible.Items.Count - 1
    else
      lbxVisible.ItemIndex := Index;
  lbxVisible.SetFocus;
  UpdateButtons;
end;

procedure TfrmToolbarSetup.btnUpClick(Sender: TObject);
var
  Index: integer;
begin
  if lbxVisible.ItemIndex <= 0 then exit;
  Index := lbxVisible.ItemIndex;
  lbxVisible.Items.Exchange(Index, Index - 1);
  lbxVisible.ItemIndex := Index - 1;
  lbxVisible.SetFocus;
  UpdateButtons;
end;

procedure TfrmToolbarSetup.btnDownClick(Sender: TObject);
var
  Index: integer;
begin
  if (lbxVisible.ItemIndex = -1) or (lbxVisible.ItemIndex = lbxVisible.Items.Count - 1) then exit;
  Index := lbxVisible.ItemIndex;
  lbxVisible.Items.Exchange(Index, Index + 1);
  lbxVisible.ItemIndex := Index + 1;
  lbxVisible.SetFocus;
  UpdateButtons;
end;

procedure TfrmToolbarSetup.lbxVisibleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then lbxVisible.BeginDrag(False);
end;

procedure TfrmToolbarSetup.lbxVisibleDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source = lbxVisible) or (Source = lbxAvailable) then
    Accept := True;
end;

procedure TfrmToolbarSetup.lbxVisibleDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Index,
  Index1 : integer;
begin
  if Source = lbxVisible then
  begin
    Index := lbxVisible.ItemAtPos(Point(X, Y), True);
    if Index <> -1 then
    begin
      lbxVisible.Items.Move(lbxVisible.ItemIndex, Index);
      lbxVisible.ItemIndex := Index;
      UpdateButtons;
    end;
  end
  else
  if Source = lbxAvailable then
  begin
    Index := lbxAvailable.ItemIndex;
    Index1 := lbxVisible.ItemAtPos(Point(X, Y), True);
    if Index1 = -1 then Index1 := lbxVisible.Items.Count;
    lbxVisible.Items.InsertObject(Index1, lbxAvailable.Items[Index],
      lbxAvailable.Items.Objects[Index]);
    lbxVisible.ItemIndex := Index1;

    lbxAvailable.Items.Delete(Index);
    if lbxAvailable.Items.Count > 0 then
      if lbxAvailable.Items.Count <= Index then
        lbxAvailable.ItemIndex := lbxAvailable.Items.Count - 1
      else
        lbxAvailable.ItemIndex := Index;
    lbxAvailable.SetFocus;
    UpdateButtons;
  end;
end;

procedure TfrmToolbarSetup.lbxAvailableDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbxVisible);
end;

procedure TfrmToolbarSetup.lbxAvailableMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then lbxAvailable.BeginDrag(False);
end;

procedure TfrmToolbarSetup.lbxAvailableDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Index: integer;
begin
  if Source = lbxVisible then
  begin
    Index := lbxVisible.ItemIndex;

    lbxAvailable.ItemIndex := lbxAvailable.Items.AddObject(lbxVisible.Items[Index],
      lbxVisible.Items.Objects[Index]);
    lbxVisible.Items.Delete(Index);
    if lbxVisible.Items.Count > 0 then
      if lbxVisible.Items.Count <= Index then
        lbxVisible.ItemIndex := lbxVisible.Items.Count - 1
      else
        lbxVisible.ItemIndex := Index;
    lbxVisible.SetFocus;
    UpdateButtons;
  end;
end;

procedure TfrmToolbarSetup.SaveToolbarControls(Toolbar : TElToolbar);
var
  I, K : integer;
  // Section: TElHeaderSection;
  AControl : TControl;

begin
  Toolbar.BeginUpdate;

  for i := 0 to Toolbar.ControlCount - 1 do
  begin
    AControl := Toolbar.Controls[i];
    if (AControl is TElToolButton) and (AControl.Name = '') then
    begin
      if TElToolButton(AControl).ButtonType <> ebtButton then
      begin
        K := lbxAvailable.Items.IndexOfObject(AControl);
        if K <> -1 then
        begin
          lbxAvailable.Items.Delete(K);
          AControl.Free;
        end;
      end;
    end
    else
    begin
 //     K := lbxVisible.Items.IndexOfObject(AControl);
 //     if K = -1 then
    end;
  end;

  for i := 0 to lbxAvailable.Items.Count - 1 do
  begin
    AControl := TControl(lbxAvailable.Items.Objects[I]);
    if AControl <> nil then
      AControl.Visible := false;
  end;

  for I := 0 to lbxVisible.Items.Count - 1 do
  begin
    AControl := TControl(lbxVisible.Items.Objects[I]);
    if AControl = nil then
    begin
      AControl := Toolbar.AddButton(ebtDivider);
    end;
    AControl.Visible := true;
    AControl.Left := 10000 + i;
    AControl.Top  := 10000 + i;
    // Section := TElHeaderSection(lbxVisible.Items.Objects[I]);
  end;

  Toolbar.EndUpdate;
  Toolbar.ShowCaption := TextOptionsCombo.ItemIndex = 0;
  Toolbar.LargeSize := IconOptionsCombo.ItemIndex = 1;
end;

end.
