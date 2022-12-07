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

(*

Version History

05/07/2002

  Fixed adjustment of Margin property that caused infinite loop and hanging of the application

03/12/2002

  Fixed positioning of invisible groups in design-time
  Fixed theme painting when caption is not visible

*)

unit ElExpBar;

interface

uses
    Windows,
    Classes,
    SysUtils,
    Graphics,
    Messages,
    Controls,
{$ifdef VCL_6_USED}
Types,
{$endif}

    ElTools,
    ElList,
    ElPopBtn,
    ElScrollBox,
    ElStrToken,
    ElPanel,
    ElIni,
    ElUxTheme,
    ElTmSchema,
    ElAdvPanel;

type

    EExplorerBarError = class(Exception);

    TElExplorerBar = class;

    TElExplorerBarGroupButton = class(TElAdvCaptionButton)
    protected
      procedure DrawThemedBackground(Canvas : TCanvas); override;
      function GetThemedClassName: WideString; override;
      function GetThemePartID: Integer; override;
      function GetThemeStateID: Integer; override;
    end;

    TElExplorerBarGroup = class(TCustomElAdvancedPanel)
    private
    protected
      FAlign: TAlign;
      procedure TriggerMinimizeEvent; override;
      procedure TriggerRestoreEvent; override;
      procedure Resize; override;
      procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
      procedure WMMove(var Message: TMessage); message WM_MOVE;
      function GetThemedClassName: WideString; override;
      procedure DrawThemedBackground; override;
      function CreateButton: TElAdvCaptionButton; override;
      function CreatePanel: TElAdvCaptionPanel; override;
      procedure WMEraseBkGnd(var Msg : TWMEraseBkGnd); message WM_EraseBkgnd;
      procedure SetUseXPThemes(const Value: Boolean); override;
      procedure CreateWnd; override;
    public
      constructor Create(AOwner : TComponent); override;
      function GetButtonWidth: Integer; override;
      function GetCaptionHeight: Integer; override;
    published
      property Align: TAlign read FAlign write FAlign stored False default alNone;

      property OnImageNeeded;
      property OnLinkClick;
      property Cursor;
      property LinkColor;
      property LinkPopupMenu;
      property LinkStyle;

      property Background;
      property BackgroundType;
      property GradientEndColor;
      property GradientStartColor;
      property GradientSteps;
      property Alignment;
      property Layout;
      property ImageForm;

      property OnMove;

      property BevelInner nodefault;
      property BevelOuter nodefault;
      property BevelSpaceColor nodefault;
      property BevelWidth;
      property BorderStyle;
      property BorderWidth;
      property Canvas;
      property Color;
      property DragCursor;
      property DragMode;
      property Enabled;
      property Font;
      property Locked;
      property MouseCapture;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabStop;
      property Transparent;
      property TransparentXPThemes;
      property UseXPThemes;
      property Visible;
      
      property Caption;
      property OnClick;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnResize;
  {$IFDEF VCL_4_USED}
      property Action;
  {$ifdef MSWINDOWS}
      property Floating;
      property BevelKind;
  {$endif}
  {$ENDIF}

      property Minimized;
      property CaptionSettings;
      property OnMinimize;
      property OnRestore;
      property OnClose;
    end;

    TElExplorerBar = class(TElScrollBox)
    private
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    protected
      FInRealign : integer;
      FUpdated   : boolean;
      FMargin: Integer;
      FSpacing: Integer;
      FGroupWidth: Integer;
      FStorage: TElIniFile;
      FStoragePath: string;
      procedure RealignGroups; virtual;
      procedure CMControlChange(var Msg : TCMControlChange); message CM_CONTROLCHANGE;
      procedure CMControlListChange(var Msg : TMessage); message CM_CONTROLLISTCHANGE;
      procedure Resize; override;
      procedure SetMargin(Value: Integer);
      procedure SetSpacing(Value: Integer);
      procedure CreateWnd; override;
      procedure SetGroupWidth(Value: Integer);
      function GetThemedClassName: WideString; override;
      procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
      procedure WMHScroll(var Message: TMessage); message WM_HSCROLL;
      procedure Loaded; override;
      {$ifndef VCL_4_USED}
      Function FindChildControl(const ControlName : string) : TControl;
      {$endif}
      procedure DrawThemedBackground(DC : HDC); override;
    public
      constructor Create(AOwner : TComponent); override;
      function AddPanel: TElExplorerBarGroup;
      procedure BeginUpdate;
      procedure EndUpdate;
      procedure Restore;
      procedure Save;
    published
      property Margin: Integer read FMargin write SetMargin default 4;
      property Spacing: Integer read FSpacing write SetSpacing default 8;
      property GroupWidth: Integer read FGroupWidth write SetGroupWidth default 0;
      property Storage: TElIniFile read FStorage write FStorage;
      property StoragePath: string read FStoragePath write FStoragePath;
    end;

  TElExplorerBarGroupCaption = class(TElAdvCaptionPanel)
  protected
    procedure DrawThemedBackground; override;
    procedure WMEraseBkGnd(var Msg : TWMEraseBkGnd); message WM_EraseBkgnd;
    function GetThemedClassName: WideString; override;
  end;

implementation


constructor TElExplorerBar.Create(AOwner : TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls];
  FMargin := 4;
  VertScrollbar.Margin := 4;
  HorzScrollbar.Margin := 4;
  FGroupWidth := 0;
  FSpacing := 8;
  Width := 100;
  Align := alLeft;
end;

procedure TElExplorerBar.RealignGroups;
var i, j : integer;
    Group : TElExplorerBarGroup;
    CurY : integer;
    L : TElList;
    b : boolean;
    gw : integer;
    OldVertVis : boolean;
begin
  if (FInRealign > 0) then
  begin
    FUpdated := true;
    exit;
  end;           
  if (not HandleAllocated) or (ComponentState * [csDestroying, csReading, csLoading] <> [])  then exit;
  //gw := 0;
  OldVertVis := VertScrollBar.Visible and ((GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL) <> 0);

  inc(FInRealign);
  try
    CurY := Margin - VertScrollBar.Position;
    L := TElList.Create;
    try
      for i := 0 to ControlCount - 1 do
      begin
        Group := TElExplorerBarGroup(Controls[i]);
        b := false;
        for j := 0 to L.Count - 1 do
        begin
          if TElExplorerBarGroup(L[j]).Top > Group.Top then
          begin
            L.Insert(j, Group);
            b := true;
            break;
          end;
        end;
        if not b then
          L.Add(Group);
      end;
      gw := GroupWidth;
      if gw = 0 then
        gw := Max(ClientWidth - Margin * 2, 0);

      for i := 0 to L.Count - 1 do
      begin
        if TObject(L[i]) is TElExplorerBarGroup then
        begin
          Group := TElExplorerBarGroup(L[i]);
          if (ClientWidth < Margin) and (GroupWidth = 0) then
            Group.SetBounds(0, CurY, gw, Group.Height)
          else
            Group.SetBounds(Margin - HorzScrollBar.Position, CurY, gw, Group.Height);

          if not (Group.Visible or (csDesigning in ComponentState)) then Continue;
          inc(CurY, Group.Height + Spacing);
        end;
      end;
    finally
      L.Free;
    end;
  finally
    dec(FInRealign);
  end;

  if ((HorzScrollBar.Visible and ((GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL) <> 0)) or (OldVertVis <> (VertScrollBar.Visible and ((GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL) <> 0)))) and (GroupWidth = 0) then
    HorzScrollBar.Visible := false;
    // PostMessage(Handle, WM_SIZE, 0, 0);
end;

procedure TElExplorerBar.CMControlChange(var Msg : TCMControlChange);
begin
  inherited;
  if Msg.Inserting then
  begin
    if not (Msg.Control is TElExplorerBarGroup) then
      raise EExplorerBarError.Create('ElExplorerBar doesn''t accept controls');
    RealignGroups;
  end;
end;

procedure TElExplorerBar.CMControlListChange(var Msg : TMessage);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if not (boolean(Msg.LParam)) then RealignGroups;
end;

function TElExplorerBar.AddPanel: TElExplorerBarGroup;
begin
  Result := TElExplorerBarGroup.Create(Self);
  Result.Parent := Self;
end;

procedure TElExplorerBar.Resize;
begin
  inherited;
  RealignGroups;
end;

procedure TElExplorerBar.SetMargin(Value: Integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    VertScrollBar.Margin := FMargin; 
    RealignGroups;
  end;
end;

procedure TElExplorerBar.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    RealignGroups;
  end;
end;

procedure TElExplorerBar.CreateWnd;
begin
  inherited;
  RealignGroups;
end;

procedure TElExplorerBar.SetGroupWidth(Value: Integer);
begin
  if FGroupWidth <> Value then
  begin
    FGroupWidth := Value;
    RealignGroups;
  end;
end;

function TElExplorerBar.GetThemedClassName: WideString;
begin
  Result := 'EXPLORERBAR';
end;

procedure TElExplorerBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var R, R1 : TRect;
begin
  if IsThemeApplied then
  begin
    R := ClientRect;
    GetClipBox(Msg.DC, R1);
    DrawThemeBackground(Theme, Msg.DC, 0, 0, R, @R1);
    Msg.result := 1;
  end
  else
    inherited;
end;

procedure TElExplorerBar.BeginUpdate;
begin
  inc(FInRealign);
end;

procedure TElExplorerBar.EndUpdate;
begin
  dec(FInRealign);
  if (FInRealign = 0) and FUpdated then
    RealignGroups;
  FUpdated := false;
end;

procedure TElExplorerBar.WMVScroll(var Message: TMessage);
begin
  BeginUpdate;
  inherited;
  EndUpdate;
end;

procedure TElExplorerBar.WMHScroll(var Message: TMessage);
begin
  BeginUpdate;
  inherited;
  EndUpdate;
end;

procedure TElExplorerBar.Restore;
var
  SaveKey : string;
  s : string;
  Ctl : TElExplorerBarGroup;
  comp: TControl;
  Token : TElStringTokenizer;
begin
  BeginUpdate;
  if Assigned(FStorage) then
  begin
    SaveKey := FStorage.CurrentKey;
    if FStorage.OpenKey(StoragePath + FStorage.Delimiter + 'ElExplorerBar', false) then
    begin
      if FStorage.ReadString('', 'Groups', '', s) then
      begin
        Token := TElStringTokenizer.CreateStrDelim(s, ':;');
        try
          try
            while Token.HasMoreTokens do
            begin
              s := Token.NextToken;
              comp := FindChildControl(s);
              if comp is TElExplorerBarGroup then
              begin
                Ctl := TElExplorerBarGroup(Comp);
                s := Token.NextToken;
                if s = 'h' then
                  ctl.Visible := false
                else
                if s = 'v' then
                  ctl.Visible := true;
                s := Token.NextToken;
                if s = 'm' then
                  ctl.Minimized := true
                else
                if s = 'n' then
                  ctl.Minimized := false;
              end
              else
              begin
                Token.NextToken;
                Token.NextToken;
              end;
            end;
          finally
            Token.Free;
          end;
        except
        end;
      end;
      FStorage.OpenKey(SaveKey, false);
    end;
  end;
  EndUpdate;
end; {Restore}

procedure TElExplorerBar.Save;
var
  SaveKey : string;
  i : integer;
  vs, hs : string;
begin
  if Assigned(FStorage) then
  begin
    SaveKey := FStorage.CurrentKey;
    if FStorage.OpenKey(StoragePath + FStorage.Delimiter + 'ElExplorerBar', true) then
    begin
      vs := '';
      hs := '';
      for i := 0 to ControlCount - 1 do
      begin
        if i > 0 then
          vs := vs + ';';
        vs := vs + Controls[i].Name;
        if Controls[i].Visible then
          vs := vs + ':v'
        else
          vs := vs + ':h';
        if TElExplorerBarGroup(Controls[i]).Minimized then
          vs := vs + ':m'
        else
          vs := vs + ':n';
      end;
      FStorage.WriteString('', 'Groups', vs);
      FStorage.OpenKey(SaveKey, false);
    end;
  end;
end; {Save}

{$ifndef VCL_4_USED}
function TElExplorerBar.FindChildControl(const ControlName: string): TControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ControlCount - 1 do
    if CompareText(TControl(Controls[I]).Name, ControlName) = 0 then
    begin
      Result := Controls[I];
      Exit;
    end;
end;
{$endif}

procedure TElExplorerBar.Loaded;
begin
  inherited;
  RealignGroups;
end;

procedure TElExplorerBar.DrawThemedBackground(DC : HDC);
begin
  // intentionally left blank
end;

constructor TElExplorerBarGroup.Create(AOwner : TComponent);
begin
  inherited;
  FCaptionSettings.ShowCloseButton := false;
  FCloseButton.ShowGlyph := false;
  FMinButton.ShowGlyph := false;
end;

procedure TElExplorerBarGroup.TriggerMinimizeEvent;
begin
  if Parent is TElExplorerBar then
    TElExplorerBar(Parent).RealignGroups;
  inherited;
end;

procedure TElExplorerBarGroup.TriggerRestoreEvent;
begin
  if Parent is TElExplorerBar then
    TElExplorerBar(Parent).RealignGroups;
  inherited;
end;

procedure TElExplorerBarGroup.Resize;
begin
  inherited;
  if Parent is TElExplorerBar then
    TElExplorerBar(Parent).RealignGroups;
end;

procedure TElExplorerBarGroup.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Parent is TElExplorerBar then
    TElExplorerBar(Parent).RealignGroups;
end;

procedure TElExplorerBarGroup.WMMove(var Message: TMessage);
begin
  inherited;
  if Parent is TElExplorerBar then
    TElExplorerBar(Parent).RealignGroups;
end;

function TElExplorerBarGroup.GetThemedClassName: WideString;
begin
  Result := 'EXPLORERBAR';
end;

procedure TElExplorerBarGroup.DrawThemedBackground;
var FBar : TElExplorerBar;
    R    : TRect;
begin
  if Self.FCaptionSettings.Visible then
  begin
    R := ClientRect;
    R.Bottom := FCaptionPanel.Height;
    DrawThemeParentBackground(Handle, Canvas.Handle, R);
  end;
  R := ClientRect;
  if Self.FCaptionSettings.Visible then
    R.Top := FCaptionPanel.Height - 1;
  FBar := Parent as TElExplorerBar;
  if FBar <> nil then
    DrawThemeBackground(Theme, Canvas.Handle, EBP_NORMALGROUPBACKGROUND, 0, R, nil);
end;

function TElExplorerBarGroup.CreateButton: TElAdvCaptionButton;
begin
  Result := TElExplorerBarGroupButton.Create(FCaptionPanel);
end;

function TElExplorerBarGroup.CreatePanel: TElAdvCaptionPanel;
begin
  Result := TElExplorerBarGroupCaption.Create(Self);
end;

procedure TElExplorerBarGroup.WMEraseBkGnd(var Msg : TWMEraseBkGnd);
var FBar : TElExplorerBar;
    R    : TRect;
begin
  if IsThemeApplied then
  begin
    if Self.FCaptionSettings.Visible then
    begin
      R := ClientRect;
      R.Bottom := FCaptionPanel.Height;
      DrawThemeParentBackground(Handle, Msg.DC, R);
    end;
    R := ClientRect;
    if Self.FCaptionSettings.Visible then
      R.Top := FCaptionPanel.Height - 1;
    FBar := Parent as TElExplorerBar;
    if FBar <> nil then
      DrawThemeBackground(Theme, Msg.DC, EBP_NORMALGROUPBACKGROUND, EBHC_NORMAL, R, nil);
  end;

  Msg.Result := 1;
end;

procedure TElExplorerBarGroup.SetUseXPThemes(const Value: Boolean);
begin
  inherited;
  FCloseButton.ShowGlyph := not IsThemeApplied;
  FMinButton.ShowGlyph := not IsThemeApplied;  
end;

procedure TElExplorerBarGroup.CreateWnd;
begin
  inherited;
  FCloseButton.ShowGlyph := not IsThemeApplied;
  FMinButton.ShowGlyph := not IsThemeApplied;
end;

function TElExplorerBarGroup.GetButtonWidth: Integer;
var S1, S2 : TSize;
begin
  if IsThemeApplied and (Parent is TElExplorerBar) then
  begin
    GetThemePartSize((Parent as TElExplorerBar).Theme, Canvas.Handle, EBP_HEADERCLOSE, 1, nil, TS_TRUE, S1);
    GetThemePartSize((Parent as TElExplorerBar).Theme, Canvas.Handle, EBP_NORMALGROUPEXPAND, 1, nil, TS_TRUE, S2);
    result := Max(Max(S1.cx, S1.cy), Max(S2.cx, S2.cy));
  end
  else
    Result := FCaptionSettings.ButtonWidth;
end;

function TElExplorerBarGroup.GetCaptionHeight: Integer;
begin
  result := Max(FCaptionSettings.Height, GetButtonWidth + 2);
end;


procedure TElExplorerBarGroupButton.DrawThemedBackground;
begin
  // intentionally left blank
end;

function TElExplorerBarGroupButton.GetThemedClassName: WideString;
begin
  Result := 'EXPLORERBAR';
end;

function TElExplorerBarGroupButton.GetThemePartID: Integer;
begin
  if Self = TElExplorerBarGroup(Parent.Parent).FCloseButton then
    result := EBP_HEADERCLOSE
  else
  if TElExplorerBarGroup(Parent.Parent).Minimized then
    result := EBP_NORMALGROUPEXPAND
  else
    result := EBP_NORMALGROUPCOLLAPSE;
end;

function TElExplorerBarGroupButton.GetThemeStateID: Integer;
begin
  if FState in [ebsDown, ebsExclusive] then
    result := EBHC_PRESSED
  else
  if FMouseInControl or FMouseInArrow then
    result := EBHC_HOT
  else
    result := EBHC_NORMAL;
end;

procedure TElExplorerBarGroupCaption.DrawThemedBackground;
var FBar : TElExplorerBar;
begin
  FBar := Parent.Parent as TElExplorerBar;
  if FBar <> nil then
    DrawThemeBackground(Theme, Canvas.Handle, EBP_NORMALGROUPHEAD, EBHC_NORMAL, ClientRect, nil);
end;

procedure TElExplorerBarGroupCaption.WMEraseBkGnd(var Msg : TWMEraseBkGnd);
var FBar : TElExplorerBar;
    R    : TRect;
begin
  if IsThemeApplied then
  begin
    R := ClientRect;
    DrawThemeParentBackground(Handle, Msg.DC, R);
    FBar := Parent.Parent as TElExplorerBar;
    if FBar <> nil then
      DrawThemeBackground(Theme, Msg.DC, EBP_NORMALGROUPHEAD, EBHC_NORMAL, R, nil);
  end;
  Msg.Result := 1;
end;

function TElExplorerBarGroupCaption.GetThemedClassName: WideString;
begin
  Result := 'EXPLORERBAR';
end;


end.

