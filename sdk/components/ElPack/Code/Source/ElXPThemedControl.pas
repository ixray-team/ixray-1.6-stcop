{

Known problems:

1. Tick marks are of wrong color in ElTrackbar
3. Header sort marks are not drawn correctly

}

{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Copyright (c) 2001, Akzhan Abdulin               }
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

09/21/2001 (c) Akzhan Abdulin

  Now nonclient area size will be recalculated after theme changes

09/17/2001 (c) Akzhan Abdulin

  Initiated.
  Themed control fully supports one theme per control paradigm.
  Bevel is drawn using Theme API.

  Developer note: You must override in descendants only one method named
  GetThemedClassName.

*)
                        
unit ElXPThemedControl;

interface

uses
  ElUxTheme,
  ElTmSchema,
  Classes,
{$ifndef CLX_USED}
  Controls,
  Forms,
{$ifdef VCL_6_USED}
  Types,
{$endif}
  Messages
{$else}
  QForms,
  QControls,
  Qt
{$ENDIF}
  ;

type
  TElXPThemedControl = class(TCustomControl)
  private
    FUseXPThemes: Boolean;
    FTheme: HTheme;
  protected
    procedure SetUseXPThemes(const Value: Boolean); virtual;
    function GetThemedClassName: WideString; virtual; abstract;
{$ifdef MSWINDOWS}
{$ifdef CLX_USED}
    procedure CreateWidget; override;
    procedure DestroyWidget; override;
{$else}
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
{$endif}
{$ifndef CLX_USED}
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
{$endif}
{$endif}
    procedure FreeThemeHandle; virtual;
    procedure CreateThemeHandle; virtual;

    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default True;
  public
    constructor Create(AOwner : TComponent); override;
    function IsThemeApplied: Boolean;
    property Theme: HTheme read FTheme;
  end;

implementation

{$ifndef ELPACK_EXCLUDE_MANIFEST}
{$r *.res}
{$endif}

{$ifdef MSWINDOWS}
uses
  Windows;
{$endif}

{ TElXPThemedControl }

constructor TElXPThemedControl.Create(AOwner: TComponent);
begin
  inherited;
  FUseXPThemes := True;
end;

procedure TElXPThemedControl.CreateThemeHandle;
begin
  if ThemesAvailable then
    {$ifndef CLX_USED}
    FTheme := OpenThemeData(Handle, PWideChar(GetThemedClassName()))
    {$else}
    {$ifdef MSWINDOWS}
    FTheme := OpenThemeData(QWidget_winID(Handle), PWideChar(GetThemedClassName()))
    {$endif}
    {$endif}
  else
    FTheme := 0;
end;

{$ifdef MSWINDOWS}

{$ifndef CLX_USED}
procedure TElXPThemedControl.CreateWnd;
{$else}
procedure TElXPThemedControl.CreateWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and (not IsThemeApplied) then
  begin
    CreateThemeHandle;
  end;
end;

{$ifndef CLX_USED}
procedure TElXPThemedControl.DestroyWnd;
{$else}
procedure TElXPThemedControl.DestroyWidget;
{$endif}
begin
  inherited;
  if UseXPThemes and IsThemeApplied then
  begin
    FreeThemeHandle;
  end;
end;

{$endif}

procedure TElXPThemedControl.FreeThemeHandle;
begin
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
    CloseThemeData(FTheme);
  {$endif}
  FTheme := 0;
end;

function TElXPThemedControl.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TElXPThemedControl.SetUseXPThemes(const Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    {$ifdef MSWINDOWS}
    if ThemesAvailable and HandleAllocated and not (csDestroying in ComponentState) then
    begin
      if FUseXPThemes then
      begin
        CreateThemeHandle;
      end
      else
      begin
        FreeThemeHandle;
      end;
      {$ifndef CLX_USED}
      RecreateWnd;
      {$else}
      RecreateWidget;
      {$endif}
    end;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElXPThemedControl.WMNCPaint(var Message: TMessage);
{$ifdef VCL_4_USED}
const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  DC: HDC;
  RC, RW, SaveRW: TRect;
  EdgeSize: Integer;
  WinStyle: Longint;
{$endif}
begin
  {$ifdef VCL_4_USED}
  if IsThemeApplied() then
  begin
    { Get window DC that is clipped to the non-client area }
    if (BevelKind <> bkNone) or (BorderWidth > 0) then
    begin
      DC := GetWindowDC(Handle);
      try
        Windows.GetClientRect(Handle, RC);
        GetWindowRect(Handle, RW);
        MapWindowPoints(0, Handle, RW, 2);
        OffsetRect(RC, -RW.Left, -RW.Top);
        ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
        { Draw borders in non-client area }
        SaveRW := RW;
        InflateRect(RC, BorderWidth, BorderWidth);
        RW := RC;
        if BevelKind <> bkNone then
        begin
          EdgeSize := 0;
          if BevelInner <> bvNone then Inc(EdgeSize, BevelWidth);
          if BevelOuter <> bvNone then Inc(EdgeSize, BevelWidth);

          with RW do
          begin
            WinStyle := GetWindowLong(Handle, GWL_STYLE);
            if beLeft in BevelEdges then Dec(Left, EdgeSize);
            if beTop in BevelEdges then Dec(Top, EdgeSize);
            if beRight in BevelEdges then Inc(Right, EdgeSize);
            if (WinStyle and WS_VSCROLL) <> 0 then Inc(Right, GetSystemMetrics(SM_CYVSCROLL));
            if beBottom in BevelEdges then Inc(Bottom, EdgeSize);
            if (WinStyle and WS_HSCROLL) <> 0 then Inc(Bottom, GetSystemMetrics(SM_CXHSCROLL));
          end;
          DrawThemeEdge(FTheme, DC, 0, 0, RW, InnerStyles[BevelInner] or OuterStyles[BevelOuter],
            Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[Ctl3D] or BF_ADJUST, @RC);
        end;
        IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
//        RW := SaveRW;
        { Erase parts not drawn }
//        OffsetRect(RW, -RW.Left, -RW.Top);
        DrawThemeBackground(FTheme, DC, 0, 0, RC, nil);
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
    DefaultHandler(Message);
  end
  else
  {$endif}
  begin
    Inherited;
  end;
end;

procedure TElXPThemedControl.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    FreeThemeHandle;
    CreateThemeHandle;
    SetWindowPos(
      Handle,
      0,
      0, 0, 0, 0,
      SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER
      );
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end;
  Message.Result := 1;
end;
{$endif}

end.
