{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1997 Paolo Giacomuzzi              }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

(*

Version History

12/16/2000

  Improved auto-hiding of the auto-hide bars. 

*)
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

unit ElAppBar;

interface

{$ifdef CLX_USED}
error ElAppBar is not compatible with CLX
{$endif}

uses
  Windows, Messages, SysUtils, Classes, Forms, Dialogs, Controls, ExtCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ShellApi, Menus;

const
  // AppBar's user notification message
  WM_APPBARNOTIFY = WM_USER + 100;

  // Timer interval
  SLIDE_DEF_TIMER_INTERVAL = 400; // milliseconds

  // Defaults
  AB_DEF_SIZE_INC = 1;
  AB_DEF_DOCK_DIM = 32;

type
  // You can send to the Windows shell one of the following messages:
  // Message             Description
  // --------------      --------------------------------------------------
  // ABM_NEW             Register a new AppBar to the system
  // ABM_REMOVE          Remove a previously created AppBar from the system
  // ABM_QUERYPOS        Query the AppBar position
  // ABM_SETPOS          Set the AppBar position
  // ABM_GETSTATE        Get the edge the Appbar is docked to
  // ABM_GETTASKBARPOS   Get the Explorer Taskbar position
  // ABM_ACTIVATE        Activate the AppBar
  // ABM_GETAUTOHIDEBAR  Query if AppBar has Auto-hide behavior
  // ABM_SETAUTOHIDEBAR  Set the AppBar's Auto-hide behavior

  // The ABM_message constants are defined in SHELLAPI.PAS as follows:
  // ABM_NEW              = $00000000;
  // ABM_REMOVE           = $00000001;
  // ABM_QUERYPOS         = $00000002;
  // ABM_SETPOS           = $00000003;
  // ABM_GETSTATE         = $00000004;
  // ABM_GETTASKBARPOS    = $00000005;
  // ABM_ACTIVATE         = $00000006;
  // ABM_GETAUTOHIDEBAR   = $00000007;
  // ABM_SETAUTOHIDEBAR   = $00000008;
  // ABM_WINDOWPOSCHANGED = $00000009;

  // The following enumerated type defines the constants in the table
  TAppBarMessage = (abmNew, abmRemove, abmQueryPos, abmSetPos, abmGetState,
    abmGetTaskBarPos, abmActivate, abmGetAutoHideBar,
    abmSetAutoHideBar, abmWindowPosChanged);

  // An AppBar can be in one of 6 states shown in the table below:
  // State          Description
  // -----------    -----------------------------------------------------
  // ABE_UNKNOWN    The Appbar is in an unknown state
  //                (usually during construction/destruction)
  // ABE_FLOAT      The AppBar is floating on the screen
  // ABE_LEFT       The Appbar is docked on the left   edge of the screen
  // ABE_TOP        The Appbar is docked on the top    edge of the screen
  // ABE_RIGHT      The Appbar is docked on the right  edge of the screen
  // ABE_BOTTOM     The Appbar is docked on the bottom edge of the screen

  // The ABE_edge state constants are defined in SHELLAPI.PAS as follows:
  // ABE_LEFT    = 0;
  // ABE_TOP     = 1;
  // ABE_RIGHT   = 2;
  // ABE_BOTTOM  = 3;

  // The ABE_UNKNOWN and ABE_FLOAT constants are defined here as follows:
  // ABE_UNKNOWN = 4;
  // ABE_FLOAT   = 5;

  // The following enumerated type defines the constants in the table
  // (Values are mutually exclusive)
  TAppBarEdge = (abeLeft, abeTop, abeRight, abeBottom, abeUnknown, abeFloat);

  // An AppBar can have several behavior flags as shown below:
  // Flag                        Description
  // --------------------------- -----------------------------------
  // ABF_ALLOWLEFT               Allow dock on left   of screen
  // ABF_ALLOWRIGHT              Allow dock on right  of screen
  // ABF_ALLOWTOP                Allow dock on top    of screen
  // ABF_ALLOWBOTTOM             Allow dock on bottom of screen
  // ABF_ALLOWFLOAT              Allow float in the middle of screen

  // The following enumerated type defines the constants in the table
  TAppBarFlag = (abfAllowLeft, abfAllowTop, abfAllowRight, abfAllowBottom,
    abfAllowFloat);
  TAppBarFlags = set of TAppBarFlag;

  // The following enumerated type defines the AppBar behavior in the Taskbar
  TAppBarTaskEntry = (abtShow, abtHide, abtFloatDependent);

  // The record below contains all of the AppBar settings that
  // can be saved/loaded in/from the Registry
  TAppBarSettings = record
    abEdge : TAppBarEdge; // ABE_UNKNOWN, ABE_FLOAT, or ABE_edge
    bAutohide : Boolean; // Should AppBar be auto-hidden when docked?
    bAlwaysOnTop : Boolean; // Should AppBar always be on top?
    bSlideEffect : Boolean; // Should AppBar slide?
    nTimerInterval : Integer; // Slide Timer Interval (determines speed)
    rcDockDims : TRect; // Width/height for docked bar on 4 edges
    rcFloat : TRect; // Floating rectangle (in screen coordinates)
    nMinWidth : LongInt; // Min allowed width
    nMinHeight : LongInt; // Min allowed height
    nMaxWidth : LongInt; // Max allowed width
    nMaxHeight : LongInt; // Max allowed height
    abTaskEntry : TAppBarTaskEntry; // AppBar behavior in the Windows Taskbar
  end;

  // TAppBar class ////////////////////////////////////////////////////////////
  TElAppBar = class(TForm)
  private
    FInPosChanged  : boolean;

  protected
    FForceReHide : boolean;
    FPreventOffScreen : Boolean;
    FKeepSize : Boolean;
    FDraggingOffset : TPoint; // offset from TopLeft corner to mouse cursor
                              // when the window is about to be moved
    { Internal implementation state variables }

    // This AppBar's settings info
    FABS : TAppBarSettings;

    // ABF_* flags
    FabFlags : TAppBarFlags;

    // Discrete width/height size increments
    FszSizeInc : TSize;

    // We need a member variable which tracks the proposed edge of the
    // AppBar while the user is moving it, deciding where to position it.
    // While not moving, this member must contain ABE_UNKNOWN so that
    // GetEdge returns the current edge contained in FABS.abEdge.
    // While moving the AppBar, FabEdgeProposedPrev contains the
    // proposed edge based on the position of the AppBar.  The proposed
    // edge becomes the new edge when the user stops moving the AppBar.
    FabEdgeProposedPrev : TAppBarEdge;

    // We need a member variable which tracks whether a full screen
    // application window is open
    FbFullScreenAppOpen : Boolean;

    // We need a member variable which tracks whether our autohide window
    // is visible or not
    FbAutoHideIsVisible : Boolean;

    // We need a timer to to determine when the AppBar should be re-hidden
    FTimer : TTimer;

    FOnEdgeChanged : TNotifyEvent;

    // These functions encapsulate the shell's SHAppBarMessage function
    function AppBarMessage(abMessage : TAppBarMessage;
      abEdge : TAppBarEdge;
      lParam : LPARAM;
      bRect : Boolean;
      var rc : TRect) : UINT;

    function AppBarMessage1(abMessage : TAppBarMessage) : UINT;

    function AppBarMessage2(abMessage : TAppBarMessage;
      abEdge : TAppBarEdge) : UINT;

    function AppBarMessage3(abMessage : TAppBarMessage;
      abEdge : TAppBarEdge;
      lParam : LPARAM) : UINT;

    function AppBarMessage4(abMessage : TAppBarMessage;
      abEdge : TAppBarEdge;
      lParam : LPARAM;
      var rc : TRect) : UINT;

    // Gets a edge (ABE_FLOAT or ABE_edge) from a point (screen coordinates)
    function CalcProposedState(var pt : TSmallPoint) : TAppBarEdge;

    // Adjusts the AppBar's location to account for autohide
    // Returns TRUE if rectangle was adjusted
    function AdjustLocationForAutohide(bShow : Boolean;
      var rc : TRect) : Boolean;

    // When Autohide AppBar is shown/hidden, slides in/out of view
    procedure SlideWindow(var rcEnd : TRect);

    // Returns which edge we're autohidden on or ABE_UNKNOWN
    function GetAutohideEdge : TAppBarEdge;

    // Returns a TSmallPoint that gives the cursor position in screen coords
    function GetMessagePosition : TSmallPoint;

    procedure SetKeepSize(newValue : Boolean);
    procedure SetPreventOffScreen(newValue : Boolean);
    procedure SetHorzInc(newValue : integer);
    procedure SetVertInc(newValue : integer);
    function  GetVertInc : integer;
    function  GetHorzInc : integer;

  protected

    procedure DoEdgeChanged;

    // Gets a retangle position (screen coordinates) from a proposed state
    procedure GetRect(abEdgeProposed : TAppBarEdge; var rcProposed : TRect); virtual;

  { Property selector functions }

    // Retrieves the AppBar's edge.  If the AppBar is being positioned, its
    // proposed state is returned instead
    function GetEdge : TAppBarEdge;

    // Changes the AppBar's edge to ABE_UNKNOWN, ABE_FLOAT or an ABE_edge
    procedure SetEdge(abEdge : TAppBarEdge);

    procedure SetSlideTime(nInterval : Integer);

    // Returns TRUE if Auto-hide is on, FALSE if Auto-hide is off
    function IsAutoHide : Boolean;

    // Sets the Auto-hide behavior
    procedure SetAutoHide(bAutoHide : Boolean);

    // Returns TRUE if AppBar is always on topAuto-hide, FALSE otherwise
    function IsAlwaysOnTop : Boolean;

    // Sets the AlwaysOnTop behavior
    procedure SetAlwaysOnTop(bAlwaysOnTop : Boolean);

    procedure SetFlags(newValue : TAppBarFlags);

  { Overridable functions }

    // Called if user attempts to dock an Autohide AppBar on
    // an edge that already contains an Autohide AppBar
    procedure OnAppBarForcedToDocked; virtual;

    // Called when AppBar gets an ABN_FULLSCREENAPP notification
    procedure OnABNFullScreenApp(bOpen : Boolean); virtual;

    // Called when AppBar gets an ABN_POSCHANGED notification
    procedure OnABNPosChanged; virtual;

    // Called when AppBar gets an ABN_WINDOWARRANGE notification
    procedure OnABNWindowArrange(bBeginning : Boolean); virtual;

  { Message handlers }

    // Called when the AppBar receives a WM_APPBARNOTIFY window message
    procedure OnAppBarCallbackMsg(var Msg : TMessage); message WM_APPBARNOTIFY;

    // Called when the AppBar form is first created
    procedure WmCreate(var Msg : TWMCreate); message WM_CREATE;

    // Called when the AppBar form is about to be destroyed
    procedure WmDestroy(var Msg : TWMDestroy); message WM_DESTROY;

    // Called when the AppBar receives a WM_WINDOWPOSCHANGED message
    procedure OnWindowPosChanged(var Msg : TWMWindowPosChanged);
      message WM_WINDOWPOSCHANGED;

    // Called when the AppBar receives a WM_ACTIVATE message
    procedure OnActivate(var Msg : TWMActivate); message WM_ACTIVATE;

    // Called every timer tick
    procedure OnAppBarTimer(Sender : TObject);

    // Called when the AppBar receives a WM_NCMOUSEMOVE message
    procedure OnNcMouseMove(var Msg : TWMNCMouseMove); message WM_NCMOUSEMOVE;

    procedure OnMouseMove(var Msg : TWMMouseMove);  message WM_MOUSEMOVE;

    // Called when the AppBar receives a WM_NCHITTEST message
    procedure OnNcHitTest(var Msg : TWMNCHitTest); message WM_NCHITTEST;

    // Called when the AppBar receives a WM_ENTERSIZEMOVE message
    procedure OnEnterSizeMove(var Msg : TMessage); message WM_ENTERSIZEMOVE;

    // Called when the AppBar receives a WM_EXITSIZEMOVE message
    procedure OnExitSizeMove(var Msg : TMessage); message WM_EXITSIZEMOVE;

    // Called when the AppBar receives a WM_MOVING message
    procedure OnMoving(var Msg : TMessage); message WM_MOVING;

    // Called when the AppBar receives a WM_SIZING message
    procedure OnSizing(var Msg : TMessage); message WM_SIZING;

    // Called when the AppBar receives a WM_GETMINMAXINFO message
    procedure OnGetMinMaxInfo(var Msg : TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;

  { AppBar-specific helper functions }

    // Returns TRUE if abEdge is ABE_LEFT or ABE_RIGHT, else FALSE is returned
    function IsEdgeLeftOrRight(abEdge : TAppBarEdge) : Boolean;

    // Returns TRUE if abEdge is ABE_TOP or ABE_BOTTOM, else FALSE is returned
    function IsEdgeTopOrBottom(abEdge : TAppBarEdge) : Boolean;

    // Returns TRUE if abEdge is ABE_FLOAT, else FALSE is returned
    function IsFloating(abEdge : TAppBarEdge) : Boolean;

    // Returns TRUE if abFlags contain an at least allowed edge to dock on
    function IsDockable(abFlags : TAppBarFlags) : Boolean;

    // Returns TRUE if abFlags contain abfAllowLeft and abfAllowRight
    function IsDockableVertically(abFlags : TAppBarFlags) : Boolean;

    // Returns TRUE if abFlags contain abfAllowTop and abfAllowBottom
    function IsDockableHorizontally(abFlags : TAppBarFlags) : Boolean;

    // Forces the shell to update its AppBar list and the workspace area
    procedure ResetSystemKnowledge;

    // Returns a proposed edge or ABE_FLOAT based on ABF_* flags and a
    // point specified in screen coordinates
    function GetEdgeFromPoint(abFlags : TAppBarFlags;
      pt : TSmallPoint) : TAppBarEdge;

      procedure CreateParams (var Params: TCreateParams); override;

  public
    // Gets the AppBar's docked dimensions
{$IFDEF BUILDER_USED}
    procedure GetDockDims(var rc : TRect);
{$ELSE}
    function GetDockDims : TRect;
{$ENDIF}
    // Sets the AppBar's docked dimensions
    procedure SetDockDims(rc : TRect);

    // Gets the AppBar's floating rectangle
{$IFDEF BUILDER_USED}
    procedure GetFloatRect(var rc : TRect);
{$ELSE}
    function GetFloatRect : TRect;
{$ENDIF}

    // Sets the AppBar's floating rectangle
    procedure SetFloatRect(rc : TRect);

    // Constructs an AppBar
    constructor Create(Owner : TComponent); override;

    // Destroys a previously created AppBar
    destructor Destroy; override;

    // Forces the AppBar's visual appearance to match its internal state
    procedure UpdateBar;

    // If AppBar is Autohide and docked, shows/hides the AppBar
    procedure ShowHiddenAppBar(bShow : Boolean); virtual;

    property AutoHideIsVisible : boolean read FbAutoHideIsVisible;

{$IFNDEF BUILDER_USED}
    // Dimensions when docked on left, top, right and bottom
    property DockDims : TRect read GetDockDims write SetDockDims;

    // AppBar rectangle when floating
    property FloatRect : TRect read GetFloatRect write SetFloatRect;
   
{$ENDIF}

  published

  { Properties }

    // Allowed dockable edges
    property Flags : TAppBarFlags read FabFlags write SetFlags;

    // Horizontal size increment
    property HorzSizeInc : LongInt read GetHorzInc write SetHorzInc;

    // Vertical size increment
    property VertSizeInc : LongInt read GetVertInc write SetVertInc;

    // Edge to dock on
    property Edge : TAppBarEdge read GetEdge write SetEdge;

    // Auto-hide On/Off
    property AutoHide : Boolean read IsAutoHide write SetAutoHide;

    // Always On Top On/Off
    property AlwaysOnTop : Boolean read IsAlwaysOnTop write SetAlwaysOnTop;

    // AppBar MinMax dimensions when floating
    property MinWidth : LongInt read FABS.nMinWidth write FABS.nMinWidth;
    property MinHeight : LongInt read FABS.nMinHeight write FABS.nMinHeight;
    property MaxWidth : LongInt read FABS.nMaxWidth write FABS.nMaxWidth;
    property MaxHeight : LongInt read FABS.nMaxHeight write FABS.nMaxHeight;

    // AppBar behavior in the Window Taskbar
    property TaskEntry : TAppBarTaskEntry read FABS.abTaskEntry write FABS.abTaskEntry;
    // If set, the window won't be stretched when docked, and will keep it's size
    property KeepSize : Boolean read FKeepSize write SetKeepSize;
    // if set, the window will always be kept inside of viewable area (of cause, except when hidden or auto-hide bar)
    property PreventOffScreen : Boolean read FPreventOffScreen write SetPreventOffScreen default false;

    property OnEdgeChanged : TNotifyEvent read FOnEdgeChanged write FOnEdgeChanged;
  end;

implementation

{ Internal implementation functions }

// TElAppBar.AppBarMessage //////////////////////////////////////////////////////

function TElAppBar.AppBarMessage(abMessage : TAppBarMessage;
  abEdge : TAppBarEdge;
  lParam : LPARAM;
  bRect : Boolean;
  var rc : TRect) : UINT;
var
  abd : TAppBarData;
begin
  // Initialize an APPBARDATA structure
  abd.cbSize := sizeof(abd);
  abd.hWnd := Handle;
  abd.uCallbackMessage := WM_APPBARNOTIFY;
  abd.uEdge := Ord(abEdge);
  if bRect then
    abd.rc := rc
  else
    abd.rc := Rect(0, 0, 0, 0);
  abd.lParam := lParam;
  Result := SHAppBarMessage(Ord(abMessage), abd);

  // If the caller passed a rectangle, return the updated rectangle
  if bRect then
    rc := abd.rc;
end;

// TElAppBar.AppBarMessage1 /////////////////////////////////////////////////////

function TElAppBar.AppBarMessage1(abMessage : TAppBarMessage) : UINT;
var
  rc : TRect;
begin
  Result := AppBarMessage(abMessage, abeFloat, 0, False, rc);
end;

// TElAppBar.AppBarMessage2 /////////////////////////////////////////////////////

function TElAppBar.AppBarMessage2(abMessage : TAppBarMessage;
  abEdge : TAppBarEdge) : UINT;
var
  rc : TRect;
begin
  Result := AppBarMessage(abMessage, abEdge, 0, False, rc);
end;

// TElAppBar.AppBarMessage3 /////////////////////////////////////////////////////

function TElAppBar.AppBarMessage3(abMessage : TAppBarMessage;
  abEdge : TAppBarEdge;
  lParam : LPARAM) : UINT;
var
  rc : TRect;
begin
  Result := AppBarMessage(abMessage, abEdge, lParam, False, rc);
end;

// TElAppBar.AppBarMessage4 /////////////////////////////////////////////////////

function TElAppBar.AppBarMessage4(abMessage : TAppBarMessage;
  abEdge : TAppBarEdge;
  lParam : LPARAM;
  var rc : TRect) : UINT;
begin
  Result := AppBarMessage(abMessage, abEdge, lParam, True, rc);
end;

// TElAppBar.CalcProposedState //////////////////////////////////////////////////

function TElAppBar.CalcProposedState(var pt : TSmallPoint) : TAppBarEdge;
var
  bForceFloat : Boolean;
begin
  // Force the AppBar to float if the user is holding down the Ctrl key
  // and the AppBar's style allows floating
  bForceFloat := ((GetKeyState(VK_CONTROL) and $8000) <> 0) and
    (abfAllowFloat in FabFlags);
  if bForceFloat then
    Result := abeFloat
  else
    Result := GetEdgeFromPoint(FabFlags, pt);
end;

// TElAppBar.GetRect ////////////////////////////////////////////////////////////

procedure TElAppBar.GetRect(abEdgeProposed : TAppBarEdge; var rcProposed : TRect);
var
  P : TPoint;
begin
  // This function finds the x, y, cx, cy of the AppBar window
  if abEdgeProposed <> abeFloat then
  begin
    // The AppBar is docked or auto-hide
    // Set dimensions to full screen
    with rcProposed do
    begin
      Left := 0;
      Top := 0;
      Right := GetSystemMetrics(SM_CXSCREEN);
      Bottom := GetSystemMetrics(SM_CYSCREEN);
      if KeepSize then
      begin
        GetCursorPos(P);
        case abEdgeProposed of
          abeTop :
            begin
              Left := P.x;
              Dec(Left, FDraggingOffset.X);
            end;
          abeBottom :
            begin
              Left := P.x;
              Dec(Left, FDraggingOffset.X);
              Top := Bottom - (FABS.rcFloat.Bottom - FABS.rcFloat.Top);
            end;
          abeLeft :
            begin
              Top := P.Y;
              Dec(Top, FDraggingOffset.Y);
            end;
          abeRight :
            begin
              Top := P.Y;
              Dec(Top, FDraggingOffset.Y);
              Left := Right - (FABS.rcFloat.Right - FABS.rcFloat.Left);
            end;
        end;
        Right := Left + FABS.rcFloat.Right - FABS.rcFloat.Left;
        Bottom := Top + FABS.rcFloat.Bottom - FABS.rcFloat.Top;
      end;
    end;
    // Subtract off what we want from the full screen dimensions
    if not IsAutohide then
      // Ask the shell where we can dock
      AppBarMessage4(abmQueryPos, abEdgeProposed, LPARAM(False), rcProposed);
    if KeepSize then
    begin
      case abEdgeProposed of
        abeLeft :
          rcProposed.Right := rcProposed.Left + (FABS.rcFloat.Right - FABS.rcFloat.Left);
        abeTop :
          rcProposed.Bottom := rcProposed.Top + (FABS.rcFloat.Bottom - FABS.rcFloat.Top);
        abeRight :
          rcProposed.Left := rcProposed.Right - (FABS.rcFloat.Right - FABS.rcFloat.Left);
        abeBottom :
          rcProposed.Top := rcProposed.Bottom - (FABS.rcFloat.Bottom - FABS.rcFloat.Top);
      end; // end of case
    end
    else
    begin
      case abEdgeProposed of
        abeLeft :
          rcProposed.Right := rcProposed.Left + FABS.rcDockDims.Left;
        abeTop :
          rcProposed.Bottom := rcProposed.Top + FABS.rcDockDims.Top;
        abeRight :
          rcProposed.Left := rcProposed.Right - FABS.rcDockDims.Right;
        abeBottom :
          rcProposed.Top := rcProposed.Bottom - FABS.rcDockDims.Bottom;
      end; // end of case
    end;
  end;
end;

// TElAppBar.AdjustLocationForAutohide //////////////////////////////////////////

function TElAppBar.AdjustLocationForAutohide(bShow : Boolean;
  var rc : TRect) : Boolean;
var
  x, y : Integer;
  cxVisibleBorder, cyVisibleBorder : Integer;
begin
  if ((GetEdge = abeUnknown) or (GetEdge = abeFloat) or
    (not FABS.bAutohide)) then
  begin
    // If we are not docked on an edge OR we are not auto-hidden, there is
    // nothing for us to do; just return
    Result := False;
    Exit;
  end;

  // Keep a part of the AppBar visible at all times
  cxVisibleBorder := 2 * GetSystemMetrics(SM_CXBORDER);
  cyVisibleBorder := 2 * GetSystemMetrics(SM_CYBORDER);

  if not FKeepSize then
  begin
    // Showing/hiding doesn't change our size; only our position
    x := 0;
    y := 0; // Assume a position of (0, 0)
    if bShow then
    // If we are on the right or bottom, calculate our visible position
      case GetEdge of
        abeRight :
          x := GetSystemMetrics(SM_CXSCREEN) - (rc.Right - rc.Left);
        abeBottom :
          y := GetSystemMetrics(SM_CYSCREEN) - (rc.Bottom - rc.Top);
      end
    else
      // Calculate our x or y coordinate so that only the border is visible
      case GetEdge of
        abeLeft :
          x := -((rc.Right - rc.Left) - cxVisibleBorder);
        abeRight :
          x := GetSystemMetrics(SM_CXSCREEN) - cxVisibleBorder;
        abeTop :
          y := -((rc.Bottom - rc.Top) - cyVisibleBorder);
        abeBottom :
          y := GetSystemMetrics(SM_CYSCREEN) - cyVisibleBorder;
      end;
  end
  else
  begin
    x := Left;
    y := Top;
    if bShow then
      case GetEdge of
        abeTop : y := 0;
        abeBottom : y := GetSystemMetrics(SM_CYSCREEN) - (rc.Bottom - rc.Top);
        abeLeft : x := 0;
        abeRight : x := GetSystemMetrics(SM_CXSCREEN) - (rc.Right - rc.Left);
      end
    else
      case GetEdge of
        abeTop : y := -((rc.Bottom - rc.Top) - cyVisibleBorder);
        abeBottom : y := GetSystemMetrics(SM_CYSCREEN) - cyVisibleBorder;
        abeLeft : x := -((rc.Right - rc.Left) - cxVisibleBorder);
        abeRight : x := GetSystemMetrics(SM_CXSCREEN) - cxVisibleBorder;
      end;
  end;

  with rc do
  begin
    Right := x + (Right - Left);
    Bottom := y + (Bottom - Top);
    Left := x;
    Top := y;
  end;

  Result := True;
end;

// TElAppBar.ShowHiddenAppBar ///////////////////////////////////////////////////

procedure TElAppBar.ShowHiddenAppBar(bShow : Boolean);
var
  rc : TRect;
begin
  //if bShow = FbAutoHideIsVisible then exit;
  // Get our window location in screen coordinates
  GetWindowRect(Handle, rc);

  // Assume  that we are visible
  FbAutoHideIsVisible := True;

  if AdjustLocationForAutohide(bShow, rc) then
  begin
    // the rectangle was adjusted, we are an autohide bar
    // Rememebr whether we are visible or not
    FbAutoHideIsVisible := bShow;

    // Slide window in from or out to the edge
    SlideWindow(rc);
  end;
end;

// TElAppBar.SlideWindow ////////////////////////////////////////////////////////

procedure TElAppBar.SlideWindow(var rcEnd : TRect);
var
  bFullDragOn : LongBool;
  rcStart : TRect;
  dwTimeStart, dwTimeEnd, dwTime : DWORD;
  x, y, w, h : Integer;
begin
  // Only slide the window if the user has FullDrag turned on
  SystemParametersInfo(SPI_GETDRAGFULLWINDOWS, 0, @bFullDragOn, 0);

  // Get the current window position
  GetWindowRect(Handle, rcStart);
  if (FABS.bSlideEffect and bFullDragOn and
    ((rcStart.Left <> rcEnd.Left) or
    (rcStart.Top <> rcEnd.Top) or
    (rcStart.Right <> rcEnd.Right) or
    (rcStart.Bottom <> rcEnd.Bottom))) then
  begin

    // Get our starting and ending time
    dwTimeStart := GetTickCount;
    {$WARNINGS OFF}
    dwTimeEnd := dwTimeStart + FABS.nTimerInterval;
    {$WARNINGS ON}
    dwTime := dwTimeStart;
    while (dwTime < dwTimeEnd) do
    begin
      // While we are still sliding, calculate our new position
      x := rcStart.Left - (rcStart.Left - rcEnd.Left)
        * Integer(dwTime - dwTimeStart) div FABS.nTimerInterval;

      y := rcStart.Top - (rcStart.Top - rcEnd.Top)
        * Integer(dwTime - dwTimeStart) div FABS.nTimerInterval;

      w := (rcStart.Right - rcStart.Left)
        - ((rcStart.Right - rcStart.Left) - (rcEnd.Right - rcEnd.Left))
        * Integer(dwTime - dwTimeStart) div FABS.nTimerInterval;

      h := (rcStart.Bottom - rcStart.Top)
        - ((rcStart.Bottom - rcStart.Top) - (rcEnd.Bottom - rcEnd.Top))
        * Integer(dwTime - dwTimeStart) div FABS.nTimerInterval;

      // Show the window at its changed position
      SetWindowPos(Handle, 0, x, y, w, h,
        SWP_NOZORDER or SWP_NOACTIVATE or SWP_DRAWFRAME);
      UpdateWindow(Handle);
      dwTime := GetTickCount;
    end;
  end;

  // Make sure that the window is at its final position
  SetBounds(rcEnd.Left, rcEnd.Top, rcEnd.Right - rcEnd.Left, rcEnd.Bottom - rcEnd.Top);
end;

// TElAppBar.GetAutohideEdge ////////////////////////////////////////////////////

function TElAppBar.GetAutohideEdge : TAppBarEdge;
begin
  if Handle = AppBarMessage2(abmGetAutoHideBar, abeLeft) then
    Result := abeLeft
  else if Handle = AppBarMessage2(abmGetAutoHideBar, abeTop) then
    Result := abeTop
  else if Handle = AppBarMessage2(abmGetAutoHideBar, abeRight) then
    Result := abeRight
  else if Handle = AppBarMessage2(abmGetAutoHideBar, abeBottom) then
    Result := abeBottom
  else
    // NOTE: If AppBar is docked but not auto-hidden, we return ABE_UNKNOWN
    Result := abeUnknown;
end;

// TElAppBar.GetMessagePosition /////////////////////////////////////////////////

function TElAppBar.GetMessagePosition : TSmallPoint;
var
  pt : TSmallPoint;
  dw : DWORD;
begin
  dw := GetMessagePos;
  pt.X := SHORT(dw);
  pt.Y := SHORT((dw and $FFFF0000) shr 16);
  Result := pt;
end;

{ Property selector functions }

// TElAppBar.GetEdge ////////////////////////////////////////////////////////////

function TElAppBar.GetEdge : TAppBarEdge;
begin
  if FabEdgeProposedPrev <> abeUnknown then
    Result := FabEdgeProposedPrev
  else
    Result := FABS.abEdge;
end;

// TElAppBar.SetEdge ////////////////////////////////////////////////////////////

procedure TElAppBar.SetEdge(abEdge : TAppBarEdge);
var
  abCurrentEdge : TAppBarEdge;
  currentRect : TRect;
  rc : TRect;
  hWnd : THandle;
  ex : boolean;
begin
  if IsRectEmpty(FABS.rcFloat) then
    FABS.rcFloat := BoundsRect;
  // Do not set the edge to non-allowed
  ex := false;
   case abEdge of //
    abeUnknown : ex := false;
    abeBottom : ex := not (abfAllowBottom in Flags);
    abeFloat : ex := not (abfAllowFloat in Flags);
    abeRight : ex := not (abfAllowRight in Flags);
    abeLeft : ex := not (abfAllowLeft in Flags);
    abeTop : ex := not (abfAllowTop in Flags);
  end; // case
  if ex then exit;
  // If the AppBar is registered as auto-hide, unregister it
  abCurrentEdge := GetAutohideEdge;
  if abCurrentEdge <> abeUnknown then
    // Our AppBar is auto-hidden, unregister it
    AppBarMessage3(abmSetAutoHideBar, abCurrentEdge, LPARAM(False));
  // Save the new requested state
  //fOldEdge := FABS.abEdge;
  FABS.abEdge := abEdge;
  {if FForceRehide and (fOldEdge <> FABS.abEdge) then
     FABS.bAutohide := true;}
  case abEdge of
    abeUnknown :
        // We are being completely unregistered.
        // Probably, the AppBar window is being destroyed.
        // If the AppBar is registered as NOT auto-hide, unregister it
      AppBarMessage1(abmRemove);
    abeFloat :
      begin
        // We are floating and therefore are just a regular window.
        // Tell the shell that the docked AppBar should be of 0x0 dimensions
        // so that the workspace is not affected by the AppBar
        currentRect := Rect(0, 0, 0, 0);
        AppBarMessage4(abmSetPos, abEdge, LPARAM(False), currentRect);
        with FABS.rcFloat do
          SetBounds(Left, Top, Right - Left, Bottom - Top);
      end;
  else
    begin
      if IsAutohide and (AppBarMessage3(abmSetAutoHideBar, GetEdge, LPARAM(True)) = 0) then
      begin
        FABS.bAutohide := False; // We couldn't set the AppBar on a new edge, let's dock it instead
          // Call a virtual function to let derived classes know that the AppBar
          // changed from auto-hide to docked
        OnAppBarForcedToDocked;
      end;
      GetRect(GetEdge, rc);
      if IsAutohide then
      begin
        currentRect := Rect(0, 0, 0, 0);
        AppBarMessage4(abmSetPos, abeLeft, LPARAM(False), currentRect);
      end
      else
        AppBarMessage4(abmSetPos, abEdge, LPARAM(False), rc); // Tell the shell where the AppBar is
      AdjustLocationForAutohide(FbAutoHideIsVisible, rc);
        // Slide window in from or out to the edge
      SlideWindow(rc);
    end; // end of else
  end; // end of case

  // Set the AppBar's z-order appropriately
  hWnd := HWND_NOTOPMOST; // Assume normal Z-Order
  if FABS.bAlwaysOnTop then
  begin
    // If we are supposed to be always-on-top, put us there
    hWnd := HWND_TOPMOST;
    if FbFullScreenAppOpen then
      // But, if a full-screen window is opened, put ourself at the bottom
      // of the z-order so that we don't cover the full-screen window
      hWnd := HWND_BOTTOM;
  end;
  SetWindowPos(Handle, hWnd, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);

  // Make sure that any auto-hide appbars stay on top of us after we move
  // even though our activation state has not changed
  AppBarMessage1(abmActivate);

  if not (csDesigning in ComponentState) then
  begin
    // Show or hide the taskbar entry depending on AppBar position
    case FABS.abTaskEntry of
      abtShow :
        ShowWindow(Application.Handle, SW_SHOW);
      abtHide :
        ShowWindow(Application.Handle, SW_HIDE);
      abtFloatDependent :
        case abEdge of
          abeFloat :
            ShowWindow(Application.Handle, SW_SHOW);
          abeLeft, abeTop, abeRight, abeBottom :
            ShowWindow(Application.Handle, SW_HIDE);
        end;
    end;
  end;
end;

// TElAppBar.IsAutoHide /////////////////////////////////////////////////////////

function TElAppBar.IsAutoHide : Boolean;
begin
  Result := FABS.bAutohide;
end;

// TElAppBar.SetAutoHide ////////////////////////////////////////////////////////

procedure TElAppBar.SetAutoHide(bAutoHide : Boolean);
begin
  FABS.bAutohide := bAutoHide;
  if Edge <> abeFloat then UpdateBar;
end;

// TElAppBar.IsAlwaysOnTop //////////////////////////////////////////////////////

function TElAppBar.IsAlwaysOnTop : Boolean;
begin
  Result := FABS.bAlwaysOnTop;
end;

// TElAppBar.SetAlwaysOnTop /////////////////////////////////////////////////////

procedure TElAppBar.SetAlwaysOnTop(bAlwaysOnTop : Boolean);
begin
  FABS.bAlwaysOnTop := bAlwaysOnTop;
  if Edge <> abeFloat then UpdateBar;
end;

// TElAppBar.GetFloatRect ///////////////////////////////////////////////////////

{$IFDEF BUILDER_USED}
procedure TElAppBar.GetFloatRect(var rc : TRect);
begin
  rc := FABS.rcFloat;
end;
{$ELSE}
function TElAppBar.GetFloatRect : TRect;
begin
  Result := FABS.rcFloat;
end;
{$ENDIF}

// TElAppBar.SetFloatRect ///////////////////////////////////////////////////////

procedure TElAppBar.SetFloatRect(rc : TRect);
begin
  FABS.rcFloat := rc;
end;

// TElAppBar.GetDockDims ////////////////////////////////////////////////////////

{$IFDEF BUILDER_USED}
procedure TElAppBar.GetDockDims(var rc : TRect);
begin
  rc := FABS.rcDockDims;
end;
{$ELSE}
function TElAppBar.GetDockDims : TRect;
begin
  Result := FABS.rcDockDims;
end;
{$ENDIF}

// TElAppBar.SetDockDims ////////////////////////////////////////////////////////

procedure TElAppBar.SetDockDims(rc : TRect);
begin
  FABS.rcDockDims := rc;
end;

procedure TElAppBar.SetFlags(newValue : TAppBarFlags);
var
  ex : boolean;
begin
  fabFlags := newValue;
  ex := false;
  case Edge of //
    abeUnknown : ex := false;
    abeBottom : ex := not (abfAllowBottom in Flags);
    abeFloat : ex := not (abfAllowFloat in Flags);
    abeRight : ex := not (abfAllowRight in Flags);
    abeLeft : ex := not (abfAllowLeft in Flags);
    abeTop : ex := not (abfAllowTop in Flags);
  end; // case
  if ex then
  begin
    if abfAllowFloat in fabFlags then
      Edge := abeFloat
    else
    begin
      if abfAllowLeft in fabFlags then
        Edge := abeLeft
      else if abfAllowTop in fabFlags then
        Edge := abeTop
      else if abfAllowBottom in fabFlags then
        Edge := abeBottom
      else if abfAllowRight in fabFlags then
        Edge := abeRight
      else
        Edge := abeUnknown;
    end;
  end;
end;

{ Overridable functions }

// TElAppBar.OnAppBarForcedToDocked /////////////////////////////////////////////

procedure TElAppBar.OnAppBarForcedToDocked;
const
  CRLF = #13#10;
begin
  // Display the application name as the message box caption text.
  MessageDlg('There is already an auto hidden window on this edge.' + CRLF +
    'Only one auto hidden window is allowed on each edge.', mtInformation, [mbOk], 0);
  FForceRehide := true;
end;

// TElAppBar.OnABNFullScreenApp /////////////////////////////////////////////////

procedure TElAppBar.OnABNFullScreenApp(bOpen : Boolean);
begin
  // This function is called when a FullScreen window is openning or
  // closing. A FullScreen window is a top-level window that has its caption
  // above the top of the screen allowing the entire screen to be occupied
  // by the window's client area.

  // If the AppBar is a topmost window when a FullScreen window is activated,
  // we need to change our window to a non-topmost window so that the AppBar
  // doesn't cover the FullScreen window's client area.

  // If the FullScreen window is closing, we need to set the AppBar's
  // Z-Order back to when the user wants it to be.
  FbFullScreenAppOpen := bOpen;
  UpdateBar;
end;

// TElAppBar.OnABNPosChanged ////////////////////////////////////////////////////

procedure TElAppBar.OnABNPosChanged;
begin
  // The TaskBar or another AppBar has changed its size or position
  if (GetEdge <> abeFloat) and (not FABS.bAutohide) then
    // If we're not floating and we're not auto-hidden, we have to
    // reposition our window
    UpdateBar;
end;

// TElAppBar.OnABNWindowArrange /////////////////////////////////////////////////

procedure TElAppBar.OnABNWindowArrange(bBeginning : Boolean);
begin
  // This function intentionally left blank
end;

{ Message handlers }

// TElAppBar.OnAppBarCallbackMsg ////////////////////////////////////////////////

procedure TElAppBar.OnAppBarCallbackMsg(var Msg : TMessage);
begin
  case Msg.WParam of

    ABN_FULLSCREENAPP :
      OnABNFullScreenApp(Msg.LParam <> 0);

    ABN_POSCHANGED :
      OnABNPosChanged;

    ABN_WINDOWARRANGE :
      OnABNWindowArrange(Msg.LParam <> 0);
  end;
end;

// TElAppBar.WmCreate ///////////////////////////////////////////////////////////

procedure TElAppBar.WmCreate(var Msg : TWMCreate);
var
  hMenu : THandle;
begin
  inherited;
  // Associate a timer with the AppBar.  The timer is used to determine
  // when a visible, inactive, auto-hide AppBar should be re-hidden
  FTimer := TTimer.Create(nil);
  with FTimer do
  begin
    Interval := FABS.nTimerInterval;
    OnTimer := OnAppBarTimer;
    Enabled := True;
  end;

  // Save the initial position of the floating AppBar
  FABS.rcFloat.Left := Left;
  FABS.rcFloat.Top := Top;

  // Register our AppBar window with the Shell
  AppBarMessage1(abmNew);

  // Update AppBar internal state
  //UpdateBar;

  // Save the initial size of the floating AppBar
  PostMessage(Handle, WM_ENTERSIZEMOVE, 0, 0);
  PostMessage(Handle, WM_EXITSIZEMOVE, 0, 0);

  // Remove system menu
  hMenu := GetSystemMenu(Handle, False);
  DeleteMenu(hMenu, SC_RESTORE, MF_BYCOMMAND);
  DeleteMenu(hMenu, SC_MINIMIZE, MF_BYCOMMAND);
  DeleteMenu(hMenu, SC_MAXIMIZE, MF_BYCOMMAND);
end;

// TElAppBar.OnDestroy //////////////////////////////////////////////////////////

procedure TElAppBar.WmDestroy(var Msg : TWMDestroy);
begin
  // Free the Autohide timer
  FTimer.Enabled := False;
  FTimer.Free;
  // Unregister our AppBar window with the Shell
  SetEdge(abeUnknown);
  inherited;
end;

// TElAppBar.OnWindowPosChanged /////////////////////////////////////////////////

procedure TElAppBar.OnWindowPosChanged(var Msg : TWMWindowPosChanged);
begin
  if FInPosChanged then
     exit;
  inherited;
  // When our window changes position, tell the Shell so that any
  // auto-hidden AppBar on our edge stays on top of our window making it
  // always accessible to the user
  AppBarMessage1(abmWindowPosChanged);
  if (Msg.WindowPos.hwndInsertAfter <> HWND_TOPMOST) and (FAbs.bAlwaysOnTop) then
  begin
    FInPosChanged := true;
    SetWindowPos(Handle, hwnd_TopMost, 0, 0, 0, 0, swp_NoMove + swp_NoSize + swp_NoActivate);
    FInPosChanged := false;
  end;
end;

// TElAppBar.OnActivate /////////////////////////////////////////////////////////
                                                 
procedure TElAppBar.OnActivate(var Msg : TWMActivate);
begin
  inherited;
  if Msg.Active = WA_INACTIVE then
    // Hide the AppBar if we are docked and auto-hidden
    ShowHiddenAppBar(False);
  // When our window changes position, tell the Shell so that any
  // auto-hidden AppBar on our edge stays on top of our window making it
  // always accessible to the user.
  AppBarMessage1(abmActivate);
end;

// TElAppBar.OnAppBarTimer //////////////////////////////////////////////////////

procedure TElAppBar.OnAppBarTimer(Sender : TObject);
var
  pt : TSmallPoint;
  rc : TRect;
begin
  if (GetActiveWindow <> Handle) or (GetForegroundWindow <> Handle) then
  begin
    // Possibly hide the AppBar if we are not the active window
    // Get the position of the mouse and the AppBar's position
    // Everything must be in screen coordinates
    pt := GetMessagePosition;
    GetWindowRect(Handle, rc);
    // Add a little margin around the AppBar
    InflateRect(rc,
      2 * GetSystemMetrics(SM_CXDOUBLECLK),
      2 * GetSystemMetrics(SM_CYDOUBLECLK));
    if not PtInRect(rc, SmallPointToPoint(pt)) then
      // If the mouse is NOT over the AppBar, hide the AppBar
      ShowHiddenAppBar(False);
  end;
  inherited;
end;

// TElAppBar.OnNcMouseMove //////////////////////////////////////////////////////

procedure TElAppBar.OnNcMouseMove(var Msg : TWMNCMouseMove);
begin
  // If we are a docked, auto-hidden AppBar, shown us
  // when the user moves over our non-client area
  ShowHiddenAppBar(True);
  inherited;
end;

// TElAppBar.OnNcHitTest ////////////////////////////////////////////////////////

procedure TElAppBar.OnNcHitTest(var Msg : TWMNCHitTest);
var
  u : UINT;
  bPrimaryMouseBtnDown : Boolean;
  rcClient : TRect;
  pt : TPoint;
  vKey : Integer;
begin
  // Find out what the system thinks is the hit test code
  inherited;
  {$ifdef D_6_UP}
  if (csDesigning in ComponentState) then exit;
  {$endif}
  u := Msg.Result;
  if (Edge <> abeFloat) and KeepSize then exit;
  // NOTE: If the user presses the secondary mouse button, pretend that the
  // user clicked on the client area so that we get WM_CONTEXTMENU messages
  if GetSystemMetrics(SM_SWAPBUTTON) <> 0 then
    vKey := VK_RBUTTON
  else
    vKey := VK_LBUTTON;
  bPrimaryMouseBtnDown := ((GetAsyncKeyState(vKey) and $8000) <> 0);

  if (u = HTCLIENT) and bPrimaryMouseBtnDown then
    // User clicked in client area, allow AppBar to move.  We get this
    // behavior by pretending that the user clicked on the caption area
    u := HTCAPTION;

  // If the AppBar is floating and the hittest code is a resize code...
  if ((GetEdge = abeFloat) and
    (HTSIZEFIRST <= u) and (u <= HTSIZELAST)) then
  begin
    case u of
      HTLEFT, HTRIGHT :
        if FszSizeInc.cx = 0 then u := HTBORDER;
      HTTOP, HTBOTTOM :
        if FszSizeInc.cy = 0 then u := HTBORDER;
      HTTOPLEFT :
        if (FszSizeInc.cx = 0) and (FszSizeInc.cy = 0) then
          u := HTBORDER
        else if (FszSizeInc.cx = 0) and (FszSizeInc.cy <> 0) then
          u := HTTOP
        else if (FszSizeInc.cx <> 0) and (FszSizeInc.cy = 0) then
          u := HTLEFT;
      HTTOPRIGHT :
        if (FszSizeInc.cx = 0) and (FszSizeInc.cy = 0) then
          u := HTBORDER
        else if (FszSizeInc.cx = 0) and (FszSizeInc.cy <> 0) then
          u := HTTOP
        else if (FszSizeInc.cx <> 0) and (FszSizeInc.cy = 0) then
          u := HTRIGHT;
      HTBOTTOMLEFT :
        if (FszSizeInc.cx = 0) and (FszSizeInc.cy = 0) then
          u := HTBORDER
        else if (FszSizeInc.cx = 0) and (FszSizeInc.cy <> 0) then
          u := HTBOTTOM
        else if (FszSizeInc.cx <> 0) and (FszSizeInc.cy = 0) then
          u := HTLEFT;
      HTBOTTOMRIGHT :
        if (FszSizeInc.cx = 0) and (FszSizeInc.cy = 0) then
          u := HTBORDER
        else if (FszSizeInc.cx = 0) and (FszSizeInc.cy <> 0) then
          u := HTBOTTOM
        else if (FszSizeInc.cx <> 0) and (FszSizeInc.cy = 0) then
          u := HTRIGHT;
    end;
  end;

  // When the AppBar is docked, the user can resize only one edge.
  // This next section determines which edge the user can resize.
  // To allow resizing, the AppBar window must have the WS_THICKFRAME style

  // If the AppBar is docked and the hittest code is a resize code...
  if ((GetEdge <> abeFloat) and (GetEdge <> abeUnknown) and
    (HTSIZEFIRST <= u) and (u <= HTSIZELAST)) then
  begin

    if (IsEdgeLeftOrRight(GetEdge) and (FszSizeInc.cx = 0)) or
      (not IsEdgeLeftOrRight(GetEdge) and (FszSizeInc.cy = 0)) then
    begin
      // If the width/height size increment is zero, then resizing is NOT
      // allowed for the edge that the AppBar is docked on
      u := HTBORDER; // Pretend that the mouse is not on a resize border
    end
    else
    begin
      // Resizing IS allowed for the edge that the AppBar is docked on
      // Get the location of the appbar's client area in screen coordinates
      rcClient := GetClientRect;
      pt.X := rcClient.Left;
      pt.Y := rcClient.Top;
      pt := ClientToScreen(pt);
      rcClient.Left := pt.X;
      rcClient.Top := pt.Y;
      pt.X := rcClient.Right;
      pt.Y := rcClient.Bottom;
      pt := ClientToScreen(pt);
      rcClient.Right := pt.X;
      rcClient.Bottom := pt.Y;

      u := HTBORDER; // Assume that we can't resize
      case GetEdge of
        abeLeft :
          if Msg.XPos > rcClient.Right then
            u := HTRIGHT;
        abeTop :
          if Msg.YPos > rcClient.Bottom then
            u := HTBOTTOM;
        abeRight :
          if Msg.XPos < rcClient.Left then
            u := HTLEFT;
        abeBottom :
          if Msg.YPos < rcClient.Top then
            u := HTTOP;
      end; // end of case
    end; // end of else
  end;
  // Return the hittest code
  Msg.Result := u;
end;

// TElAppBar.OnEnterSizeMove ////////////////////////////////////////////////////

procedure TElAppBar.OnEnterSizeMove(var Msg : TMessage);
begin
  // The user started moving/resizing the AppBar, save its current state
  FabEdgeProposedPrev := GetEdge;
  GetCursorPos(FDraggingOffset);
  Dec(FDraggingOffset.X, Left);
  Dec(FDraggingOffset.Y, Top);
end;

// TElAppBar.OnExitSizeMove /////////////////////////////////////////////////////

procedure TElAppBar.OnExitSizeMove(var Msg : TMessage);
var
  p : TPoint;
  abEdgeProposedPrev: TAppBarEdge;
  rc, rcWorkArea : TRect;
  w, h : Integer;
begin
  // The user stopped moving/resizing the AppBar, set the new state
  // Save the new proposed state of the AppBar
  abEdgeProposedPrev := FabEdgeProposedPrev;

  // Set the proposed state back to unknown.  This causes GetState
  // to return the current state rather than the proposed state
  FabEdgeProposedPrev := abeUnknown;

  // Get the location of the window in screen coordinates
  GetWindowRect(Handle, rc);

  // If the AppBar's state has changed...
  if GetEdge = abEdgeProposedPrev then
    case GetEdge of
      abeLeft :
        // Save the new width of the docked AppBar
        FABS.rcDockDims.Left := rc.Right - rc.Left;
      abeTop :
        // Save the new height of the docked AppBar
        FABS.rcDockDims.Top := rc.Bottom - rc.Top;
      abeRight :
        // Save the new width of the docked AppBar
        FABS.rcDockDims.Right := rc.Right - rc.Left;
      abeBottom :
        // Save the new height of the docked AppBar
        FABS.rcDockDims.Bottom := rc.Bottom - rc.Top;
    end;

  // Always save the new position of the floating AppBar
  if abEdgeProposedPrev = abeFloat then
  begin
    // If AppBar was floating and keeps floating...
    if GetEdge = abeFloat then
    begin
      FABS.rcFloat := rc;
    // If AppBar was docked and is going to float...
    end
    else
    begin
      // Propose width and height depending on the current window position
      w := rc.Right - rc.Left;
      h := rc.Bottom - rc.Top;
      // Adjust width and height
      SystemParametersInfo(SPI_GETWORKAREA, 0, @rcWorkArea, 0);
      if (w >= (rcWorkArea.Right - rcWorkArea.Left)) or
        (h >= (rcWorkArea.Bottom - rcWorkArea.Top)) then
      begin
        w := FABS.rcFloat.Right - FABS.rcFloat.Left;
        h := FABS.rcFloat.Bottom - FABS.rcFloat.Top;
      end;
      // Save new floating position
      if FKeepSize then
      begin
        GetCursorPos(P);
        FABS.rcFloat.Left := P.x - FDraggingOffset.X;
        FABS.rcFloat.Top := P.y - FDraggingOffset.Y;
        FABS.rcFloat.Right := FABS.rcFloat.Left + w;
        FABS.rcFloat.Bottom := FABS.rcFloat.Top + h;
      end
      else
      begin
        FABS.rcFloat.Left := rc.Left;
        FABS.rcFloat.Top := rc.Top;
        FABS.rcFloat.Right := rc.Left + w;
        FABS.rcFloat.Bottom := rc.Top + h;
      end;
    end;
  end;
  if FKeepSize and (abEdgeProposedPrev <> abeFloat) and (Edge <> abeFloat) then
  begin
    GetWindowRect(Handle, rc);
    FABS.rcFloat.Right := FABS.rcFloat.Left + rc.Right - rc.Left;
    FABS.rcFloat.Bottom := FABS.rcFloat.Top + rc.Bottom - rc.Top;
  end;
  // After setting the dimensions, set the AppBar to the proposed state
  SetEdge(abEdgeProposedPrev);
  DoEdgeChanged;

  if (Edge = abeFloat) and PreventOffScreen then
  begin
    Rc := BoundsRect;
    with Rc do
    begin
      if Top < 0 then OffsetRect(Rc, 0, -Top);
      if Bottom > GetSystemMetrics(SM_CYSCREEN) then OffsetRect(Rc, 0, GetSystemMetrics(SM_CYSCREEN) - Bottom);
      if Left < 0 then OffsetRect(Rc, -Left, 0);
      if Right > GetSystemMetrics(SM_CXSCREEN) then OffsetRect(Rc, GetSystemMetrics(SM_CXSCREEN) - Right, 0);
      BoundsRect := Rc;
    end;
  end;
end;

// TElAppBar.OnMoving ///////////////////////////////////////////////////////////

procedure TElAppBar.OnMoving(var Msg : TMessage);
var
  prc : PRect;
  pt : TSmallPoint;
  abEdgeProposed : TAppBarEdge;
  w, h : Integer;
begin
  // We control the moving of the AppBar.  For example, if the mouse moves
  // close to an edge, we want to dock the AppBar
  // The lParam contains the window's position proposed by the system
  prc := PRect(Msg.LParam);

  // Get the location of the mouse cursor
  pt := GetMessagePosition;

  // Where should the AppBar be based on the mouse position?
  abEdgeProposed := CalcProposedState(pt);

  if ((FabEdgeProposedPrev <> abeFloat) and (abEdgeProposed = abeFloat)) then
  begin
    // While moving, the user took us from a docked/autohidden state to
    // the float state.  We have to calculate a rectangle location so that
    // the mouse cursor stays inside the window.
    prc^ := FABS.rcFloat;
    if FKeepSize then
    begin
      w := prc.Right - prc.Left { + 1};
      h := prc.Bottom - prc.Top { + 1};
      with prc^ do
      begin
        Left := pt.X - FDraggingOffset.X;
        Top := pt.Y - FDraggingOffset.Y;
        Right := Left + w;
        Bottom := Top + h;
      end;
    end
    else
    begin
      w := prc^.Right - prc^.Left;
      h := prc^.Bottom - prc^.Top;
      with prc^ do
      begin
        Left := pt.X - w div 2;
        Top := pt.Y;
        Right := pt.X - w div 2 + w;
        Bottom := pt.Y + h;
      end;
    end;
  end;

  // Remember the most-recently proposed state
  FabEdgeProposedPrev := abEdgeProposed;

  // Tell the system where to move the window based on the proposed state
  GetRect(abEdgeProposed, prc^);

end;

// TElAppBar.OnSizing ///////////////////////////////////////////////////////////

procedure TElAppBar.OnSizing(var Msg : TMessage);
var
  prc : PRect;
  rcBorder : TRect;
  nWidthNew, nHeightNew : Integer;
begin
  // We control the sizing of the AppBar.  For example, if the user re-sizes
  // an edge, we want to change the size in descrete increments.
  // The lParam contains the window's position proposed by the system
  prc := PRect(Msg.LParam);

  // Get the minimum size of the window assuming it has no client area.
  // This is the width/height of the window that must always be present
  rcBorder := Rect(0, 0, 0, 0);
  AdjustWindowRectEx(rcBorder,
    GetWindowLong(Handle, GWL_STYLE),
    False,
    GetWindowLong(Handle, GWL_EXSTYLE));

  // We force the window to resize in discrete units set by the FszSizeInc
  // member.  From the new, proposed window dimensions passed to us, round
  // the width/height to the nearest discrete unit
  if FszSizeInc.cx <> 0 then
    nWidthNew := ((prc^.Right - prc^.Left) - (rcBorder.Right - rcBorder.Left)
      + FszSizeInc.cx div 2) div FszSizeInc.cx * FszSizeInc.cx
      + (rcBorder.Right - rcBorder.Left)
  else
    nWidthNew := prc^.Right - prc^.Left;

  if FszSizeInc.cy <> 0 then
    nHeightNew := ((prc^.Bottom - prc^.Top) - (rcBorder.Bottom - rcBorder.Top)
      + FszSizeInc.cy div 2) div FszSizeInc.cy * FszSizeInc.cy
      + (rcBorder.Bottom - rcBorder.Top)
  else
    nHeightNew := prc^.Bottom - prc^.Top;

  // Adjust the rectangle's dimensions
  case Msg.wParam of
    WMSZ_LEFT :
      prc^.Left := prc^.Right - nWidthNew;

    WMSZ_TOP :
      prc^.Top := prc^.Bottom - nHeightNew;

    WMSZ_RIGHT :
      prc^.Right := prc^.Left + nWidthNew;

    WMSZ_BOTTOM :
      prc^.Bottom := prc^.Top + nHeightNew;

    WMSZ_BOTTOMLEFT :
      begin
        prc^.Bottom := prc^.Top + nHeightNew;
        prc^.Left := prc^.Right - nWidthNew;
      end;

    WMSZ_BOTTOMRIGHT :
      begin
        prc^.Bottom := prc^.Top + nHeightNew;
        prc^.Right := prc^.Left + nWidthNew;
      end;

    WMSZ_TOPLEFT :
      begin
        prc^.Left := prc^.Right - nWidthNew;
        prc^.Top := prc^.Bottom - nHeightNew;
      end;

    WMSZ_TOPRIGHT :
      begin
        prc^.Top := prc^.Bottom - nHeightNew;
        prc^.Right := prc^.Left + nWidthNew;
      end;
  end; // end of case
end;

// TElAppBar.OnGetMinMaxInfo ////////////////////////////////////////////////////

procedure TElAppBar.OnGetMinMaxInfo(var Msg : TWMGetMinMaxInfo);
begin
  if GetEdge = abeFloat then
    with Msg.MinMaxInfo^ do
    begin
      ptMinTrackSize.X := FABS.nMinWidth;
      ptMinTrackSize.Y := FABS.nMinHeight;
      ptMaxTrackSize.X := FABS.nMaxWidth;
      ptMaxTrackSize.Y := FABS.nMaxHeight;
    end
  else
    with Msg.MinMaxInfo^ do
    begin
      ptMinTrackSize.X := 0;
      ptMinTrackSize.Y := 0;
      ptMaxTrackSize.X := GetSystemMetrics(SM_CXSCREEN);
      ptMaxTrackSize.Y := GetSystemMetrics(SM_CYSCREEN);
      if (not IsEdgeTopOrBottom(GetEdge)) and (not KeepSize) then
        ptMaxTrackSize.X := ptMaxTrackSize.X div 2;
      if (not IsEdgeLeftOrRight(GetEdge)) and (not KeepSize) then
        ptMaxTrackSize.Y := ptMaxTrackSize.Y div 2;
    end;
end;

{ AppBar-specific helper functions }

// TElAppBar.IsEdgeLeftOrRight //////////////////////////////////////////////////

function TElAppBar.IsEdgeLeftOrRight(abEdge : TAppBarEdge) : Boolean;
begin
  Result := (abEdge in [abeLeft, abeRight]);
end;

// TElAppBar.IsEdgeTopOrBottom //////////////////////////////////////////////////

function TElAppBar.IsEdgeTopOrBottom(abEdge : TAppBarEdge) : Boolean;
begin
  Result := (abEdge in [abeTop, abeBottom]);
end;

// TElAppBar.IsFloating /////////////////////////////////////////////////////////

function TElAppBar.IsFloating(abEdge : TAppBarEdge) : Boolean;
begin
  Result := (abEdge = abeFloat);
end;

// TElAppBar.IsDockable /////////////////////////////////////////////////////////

function TElAppBar.IsDockable(abFlags : TAppBarFlags) : Boolean;
begin
  Result := ((abFlags * [abfAllowLeft..abfAllowBottom]) <> []);
end;

// TElAppBar.IsDockableVertically ///////////////////////////////////////////////

function TElAppBar.IsDockableVertically(abFlags : TAppBarFlags) : Boolean;
begin
  Result := ((abFlags * [abfAllowLeft, abfAllowRight]) <> []);
end;

// TElAppBar.IsDockableHorizontally /////////////////////////////////////////////

function TElAppBar.IsDockableHorizontally(abFlags : TAppBarFlags) : Boolean;
begin
  Result := ((abFlags * [abfAllowTop, abfAllowBottom]) <> []);
end;

// TElAppBar.ResetSystemKnowledge ///////////////////////////////////////////////

procedure TElAppBar.ResetSystemKnowledge;
{$IFDEF DEBUG}
var
  abd : TAppBarData;
begin
  abd.cbSize := sizeof(abd);
  abd.hWnd := 0;
  SHAppBarMessage(ABM_REMOVE, abd);
end;
{$ELSE}
begin
  // nothing to do when not in debug mode
end;
{$ENDIF}

// TElAppBar.GetEdgeFromPoint ///////////////////////////////////////////////////

function TElAppBar.GetEdgeFromPoint(abFlags : TAppBarFlags;
  pt : TSmallPoint) : TAppBarEdge;
var
  rc : TRect;
  cxScreen : Integer;
  cyScreen : Integer;
  ptCenter : TSmallPoint;
  ptOffset : TSmallPoint;
  bIsLeftOrRight : Boolean;
  abSubstEdge : TAppBarEdge;
begin
  // Let's get floating out of the way first
  if abfAllowFloat in abFlags then
  begin

    // Get the rectangle that bounds the size of the screen
    // minus any docked (but not-autohidden) AppBars
    SystemParametersInfo(SPI_GETWORKAREA, 0, @rc, 0);

    // Leave a 1/2 width/height-of-a-scrollbar gutter around the workarea
    InflateRect(rc,
      -GetSystemMetrics(SM_CXVSCROLL),
      -GetSystemMetrics(SM_CYHSCROLL));

    // If the point is in the adjusted workarea OR no edges are allowed
    if PtInRect(rc, SmallPointToPoint(pt)) or
      not IsDockable(abFlags) then
    begin
      // The AppBar should float
      Result := abeFloat;
      Exit;
    end;
  end;

  // If we get here, the AppBar should be docked; determine the proper edge
  // Get the dimensions of the screen
  cxScreen := GetSystemMetrics(SM_CXSCREEN);
  cyScreen := GetSystemMetrics(SM_CYSCREEN);

  // Find the center of the screen
  ptCenter.X := cxScreen div 2;
  ptCenter.Y := cyScreen div 2;

  // Find the distance from the point to the center
  ptOffset.X := pt.X - ptCenter.X;
  ptOffset.Y := pt.Y - ptCenter.Y;

  // Determine if the point is farther from the left/right or top/bottom
  bIsLeftOrRight :=
    ((Abs(ptOffset.Y) * cxScreen) <= (Abs(ptOffset.X) * cyScreen));

  // Propose an edge
  if bIsLeftOrRight then
  begin
    if 0 <= ptOffset.X then
      Result := abeRight
    else
      Result := abeLeft;
  end
  else
  begin
    if 0 <= ptOffset.Y then
      Result := abeBottom
    else
      Result := abeTop;
  end;

  // Calculate an edge substitute
  if abfAllowFloat in abFlags then
    abSubstEdge := abeFloat
  else
    abSubstEdge := FABS.abEdge;

  // Check if the proposed edge is allowed. If not, return the edge substitute
  case Result of
    abeLeft :
      if not (abfAllowLeft in abFlags) then Result := abSubstEdge;
    abeTop :
      if not (abfAllowTop in abFlags) then Result := abSubstEdge;
    abeRight :
      if not (abfAllowRight in abFlags) then Result := abSubstEdge;
    abeBottom :
      if not (abfAllowBottom in abFlags) then Result := abSubstEdge;
  end;

end;

{ Public member functions }

// TElAppBar.Create /////////////////////////////////////////////////////////////

constructor TElAppBar.Create(Owner : TComponent);
begin
{$IFDEF BUILDER_USED}
  inherited;
{$ENDIF}
  // Force the shell to update its list of AppBars and the workarea.
  // This is a precaution and is very useful when debugging.  If you create
  // an AppBar and then just terminate the application, the shell still
  // thinks that the AppBar exists and the user's workarea is smaller than
  // it should be.  When a new AppBar is created, calling this function
  // fixes the user's workarea.
  ResetSystemKnowledge;

  // Set default state of AppBar to float with no width & height
  FABS.abEdge := abeFloat;
  FABS.bAutohide := False;
  FABS.bAlwaysOnTop := false;
  FABS.nTimerInterval := SLIDE_DEF_TIMER_INTERVAL;
  FABS.bSlideEffect := True;
  FABS.rcDockDims.Left := AB_DEF_DOCK_DIM;
  FABS.rcDockDims.Top := AB_DEF_DOCK_DIM;
  FABS.rcDockDims.Right := AB_DEF_DOCK_DIM;
  FABS.rcDockDims.Bottom := AB_DEF_DOCK_DIM;
  FABS.rcFloat.Left := 0;
  FABS.rcFloat.Top := 0;
  FABS.rcFloat.Right := 0;
  FABS.rcFloat.Bottom := 0;
  FABS.nMinWidth := 0;
  FABS.nMinHeight := 0;
  FABS.nMaxWidth := GetSystemMetrics(SM_CXSCREEN);
  FABS.nMaxHeight := GetSystemMetrics(SM_CYSCREEN);
  FABS.abTaskEntry := abtHide;
  FabFlags := [abfAllowLeft..abfAllowFloat];
  FszSizeInc.cx := AB_DEF_SIZE_INC;
  FszSizeInc.cy := AB_DEF_SIZE_INC;
  FabEdgeProposedPrev := abeUnknown;
  FbFullScreenAppOpen := False;
  FbAutoHideIsVisible := False;
  FPreventOffScreen := false;
{$IFNDEF BUILDER_USED}
  inherited;
{$ENDIF}
end;

// TElAppBar.Destroy ////////////////////////////////////////////////////////////

destructor TElAppBar.Destroy;
begin
  ResetSystemKnowledge;
  // Call base class
  inherited Destroy;
end;

// TElAppBar.UpdateBar //////////////////////////////////////////////////////////

procedure TElAppBar.UpdateBar;
begin
  SetEdge(GetEdge);
end;

// TAppBar.SetSlideTime ///////////////////////////////////////////////////////

procedure TElAppBar.SetSlideTime(nInterval : Integer);
begin
  FABS.nTimerInterval := nInterval;
  FTimer.Interval := nInterval;
end;

procedure TElAppBar.SetKeepSize(newValue : Boolean);
begin
  if (FKeepSize <> newValue) then
  begin
    if abfAllowFloat in Flags then
    begin
      FKeepSize := newValue;
      if GetEdge <> abeFloat then UpdateBar;
    end;
  end; {if}
end;

procedure TElAppBar.SetPreventOffScreen(newValue : Boolean);
begin
  if (FPreventOffScreen <> newValue) then
  begin
    FPreventOffScreen := newValue;
    UpdateBar;
  end; {if}
end;

procedure TElAppBar.SetHorzInc(newValue : integer);
begin
  FszSizeInc.cx := newValue;
end;

procedure TElAppBar.SetVertInc(newValue : integer);
begin
  FszSizeInc.cy := newValue;
end;

function  TElAppBar.GetVertInc : integer;
begin
  result := FszSizeInc.cy;
end;

function  TElAppBar.GetHorzInc : integer;
begin
  result := FszSizeInc.cx;
end;

procedure TElAppBar.DoEdgeChanged;
begin
  if Assigned (FOnEdgeChanged) then FOnEdgeChanged(Self); 
end;

procedure TElAppBar.CreateParams (var Params: TCreateParams);
var
  dwAdd, dwRemove, dwAddEx, dwRemoveEx : DWORD;
begin
  // Call the inherited first
  inherited CreateParams(Params);

  if not (csDesigning in ComponentState) then
  begin
    // Styles to be added
    dwAdd := WS_POPUP or WS_THICKFRAME;
    dwAddEx := WS_EX_TOOLWINDOW;

    // Styles to be removed
    dwRemove := WS_SYSMENU or WS_MAXIMIZEBOX or WS_MINIMIZEBOX;
    dwRemoveEx := WS_EX_APPWINDOW;

    // Modify style flags
    with Params do
    begin
      Style := Style and (not dwRemove);
      Style := Style or dwAdd;
      ExStyle := ExStyle and (not dwRemoveEx);
      ExStyle := ExStyle or dwAddEx;
    end;
  end;
end;

procedure TElAppBar.OnMouseMove(var Msg : TWMMouseMove);
begin
  ShowHiddenAppBar(True);
  inherited;
end;  { OnMouseMove }


end.

