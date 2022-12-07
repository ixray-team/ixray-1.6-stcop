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

{$R ElNameEdits.res}

(*

Version History

07/08/2002

  Added OnBeforeDialogExecute and OnDialogExecute events

06/05/2002

  Button width was not preserved. Fixed.

05/12/2002

  Fixed dialog type in FileNameEdit (dialog of wrong type was created) 

04/19/2002

  Optimized behaviour of Title/DialogTitle properties. Title is the same as
  DialogTitle now

04/15/2002

  OnDialogExecute event added to TElFileNameEdit

04/07/2002

  Corrected error that caused an AV when pressing button

04/02/2002

  OnChange is called now after the dialog is executed and the user presses ok in the dialog

03/28/2002

  Added an option to show open OR save dialog in FileNameEdit

03/03/2002

  DirExists failed on empty DirName parameter. Fixed.

01/18/2002

  ButtonWidth and ButtonNumGlyphs properties made visible

01/01/2002

  Fixed some problems with painting borders when focus is moved

12/27/2001

  It's possible to set custom glyphs to buttons now.

12/16/2001

  When TElFolderNameEdit was placed to the form other than main, main form
  was activated after showing folder dialog. Fixed.


12/12/2001

  Fixed work with empty strings (spoiled in 3.02)

10/12/2001

   Now ElFilenameEdit correctly processes strings with parameters

*)

unit ElNameEdits;

interface

uses SysUtils,
     ElPopBtn,
     ElTools,
     ElFolderDlg,
     Classes,
     ElShellUtils,
     ElBtnEdit,
     Forms,
     CommDlg,
{$ifdef VCL_6_USED}
     Types,
{$endif}
     Dialogs;

type

     TBeforeFileDialogEvent = procedure(Sender : TObject; Dialog : TOpenDialog) of object;
     TBeforeFolderDialogEvent = procedure(Sender : TObject; Dialog : TElFolderDialog) of object;

     TElFolderNameEdit = class(TCustomElButtonEdit)
     private
       FileDlg : TElFolderDialog;
       function GetOptions: TBrowseForFolderOptions;
       function GetTitle: string;
       procedure SetOptions(Value: TBrowseForFolderOptions);
       procedure SetTitle(const Value: string);
       procedure SetRootFolder(Value: TShellFolders);
       function GetRootFolder: TShellFolders;
     protected
       FDialogTitle: string;
       FOnDialogExecute: TNotifyEvent;
       FOnBeforeDialogExecute: TBeforeFolderDialogEvent;
       procedure BtnClick(Sender : TObject);
       procedure CreateHandle; override;
       procedure Loaded; override;
       procedure SetStatusText(const Value: string);
       function GetStatusText: string;
       function GetCustomRootFolder: string;
       procedure SetCustomRootFolder(const Value: string);
       procedure TriggerBeforeDialogExecute(Dialog : TElFolderDialog); virtual;
       procedure TriggerDialogExecute; virtual;
     public
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;
     published
       property Options: TBrowseForFolderOptions read GetOptions write SetOptions;
       property Title: string read GetTitle write SetTitle stored false;
       property RootFolder: TShellFolders read GetRootFolder write SetRootFolder;
       property DialogTitle: string read GetTitle write SetTitle;
       property StatusText: string read GetStatusText write SetStatusText;
       property CustomRootFolder: string read GetCustomRootFolder write
           SetCustomRootFolder;
       property OnBeforeDialogExecute: TBeforeFolderDialogEvent read
           FOnBeforeDialogExecute write FOnBeforeDialogExecute;
       property OnDialogExecute: TNotifyEvent read FOnDialogExecute write
           FOnDialogExecute;

       property ButtonVisible;
       property TopMargin;
       property LeftMargin;
       property RightMargin;
       property BorderSides;
       property MaxLength;
       property Transparent;
       property HandleDialogKeys;
       property HideSelection;
       property LineBorderActiveColor;
       property LineBorderInactiveColor;
       {$ifdef ELPACK_COMPLETE}
       property ImageForm;
       {$endif}
       
       property OnMouseEnter;
       property OnMouseLeave;
       property OnResize;
       property OnChange;
       property OnSelectionChange;

       property Text;

       // inherited
       property Flat;
       property ActiveBorderType;
       property InactiveBorderType;
       property UseBackground;
       property Alignment;
       property AutoSelect;
       property Multiline;
       property ChangeDisabledText;

       property Background;
       property ButtonClickSound;
       property ButtonDownSound;
       property ButtonUpSound;
   {$IFDEF USE_SOUND_MAP}
       property ButtonSoundMap;
   {$ENDIF}
       property ButtonColor;
       property ButtonFlat;
       property ButtonHint;
       property ButtonShortcut;
       property ButtonGlyph;
       property ButtonIcon;
       property ButtonUseIcon;
       property ButtonNumGlyphs; 
       property ButtonWidth;

       property AltButtonCaption;
       property AltButtonClickSound;
       property AltButtonDownSound;
       property AltButtonUpSound;
   {$IFDEF USE_SOUND_MAP}
       property AltButtonSoundMap;
   {$ENDIF}
       property AltButtonDown;
       property AltButtonEnabled;
       property AltButtonFlat;
       property AltButtonGlyph;
       property AltButtonHint;
       property AltButtonIcon;
       property AltButtonUseIcon;
       property AltButtonNumGlyphs;
       property AltButtonPopupPlace;
       property AltButtonPosition;
       property AltButtonPullDownMenu;
       property AltButtonShortcut;
       property AltButtonVisible;
       property AltButtonWidth;
       property OnAltButtonClick;

       // VCL properties
       property AutoSize;
       property BorderStyle;
       property Ctl3D;
       property ParentCtl3D;
       property Enabled;
       property TabStop default True;
       property TabOrder;
       property PopupMenu;
       property Color;
       property ParentColor;
       property Align;
       property Font;
       property ParentFont;
       property ParentShowHint;
       property ShowHint;
       property Visible;
       property ReadOnly;

       property OnEnter;
       property OnExit;
       property OnClick;
       property OnDblClick;
       property OnKeyDown;
       property OnKeyPress;
       property OnKeyUp;
       property OnStartDrag;
       property OnDragDrop;
       property OnDragOver;
       {$IFDEF VCL_4_USED}
       property OnEndDock;
       {$ENDIF}
       property OnEndDrag;
       property OnMouseDown;
       property OnMouseMove;
       property OnMouseUp;
   {$IFDEF VCL_4_USED}
       property OnStartDock;
   {$ENDIF}

   {$IFDEF VCL_4_USED}
       property Anchors;
       property Constraints;
       property DockOrientation;
       property Floating;
       property DoubleBuffered;
       property DragKind;
   {$ENDIF}
     end;

     TElFileDialogType = (fdtOpen, fdtSave);

     TElFileNameEdit = class(TCustomElButtonEdit)
     private
       FileDlg : TOpenDialog;
       function GetFilterIndex: Integer;
       procedure SetHistoryList(Value: TStrings);
       procedure SetInitialDir(const Value: string);
       function GetDefaultExt: string;
       procedure SetDefaultExt(const Value: string);
       function GetFiles: TStrings;
       function GetHistoryList: TStrings;
       function GetInitialDir: string;
       function GetOptions: TOpenOptions;
       procedure SetOptions(Value: TOpenOptions);
       procedure SetFilterIndex(Value: Integer);
       function GetTitle: string;
       procedure SetTitle(const Value: string);
       function GetFilter: string;
       procedure SetFilter(const Value: string);
     protected
       FDialogTitle: string;
       FDialogType: TElFileDialogType;
       FOnDialogExecute: TNotifyEvent;
       FParseParameters: Boolean;
       FOnBeforeDialogExecute: TBeforeFileDialogEvent;
       procedure BtnClick(Sender : TObject);
       procedure CreateHandle; override;
       procedure Loaded; override;
       procedure TriggerDialogExecute; virtual;
       procedure TriggerBeforeDialogExecute(Dialog : TOpenDialog); virtual;
     public
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;
       property Files: TStrings read GetFiles;
       property HistoryList: TStrings read GetHistoryList write SetHistoryList;
     published
       property DefaultExt: string read GetDefaultExt write SetDefaultExt;
       property Filter: string read GetFilter write SetFilter;
       property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default
           1;
       property InitialDir: string read GetInitialDir write SetInitialDir;
       property Options: TOpenOptions read GetOptions write SetOptions
         default [ofHideReadOnly{$ifdef VCL_4_USED}, ofEnableSizing{$endif}];
       property Title: string read GetTitle write SetTitle stored false;
       property DialogTitle: string read GetTitle write SetTitle;
       property DialogType: TElFileDialogType read FDialogType write FDialogType;
       property ParseParameters: Boolean read FParseParameters write FParseParameters default true;
       property OnDialogExecute: TNotifyEvent read FOnDialogExecute write FOnDialogExecute;
       property OnBeforeDialogExecute: TBeforeFileDialogEvent read
           FOnBeforeDialogExecute write FOnBeforeDialogExecute;

       property ButtonVisible;
       property TopMargin;
       property LeftMargin;
       property RightMargin;
       property BorderSides;
       property MaxLength;
       property Transparent;
       property HandleDialogKeys;
       property HideSelection;
       property LineBorderActiveColor;
       property LineBorderInactiveColor;
       {$ifdef ELPACK_COMPLETE}
       property ImageForm;
       {$endif}

       property OnMouseEnter;
       property OnMouseLeave;
       property OnResize;
       property OnChange;
       property OnSelectionChange;
       property Text;

       // inherited
       property Flat;
       property ActiveBorderType;
       property InactiveBorderType;
       property UseBackground;
       property Alignment;
       property AutoSelect;
       property Multiline;
       property ChangeDisabledText;

       property Background;
       property ButtonClickSound;
       property ButtonDownSound;
       property ButtonUpSound;
   {$IFDEF USE_SOUND_MAP}
       property ButtonSoundMap;
   {$ENDIF}
       property ButtonColor;
       property ButtonFlat;
       property ButtonHint;
       property ButtonShortcut;
       property ButtonGlyph;
       property ButtonIcon;
       property ButtonUseIcon;
       property ButtonNumGlyphs; 
       property ButtonWidth;

       property AltButtonCaption;
       property AltButtonClickSound;
       property AltButtonDownSound;
       property AltButtonUpSound;
   {$IFDEF USE_SOUND_MAP}
       property AltButtonSoundMap;
   {$ENDIF}
       property AltButtonDown;
       property AltButtonEnabled;
       property AltButtonFlat;
       property AltButtonGlyph;
       property AltButtonHint;
       property AltButtonIcon;
       property AltButtonNumGlyphs;
       property AltButtonPopupPlace;
       property AltButtonPosition;
       property AltButtonPullDownMenu;
       property AltButtonShortcut;
       property AltButtonUseIcon;
       property AltButtonVisible;
       property AltButtonWidth;
       property OnAltButtonClick;

       // VCL properties
       property AutoSize;
       property BorderStyle;
       property Ctl3D;
       property ParentCtl3D;
       property Enabled;
       property TabStop default True;
       property TabOrder;
       property PopupMenu;
       property Color;
       property ParentColor;
       property Align;
       property Font;
       property ParentFont;
       property ParentShowHint;
       property ShowHint;
       property Visible;
       property ReadOnly;

       property OnEnter;
       property OnExit;
       property OnClick;
       property OnDblClick;
       property OnKeyDown;
       property OnKeyPress;
       property OnKeyUp;
       property OnStartDrag;
       property OnDragDrop;
       property OnDragOver;
       {$IFDEF VCL_4_USED}
       property OnEndDock;
       {$ENDIF}
       property OnEndDrag;
       property OnMouseDown;
       property OnMouseMove;
       property OnMouseUp;
   {$IFDEF VCL_4_USED}
       property OnStartDock;
   {$ENDIF}

   {$IFDEF VCL_4_USED}
       property Anchors;
       property Constraints;
       property DockOrientation;
       property Floating;
       property DoubleBuffered;
       property DragKind;
   {$ENDIF}
     end;

implementation

uses
  ElStrUtils;
type
    THackElEditBtn = class(TCustomElGraphicButton);
    TElFileNameDialog = class(TSaveDialog)
      function Execute: Boolean; override;
    end;

constructor TElFileNameEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FParseParameters := true;
  FileDlg := TElFileNameDialog.Create(Self);
  THackElEditBtn(FButton).Glyph.LoadFromResourceName(HInstance, 'ELFIlENAMEEDITBUTTON');
  THackElEditBtn(FButton).OnClick := BtnClick;
end; { Create }

function GetParamStr(P: PElFChar; var Param: String): PElFChar;
var
  Len: Integer;
  Buffer: array[0..4095] of TElFChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  while (P[0] > ' ') and (Len < SizeOf(Buffer)) do
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Buffer[Len] := P[0];
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then Inc(P);
    end else
    begin
      Buffer[Len] := P[0];
      Inc(Len);
      Inc(P);
    end;
  SetString(Param, Buffer, Len);
  Result := P;
end;

procedure TElFileNameEdit.BtnClick(Sender : TObject);
var AFileName : String;
    APath,
    AParams   : string;

begin
  AParams := '';
  AFileName := Text;
  if ((not FileExists(AFileName)) and (Trim(AFilename) <> '')) then
  begin
    if ParseParameters then
    begin
      GetParamStr(PElFChar(Text), AFileName);
      AParams := Copy(Text, Length(AFileName) + 1, Length(Text));
    end
    else
    begin
      AParams := '';
    end;
    APath := ExtractFilePath(AFileName);
    if DirExists(APath) then
      SetInitialDir(APath);
  end
  else
    SetInitialDir(ExtractFilePath(AFileName));
  AFileName := ExtractFileName(AFileName);
  FileDlg.FileName := AFileName;
  FileDlg.Title := DialogTitle;

  TriggerBeforeDialogExecute(FileDlg);
  
  if FileDlg.Execute then
  begin
    Text := FileDlg.FileName + AParams;
    TriggerDialogExecute;
    Change;
  end;
end;

destructor TElFileNameEdit.Destroy;
begin
  FileDlg.Free;
  inherited;
end;

function TElFileNameEdit.GetFilterIndex: Integer;
begin
  Result := FileDlg.FilterIndex;
end;

procedure TElFileNameEdit.SetHistoryList(Value: TStrings);
begin
  FileDlg.HistoryList := Value;
end;

procedure TElFileNameEdit.SetInitialDir(const Value: string);
begin
  FileDlg.InitialDir := Value;
end;

function TElFileNameEdit.GetDefaultExt: string;
begin
  Result := FileDlg.DefaultExt;
end;

procedure TElFileNameEdit.SetDefaultExt(const Value: string);
begin
  FileDlg.DefaultExt := Value;
end;

function TElFileNameEdit.GetFiles: TStrings;
begin
  Result := FileDlg.Files;
end;

function TElFileNameEdit.GetHistoryList: TStrings;
begin
  Result := FileDlg.HistoryList;
end;

function TElFileNameEdit.GetInitialDir: string;
begin
  Result := FileDlg.InitialDir;
end;

function TElFileNameEdit.GetOptions: TOpenOptions;
begin
  Result := FileDlg.Options;
end;

procedure TElFileNameEdit.SetOptions(Value: TOpenOptions);
begin
  FileDlg.Options := Value;
end;

procedure TElFileNameEdit.SetFilterIndex(Value: Integer);
begin
  FileDlg.FilterIndex := Value;
end;

function TElFileNameEdit.GetTitle: string;
begin
  Result := FDialogTitle;
end;

procedure TElFileNameEdit.SetTitle(const Value: string);
begin
  FileDlg.Title := Value;
  FDialogTitle := Value;
end;

function TElFileNameEdit.GetFilter: string;
begin
  Result := FileDlg.Filter;
end;

procedure TElFileNameEdit.SetFilter(const Value: string);
begin
  FileDlg.Filter := Value;
end;

procedure TElFileNameEdit.CreateHandle;
begin
  inherited;
  if (THackElEditBtn(FButton).Width <> 0) and (THackElEditBtn(FButton).Width < ButtonGlyph.Width + 2) then
    THackElEditBtn(FButton).Width := ClientHeight;
  SetEditRect(ClientRect);
end;

procedure TElFileNameEdit.Loaded;
begin
  inherited;
  if THackElEditBtn(FButton).Glyph.Empty then
    THackElEditBtn(FButton).Glyph.LoadFromResourceName(HInstance, 'ELFILENAMEEDITBUTTON');
end;

procedure TElFileNameEdit.TriggerDialogExecute;
begin
  if Assigned(FOnDialogExecute) then
    FOnDialogExecute(Self);
end;

procedure TElFileNameEdit.TriggerBeforeDialogExecute(Dialog : TOpenDialog);
begin
  if assigned(FOnBeforeDialogExecute) then
    FOnBeforeDialogExecute(Self, Dialog); 
end;

constructor TElFolderNameEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FileDlg := TElFolderDialog.Create(nil);
  THackElEditBtn(FButton).Glyph.LoadfromResourceName(HInstance, 'ELFOLDERNAMEEDITBUTTON');
  THackElEditBtn(FButton).OnClick := BtnClick;
end; { Create }

destructor TElFolderNameEdit.Destroy;
begin
  FileDlg.Free;
  inherited;
end;

procedure TElFolderNameEdit.BtnClick(Sender : TObject);
var AForm : TCustomForm;
begin
  FileDlg.Folder := Text;
  FileDlg.DialogTitle := DialogTitle;
  FileDlg.Parent := GetParentForm(Self);
  TriggerBeforeDialogExecute(FileDlg);
  if FileDlg.Execute then
  begin
    Text := FileDlg.Folder;
    TriggerDialogExecute;
    AForm := GetParentForm(Self);
    if AForm <> nil then AForm.BringToFront;
    Change;
  end;
end;

function TElFolderNameEdit.GetOptions: TBrowseForFolderOptions;
begin
  Result := FileDlg.Options;
end;

function TElFolderNameEdit.GetTitle: string;
begin
  Result := FDialogTitle;
end;

procedure TElFolderNameEdit.SetOptions(Value: TBrowseForFolderOptions);
begin
  FileDlg.Options := Value;
end;

procedure TElFolderNameEdit.SetTitle(const Value: string);
begin
  FileDlg.DialogTitle := Value;
  FDialogTitle := Value;
end;

procedure TElFolderNameEdit.SetRootFolder(Value: TShellFolders);
begin
  FileDlg.RootFolder := Value;
end;

function TElFolderNameEdit.GetRootFolder: TShellFolders;
begin
  Result := FileDlg.RootFolder;
end;

procedure TElFolderNameEdit.CreateHandle;
begin
  inherited;
  if (THackElEditBtn(FButton).Width <> 0) and (THackElEditBtn(FButton).Width < ButtonGlyph.Width + 2) then
    THackElEditBtn(FButton).Width := ClientHeight;
  SetEditRect(ClientRect);
end;

procedure TElFolderNameEdit.Loaded;
begin
  inherited;
  if THackElEditBtn(FButton).Glyph.Empty then
    THackElEditBtn(FButton).Glyph.LoadFromResourceName(HInstance, 'ELFOLDERNAMEEDITBUTTON');
end;

procedure TElFolderNameEdit.SetStatusText(const Value: string);
begin
  FileDlg.StatusText := Value;
end;

function TElFolderNameEdit.GetStatusText: string;
begin
  Result := FileDlg.StatusText;
end;

function TElFolderNameEdit.GetCustomRootFolder: string;
begin
  Result := FileDlg.CustomRootFolder;
end;

procedure TElFolderNameEdit.SetCustomRootFolder(const Value: string);
begin
  FileDlg.CustomRootFolder := Value;
end;

procedure TElFolderNameEdit.TriggerBeforeDialogExecute(Dialog : 
    TElFolderDialog);
begin
  if assigned(FOnBeforeDialogExecute) then
    FOnBeforeDialogExecute(Self, Dialog);
end;

procedure TElFolderNameEdit.TriggerDialogExecute;
begin
  if Assigned(FOnDialogExecute) then
    FOnDialogExecute(Self);
end;

function TElFileNameDialog.Execute: Boolean;
begin
  if TElFileNameEdit(Owner).DialogType = fdtOpen then
    Result := DoExecute(@GetOpenFileName)
  else
    Result := DoExecute(@GetSaveFileName);
end;

end.

