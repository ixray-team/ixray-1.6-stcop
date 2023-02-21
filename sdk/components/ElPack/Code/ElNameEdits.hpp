// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElNameEdits.pas' rev: 35.00 (Windows)

#ifndef ElnameeditsHPP
#define ElnameeditsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <ElPopBtn.hpp>
#include <ElTools.hpp>
#include <ElFolderDlg.hpp>
#include <System.Classes.hpp>
#include <ElShellUtils.hpp>
#include <ElBtnEdit.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.CommDlg.hpp>
#include <Vcl.Dialogs.hpp>
#include <ElEdits.hpp>
#include <Vcl.Controls.hpp>
#include <System.UITypes.hpp>
#include <ElXPThemedControl.hpp>
#include <ElVCLUtils.hpp>
#include <ElImgFrm.hpp>
#include <ElStrUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <ElSndMap.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elnameedits
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElFolderNameEdit;
class DELPHICLASS TElFileNameEdit;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TBeforeFileDialogEvent)(System::TObject* Sender, Vcl::Dialogs::TOpenDialog* Dialog);

typedef void __fastcall (__closure *TBeforeFolderDialogEvent)(System::TObject* Sender, Elfolderdlg::TElFolderDialog* Dialog);

class PASCALIMPLEMENTATION TElFolderNameEdit : public Elbtnedit::TCustomElButtonEdit
{
	typedef Elbtnedit::TCustomElButtonEdit inherited;
	
private:
	Elfolderdlg::TElFolderDialog* FileDlg;
	Elfolderdlg::TBrowseForFolderOptions __fastcall GetOptions();
	System::UnicodeString __fastcall GetTitle();
	void __fastcall SetOptions(Elfolderdlg::TBrowseForFolderOptions Value);
	void __fastcall SetTitle(const System::UnicodeString Value);
	void __fastcall SetRootFolder(Elshellutils::TShellFolders Value);
	Elshellutils::TShellFolders __fastcall GetRootFolder();
	
protected:
	System::UnicodeString FDialogTitle;
	System::Classes::TNotifyEvent FOnDialogExecute;
	TBeforeFolderDialogEvent FOnBeforeDialogExecute;
	void __fastcall BtnClick(System::TObject* Sender);
	virtual void __fastcall CreateHandle();
	virtual void __fastcall Loaded();
	void __fastcall SetStatusText(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetStatusText();
	System::UnicodeString __fastcall GetCustomRootFolder();
	void __fastcall SetCustomRootFolder(const System::UnicodeString Value);
	virtual void __fastcall TriggerBeforeDialogExecute(Elfolderdlg::TElFolderDialog* Dialog);
	virtual void __fastcall TriggerDialogExecute();
	
public:
	__fastcall virtual TElFolderNameEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElFolderNameEdit();
	
__published:
	__property Elfolderdlg::TBrowseForFolderOptions Options = {read=GetOptions, write=SetOptions, nodefault};
	__property System::UnicodeString Title = {read=GetTitle, write=SetTitle, stored=false};
	__property Elshellutils::TShellFolders RootFolder = {read=GetRootFolder, write=SetRootFolder, nodefault};
	__property System::UnicodeString DialogTitle = {read=GetTitle, write=SetTitle};
	__property System::UnicodeString StatusText = {read=GetStatusText, write=SetStatusText};
	__property System::UnicodeString CustomRootFolder = {read=GetCustomRootFolder, write=SetCustomRootFolder};
	__property TBeforeFolderDialogEvent OnBeforeDialogExecute = {read=FOnBeforeDialogExecute, write=FOnBeforeDialogExecute};
	__property System::Classes::TNotifyEvent OnDialogExecute = {read=FOnDialogExecute, write=FOnDialogExecute};
	__property ButtonVisible;
	__property TopMargin = {default=1};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property BorderSides;
	__property MaxLength = {default=0};
	__property Transparent;
	__property HandleDialogKeys = {default=0};
	__property HideSelection = {default=1};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property ImageForm;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property OnSelectionChange;
	__property Text;
	__property Flat = {default=0};
	__property ActiveBorderType = {default=1};
	__property InactiveBorderType = {default=3};
	__property UseBackground = {default=0};
	__property Alignment;
	__property AutoSelect = {default=0};
	__property Multiline = {default=0};
	__property ChangeDisabledText = {default=0};
	__property Background;
	__property ButtonClickSound = {default=0};
	__property ButtonDownSound = {default=0};
	__property ButtonUpSound = {default=0};
	__property ButtonSoundMap;
	__property ButtonColor;
	__property ButtonFlat;
	__property ButtonHint = {default=0};
	__property ButtonShortcut;
	__property ButtonGlyph;
	__property ButtonIcon;
	__property ButtonUseIcon;
	__property ButtonNumGlyphs;
	__property ButtonWidth;
	__property AltButtonCaption = {default=0};
	__property AltButtonClickSound = {default=0};
	__property AltButtonDownSound = {default=0};
	__property AltButtonUpSound = {default=0};
	__property AltButtonSoundMap;
	__property AltButtonDown;
	__property AltButtonEnabled;
	__property AltButtonFlat;
	__property AltButtonGlyph;
	__property AltButtonHint = {default=0};
	__property AltButtonIcon;
	__property AltButtonUseIcon;
	__property AltButtonNumGlyphs;
	__property AltButtonPopupPlace;
	__property AltButtonPosition = {default=1};
	__property AltButtonPullDownMenu;
	__property AltButtonShortcut;
	__property AltButtonVisible;
	__property AltButtonWidth;
	__property OnAltButtonClick;
	__property AutoSize = {default=1};
	__property BorderStyle;
	__property Ctl3D;
	__property ParentCtl3D = {default=1};
	__property Enabled = {default=1};
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property PopupMenu;
	__property Color = {default=-16777211};
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property ReadOnly = {default=0};
	__property OnEnter;
	__property OnExit;
	__property OnClick;
	__property OnDblClick;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnStartDrag;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElFolderNameEdit(HWND ParentWindow) : Elbtnedit::TCustomElButtonEdit(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TElFileDialogType : unsigned char { fdtOpen, fdtSave };

class PASCALIMPLEMENTATION TElFileNameEdit : public Elbtnedit::TCustomElButtonEdit
{
	typedef Elbtnedit::TCustomElButtonEdit inherited;
	
private:
	Vcl::Dialogs::TOpenDialog* FileDlg;
	int __fastcall GetFilterIndex();
	void __fastcall SetHistoryList(System::Classes::TStrings* Value);
	void __fastcall SetInitialDir(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetDefaultExt();
	void __fastcall SetDefaultExt(const System::UnicodeString Value);
	System::Classes::TStrings* __fastcall GetFiles();
	System::Classes::TStrings* __fastcall GetHistoryList();
	System::UnicodeString __fastcall GetInitialDir();
	System::Uitypes::TOpenOptions __fastcall GetOptions();
	void __fastcall SetOptions(System::Uitypes::TOpenOptions Value);
	void __fastcall SetFilterIndex(int Value);
	System::UnicodeString __fastcall GetTitle();
	void __fastcall SetTitle(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetFilter();
	void __fastcall SetFilter(const System::UnicodeString Value);
	
protected:
	System::UnicodeString FDialogTitle;
	TElFileDialogType FDialogType;
	System::Classes::TNotifyEvent FOnDialogExecute;
	bool FParseParameters;
	TBeforeFileDialogEvent FOnBeforeDialogExecute;
	void __fastcall BtnClick(System::TObject* Sender);
	virtual void __fastcall CreateHandle();
	virtual void __fastcall Loaded();
	virtual void __fastcall TriggerDialogExecute();
	virtual void __fastcall TriggerBeforeDialogExecute(Vcl::Dialogs::TOpenDialog* Dialog);
	
public:
	__fastcall virtual TElFileNameEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElFileNameEdit();
	__property System::Classes::TStrings* Files = {read=GetFiles};
	__property System::Classes::TStrings* HistoryList = {read=GetHistoryList, write=SetHistoryList};
	
__published:
	__property System::UnicodeString DefaultExt = {read=GetDefaultExt, write=SetDefaultExt};
	__property System::UnicodeString Filter = {read=GetFilter, write=SetFilter};
	__property int FilterIndex = {read=GetFilterIndex, write=SetFilterIndex, default=1};
	__property System::UnicodeString InitialDir = {read=GetInitialDir, write=SetInitialDir};
	__property System::Uitypes::TOpenOptions Options = {read=GetOptions, write=SetOptions, default=4};
	__property System::UnicodeString Title = {read=GetTitle, write=SetTitle, stored=false};
	__property System::UnicodeString DialogTitle = {read=GetTitle, write=SetTitle};
	__property TElFileDialogType DialogType = {read=FDialogType, write=FDialogType, nodefault};
	__property bool ParseParameters = {read=FParseParameters, write=FParseParameters, default=1};
	__property System::Classes::TNotifyEvent OnDialogExecute = {read=FOnDialogExecute, write=FOnDialogExecute};
	__property TBeforeFileDialogEvent OnBeforeDialogExecute = {read=FOnBeforeDialogExecute, write=FOnBeforeDialogExecute};
	__property ButtonVisible;
	__property TopMargin = {default=1};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property BorderSides;
	__property MaxLength = {default=0};
	__property Transparent;
	__property HandleDialogKeys = {default=0};
	__property HideSelection = {default=1};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property ImageForm;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property OnSelectionChange;
	__property Text;
	__property Flat = {default=0};
	__property ActiveBorderType = {default=1};
	__property InactiveBorderType = {default=3};
	__property UseBackground = {default=0};
	__property Alignment;
	__property AutoSelect = {default=0};
	__property Multiline = {default=0};
	__property ChangeDisabledText = {default=0};
	__property Background;
	__property ButtonClickSound = {default=0};
	__property ButtonDownSound = {default=0};
	__property ButtonUpSound = {default=0};
	__property ButtonSoundMap;
	__property ButtonColor;
	__property ButtonFlat;
	__property ButtonHint = {default=0};
	__property ButtonShortcut;
	__property ButtonGlyph;
	__property ButtonIcon;
	__property ButtonUseIcon;
	__property ButtonNumGlyphs;
	__property ButtonWidth;
	__property AltButtonCaption = {default=0};
	__property AltButtonClickSound = {default=0};
	__property AltButtonDownSound = {default=0};
	__property AltButtonUpSound = {default=0};
	__property AltButtonSoundMap;
	__property AltButtonDown;
	__property AltButtonEnabled;
	__property AltButtonFlat;
	__property AltButtonGlyph;
	__property AltButtonHint = {default=0};
	__property AltButtonIcon;
	__property AltButtonNumGlyphs;
	__property AltButtonPopupPlace;
	__property AltButtonPosition = {default=1};
	__property AltButtonPullDownMenu;
	__property AltButtonShortcut;
	__property AltButtonUseIcon;
	__property AltButtonVisible;
	__property AltButtonWidth;
	__property OnAltButtonClick;
	__property AutoSize = {default=1};
	__property BorderStyle;
	__property Ctl3D;
	__property ParentCtl3D = {default=1};
	__property Enabled = {default=1};
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property PopupMenu;
	__property Color = {default=-16777211};
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property ReadOnly = {default=0};
	__property OnEnter;
	__property OnExit;
	__property OnClick;
	__property OnDblClick;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnStartDrag;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElFileNameEdit(HWND ParentWindow) : Elbtnedit::TCustomElButtonEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elnameedits */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELNAMEEDITS)
using namespace Elnameedits;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElnameeditsHPP
