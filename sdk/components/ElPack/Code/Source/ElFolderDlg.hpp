// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElFolderDlg.pas' rev: 34.00 (Windows)

#ifndef ElfolderdlgHPP
#define ElfolderdlgHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.ActiveX.hpp>
#include <Winapi.ShlObj.hpp>
#include <ElShellUtils.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------
typedef int (CALLBACK* BFFCALLBACK)(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData);
typedef struct _browseinfoA {
    HWND hwndOwner;
    PItemIDList pidlRoot;
    LPTSTR pszDisplayName;
    LPCTSTR lpszTitle;
    UINT ulFlags;
    BFFCALLBACK lpfn;
    LPARAM lParam;
    int iImage;
} BROWSEINFO, *PBROWSEINFO, *LPBROWSEINFO;

namespace Elfolderdlg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElFolderDialog;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBrowseForFolderOption : unsigned char { bfoFileSysDirsOnly, bfoDontGoBelowDomain, bfoStatusText, bfoFileSysAncestors, bfoBrowseForComputer, bfBrowseForPrinter, bfoBrowseIncludeFiles };

typedef System::Set<TBrowseForFolderOption, TBrowseForFolderOption::bfoFileSysDirsOnly, TBrowseForFolderOption::bfoBrowseIncludeFiles> TBrowseForFolderOptions;

class PASCALIMPLEMENTATION TElFolderDialog : public Vcl::Dialogs::TCommonDialog
{
	typedef Vcl::Dialogs::TCommonDialog inherited;
	
private:
	void *FXDefWndProc;
	void *ObjInstance;
	_browseinfoW FBrowseInfo;
	Vcl::Stdctrls::TButton* FCustBtn;
	System::UnicodeString FCustomButtonCaption;
	System::UnicodeString FDialogTitle;
	System::StaticArray<System::WideChar, 261> FDisplayName;
	System::UnicodeString FFolder;
	_ITEMIDLIST *FFolderPIDL;
	int FHandle;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnCustomButtonClick;
	TBrowseForFolderOptions FOptions;
	Vcl::Controls::TWinControl* FParent;
	Elshellutils::TShellFolders FRootFolder;
	bool FShowCustomButton;
	bool JustInit;
	bool OriginalSelect;
	bool FVisible;
	System::UnicodeString __fastcall GetFolder();
	Vcl::Controls::TWinControl* __fastcall GetParent();
	void __fastcall SetFolder(const System::UnicodeString Value);
	void __fastcall SetParent(Vcl::Controls::TWinControl* Value);
	void __fastcall SetRootFolder(Elshellutils::TShellFolders Value);
	
protected:
	System::UnicodeString FCustomRootFolder;
	System::UnicodeString FStatusText;
	System::UnicodeString FPrompt;
	int __fastcall Perform(unsigned Msg, int WParam, int LParam);
	void __fastcall SetStatusText(const System::UnicodeString Value);
	
public:
	__fastcall virtual TElFolderDialog(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElFolderDialog();
	virtual void __fastcall DefaultHandler(void *Message);
	virtual bool __fastcall Execute()/* overload */;
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetSelectionPIDL(Winapi::Shlobj::PItemIDList PIDL);
	void __fastcall WinInitialized(int Param);
	void __fastcall WinSelChanged(int Param);
	__property int Handle = {read=FHandle, nodefault};
	__property Winapi::Shlobj::PItemIDList SelectionPIDL = {read=FFolderPIDL};
	
__published:
	__property System::UnicodeString DialogTitle = {read=FDialogTitle, write=FDialogTitle};
	__property System::UnicodeString CustomButtonCaption = {read=FCustomButtonCaption, write=FCustomButtonCaption};
	__property System::UnicodeString Folder = {read=GetFolder, write=SetFolder};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnCustomButtonClick = {read=FOnCustomButtonClick, write=FOnCustomButtonClick};
	__property TBrowseForFolderOptions Options = {read=FOptions, write=FOptions, nodefault};
	__property Vcl::Controls::TWinControl* Parent = {read=GetParent, write=SetParent};
	__property Elshellutils::TShellFolders RootFolder = {read=FRootFolder, write=SetRootFolder, nodefault};
	__property bool ShowCustomButton = {read=FShowCustomButton, write=FShowCustomButton, nodefault};
	__property System::UnicodeString CustomRootFolder = {read=FCustomRootFolder, write=FCustomRootFolder};
	__property System::UnicodeString StatusText = {read=FStatusText, write=SetStatusText};
	__property System::UnicodeString Prompt = {read=FPrompt, write=FPrompt};
	/* Hoisted overloads: */
	
public:
	inline bool __fastcall  Execute(HWND ParentWnd){ return Vcl::Dialogs::TCommonDialog::Execute(ParentWnd); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elfolderdlg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELFOLDERDLG)
using namespace Elfolderdlg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElfolderdlgHPP
