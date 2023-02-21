// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxAppUtils.pas' rev: 35.00 (Windows)

#ifndef MxapputilsHPP
#define MxapputilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <System.IniFiles.hpp>
#include <Vcl.Grids.hpp>
#include <mxVCLUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxapputils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::UnicodeString __fastcall (*TOnGetDefaultIniName)(void);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TOnGetDefaultIniName OnGetDefaultIniName;
extern DELPHI_PACKAGE System::UnicodeString DefCompanyName;
extern DELPHI_PACKAGE bool RegUseAppTitle;
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetDefaultSection(System::Classes::TComponent* Component);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetDefaultIniName(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetDefaultIniRegKey(void);
extern DELPHI_PACKAGE void __fastcall GetDefaultIniData(Vcl::Controls::TControl* Control, System::UnicodeString &IniFileName, System::UnicodeString &Section, bool UseRegistry);
extern DELPHI_PACKAGE Vcl::Forms::TForm* __fastcall FindForm(Vcl::Forms::TFormClass FormClass);
extern DELPHI_PACKAGE Vcl::Forms::TForm* __fastcall FindShowForm(Vcl::Forms::TFormClass FormClass, const System::UnicodeString Caption);
extern DELPHI_PACKAGE bool __fastcall ShowDialog(Vcl::Forms::TFormClass FormClass);
extern DELPHI_PACKAGE Vcl::Forms::TForm* __fastcall InstantiateForm(Vcl::Forms::TFormClass FormClass, void *Reference);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrToIniStr(const System::UnicodeString Str);
extern DELPHI_PACKAGE System::UnicodeString __fastcall IniStrToStr(const System::UnicodeString Str);
extern DELPHI_PACKAGE System::UnicodeString __fastcall IniReadString(System::TObject* IniFile, const System::UnicodeString Section, const System::UnicodeString Ident, const System::UnicodeString Default);
extern DELPHI_PACKAGE void __fastcall IniWriteString(System::TObject* IniFile, const System::UnicodeString Section, const System::UnicodeString Ident, const System::UnicodeString Value);
extern DELPHI_PACKAGE int __fastcall IniReadInteger(System::TObject* IniFile, const System::UnicodeString Section, const System::UnicodeString Ident, int Default);
extern DELPHI_PACKAGE void __fastcall IniWriteInteger(System::TObject* IniFile, const System::UnicodeString Section, const System::UnicodeString Ident, int Value);
extern DELPHI_PACKAGE bool __fastcall IniReadBool(System::TObject* IniFile, const System::UnicodeString Section, const System::UnicodeString Ident, bool Default);
extern DELPHI_PACKAGE void __fastcall IniWriteBool(System::TObject* IniFile, const System::UnicodeString Section, const System::UnicodeString Ident, bool Value);
extern DELPHI_PACKAGE void __fastcall IniEraseSection(System::TObject* IniFile, const System::UnicodeString Section);
extern DELPHI_PACKAGE void __fastcall IniDeleteKey(System::TObject* IniFile, const System::UnicodeString Section, const System::UnicodeString Ident);
extern DELPHI_PACKAGE void __fastcall IniReadSections(System::TObject* IniFile, System::Classes::TStrings* Strings);
extern DELPHI_PACKAGE void __fastcall InternalSaveMDIChildren(Vcl::Forms::TForm* MainForm, System::TObject* IniFile);
extern DELPHI_PACKAGE void __fastcall InternalRestoreMDIChildren(Vcl::Forms::TForm* MainForm, System::TObject* IniFile);
extern DELPHI_PACKAGE void __fastcall SaveMDIChildrenReg(Vcl::Forms::TForm* MainForm, System::Win::Registry::TRegIniFile* IniFile);
extern DELPHI_PACKAGE void __fastcall RestoreMDIChildrenReg(Vcl::Forms::TForm* MainForm, System::Win::Registry::TRegIniFile* IniFile);
extern DELPHI_PACKAGE void __fastcall SaveMDIChildren(Vcl::Forms::TForm* MainForm, System::Inifiles::TIniFile* IniFile);
extern DELPHI_PACKAGE void __fastcall RestoreMDIChildren(Vcl::Forms::TForm* MainForm, System::Inifiles::TIniFile* IniFile);
extern DELPHI_PACKAGE void __fastcall InternalSaveGridLayout(Vcl::Grids::TCustomGrid* Grid, System::TObject* IniFile, const System::UnicodeString Section);
extern DELPHI_PACKAGE void __fastcall InternalRestoreGridLayout(Vcl::Grids::TCustomGrid* Grid, System::TObject* IniFile, const System::UnicodeString Section);
extern DELPHI_PACKAGE void __fastcall RestoreGridLayoutReg(Vcl::Grids::TCustomGrid* Grid, System::Win::Registry::TRegIniFile* IniFile);
extern DELPHI_PACKAGE void __fastcall SaveGridLayoutReg(Vcl::Grids::TCustomGrid* Grid, System::Win::Registry::TRegIniFile* IniFile);
extern DELPHI_PACKAGE void __fastcall RestoreGridLayout(Vcl::Grids::TCustomGrid* Grid, System::Inifiles::TIniFile* IniFile);
extern DELPHI_PACKAGE void __fastcall SaveGridLayout(Vcl::Grids::TCustomGrid* Grid, System::Inifiles::TIniFile* IniFile);
extern DELPHI_PACKAGE void __fastcall WriteFormPlacementReg(Vcl::Forms::TForm* Form, System::Win::Registry::TRegIniFile* IniFile, const System::UnicodeString Section);
extern DELPHI_PACKAGE void __fastcall WriteFormPlacement(Vcl::Forms::TForm* Form, System::Inifiles::TIniFile* IniFile, const System::UnicodeString Section);
extern DELPHI_PACKAGE void __fastcall SaveFormPlacement(Vcl::Forms::TForm* Form, const System::UnicodeString IniFileName, bool UseRegistry);
extern DELPHI_PACKAGE void __fastcall ReadFormPlacementReg(Vcl::Forms::TForm* Form, System::Win::Registry::TRegIniFile* IniFile, const System::UnicodeString Section, bool LoadState, bool LoadPosition);
extern DELPHI_PACKAGE void __fastcall ReadFormPlacement(Vcl::Forms::TForm* Form, System::Inifiles::TIniFile* IniFile, const System::UnicodeString Section, bool LoadState, bool LoadPosition);
extern DELPHI_PACKAGE void __fastcall RestoreFormPlacement(Vcl::Forms::TForm* Form, const System::UnicodeString IniFileName, bool UseRegistry);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetUniqueFileNameInDir(const System::UnicodeString Path, const System::UnicodeString FileNameMask);
extern DELPHI_PACKAGE void __fastcall AppBroadcast(int Msg, int wParam, int lParam);
extern DELPHI_PACKAGE void __fastcall AppTaskbarIcons(bool AppOnly);
}	/* namespace Mxapputils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXAPPUTILS)
using namespace Mxapputils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxapputilsHPP
