// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElImgLst.pas' rev: 35.00 (Windows)

#ifndef ElimglstHPP
#define ElimglstHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Consts.hpp>
#include <Winapi.CommCtrl.hpp>
#include <Vcl.Graphics.hpp>
#include <ElTools.hpp>
#include <Vcl.ImgList.hpp>
#include <System.ImageList.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elimglst
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElImageList;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElImageList : public Vcl::Controls::TImageList
{
	typedef Vcl::Controls::TImageList inherited;
	
public:
	void __fastcall ReadImg(System::Classes::TStream* Stream);
	void __fastcall WriteImg(System::Classes::TStream* Stream);
	HIDESBASE bool __fastcall Equal(TElImageList* IL);
	__fastcall virtual TElImageList(System::Classes::TComponent* AOwner);
	
protected:
	void __fastcall GetFullImages(Vcl::Graphics::TBitmap* Image, Vcl::Graphics::TBitmap* Mask);
	HIDESBASE void __fastcall ReadLeft(System::Classes::TReader* Reader);
	HIDESBASE void __fastcall ReadTop(System::Classes::TReader* Reader);
	HIDESBASE void __fastcall WriteLeft(System::Classes::TWriter* Writer);
	HIDESBASE void __fastcall WriteTop(System::Classes::TWriter* Writer);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
public:
	/* TCustomImageList.CreateSize */ inline __fastcall TElImageList(int AWidth, int AHeight) : Vcl::Controls::TImageList(AWidth, AHeight) { }
	/* TCustomImageList.Destroy */ inline __fastcall virtual ~TElImageList() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int __fastcall DecodeRLE(const void * Source, const void * Target, unsigned Count, unsigned ColorDepth);
extern DELPHI_PACKAGE int __fastcall EncodeRLE(const System::PByte Source, const System::PByte Target, int Count, int BPP);
}	/* namespace Elimglst */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELIMGLST)
using namespace Elimglst;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElimglstHPP
