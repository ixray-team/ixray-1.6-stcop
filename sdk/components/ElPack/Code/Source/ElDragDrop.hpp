// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDragDrop.pas' rev: 34.00 (Windows)

#ifndef EldragdropHPP
#define EldragdropHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <ElTools.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Clipbrd.hpp>
#include <Winapi.ActiveX.hpp>
#include <ElCBFmts.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElXPThemedControl.hpp>
#include <ElHook.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldragdrop
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TEnumFormatEtc;
class DELPHICLASS TOleDragObject;
class DELPHICLASS IElDropSource;
class DELPHICLASS IElDropTarget;
class DELPHICLASS TElDropTarget;
class DELPHICLASS TElDragDrop;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDragType : unsigned char { dtCopy, dtMove, dtLink, dtNone };

typedef System::Set<TDragType, TDragType::dtCopy, TDragType::dtNone> TDragTypes;

enum DECLSPEC_DENUM TDragResult : unsigned char { drDropCopy, drDropMove, drDropLink, drCancel, drOutMemory, drUnknown };

enum DECLSPEC_DENUM TDragContent : unsigned char { edcText, edcBitmap, edcMetafile, edcFileList, edcOther };

typedef void __fastcall (__closure *TTargetDragEvent)(System::TObject* Sender, System::Uitypes::TDragState State, TOleDragObject* Source, System::Classes::TShiftState Shift, int X, int Y, TDragType &DragType);

typedef void __fastcall (__closure *TTargetDropEvent)(System::TObject* Sender, TOleDragObject* Source, System::Classes::TShiftState Shift, int X, int Y, TDragType &DragType);

typedef void __fastcall (__closure *TSourceDragEvent)(System::TObject* Sender, TDragType DragType, System::Classes::TShiftState shift, bool &ContinueDrop);

typedef void __fastcall (__closure *TSourceDropEvent)(System::TObject* Sender, TDragResult DragResult);

typedef void __fastcall (__closure *TOleStartDragEvent)(System::TObject* Sender, void * &DragData, int &DragDataType, int &DragDataSize);

typedef System::StaticArray<tagFORMATETC, 256> TFormatList;

typedef TFormatList *pFormatList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TEnumFormatEtc : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TFormatList *fFormatList;
	int fFormatCount;
	int fIndex;
	
public:
	__fastcall TEnumFormatEtc(pFormatList FormatList, int FormatCount, int Index);
	HRESULT __stdcall Next(int Celt, /* out */ void *Elt, System::PLongInt pCeltFetched);
	HRESULT __stdcall Skip(int Celt);
	HRESULT __stdcall Reset();
	HRESULT __stdcall Clone(/* out */ _di_IEnumFORMATETC &Enum);
	__fastcall virtual ~TEnumFormatEtc();
private:
	void *__IEnumFORMATETC;	// IEnumFORMATETC 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000103-0000-0000-C000-000000000046}
	operator _di_IEnumFORMATETC()
	{
		_di_IEnumFORMATETC intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IEnumFORMATETC*(void) { return (IEnumFORMATETC*)&__IEnumFORMATETC; }
	#endif
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TOleDragObject : public Vcl::Controls::TDragObject
{
	typedef Vcl::Controls::TDragObject inherited;
	
private:
	_di_IDataObject dataObj;
	int Fkeys;
	bool FDown;
	System::UnicodeString FString;
	System::Classes::TStringList* FList;
	
protected:
	System::Classes::TStringList* __fastcall GetFileList();
	System::UnicodeString __fastcall GetString();
	Vcl::Graphics::TBitmap* __fastcall GetBitmap();
	TDragContent __fastcall GetDragContent();
	__property int Keys = {read=Fkeys, nodefault};
	
public:
	__fastcall TOleDragObject();
	__fastcall virtual ~TOleDragObject();
	void * __fastcall GetFormatData(int Format);
	bool __fastcall HasDataFormat(int Format);
	_di_IDataObject __fastcall DataObject();
	__property TDragContent Content = {read=GetDragContent, nodefault};
	__property System::Classes::TStringList* FileList = {read=GetFileList};
	__property System::UnicodeString StringData = {read=GetString};
	__property Vcl::Graphics::TBitmap* Bitmap = {read=GetBitmap};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION IElDropSource : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TElDragDrop* FOwner;
	
protected:
	HRESULT __stdcall QueryContinueDrag(System::LongBool FEscapePressed, int GrfKeyState);
	HRESULT __stdcall GiveFeedback(int dwEffect);
	HRESULT __stdcall GetData(const tagFORMATETC &FormatEtcIn, /* out */ tagSTGMEDIUM &Medium);
	HRESULT __stdcall GetDataHere(const tagFORMATETC &FormatEtcIn, /* out */ tagSTGMEDIUM &Medium);
	HRESULT __stdcall QueryGetData(const tagFORMATETC &FormatEtc);
	HRESULT __stdcall GetCanonicalFormatEtc(const tagFORMATETC &FormatEtc, /* out */ tagFORMATETC &FormatEtcOut);
	HRESULT __stdcall SetData(const tagFORMATETC &FormatEtc, tagSTGMEDIUM &Medium, System::LongBool fRelease);
	HRESULT __stdcall EnumFormatEtc(int dwDirection, /* out */ _di_IEnumFORMATETC &EnumFormatEtc);
	HRESULT __stdcall dAdvise(const tagFORMATETC &FormatEtc, int advf, const _di_IAdviseSink advsink, /* out */ int &dwConnection);
	HRESULT __stdcall dUnadvise(int dwConnection);
	HRESULT __stdcall EnumdAdvise(/* out */ _di_IEnumSTATDATA &EnumAdvise);
	__fastcall IElDropSource(TElDragDrop* aOwner);
	
public:
	__fastcall virtual ~IElDropSource();
private:
	void *__IDataObject;	// IDataObject 
	void *__IDropSource;	// IDropSource 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0000010E-0000-0000-C000-000000000046}
	operator _di_IDataObject()
	{
		_di_IDataObject intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IDataObject*(void) { return (IDataObject*)&__IDataObject; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000121-0000-0000-C000-000000000046}
	operator _di_IDropSource()
	{
		_di_IDropSource intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IDropSource*(void) { return (IDropSource*)&__IDropSource; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION IElDropTarget : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TElDragDrop* FOwner;
	TOleDragObject* FdragObj;
	
public:
	HRESULT __stdcall DragEnter(const _di_IDataObject dataObj, int grfKeyState, System::Types::TPoint pt, int &dwEffect);
	HRESULT __stdcall DragOver(int grfKeyState, System::Types::TPoint pt, int &dwEffect);
	HRESULT __stdcall DragLeave();
	HRESULT __stdcall Drop(const _di_IDataObject dataObj, int grfKeyState, System::Types::TPoint pt, int &dwEffect);
	__fastcall IElDropTarget(TElDragDrop* aOwner);
	__fastcall virtual ~IElDropTarget();
private:
	void *__IDropTarget;	// IDropTarget 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000122-0000-0000-C000-000000000046}
	operator _di_IDropTarget()
	{
		_di_IDropTarget intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IDropTarget*(void) { return (IDropTarget*)&__IDropTarget; }
	#endif
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElDropTarget : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	int FRefCount;
	Vcl::Controls::TWinControl* FTarget;
	TOleDragObject* FDragObj;
	TTargetDropEvent FOnTargetDrop;
	TTargetDragEvent FOnTargetDrag;
	System::Classes::TStrings* FDataFormats;
	Elhook::TElHook* FHook;
	void __fastcall SetTarget(Vcl::Controls::TWinControl* newValue);
	void __fastcall AfterMessage(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall BeforeMessage(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	
protected:
	HIDESBASE HRESULT __stdcall QueryInterface(const GUID &IID, /* out */ void *Obj);
	HIDESBASE int __stdcall _AddRef();
	HIDESBASE int __stdcall _Release();
	HRESULT __stdcall DragEnter(const _di_IDataObject dataObj, int grfKeyState, System::Types::TPoint pt, int &dwEffect);
	HRESULT __stdcall DragOver(int grfKeyState, System::Types::TPoint pt, int &dwEffect);
	HRESULT __stdcall DragLeave();
	HRESULT __stdcall Drop(const _di_IDataObject dataObj, int grfKeyState, System::Types::TPoint pt, int &dwEffect);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TElDropTarget(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDropTarget();
	__property System::Classes::TStrings* DataFormats = {read=FDataFormats};
	bool __fastcall HasDataFormat(int Format);
	
__published:
	__property TTargetDragEvent OnTargetDrag = {read=FOnTargetDrag, write=FOnTargetDrag};
	__property TTargetDropEvent OnTargetDrop = {read=FOnTargetDrop, write=FOnTargetDrop};
	__property Vcl::Controls::TWinControl* Target = {read=FTarget, write=SetTarget};
private:
	void *__IDropTarget;	// IDropTarget 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000122-0000-0000-C000-000000000046}
	operator _di_IDropTarget()
	{
		_di_IDropTarget intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IDropTarget*(void) { return (IDropTarget*)&__IDropTarget; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000000-0000-0000-C000-000000000046}
	operator System::_di_IInterface()
	{
		System::_di_IInterface intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator System::IInterface*(void) { return (System::IInterface*)&__IDropTarget; }
	#endif
	
};


class PASCALIMPLEMENTATION TElDragDrop : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
private:
	System::Uitypes::TCursor FOldCursor;
	bool FPressed;
	TTargetDropEvent FOnTargetDrop;
	TTargetDragEvent FOnTargetDrag;
	System::Classes::TNotifyEvent FOnPaint;
	bool FIsDropSource;
	bool FIsDropTarget;
	Vcl::Graphics::TPicture* FPicture;
	bool FAutoSize;
	int FContentLength;
	void *FContent;
	int FContentType;
	TOleStartDragEvent FOnOleStartDrag;
	TSourceDragEvent FOnSourceDrag;
	TSourceDropEvent FOnSourceDrop;
	_di_IDropTarget FElDropTarget;
	_di_IDropSource FElDropSource;
	TDragTypes FDragTypes;
	System::Classes::TStrings* FDataFormats;
	void __fastcall SetContentType(int newValue);
	void __fastcall SetContentLength(int newValue);
	void __fastcall SetPicture(Vcl::Graphics::TPicture* newValue);
	void __fastcall SetIsDropTarget(bool newValue);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	
protected:
	HIDESBASE void __fastcall SetAutoSize(bool newValue);
	virtual void __fastcall TriggerSourceDropEvent(TDragResult DragResult);
	virtual void __fastcall TriggerOleStartDragEvent(void * &DragData, int &DragDataType, int &DragDataSize);
	virtual void __fastcall TriggerSourceDragEvent(TDragType DragType, System::Classes::TShiftState Shift, bool &ContinueDrop);
	virtual void __fastcall Paint();
	virtual void __fastcall OnPictureChange(System::TObject* Sender);
	virtual void __fastcall TriggerPaintEvent();
	virtual void __fastcall TriggerTargetDragEvent(System::Uitypes::TDragState State, TOleDragObject* Source, System::Classes::TShiftState Shift, int X, int Y, TDragType &DragType);
	virtual void __fastcall TriggerTargetDropEvent(System::Classes::TShiftState Shift, TOleDragObject* Source, int X, int Y, TDragType &DragType);
	MESSAGE void __fastcall WMCreate(Winapi::Messages::TMessage &Message);
	virtual System::WideString __fastcall GetThemedClassName();
	
public:
	__fastcall virtual TElDragDrop(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDragDrop();
	__property void * Content = {read=FContent};
	__property int ContentType = {read=FContentType, write=SetContentType, nodefault};
	__property int ContentLength = {read=FContentLength, write=SetContentLength, nodefault};
	__property System::Classes::TStrings* DataFormats = {read=FDataFormats};
	__property Canvas;
	
__published:
	__property DragCursor = {default=-12};
	__property Visible = {default=1};
	__property Enabled = {default=1};
	__property TDragTypes DragTypes = {read=FDragTypes, write=FDragTypes, nodefault};
	__property TOleStartDragEvent OnOleStartDrag = {read=FOnOleStartDrag, write=FOnOleStartDrag};
	__property TSourceDragEvent OnOleSourceDrag = {read=FOnSourceDrag, write=FOnSourceDrag};
	__property TSourceDropEvent OnSourceDrop = {read=FOnSourceDrop, write=FOnSourceDrop};
	__property Vcl::Graphics::TPicture* Picture = {read=FPicture, write=SetPicture};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=0};
	__property bool IsDropSource = {read=FIsDropSource, write=FIsDropSource, nodefault};
	__property bool IsDropTarget = {read=FIsDropTarget, write=SetIsDropTarget, nodefault};
	__property UseXPThemes = {default=1};
	__property System::Classes::TNotifyEvent OnPaint = {read=FOnPaint, write=FOnPaint};
	__property TTargetDragEvent OnTargetDrag = {read=FOnTargetDrag, write=FOnTargetDrag};
	__property TTargetDropEvent OnTargetDrop = {read=FOnTargetDrop, write=FOnTargetDrop};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDragDrop(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eldragdrop */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDRAGDROP)
using namespace Eldragdrop;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldragdropHPP
