// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTree.pas' rev: 34.00 (Windows)

#ifndef EltreeHPP
#define EltreeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Menus.hpp>
#include <ElTools.hpp>
#include <ElHook.hpp>
#include <ElXPThemedControl.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElHeader.hpp>
#include <ElList.hpp>
#include <ElScrollBar.hpp>
#include <ElStrUtils.hpp>
#include <ElHintWnd.hpp>
#include <System.Variants.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElIni.hpp>
#include <ElPopBtn.hpp>
#include <ElImgFrm.hpp>
#include <ElArray.hpp>
#include <HTMLRender.hpp>
#include <ElDragDrop.hpp>
#include <ElExtBkgnd.hpp>
#include <ElVCLUtils.hpp>
#include <Winapi.ActiveX.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltree
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EElTreeError;
class DELPHICLASS TElTreeInplaceEditor;
class DELPHICLASS TElTreeInplaceManager;
class DELPHICLASS TElCellControl;
class DELPHICLASS TElCellCheckBox;
class DELPHICLASS TElCellButtonGlyph;
class DELPHICLASS TElCellButton;
class DELPHICLASS TElCellProgressBar;
class DELPHICLASS TElCellStyle;
struct TElTreeItemStaticData;
class DELPHICLASS TElTreeItem;
__interface DELPHIINTERFACE TIterateProcAnonymusMethod;
typedef System::DelphiInterface<TIterateProcAnonymusMethod> _di_TIterateProcAnonymusMethod;
class DELPHICLASS TElTreeItems;
class DELPHICLASS TElTreeView;
class DELPHICLASS TCustomElTree;
class DELPHICLASS TElTree;
class DELPHICLASS TElTreeDragObject;
//-- type declarations -------------------------------------------------------
;

;

using Elheader::TElFieldType;

using Elheader::TElFieldTypes;

using Elscrollbar::TElScrollBarPart;

using Eldragdrop::TDragType;

;

;

using Eldragdrop::TDragTypes;

;

typedef System::Set<System::Int8, 1, 8> TSTIStates;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EElTreeError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElTreeError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElTreeError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElTreeError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElTreeError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElTreeError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElTreeError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElTreeError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElTreeError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElTreeError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElTreeError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElTreeError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElTreeError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElTreeError() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TItemChangeMode : unsigned char { icmText, icmState, icmCheckState, icmColumnText };

enum DECLSPEC_DENUM TSTItemPart : unsigned char { ipButton, ipMainText, ipColumn, ipInside, ipPicture, ipPicture2, ipCheckBox, ipOutside };

enum DECLSPEC_DENUM TSTSelModes : unsigned char { smSimple, smUsual };

enum DECLSPEC_DENUM TSortDirs : unsigned char { sdAscend, sdDescend };

enum DECLSPEC_DENUM TSortModes : unsigned char { smNone, smAdd, smClick, smAddClick };

enum DECLSPEC_DENUM TSortTypes : unsigned char { stCustom, stText, stNumber, stFloating, stDateTime, stDate, stTime, stBoolean, stCurrency };

enum DECLSPEC_DENUM THintModes : unsigned char { shmNone, shmLong, shmAll };

enum DECLSPEC_DENUM TLineHintType : unsigned char { lhtMainTextOnly, lhtCellTextOnly, lhtSmart };

enum DECLSPEC_DENUM TElHintType : unsigned char { shtMainText, shtHintOnly, shtHintOrText };

enum DECLSPEC_DENUM TDragImgMode : unsigned char { dimNever, dimOne, dimAll };

enum DECLSPEC_DENUM TNodeAttachMode : unsigned char { naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert };

enum DECLSPEC_DENUM TElCheckBoxType : unsigned char { ectCheckBox, ect3SCheckBox, ectRadioButton };

enum DECLSPEC_DENUM TVirtualityLevel : unsigned char { vlNone, vlTextAndStyles };

enum DECLSPEC_DENUM TElItemBorderStyle : unsigned char { ibsNone, ibsRaised, ibsFlat, ibsSunken, ibsSpace };

enum DECLSPEC_DENUM TElDragType : unsigned char { dtOLE, dtDelphi, dtBoth };

enum DECLSPEC_DENUM TElDblClickMode : unsigned char { dcmNone, dcmExpand, dcmEdit };

enum DECLSPEC_DENUM TDragTargetDraw : unsigned char { ColorFrame, ColorRect, SelColorRect, dtdNone, dtdUpColorLine, dtdDownColorLine, dtdUpSelColorLine, dtdDownSelColorLine };

typedef void __fastcall (__closure *TInplaceEditorNeededEvent)(System::TObject* Sender, TElTreeItem* Item, int SectionIndex, Elheader::TElFieldType SupposedFieldType, TElTreeInplaceEditor* &Editor);

typedef void __fastcall (__closure *TInplaceOperationEvent)(System::TObject* Sender, bool &DefaultConversion);

typedef void __fastcall (__closure *TInplaceAfterOperationEvent)(System::TObject* Sender, bool &Accepted, bool &DefaultConversion);

typedef void __fastcall (__closure *TInplaceValidationEvent)(System::TObject* Sender, bool &InputValid);

class PASCALIMPLEMENTATION TElTreeInplaceEditor : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	Elstrutils::TElFString FDefaultValueAsText;
	bool FEditing;
	TCustomElTree* FTree;
	Elheader::TElFieldTypes FTypes;
	TElTreeItem* FItem;
	Elstrutils::TElFString FValueAsText;
	int FSectionIndex;
	Elheader::TElFieldType FDataType;
	System::Types::TRect FCellRect;
	bool CanReFocus;
	TInplaceOperationEvent FOnBeforeOperation;
	TInplaceAfterOperationEvent FOnAfterOperation;
	TInplaceValidationEvent FOnValidateResult;
	void __fastcall SetTree(TCustomElTree* Value);
	virtual bool __fastcall GetVisible() = 0 ;
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SetEditorParent();
	virtual void __fastcall StartOperation();
	virtual void __fastcall CompleteOperation(bool Accepted);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerValidateResult(bool &InputValid);
	virtual void __fastcall DoStartOperation() = 0 ;
	virtual void __fastcall DoStopOperation(bool Accepted);
	
public:
	__property TElTreeItem* Item = {read=FItem};
	__property Elstrutils::TElFString ValueAsText = {read=FValueAsText, write=FValueAsText};
	__property int SectionIndex = {read=FSectionIndex, nodefault};
	__property Elheader::TElFieldType DataType = {read=FDataType, nodefault};
	__property System::Types::TRect CellRect = {read=FCellRect};
	__property bool Visible = {read=GetVisible, nodefault};
	
__published:
	__property TCustomElTree* Tree = {read=FTree, write=SetTree};
	__property Elheader::TElFieldTypes Types = {read=FTypes, write=FTypes, default=0};
	__property Elstrutils::TElFString DefaultValueAsText = {read=FDefaultValueAsText, write=FDefaultValueAsText};
	__property TInplaceOperationEvent OnBeforeOperation = {read=FOnBeforeOperation, write=FOnBeforeOperation};
	__property TInplaceAfterOperationEvent OnAfterOperation = {read=FOnAfterOperation, write=FOnAfterOperation};
	__property TInplaceValidationEvent OnValidateResult = {read=FOnValidateResult, write=FOnValidateResult};
public:
	/* TComponent.Create */ inline __fastcall virtual TElTreeInplaceEditor(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TElTreeInplaceEditor() { }
	
};


class PASCALIMPLEMENTATION TElTreeInplaceManager : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Ellist::TElList* FEditorsList;
	
protected:
	void __fastcall RegisterEditor(TElTreeInplaceEditor* Editor);
	void __fastcall UnregisterEditor(TElTreeInplaceEditor* Editor);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TElTreeInplaceManager(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceManager();
	TElTreeInplaceEditor* __fastcall GetSuitableEditor(Elheader::TElFieldType SupposedFieldType);
};


class PASCALIMPLEMENTATION TElCellControl : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Vcl::Menus::TPopupMenu* FPopupMenu;
	TElCellStyle* FOwner;
	Elstrutils::TElFString FCaption;
	bool FVisible;
	bool FEnabled;
	System::Classes::TNotifyEvent FOnClick;
	Vcl::Controls::TMouseEvent FOnMouseDown;
	Vcl::Controls::TMouseEvent FOnMouseUp;
	System::Classes::TNotifyEvent FOnDblClick;
	Vcl::Controls::TMouseMoveEvent FOnMouseMove;
	Vcl::Graphics::TFont* FFont;
	void __fastcall SetPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	void __fastcall FontChanged(System::TObject* Sender);
	void __fastcall SetFont(Vcl::Graphics::TFont* newValue);
	
protected:
	int FBorderWidth;
	virtual void __fastcall SetCaption(Elstrutils::TElFString newValue);
	virtual void __fastcall SetVisible(bool newValue);
	virtual void __fastcall SetEnabled(bool newValue);
	virtual void __fastcall TriggerClickEvent();
	virtual void __fastcall TriggerMouseDownEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TriggerMouseUpEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TriggerDblClickEvent();
	virtual void __fastcall TriggerMouseMoveEvent(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetBorderWidth(int Value);
	virtual bool __fastcall PassClicks();
	
public:
	virtual void __fastcall Update();
	HIDESBASE virtual void __fastcall Assign(TElCellControl* Source) = 0 ;
	virtual void __fastcall Paint(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &Rect) = 0 ;
	__fastcall virtual TElCellControl();
	__fastcall virtual ~TElCellControl();
	__property int BorderWidth = {read=FBorderWidth, write=SetBorderWidth, nodefault};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	
__published:
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property TElCellStyle* Owner = {read=FOwner};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property System::Classes::TNotifyEvent OnClick = {read=FOnClick, write=FOnClick};
	__property Vcl::Controls::TMouseEvent OnMouseDown = {read=FOnMouseDown, write=FOnMouseDown};
	__property Vcl::Controls::TMouseEvent OnMouseUp = {read=FOnMouseUp, write=FOnMouseUp};
	__property System::Classes::TNotifyEvent OnDblClick = {read=FOnDblClick, write=FOnDblClick};
	__property Vcl::Controls::TMouseMoveEvent OnMouseMove = {read=FOnMouseMove, write=FOnMouseMove};
	__property Vcl::Menus::TPopupMenu* PopupMenu = {read=FPopupMenu, write=SetPopupMenu};
};


class PASCALIMPLEMENTATION TElCellCheckBox : public TElCellControl
{
	typedef TElCellControl inherited;
	
private:
	System::Classes::TAlignment FAlignment;
	Vcl::Stdctrls::TCheckBoxState FState;
	bool FAllowGrayed;
	void __fastcall SetState(Vcl::Stdctrls::TCheckBoxState newValue);
	void __fastcall SetAllowGrayed(bool newValue);
	bool __fastcall GetChecked();
	void __fastcall SetChecked(bool newValue);
	void __fastcall SetAlignment(System::Classes::TAlignment newValue);
	
protected:
	virtual void __fastcall TriggerClickEvent();
	virtual void __fastcall TriggerMouseDownEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TriggerMouseUpEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	
public:
	virtual void __fastcall Assign(TElCellControl* Source);
	__fastcall virtual TElCellCheckBox();
	__fastcall virtual ~TElCellCheckBox();
	virtual void __fastcall Paint(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	__property Vcl::Stdctrls::TCheckBoxState State = {read=FState, write=SetState, nodefault};
	__property bool Checked = {read=GetChecked, write=SetChecked, nodefault};
	__property bool AllowGrayed = {read=FAllowGrayed, write=SetAllowGrayed, nodefault};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=1};
};


class PASCALIMPLEMENTATION TElCellButtonGlyph : public Elpopbtn::TElButtonGlyph
{
	typedef Elpopbtn::TElButtonGlyph inherited;
	
public:
	__property ImageList;
	__property ImageIndex = {default=-1};
	__property UseImageList;
public:
	/* TElButtonGlyph.Create */ inline __fastcall TElCellButtonGlyph() : Elpopbtn::TElButtonGlyph() { }
	/* TElButtonGlyph.Destroy */ inline __fastcall virtual ~TElCellButtonGlyph() { }
	
};


class PASCALIMPLEMENTATION TElCellButton : public TElCellControl
{
	typedef TElCellControl inherited;
	
private:
	TElCellButtonGlyph* FGlyph;
	Vcl::Buttons::TButtonLayout FLayout;
	bool FFixClick;
	bool FDown;
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* newValue);
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall SetDown(bool newValue);
	void __fastcall SetLayout(Vcl::Buttons::TButtonLayout newValue);
	bool __fastcall GetUseImageList();
	void __fastcall SetUseImageList(bool newValue);
	Vcl::Controls::TImageList* __fastcall GetImageList();
	void __fastcall SetImageList(Vcl::Controls::TImageList* newValue);
	int __fastcall GetImageIndex();
	void __fastcall SetImageIndex(int newValue);
	
protected:
	virtual void __fastcall TriggerMouseDownEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TriggerMouseUpEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	
public:
	virtual void __fastcall Assign(TElCellControl* Source);
	__fastcall virtual TElCellButton();
	__fastcall virtual ~TElCellButton();
	virtual void __fastcall Paint(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	__property bool UseImageList = {read=GetUseImageList, write=SetUseImageList, nodefault};
	__property Vcl::Controls::TImageList* ImageList = {read=GetImageList, write=SetImageList};
	__property int ImageIndex = {read=GetImageIndex, write=SetImageIndex, nodefault};
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property bool FixClick = {read=FFixClick, write=FFixClick, nodefault};
	__property bool Down = {read=FDown, write=SetDown, default=0};
	__property Vcl::Buttons::TButtonLayout Layout = {read=FLayout, write=SetLayout, default=0};
};


class PASCALIMPLEMENTATION TElCellProgressBar : public TElCellControl
{
	typedef TElCellControl inherited;
	
protected:
	int FMinValue;
	int FMaxValue;
	int FValue;
	System::Uitypes::TColor FBarColor;
	bool FShowProgressText;
	System::Classes::TAlignment FTextAlignment;
	System::Uitypes::TColor FFrameColor;
	System::Uitypes::TColor FColor;
	void __fastcall SetMinValue(int Value);
	void __fastcall SetMaxValue(int Value);
	void __fastcall SetValue(int Value);
	void __fastcall SetBarColor(System::Uitypes::TColor Value);
	void __fastcall SetShowProgressText(bool Value);
	void __fastcall SetTextAlignment(System::Classes::TAlignment Value);
	void __fastcall SetFrameColor(System::Uitypes::TColor Value);
	void __fastcall SetColor(System::Uitypes::TColor Value);
	virtual bool __fastcall PassClicks();
	
public:
	__fastcall virtual TElCellProgressBar();
	__fastcall virtual ~TElCellProgressBar();
	virtual void __fastcall Assign(TElCellControl* Source);
	virtual void __fastcall Paint(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	__property int MinValue = {read=FMinValue, write=SetMinValue, nodefault};
	__property int MaxValue = {read=FMaxValue, write=SetMaxValue, default=100};
	__property int Value = {read=FValue, write=SetValue, nodefault};
	__property System::Uitypes::TColor BarColor = {read=FBarColor, write=SetBarColor, default=-16777203};
	__property System::Classes::TAlignment TextAlignment = {read=FTextAlignment, write=SetTextAlignment, default=2};
	__property bool ShowProgressText = {read=FShowProgressText, write=SetShowProgressText, default=1};
	__property System::Uitypes::TColor FrameColor = {read=FFrameColor, write=SetFrameColor, default=-16777208};
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, default=-16777211};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElCellStyle : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FTag;
	TElTreeItem* FOwner;
	System::Uitypes::TColor FCellBkColor;
	System::Uitypes::TColor FTextBkColor;
	System::Uitypes::TColor FTextColor;
	unsigned FTextFlags;
	Vcl::Graphics::TBitmap* FPicture;
	Elheader::TElFieldType FCellType;
	Elheader::TElSectionStyle FStyle;
	bool FOwnerProps;
	int FFontSize;
	System::Uitypes::TFontStyles FFontStyles;
	System::Uitypes::TFontName FFontName;
	TElCellControl* FControl;
	bool FUseBkColor;
	void __fastcall SetControl(TElCellControl* newValue);
	void __fastcall SetFontSize(int newValue);
	void __fastcall SetFontStyles(System::Uitypes::TFontStyles newValue);
	void __fastcall SetFontName(System::Uitypes::TFontName newValue);
	void __fastcall SetOwnerColors(bool newValue);
	void __fastcall SetStyle(Elheader::TElSectionStyle newValue);
	void __fastcall SetCellBkColor(System::Uitypes::TColor newValue);
	void __fastcall SetTextBkColor(System::Uitypes::TColor newValue);
	void __fastcall SetTextColor(System::Uitypes::TColor newValue);
	void __fastcall SetTextFlags(unsigned newValue);
	void __fastcall SetPicture(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetCellType(Elheader::TElFieldType newValue);
	void __fastcall SetUseBkColor(bool Value);
	
public:
	__fastcall TElCellStyle(TElTreeItem* Owner);
	__fastcall virtual ~TElCellStyle();
	void __fastcall Assign(TElCellStyle* Source);
	void __fastcall Update();
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property TElCellControl* Control = {read=FControl, write=SetControl};
	__property System::Uitypes::TColor CellBkColor = {read=FCellBkColor, write=SetCellBkColor, nodefault};
	__property System::Uitypes::TColor TextBkColor = {read=FTextBkColor, write=SetTextBkColor, nodefault};
	__property System::Uitypes::TColor TextColor = {read=FTextColor, write=SetTextColor, nodefault};
	__property unsigned TextFlags = {read=FTextFlags, write=SetTextFlags, nodefault};
	__property Vcl::Graphics::TBitmap* Picture = {read=FPicture, write=SetPicture};
	__property Elheader::TElFieldType CellType = {read=FCellType, write=SetCellType, default=1};
	__property Elheader::TElSectionStyle Style = {read=FStyle, write=SetStyle, nodefault};
	__property bool OwnerProps = {read=FOwnerProps, write=SetOwnerColors, nodefault};
	__property int FontSize = {read=FFontSize, write=SetFontSize, nodefault};
	__property System::Uitypes::TFontStyles FontStyles = {read=FFontStyles, write=SetFontStyles, nodefault};
	__property System::Uitypes::TFontName FontName = {read=FFontName, write=SetFontName};
	__property TElTreeItem* Owner = {read=FOwner};
	__property bool UseBkColor = {read=FUseBkColor, write=SetUseBkColor, nodefault};
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TElTreeItemStaticData
{
public:
	Elstrutils::TElFString FText;
	Elunicodestrings::TElWideStringList* FColText;
	Elstrutils::TElFString FHint;
	TElCellStyle* FMainStyle;
	Ellist::TElList* FStyles;
};


typedef TElTreeItemStaticData *PElTreeItemStaticData;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElTreeItem : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	Htmlrender::TElHTMLData* FHTMLData;
	Elarray::TElArray* FHTMLDataArray;
	int FTag;
	System::TObject* FObject;
	System::_di_IInterface FDataInterface;
	void *FSortData;
	int FSortType;
	int FSortRef;
	TElTreeItemStaticData *FStaticData;
	TElItemBorderStyle FBorderStyle;
	unsigned FComplexHeight;
	Vcl::Stdctrls::TCheckBoxState FCheckBoxState;
	TElCheckBoxType FCheckBoxType;
	TSTIStates FState;
	int FIState;
	Ellist::TElList* FChildren;
	TCustomElTree* FOwner;
	TElTreeItems* FList;
	void *FData;
	System::Uitypes::TColor FRowBkColor;
	System::Uitypes::TColor FColor;
	System::Uitypes::TColor FBkColor;
	System::Uitypes::TColor FStrikedLineColor;
	int FBoolData1;
	int FTextLeft;
	int FTextRight;
	int FImageIndex;
	int FStImageIndex;
	int FImageIndex2;
	int FStImageIndex2;
	TElTreeItem* FParent;
	TElTreeItem* FRoot;
	int FIndentAdjust;
	System::Uitypes::TColor FBorderSpaceColor;
	System::Int8 FOverlayIndex;
	System::Int8 FOverlayIndex2;
	Elstrutils::TElFString __fastcall GetText();
	virtual Elstrutils::TElFString __fastcall GetHint();
	void __fastcall SetHint(Elstrutils::TElFString Value);
	void __fastcall SetBorderStyle(TElItemBorderStyle Value);
	TElTreeItem* __fastcall GetParent();
	int __fastcall GetLevel();
	void __fastcall SetColor(int index, System::Uitypes::TColor value);
	void __fastcall SetUseBkColor(bool newValue);
	bool __fastcall GetHasChildren();
	bool __fastcall GetHasVisibleChildren();
	void __fastcall SetExpanded(bool value);
	void __fastcall SetParentColors(bool value);
	void __fastcall SetParentStyle(bool value);
	int __fastcall GetIndex();
	int __fastcall GetAbsIndex();
	int __fastcall GetVisIndex();
	int __fastcall GetChildIndex(TElTreeItem* Child);
	bool __fastcall IsExpanded();
	bool __fastcall GetFullExpand();
	void __fastcall MakeFullyExpanded(bool value);
	void __fastcall OnColTextChange(System::TObject* Sender);
	void __fastcall SetImageIndex(int value);
	void __fastcall SetStImageIndex(int value);
	void __fastcall SetImageIndex2(int value);
	void __fastcall SetStImageIndex2(int value);
	void __fastcall SetForceButtons(bool newValue);
	int __fastcall GetChildrenCount();
	int __fastcall GetCount();
	TElTreeItem* __fastcall GetItems(int Index);
	void __fastcall SetUseStyles(bool newValue);
	void __fastcall OnStyleDelete(System::TObject* Sender, void * Item);
	TElCellStyle* __fastcall GetStyles(int index);
	void __fastcall SetStyles(int index, TElCellStyle* newValue);
	int __fastcall GetStylesCount();
	void __fastcall SetCheckBoxState(Vcl::Stdctrls::TCheckBoxState newValue);
	void __fastcall SetChecked(bool newValue);
	bool __fastcall GetChecked();
	void __fastcall SetShowCheckBox(bool newValue);
	void __fastcall SetCheckBoxType(TElCheckBoxType newValue);
	void __fastcall SetCheckBoxEnabled(bool newValue);
	void __fastcall SetSuppressButtons(bool newValue);
	void __fastcall SetEnabled(bool newValue);
	void __fastcall SetHidden(bool newValue);
	bool __fastcall GetFullyVisible();
	void __fastcall SetFullyVisible(bool newValue);
	bool __fastcall GetSelected();
	void __fastcall SetSelected(bool newValue);
	void __fastcall CreateStyles();
	void __fastcall SetOwnerHeight(bool newValue);
	void __fastcall SetHeight(int newValue);
	int __fastcall GetHeight();
	void __fastcall SetSuppressLines(bool newValue);
	void __fastcall UpdateItem();
	virtual void __fastcall SetText(Elstrutils::TElFString Value);
	bool __fastcall GetState(int index);
	void __fastcall SetState(int index, bool value);
	void __fastcall RemoveChild(TElTreeItem* Child);
	void __fastcall RemoveSubChild(TElTreeItem* Child);
	void __fastcall DeleteChild(TElTreeItem* Child);
	int __fastcall AddChild(TElTreeItem* Child);
	void __fastcall AddExistingChild(TElTreeItem* Child);
	int __fastcall AddLastChild(TElTreeItem* Child);
	int __fastcall InsertChild(int index, TElTreeItem* Child);
	void __fastcall ExchangeItems(int I, int J);
	void __fastcall QuickSort(bool recursive, int L, int R, Elheader::TElSSortMode SM, TSortTypes SortType, int FSortSection);
	void __fastcall AddSortData(TSortTypes SortType, int FSortSection);
	void __fastcall ReleaseSortData();
	void __fastcall NormalizeSorts(int StartIdx);
	virtual void __fastcall SetRowBkColor(System::Uitypes::TColor newValue);
	bool __fastcall GetOwnerHeight();
	virtual void __fastcall SetMultiline(bool newValue);
	void __fastcall SetIsHTML(bool newValue);
	void __fastcall OnHTMLDataDestroy(System::TObject* Sender, void * Item);
	void __fastcall ReRenderMainText();
	void __fastcall ReRenderAllTexts();
	TElTreeItem* __fastcall GetAncestor();
	void __fastcall SetStrikedOutLine(const bool Value);
	void __fastcall SetStrikedLineColor(const System::Uitypes::TColor Value);
	bool __fastcall GetStrikedOutLine();
	void __fastcall SetDrawHLine(const bool Value);
	void __fastcall SetAllowEdit(const bool Value);
	int __fastcall CalcSubItemsHeight();
	void __fastcall NewStaticData();
	void __fastcall DisposeStaticData();
	void __fastcall FillStaticData();
	Elunicodestrings::TElWideStrings* __fastcall GetColText();
	bool __fastcall GetParentStyle();
	TElCellStyle* __fastcall GetMainStyle();
	bool __fastcall GetUseStyles();
	bool __fastcall GetUseBkColor();
	bool __fastcall GetParentColors();
	bool __fastcall GetDrawHLine();
	bool __fastcall GetAllowEdit();
	bool __fastcall GetForceButtons();
	bool __fastcall GetSuppressButtons();
	bool __fastcall GetSuppressLines();
	bool __fastcall GetIsHTML();
	bool __fastcall GetMultiline();
	bool __fastcall GetShowCheckBox();
	bool __fastcall GetCheckBoxEnabled();
	bool __fastcall GetEnabled();
	bool __fastcall GetHidden();
	void __fastcall SetIndentAdjust(int Value);
	bool __fastcall GetDropTarget();
	bool __fastcall GetHintIsHTML();
	void __fastcall SetHintIsHTML(bool Value);
	void __fastcall SetBorderSpaceColor(System::Uitypes::TColor Value);
	void __fastcall SetOverlayIndex(System::Int8 value);
	void __fastcall SetOverlayIndex2(System::Int8 value);
	void __fastcall ClearSubChild();
	
public:
	__fastcall virtual TElTreeItem(TCustomElTree* AOwner);
	__fastcall virtual ~TElTreeItem();
	virtual int __fastcall GetWidth();
	virtual void __fastcall ReadData(System::Classes::TStream* Stream);
	virtual void __fastcall WriteData(System::Classes::TStream* Stream);
	bool __fastcall IsUnder(TElTreeItem* Item);
	Elstrutils::TElFString __fastcall GetFullName(Elstrutils::TElFString separator);
	Elstrutils::TElFString __fastcall GetFullNameEx(Elstrutils::TElFString separator, bool AddRoot);
	void __fastcall Expand(bool recursive);
	void __fastcall Collapse(bool recursive);
	void __fastcall Sort(bool recursive);
	void __fastcall MoveTo(TElTreeItem* NewParent);
	void __fastcall MoveToIns(TElTreeItem* NewParent, int AnIndex);
	void __fastcall Clear();
	TElTreeItem* __fastcall GetFirstVisibleChild();
	TElTreeItem* __fastcall GetFirstChild();
	TElTreeItem* __fastcall GetLastChild();
	TElTreeItem* __fastcall GetNextChild(TElTreeItem* Child);
	TElTreeItem* __fastcall GetPrevChild(TElTreeItem* Child);
	TElTreeItem* __fastcall GetFirstSibling();
	TElTreeItem* __fastcall GetLastSibling();
	TElTreeItem* __fastcall GetNextSibling();
	TElTreeItem* __fastcall GetPrevSibling();
	TElTreeItem* __fastcall GetLastSubItem();
	TElTreeItem* __fastcall GetChildByIndex(int index);
	void __fastcall EditText();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Delete();
	__property TCustomElTree* TreeView = {read=FOwner};
	bool __fastcall IsVisible();
	TElTreeItem* __fastcall GetNextVisible();
	TElTreeItem* __fastcall GetPrevVisible();
	virtual TElTreeItem* __fastcall GetPrev();
	virtual TElTreeItem* __fastcall GetNext();
	virtual void __fastcall MoveToItem(TElTreeItem* Item, TNodeAttachMode Mode);
	virtual TElCellStyle* __fastcall AddStyle();
	virtual void __fastcall RemoveStyle(TElCellStyle* Style);
	void __fastcall RedrawItem(bool DoCheck);
	void __fastcall RedrawItemPart(bool DoCheck, int Left, int Right);
	System::Types::TRect __fastcall DisplayRect(bool TextOnly);
	void __fastcall EndEdit(bool ByCancel);
	bool __fastcall HasAsParent(TElTreeItem* Item);
	int __fastcall IndexOf(TElTreeItem* Item);
	void __fastcall MakeVisible();
	__property int TextLeft = {read=FTextLeft, nodefault};
	__property int TextRight = {read=FTextRight, nodefault};
	__property void * Data = {read=FData, write=FData};
	__property System::TObject* AnObject = {read=FObject, write=FObject};
	__property System::_di_IInterface DataInterface = {read=FDataInterface, write=FDataInterface};
	__property TCustomElTree* Owner = {read=FOwner};
	__property TElTreeItem* Parent = {read=GetParent, write=MoveTo};
	__property bool HasVisibleChildren = {read=GetHasVisibleChildren, nodefault};
	__property bool HasChildren = {read=GetHasChildren, nodefault};
	__property int Index = {read=GetIndex, nodefault};
	__property int AbsoluteIndex = {read=GetAbsIndex, nodefault};
	__property int VisIndex = {read=GetVisIndex, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property int ChildrenCount = {read=GetChildrenCount, nodefault};
	__property TElTreeItem* Children[int Index] = {read=GetItems};
	__property TElTreeItem* Item[int Index] = {read=GetItems};
	__property int Level = {read=GetLevel, nodefault};
	__property int Tag = {read=FTag, write=FTag, nodefault};
	__property TElCellStyle* Styles[int index] = {read=GetStyles, write=SetStyles};
	__property TElTreeItem* Ancestor = {read=GetAncestor};
	__property bool StrikedOutLine = {read=GetStrikedOutLine, write=SetStrikedOutLine, nodefault};
	__property System::Uitypes::TColor StrikedLineColor = {read=FStrikedLineColor, write=SetStrikedLineColor, nodefault};
	__property bool DrawHLine = {read=GetDrawHLine, write=SetDrawHLine, nodefault};
	__property bool AllowEdit = {read=GetAllowEdit, write=SetAllowEdit, nodefault};
	__property bool Focused = {read=GetState, write=SetState, index=1, nodefault};
	__property bool Selected = {read=GetSelected, write=SetSelected, nodefault};
	__property bool Cut = {read=GetState, write=SetState, index=3, nodefault};
	__property bool Underlined = {read=GetState, write=SetState, index=4, nodefault};
	__property bool Bold = {read=GetState, write=SetState, index=5, nodefault};
	__property bool Italic = {read=GetState, write=SetState, index=6, nodefault};
	__property bool StrikeOut = {read=GetState, write=SetState, index=8, nodefault};
	__property bool ParentStyle = {read=GetParentStyle, write=SetParentStyle, nodefault};
	__property Elstrutils::TElFString Text = {read=GetText, write=SetText};
	__property Elunicodestrings::TElWideStrings* ColumnText = {read=GetColText};
	__property Elunicodestrings::TElWideStrings* SubItems = {read=GetColText};
	__property bool Expanded = {read=IsExpanded, write=SetExpanded, nodefault};
	__property bool FullyExpanded = {read=GetFullExpand, write=MakeFullyExpanded, nodefault};
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, index=1, nodefault};
	__property System::Uitypes::TColor BkColor = {read=FBkColor, write=SetColor, index=2, nodefault};
	__property bool UseBkColor = {read=GetUseBkColor, write=SetUseBkColor, nodefault};
	__property bool ParentColors = {read=GetParentColors, write=SetParentColors, nodefault};
	__property int ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property int StateImageIndex = {read=FStImageIndex, write=SetStImageIndex, default=-1};
	__property int ImageIndex2 = {read=FImageIndex2, write=SetImageIndex2, default=-1};
	__property int StateImageIndex2 = {read=FStImageIndex2, write=SetStImageIndex2, default=-1};
	__property bool ForceButtons = {read=GetForceButtons, write=SetForceButtons, default=0};
	__property bool SuppressButtons = {read=GetSuppressButtons, write=SetSuppressButtons, default=0};
	__property bool SuppressLines = {read=GetSuppressLines, write=SetSuppressLines, nodefault};
	__property Elstrutils::TElFString Hint = {read=GetHint, write=SetHint};
	__property bool UseStyles = {read=GetUseStyles, write=SetUseStyles, nodefault};
	__property TElCellStyle* MainStyle = {read=GetMainStyle};
	__property int StylesCount = {read=GetStylesCount, nodefault};
	__property Vcl::Stdctrls::TCheckBoxState CheckBoxState = {read=FCheckBoxState, write=SetCheckBoxState, default=0};
	__property bool Checked = {read=GetChecked, write=SetChecked, default=0};
	__property bool ShowCheckBox = {read=GetShowCheckBox, write=SetShowCheckBox, default=1};
	__property TElCheckBoxType CheckBoxType = {read=FCheckBoxType, write=SetCheckBoxType, default=0};
	__property bool CheckBoxEnabled = {read=GetCheckBoxEnabled, write=SetCheckBoxEnabled, nodefault};
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, default=1};
	__property bool Hidden = {read=GetHidden, write=SetHidden, nodefault};
	__property bool FullyVisible = {read=GetFullyVisible, write=SetFullyVisible, nodefault};
	__property int Height = {read=GetHeight, write=SetHeight, nodefault};
	__property bool OwnerHeight = {read=GetOwnerHeight, write=SetOwnerHeight, nodefault};
	__property bool Multiline = {read=GetMultiline, write=SetMultiline, nodefault};
	__property System::Uitypes::TColor RowBkColor = {read=FRowBkColor, write=SetRowBkColor, nodefault};
	__property bool IsHTML = {read=GetIsHTML, write=SetIsHTML, nodefault};
	__property TElItemBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, nodefault};
	__property int IndentAdjust = {read=FIndentAdjust, write=SetIndentAdjust, nodefault};
	__property bool DropTarget = {read=GetDropTarget, nodefault};
	__property bool HintIsHTML = {read=GetHintIsHTML, write=SetHintIsHTML, nodefault};
	__property System::Uitypes::TColor BorderSpaceColor = {read=FBorderSpaceColor, write=SetBorderSpaceColor, default=-16777211};
	__property System::Int8 OverlayIndex = {read=FOverlayIndex, write=SetOverlayIndex, default=-1};
	__property System::Int8 OverlayIndex2 = {read=FOverlayIndex2, write=SetOverlayIndex2, default=-1};
};

#pragma pack(pop)

typedef System::TMetaClass* TElTreeItemClass;

typedef bool __fastcall (*TElLookupCompareProc)(TElTreeItem* Item, void * SearchDetails);

typedef void __fastcall (*TIterateProc)(TElTreeItem* Item, int Index, bool &ContinueIterate, void * IterateData, TCustomElTree* Tree);

__interface TIterateProcAnonymusMethod  : public System::IInterface 
{
	virtual void __fastcall Invoke(TElTreeItem* Item, int Index, bool &ContinueIterate, void * IterateData, TCustomElTree* Tree) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElTreeItems : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	TElTreeItem* operator[](int Index) { return this->Item[Index]; }
	
protected:
	TCustomElTree* FOwner;
	TElTreeItem* FRoot;
	TElTreeItemClass FItemClass;
	int __fastcall GetVisCount();
	virtual void __fastcall ReadData(System::Classes::TStream* Stream);
	virtual void __fastcall WriteData(System::Classes::TStream* Stream);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual TElTreeItem* __fastcall CreateItem(TCustomElTree* FOwner);
	int __fastcall GetCount();
	int __fastcall GetRootCount();
	TElTreeItem* __fastcall GetRootItem(int Index);
	TElTreeItem* __fastcall GetItem(int index);
	TElTreeItem* __fastcall GetVisItem(int index);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TElTreeItems(TCustomElTree* AOwner);
	__fastcall TElTreeItems(TCustomElTree* AOwner, TElTreeItemClass ItemClass);
	__fastcall virtual ~TElTreeItems();
	void __fastcall AddExistingItem(TElTreeItem* Item, TElTreeItem* Parent);
	void __fastcall InsertExistingItem(TElTreeItem* Item, TElTreeItem* Parent, int Index);
	void __fastcall RemoveItem(TElTreeItem* Child);
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	void __fastcall SaveToFile(System::UnicodeString FileName);
	void __fastcall LoadFromFile(System::UnicodeString FileName);
	void __fastcall SaveToStringList(System::Classes::TStrings* AStrings);
	void __fastcall LoadFromStringList(System::Classes::TStrings* Strings);
	void __fastcall LoadFromWideStringList(Elunicodestrings::TElWideStrings* Strings);
	void __fastcall SaveToWideStringList(Elunicodestrings::TElWideStrings* AStrings);
	void __fastcall DeleteItem(TElTreeItem* Child);
	int __fastcall GetAbsIndex(TElTreeItem* Child);
	int __fastcall GetVisIndex(TElTreeItem* Child);
	TElTreeItem* __fastcall AddItem(TElTreeItem* Parent);
	TElTreeItem* __fastcall AddLastItem(TElTreeItem* Parent);
	void __fastcall SetItem(int Index, TElTreeItem* Value);
	TElTreeItem* __fastcall InsertItem(int Index, TElTreeItem* Parent);
	void __fastcall AllocateStorage(int MaxItems);
	virtual TElTreeItem* __fastcall Add(TElTreeItem* Item, Elstrutils::TElFString Text);
	virtual TElTreeItem* __fastcall AddChild(TElTreeItem* Item, Elstrutils::TElFString Text);
	virtual TElTreeItem* __fastcall AddChildFirst(TElTreeItem* Item, Elstrutils::TElFString Text);
	virtual TElTreeItem* __fastcall AddChildObject(TElTreeItem* Item, Elstrutils::TElFString Text, void * Ptr);
	virtual TElTreeItem* __fastcall AddChildObjectFirst(TElTreeItem* Item, Elstrutils::TElFString Text, void * Ptr);
	virtual TElTreeItem* __fastcall AddFirst(TElTreeItem* Item, Elstrutils::TElFString Text);
	virtual TElTreeItem* __fastcall AddObject(TElTreeItem* Item, Elstrutils::TElFString Text, void * Ptr);
	virtual TElTreeItem* __fastcall AddObjectFirst(TElTreeItem* Item, Elstrutils::TElFString Text, void * Ptr);
	virtual TElTreeItem* __fastcall Insert(TElTreeItem* Item, Elstrutils::TElFString Text);
	virtual TElTreeItem* __fastcall InsertObject(TElTreeItem* Item, Elstrutils::TElFString Text, void * Ptr);
	virtual TElTreeItem* __fastcall InsertAfter(TElTreeItem* Item, Elstrutils::TElFString Text);
	virtual TElTreeItem* __fastcall InsertAfterObject(TElTreeItem* Item, Elstrutils::TElFString Text, void * Ptr);
	void __fastcall InsertItemFromString(int Index, Elstrutils::TElFString AString);
	virtual void __fastcall Delete(TElTreeItem* Item);
	TElTreeItem* __fastcall GetFirstNode();
	void __fastcall Clear();
	void __fastcall IterateBranch(bool VisibleOnly, TIterateProc IterateProc, void * IterateData, TElTreeItem* BranchParent);
	void __fastcall IterateFrom(bool VisibleOnly, bool CheckCollapsed, _di_TIterateProcAnonymusMethod IterateProc, void * IterateData, TElTreeItem* StartFrom);
	void __fastcall IterateBackFrom(bool VisibleOnly, bool CheckCollapsed, _di_TIterateProcAnonymusMethod IterateProc, void * IterateData, TElTreeItem* StartFrom);
	void __fastcall Iterate(bool VisibleOnly, bool CheckCollapsed, _di_TIterateProcAnonymusMethod IterateProc, void * IterateData);
	void __fastcall IterateBack(bool VisibleOnly, bool CheckCollapsed, _di_TIterateProcAnonymusMethod IterateProc, void * IterateData);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	TElTreeItem* __fastcall LookForItem(TElTreeItem* StartItem, Elstrutils::TElFString TextToFind, void * DataToFind, int ColumnNum, bool LookForData, bool CheckStartItem, bool SubItemsOnly, bool VisibleOnly, bool NoCase);
	TElTreeItem* __fastcall LookForItem2(TElTreeItem* StartItem, Elstrutils::TElFString TextToFind, bool WholeTextOnly, void * DataToFind, int ColumnNum, bool LookForData, bool CheckStartItem, bool SubItemsOnly, bool VisibleOnly, bool CheckCollapsed, bool NoCase);
	TElTreeItem* __fastcall LookForItemEx(TElTreeItem* StartItem, int ColumnNum, bool CheckStartItem, bool SubItemsOnly, bool VisibleOnly, void * SearchDetails, TElLookupCompareProc CompareProc);
	TElTreeItem* __fastcall LookBackForItemEx2(TElTreeItem* StartItem, int ColumnNum, bool CheckStartItem, bool SubItemsOnly, bool VisibleOnly, bool CheckCollapsed, void * SearchDetails, TElLookupCompareProc CompareProc);
	TElTreeItem* __fastcall LookForItemEx2(TElTreeItem* StartItem, int ColumnNum, bool CheckStartItem, bool SubItemsOnly, bool VisibleOnly, bool CheckCollapsed, void * SearchDetails, TElLookupCompareProc CompareProc);
	__property TElTreeItemClass ItemClass = {read=FItemClass, write=FItemClass};
	__property TCustomElTree* Owner = {read=FOwner};
	__property TElTreeItem* Item[int Index] = {read=GetItem/*, default*/};
	__property TElTreeItem* ItemAsVis[int Index] = {read=GetVisItem};
	__property int Count = {read=GetCount, nodefault};
	__property int VisCount = {read=GetVisCount, nodefault};
	__property int RootCount = {read=GetRootCount, nodefault};
	__property TElTreeItem* RootItem[int Index] = {read=GetRootItem};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TOleDragStartEvent)(System::TObject* Sender, _di_IDataObject &dataObj, _di_IDropSource &dropSource, Eldragdrop::TDragTypes &dwOKEffects);

typedef void __fastcall (__closure *TOleDragFinishEvent)(System::TObject* Sender, Eldragdrop::TDragType dwEffect, HRESULT Result);

typedef void __fastcall (__closure *TMeasureItemPartEvent)(System::TObject* Sender, TElTreeItem* Item, int PartIndex, System::Types::TPoint &Size);

typedef void __fastcall (__closure *THeaderSectionEvent)(System::TObject* Sender, Elheader::TElHeaderSection* Section);

typedef void __fastcall (__closure *TColumnNotifyEvent)(System::TObject* Sender, int SectionIndex);

typedef void __fastcall (__closure *TOnItemDrawEvent)(System::TObject* Sender, TElTreeItem* Item, Vcl::Graphics::TCanvas* Surface, const System::Types::TRect &R, int SectionIndex);

typedef void __fastcall (__closure *TOnShowHintEvent)(System::TObject* Sender, TElTreeItem* Item, Elheader::TElHeaderSection* Section, Elstrutils::TElFString &Text, Vcl::Controls::THintWindow* HintWindow, const System::Types::TPoint &MousePos, bool &DoShowHint);

typedef void __fastcall (__closure *TApplyVisFilterEvent)(System::TObject* Sender, TElTreeItem* Item, bool &Hidden);

typedef void __fastcall (__closure *TTuneUpInplaceEditEvent)(System::TObject* Sender, TElTreeItem* Item, int SectionIndex, Vcl::Stdctrls::TCustomEdit* Editor);

typedef void __fastcall (__closure *TOnItemExpandEvent)(System::TObject* Sender, TElTreeItem* Item);

typedef void __fastcall (__closure *TOnItemCheckedEvent)(System::TObject* Sender, TElTreeItem* Item);

typedef void __fastcall (__closure *TItemSelChangeEvent)(System::TObject* Sender, TElTreeItem* Item);

typedef void __fastcall (__closure *TOnItemChangeEvent)(System::TObject* Sender, TElTreeItem* Item, TItemChangeMode ItemChangeMode);

typedef void __fastcall (__closure *TOnCompareItems)(System::TObject* Sender, TElTreeItem* Item1, TElTreeItem* Item2, int &res);

typedef void __fastcall (__closure *TOnItemExpanding)(System::TObject* Sender, TElTreeItem* Item, bool &CanProcess);

typedef void __fastcall (__closure *TOnPicDrawEvent)(System::TObject* Sender, TElTreeItem* Item, int &ImageIndex);

typedef void __fastcall (__closure *THotTrackEvent)(System::TObject* Sender, TElTreeItem* OldItem, TElTreeItem* NewItem);

typedef void __fastcall (__closure *TOnValidateEvent)(System::TObject* Sender, TElTreeItem* Item, Elheader::TElHeaderSection* Section, System::UnicodeString &Text, bool &Accept);

typedef void __fastcall (__closure *TTryEditEvent)(System::TObject* Sender, TElTreeItem* Item, int SectionIndex, Elheader::TElFieldType &CellType, bool &CanEdit);

typedef void __fastcall (__closure *TEditRequestEvent)(System::TObject* Sender, TElTreeItem* Item, Elheader::TElHeaderSection* Section);

typedef void __fastcall (__closure *TComboEditShowEvent)(System::TObject* Sender, TElTreeItem* Item, Elheader::TElHeaderSection* Section, Vcl::Stdctrls::TComboBox* Combobox);

typedef void __fastcall (__closure *TValidateComboEvent)(System::TObject* Sender, TElTreeItem* Item, Elheader::TElHeaderSection* Section, Vcl::Stdctrls::TComboBox* Combo, bool &Accept);

typedef void __fastcall (__closure *TElScrollEvent)(System::TObject* Sender, Vcl::Forms::TScrollBarKind ScrollBarKind, int ScrollCode);

typedef void __fastcall (__closure *TElColumnMoveEvent)(TCustomElTree* Sender, Elheader::TElHeaderSection* Section, int OldPos, int NewPos);

typedef void __fastcall (__closure *TItemSaveEvent)(System::TObject* Sender, System::Classes::TStream* Stream, TElTreeItem* Item);

typedef void __fastcall (__closure *TCellStyleSaveEvent)(System::TObject* Sender, System::Classes::TStream* Stream, TElCellStyle* Style);

typedef void __fastcall (__closure *TElTreeChangingEvent)(System::TObject* Sender, TElTreeItem* Item, bool &AllowChange);

typedef void __fastcall (__closure *TElTreeItemPostDrawEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* Canvas, TElTreeItem* Item, const System::Types::TRect &ItemRect, bool &DrawFocusRect);

typedef void __fastcall (__closure *TElTreeItemDragTargetEvent)(System::TObject* Sender, TElTreeItem* Item, const System::Types::TRect &ItemRect, int X, int Y);

typedef void __fastcall (__closure *TVirtualTextNeededEvent)(System::TObject* Sender, TElTreeItem* Item, int SectionIndex, Elstrutils::TElFString &Text);

typedef void __fastcall (__closure *TVirtualHintNeededEvent)(System::TObject* Sender, TElTreeItem* Item, Elstrutils::TElFString &Hint);

typedef void __fastcall (__closure *TVirtualValueNeededEvent)(System::TObject* Sender, TElTreeItem* Item, int SectionIndex, int VarType, System::Variant &Value);

typedef void __fastcall (__closure *TVirtualStyleNeededEvent)(System::TObject* Sender, TElTreeItem* Item, int SectionIndex, TElCellStyle* Style);

class PASCALIMPLEMENTATION TElTreeView : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
protected:
	Elheader::TElHeader* FHeader;
	TCustomElTree* FOwner;
	TElTreeItems* FItems;
	TElCellStyle* VirtStyle;
	Vcl::Extctrls::TTimer* FHintTimer;
	Elhintwnd::TElHintWindow* FHintWnd;
	System::Types::TPoint FHintCoord;
	TElTreeItem* FHintItem;
	TElTreeItem* FHintItemEx;
	bool FPainting;
	bool FClearVis;
	bool FClearAll;
	bool FVisUpdated;
	bool FRangeUpdate;
	int FHRange;
	System::Types::TPoint FPressCoord;
	bool FPressed;
	bool FMouseSel;
	System::Types::TPoint FClickCoord;
	bool FClicked;
	TElCellControl* FClickControl;
	bool FIgnoreClick;
	bool FIgnoreClick2;
	bool FClickPassed;
	TElTreeItem* FPassedItem;
	System::Classes::TShiftState FPassedShift;
	int FClickSection;
	TElTreeItem* FClickItem;
	TElTreeItem* FTrackItem;
	TElTreeItem* FEditingItem;
	TElTreeItem* FFocused;
	TElTreeItem* FSelected;
	TElTreeItem* FDropTrg;
	TElTreeItem* FMFSStartItem;
	System::Types::TPoint FMFSStartCoord;
	TElTreeItem* FMFSEndItem;
	System::Types::TPoint FMFSendCoord;
	Ellist::TElList* FMFSList;
	Ellist::TElList* FVisible;
	bool FOverColors;
	bool FRowOvColors;
	Vcl::Extctrls::TTimer* FDragScrollTimer;
	Vcl::Extctrls::TTimer* FDragExpandTimer;
	bool FDropAcc;
	bool FInDragging;
	int FDDY;
	Vcl::Controls::TImageList* FDragImages;
	TElTreeInplaceEditor* FInpEdit;
	bool FEditing;
	Elheader::TElFieldType FEditType;
	int FEditSect;
	Vcl::Extctrls::TTimer* FEditTimer;
	TElTreeItem* FItemToEdit;
	bool FOldHide;
	Vcl::Menus::TPopupMenu* FFakePopup;
	Htmlrender::TElHTMLRender* FRender;
	Vcl::Graphics::TBitmap* FTmpBmp;
	System::UnicodeString SearchText;
	System::Classes::TThread* SearchTextTimeoutThread;
	bool FScrollFirstClick;
	bool FHasFocus;
	void __fastcall StartClearSearchTimeoutThread();
	void __fastcall StopClearSearchTimeoutThread();
	void __fastcall SearchTextTimeout(System::TObject* Sender);
	bool __fastcall ProcessSearch(System::WideChar Key);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	void __fastcall RedoTmpBmp();
	void __fastcall RedrawTree(Vcl::Graphics::TCanvas* ACanvas, int RealLeftPos, Ellist::TElList* ItemsList);
	void __fastcall DrawImages(Vcl::Graphics::TCanvas* ACanvas, TElTreeItem* Item, Vcl::Graphics::TBitmap* HelperBitmap, System::Types::TRect &R, System::Types::TRect &ItemRect);
	void __fastcall DrawButtons(Vcl::Graphics::TCanvas* ACanvas, TElTreeItem* Item, bool IsNode, Vcl::Graphics::TBitmap* HelperBitmap, System::Types::TRect &R, System::Types::TRect &ItemRect);
	void __fastcall DrawCheckBoxes(Vcl::Graphics::TCanvas* ACanvas, TElTreeItem* Item, Vcl::Graphics::TBitmap* HelperBitmap, System::Types::TRect &R, System::Types::TRect &ItemRect);
	void __fastcall DrawItemLines(Vcl::Graphics::TCanvas* ACanvas, TElTreeItem* Item, System::Types::TRect &R, System::Types::TRect &ItemRect);
	void __fastcall DoRedrawItem(Vcl::Graphics::TCanvas* ACanvas, TElTreeItem* Item, const System::Types::TRect &ItemRect, const System::Types::TRect &SurfRect);
	void __fastcall DoRedrawItemTree(Vcl::Graphics::TCanvas* ACanvas, TElTreeItem* Item, const System::Types::TRect &ItemRect, const System::Types::TRect &SurfRect);
	virtual void __fastcall Paint();
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	void __fastcall DoPaintBkgnd(HDC DC, const System::Types::TRect &ClipRect);
	void __fastcall UpdateView();
	void __fastcall TryStartHint(int XPos, int YPos);
	void __fastcall OnHintTimer(System::TObject* Sender);
	void __fastcall DoHideLineHint();
	void __fastcall DoShowLineHint(TElTreeItem* Item, Elheader::TElHeaderSection* Section);
	Elstrutils::TElFString __fastcall GetHintText(TElTreeItem* Item, Elheader::TElHeaderSection* &Section);
	int __fastcall CalcPageUpPos(int CurIdx);
	int __fastcall CalcPageDownPos(int CurIdx);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDblClk(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	void __fastcall IntLButtonDown(int X, int Y, System::Classes::TShiftState Shift);
	bool __fastcall IntLButtonUp(int X, int Y, System::Classes::TShiftState Shift);
	void __fastcall IntRButtonDown(int X, int Y, System::Classes::TShiftState Shift);
	bool __fastcall IntRButtonUp(int X, int Y, System::Classes::TShiftState Shift);
	bool __fastcall IntLButtonDblClick(int X, int Y, System::Classes::TShiftState Shift);
	bool __fastcall IntRButtonDblClick(int X, int Y, System::Classes::TShiftState Shift);
	void __fastcall IntMouseMove(int X, int Y, System::Classes::TShiftState Shift);
	HIDESBASE MESSAGE void __fastcall CMMouseWheel(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Msg);
	void __fastcall SetHPosition(int value);
	void __fastcall SetVPosition(int value);
	void __fastcall DoSetTopIndex(int Value);
	void __fastcall OnHScroll(System::TObject* Sender, Elscrollbar::TElScrollCode ScrollCode, int &ScrollPos, bool &DoChange);
	void __fastcall OnVScroll(System::TObject* Sender, Elscrollbar::TElScrollCode ScrollCode, int &ScrollPos, bool &DoChange);
	void __fastcall FillVisFwd(int StartIndex);
	void __fastcall DefineHRange();
	virtual int __fastcall GetVisCount();
	int __fastcall GetVisiblesHeight();
	void __fastcall OnEditTimer(System::TObject* Sender);
	virtual void __fastcall DoEditItem(TElTreeItem* Item, int SectionNum);
	virtual void __fastcall DoEndEdit(bool ByCancel);
	virtual void __fastcall EditOperationCancelled();
	virtual void __fastcall EditOperationAccepted();
	void __fastcall FillDragImage();
	DYNAMIC void __fastcall DoStartDrag(Vcl::Controls::TDragObject* &DragObject);
	virtual void __fastcall DoDragOver(Vcl::Controls::TDragObject* Source, int X, int Y, bool CanDrop);
	DYNAMIC void __fastcall DoEndDrag(System::TObject* Target, int X, int Y);
	HIDESBASE MESSAGE void __fastcall CMDrag(Vcl::Controls::TCMDrag &Message);
	virtual bool __fastcall DragScroll(Vcl::Controls::TDragObject* Source, int X, int Y);
	void __fastcall OnScrollTimer(System::TObject* Sender);
	void __fastcall OnDragExpandTimer(System::TObject* Sender);
	virtual Vcl::Controls::TDragImageList* __fastcall GetDragImages();
	void __fastcall OnDropTargetDrag(System::TObject* Sender, System::Uitypes::TDragState State, Eldragdrop::TOleDragObject* Source, System::Classes::TShiftState Shift, int X, int Y, Eldragdrop::TDragType &DragType);
	void __fastcall OnDropTargetDrop(System::TObject* Sender, Eldragdrop::TOleDragObject* Source, System::Classes::TShiftState Shift, int X, int Y, Eldragdrop::TDragType &DragType);
	virtual System::Types::TRect __fastcall GetItemRect(int ItemIndex);
	virtual TElTreeItem* __fastcall GetItemAtY(int Y);
	virtual TElTreeItem* __fastcall GetItemAt(int X, int Y, TSTItemPart &ItemPart, int &HitColumn);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	void __fastcall ProcessPassedClick();
	void __fastcall FitMostChildren(TElTreeItem* Item);
	DYNAMIC void __fastcall DoEnter();
	DYNAMIC void __fastcall DoExit();
	virtual void __fastcall DestroyWnd();
	void __fastcall DoSetSelected(TElTreeItem* value);
	virtual int __fastcall GetVisCount2();
	virtual TElTreeItem* __fastcall FindNewFocused(System::Word Key, System::PInteger PVal1, bool &Sel);
	void __fastcall DrawMouseSelectFrame();
	void __fastcall AllocateMouseSelectFrame();
	void __fastcall DeallocateMouseSelectFrame();
	void __fastcall SelectMouseSelectItems();
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Winapi::Messages::TMessage &Message);
	void __fastcall CancelMouseSel();
	MESSAGE void __fastcall CMDeactivate(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	virtual void __fastcall InitiateEditOp(TElTreeItem* Item, int HCol, bool Immediate);
	bool __fastcall IsControlCell(TElTreeItem* Item, int SectionIndex);
	
public:
	__fastcall virtual TElTreeView(System::Classes::TComponent* Owner);
	__fastcall virtual ~TElTreeView();
	virtual void __fastcall SetFocus();
	__property TCustomElTree* Owner = {read=FOwner};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElTreeView(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TCustomElTree : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
protected:
	TElTreeInplaceManager* FEditorManager;
	System::Uitypes::TColor FStripedOddColor;
	System::Uitypes::TColor FStripedEvenColor;
	bool FStripedItems;
	TVirtualStyleNeededEvent FOnVirtualStyleNeeded;
	Ellist::TElList* FSortSections;
	TVirtualTextNeededEvent FOnVirtualTextNeeded;
	TVirtualityLevel FVirtualityLevel;
	TVirtualHintNeededEvent FOnVirtualHintNeeded;
	TVirtualValueNeededEvent FOnVirtualValueNeeded;
	TLineHintType FLineHintType;
	int FLineHintTimeout;
	int FFireFocusEvents;
	bool FTransButtons;
	bool FTransCheckBoxes;
	System::Uitypes::TColor FTrackColor;
	bool FExpandOnDragOver;
	System::Uitypes::TScrollStyle FForcedScrollBars;
	bool FMoveFocusOnCollapse;
	int FHeaderHeight;
	Elscrollbar::TElScrollHintNeededEvent FOnVertScrollHintNeeded;
	Elscrollbar::TElScrollDrawPartEvent FOnHorzScrollDrawPart;
	Elscrollbar::TElScrollHintNeededEvent FOnHorzScrollHintNeeded;
	Elscrollbar::TElScrollDrawPartEvent FOnVertScrollDrawPart;
	Elscrollbar::TElScrollHitTestEvent FOnVertScrollHitTest;
	TElTreeChangingEvent FOnChanging;
	TElHintType FHintType;
	System::Classes::TNotifyEvent FOnClick;
	System::Classes::TNotifyEvent FOnDblClick;
	Vcl::Controls::TDragDropEvent FOnDrop;
	Vcl::Controls::TDragOverEvent FOnOver;
	Vcl::Controls::TEndDragEvent FOnDrag;
	System::Classes::TNotifyEvent FOnEnter;
	System::Classes::TNotifyEvent FOnExit;
	Vcl::Controls::TKeyEvent FOnKeyDown;
	Vcl::Controls::TKeyPressEvent FOnKeyPress;
	Vcl::Controls::TKeyEvent FOnKeyUp;
	Vcl::Controls::TMouseEvent FOnMouseDown;
	Vcl::Controls::TMouseMoveEvent FOnMouseMove;
	Vcl::Controls::TMouseEvent FOnMouseUp;
	Vcl::Controls::TStartDragEvent FOnStartDrag;
	TOnItemExpandEvent FOnItemPreDraw;
	TElTreeItemDragTargetEvent FOnDragTargetChange;
	System::Uitypes::TColor FGradientStartColor;
	System::Uitypes::TColor FGradientEndColor;
	int FGradientSteps;
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FRowHotTrack;
	System::Uitypes::TColor FFocusedSelectColor;
	System::Uitypes::TColor FHideSelectColor;
	System::Uitypes::TColor FFocusedSelectTextColor;
	System::Uitypes::TColor FHideSelectTextColor;
	bool FNoBlendSelected;
	bool FScrollBackground;
	Vcl::Graphics::TBitmap* FBackground;
	Elvclutils::TElBkGndType FBackgroundType;
	bool FAdjustMultilineHeight;
	bool FFlatFocusedScrollbars;
	bool FAutoResizeColumns;
	bool FHideFocusRect;
	bool FShowEmptyImages;
	bool FShowEmptyImages2;
	bool FShowRootButtons;
	bool FUnderlineTracked;
	bool FCustomCheckboxes;
	Vcl::Graphics::TBitmap* FCheckBoxGlyph;
	Vcl::Graphics::TBitmap* FRadioButtonGlyph;
	bool FFilteredVisibility;
	TApplyVisFilterEvent FOnApplyVisFilter;
	bool FRightAlignedText;
	bool FFlat;
	bool FRightAlignedTree;
	System::WideChar FPathSeparator;
	Vcl::Graphics::TPenStyle FLinesStyle;
	System::Uitypes::TColor FLinesColor;
	bool FDeselectChildrenOnCollapse;
	bool FDrawFocusRect;
	bool FBarStyle;
	bool FAlwaysKeepFocus;
	bool FAlwaysKeepSelection;
	bool FFullRowSelect;
	TElDragType FDragType;
	bool FMouseOver;
	Eldragdrop::TElDropTarget* FDropTarget;
	Vcl::Controls::TDragObject* FDragObject;
	bool FAutoLookup;
	int FSelectColumn;
	bool FAutoExpand;
	Vcl::Graphics::TBitmap* FLeafPicture;
	Vcl::Graphics::TBitmap* FPlusPicture;
	Vcl::Graphics::TBitmap* FMinusPicture;
	bool FCustomPlusMinus;
	bool FShowHeader;
	bool FShowCheckboxes;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	Elini::TElIniFile* FStorage;
	System::UnicodeString FStoragePath;
	TDragImgMode FDragImageMode;
	bool FHideHorzScrollBar;
	bool FHideVertScrollBar;
	bool FExpandOnDblClick;
	bool FHideHintOnMove;
	int FSortSection;
	TSortModes FSortMode;
	TSortTypes FSortType;
	bool FDragAllowed;
	System::Uitypes::TColor FBkColor;
	System::Uitypes::TColor FTextColor;
	bool FShowButtons;
	bool FShowLines;
	bool FShowImages;
	bool FShowRoot;
	System::Uitypes::TColor FLineHintColor;
	THintModes FShowHintMode;
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	bool FCanEdit;
	bool FIgnoreSBChange;
	bool FScrollbarsInitialized;
	bool FSortRequired;
	bool FProcUpdate;
	bool FUpdated;
	int FInSorting;
	bool FBSVLines;
	bool FHLines;
	bool FVLines;
	Ellist::TElList* FAllList;
	Ellist::TElList* FSelectedList;
	bool FScrollTracking;
	bool FTracking;
	bool FHeaderHotTrack;
	bool FODFollowCol;
	Elstrutils::TElFString FODMask;
	Vcl::Controls::TImageList* FImages;
	Vcl::Controls::TImageList* FImages2;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	int FTopIndex;
	int FBottomIndex;
	bool FChStateImage;
	Elstrutils::TElFString FRealHint;
	Elstrutils::TElFString FHint;
	int FMainTreeCol;
	bool FMultiSelect;
	int FMultiSelectLevel;
	bool FRowSelect;
	bool FHideSelect;
	int FLineHeight;
	bool FAutoLineHeight;
	int ItemExt;
	bool FUseCustomBars;
	bool FTreeIsFocused;
	int FHPos;
	bool FVScrollVisible;
	bool FHScrollVisible;
	TSTSelModes FSelMode;
	TSortDirs FSortDir;
	bool FSelChange;
	bool FColSizeUpdate;
	bool FUpdating;
	int FUpdateCount;
	bool FHintHide;
	bool FUseSystemHintColors;
	bool IgnoreResize;
	System::Uitypes::TColor FCurBkColor;
	System::Uitypes::TColor FCurTextColor;
	bool FDelOnEdit;
	bool FAutoSizingColumns;
	TElTreeItems* FItems;
	TColumnNotifyEvent FOnColumnResize;
	TColumnNotifyEvent FOnColumnClick;
	Elheader::TElSectionRedrawEvent FOnColumnDraw;
	System::Classes::TNotifyEvent FOnResize;
	TOnItemChangeEvent FOnItemChange;
	TOnItemDrawEvent FOnItemDraw;
	TOnItemCheckedEvent FOnItemChecked;
	TOnItemExpandEvent FOnItemExpand;
	TOnItemExpandEvent FOnItemCollapse;
	TOnItemExpanding FOnItemExpanding;
	TOnItemExpanding FOnItemCollapsing;
	TOnItemExpandEvent FOnItemDelete;
	System::Classes::TNotifyEvent FOnItemFocused;
	TElTreeItemPostDrawEvent FOnItemPostDraw;
	TOnShowHintEvent FOnShowHint;
	TOnCompareItems FOnCompareItems;
	TOnPicDrawEvent FOnItemPicDraw;
	TOnPicDrawEvent FOnItemPicDraw2;
	THotTrackEvent FOnHotTrack;
	TElScrollEvent FOnScroll;
	TItemSaveEvent FOnItemSave;
	TItemSaveEvent FOnItemLoad;
	TTryEditEvent FOnTryEdit;
	TElColumnMoveEvent FOnHeaderColumnMove;
	TCellStyleSaveEvent FOnSave;
	TCellStyleSaveEvent FOnLoad;
	TItemSelChangeEvent FOnItemSelectedChange;
	Elheader::TElHeaderLookupEvent FOnHeaderLookup;
	Elheader::TElHeaderLookupDoneEvent FOnHeaderLookupDone;
	System::Classes::TNotifyEvent FOnHeaderResize;
	THeaderSectionEvent FOnHeaderSectionExpand;
	THeaderSectionEvent FOnHeaderSectionCollapse;
	Elheader::TMeasureSectionEvent FOnHeaderSectionMeasure;
	TColumnNotifyEvent FOnSectionAutoSize;
	TColumnNotifyEvent FOnSectionFilterCall;
	TMeasureItemPartEvent FOnMeasureItemPart;
	System::Classes::TNotifyEvent FOnSortBegin;
	System::Classes::TNotifyEvent FOnSortEnd;
	Vcl::Controls::TKeyEvent FOnEditKeyDown;
	TOleDragFinishEvent FOnOleDragFinish;
	TOleDragStartEvent FOnOleDragStart;
	Eldragdrop::TTargetDragEvent FOnOleTargetDrag;
	Eldragdrop::TTargetDropEvent FOnOleTargetDrop;
	int TotalHiddenCount;
	int TotalVisCount;
	int TotalVarHeightCount;
	TElTreeView* FView;
	Elheader::TElHeader* FHeader;
	Elscrollbar::TElScrollBar* FHScrollBar;
	Elscrollbar::TElScrollBar* FVScrollBar;
	Elscrollbar::TElScrollBarStyles* FHorzScrollBarStyle;
	Elscrollbar::TElScrollBarStyles* FVertScrollBarStyle;
	bool FFakeBool;
	int SavedHH;
	Vcl::Extctrls::TTimer* FDelayTimer;
	TElTreeItem* FDelayedItem;
	int FDragExpandDelay;
	int FChangeDelay;
	TDragTargetDraw FDragTrgDrawMode;
	Vcl::Controls::TMouseEvent FOnHeaderMouseDown;
	System::Classes::TNotifyEvent FOnAfterSelectionChange;
	System::Uitypes::TColor FDragRectAcceptColor;
	System::Uitypes::TColor FDragRectDenyColor;
	bool FIncrementalSearch;
	bool FRightClickSelect;
	bool FScrollbarOpposite;
	bool FVerticalLinesLong;
	Elvclutils::TElBorderSides FBorderSides;
	TInplaceEditorNeededEvent FOnInplaceEditorNeeded;
	System::Uitypes::TCursor FCursor;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	System::Uitypes::TCursor FLinkCursor;
	System::Uitypes::TColor FLinkColor;
	System::Uitypes::TFontStyles FLinkStyle;
	bool FQuickEditMode;
	Elheader::TElFieldType FMainTextType;
	Elscrollbar::TElScrollHitTestEvent FOnHorzScrollHitTest;
	bool FMouseFrameSelect;
	System::Uitypes::TColor FVertDivLinesColor;
	System::Uitypes::TColor FHorzDivLinesColor;
	int FDragScrollInterval;
	bool FShowLeafButton;
	bool FExplorerEditMode;
	int FCheckBoxSize;
	bool FIgnoreEnabled;
	int FInplaceEditorDelay;
	Vcl::Graphics::TFont* FHeaderFont;
	bool FHeaderUseTreeFont;
	bool FKeepSelectionWithinLevel;
	bool FAutoCollapse;
	bool FIgnoreResizes;
	bool FSortUseCase;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	TElDblClickMode FDblClickMode;
	bool FDoubleBuffered;
	bool InSizeMove;
	Elhook::TElHook* FHook;
	void __fastcall SetStripedOddColor(System::Uitypes::TColor Value);
	void __fastcall SetStripedEvenColor(System::Uitypes::TColor Value);
	void __fastcall SetStripedItems(bool Value);
	virtual void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	virtual void __fastcall TriggerLinkClickEvent(System::UnicodeString HRef, int X, int Y);
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	void __fastcall OnBeforeHook(System::TObject* Sender, Winapi::Messages::TMessage &Message, bool &Handled);
	virtual void __fastcall SetParent(Vcl::Controls::TWinControl* AParent);
	void __fastcall SetVirtualityLevel(TVirtualityLevel Value);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	int __fastcall GetDefaultSectionWidth();
	void __fastcall SetDefaultSectionWidth(int Value);
	void __fastcall OnHeaderSectionResize(Elheader::TCustomElHeader* Header, Elheader::TElHeaderSection* Section);
	void __fastcall OnHeaderSectionClick(Elheader::TCustomElHeader* Header, Elheader::TElHeaderSection* Section);
	void __fastcall OnHeaderSectionDelete(Elheader::TCustomElHeader* Header, Elheader::TElHeaderSection* Section);
	void __fastcall DoHeaderMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall OnHeaderSectionLookup(System::TObject* Sender, Elheader::TElHeaderSection* Section, System::UnicodeString &Text);
	void __fastcall OnHeaderSectionLookupDone(System::TObject* Sender, Elheader::TElHeaderSection* Section, System::UnicodeString Text, bool Accepted);
	void __fastcall OnHeaderExpand(Elheader::TCustomElHeader* Sender, Elheader::TElHeaderSection* Section);
	void __fastcall OnHeaderCollapse(Elheader::TCustomElHeader* Sender, Elheader::TElHeaderSection* Section);
	void __fastcall OnHeaderSectionVisChange(Elheader::TCustomElHeader* Sender, Elheader::TElHeaderSection* Section);
	void __fastcall HeaderSectionAutoSizeHandler(Elheader::TCustomElHeader* Sender, Elheader::TElHeaderSection* Section);
	void __fastcall SectionAutoSizeTransfer(Elheader::TCustomElHeader* Sender, Elheader::TElHeaderSection* Section);
	void __fastcall SectionFilterCallTransfer(Elheader::TCustomElHeader* Sender, Elheader::TElHeaderSection* Section);
	void __fastcall DoHeaderResize(System::TObject* Sender);
	virtual void __fastcall OnFontChange(System::TObject* Sender);
	void __fastcall OnSignChange(System::TObject* Sender);
	void __fastcall ImageListChange(System::TObject* Sender);
	TElTreeItem* __fastcall GetDropTarget();
	void __fastcall SetTextColor(System::Uitypes::TColor value);
	void __fastcall SetBkColor(System::Uitypes::TColor value);
	bool __fastcall GetHeaderWrapCaptions();
	void __fastcall SetHeaderWrapCaptions(bool Value);
	void __fastcall SetHeaderHotTrack(bool value);
	void __fastcall SetHeaderHeight(int value);
	void __fastcall SetShowEmptyImages(bool newValue);
	void __fastcall SetShowEmptyImages2(bool newValue);
	void __fastcall SetImages(Vcl::Controls::TImageList* Value);
	void __fastcall SetImages2(Vcl::Controls::TImageList* newValue);
	void __fastcall SetLineHintTimeout(int Value);
	void __fastcall SetLineStyle(bool Value);
	void __fastcall SetRootStyle(bool Value);
	void __fastcall SetImagesStyle(bool Value);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetButtonStyle(bool Value);
	void __fastcall SetUpdating(bool value);
	bool __fastcall GetUpdating();
	void __fastcall SetHLines(bool value);
	void __fastcall SetVLines(bool value);
	void __fastcall SetBSVLines(bool value);
	void __fastcall SetRowSelect(bool value);
	void __fastcall SetMultiSelectLevel(int Value);
	void __fastcall SetMultiSelect(bool value);
	void __fastcall SetFocused(TElTreeItem* value);
	void __fastcall SetHideSelect(bool value);
	void __fastcall SetAutoExpand(bool value);
	void __fastcall SetMoveFocusOnCollapse(bool value);
	Elheader::TElHeaderSections* __fastcall GetHeaderSections();
	void __fastcall SetHeaderSections(Elheader::TElHeaderSections* value);
	void __fastcall SetChStateImage(bool value);
	void __fastcall SetUseStdBars(bool value);
	void __fastcall SetItemIndent(int value);
	void __fastcall SetLineHeight(int value);
	void __fastcall SetAutoLineHeight(bool value);
	int __fastcall GetHeaderHeight();
	void __fastcall SetMainTreeCol(int value);
	void __fastcall SetItems(TElTreeItems* value);
	int __fastcall GetTotalVisCount();
	bool __fastcall GetDraggableSections();
	void __fastcall SetDraggableSections(bool newValue);
	void __fastcall SetSortMode(TSortModes newValue);
	void __fastcall SetSortSection(int newValue);
	bool __fastcall GetMoveColumnOnDrag();
	void __fastcall SetMoveColumnOnDrag(bool newValue);
	void __fastcall SetHideHorzScrollBar(bool newValue);
	void __fastcall SetHideVertScrollBar(bool newValue);
	Vcl::Controls::TImageList* __fastcall GetHeaderImages();
	void __fastcall SetHeaderImages(Vcl::Controls::TImageList* newValue);
	bool __fastcall GetFireFocusEvents();
	void __fastcall SetFireFocusEvents(bool Value);
	void __fastcall SetScrollbarOpposite(bool Value);
	void __fastcall SetVerticalLinesLong(bool Value);
	int __fastcall GetSelCount();
	TElTreeItem* __fastcall GetSelected();
	TElTreeItem* __fastcall GetFocused();
	void __fastcall SetSelected(TElTreeItem* newValue);
	void __fastcall SetStorage(Elini::TElIniFile* newValue);
	void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetHeaderImageForm(Elimgfrm::TElImageForm* newValue);
	Elimgfrm::TElImageForm* __fastcall GetHeaderImageForm();
	void __fastcall SetShowCheckboxes(bool newValue);
	void __fastcall SetPlusPicture(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetMinusPicture(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetCustomPlusMinus(bool newValue);
	void __fastcall SetSelectColumn(int newValue);
	void __fastcall SetDragType(TElDragType newValue);
	void __fastcall HeaderResizeTransfer(System::TObject* Sender);
	void __fastcall HeaderResizeHandler(System::TObject* Sender);
	bool __fastcall GetStickyHeaderSections();
	void __fastcall SetStickyHeaderSections(bool newValue);
	void __fastcall SetBarStyle(bool newValue);
	void __fastcall SetDrawFocusRect(bool newValue);
	void __fastcall SetLinesColor(System::Uitypes::TColor newValue);
	void __fastcall SetHorzDivLinesColor(System::Uitypes::TColor newValue);
	void __fastcall SetLinesStyle(Vcl::Graphics::TPenStyle newValue);
	void __fastcall SetRightAlignedTree(bool newValue);
	void __fastcall SetFlat(bool newValue);
	void __fastcall SetRightAlignedText(bool newValue);
	void __fastcall SetFilteredVisibility(bool newValue);
	void __fastcall SetUnderlineTracked(bool newValue);
	void __fastcall SetCustomCheckboxes(bool newValue);
	void __fastcall SetCheckBoxGlyph(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetRadioButtonGlyph(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetShowRootButtons(bool newValue);
	void __fastcall SetHideFocusRect(bool newValue);
	bool __fastcall GetLockHeaderHeight();
	void __fastcall SetLockHeaderHeight(bool newValue);
	void __fastcall SetTransButtons(bool newValue);
	void __fastcall UpdateFrame();
	void __fastcall SetHeaderActiveFilterColor(System::Uitypes::TColor newValue);
	System::Uitypes::TColor __fastcall GetHeaderActiveFilterColor();
	void __fastcall SetHeaderFilterColor(System::Uitypes::TColor newValue);
	System::Uitypes::TColor __fastcall GetHeaderFilterColor();
	void __fastcall SetHeaderFlat(bool newValue);
	bool __fastcall GetHeaderFlat();
	void __fastcall DrawFlatBorder(bool HorzTracking, bool VertTracking);
	void __fastcall DrawFlatBorderEx(HDC DC, bool HorzTracking, bool VertTracking);
	void __fastcall ReRenderAllHTMLItems();
	void __fastcall SetFlatFocusedScrollbars(bool newValue);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* newValue);
	void __fastcall SetBackgroundType(Elvclutils::TElBkGndType newValue);
	void __fastcall BackgroundChange(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall WMNCHITTEST(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TWMScroll &Msg);
	MESSAGE void __fastcall WMEnable(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSysColorChange(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	MESSAGE void __fastcall IFMCanPaintBkgnd(Winapi::Messages::TMessage &Message);
	void __fastcall SetHideSelectColor(System::Uitypes::TColor newValue);
	void __fastcall SetFocusedSelectColor(System::Uitypes::TColor newValue);
	void __fastcall SetHideSelectTextColor(System::Uitypes::TColor newValue);
	void __fastcall SetFocusedSelectTextColor(System::Uitypes::TColor newValue);
	void __fastcall SetRowHotTrack(bool newValue);
	void __fastcall SetActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetInactiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetGradientStartColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientEndColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientSteps(int newValue);
	void __fastcall SetHPosition(int value);
	void __fastcall SetVPosition(int value);
	virtual void __fastcall ClickTransfer(System::TObject* Sender);
	virtual void __fastcall DblClickTransfer(System::TObject* Sender);
	virtual void __fastcall DropTransfer(System::TObject* Sender, System::TObject* Source, int X, int Y);
	virtual void __fastcall OverTransfer(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	virtual void __fastcall DragTransfer(System::TObject* Sender, System::TObject* Target, int X, int Y);
	virtual void __fastcall EnterTransfer(System::TObject* Sender);
	virtual void __fastcall ExitTransfer(System::TObject* Sender);
	virtual void __fastcall KeyDownTransfer(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall KeyPressTransfer(System::TObject* Sender, System::WideChar &Key);
	virtual void __fastcall KeyUpTransfer(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall MouseDownTransfer(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall MouseMoveTransfer(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall MouseUpTransfer(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall StartDragTransfer(System::TObject* Sender, Vcl::Controls::TDragObject* &DragObject);
	virtual void __fastcall MeasureSectionTransfer(System::TObject* Sender, Elheader::TElHeaderSection* Section, System::Types::TPoint &Size);
	HIDESBASE void __fastcall SetCursor(System::Uitypes::TCursor newValue);
	System::Uitypes::TCursor __fastcall GetCursor();
	int __fastcall SetScrollInfo(HWND hWnd, int BarFlag, const tagSCROLLINFO &ScrollInfo, System::LongBool Redraw);
	System::LongBool __fastcall GetScrollInfo(HWND hWnd, int BarFlag, tagSCROLLINFO &ScrollInfo);
	void __fastcall SetHorzScrollBarStyle(Elscrollbar::TElScrollBarStyles* newValue);
	void __fastcall SetVertScrollBarStyle(Elscrollbar::TElScrollBarStyles* newValue);
	void __fastcall HorzScrollDrawPartTransfer(System::TObject* Sender, Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R, Elscrollbar::TElScrollBarPart Part, bool Enabled, bool Focused, bool Pressed, bool &DefaultDraw);
	void __fastcall HorzScrollHintNeededTransfer(System::TObject* Sender, int TrackPosition, Elstrutils::TElFString &Hint);
	void __fastcall VertScrollDrawPartTransfer(System::TObject* Sender, Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R, Elscrollbar::TElScrollBarPart Part, bool Enabled, bool Focused, bool Pressed, bool &DefaultDraw);
	void __fastcall VertScrollHintNeededHandler(System::TObject* Sender, int TrackPosition, Elstrutils::TElFString &Hint);
	void __fastcall VertScrollHintNeededTransfer(System::TObject* Sender, int TrackPosition, Elstrutils::TElFString &Hint);
	bool __fastcall GetHeaderInvertSortArrows();
	void __fastcall SetHeaderInvertSortArrows(bool newValue);
	void __fastcall SBChanged(System::TObject* Sender);
	void __fastcall ScrollBarMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SetForcedScrollBars(System::Uitypes::TScrollStyle newValue);
	System::Uitypes::TCursor __fastcall GetDragCursor();
	void __fastcall SetDragCursor(System::Uitypes::TCursor Value);
	void __fastcall SetTrackColor(System::Uitypes::TColor value);
	void __fastcall SetNoBlendSelected(bool newValue);
	Elheader::TElHeaderSection* __fastcall GetLockedHeaderSection();
	void __fastcall SetLockedHeaderSection(Elheader::TElHeaderSection* newValue);
	virtual void __fastcall SetAdjustMultilineHeight(bool newValue);
	virtual void __fastcall AlignControls(Vcl::Controls::TControl* AControl, System::Types::TRect &Rect);
	void __fastcall AlignPieces();
	virtual TElTreeItem* __fastcall GetRoot();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual int __fastcall CompareItems(TElTreeItem* Item1, TElTreeItem* Item2, Elheader::TElSSortMode SM, TSortTypes ST, int FSortSection);
	virtual void __fastcall SetCanEdit(bool value);
	virtual void __fastcall SetShowHeader(bool value);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Winapi::Messages::TMessage &Message);
	HIDESBASEDYNAMIC void __fastcall Resize();
	virtual int __fastcall DoGetPicture(TElTreeItem* Item);
	virtual int __fastcall DoGetPicture2(TElTreeItem* Item);
	virtual int __fastcall DefineLineHeight();
	virtual void __fastcall UpdateScrollBars();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual TElTreeItems* __fastcall CreateItems();
	virtual TElTreeItems* __fastcall CreateItemsExt(TElTreeItemClass ItemClass);
	virtual Elheader::TElHeader* __fastcall CreateHeader();
	bool __fastcall DoSetFocused(TElTreeItem* value, bool Forced);
	bool __fastcall DoSetFocusedEx(TElTreeItem* value, bool Forced, bool Delayed);
	virtual void __fastcall SetHeaderColor(System::Uitypes::TColor newValue);
	virtual System::Uitypes::TColor __fastcall GetHeaderColor();
	Elstrutils::TElFString __fastcall GetHint();
	void __fastcall SetHint(Elstrutils::TElFString newValue);
	virtual void __fastcall DoChanging(TElTreeItem* Item, bool &AllowChange);
	virtual void __fastcall DoOnColumnResize(int SectionIndex);
	virtual void __fastcall DoColumnClick(int SectionIndex);
	virtual void __fastcall DoItemFocused();
	virtual void __fastcall DoItemDraw(TElTreeItem* Item, Vcl::Graphics::TCanvas* Surface, const System::Types::TRect &R, int SectionIndex);
	virtual void __fastcall DoItemChange(TElTreeItem* Item, TItemChangeMode ItemChangeMode);
	virtual void __fastcall DoItemExpanding(TElTreeItem* Item, bool &CanProcess);
	virtual void __fastcall DoItemCollapsing(TElTreeItem* Item, bool &CanProcess);
	virtual void __fastcall DoItemChecked(TElTreeItem* Item);
	virtual void __fastcall DoItemExpand(TElTreeItem* Item);
	virtual void __fastcall DoItemCollapse(TElTreeItem* Item);
	virtual void __fastcall DoItemDelete(TElTreeItem* Item);
	virtual void __fastcall DoCompareItems(TElTreeItem* Item1, TElTreeItem* Item2, int &res);
	virtual void __fastcall DoHeaderDraw(Elheader::TCustomElHeader* Header, Vcl::Graphics::TCanvas* Canvas, Elheader::TElHeaderSection* Section, const System::Types::TRect &Rect, bool Pressed);
	virtual void __fastcall OnHeaderSectionChange(Elheader::TCustomElHeader* Sender, Elheader::TElHeaderSection* Section, Elheader::TSectionChangeMode Change);
	virtual void __fastcall OnHeaderSectionMove(Elheader::TCustomElHeader* Sender, Elheader::TElHeaderSection* Section, int OldPos, int NewPos);
	virtual void __fastcall TriggerHotTrackEvent(TElTreeItem* OldItem, TElTreeItem* NewItem);
	virtual void __fastcall TriggerScrollEvent(Vcl::Forms::TScrollBarKind ScrollBarKind, int ScrollCode);
	virtual void __fastcall TriggerHeaderColumnMoveEvent(Elheader::TElHeaderSection* Section, int OldPos, int NewPos);
	virtual void __fastcall TriggerItemSaveEvent(System::Classes::TStream* Stream, TElTreeItem* Item);
	virtual void __fastcall TriggerItemLoadEvent(System::Classes::TStream* Stream, TElTreeItem* Item);
	virtual void __fastcall TriggerItemSelectedChangeEvent(TElTreeItem* Item);
	virtual void __fastcall DoShowHint(TElTreeItem* Item, Elheader::TElHeaderSection* Section, Elstrutils::TElFString &Text, Vcl::Controls::THintWindow* HintWindow, const System::Types::TPoint &MousePos, bool &DoShowHint);
	virtual void __fastcall Paint();
	virtual void __fastcall OnHeaderSectionCreate(Elheader::TCustomElHeader* Header, Elheader::TElHeaderSection* Section);
	virtual void __fastcall TriggerHeaderLookupEvent(Elheader::TElHeaderSection* Section, System::UnicodeString &Text);
	virtual void __fastcall TriggerHeaderLookupDoneEvent(Elheader::TElHeaderSection* Section, System::UnicodeString Text, bool Accepted);
	virtual void __fastcall TriggerHeaderSectionExpandEvent(Elheader::TElHeaderSection* Section);
	virtual void __fastcall TriggerHeaderSectionCollapseEvent(Elheader::TElHeaderSection* Section);
	virtual void __fastcall TriggerMeasureItemPartEvent(TElTreeItem* Item, int PartIndex, System::Types::TPoint &Size);
	virtual void __fastcall TriggerApplyVisFilterEvent(TElTreeItem* Item, bool &Hidden);
	virtual void __fastcall TriggerItemPostDrawEvent(Vcl::Graphics::TCanvas* Canvas, TElTreeItem* Item, const System::Types::TRect &ItemRect, bool &DrawFocusRect);
	virtual void __fastcall TriggerOleTargetDragEvent(System::Uitypes::TDragState State, Eldragdrop::TOleDragObject* Source, System::Classes::TShiftState Shift, int X, int Y, Eldragdrop::TDragType &DragType);
	virtual void __fastcall TriggerOleTargetDropEvent(Eldragdrop::TOleDragObject* Source, System::Classes::TShiftState Shift, int X, int Y, Eldragdrop::TDragType &DragType);
	virtual void __fastcall TriggerOleDragStartEvent(_di_IDataObject &dataObj, _di_IDropSource &dropSource, Eldragdrop::TDragTypes &dwOKEffects);
	virtual void __fastcall TriggerOleDragFinishEvent(Eldragdrop::TDragType dwEffect, HRESULT Result);
	virtual Vcl::Controls::TDragImageList* __fastcall GetDragImages();
	void __fastcall AutoSizeAllColumns();
	void __fastcall AutoSizeColumn(int SectionIndex);
	virtual TElTreeItem* __fastcall GetTopItem();
	virtual void __fastcall SetTopItem(TElTreeItem* Item);
	virtual void __fastcall Loaded();
	TSortTypes __fastcall SectionTypeToSortType(Elheader::TElFieldType SectionType);
	virtual void __fastcall TriggerSortBegin();
	virtual void __fastcall TriggerSortEnd();
	virtual TElTreeView* __fastcall CreateView();
	virtual void __fastcall CreateWnd();
	void __fastcall StartDelayedFocus(TElTreeItem* FocusItemToReport);
	void __fastcall StopDelayedFocus();
	void __fastcall OnDelayTimer(System::TObject* Sender);
	virtual void __fastcall DoAfterSelectionChange();
	void __fastcall SetDragRectAcceptColor(const System::Uitypes::TColor Value);
	void __fastcall SetDragRectDenyColor(System::Uitypes::TColor Value);
	void __fastcall SetDragTrgDrawMode(TDragTargetDraw Value);
	int __fastcall GetVisibleRowCount();
	void __fastcall DoSetDragTrgDrawMode(TDragTargetDraw Value, bool RedrawItem);
	DYNAMIC void __fastcall DoEndDrag(System::TObject* Target, int X, int Y);
	void __fastcall UpdateDiffItems();
	void __fastcall SlowCompareItems(TElTreeItem* Item1, TElTreeItem* Item2, Elheader::TElHeaderSection* Section, int &Result);
	virtual void __fastcall TriggerVirtualTextNeeded(TElTreeItem* Item, int SectionIndex, Elstrutils::TElFString &Text);
	virtual void __fastcall TriggerVirtualHintNeeded(TElTreeItem* Item, Elstrutils::TElFString &Hint);
	virtual void __fastcall TriggerVirtualValueNeeded(TElTreeItem* Item, int SectionIndex, int VarType, System::Variant &Value);
	virtual void __fastcall TriggerVirtualStyleNeeded(TElTreeItem* Item, int SectionIndex, TElCellStyle* Style);
	virtual void __fastcall TriggerTryEditEvent(TElTreeItem* Item, int SectionIndex, Elheader::TElFieldType &CellType, bool &CanEdit);
	virtual void __fastcall TriggerInplaceEditorNeeded(TElTreeItem* Item, int SectionIndex, Elheader::TElFieldType SupposedFieldType, TElTreeInplaceEditor* &Editor);
	virtual void __fastcall VertScrollHitTestTransfer(System::TObject* Sender, int X, int Y, Elscrollbar::TElScrollBarPart &Part, bool &DefaultTest);
	virtual void __fastcall HorzScrollHitTestTransfer(System::TObject* Sender, int X, int Y, Elscrollbar::TElScrollBarPart &Part, bool &DefaultTest);
	void __fastcall SetVertDivLinesColor(System::Uitypes::TColor Value);
	void __fastcall SetCheckBoxSize(int Value);
	TElTreeItem* __fastcall GetTrackItem();
	bool __fastcall GetDragging();
	void __fastcall SetShowLeafButton(bool Value);
	void __fastcall SetLeafPicture(Vcl::Graphics::TBitmap* Value);
	void __fastcall FitMostChildren(TElTreeItem* Item);
	virtual System::WideString __fastcall GetThemedClassName();
	virtual void __fastcall SetUseXPThemes(const bool Value);
	int __fastcall GetCheckBoxSize();
	Vcl::Menus::TPopupMenu* __fastcall GetHeaderPopupMenu();
	void __fastcall SetHeaderPopupMenu(Vcl::Menus::TPopupMenu* Value);
	void __fastcall SetHeaderFont(Vcl::Graphics::TFont* Value);
	void __fastcall SetHeaderUseTreeFont(bool Value);
	void __fastcall HeaderFontChanged(System::TObject* Sender);
	bool __fastcall IsStripedColorStored();
	HIDESBASE MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	DYNAMIC void __fastcall DoEnter();
	DYNAMIC void __fastcall DoExit();
	void __fastcall SetSortUseCase(bool Value);
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetDblClickMode(TElDblClickMode Value);
	void __fastcall SetExpandOnDblClick(bool Value);
	Vcl::Graphics::TBitmap* __fastcall GetPlusPicture();
	Vcl::Graphics::TBitmap* __fastcall GetLeafPicture();
	Vcl::Graphics::TBitmap* __fastcall GetMinusPicture();
	Vcl::Graphics::TBitmap* __fastcall GetCheckBoxGlyph();
	Vcl::Graphics::TBitmap* __fastcall GetRadioButtonGlyph();
	void __fastcall OnCheckSignChange(System::TObject* Sender);
	MESSAGE void __fastcall WMUpdateSBFrame(Winapi::Messages::TMessage &Message);
	HIDESBASE void __fastcall SetDoubleBuffered(bool Value);
	__property System::Uitypes::TColor TextColor = {read=FTextColor, write=SetTextColor, default=-16777208};
	__property System::Uitypes::TColor BkColor = {read=FBkColor, write=SetBkColor, default=-16777211};
	__property bool ShowButtons = {read=FShowButtons, write=SetButtonStyle, default=1};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property bool ShowLines = {read=FShowLines, write=SetLineStyle, default=1};
	__property bool ShowImages = {read=FShowImages, write=SetImagesStyle, default=1};
	__property bool ShowRoot = {read=FShowRoot, write=SetRootStyle, default=0};
	__property THintModes LineHintMode = {read=FShowHintMode, write=FShowHintMode, default=1};
	__property System::Uitypes::TColor LineHintColor = {read=FLineHintColor, write=FLineHintColor, default=-16777211};
	__property bool HideSelection = {read=FHideSelect, write=SetHideSelect, default=0};
	__property bool HideHintOnTimer = {read=FHintHide, write=FHintHide, default=0};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property Vcl::Controls::TImageList* Images2 = {read=FImages2, write=SetImages2};
	__property bool ChangeStateImage = {read=FChStateImage, write=SetChStateImage, default=0};
	__property bool ShowColumns = {read=FShowHeader, write=SetShowHeader, default=0};
	__property TDragTargetDraw DragTrgDrawMode = {read=FDragTrgDrawMode, write=SetDragTrgDrawMode, default=2};
	__property bool DraggableSections = {read=GetDraggableSections, write=SetDraggableSections, default=0};
	__property TSTSelModes SelectionMode = {read=FSelMode, write=FSelMode, default=1};
	__property bool DoInplaceEdit = {read=FCanEdit, write=SetCanEdit, default=1};
	__property bool VerticalLines = {read=FVLines, write=SetVLines, default=0};
	__property bool BarStyleVerticalLines = {read=FBSVLines, write=SetBSVLines, default=0};
	__property bool HorizontalLines = {read=FHLines, write=SetHLines, default=0};
	__property bool ScrollTracking = {read=FScrollTracking, write=FScrollTracking, default=0};
	__property bool HotTrack = {read=FTracking, write=FTracking, default=1};
	__property bool Tracking = {read=FTracking, write=FTracking, default=1};
	__property bool RowSelect = {read=FRowSelect, write=SetRowSelect, default=1};
	__property bool MultiSelect = {read=FMultiSelect, write=SetMultiSelect, default=1};
	__property int MultiSelectLevel = {read=FMultiSelectLevel, write=SetMultiSelectLevel, default=-1};
	__property int LineHeight = {read=FLineHeight, write=SetLineHeight, nodefault};
	__property bool AutoLineHeight = {read=FAutoLineHeight, write=SetAutoLineHeight, default=1};
	__property bool HeaderHotTrack = {read=FHeaderHotTrack, write=SetHeaderHotTrack, default=1};
	__property Elheader::TElHeaderSections* HeaderSections = {read=GetHeaderSections, write=SetHeaderSections};
	__property int HeaderHeight = {read=GetHeaderHeight, write=SetHeaderHeight, nodefault};
	__property int MainTreeColumn = {read=FMainTreeCol, write=SetMainTreeCol, default=0};
	__property bool OwnerDrawByColumn = {read=FODFollowCol, write=FODFollowCol, default=1};
	__property Elstrutils::TElFString OwnerDrawMask = {read=FODMask, write=FODMask};
	__property bool DragAllowed = {read=FDragAllowed, write=FDragAllowed, default=0};
	__property TSortDirs SortDir = {read=FSortDir, write=FSortDir, default=0};
	__property TSortModes SortMode = {read=FSortMode, write=SetSortMode, default=0};
	__property int SortSection = {read=FSortSection, write=SetSortSection, default=0};
	__property TSortTypes SortType = {read=FSortType, write=FSortType, default=1};
	__property bool HideHintOnMove = {read=FHideHintOnMove, write=FHideHintOnMove, default=1};
	__property bool ExpandOnDblClick = {read=FExpandOnDblClick, write=SetExpandOnDblClick, default=1};
	__property bool MoveColumnOnDrag = {read=GetMoveColumnOnDrag, write=SetMoveColumnOnDrag, default=0};
	__property bool HideHorzScrollBar = {read=FHideHorzScrollBar, write=SetHideHorzScrollBar, default=0};
	__property bool HideVertScrollBar = {read=FHideVertScrollBar, write=SetHideVertScrollBar, default=0};
	__property Elscrollbar::TElScrollBarStyles* HorzScrollBarStyles = {read=FHorzScrollBarStyle, write=SetHorzScrollBarStyle, stored=true};
	__property Elscrollbar::TElScrollBarStyles* VertScrollBarStyles = {read=FVertScrollBarStyle, write=SetVertScrollBarStyle, stored=true};
	__property bool NoBlendSelected = {read=FNoBlendSelected, write=SetNoBlendSelected, default=0};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property Elvclutils::TElBkGndType BackgroundType = {read=FBackgroundType, write=SetBackgroundType, default=2};
	__property bool ScrollBackground = {read=FScrollBackground, write=FScrollBackground, default=0};
	__property Vcl::Controls::TImageList* HeaderImages = {read=GetHeaderImages, write=SetHeaderImages};
	__property TDragImgMode DragImageMode = {read=FDragImageMode, write=FDragImageMode, default=0};
	__property System::UnicodeString StoragePath = {read=FStoragePath, write=FStoragePath};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=SetStorage};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property Elimgfrm::TElImageForm* HeaderImageForm = {read=GetHeaderImageForm, write=SetHeaderImageForm};
	__property bool ShowCheckboxes = {read=FShowCheckboxes, write=SetShowCheckboxes, default=0};
	__property Vcl::Graphics::TBitmap* PlusPicture = {read=GetPlusPicture, write=SetPlusPicture};
	__property Vcl::Graphics::TBitmap* MinusPicture = {read=GetMinusPicture, write=SetMinusPicture};
	__property bool CustomPlusMinus = {read=FCustomPlusMinus, write=SetCustomPlusMinus, default=0};
	__property int SelectColumn = {read=FSelectColumn, write=SetSelectColumn, default=-1};
	__property bool AutoExpand = {read=FAutoExpand, write=SetAutoExpand, default=0};
	__property bool AutoLookup = {read=FAutoLookup, write=FAutoLookup, default=0};
	__property TElDragType DragType = {read=FDragType, write=SetDragType, default=1};
	__property bool FullRowSelect = {read=FFullRowSelect, write=FFullRowSelect, default=1};
	__property bool AlwaysKeepSelection = {read=FAlwaysKeepSelection, write=FAlwaysKeepSelection, default=1};
	__property bool AlwaysKeepFocus = {read=FAlwaysKeepFocus, write=FAlwaysKeepFocus, default=0};
	__property bool StickyHeaderSections = {read=GetStickyHeaderSections, write=SetStickyHeaderSections, default=0};
	__property bool BarStyle = {read=FBarStyle, write=SetBarStyle, default=0};
	__property bool DrawFocusRect = {read=FDrawFocusRect, write=SetDrawFocusRect, default=1};
	__property bool DeselectChildrenOnCollapse = {read=FDeselectChildrenOnCollapse, write=FDeselectChildrenOnCollapse, default=0};
	__property System::Uitypes::TColor HorzDivLinesColor = {read=FHorzDivLinesColor, write=SetHorzDivLinesColor, default=-16777201};
	__property System::Uitypes::TColor LinesColor = {read=FLinesColor, write=SetLinesColor, default=-16777201};
	__property Vcl::Graphics::TPenStyle LinesStyle = {read=FLinesStyle, write=SetLinesStyle, default=2};
	__property System::WideChar PathSeparator = {read=FPathSeparator, write=FPathSeparator, default=92};
	__property bool RightAlignedTree = {read=FRightAlignedTree, write=SetRightAlignedTree, default=0};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property bool RightAlignedText = {read=FRightAlignedText, write=SetRightAlignedText, default=0};
	__property bool FilteredVisibility = {read=FFilteredVisibility, write=SetFilteredVisibility, default=0};
	__property bool UnderlineTracked = {read=FUnderlineTracked, write=SetUnderlineTracked, default=1};
	__property bool CustomCheckboxes = {read=FCustomCheckboxes, write=SetCustomCheckboxes, default=0};
	__property Vcl::Graphics::TBitmap* CheckBoxGlyph = {read=GetCheckBoxGlyph, write=SetCheckBoxGlyph};
	__property Vcl::Graphics::TBitmap* RadioButtonGlyph = {read=GetRadioButtonGlyph, write=SetRadioButtonGlyph};
	__property bool ScrollbarOpposite = {read=FScrollbarOpposite, write=SetScrollbarOpposite, nodefault};
	__property bool ShowRootButtons = {read=FShowRootButtons, write=SetShowRootButtons, default=1};
	__property bool ShowEmptyImages = {read=FShowEmptyImages, write=SetShowEmptyImages, default=0};
	__property bool ShowEmptyImages2 = {read=FShowEmptyImages2, write=SetShowEmptyImages2, default=0};
	__property bool HideFocusRect = {read=FHideFocusRect, write=SetHideFocusRect, default=0};
	__property bool LockHeaderHeight = {read=GetLockHeaderHeight, write=SetLockHeaderHeight, default=0};
	__property bool AutoResizeColumns = {read=FAutoResizeColumns, write=FAutoResizeColumns, default=1};
	__property System::Uitypes::TColor HeaderActiveFilterColor = {read=GetHeaderActiveFilterColor, write=SetHeaderActiveFilterColor, default=0};
	__property System::Uitypes::TColor HeaderFilterColor = {read=GetHeaderFilterColor, write=SetHeaderFilterColor, default=-16777198};
	__property bool HeaderFlat = {read=GetHeaderFlat, write=SetHeaderFlat, default=0};
	__property bool HeaderWrapCaptions = {read=GetHeaderWrapCaptions, write=SetHeaderWrapCaptions, default=0};
	__property bool FlatFocusedScrollbars = {read=FFlatFocusedScrollbars, write=SetFlatFocusedScrollbars, default=1};
	__property System::Uitypes::TColor HideSelectColor = {read=FHideSelectColor, write=SetHideSelectColor, default=-16777201};
	__property System::Uitypes::TColor FocusedSelectColor = {read=FFocusedSelectColor, write=SetFocusedSelectColor, default=-16777203};
	__property System::Uitypes::TColor HideSelectTextColor = {read=FHideSelectTextColor, write=SetHideSelectTextColor, default=-16777200};
	__property System::Uitypes::TColor FocusedSelectTextColor = {read=FFocusedSelectTextColor, write=SetFocusedSelectTextColor, default=-16777202};
	__property bool UseCustomScrollBars = {read=FUseCustomBars, write=SetUseStdBars, default=1};
	__property bool RowHotTrack = {read=FRowHotTrack, write=SetRowHotTrack, default=0};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property int ItemIndent = {read=ItemExt, write=SetItemIndent, default=17};
	__property System::Uitypes::TColor GradientStartColor = {read=FGradientStartColor, write=SetGradientStartColor, default=0};
	__property System::Uitypes::TColor GradientEndColor = {read=FGradientEndColor, write=SetGradientEndColor, default=0};
	__property int GradientSteps = {read=FGradientSteps, write=SetGradientSteps, default=64};
	__property System::Uitypes::TCursor Cursor = {read=GetCursor, write=SetCursor, default=-2};
	__property bool HeaderInvertSortArrows = {read=GetHeaderInvertSortArrows, write=SetHeaderInvertSortArrows, default=0};
	__property bool MoveFocusOnCollapse = {read=FMoveFocusOnCollapse, write=SetMoveFocusOnCollapse, default=0};
	__property System::Uitypes::TScrollStyle ForcedScrollBars = {read=FForcedScrollBars, write=SetForcedScrollBars, default=0};
	__property bool PlusMinusTransparent = {read=FTransButtons, write=SetTransButtons, default=0};
	__property Elstrutils::TElFString Hint = {read=GetHint, write=SetHint};
	__property System::Uitypes::TColor DragRectAcceptColor = {read=FDragRectAcceptColor, write=SetDragRectAcceptColor, default=32768};
	__property System::Uitypes::TColor DragRectDenyColor = {read=FDragRectDenyColor, write=SetDragRectDenyColor, default=255};
	__property int DragExpandDelay = {read=FDragExpandDelay, write=FDragExpandDelay, default=500};
	__property bool IncrementalSearch = {read=FIncrementalSearch, write=FIncrementalSearch, nodefault};
	__property bool AdjustMultilineHeight = {read=FAdjustMultilineHeight, write=SetAdjustMultilineHeight, default=1};
	__property bool ExpandOnDragOver = {read=FExpandOnDragOver, write=FExpandOnDragOver, default=0};
	__property System::Uitypes::TCursor DragCursor = {read=GetDragCursor, write=SetDragCursor, nodefault};
	__property System::Uitypes::TColor TrackColor = {read=FTrackColor, write=SetTrackColor, default=-16777203};
	__property bool UseSystemHintColors = {read=FUseSystemHintColors, write=FUseSystemHintColors, default=0};
	__property System::Uitypes::TColor HeaderColor = {read=GetHeaderColor, write=SetHeaderColor, default=-16777201};
	__property int ChangeDelay = {read=FChangeDelay, write=FChangeDelay, default=500};
	__property bool RightClickSelect = {read=FRightClickSelect, write=FRightClickSelect, default=1};
	__property System::Uitypes::TColor StripedOddColor = {read=FStripedOddColor, write=SetStripedOddColor, stored=IsStripedColorStored, nodefault};
	__property System::Uitypes::TColor StripedEvenColor = {read=FStripedEvenColor, write=SetStripedEvenColor, stored=IsStripedColorStored, nodefault};
	__property bool StripedItems = {read=FStripedItems, write=SetStripedItems, default=0};
	__property TInplaceEditorNeededEvent OnInplaceEditorNeeded = {read=FOnInplaceEditorNeeded, write=FOnInplaceEditorNeeded};
	__property bool QuickEditMode = {read=FQuickEditMode, write=FQuickEditMode, default=0};
	__property Elheader::TElFieldType MainTextType = {read=FMainTextType, write=FMainTextType, default=1};
	__property TElHintType HintType = {read=FHintType, write=FHintType, default=2};
	__property Elscrollbar::TElScrollHitTestEvent OnVertScrollHitTest = {read=FOnVertScrollHitTest, write=FOnVertScrollHitTest};
	__property Elscrollbar::TElScrollHitTestEvent OnHorzScrollHitTest = {read=FOnHorzScrollHitTest, write=FOnHorzScrollHitTest};
	__property bool MouseFrameSelect = {read=FMouseFrameSelect, write=FMouseFrameSelect, default=1};
	__property System::Uitypes::TColor VertDivLinesColor = {read=FVertDivLinesColor, write=SetVertDivLinesColor, default=-16777201};
	__property System::Classes::TNotifyEvent OnResize = {read=FOnResize, write=FOnResize};
	__property TOnItemChangeEvent OnItemChange = {read=FOnItemChange, write=FOnItemChange};
	__property TOnItemDrawEvent OnItemDraw = {read=FOnItemDraw, write=FOnItemDraw};
	__property TOnItemCheckedEvent OnItemChecked = {read=FOnItemChecked, write=FOnItemChecked};
	__property TOnItemExpandEvent OnItemExpand = {read=FOnItemExpand, write=FOnItemExpand};
	__property TOnItemExpandEvent OnItemCollapse = {read=FOnItemCollapse, write=FOnItemCollapse};
	__property TOnItemExpanding OnItemExpanding = {read=FOnItemExpanding, write=FOnItemExpanding};
	__property TOnItemExpanding OnItemCollapsing = {read=FOnItemCollapsing, write=FOnItemCollapsing};
	__property TElScrollEvent OnScroll = {read=FOnScroll, write=FOnScroll};
	__property TOnItemExpandEvent OnItemDeletion = {read=FOnItemDelete, write=FOnItemDelete};
	__property TElTreeChangingEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property System::Classes::TNotifyEvent OnItemFocused = {read=FOnItemFocused, write=FOnItemFocused};
	__property TOnShowHintEvent OnShowLineHint = {read=FOnShowHint, write=FOnShowHint};
	__property TOnCompareItems OnCompareItems = {read=FOnCompareItems, write=FOnCompareItems};
	__property TOnPicDrawEvent OnItemPicDraw = {read=FOnItemPicDraw, write=FOnItemPicDraw};
	__property TOnPicDrawEvent OnItemPicDraw2 = {read=FOnItemPicDraw2, write=FOnItemPicDraw2};
	__property THotTrackEvent OnHotTrack = {read=FOnHotTrack, write=FOnHotTrack};
	__property TTryEditEvent OnTryEdit = {read=FOnTryEdit, write=FOnTryEdit};
	__property TItemSaveEvent OnItemSave = {read=FOnItemSave, write=FOnItemSave};
	__property TItemSaveEvent OnItemLoad = {read=FOnItemLoad, write=FOnItemLoad};
	__property TItemSelChangeEvent OnItemSelectedChange = {read=FOnItemSelectedChange, write=FOnItemSelectedChange};
	__property TCellStyleSaveEvent OnCellStyleSave = {read=FOnSave, write=FOnSave};
	__property TCellStyleSaveEvent OnCellStyleLoad = {read=FOnLoad, write=FOnLoad};
	__property System::Classes::TNotifyEvent OnSortBegin = {read=FOnSortBegin, write=FOnSortBegin};
	__property System::Classes::TNotifyEvent OnSortEnd = {read=FOnSortEnd, write=FOnSortEnd};
	__property System::Classes::TNotifyEvent OnHeaderResize = {read=FOnHeaderResize, write=FOnHeaderResize};
	__property Elheader::TElHeaderLookupEvent OnHeaderLookup = {read=FOnHeaderLookup, write=FOnHeaderLookup};
	__property Elheader::TElHeaderLookupDoneEvent OnHeaderLookupDone = {read=FOnHeaderLookupDone, write=FOnHeaderLookupDone};
	__property THeaderSectionEvent OnHeaderSectionExpand = {read=FOnHeaderSectionExpand, write=FOnHeaderSectionExpand};
	__property THeaderSectionEvent OnHeaderSectionCollapse = {read=FOnHeaderSectionCollapse, write=FOnHeaderSectionCollapse};
	__property TColumnNotifyEvent OnHeaderSectionAutoSize = {read=FOnSectionAutoSize, write=FOnSectionAutoSize};
	__property TColumnNotifyEvent OnHeaderColumnResize = {read=FOnColumnResize, write=FOnColumnResize};
	__property TColumnNotifyEvent OnHeaderColumnClick = {read=FOnColumnClick, write=FOnColumnClick};
	__property TElColumnMoveEvent OnHeaderColumnMove = {read=FOnHeaderColumnMove, write=FOnHeaderColumnMove};
	__property Elheader::TElSectionRedrawEvent OnHeaderColumnDraw = {read=FOnColumnDraw, write=FOnColumnDraw};
	__property TColumnNotifyEvent OnHeaderSectionFilterCall = {read=FOnSectionFilterCall, write=FOnSectionFilterCall};
	__property Elheader::TMeasureSectionEvent OnHeaderSectionMeasure = {read=FOnHeaderSectionMeasure, write=FOnHeaderSectionMeasure};
	__property TApplyVisFilterEvent OnApplyVisFilter = {read=FOnApplyVisFilter, write=FOnApplyVisFilter};
	__property TElTreeItemPostDrawEvent OnItemPostDraw = {read=FOnItemPostDraw, write=FOnItemPostDraw};
	__property TMeasureItemPartEvent OnMeasureItemPart = {read=FOnMeasureItemPart, write=FOnMeasureItemPart};
	__property Htmlrender::TElHTMLImageNeededEvent OnHTMLImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property System::Uitypes::TCursor LinkCursor = {read=FLinkCursor, write=FLinkCursor, default=-21};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=SetLinkColor, default=16711680};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=SetLinkStyle, nodefault};
	__property System::Classes::TNotifyEvent OnClick = {read=FOnClick, write=FOnClick};
	__property System::Classes::TNotifyEvent OnDblClick = {read=FOnDblClick, write=FOnDblClick};
	__property Vcl::Controls::TDragDropEvent OnDragDrop = {read=FOnDrop, write=FOnDrop};
	__property Vcl::Controls::TDragOverEvent OnDragOver = {read=FOnOver, write=FOnOver};
	__property System::Classes::TNotifyEvent OnEnter = {read=FOnEnter, write=FOnEnter};
	__property System::Classes::TNotifyEvent OnExit = {read=FOnExit, write=FOnExit};
	__property Vcl::Controls::TKeyEvent OnKeyDown = {read=FOnKeyDown, write=FOnKeyDown};
	__property Vcl::Controls::TKeyPressEvent OnKeyPress = {read=FOnKeyPress, write=FOnKeyPress};
	__property Vcl::Controls::TKeyEvent OnKeyUp = {read=FOnKeyUp, write=FOnKeyUp};
	__property Vcl::Controls::TMouseEvent OnMouseDown = {read=FOnMouseDown, write=FOnMouseDown};
	__property Vcl::Controls::TMouseMoveEvent OnMouseMove = {read=FOnMouseMove, write=FOnMouseMove};
	__property Vcl::Controls::TMouseEvent OnMouseUp = {read=FOnMouseUp, write=FOnMouseUp};
	__property Vcl::Controls::TStartDragEvent OnStartDrag = {read=FOnStartDrag, write=FOnStartDrag};
	__property Eldragdrop::TTargetDragEvent OnOleTargetDrag = {read=FOnOleTargetDrag, write=FOnOleTargetDrag};
	__property Eldragdrop::TTargetDropEvent OnOleTargetDrop = {read=FOnOleTargetDrop, write=FOnOleTargetDrop};
	__property TOleDragStartEvent OnOleDragStart = {read=FOnOleDragStart, write=FOnOleDragStart};
	__property TOleDragFinishEvent OnOleDragFinish = {read=FOnOleDragFinish, write=FOnOleDragFinish};
	__property Elscrollbar::TElScrollDrawPartEvent OnHorzScrollDrawPart = {read=FOnHorzScrollDrawPart, write=FOnHorzScrollDrawPart};
	__property Elscrollbar::TElScrollHintNeededEvent OnHorzScrollHintNeeded = {read=FOnHorzScrollHintNeeded, write=FOnHorzScrollHintNeeded};
	__property Elscrollbar::TElScrollDrawPartEvent OnVertScrollDrawPart = {read=FOnVertScrollDrawPart, write=FOnVertScrollDrawPart};
	__property Elscrollbar::TElScrollHintNeededEvent OnVertScrollHintNeeded = {read=FOnVertScrollHintNeeded, write=FOnVertScrollHintNeeded};
	__property Vcl::Controls::TMouseEvent OnHeaderMouseDown = {read=FOnHeaderMouseDown, write=FOnHeaderMouseDown};
	__property System::Classes::TNotifyEvent OnAfterSelectionChange = {read=FOnAfterSelectionChange, write=FOnAfterSelectionChange};
	__property TOnItemExpandEvent OnItemPreDraw = {read=FOnItemPreDraw, write=FOnItemPreDraw};
	__property TElTreeItemDragTargetEvent OnDragTargetChange = {read=FOnDragTargetChange, write=FOnDragTargetChange};
	__property int LineHintTimeout = {read=FLineHintTimeout, write=SetLineHintTimeout, default=3000};
	__property bool VerticalLinesLong = {read=FVerticalLinesLong, write=SetVerticalLinesLong, default=1};
	__property int DefaultSectionWidth = {read=GetDefaultSectionWidth, write=SetDefaultSectionWidth, nodefault};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property TLineHintType LineHintType = {read=FLineHintType, write=FLineHintType, default=2};
	__property TVirtualTextNeededEvent OnVirtualTextNeeded = {read=FOnVirtualTextNeeded, write=FOnVirtualTextNeeded};
	__property TVirtualityLevel VirtualityLevel = {read=FVirtualityLevel, write=SetVirtualityLevel, nodefault};
	__property TVirtualHintNeededEvent OnVirtualHintNeeded = {read=FOnVirtualHintNeeded, write=FOnVirtualHintNeeded};
	__property TVirtualValueNeededEvent OnVirtualValueNeeded = {read=FOnVirtualValueNeeded, write=FOnVirtualValueNeeded};
	__property TVirtualStyleNeededEvent OnVirtualStyleNeeded = {read=FOnVirtualStyleNeeded, write=FOnVirtualStyleNeeded};
	__property int CheckBoxSize = {read=GetCheckBoxSize, write=SetCheckBoxSize, default=15};
	__property int DragScrollInterval = {read=FDragScrollInterval, write=FDragScrollInterval, default=100};
	__property bool ShowLeafButton = {read=FShowLeafButton, write=SetShowLeafButton, nodefault};
	__property Vcl::Graphics::TBitmap* LeafPicture = {read=GetLeafPicture, write=SetLeafPicture};
	__property bool ExplorerEditMode = {read=FExplorerEditMode, write=FExplorerEditMode, nodefault};
	__property bool IgnoreEnabled = {read=FIgnoreEnabled, write=FIgnoreEnabled, nodefault};
	__property int InplaceEditorDelay = {read=FInplaceEditorDelay, write=FInplaceEditorDelay, default=500};
	__property Vcl::Graphics::TFont* HeaderFont = {read=FHeaderFont, write=SetHeaderFont};
	__property bool HeaderUseTreeFont = {read=FHeaderUseTreeFont, write=SetHeaderUseTreeFont, default=1};
	__property bool KeepSelectionWithinLevel = {read=FKeepSelectionWithinLevel, write=FKeepSelectionWithinLevel, nodefault};
	__property bool AutoCollapse = {read=FAutoCollapse, write=FAutoCollapse, default=0};
	__property bool SortUseCase = {read=FSortUseCase, write=SetSortUseCase, default=1};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property TElDblClickMode DblClickMode = {read=FDblClickMode, write=SetDblClickMode, default=1};
	
public:
	__fastcall virtual TCustomElTree(System::Classes::TComponent* AOwner);
	__fastcall TCustomElTree(System::Classes::TComponent* AOwner, TElTreeItemClass ItemClass);
	__fastcall virtual ~TCustomElTree();
	virtual void __fastcall Update();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall FullCollapse();
	virtual void __fastcall FullExpand();
	HIDESBASE virtual bool __fastcall CanFocus();
	HIDESBASE bool __fastcall Focused();
	virtual System::Types::TRect __fastcall GetItemRect(int ItemIndex);
	virtual TElTreeItem* __fastcall GetItemAtY(int Y);
	virtual TElTreeItem* __fastcall GetItemAt(int X, int Y, TSTItemPart &ItemPart, int &HitColumn);
	virtual void __fastcall MeasureCell(TElTreeItem* Item, int ColumnNum, System::Types::TPoint &Size);
	virtual TElTreeItem* __fastcall GetNextSelected(TElTreeItem* Prev);
	virtual void __fastcall AllSelected(Ellist::TElList* SelectedItems);
	virtual void __fastcall SelectAll();
	virtual void __fastcall InvertSelection();
	virtual void __fastcall SelectAllEx(bool IncludeHidden);
	virtual void __fastcall InvertSelectionEx(bool IncludeHidden);
	virtual void __fastcall DeselectAll();
	virtual void __fastcall DeselectAllEx(bool IncludeHidden);
	virtual void __fastcall SelectRange(TElTreeItem* FromItem, TElTreeItem* ToItem);
	virtual void __fastcall SelectRange2(TElTreeItem* FromItem, TElTreeItem* ToItem, bool SelectDisabled);
	virtual void __fastcall SelectRangeEx(TElTreeItem* FromItem, TElTreeItem* ToItem, bool IncludeHidden);
	virtual void __fastcall SelectRangeEx2(TElTreeItem* FromItem, TElTreeItem* ToItem, bool IncludeHidden, bool SelectDisabled);
	virtual void __fastcall Sort(bool recursive);
	virtual void __fastcall Save();
	virtual void __fastcall Restore();
	void __fastcall EnsureVisible(TElTreeItem* Item);
	void __fastcall EnsureVisibleBottom(TElTreeItem* Item);
	bool __fastcall IsEditing();
	virtual void __fastcall EditItem(TElTreeItem* Item, int SectionNum);
	void __fastcall EndEdit(bool ByCancel);
	virtual void __fastcall SaveStringsToStream(System::Classes::TStream* Stream);
	TElTreeItem* __fastcall GetNodeAt(int X, int Y);
	virtual void __fastcall CreateWindowHandle(const Vcl::Controls::TCreateParams &Params);
	virtual bool __fastcall IsInView(TElTreeItem* Item);
	int __fastcall MeasureColumnWidth(int ColumnNum, bool VisibleOnly);
	int __fastcall IndexInView(TElTreeItem* Item);
	void __fastcall AllSelectedEx(Ellist::TElList* SelectedItems, bool Order);
	void __fastcall AddSortSection(int Index, bool ReSort);
	void __fastcall RemoveSortSection(int Index, bool ReSort);
	void __fastcall ClearSortList(bool ReSort);
	__property int TopIndex = {read=FTopIndex, write=SetVPosition, nodefault};
	__property int BottomIndex = {read=FBottomIndex, nodefault};
	__property bool IsUpdating = {read=GetUpdating, write=SetUpdating, nodefault};
	__property TElTreeItems* Items = {read=FItems, write=SetItems};
	__property TElTreeItem* ItemFocused = {read=GetFocused, write=SetFocused};
	__property int SelectedCount = {read=GetSelCount, nodefault};
	__property bool FireFocusEvents = {read=GetFireFocusEvents, write=SetFireFocusEvents, default=1};
	__property TElTreeItem* Selected = {read=GetSelected, write=SetSelected};
	__property TElTreeItem* TopItem = {read=GetTopItem, write=SetTopItem};
	__property Vcl::Controls::TDragObject* DragObject = {read=FDragObject};
	__property TElTreeView* View = {read=FView};
	__property bool HorzScrollBarVisible = {read=FHScrollVisible, nodefault};
	__property bool VertScrollBarVisible = {read=FVScrollVisible, nodefault};
	__property Elheader::TElHeaderSection* LockedHeaderSection = {read=GetLockedHeaderSection, write=SetLockedHeaderSection};
	__property int VisibleRowCount = {read=GetVisibleRowCount, nodefault};
	__property TElTreeItem* DropTarget = {read=GetDropTarget};
	__property Elscrollbar::TElScrollBar* HScrollBar = {read=FHScrollBar};
	__property Elscrollbar::TElScrollBar* VScrollBar = {read=FVScrollBar};
	__property TElTreeItem* TrackItem = {read=GetTrackItem};
	
__published:
	__property int LeftPosition = {read=FHPos, write=SetHPosition, nodefault};
	__property Vcl::Menus::TPopupMenu* HeaderPopupMenu = {read=GetHeaderPopupMenu, write=SetHeaderPopupMenu};
	__property bool DoubleBuffered = {read=FDoubleBuffered, write=SetDoubleBuffered, default=1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElTree(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElTree : public TCustomElTree
{
	typedef TCustomElTree inherited;
	
__published:
	__property ActiveBorderType = {default=1};
	__property DragCursor;
	__property Align = {default=0};
	__property AlwaysKeepFocus = {default=0};
	__property AlwaysKeepSelection = {default=1};
	__property AutoCollapse = {default=0};
	__property AutoExpand = {default=0};
	__property AutoLineHeight = {default=1};
	__property AutoLookup = {default=0};
	__property AutoResizeColumns = {default=1};
	__property Anchors = {default=3};
	__property Action;
	__property Constraints;
	__property DockOrientation;
	__property Floating;
	__property BevelKind = {default=0};
	__property DoubleBuffered = {default=1};
	__property DragKind = {default=0};
	__property DefaultSectionWidth;
	__property AdjustMultilineHeight = {default=1};
	__property Background;
	__property BackgroundType = {default=2};
	__property BarStyle = {default=0};
	__property BarStyleVerticalLines = {default=0};
	__property BorderStyle = {default=1};
	__property BorderSides;
	__property ChangeDelay = {default=500};
	__property ChangeStateImage = {default=0};
	__property CheckBoxGlyph;
	__property CheckBoxSize = {default=15};
	__property Ctl3D;
	__property Color = {default=-16777211};
	__property Cursor = {default=-2};
	__property CustomCheckboxes = {default=0};
	__property CustomPlusMinus = {default=0};
	__property DeselectChildrenOnCollapse = {default=0};
	__property DblClickMode = {default=1};
	__property DoInplaceEdit = {default=1};
	__property DragAllowed = {default=0};
	__property DragExpandDelay = {default=500};
	__property DraggableSections = {default=0};
	__property DrawFocusRect = {default=1};
	__property DragImageMode = {default=0};
	__property DragRectAcceptColor = {default=32768};
	__property DragRectDenyColor = {default=255};
	__property DragScrollInterval = {default=100};
	__property DragTrgDrawMode = {default=2};
	__property DragType = {default=1};
	__property Enabled = {default=1};
	__property ExpandOnDblClick = {default=1};
	__property ExpandOnDragOver = {default=0};
	__property ExplorerEditMode;
	__property FilteredVisibility = {default=0};
	__property Flat = {default=0};
	__property FlatFocusedScrollbars = {default=1};
	__property FocusedSelectColor = {default=-16777203};
	__property FocusedSelectTextColor = {default=-16777202};
	__property ForcedScrollBars = {default=0};
	__property Font = {stored=true};
	__property FullRowSelect = {default=1};
	__property GradientStartColor = {default=0};
	__property GradientEndColor = {default=0};
	__property GradientSteps = {default=64};
	__property HeaderActiveFilterColor = {default=0};
	__property HeaderColor = {default=-16777201};
	__property HeaderHeight;
	__property HeaderHotTrack = {default=1};
	__property HeaderInvertSortArrows = {default=0};
	__property HeaderSections;
	__property HeaderFilterColor = {default=-16777198};
	__property HeaderFlat = {default=0};
	__property HeaderFont;
	__property HeaderUseTreeFont = {default=1};
	__property HeaderImages;
	__property HeaderWrapCaptions = {default=0};
	__property HideFocusRect = {default=0};
	__property HideHintOnTimer = {default=0};
	__property HideHintOnMove = {default=1};
	__property HideSelectColor = {default=-16777201};
	__property HideSelectTextColor = {default=-16777200};
	__property HideSelection = {default=0};
	__property HorizontalLines = {default=0};
	__property HideHorzScrollBar = {default=0};
	__property HideVertScrollBar = {default=0};
	__property Hint;
	__property HintType = {default=2};
	__property HorzDivLinesColor = {default=-16777201};
	__property HorzScrollBarStyles;
	__property IgnoreEnabled;
	__property HeaderImageForm;
	__property ImageForm;
	__property Images;
	__property Images2;
	__property InactiveBorderType = {default=3};
	__property IncrementalSearch;
	__property InplaceEditorDelay = {default=500};
	__property ItemIndent = {default=17};
	__property Items;
	__property KeepSelectionWithinLevel;
	__property LeafPicture;
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property LineHeight;
	__property LinesColor = {default=-16777201};
	__property LinesStyle = {default=2};
	__property LineHintColor = {default=-16777211};
	__property LineHintMode = {default=1};
	__property LineHintTimeout = {default=3000};
	__property LineHintType = {default=2};
	__property LockHeaderHeight = {default=0};
	__property MainTextType = {default=1};
	__property MainTreeColumn = {default=0};
	__property MinusPicture;
	__property MoveColumnOnDrag = {default=0};
	__property MoveFocusOnCollapse = {default=0};
	__property MouseFrameSelect = {default=1};
	__property MultiSelect = {default=1};
	__property MultiSelectLevel = {default=-1};
	__property OwnerDrawByColumn = {default=1};
	__property OwnerDrawMask = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PathSeparator = {default=92};
	__property PlusMinusTransparent = {default=0};
	__property PlusPicture;
	__property PopupMenu;
	__property QuickEditMode = {default=0};
	__property RadioButtonGlyph;
	__property RightAlignedText = {default=0};
	__property RightAlignedTree = {default=0};
	__property RightClickSelect = {default=1};
	__property RowHotTrack = {default=0};
	__property RowSelect = {default=1};
	__property NoBlendSelected = {default=0};
	__property ScrollBackground = {default=0};
	__property ScrollbarOpposite;
	__property ScrollTracking = {default=0};
	__property SelectColumn = {default=-1};
	__property ShowButtons = {default=1};
	__property ShowColumns = {default=0};
	__property ShowCheckboxes = {default=0};
	__property ShowEmptyImages = {default=0};
	__property ShowEmptyImages2 = {default=0};
	__property ShowHint;
	__property ShowImages = {default=1};
	__property ShowLeafButton;
	__property ShowLines = {default=1};
	__property ShowRoot = {default=0};
	__property ShowRootButtons = {default=1};
	__property SelectionMode = {default=1};
	__property SortDir = {default=0};
	__property SortMode = {default=0};
	__property SortSection = {default=0};
	__property SortType = {default=1};
	__property Storage;
	__property StoragePath = {default=0};
	__property SortUseCase = {default=1};
	__property StickyHeaderSections = {default=0};
	__property StripedOddColor;
	__property StripedEvenColor;
	__property StripedItems = {default=0};
	__property TabOrder = {default=-1};
	__property TabStop = {default=0};
	__property Tracking = {default=1};
	__property TrackColor = {default=-16777203};
	__property UnderlineTracked = {default=1};
	__property UseCustomScrollBars = {default=1};
	__property VertDivLinesColor = {default=-16777201};
	__property VerticalLines = {default=0};
	__property VerticalLinesLong = {default=1};
	__property VertScrollBarStyles;
	__property VirtualityLevel;
	__property Visible = {default=1};
	__property UseSystemHintColors = {default=0};
	__property UseXPThemes = {default=1};
	__property TextColor = {default=-16777208};
	__property BkColor = {default=-16777211};
	__property OnScroll;
	__property OnHeaderColumnClick;
	__property OnHeaderColumnDraw;
	__property OnHeaderColumnResize;
	__property OnHeaderColumnMove;
	__property OnHeaderLookup;
	__property OnHeaderLookupDone;
	__property OnHeaderResize;
	__property OnHeaderSectionExpand;
	__property OnHeaderSectionCollapse;
	__property OnHeaderSectionFilterCall;
	__property OnHeaderSectionAutoSize;
	__property OnHeaderSectionMeasure;
	__property OnHorzScrollDrawPart;
	__property OnHorzScrollHintNeeded;
	__property OnAfterSelectionChange;
	__property OnChanging;
	__property OnDragTargetChange;
	__property OnItemChange;
	__property OnItemPreDraw;
	__property OnItemDraw;
	__property OnResize;
	__property OnTryEdit;
	__property OnInplaceEditorNeeded;
	__property OnItemChecked;
	__property OnItemExpand;
	__property OnItemCollapse;
	__property OnItemExpanding;
	__property OnItemCollapsing;
	__property OnItemDeletion;
	__property OnItemFocused;
	__property OnShowLineHint;
	__property OnCompareItems;
	__property OnItemPicDraw;
	__property OnItemPicDraw2;
	__property OnItemPostDraw;
	__property OnHotTrack;
	__property OnMeasureItemPart;
	__property OnSortBegin;
	__property OnSortEnd;
	__property OnItemSave;
	__property OnItemLoad;
	__property OnItemSelectedChange;
	__property OnCellStyleSave;
	__property OnCellStyleLoad;
	__property OnVertScrollDrawPart;
	__property OnVertScrollHintNeeded;
	__property OnHTMLImageNeeded;
	__property OnLinkClick;
	__property LinkCursor = {default=-21};
	__property LinkColor = {default=16711680};
	__property LinkStyle;
	__property OnVirtualTextNeeded;
	__property OnVirtualHintNeeded;
	__property OnVirtualValueNeeded;
	__property OnVirtualStyleNeeded;
	__property OnHeaderMouseDown;
	__property OnClick;
	__property OnEnter;
	__property OnExit;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnStartDrag;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnDblClick;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnOleTargetDrag;
	__property OnOleTargetDrop;
	__property OnOleDragStart;
	__property OnOleDragFinish;
public:
	/* TCustomElTree.Create */ inline __fastcall virtual TElTree(System::Classes::TComponent* AOwner) : TCustomElTree(AOwner) { }
	/* TCustomElTree.CreateClass */ inline __fastcall TElTree(System::Classes::TComponent* AOwner, TElTreeItemClass ItemClass) : TCustomElTree(AOwner, ItemClass) { }
	/* TCustomElTree.Destroy */ inline __fastcall virtual ~TElTree() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElTree(HWND ParentWindow) : TCustomElTree(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElTreeDragObject : public Vcl::Controls::TDragControlObject
{
	typedef Vcl::Controls::TDragControlObject inherited;
	
public:
	virtual void __fastcall Finished(System::TObject* Target, int X, int Y, bool Accepted);
	virtual System::Uitypes::TCursor __fastcall GetDragCursor(bool Accepted, int X, int Y);
	__fastcall virtual ~TElTreeDragObject();
public:
	/* TBaseDragControlObject.Create */ inline __fastcall virtual TElTreeDragObject(Vcl::Controls::TControl* AControl) : Vcl::Controls::TDragControlObject(AControl) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 stsFocused = System::Int8(0x1);
static const System::Int8 stsSelected = System::Int8(0x2);
static const System::Int8 stsCut = System::Int8(0x3);
static const System::Int8 stsUnderlined = System::Int8(0x4);
static const System::Int8 stsBold = System::Int8(0x5);
static const System::Int8 stsItalic = System::Int8(0x6);
static const System::Int8 stsExpanded = System::Int8(0x7);
static const System::Int8 stsStrikeOut = System::Int8(0x8);
static const System::Int8 stiMaxState = System::Int8(0x8);
static const System::Int8 tisFocused = System::Int8(0x1);
static const System::Int8 tisSelected = System::Int8(0x2);
static const System::Int8 tisCut = System::Int8(0x4);
static const System::Int8 tisExpanded = System::Int8(0x8);
static const System::Int8 tisBold = System::Int8(0x10);
static const System::Int8 tisItalic = System::Int8(0x20);
static const System::Int8 tisUnderlined = System::Int8(0x40);
static const System::Byte tisStrikeout = System::Byte(0x80);
static const System::Int8 ibfParentColors = System::Int8(0x1);
static const System::Int8 ibfParentStyle = System::Int8(0x2);
static const System::Int8 ibfSuppressLines = System::Int8(0x4);
static const System::Int8 ibfImageDrawn = System::Int8(0x8);
static const System::Int8 ibfImageDrawn2 = System::Int8(0x10);
static const System::Int8 ibfForceButtons = System::Int8(0x20);
static const System::Int8 ibfStrikedOutLine = System::Int8(0x40);
static const System::Byte ibfDrawHLine = System::Byte(0x80);
static const System::Word ibfAllowSelection = System::Word(0x100);
static const System::Word ibfAllowEdit = System::Word(0x200);
static const System::Word ibfUseBkColor = System::Word(0x400);
static const System::Word ibfDeleting = System::Word(0x800);
static const System::Word ibfUseStyles = System::Word(0x1000);
static const System::Word ibfMultiline = System::Word(0x2000);
static const System::Word ibfHidden = System::Word(0x4000);
static const System::Word ibfEnabled = System::Word(0x8000);
static const int ibfSuppressButtons = int(0x10000);
static const int ibfCheckBoxEnabled = int(0x20000);
static const int ibfShowCheckBox = int(0x40000);
static const int ibfIsHTML = int(0x80000);
static const int ibfOwnerHeight = int(0x100000);
static const int ibfRec = int(0x200000);
static const int ibfHintIsHTML = int(0x400000);
static const System::Word CM_MOUSEWHEEL = System::Word(0xb20a);
static const System::Word WM_UPDATESBFRAME = System::Word(0x912);
extern DELPHI_PACKAGE System::ResourceString _STExOutOfBounds;
#define Eltree_STExOutOfBounds System::LoadResourceString(&Eltree::_STExOutOfBounds)
extern DELPHI_PACKAGE System::ResourceString _STexInvItem;
#define Eltree_STexInvItem System::LoadResourceString(&Eltree::_STexInvItem)
extern DELPHI_PACKAGE System::ResourceString _STexRecursiveMove;
#define Eltree_STexRecursiveMove System::LoadResourceString(&Eltree::_STexRecursiveMove)
static const System::Word TM_CLOSEINPLACEEDITOR = System::Word(0xf13);
static const System::Int8 FDivLineWidth = System::Int8(0x1);
static const System::Int8 CheckMargin = System::Int8(0x2);
static const System::Int8 CheckBoxSize = System::Int8(0xf);
static const System::Word crDragSingleNo = System::Word(0x4e21);
static const System::Word crDragSingleMove = System::Word(0x4e22);
static const System::Word crDragSingleCopy = System::Word(0x4e23);
extern DELPHI_PACKAGE System::StaticArray<int, 2> MultiLineFlags;
extern DELPHI_PACKAGE System::StaticArray<int, 2> MultiLineEllipseFlags;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* LeafBmp;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* PlusBmp;
extern DELPHI_PACKAGE Vcl::Graphics::TBitmap* MinusBmp;
}	/* namespace Eltree */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREE)
using namespace Eltree;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreeHPP
