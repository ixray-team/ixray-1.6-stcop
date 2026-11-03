#pragma once
#include <span>

// Widget kinds the layout editor can place. Each one maps to a CScriptXmlInit::Init* call
enum class ELayoutWidget : u8
{
	Window,
	Static,
	TextWnd,
	Frame,
	Button,
	Check,
	EditBox,
	TrackBar,
	ProgressBar,
	ScrollView,
	ListBox,
	ComboBox,
	FrameLine,
	AnimStatic,
	StackPanel,
	TabControl,
	SpinNum,
	SpinFloat,
	SpinText,
	ArrowStepper,
	ListWnd,
	Count
};

enum class ELayoutAlign : u8
{
	Left,
	Center,
	Right
};

enum class ELayoutVertAlign : u8
{
	Top,
	Center,
	Bottom
};

// A ui_events message a widget type can send to the dialog
struct SLayoutEventInfo
{
	const char* Event;
	// Default handler suffix: OnBtnOkClicked
	const char* Suffix;
};

static constexpr u32 MaxLayoutEvents = 3;

// Subscription of the widget to one event of its type, see LayoutWidget::GetEvents
struct SLayoutEventBinding
{
	bool Enabled = false;
	// Empty: On<Name><Suffix>
	xr_string Handler;
};

struct SLayoutWidget
{
	u32 Id = 0;
	// 0: the dialog window itself
	u32 ParentId = 0;
	ELayoutWidget Type = ELayoutWidget::Static;
	xr_string Name;

	// Position is relative to the parent, both are in 1024x768 UI units
	Fvector2 Pos = { 0.0f, 0.0f };
	Fvector2 Size = { 100.0f, 30.0f };

	xr_string Texture;
	bool Stretch = false;

	// Text or string table id, UTF-8
	xr_string Text;
	xr_string Font = "letterica16";
	u32 TextColor = color_rgba(255, 255, 255, 255);
	ELayoutAlign Align = ELayoutAlign::Left;
	ELayoutVertAlign VertAlign = ELayoutVertAlign::Center;

	// Track bar and progress bar range
	float Min = 0.0f;
	float Max = 100.0f;
	float Value = 0.0f;
	float Step = 1.0f;
	bool IsInteger = true;
	bool Horizontal = true;

	// List box item height
	float ItemHeight = 20.0f;

	// Options item binding: <options_item entry="..." group="..."/>
	xr_string OptionsEntry;
	xr_string OptionsGroup;

	// Indexed like LayoutWidget::GetEvents(Type). Every enabled one gets
	// an AddCallback and a handler in the script
	SLayoutEventBinding Events[MaxLayoutEvents];

	// Editor only
	bool Hidden = false;
	bool Locked = false;
};

namespace LayoutWidget
{
	const char* GetTypeName(ELayoutWidget Type);
	const char* GetInitFunction(ELayoutWidget Type);
	const char* GetDefaultName(ELayoutWidget Type);
	// Events the widget type sends, empty when none
	std::span<const SLayoutEventInfo> GetEvents(ELayoutWidget Type);
	// Index in GetEvents(Type), -1 when the type doesn't send the event
	int FindEvent(ELayoutWidget Type, const char* Event);
	bool HasEnabledEvents(const SLayoutWidget& Widget);
	bool HasTexture(ELayoutWidget Type);
	bool HasText(ELayoutWidget Type);
	bool HasOptions(ELayoutWidget Type);
	// Default widget for a fresh drop on the canvas
	SLayoutWidget MakeDefault(ELayoutWidget Type);
}

// A scripted UI dialog: <Name>.xml, <Name>_16.xml and <Name>.script
class CUILayoutDocument
{
public:
	static constexpr const char* WidgetExt = ".uiw";
	static constexpr float ScreenWidth = 1024.0f;
	static constexpr float ScreenHeight = 768.0f;
	// Widescreen files are squeezed horizontally, like the stock *_16.xml
	static constexpr float WideScale = 0.8f;

	void Clear();

	SLayoutWidget* Find(u32 Id);
	const SLayoutWidget* Find(u32 Id) const;
	SLayoutWidget* FindByName(const char* WidgetName);
	SLayoutWidget& Add(const SLayoutWidget& Source, u32 Parent);
	// Removes the widget with all its children
	void Remove(u32 Id);
	// Copies the widget with all its children next to it, returns the id of the copy
	u32 Duplicate(u32 Id);
	// Keeps the absolute position. Fails when NewParent is the widget or one of its children
	bool Reparent(u32 Id, u32 NewParent);
	// Swaps the widget with its previous (Direction < 0) or next sibling in the draw order
	bool MoveSibling(u32 Id, int Direction);

	bool IsAncestor(u32 Ancestor, u32 Id) const;
	Fvector2 GetAbsPos(u32 Id) const;
	xr_string MakeUniqueName(const char* Base, u32 IgnoreId = 0) const;
	bool IsValidName(const char* WidgetName) const;
	// Ids in the order of the script: every parent comes before its children
	xr_vector<u32> GetOrdered() const;

	xr_string BuildXml(bool Wide) const;
	// Whole script for a new file
	xr_string BuildScript() const;
	// Updates the generated region of an existing script and adds missing handlers.
	// Returns false when the script has no generated region
	bool MergeScript(const xr_string& Existing, xr_string& Out) const;

	// Script is the whole script text: its generated region is updated before writing
	bool Save(const xr_string& Script, xr_string& Error) const;
	bool Load(const char* LayoutName, xr_string& Error);
	// Working copy for the editor: rawdata\widgets\<Name>.uiw keeps the widgets with
	// their types, parents, events and editor flags, so they load without parsing the xml
	bool SaveWidget(xr_string& Error) const;
	bool LoadWidget(const char* LayoutName, xr_string& Error);
	static bool WidgetExists(const char* LayoutName);
	// Names of the saved working widgets
	static void ListWidgets(xr_vector<xr_string>& Out);
	// Takes a stock window: nested nodes become child widgets, the types are guessed from the
	// nodes and the names made unique. FileName is relative to configs, the layout gets a new name
	bool Import(const char* FileName, xr_string& Error);
	// configs\ui xml files that can be imported
	static void ListImportable(xr_vector<xr_string>& Out);
	// Replaces the widgets with the ones of the xml. Types, parents and callbacks come from the script
	bool ApplyXml(const xr_string& XmlText, const xr_string& Script, xr_string& Error);
	// The layout script as UTF-8 with "\n" line breaks
	static bool ReadScript(const char* LayoutName, xr_string& Out);

	// Lua class name from the layout name: ui_my_dialog -> my_dialog
	xr_string GetClassName() const;
	xr_string GetHandlerName(const SLayoutWidget& Widget, u32 EventIndex) const;
	xr_string GetDefaultHandlerName(const SLayoutWidget& Widget, u32 EventIndex) const;
	xr_string GetVarName(u32 Id) const;

public:
	xr_string Name = "ui_new_dialog";
	xr_vector<SLayoutWidget> Widgets;
	u32 NextId = 1;
};
