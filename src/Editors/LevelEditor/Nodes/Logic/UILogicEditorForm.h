#pragma once
#include "LogicDocument.h"
#include "LogicDebugger.h"

#include <imgui_node_editor.h>

// Node graph of the scripted logic: a section is a node, a condlist branch is a link to the section it switches to
class UILogicEditorForm :
	public IEditorWnd,
	public pureFrame
{
public:
	UILogicEditorForm();
	virtual ~UILogicEditorForm();

	virtual void Draw() override;
	// The debugger talks to the scripts here: Lua is not running in the game thread then
	virtual void OnFrame() override;
	// Keeps the opened files between the runs
	virtual void Serialize(nlohmann::json& Data, bool IsSaving) override;

	static void Create(int Tab);
	static void Destroy();
	static bool IsActive();
	static bool IsModified();

	static bool CanOpen(const xr_string& FullPath);
	static bool OpenFile(const xr_string& FullPath);
	static bool Save();
	static bool Undo(bool Redo);
	static void DrawTopBar(float ButtonSize);

private:
	enum class ESchemeKind : u8
	{
		Logic,
		Stalker,
		Monster,
		Item,
		Heli,
		Restrictor,
		Data
	};

	struct SDocument
	{
		~SDocument();

		xr_string Path;
		xr_string Name;
		CLogicDocument Logic;
		ax::NodeEditor::EditorContext* Editor = nullptr;
		// A parent section is shown alone, like a function of a Blueprint
		ax::NodeEditor::EditorContext* FunctionEditor = nullptr;
		// Empty for the event graph
		xr_string Function;
		int FunctionLayoutDelay = 0;

		bool Modified = false;
		bool UseCRLF = true;
		bool Utf8 = false;
		bool Utf8Bom = false;

		xr_vector<xr_string> UndoStack;
		xr_vector<xr_string> RedoStack;

		// Frames to wait for the node sizes before the layout
		int LayoutDelay = 2;
		bool NeedFit = false;
		int Selected = 0;
		int FocusRequest = 0;
		int FlowFrames = 0;

		// Sections that pause Play In Editor when the debugged object switches to them
		xr_set<xr_string> Breakpoints;
	};

	struct SPin
	{
		int Section = 0;
		int Line = -1;
		int Branch = -1;
		bool IsInput = false;
		// "New switch" output of a node
		bool IsAdd = false;
	};

	// A key read by the scripts: utils.cfg_get_<Type>(ini, section, "<Name>")
	struct SKeyInfo
	{
		xr_string Name;
		xr_string Type;
		// Read for every scheme by xr_logic
		bool Common = false;
	};

	struct SMessage
	{
		bool IsError = false;
		int Section = 0;
		xr_string Text;
	};

	static ax::NodeEditor::PinId InputPin(int Section);
	static ax::NodeEditor::PinId OutputPin(int Section, int Line, int Branch);
	static ax::NodeEditor::PinId AddPin(int Section);
	static SPin DecodePin(ax::NodeEditor::PinId Pin);

	// The pins of the inherited keys: Line is InheritedLine + the index of the key in CollectInherited
	static constexpr int InheritedLine = 0x8000;

	struct SInherited
	{
		const SLogicSection* Parent = nullptr;
		const SLogicLine* Line = nullptr;
	};

	static void SplitParents(const xr_string& Parents, xr_vector<xr_string>& Names);
	// The keys of the parents the section does not set itself, the nearest parent first
	static void CollectInherited(CLogicDocument& Logic, const SLogicSection& Section, xr_vector<SInherited>& Into);
	// A parent of other sections nothing switches to
	static bool IsFunction(CLogicDocument& Logic, const SLogicSection& Section);
	bool IsInView(SDocument& Document, const SLogicSection& Section);
	void OpenFunction(SDocument& Document, const xr_string& Name);
	void LayoutFunction(SDocument& Document);
	void DrawGraphViews(SDocument& Document);

	ESchemeKind GetKind(const SLogicSection& Section) const;
	static ImColor KindColor(ESchemeKind Kind);
	static const char* KindIcon(ESchemeKind Kind);

	SDocument* Current();
	bool Load(SDocument& Document);
	bool SaveDocument(SDocument& Document);
	bool CloseDocument(size_t Index);
	void Reload(SDocument& Document, const xr_string& Text);

	// Before a change of the document
	void PushUndo(SDocument& Document);
	bool UndoDocument(SDocument& Document, bool Redo);

	void LoadCatalogs();
	// Keys of the scheme of the section, the common ones included. Empty when the scheme is unknown
	const xr_vector<SKeyInfo>& GetKeys(const SLogicSection& Section);
	static void ReadScriptKeys(const char* FileName, bool Common, xr_vector<SKeyInfo>& Into);
	void DrawKeySuggestions(SDocument& Document, SLogicSection& Section, bool& Changed);
	void Validate(SDocument& Document);
	void Layout(SDocument& Document);

	void DrawToolbar(SDocument* Document, float ButtonSize);
	void DrawGraph(SDocument& Document);
	void DrawNode(SDocument& Document, SLogicSection& Section);
	void DrawLinks(SDocument& Document);
	void HandleCreate(SDocument& Document);
	void HandleDelete(SDocument& Document);
	void DrawContextMenus(SDocument& Document);
	void DrawCreateMenu(SDocument& Document);

	void DrawOutliner(SDocument* Document);
	void DrawDetails(SDocument* Document);
	void DrawMessages(SDocument* Document);
	bool DrawTokens(SDocument& Document, const char* Id, xr_vector<LogicCondlist::SToken>& Tokens, bool Effects);
	bool DrawFunctionPicker(const char* Id, bool Effects, LogicCondlist::SToken& Token);

	SLogicSection& CreateSection(SDocument& Document, const xr_string& Scheme, ImVec2 Position);
	void Link(SDocument& Document, const SPin& From, const xr_string& Target);

	static UILogicEditorForm* Form;

	xr_vector<xr_unique_ptr<SDocument>> Documents;
	size_t Active = 0;
	int SelectRequest = -1;

	// From modules.script, xr_conditions.script, xr_effects.script
	bool CatalogsLoaded = false;
	xr_vector<std::pair<xr_string, ESchemeKind>> Schemes;
	xr_vector<xr_string> Conditions;
	xr_vector<xr_string> Effects;
	xr_hash_map<xr_string, xr_string> SchemeFiles;
	xr_vector<SKeyInfo> CommonKeys;
	xr_vector<SKeyInfo> LogicKeys;
	xr_hash_map<xr_string, xr_vector<SKeyInfo>> SchemeKeys;

	xr_vector<SMessage> Messages;

	// Play In Editor: one object is watched, like the debug object of a Blueprint
	struct SDebugHistory
	{
		float Time = 0.0f;
		xr_string From;
		xr_string To;
	};

	struct SDebugState
	{
		int ObjectId = -1;
		xr_string ObjectName;
		xr_string IniFile;
		xr_string LogicSection;
		// The document of the object
		xr_string Path;
		xr_string Active;
		// Pin of a branch of the active section: CLogicDebugger::EResult, Always for no conditions
		xr_hash_map<u64, s8> Results;
		xr_vector<SDebugHistory> History;
		xr_vector<CLogicDebugger::SObject> Objects;
		xr_string PendingSwitch;
		bool UseCpp = true;
		bool Paused = false;
		double NextPoll = 0.0;
		double NextObjects = 0.0;
		int Polls = 0;
		bool Flow = false;
	};

	static constexpr s8 ResultAlways = 2;

	bool IsDebugged(const SDocument& Document) const;
	void StartDebug(const CLogicDebugger::SObject& Object);
	void StopDebug();
	void EvaluateActive();
	void ToggleBreakpoint(SDocument& Document, const xr_string& Section);
	void SetPaused(bool Paused);
	void DrawDebugger(SDocument* Document);

	CLogicDebugger Debugger;
	SDebugState Debug;
	char DebugFilter[64] = {};

	// The windows of the tab are not drawn on the other tabs: the dock tab bars are made again
	// in the order of the windows, so the selected tab of each dock is put back
	bool BeginDocked(const char* Name);
	xr_hash_map<u32, u32> DockSelection;
	int LastDrawFrame = -10;
	int RestoreFrames = 0;

	// Node to create where a dragged link was dropped
	SPin PendingLink;
	bool HasPendingLink = false;
	ImVec2 CreatePosition = { 0, 0 };
	int ContextSection = 0;

	char OutlinerFilter[64] = {};
	char PickerFilter[64] = {};
	char NewKey[64] = {};
	bool ShowKeyList = false;
	bool ShowProperties = true;
	xr_string StatusText;
};
