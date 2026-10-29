#pragma once
#include "NodeBuilder/MacroNodes.h"
#include "NodesMode.h"
#include "NodesCommon.h"
#include "Tree/Properties/UIPropertiesForm.h"

#include <imgui_node_editor.h>

// The Macros mode of the Nodes tab: editor commands run one after another from the Start node
class CUIMacroView :
	public INodesMode
{
public:
	CUIMacroView();
	virtual ~CUIMacroView();

	virtual const char* GetName() const override;
	virtual const char* GetIcon() const override;
	virtual void GetWindows(xr_vector<SNodesWindow>& Windows) const override;
	virtual void DrawMode() override;
	virtual void DrawModeTopBar(float ButtonSize) override;
	virtual bool IsModeModified() const override;
	virtual bool SaveMode() override;
	virtual bool ReloadMode() override;
	virtual bool UndoMode(bool Redo) override;
	virtual xr_string GetModePath() const override;
	virtual void SerializeMode(nlohmann::json& Data, bool IsSaving) override;

	void Exec();

private:
	struct SCommand
	{
		xr_string Name;
		u32 Command = 0;
		MacroType Type = MacroType::eVoid;
		bool IntArg = false;
		u32 Group = 0;
	};

	struct SNode
	{
		int Id = 0;
		// Index in Commands, -1 for the Start node
		int Command = -1;
		xr_string First;
		xr_string Second;
		// The nodes run after this one, in this order
		xr_vector<int> Next;

		ImVec2 Position = { 0, 0 };
		bool HasPosition = false;
	};

	struct SMessage
	{
		bool IsError = false;
		int Node = 0;
		xr_string Text;
	};

	static constexpr int StartId = 1;

	static ax::NodeEditor::PinId InputPin(int Node);
	static ax::NodeEditor::PinId OutputPin(int Node);
	static ax::NodeEditor::LinkId LinkOf(int From, int To);
	static int PinNode(ax::NodeEditor::PinId Pin, bool& IsOutput);

	SNode* Find(int Id);
	const SCommand* CommandOf(const SNode& Node) const;
	int FindCommand(const xr_string& Name) const;
	SNode& CreateNode(int Command, ImVec2 Position);
	void RemoveNode(int Id);

	nlohmann::json Write() const;
	void Read(const nlohmann::json& Data);
	bool LoadFile();
	void PushUndo();
	void MarkModified();

	void ExecNode(const SNode& Node, xr_set<int>& Visited);
	void Layout();

	void DrawGraph();
	void DrawNode(SNode& Node);
	void HandleCreate();
	void HandleDelete();
	void DrawContextMenus();
	bool DrawCommandList(const char* Id, char* Filter, size_t FilterSize, int& Picked);
	void DrawOutliner();
	void DrawDetails();
	// The properties of the selected node
	void FillProperties();
	void OnPropertyChanged(PropValue* Value);
	void DrawMessages();

	CNodesDock Dock;
	ax::NodeEditor::EditorContext* Editor = nullptr;

	xr_vector<SCommand> Commands;
	xr_vector<SNode> Nodes;
	int NextId = StartId + 1;
	int Selected = 0;
	int FocusRequest = 0;
	bool NeedFit = true;
	int LayoutDelay = 0;

	xr_vector<xr_string> UndoStack;
	xr_vector<xr_string> RedoStack;
	// The state on the disk: an undo back to it is not a change
	xr_string SavedState;
	NodesUI::CMinimap Minimap;
	bool Modified = false;

	xr_vector<SMessage> Messages;

	// Node to create where a dragged link was dropped
	int PendingLink = 0;
	ImVec2 CreatePosition = { 0, 0 };
	int ContextNode = 0;
	char CreateFilter[64] = {};
	char OutlinerFilter[64] = {};

	UIPropertiesForm* Properties = nullptr;
	// The node the properties were made for: -1 to make them again
	int PropertiesNode = -1;
	shared_str PropFirst;
	shared_str PropSecond;
	u32 PropFirstInt = 0;
	// The graph before the edit of a property
	xr_string PropertiesSnapshot;
};
