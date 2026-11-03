#pragma once

#include "NodeBuilder/DialogNode.h"
#include "../../xrEUI/EditorWnd.h"
#include "../../../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "Tree/Properties/UIPropertiesForm.h"
#include "../Editor/Utils/ContentView.h"
#include "UIDialogLocalization.h"
#include "NodesMode.h"
#include "NodesCommon.h"

#include <imgui_node_editor.h>

namespace ed = ax::NodeEditor;

namespace ax { namespace NodeEditor {
	inline bool operator<(const PinId& A, const PinId& B)
	{
		return (uintptr_t)A < (uintptr_t)B;
	}
} }

// The Dialogs mode of the Nodes tab: gameplay\\dialogs*.xml
class DialogEditor : public INodesMode
{
public:
	DialogEditor();
	~DialogEditor() override;

	const char* GetName() const override;
	const char* GetIcon() const override;
	void GetWindows(xr_vector<SNodesWindow>& Windows) const override;
	void DrawMode() override;
	void DrawModeTopBar(float ButtonSize) override;
	bool IsModeModified() const override;
	bool SaveMode() override;
	bool ReloadMode() override;
	bool UndoMode(bool Redo) override;
	xr_string GetModePath() const override;
	void SerializeMode(nlohmann::json& Data, bool IsSaving) override;

	static DialogEditor* Instance;
	static void OpenFile(const xr_path& Path);

	void OpenFileInstance(const xr_path& Path);

	// Opens the localization window for the phrase text
	void OpenLocalization(CDialogNode* Node);
	// Called by the localization window once the text is registered
	void OnPhraseLocalized(const char* NodeName, const char* StringId);

	enum class InputBoxMode
	{
		NodeName,
		BranchName,
		DialogName,

		None
	};

	struct InputBoxState
	{
		bool IsOpen = false;
		bool HasResult = false;
		char Buffer[256] = {};
		InputBoxMode Mode = InputBoxMode::None;
	};

private:

	void NewDialog(XML_NODE* RootNode);
	void SaveDialog();
	void RebuildDialogList();

	// The history: snapshots of the whole file, one per frame with changes
	void MarkChanged();
	xr_string CaptureState();
	void CommitChange();
	void RestoreState(const xr_string& State);
	void OpenDialog(const shared_str& Str, XML_NODE* Node);

	void SelectNodeEvent(CDialogNode* Node);
	void OnPropertiesModified();

	void ChangeHasInfo(PropValue*);
	void ChangeDontHasInfo(PropValue*);
	void ChangePrecondition(PropValue*);
	void ChangeNodeHasInfo(PropValue*);
	void ChangeNodeDontHasInfo(PropValue*);
	void ChangeNodeGiveInfo(PropValue*);
	void ChangeNodePrecondition(PropValue*);
	void ChangeNodeAction(PropValue*);
	// "namespace.function" of the script fields, from EContext.Visor
	void HintScriptFunction(const xr_string& Before, xr_vector<xr_string>& Out);
	void ChangeNodeText(PropValue*);
	void ChangeNodeFinal(PropValue*);

	// The value of the Final property of the selected phrase
	bool SelectedFinal = false;
	bool RefreshProperties = false;

	void BuildIdMaps();
	CDialogNode* NodeFromId(ed::NodeId Id);
	CDialogNode* NodeOwningPin(ed::PinId Id);
	// False when the sizes of the nodes are not known yet
	bool LayoutNodes();
	void DeleteNode(CDialogNode* Node);

	void UpdateSelection();
	void DrawNode(CDialogNode* Node);
	float ComputeNodeBodyWidth(CDialogNode* Node, float MaxWidth);
	void DrawNodeBody(CDialogNode* Node, float BodyWidth);
	void DrawNodeHeaderBackground(ed::NodeId Id, float HeaderHeight, ImColor Color);
	void HandleLinkCreation();
	void HandleDeletion();
	void DrawGraph();
	void DrawOutliner();
	void DrawDetails();
	void DrawMessages();
	void DrawCanvasToolbar();
	void SelectPhrase(CDialogNode* Node);
	void DrawContextMenu();
	void HandleShortcuts();
	XML_NODE* CurrentDialogNode();

private:
	CNodesDock Dock;
	bool Modified = false;
	bool ChangePending = false;
	xr_vector<xr_string> UndoStack;
	xr_vector<xr_string> RedoStack;
	// The file now and on the disk
	xr_string LastState;
	xr_string SavedState;

	CXml File;
	// Name of the opened dialogs file, like "dialogs_jupiter.xml"
	xr_string DialogFileName;
	NodesUI::CMinimap Minimap;
	char DialogFilter[128] = {};
	// The game dialogs files from [dialogs] files, without the extension
	xr_vector<xr_string> GameDialogs;
	CUIDialogLocalization Localization;

	shared_str HasInfo;
	shared_str DontHasInfo;
	shared_str Precondition;

	XML_NODE* NodeHasInfo = nullptr;
	XML_NODE* NodeDontHasInfo = nullptr;
	XML_NODE* NodePrecondition = nullptr;

	CDialogNode* SelectedNode = nullptr;

	shared_str LastOpenDialog;

	using DialogPair = std::pair<shared_str, XML_NODE*>;
	xr_vector<DialogPair> Dialogs;
	xr_vector<std::pair<shared_str, shared_str>> Phrases;

	xr_vector<CDialogNode*> Nodes;

	ed::EditorContext* Editor = nullptr;

	struct NodeLink
	{
		ed::LinkId Id;
		ed::PinId Start;
		ed::PinId End;
		XML_NODE* NextNode;
		CDialogNode* Source;
		CDialogNode* Target;
	};
	xr_vector<NodeLink> Links;

	xr_map<CDialogNode*, ed::NodeId> NodeIds;
	xr_map<CDialogNode*, ed::PinId> InputPins;
	xr_map<CDialogNode*, xr_vector<ed::PinId>> OutputPins;

	xr_map<ed::PinId, CDialogNode*> PinNode;
	xr_map<ed::PinId, bool> PinIsOutput;
	xr_map<ed::PinId, int> OutputPinSlot;
	xr_map<ed::PinId, XML_NODE*> OutputPinXml;
	xr_set<ed::PinId> ConnectedPins;

	// The nodes, the pins and the links share the ImGui ids of the canvas: the ranges are apart
	u64 NextPinId = 1ull << 32;
	u64 NextNodeId = 1;
	u64 NextLinkId = 1ull << 48;
	bool NeedLayout = false;
	bool FocusFirstNode = false;
	bool PendingFocus = false;
	ed::NodeId PendingSelect = ed::NodeId(0);
	ImVec2 ContextMouseCanvas = { 0, 0 };

	// The minimap of the last frame, in screen space

	InputBoxState InputBox;

	UIPropertiesForm* Properties = nullptr;

	void AddBranchOutput(CDialogNode* Node);
};
