#pragma once

#include "../../xrEProps/NodeBuilder/DialogNode.h"
#include "../../xrEUI/EditorWnd.h"
#include "../../../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "../../xrEProps/Tree/Properties/UIPropertiesForm.h"
#include "../Editor/Utils/ContentView.h"

#include <imgui_node_editor.h>

namespace ed = ax::NodeEditor;

namespace ax { namespace NodeEditor {
	inline bool operator<(const PinId& A, const PinId& B)
	{
		return (uintptr_t)A < (uintptr_t)B;
	}
} }

class DialogEditor : public IEditorWnd
{
public:
	DialogEditor();
	~DialogEditor() override;

	void Draw() override;

	static DialogEditor* Instance;
	static void OpenFile(const xr_path& Path);

	void Show(bool State);
	void OpenFileInstance(const xr_path& Path);

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
	void OpenDialog(const shared_str& Str, XML_NODE* Node);

	void SelectNodeEvent(CDialogNode* Node);

	void ChangeHasInfo(PropValue*);
	void ChangeDontHasInfo(PropValue*);
	void ChangePrecondition(PropValue*);
	void ChangeNodeHasInfo(PropValue*);
	void ChangeNodeDontHasInfo(PropValue*);
	void ChangeNodeGiveInfo(PropValue*);

	void BuildIdMaps();
	CDialogNode* NodeFromId(ed::NodeId Id);
	CDialogNode* NodeOwningPin(ed::PinId Id);
	void LayoutNodes();
	void DeleteNode(CDialogNode* Node);

	void UpdateSelection();
	void DrawNode(CDialogNode* Node);
	float ComputeNodeBodyWidth(CDialogNode* Node, float MaxWidth);
	void DrawNodeBody(CDialogNode* Node, float BodyWidth);
	void DrawNodeHeaderBackground(ed::NodeId Id, float HeaderHeight, ImColor Color);
	void HandleLinkCreation();
	void HandleDeletion();
	void DrawMinimap();

private:
	bool IsOpen = false;
	bool IsOpenList = true;
	bool IsAutoHide = true;

	CXml File;

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

	int NextPinId = 1000000;
	int NextNodeId = 1;
	int NextLinkId = 1;
	bool NeedLayout = false;
	bool FocusFirstNode = false;
	ed::NodeId PendingSelect = ed::NodeId(0);
	ImVec2 ContextMouseCanvas = { 0, 0 };

	InputBoxState InputBox;

	UIPropertiesForm* Properties = nullptr;
	CContentView* ContentBrowser = nullptr;

	void AddBranchOutput(CDialogNode* Node);
};
