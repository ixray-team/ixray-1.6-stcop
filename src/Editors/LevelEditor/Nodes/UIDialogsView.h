#pragma once
#include "../../xrEProps/NodeBuilder/DialogNode.h"
#include "../../xrEProps/NodeBuilder/Builder.h"

#include "../../../xrCore/FormatParsers/XML/xrXMLParser.h"

class CUIDialogView:
	public CNodeViewport
{
private:
	CUIDialogView();
	~CUIDialogView();

	virtual void Draw() override;
	void Show(bool State);
	void NewDialog(XML_NODE* RootNode);
	void SaveDialog();
	void OpenDialog(const shared_str& Str, XML_NODE* Node);

	void SelectNodeEvent(INodeUnknown* Node);
	void ChangeHasInfo(PropValue*);
	void ChangeDontHasInfo(PropValue*);
	void ChangePrecondition(PropValue*);

	void ChangeNodeHasInfo(PropValue*);
	void ChangeNodeDontHasInfo(PropValue*);
	void ChangeNodeGiveInfo(PropValue*);

public:
	static void OpenFile(const xr_path& Path);

private:
	enum class DialogInputBoxMode
	{
		NodeName,
		DialogName,

		None
	};

private:
	bool IsOpenList = true;
	bool IsAutoHide = true;

	CXml File;

	DialogInputBoxMode InputBoxMode = DialogInputBoxMode::None;

	shared_str HasInfo;
	shared_str DontHasInfo;
	shared_str Precondition;

	XML_NODE* NodeHasInfo;
	XML_NODE* NodeDontHasInfo;
	XML_NODE* NodePrecondition;

	CDialogNode* LastClickedDialogNode = nullptr;

	shared_str LastOpenDialog;

	using DialogPair = std::pair<shared_str, XML_NODE*>;

	xr_vector<DialogPair> Dialogs;
	xr_vector<std::pair<shared_str, shared_str>> Phrases;
};