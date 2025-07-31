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
	void SaveDialog();
	void OpenDialog(const shared_str& Str, XML_NODE* Node);

	void SelectNodeEvent(INodeUnknown* Node);
public:
	static void OpenFile(const xr_path& Path);

private:
	void ChangeHasInfo(PropValue*);
	void ChangeDontHasInfo(PropValue*);
	void ChangePrecondition(PropValue*);

	void ChangeNodeHasInfo(PropValue*);
	void ChangeNodeDontHasInfo(PropValue*);
	void ChangeNodeGiveInfo(PropValue*);

private:
	bool IsOpenList = true;
	bool IsAutoHide = true;

	CXml File;

	shared_str HasInfo;
	shared_str DontHasInfo;
	shared_str Precondition;

	XML_NODE* NodeHasInfo;
	XML_NODE* NodeDontHasInfo;
	XML_NODE* NodePrecondition;

	CDialogNode* LastClickedDialogNode = nullptr;

	shared_str LastOpenDialog;

	xr_vector<std::pair<shared_str, shared_str>> Phrases;
	xr_vector<std::pair<shared_str, XML_NODE*>> Dialogs;
};