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
	void OpenDialog(const shared_str& Str, XML_NODE* Node);

	float IterateChild(Fvector2 Offset);
	void SelectNodeEvent(INodeUnknown* Node);
public:
	static void OpenFile(const xr_path& Path);

private:
	bool IsOpen = false;
	bool IsOpenList = true;
	xr_map<shared_str, XML_NODE*> Dialogs;
	CXml File;
};