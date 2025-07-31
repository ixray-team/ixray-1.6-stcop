#pragma once
#include "Nodes.h"

#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"

class XREPROPS_API CDialogNode final:
	public INodeUnknown
{
public:
	CDialogNode(const xr_string Name);

	virtual void Draw() override;
	virtual void AddContactLink(const xr_string& Name, bool IsOut = false) override;
	virtual void MakeOutNode(INodeUnknown* Node) override;

	void ValidateNodes(shared_str& Data, const char* RawName);

public:
	shared_str HasInfo;
	shared_str DontHasInfo;
	shared_str GiveInfo;
	shared_str Action;
	shared_str Precondition;
	shared_str Text;

	bool IsFinal = false;

	XML_NODE* IsFinalNode = nullptr;
	xr_vector<XML_NODE*> HasInfoNode;
	xr_vector<XML_NODE*> DontHasInfoNode;
	xr_vector<XML_NODE*> GiveInfoNode;

	XML_NODE* ActionNode = nullptr;
	XML_NODE* PreconditionNode = nullptr;
	XML_NODE* TextNode = nullptr;

	XML_NODE* ParentNode = nullptr;

private:
	void RenderItemString(const char* RawName, const char* Name, shared_str& Data, XML_NODE*& Node, size_t Size);
	void RenderItemString(const char* RawName, const char* Name, shared_str& Data, xr_vector<XML_NODE*>& Node, size_t Size);
};