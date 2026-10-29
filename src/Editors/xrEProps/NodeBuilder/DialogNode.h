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
	virtual void MakeOutNode(INodeUnknown* Node, bool Silent) override;
			void DestroyContacts();

	void ValidateNodes(shared_str& Data, const char* RawName);
	void DrawInlineEditor(float Width);
	void SetText(const shared_str& NewText);
	void SetFinal(bool Value);
	// Writes a single child element, an empty value removes it
	void SetElement(const char* RawName, const shared_str& Value, XML_NODE*& Node);

	// Set by the dialog editor: finds the UTF-8 text of a string id
	static bool (*FindTranslation)(const shared_str& Id, xr_string& OutText);
	// Set by the dialog editor: opens the localization of the phrase text
	static void (*RequestLocalization)(CDialogNode* Node);

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

	bool IsBranch = false;
	xr_vector<XML_NODE*> NextNodes;

private:
	void RenderItemString(const char* RawName, const char* Name, shared_str& Data, XML_NODE*& Node, float Width);
	void RenderItemString(const char* RawName, const char* Name, shared_str& Data, xr_vector<XML_NODE*>& Node, float Width);
	void RenderField(const char* RawName, const char* Name, shared_str& Data, XML_NODE*& Node, float LabelWidth, float Width);
	void RenderField(const char* RawName, const char* Name, shared_str& Data, xr_vector<XML_NODE*>& Node, float LabelWidth, float Width);
	void RenderTranslation(float Width);
};