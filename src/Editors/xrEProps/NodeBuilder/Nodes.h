#pragma once

enum ELinkType
{
	eDefault,
	eShape,
	eJoint,
	eCommand
};

void RegNode(size_t NodeID, ELinkType Type);
int& GetLinkDrawCounter();

struct LinkData
{
	xr_string Name;
	size_t ID;
	ELinkType Type;
	ImColor Color;
	bool IsIn = false;
};

class XREPROPS_API INodeUnknown
{
protected:
	xr_vector<LinkData> OutLinks;
	xr_vector<LinkData> InLinks;

	xr_vector<std::pair<int, int>> LinkStorage;

	ImColor Background = { 32, 32, 32};
	ImColor Header = { 32, 32, 132};
	
	bool WeStarted = false;
	bool IsHovered = false;


public:
	xr_string NodeName;

	xr_vector<INodeUnknown*> Childs;
	xr_vector<INodeUnknown*> OutNodes;

	LinkData ContactLinkIn;
	LinkData ContactLinkOut;

	size_t NodeID = 0;
	ImVec2 StartPostion = { 0, 0 };

public:
	INodeUnknown() = delete;
	INodeUnknown(const char* Name);

	virtual void Draw();
	virtual void AddContactLink(const xr_string& Name, bool IsOut = false);
	virtual void AddOutLink(const xr_string& Name, ELinkType Type = eDefault, ImColor Color = { 55, 55, 122 });
	virtual void AddInLink(const xr_string& Name, ELinkType Type = eDefault, ImColor Color = { 55, 55, 122 });

	int GetContactLink(bool IsOut = false) const;
	void SetStartPos(float x, float y);

	int GetLink(ELinkType Type, bool IsOut = false);

	void AddChild(INodeUnknown* Node, ELinkType Type);
	void CreateContactLink(int Parent, int Child);

	INodeUnknown* GetNextNode();
	virtual void MakeOutNode(INodeUnknown* Node);
protected:
	virtual void DrawHeader();
	virtual void DrawEnd();
	virtual void DrawLinks(bool Header);
};