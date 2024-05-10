#pragma once

enum ELinkType
{
	eDefault,
	eShape,
	eJoint
};

void RegNode(size_t NodeID, ELinkType Type);

struct LinkData
{
	xr_string Name;
	size_t ID;
	ELinkType Type;
	ImColor Color;
	bool IsIn = false;
};

class INodeUnknown
{
protected:
	xr_string NodeName;
	size_t NodeID = 0;

	xr_vector<LinkData> ContactLink;
	xr_vector<LinkData> OutLinks;
	xr_vector<LinkData> InLinks;

	ImColor Background = { 32, 32, 32};

public:
	INodeUnknown() = delete;
	INodeUnknown(const char* Name);

	virtual void Draw() {};
	virtual void AddContactLink(const xr_string& Name, bool IsOut = false);
	virtual void AddOutLink(const xr_string& Name, ELinkType Type = eDefault, ImColor Color = { 55, 55, 122 });
	virtual void AddInLink(const xr_string& Name, ELinkType Type = eDefault, ImColor Color = { 55, 55, 122 });

	int GetContactLink(bool IsOut = false);
protected:
	virtual void DrawHeader();
	virtual void DrawEnd();
	virtual void DrawLinks(bool Header);
};