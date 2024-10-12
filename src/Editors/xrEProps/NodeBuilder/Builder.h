#pragma once
#include "NodeBuilder/BoneNodes.h"

class XREPROPS_API CNodeViewport:
	public IEditorWnd
{
	friend void RegNode(size_t NodeID, ELinkType Type);

	void* Context = nullptr;
	xr_hash_map<size_t, ELinkType> LinksStorage;

protected:
	xr_vector<INodeUnknown*> Nodes;
	xr_vector<std::pair<int, int>> Links;

public:
	int LinkDrawCounter = 0;

protected:
	bool CanCreateLink(size_t LeftID, size_t RightID);

public:
	CNodeViewport();
	virtual ~CNodeViewport();

	virtual void Draw() override;
	virtual void DrawEnd();
};