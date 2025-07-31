#pragma once

#include "Nodes.h"

class XREPROPS_API CDialogNode :
	public INodeUnknown
{
public:
	CDialogNode(const xr_string Name);
	void Draw() override;

public:
	shared_str HasInfo;
	shared_str DontHasInfo;
	shared_str GiveInfo;
	shared_str Action;
	shared_str Precondition;
	shared_str Text;
};