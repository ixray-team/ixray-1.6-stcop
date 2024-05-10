////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph.h
//	Created 	: 02.10.2001
//  Modified 	: 11.11.2003
//	Author		: Oles Shihkovtsov, Dmitriy Iassenev
//	Description : Level graph
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "level_graph.h"

class CLevelGraphEditor:public ILevelGraph
{
public:
	CLevelGraphEditor();
	virtual			~CLevelGraphEditor();
	bool build();
	bool empty()const;
	void clear();
	bool save_temp();
protected:
	CHeader m_RealHeader;
	xr_vector<CVertex> m_RealNodes;
};