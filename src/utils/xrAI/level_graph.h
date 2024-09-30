////////////////////////////////////////////////////////////////////////////
//	Module 		: level_graph.h
//	Created 	: 02.10.2001
//  Modified 	: 11.11.2003
//	Author		: Oles Shihkovtsov, Dmitriy Iassenev
//	Description : Level graph
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../xrEngine/xrLevel.h"
#include "../../xrEngine/AI/level_graph.h"

class CLevelGraph : public ILevelGraph
{
public:
				CLevelGraph	(LPCSTR file_name);
	virtual		~CLevelGraph	();
	IReader*	m_reader;
};
