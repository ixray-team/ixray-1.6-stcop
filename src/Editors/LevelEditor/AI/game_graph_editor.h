////////////////////////////////////////////////////////////////////////////
//	Module 		: game_graph.h
//	Created 	: 18.02.2003
//  Modified 	: 13.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Game graph class
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../xrGame/game_graph.h"

class CGameGraphEditor:
	public IGameGraph
{
public:
	CGameGraphEditor();
	virtual ~CGameGraphEditor();
	virtual	 void set_current_level(u32 level_id);
	void realloc(const CHeader&new_header);
	void clear();
	bool empty()const;
	void set_cross_table(IGameLevelCrossTable*cross_table);
	IC const CVertex* vertex(u32 vertex_id) const	{ return m_nodes + vertex_id; }
	IC		 CVertex* vertex(u32 vertex_id)			{ return m_nodes + vertex_id; };
	IC		 CEdge* edge(u32 vertex_id) { return (CEdge*)m_edges + vertex_id; };
	virtual		void					save(IWriter& stream);

};
