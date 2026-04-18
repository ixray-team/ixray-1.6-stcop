//////////////////////////////////////////////////////////////////////////
// character_community.cpp:		структура представления группировки
//							
//////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "character_community.h"

//////////////////////////////////////////////////////////////////////////
COMMUNITY_DATA::COMMUNITY_DATA (s32 idx, shared_str idn, const char* team_str)
{
	index = idx;
	id = idn;
	team = (u8)atoi(team_str);
}

//////////////////////////////////////////////////////////////////////////
CHARACTER_COMMUNITY::GOODWILL_TABLE CHARACTER_COMMUNITY::m_relation_table;
CHARACTER_COMMUNITY::SYMPATHY_TABLE CHARACTER_COMMUNITY::m_sympathy_table;

//////////////////////////////////////////////////////////////////////////
CHARACTER_COMMUNITY::CHARACTER_COMMUNITY	()
{
	m_current_index = s32(-1);
}
CHARACTER_COMMUNITY::~CHARACTER_COMMUNITY	()
{
}


void  CHARACTER_COMMUNITY::set	(shared_str id)
{
	m_current_index	 = IdToIndex(id);

}

shared_str		 CHARACTER_COMMUNITY::id			() const
{
	return IndexToId(m_current_index);
}

u8							 CHARACTER_COMMUNITY::team			() const
{
	return (*m_pItemDataVector)[m_current_index].team;
}


void CHARACTER_COMMUNITY::InitIdToIndex	()
{
	section_name = "game_relations";
	line_name = "communities";

	m_relation_table.set_table_params("communities_relations");
	m_sympathy_table.set_table_params("communities_sympathy", 1);
}


s32 CHARACTER_COMMUNITY::relation		(s32 to)
{
	return relation(m_current_index, to);
}

s32  CHARACTER_COMMUNITY::relation		(s32 from, s32 to)
{
	VERIFY(from >= 0 && from <(int)m_relation_table.table().size());
	VERIFY(to >= 0 && to <(int)m_relation_table.table().size());
	
	return m_relation_table.table()[from][to];
}

void  CHARACTER_COMMUNITY::set_relation			(s32 from, s32 to, s32 goodwill)
{
	VERIFY(from >= 0 && from <(int)m_relation_table.table().size());
	VERIFY(to >= 0 && to <(int)m_relation_table.table().size());
	VERIFY(goodwill != -type_max(s32));

	m_relation_table.table()[from][to] = goodwill;
}

float  CHARACTER_COMMUNITY::sympathy			(s32 comm)
{
	VERIFY(comm >= 0 && comm <(int)m_sympathy_table.table().size());
	return m_sympathy_table.table()[comm][0];
}

void CHARACTER_COMMUNITY::DeleteIdToIndexData	()
{
	m_relation_table.clear();
	m_sympathy_table.clear();
	inherited::DeleteIdToIndexData();
}