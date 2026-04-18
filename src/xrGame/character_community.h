//////////////////////////////////////////////////////////////////////////
// character_community.h:	структура представления группировки
//							
//////////////////////////////////////////////////////////////////////////

#pragma once

#include "ini_id_loader.h"
#include "ini_table_loader.h"

struct COMMUNITY_DATA
{
	COMMUNITY_DATA (s32, shared_str, const char*);

	shared_str		id;
	s32	index;
	u8 team;
};


class CHARACTER_COMMUNITY;

class CHARACTER_COMMUNITY: 
	public CIni_IdToIndex<1, COMMUNITY_DATA, shared_str, s32, CHARACTER_COMMUNITY>
{
private:
	typedef CIni_IdToIndex<1, COMMUNITY_DATA, shared_str, s32, CHARACTER_COMMUNITY> inherited;
	friend inherited;

public:
	CHARACTER_COMMUNITY			();
	~CHARACTER_COMMUNITY		();

	void						set				(shared_str);		
	void						set				(s32 index) {m_current_index = index;};

	shared_str		id				() const;
	s32	index			() const	{return m_current_index;};
	u8							team			() const;

private:
	s32	m_current_index;

	static	void				InitIdToIndex	();

public:
	//отношение между группировками
	static s32	relation			(s32 from, s32 to);
	s32			relation			(s32 to);
	
	static void					set_relation		(s32 from, s32 to, s32 goodwill);

	static float				sympathy			(s32);
	
	static void					DeleteIdToIndexData	();
private:
	typedef CIni_Table<s32, CHARACTER_COMMUNITY> GOODWILL_TABLE;
	friend GOODWILL_TABLE;
	static GOODWILL_TABLE m_relation_table;

	//таблица коэффициентов "сочуствия" между участниками группировки
	typedef CIni_Table<float, CHARACTER_COMMUNITY> SYMPATHY_TABLE;
	friend SYMPATHY_TABLE;
	static SYMPATHY_TABLE m_sympathy_table;
};