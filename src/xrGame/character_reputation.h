//////////////////////////////////////////////////////////////////////////
// character_reputation.h:		структура представления репутаций и 
//								отношений между ними		
//////////////////////////////////////////////////////////////////////////

#pragma once

#include "ini_id_loader.h"
#include "ini_table_loader.h"

struct REPUTATION_DATA
{
	REPUTATION_DATA (int, shared_str, const char*);

	shared_str					id;
	int							index;
	s32	threshold;
};


class CHARACTER_REPUTATION;

class CHARACTER_REPUTATION: 
	public CIni_IdToIndex<1, REPUTATION_DATA, shared_str, int, CHARACTER_REPUTATION>
{
private:
	typedef CIni_IdToIndex<1, REPUTATION_DATA, shared_str, int, CHARACTER_REPUTATION> inherited;
	friend inherited;

public:
	CHARACTER_REPUTATION		():m_current_value(-type_max(s32)){};
	~CHARACTER_REPUTATION		(){};

	void						set				(s32);

	shared_str					id				() const;
	int							index			() const	{return m_current_index;};
	s32	value			() const	{return m_current_value;};

	static int					ValueToIndex    (s32);

private:
	s32	m_current_value;
	int							m_current_index;

	static	void				InitIdToIndex	();
public:
	//отношение между репутациями
	static s32	relation			(int from, int to);
	s32			relation			(int to);

	static void					DeleteIdToIndexData	();

private:
	typedef CIni_Table<s32, CHARACTER_REPUTATION> GOODWILL_TABLE;
	friend GOODWILL_TABLE;
	static GOODWILL_TABLE m_relation_table;
};