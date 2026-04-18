//////////////////////////////////////////////////////////////////////////
// character_rank.h:	структура представления рангов и отношений между 
//						ними		
//////////////////////////////////////////////////////////////////////////

#pragma once

#include "ini_id_loader.h"
#include "ini_table_loader.h"

struct RANK_DATA
{
	RANK_DATA (int, shared_str, const char*);

	shared_str				id;
	int						index;
	s32	threshold;
};


class CHARACTER_RANK;

typedef CIni_IdToIndex<1, RANK_DATA, shared_str, int, CHARACTER_RANK> CHARACTER_RANK_base;

class CHARACTER_RANK: public CHARACTER_RANK_base
{
private:
	typedef CHARACTER_RANK_base inherited;
	friend inherited;

public:
	CHARACTER_RANK			():m_current_value(-type_max(s32)){};
	~CHARACTER_RANK			(){};

	void						set				(s32);

	shared_str					id				() const;
	int							index			() const	{return m_current_index;};
	s32		value			() const	{return m_current_value;};

	static int					ValueToIndex    (s32);

private:
	s32		m_current_value;
	int							m_current_index;

	static	void				InitIdToIndex	();
public:
	//отношение между рангами
	static s32	relation			(int from, int to);
	s32			relation			(int to);

	static s32	rank_kill_points	(int);

	static void					DeleteIdToIndexData	();

private:
	typedef CIni_Table<s32, CHARACTER_RANK> GOODWILL_TABLE;
	friend GOODWILL_TABLE;
	static GOODWILL_TABLE m_relation_table;

	//очки рейтинга которые прибавляются за убийство персонажа с определенным рангом
	typedef CIni_Table<s32, CHARACTER_RANK> RANK_KILL_TABLE;
	friend RANK_KILL_TABLE;
	static RANK_KILL_TABLE m_rank_kill_table;
};