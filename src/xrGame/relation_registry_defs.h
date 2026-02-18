//////////////////////////////////////////////////////////////////////////
// relation_registry_defs.h:	реестр для хранения данных об отношении персонажа к 
//								другим персонажам
//////////////////////////////////////////////////////////////////////////

#pragma once

#include "object_interfaces.h"


//структура, описывающая отношение одного персонажа к другому или к группировке
struct SRelation
{
	SRelation();
	~SRelation();
	s32		Goodwill		() const							{return m_iGoodwill;};
	void					SetGoodwill		(s32 new_goodwill)	{m_iGoodwill = new_goodwill;};
private:
	//благосклонность
	s32 m_iGoodwill;
};

using PERSONAL_RELATION_MAP = xr_map<ALife::_OBJECT_ID, SRelation>;
using PERSONAL_RELATION_MAP_IT = PERSONAL_RELATION_MAP::iterator;

using COMMUNITY_RELATION_MAP = xr_map<s32, SRelation>;
using COMMUNITY_RELATION_MAP_IT = COMMUNITY_RELATION_MAP::iterator;


//структура, существует для каждого персонажа в игре
struct RELATION_DATA : public IPureSerializeObject<IReader,IWriter>
{
	virtual void clear();

	virtual void load (IReader&);
	virtual void save (IWriter&);

	//личные отношения
	PERSONAL_RELATION_MAP personal; 
	//отношения с группировками
	COMMUNITY_RELATION_MAP communities;
};
