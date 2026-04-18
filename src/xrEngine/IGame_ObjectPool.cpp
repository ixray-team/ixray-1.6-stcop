#include "stdafx.h"
#include "IGame_Level.h"
#include "IGame_Persistent.h"
#include "IGame_ObjectPool.h"
#include "xr_object.h"

IGame_ObjectPool::IGame_ObjectPool(void)
{
}

IGame_ObjectPool::~IGame_ObjectPool(void)
{
	clear();
	R_ASSERT			(m_PrefetchObjects.empty());
}

void IGame_ObjectPool::prefetch()
{
	R_ASSERT(m_PrefetchObjects.empty());

	// prefetch objects
	xr_string section = "prefetch_objects_";
	section += g_pGamePersistent->m_game_params.m_game_type;

	// Workaround for SoC sections
	if (!pSettings->section_exist(section.c_str()))
	{
		if (section.compare("prefetch_objects_dm"))
			section = "prefetch_objects_deathmatch";
		if (section.compare("prefetch_objects_tdm"))
			section = "prefetch_objects_teamdeathmatch";
		if (section.compare("prefetch_objects_ah"))
			section = "prefetch_objects_artefacthunt";
	}

	CInifile::Sect const& sect = pSettings->r_section(section.c_str());
	
	for (CInifile::SectCIt I = sect.Data.begin(); I != sect.Data.end(); I++) 
	{
		const CInifile::Item& item = *I;
		CLASS_ID CLS = pSettings->r_clsid(item.first.c_str(), "class");
		CObject* pObject = (CObject*)NEW_INSTANCE(CLS);
		pObject->Load(item.first.c_str());
		VERIFY2(pObject->cNameSect().c_str(), item.first.c_str());
		m_PrefetchObjects.push_back(pObject);
	}
}

void IGame_ObjectPool::clear()
{
	// Clear POOL
	ObjectVecIt it			= m_PrefetchObjects.begin();
	ObjectVecIt itE			= m_PrefetchObjects.end();
	for (; it!=itE; it++)	
		xr_delete			(*it);
	m_PrefetchObjects.clear	(); 
}

CObject* IGame_ObjectPool::create(const char* name)
{
	CLASS_ID CLS		=	pSettings->r_clsid		(name,"class");
	CObject* O			=	(CObject*) NEW_INSTANCE	(CLS);
	O->cNameSect_set	(name);
	O->Load				(name);
	return				O;
}

void IGame_ObjectPool::destroy(CObject* O)
{
	xr_delete(O);
}