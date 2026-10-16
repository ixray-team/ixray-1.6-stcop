#include "stdafx.h"
#include "IGame_Level.h"
#include "IGame_Persistent.h"
#include "IGame_ObjectPool.h"
#include "xr_object.h"

bool g_MtPrefetch = true;

namespace
{
	thread_local xr_vector<std::function<void()>>* g_DeferredPrimaryWork = nullptr;
	thread_local bool g_PrefetchProfiled = false;

	struct PrefetchItem
	{
		CObject*								Object = nullptr;
		shared_str								Section;
		xr_vector<std::function<void()>>		PrimaryWork;
	};

	void LoadPrefetchItem(PrefetchItem& Item)
	{
		PROF_EVENT("IGame_ObjectPool::prefetch: CObject::Load");
		Item.Object->Load(Item.Section.c_str());
	}
}

IGame_ObjectPool::IGame_ObjectPool(void)
{
}

void IGame_ObjectPool::RunOnPrimaryThread(std::function<void()>&& Action)
{
	if (g_DeferredPrimaryWork != nullptr)
	{
		g_DeferredPrimaryWork->push_back(std::move(Action));
		return;
	}
	Action();
}

bool IGame_ObjectPool::IsParallelLoad()
{
	return g_DeferredPrimaryWork != nullptr;
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
	const xr_string section = g_pGamePersistent->GetPrefetchSection("prefetch_objects_");
	CInifile::Sect const& sect = pSettings->r_section(section.c_str());

	xr_vector<PrefetchItem> Items;
	Items.reserve(sect.Data.size());
	for (const CInifile::Item& item : sect.Data)
	{
		if (!pSettings->section_exist(item.first.c_str()))
		{
			Msg("~ Section [%s] not found in prefetch", item.first.c_str());
			continue;
		}

		CLASS_ID CLS = pSettings->r_clsid(item.first.c_str(), "class");
		PrefetchItem& Item = Items.emplace_back();
		Item.Section = item.first;
		PROF_EVENT("IGame_ObjectPool::prefetch: NEW_INSTANCE");
		Item.Object = (CObject*)NEW_INSTANCE(CLS);
	}

	if (g_MtPrefetch)
	{
		xr_parallel_for(size_t(0), Items.size(), [&Items](size_t Index)
		{
			if (!g_PrefetchProfiled && !g_AppInfo.IsPrimaryThread())
			{
				g_PrefetchProfiled = true;
				PROF_REGISTER_THREAD("Prefetch worker");
			}

			xrTaskBlockingScope Blocking;
			PrefetchItem& Item = Items[Index];
			g_DeferredPrimaryWork = &Item.PrimaryWork;
			LoadPrefetchItem(Item);
			g_DeferredPrimaryWork = nullptr;
		});
	}
	else
	{
		for (PrefetchItem& Item : Items)
		{
			LoadPrefetchItem(Item);
		}
	}

	for (PrefetchItem& Item : Items)
	{
		for (std::function<void()>& Action : Item.PrimaryWork)
		{
			Action();
		}
		VERIFY2(Item.Object->cNameSect().c_str(), Item.Section.c_str());
		m_PrefetchObjects.push_back(Item.Object);
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
	PROF_EVENT(__FUNCTION__);
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
