//////////////////////////////////////////////////////////////////////
// inventory_owner_info.h:	для работы с сюжетной информацией
//
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "InventoryOwner.h"
#include "GameObject.h"
#include "xrMessages.h"
#include "ai_space.h"
#include "ai_debug.h"
#include "alife_simulator.h"
#include "alife_registry_container.h"
#include "script_game_object.h"
#include "Level.h"
#include "InfoPortion.h"
#include "alife_registry_wrappers.h"
#include "../xrScripts/script_callback_ex.h"
#include "game_object_space.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "../xrScripts/script_engine.h"
#include "../xrScripts/script_thread.h"

// 0 - off, 1 - log info portion changes, 2 - also log the Lua stack
int g_info_portion_log = 0;

namespace
{
	thread_local const char* InfoChangeSource = "direct";

	class CInfoChangeSourceScope
	{
		const char* PrevSource;

	public:
		explicit CInfoChangeSourceScope(const char* Source) : PrevSource(InfoChangeSource)
		{
			InfoChangeSource = Source;
		}

		~CInfoChangeSourceScope()
		{
			InfoChangeSource = PrevSource;
		}
	};

	void LogLuaStack()
	{
		if (!g_pScriptEngine)
		{
			return;
		}

		CScriptThread* Thread = g_pScriptEngine->current_thread();
		lua_State* L = Thread ? Thread->lua() : g_pScriptEngine->lua();
		if (!L)
		{
			return;
		}

		lua_Debug DebugInfo;
		int Level = 0;
		for (; lua_getstack(L, Level, &DebugInfo); ++Level)
		{
			lua_getinfo(L, "Sln", &DebugInfo);
			Msg("    %2d : %s(%d) : %s", Level, DebugInfo.short_src, DebugInfo.currentline, DebugInfo.name ? DebugInfo.name : "?");
		}

		if (Level == 0)
		{
			Msg("    (no Lua stack)");
		}
	}
}

void CInventoryOwner::LogInfoChange(u16 OwnerId, const shared_str& InfoId, bool Added, const char* Source)
{
	if (g_info_portion_log <= 0)
	{
		return;
	}

	Msg("~ [info] %s [%s] owner=[%u] source=[%s] frame=[%u]", Added ? "give" : "disable", InfoId.c_str(), OwnerId,
		Source ? Source : InfoChangeSource, Device.dwFrame);

	if (g_info_portion_log >= 2)
	{
		LogLuaStack();
	}
}

void  CInventoryOwner::OnEvent(NET_Packet& P, u16 type)
{
	switch (type)
	{
	case GE_MONEY:
	{
		u32 amount = P.r_u32();
		if (InfinitiveMoney())
		{
			m_money = std::max(m_money, amount);
		}
		else
		{
			m_money = amount;
		}
	}
	break;
	case GE_INFO_TRANSFER:
	{
		u16 id = 0;
		shared_str info_id;
		u8 add_info = 0;

		P.r_u16(id);				//отправитель
		P.r_stringZ(info_id);		//номер полученной информации
		P.r_u8(add_info);			//добавление или убирание информации

		CInfoChangeSourceScope SourceScope("net event");
		if (add_info)
		{
			OnReceiveInfo(info_id);
		}
		else
		{
			OnDisableInfo(info_id);
		}
	}
	break;
	}
}

bool CInventoryOwner::OnReceiveInfo(shared_str info_id) const
{
	VERIFY(info_id.size());

	//добавить запись в реестр
	KNOWN_INFO_VECTOR& known_info = m_known_info_registry->registry().objects();
	auto it = std::find_if(known_info.begin(), known_info.end(), CFindByIDPred(info_id));

	if (known_info.end() == it)
	{
		known_info.emplace_back(info_id, Level().GetGameTime());
		LogInfoChange(object_id(), info_id, true, nullptr);
	}
	else
	{
		return false;
	}

#ifdef DEBUG
	if (psAI_Flags.test(aiInfoPortion))
	{
		Msg("[%s] Received Info [%s]", Name(), *info_id);
}
#endif

	//Запустить скриптовый callback
	const CGameObject* pThisGameObject = smart_cast<const CGameObject*>(this);
	VERIFY(pThisGameObject);

	CInfoPortion info_portion;
	info_portion.Load(info_id);

	//запустить скриптовые функции
	info_portion.RunScriptActions(pThisGameObject);

	//выкинуть те info portions которые стали неактуальными
	for (const shared_str& name : info_portion.DisableInfos())
	{
		TransferInfo(name, false);
	}

	return true;
}
#ifdef DEBUG
void CInventoryOwner::DumpInfo() const
{
	KNOWN_INFO_VECTOR& known_info = m_known_info_registry->registry().objects();

	Msg("------------------------------------------");
	Msg("Start KnownInfo dump for [%s]", Name());
	auto it = known_info.begin();
	for (int i = 0; it != known_info.end(); ++it, ++i) 
	{
		Msg("known info[%d]:%s", i, (*it).info_id.c_str());
	}

	Msg("------------------------------------------");

}
#endif

void CInventoryOwner::OnDisableInfo(shared_str info_id) const
{
	VERIFY(info_id.size());
	//удалить запись из реестра

#ifdef DEBUG
	if (psAI_Flags.test(aiInfoPortion))
		Msg("[%s] Disabled Info [%s]", Name(), info_id.c_str());
#endif

	KNOWN_INFO_VECTOR& known_info = m_known_info_registry->registry().objects();

	auto it = std::find_if(known_info.begin(), known_info.end(), CFindByIDPred(info_id));
	if (known_info.end() == it)
	{
		return;
	}

	known_info.erase(it);
	LogInfoChange(object_id(), info_id, false, nullptr);
}

void CInventoryOwner::TransferInfo(shared_str info_id, bool add_info) const
{
	VERIFY(info_id.size());

	const CObject* pThisObject = smart_cast<const CObject*>(this);
	VERIFY(pThisObject);

	//отправляем от нашему PDA пакет информации с номером
	NET_Packet		P;
	CGameObject::u_EventGen(P, GE_INFO_TRANSFER, pThisObject->ID());
	P.w_u16(pThisObject->ID());					//отправитель
	P.w_stringZ(info_id);							//сообщение
	P.w_u8(add_info ? 1 : 0);							//добавить/удалить информацию
	CGameObject::u_EventSend(P);

	CInfoPortion info_portion;
	info_portion.Load(info_id);
	{
		if (add_info)
		{
			OnReceiveInfo(info_id);
		}
		else
		{
			OnDisableInfo(info_id);
		}
	}
}

bool CInventoryOwner::HasInfo(shared_str info_id) const
{
	VERIFY(info_id.size());
	const KNOWN_INFO_VECTOR* known_info = m_known_info_registry->registry().objects_ptr();
	if (!known_info)
	{
		return false;
	}

	if (std::find_if(known_info->begin(), known_info->end(), CFindByIDPred(info_id)) == known_info->end())
	{
		return false;
	}

	return true;
}

bool CInventoryOwner::GetInfo(shared_str info_id, INFO_DATA& info_data) const
{
	VERIFY(info_id.size());

	const KNOWN_INFO_VECTOR* known_info = m_known_info_registry->registry().objects_ptr();
	if (!known_info)
	{
		return false;
	}

	auto it = std::find_if(known_info->cbegin(), known_info->cend(), CFindByIDPred(info_id));
	if (known_info->cend() == it)
	{
		return false;
	}

	info_data = *it;
	return true;
}
