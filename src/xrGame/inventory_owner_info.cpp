//////////////////////////////////////////////////////////////////////
// inventory_owner_info.h:	для работы с сюжетной информацией
//
//////////////////////////////////////////////////////////////////////

#include <discord_gamesdk/cpp/types.h>

#include "stdafx.h"
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
		ALife::_OBJECT_ID id = 0;
		shared_str info_id;
		u8 add_info = 0;

		P >> id;				//отправитель
		P.r_stringZ(info_id);		//номер полученной информации
		P.r_u8(add_info);			//добавление или убирание информации

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
	auto& known_info = m_known_info_registry->registry().objects();
	if (known_info.HasInfo(info_id))
	{
		return false;
	}
	known_info.AddInfo(info_id, Level().GetGameTime());

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
	auto known_info = m_known_info_registry->registry().objects();

	Msg("------------------------------------------");
	Msg("Start KnownInfo dump for [%s]", Name());
	auto it = known_info.Data.begin();
	for (int i = 0; it != known_info.Data.end(); ++it, ++i) 
	{
		Msg("known info[%d]:%s", i, it->info_id.c_str());
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

	auto& known_info = m_known_info_registry->registry().objects();
	known_info.RemoveInfo(info_id);
}

void CInventoryOwner::TransferInfo(shared_str info_id, bool add_info) const
{
	VERIFY(info_id.size());

	const CObject* pThisObject = smart_cast<const CObject*>(this);
	VERIFY(pThisObject);

	//отправляем от нашему PDA пакет информации с номером
	NET_Packet		P;
	CGameObject::u_EventGen(P, GE_INFO_TRANSFER, pThisObject->ID());
	P << pThisObject->ID();					//отправитель
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
	auto known_info = m_known_info_registry->registry().objects_ptr();
	if (!known_info)
	{
		return false;
	}
	return known_info->HasInfo(info_id);
}

bool CInventoryOwner::GetInfo(shared_str info_id, INFO_DATA& info_data) const
{
	VERIFY(info_id.size());

	auto known_info = m_known_info_registry->registry().objects_ptr();
	if (!known_info || !known_info->HasInfo(info_id))
	{
		return false;
	}
	info_data = known_info->GetInfo(info_id);
	return true;
}
