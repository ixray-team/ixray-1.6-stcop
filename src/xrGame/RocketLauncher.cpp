//////////////////////////////////////////////////////////////////////
// RocketLauncher.cpp:	интерфейс для семейства объектов 
//						стреляющих гранатами и ракетами
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "RocketLauncher.h"
#include "CustomRocket.h"
#include "xrServer_Objects_ALife_Items.h"
#include "Level.h"
#include "ai_object_location.h"
#include "SaveObjectHelpers.h"
#include "../xrEngine/IGame_Persistent.h"

void CRocketLauncher::Load(const char* section)
{
	m_fLaunchSpeed = pSettings->r_float(section, "launch_speed");
}

void CRocketLauncher::SpawnRocket(const shared_str& rocket_section, CGameObject* parent_rocket_launcher)
{
	if (OnClient())
	{
		return;
	}

	CSE_Abstract* D = F_entity_Create(rocket_section.c_str());
	R_ASSERT(D);
	CSE_Temporary* l_tpTemporary = smart_cast<CSE_Temporary*>(D);
	R_ASSERT(l_tpTemporary);
	l_tpTemporary->m_tNodeID = (g_dedicated_server) ? u32(-1) : parent_rocket_launcher->ai_location().level_vertex_id();
	D->s_name = rocket_section;
	D->set_name_replace("");

	D->s_RP = 0xff;
	D->ID = ALife::INVALID_OBJECT_ID;
	D->ID_Parent = parent_rocket_launcher->ID();
	D->ID_Phantom = ALife::INVALID_OBJECT_ID;
	D->s_flags.assign(M_SPAWN_OBJECT_LOCAL);
	D->RespawnTime = 0;

	NET_Packet P;
	if (EngineExternal()[EEngineExternalSystem::AdvancedSerialization])
	{
		SaveObjectNetPacketHelper::PrepareLocalSpawnPacket(P, *D);
	}
	else
	{
		D->Spawn_Write(P, true);
	}
	Level().Send(P, net_flags(true));
	F_entity_Destroy(D);
}

void CRocketLauncher::AttachRocket(ALife::_OBJECT_ID rocket_id, CGameObject* parent_rocket_launcher)
{
	CObject* finded_object = Level().Objects.net_Find(rocket_id);
	CCustomRocket* pRocket = finded_object != nullptr ? finded_object->cast_custom_rocket() : nullptr;

	if (pRocket == nullptr)
	{
		R_ASSERT(pRocket);
		return;
	}

	pRocket->m_pOwner = parent_rocket_launcher->H_Root() != nullptr ? parent_rocket_launcher->H_Root()->cast_game_object() : nullptr;
	VERIFY(pRocket->m_pOwner);

	pRocket->H_SetParent(parent_rocket_launcher);
	m_rockets.push_back(pRocket);
}

void CRocketLauncher::DetachRocket(ALife::_OBJECT_ID rocket_id, bool bLaunch)
{
	CObject* finded_object = Level().Objects.net_Find(rocket_id);
	CCustomRocket* pRocket = finded_object != nullptr ? finded_object->cast_custom_rocket() : nullptr;

	if (pRocket == nullptr && OnClient())
	{
		return;
	}

	VERIFY(pRocket);
	ROCKETIT It = std::ranges::find(m_rockets, pRocket);
	ROCKETIT It_l = std::ranges::find(m_launched_rockets, pRocket);

	if (OnServer())
	{
		VERIFY((It != m_rockets.end()) || (It_l != m_launched_rockets.end()));
	};

	if (It != m_rockets.end())
	{
		(*It)->m_bLaunched = bLaunch;
		(*It)->H_SetParent(nullptr);
		m_rockets.erase(It);
	};

	if (It_l != m_launched_rockets.end())
	{
		(*It_l)->m_bLaunched = bLaunch;
		(*It_l)->H_SetParent(nullptr);
		m_launched_rockets.erase(It_l);
	}
}

void CRocketLauncher::LaunchRocket(const Fmatrix& xform, const Fvector& vel, const Fvector& angular_vel)
{
	VERIFY2(_valid(xform), "CRocketLauncher::LaunchRocket. Invalid xform argument!");

	if (CCustomRocket* Rocket = getCurrentRocket())
	{
		Rocket->SetLaunchParams(xform, vel, angular_vel);
		m_launched_rockets.push_back(Rocket);
	}
}

CCustomRocket* CRocketLauncher::getCurrentRocket()
{
	if (!m_rockets.empty())
	{
		return m_rockets.back();
	}

	return nullptr;
}

void CRocketLauncher::dropCurrentRocket()
{
	m_rockets.pop_back();
}

u32 CRocketLauncher::getRocketCount()
{
	return (u32)m_rockets.size();
}
