#include "StdAfx.h"
#include "RemoteSmartZone.h"
#include "Level.h"

CRemoteSmartZone::CRemoteSmartZone() : CSE_ALifeSmartZone("smart_terrain")
{
}

bool CRemoteSmartZone::Initialize(CSE_ALifeSmartZone* remote, CLevel* serverLevel)
{
	if (remote == nullptr || serverLevel == nullptr)
		return false;

	NET_Packet packet;
	remote->Spawn_Write(packet, true);
	if (!Spawn_Read(packet))
		return false;

	Remote = remote;
	ServerLevel = serverLevel;
	m_alife_simulator = remote->m_alife_simulator;
	return true;
}

void CRemoteSmartZone::update()
{
	if (Remote != nullptr && ServerLevel != nullptr)
		ServerLevel->RunInContext(xr_make_delegate(Remote, &CSE_ALifeSmartZone::update));
}

void CRemoteSmartZone::smart_touch(CSE_ALifeMonsterAbstract* monster)
{
	struct STouchCall
	{
		CSE_ALifeSmartZone* Object;
		CSE_ALifeMonsterAbstract* Monster;
		void Invoke() { Object->smart_touch(Monster); }
	};
	if (Remote != nullptr && ServerLevel != nullptr)
	{
		STouchCall call{ Remote, monster };
		ServerLevel->RunInContext(xr_make_delegate(&call, &STouchCall::Invoke));
	}
}

bool CRemoteSmartZone::enabled(CSE_ALifeMonsterAbstract* object) const
{
	struct SEnabledCall
	{
		CSE_ALifeSmartZone* Object;
		CSE_ALifeMonsterAbstract* Monster;
		bool Result = false;
		void Invoke() { Result = Object->enabled(Monster); }
	};
	if (Remote == nullptr || ServerLevel == nullptr)
		return false;
	SEnabledCall call{ Remote, object };
	ServerLevel->RunInContext(xr_make_delegate(&call, &SEnabledCall::Invoke));
	return call.Result;
}

float CRemoteSmartZone::suitable(CSE_ALifeMonsterAbstract* object) const
{
	struct SSuitableCall
	{
		CSE_ALifeSmartZone* Object;
		CSE_ALifeMonsterAbstract* Monster;
		float Result = 0.f;
		void Invoke() { Result = Object->suitable(Monster); }
	};
	if (Remote == nullptr || ServerLevel == nullptr)
		return 0.f;
	SSuitableCall call{ Remote, object };
	ServerLevel->RunInContext(xr_make_delegate(&call, &SSuitableCall::Invoke));
	return call.Result;
}

void CRemoteSmartZone::register_npc(CSE_ALifeMonsterAbstract* object)
{
	struct SNpcCall
	{
		CSE_ALifeSmartZone* Object;
		CSE_ALifeMonsterAbstract* Monster;
		bool Register;
		void Invoke()
		{
			if (Register)
				Object->register_npc(Monster);
			else
				Object->unregister_npc(Monster);
		}
	};
	if (Remote != nullptr && ServerLevel != nullptr)
	{
		SNpcCall call{ Remote, object, true };
		ServerLevel->RunInContext(xr_make_delegate(&call, &SNpcCall::Invoke));
	}
}

void CRemoteSmartZone::unregister_npc(CSE_ALifeMonsterAbstract* object)
{
	struct SNpcCall
	{
		CSE_ALifeSmartZone* Object;
		CSE_ALifeMonsterAbstract* Monster;
		void Invoke() { Object->unregister_npc(Monster); }
	};
	if (Remote != nullptr && ServerLevel != nullptr)
	{
		SNpcCall call{ Remote, object };
		ServerLevel->RunInContext(xr_make_delegate(&call, &SNpcCall::Invoke));
	}
}

CALifeSmartTerrainTask* CRemoteSmartZone::task(CSE_ALifeMonsterAbstract* object)
{
	struct STaskCall
	{
		CSE_ALifeSmartZone* Object;
		CSE_ALifeMonsterAbstract* Monster;
		CALifeSmartTerrainTask* Result = nullptr;
		void Invoke() { Result = Object->task(Monster); }
	};
	if (Remote == nullptr || ServerLevel == nullptr)
		return nullptr;
	STaskCall call{ Remote, object };
	ServerLevel->RunInContext(xr_make_delegate(&call, &STaskCall::Invoke));
	return call.Result;
}

float CRemoteSmartZone::detect_probability()
{
	struct SProbabilityCall
	{
		CSE_ALifeSmartZone* Object;
		float Result = 0.f;
		void Invoke() { Result = Object->detect_probability(); }
	};
	if (Remote == nullptr || ServerLevel == nullptr)
		return 0.f;
	SProbabilityCall call{ Remote };
	ServerLevel->RunInContext(xr_make_delegate(&call, &SProbabilityCall::Invoke));
	return call.Result;
}
