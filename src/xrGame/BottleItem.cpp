///////////////////////////////////////////////////////////////
// BottleItem.cpp
// BottleItem - бутылка с напитком, которую можно разбить
///////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "BottleItem.h"

#include "ParticlesObject.h"
#include "xrMessages.h"

static constexpr float BREAK_POWER = 5.0f;

CBottleItem::~CBottleItem() 
{
	sndBreaking.destroy();
}

void CBottleItem::Load(const char* section)
{
	inherited::Load(section);
	CurrentBottleDesc = &SBottleItemDesc::Registry::Get(section);
}

void CBottleItem::OnEvent(NET_Packet& P, u16 type) 
{
	inherited::OnEvent(P,type);

	switch (type) 
	{
		case GE_GRENADE_EXPLODE:
		{
			BreakToPieces();
			break;
		}
	}
}

void CBottleItem::BreakToPieces()
{
	const SBottleItemDesc& Desc = BottleDesc();

	//играем звук
	if (Desc.BreakSound.size())
	{
		if (!sndBreaking.handle())
		{
			sndBreaking.create(Desc.BreakSound.c_str(), st_Effect, sg_SourceType);
		}

		sndBreaking.play_at_pos(0, Position(), false);
	}

	//отыграть партиклы разбивания
	if(*Desc.BreakParticles)
	{
		//показываем эффекты
		CParticlesObject* pStaticPG = Particles::Details::Create(*Desc.BreakParticles,true).get(); 
		pStaticPG->play_at_pos(Position());
	}

	//ликвидировать сам объект 
	if (Local())
	{
		DestroyObject();
	}
}

void CBottleItem::Hit(SHit* pHDS)
{
	inherited::Hit(pHDS);
	
	if(pHDS->damage()>BREAK_POWER)
	{
		//Generate Expode event
		if (Local()) 
		{
			NET_Packet P;
			u_EventGen(P,GE_GRENADE_EXPLODE,ID());	
			u_EventSend(P);
		};
	}
}

using namespace luabind;

#pragma optimize("s",on)
void CBottleItem::script_register(lua_State* L)
{
	module(L)
		[
			class_<CBottleItem, CGameObject>("CBottleItem")
				.def(constructor<>())
				.def("BreakToPieces", &CBottleItem::BreakToPieces)
		];
}