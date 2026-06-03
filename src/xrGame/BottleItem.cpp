///////////////////////////////////////////////////////////////
// BottleItem.cpp
// BottleItem - бутылка с напитком, которую можно разбить
///////////////////////////////////////////////////////////////

#include "stdafx.h"
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

	if (pSettings->line_exist(section, "break_particles"))
	{
		m_sBreakParticles = pSettings->r_string(section, "break_particles");
	}

	if (pSettings->line_exist(section, "break_sound"))
	{
		sndBreaking.create(pSettings->r_string(section, "break_sound"), st_Effect, sg_SourceType);
	}
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
	//играем звук
	sndBreaking.play_at_pos(0, Position(), false);

	//отыграть партиклы разбивания
	if(*m_sBreakParticles)
	{
		//показываем эффекты
		CParticlesObject* pStaticPG = Particles::Details::Create(*m_sBreakParticles,true).get(); 
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