////////////////////////////////////////////////////////////////////////////
//	Module 		: eatable_item.cpp
//	Created 	: 24.03.2003
//  Modified 	: 29.01.2004
//	Author		: Yuri Dobronravin
//	Description : Eatable item
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "eatable_item.h"
#include "../../xrCore/net_utils.h"
#include "physic_item.h"
#include "Level.h"
#include "entity_alive.h"
#include "EntityCondition.h"
#include "ActorCondition.h"
#include "InventoryOwner.h"
#include "actor.h"
#include "UIGameCustom.h"
#include "UI/UIInventoryWnd.h"
#include "../xrEngine/string_table.h"

CEatableItem::CEatableItem()
{
	m_fHealthInfluence = 0;
	m_fPowerInfluence = 0;
	m_fSatietyInfluence = 0;
	m_fThirstyInfluence = 0;
	m_fPsyHealthInfluence = 0;
	m_fRadiationInfluence = 0;

	m_iPortionsNum = 1;

	m_physic_item	= 0;

	bProlongedEffect = 0;
	iEffectorBlockingGroup = 0;

	notForQSlot_ = FALSE;
}

CEatableItem::~CEatableItem()
{
}

DLL_Pure *CEatableItem::_construct	()
{
	m_physic_item	= smart_cast<CPhysicItem*>(this);
	return			(inherited::_construct());
}

void CEatableItem::Load(LPCSTR section)
{
	inherited::Load(section);
	
	m_fHealthInfluence			= pSettings->r_float(section, "eat_health");
	m_fPowerInfluence			= pSettings->r_float(section, "eat_power");
	m_fSatietyInfluence			= pSettings->r_float(section, "eat_satiety");
	m_fThirstyInfluence			= pSettings->r_float(section, "eat_thirst");
	m_fPsyHealthInfluence		= pSettings->r_float(section, "eat_psy_health");
	m_fRadiationInfluence		= pSettings->r_float(section, "eat_radiation");
	m_fWoundsHealPerc			= pSettings->r_float(section, "wounds_heal_perc");
	m_falcohol					= READ_IF_EXISTS(pSettings, r_float, section, "eat_alcohol", 0.0f);

	clamp						(m_fWoundsHealPerc, 0.f, 1.f);

	m_iPortionsNum				= READ_IF_EXISTS	(pSettings,r_u32,section, "eat_portions_num", 1);
	m_fMaxPowerUpInfluence		= READ_IF_EXISTS	(pSettings,r_float,section,"eat_max_power",0.0f);
	VERIFY2						(m_iPortionsNum<10000 || m_iPortionsNum == -1, make_string<const char*>("'eat_portions_num' should be < 10000. Wrong section [%s]",section));

	use_sound_line				= READ_IF_EXISTS(pSettings, r_string, section, "use_sound", NULL);

	notForQSlot_				= READ_IF_EXISTS(pSettings, r_bool, section, "not_for_quick_slot", FALSE);


	//iEffectsAffectedStat 1 = Здоровье
	//iEffectsAffectedStat 2 = Кровиеотечен
	//iEffectsAffectedStat 3 = Радиация
	//iEffectsAffectedStat 4 = Пси-здоровье
	//iEffectsAffectedStat 5 = Еда
	//iEffectsAffectedStat 6 = Вода
	//iEffectsAffectedStat 7 = Энергия
	//iEffectsAffectedStat 8 = Опьянение

	bProlongedEffect = !!READ_IF_EXISTS(pSettings, r_bool, section, "use_prolonged_effect", FALSE);

	if (bProlongedEffect){

		iEffectorBlockingGroup	= READ_IF_EXISTS(pSettings, r_u8, section, "effector_blocking_group", 0);

		fItemUseTime			= READ_IF_EXISTS(pSettings, r_float, section, "item_use_time", 0.0f);

		LPCSTR templist			= READ_IF_EXISTS(pSettings, r_string, section, "effects_list", "null");
		if (templist && templist[0])
		{
			string128		effect;
			int				count = _GetItemCount(templist);
			for (int it = 0; it < count; ++it)
			{
				_GetItem(templist, it, effect);
				sEffectList.push_back(effect);
			}
		}

		string128		temp_string128;
		float			tempfloat;
		u8				tempint;
		bool			tempbool = false;
		for (u16 i = 0; i < sEffectList.size(); ++i)
		{

			xr_sprintf(temp_string128, "%s_rate", sEffectList[i].c_str());

			tempfloat = READ_IF_EXISTS(pSettings, r_float, section, temp_string128, 0.0f);
			fEffectsRate.push_back(tempfloat);

			xr_sprintf(temp_string128, "%s_dur", sEffectList[i].c_str());

			tempfloat = READ_IF_EXISTS(pSettings, r_float, section, temp_string128, 0.0f);
			fEffectsDur.push_back(tempfloat);

			xr_sprintf(temp_string128, "%s_affected_stat", sEffectList[i].c_str());

			tempint = READ_IF_EXISTS(pSettings, r_u8, section, temp_string128, 1);
			iEffectsAffectedStat.push_back(tempint);

			xr_sprintf(temp_string128, "%s_is_booster", sEffectList[i].c_str());

			tempbool = !!READ_IF_EXISTS(pSettings, r_bool, section, temp_string128, FALSE);
			BoosterParams tembusterparams;
			tembusterparams.EffectIsBooster = (u8)tempbool;
			VectorBoosterParam.push_back(tembusterparams);

			if (tempbool)
			{

				xr_sprintf(temp_string128, "%s_hit_absorbation_sect", sEffectList[i].c_str());

				LPCSTR tempsection = pSettings->r_string(section, temp_string128);
				CHitImmunity tempimun;

				tempimun.LoadImmunities(tempsection, pSettings);
				VectorBoosterParam[i].BoosterHitImmunities = tempimun;
				VectorBoosterParam[i].HitImmunitySect = tempsection;

				xr_sprintf(temp_string128, "%s_additional_weight", sEffectList[i].c_str());

				tempfloat = READ_IF_EXISTS(pSettings, r_float, section, temp_string128, 0.0f);
				VectorBoosterParam[i].AddWeight = tempfloat;
			}

		}
		
		//for (int i = 0; i < sEffectList.size(); ++i)
		//{
		//	Msg("fEffectsRate %i = %f", i, fEffectsRate[i]);
		//	Msg("fEffectsDur %i = %f", i, fEffectsDur[i]);
		//	Msg("fUseTime %i = %f", i, fItemUseTime);
		//	Msg("iEffectsAffectedStat %i = %i", i, iEffectsAffectedStat[i]);
		//}
		
	}
}

BOOL CEatableItem::net_Spawn				(CSE_Abstract* DC)
{
	if (!inherited::net_Spawn(DC)) return FALSE;

	CSE_ALifeEatableItem* server_eatable_item = smart_cast<CSE_ALifeEatableItem*>(DC);
	R_ASSERT(server_eatable_item);

	m_iPortionsNum = server_eatable_item->numOfPortionsServer_;

	return TRUE;
};

void CEatableItem::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);

	P.w_u16(u16(m_iPortionsNum));
}

bool CEatableItem::Useful() const
{
	if(!inherited::Useful()) return false;

	//проверить не все ли еще съедено
	if(Empty()) return false;

	return true;
}

void CEatableItem::OnH_B_Independent(bool just_before_destroy)
{
	if(!Useful()) 
	{
		object().setVisible(FALSE);
		object().setEnabled(FALSE);
		if (m_physic_item)
			m_physic_item->m_ready_to_destroy	= true;
	}
	inherited::OnH_B_Independent(just_before_destroy);
}

void CEatableItem::save				(NET_Packet &packet)
{
	inherited::save				(packet);
}

void CEatableItem::load				(IReader &packet)
{
	inherited::load				(packet);
}

extern bool debug_effects;
bool CEatableItem::UseBy (CEntityAlive* entity_alive)
{
	CInventoryOwner* IO = smart_cast<CInventoryOwner*>(entity_alive);
	R_ASSERT(IO);
	R_ASSERT(m_pCurrentInventory == IO->m_inventory);
	R_ASSERT(object().H_Parent()->ID() == entity_alive->ID());


	if (bProlongedEffect) // Если параметр задан, то использовать ситстему эффектов. Если нет - Мгновенное примененеие
	{ 

		CActor* ActorEntity = smart_cast<CActor*>(entity_alive);
		if (ActorEntity)
		{
			bool hands_are_blocked = false;
			bool same_blockin_ggroup = false;
			for (u16 i = 0; i < ActorEntity->conditions().Eat_Effects.size(); ++i)
			{
				Eat_Effect effector = ActorEntity->conditions().Eat_Effects[i];

				if (ActorEntity->conditions().fHandsHideTime > Device.fTimeGlobal) // Проверяем не заняты ли руки другим применяемым предметом
				{	
					hands_are_blocked = true;
				}
				if (iEffectorBlockingGroup != 0 && iEffectorBlockingGroup == effector.BlockingGroup) //0 - нету группы блока. Проверяем блокируется ли использование уже действующими эффектами
				{	
					same_blockin_ggroup = true;
				}
			}

			if (!hands_are_blocked && !same_blockin_ggroup) // Если все норм, вешаем эффекты предмета
			{	
				ActorEntity->SetWeaponHideState(INV_STATE_BLOCK_ALL, true);	//Прячем руки
				ActorEntity->conditions().fHandsHideTime = fItemUseTime + Device.fTimeGlobal;

				for (u16 i = 0; i < sEffectList.size(); ++i)
				{
					Eat_Effect eat_effect;
					eat_effect.DurationExpiration = Device.fTimeGlobal + fEffectsDur[i] + fItemUseTime;
					eat_effect.Rate = fEffectsRate[i];
					eat_effect.UseTimeExpiration = Device.fTimeGlobal + fItemUseTime;
					eat_effect.BlockingGroup = iEffectorBlockingGroup; //Так как у эффектов разное время действия, нужно давайть им всем этот параметр
					eat_effect.AffectedStat = iEffectsAffectedStat[i];
					eat_effect.BoosterParam = VectorBoosterParam[i];

					if (eat_effect.BoosterParam.EffectIsBooster)
					{
						ActorEntity->conditions().m_fBoostersAddWeight += eat_effect.BoosterParam.AddWeight;
					}

					if (debug_effects)
					Msg("Setting Up Effect: [%i] Device.fTimeGlobal %f DurationExpiration %f, UseTimeExpiration = %f, Rate = %f, AffectedStat = %u, BlockingGroup = %u, EffectIsBooster = %d, AddWeight = %f, HitImmunitySect = %s",
					i, Device.fTimeGlobal, eat_effect.DurationExpiration, eat_effect.UseTimeExpiration, eat_effect.Rate, eat_effect.AffectedStat,
					eat_effect.BlockingGroup, eat_effect.BoosterParam.EffectIsBooster, eat_effect.BoosterParam.AddWeight, eat_effect.BoosterParam.HitImmunitySect.c_str());

					ActorEntity->conditions().Eat_Effects.push_back(eat_effect);
				}

				//(Удаление произходит в след. кадр)
				if (m_iPortionsNum != -1)
				{
					//уменьшить количество порций
					if (m_iPortionsNum > 0)
						--(m_iPortionsNum);
					else
						m_iPortionsNum = 0;
				}

				if (use_sound_line)
				{
					CurrentGameUI()->m_InventoryMenu->PlayUseSound(use_sound_line);
				}

				return true;
			}

			SDrawStaticStruct* HudMessage = CurrentGameUI()->AddCustomStatic("inv_hud_message", true);
			HudMessage->m_endTime = Device.fTimeGlobal + 3.0f;

			string1024 str;
			if (hands_are_blocked)
				xr_sprintf(str, "%s", *CStringTable().translate("st_item_hands_are_buisy"));
			else
				xr_sprintf(str, "%s", *CStringTable().translate("st_item_refuse"));

			HudMessage->wnd()->TextItemControl()->SetText(str);
			return false;
		}
		else
		{	// Для НПС
			entity_alive->conditions().ChangeHealth(m_fHealthInfluence);
			entity_alive->conditions().ChangePower(m_fPowerInfluence);
			entity_alive->conditions().ChangeSatiety(m_fSatietyInfluence);
			entity_alive->conditions().ChangeThirsty(m_fThirstyInfluence);
			entity_alive->conditions().ChangePsyHealth(m_fPsyHealthInfluence);
			entity_alive->conditions().ChangeRadiation(m_fRadiationInfluence);
			entity_alive->conditions().ChangeBleeding(m_fWoundsHealPerc);
			entity_alive->conditions().SetMaxPower(entity_alive->conditions().GetMaxPower() + m_fMaxPowerUpInfluence);
			if (m_iPortionsNum != -1)
			{
				//уменьшить количество порций
				if (m_iPortionsNum > 0)
					--(m_iPortionsNum);
				else
					m_iPortionsNum = 0;
			}
			return true;
		}
	}
	else
	{
		entity_alive->conditions().ChangeHealth(m_fHealthInfluence);
		entity_alive->conditions().ChangePower(m_fPowerInfluence);
		entity_alive->conditions().ChangeSatiety(m_fSatietyInfluence);
		entity_alive->conditions().ChangeThirsty(m_fThirstyInfluence);
		entity_alive->conditions().ChangePsyHealth(m_fPsyHealthInfluence);
		entity_alive->conditions().ChangeRadiation(m_fRadiationInfluence);
		entity_alive->conditions().ChangeBleeding(m_fWoundsHealPerc);
		entity_alive->conditions().SetMaxPower(entity_alive->conditions().GetMaxPower() + m_fMaxPowerUpInfluence);
		entity_alive->conditions().ChangeAlcohol(m_falcohol);
		if (m_iPortionsNum != -1)
		{
			//уменьшить количество порций
			if (m_iPortionsNum > 0)
				--(m_iPortionsNum);
			else
				m_iPortionsNum = 0;
		}

		CActor* ActorEntity = smart_cast<CActor*>(entity_alive);

		if (ActorEntity && use_sound_line)
		{
			CurrentGameUI()->m_InventoryMenu->PlayUseSound(use_sound_line);
		}

		return true;
	}
	return false;
}
