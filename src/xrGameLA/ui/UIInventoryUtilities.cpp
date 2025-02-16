#include "stdafx.h"
#include "UIInventoryUtilities.h"
#include "../WeaponAmmo.h"
#include "../WeaponMagazined.h"
#include "../UIStaticItem.h"
#include "UIStatic.h"
#include "../eatable_item.h"
#include "../Level.h"
#include "../HUDManager.h"
#include "../date_time.h"
#include "../../xrEngine/string_table.h"
#include "../Inventory.h"
#include "../InventoryOwner.h"

#include "../InfoPortion.h"
#include "../game_base_space.h"
#include "../actor.h"
#include "../ActorCondition.h"

#include "../../Include/xrRender/UIShader.h"

#define BUY_MENU_TEXTURE "ui\\ui_mp_buy_menu"
#define EQUIPMENT_ICONS  "ui\\ui_icon_equipment"
#define CHAR_ICONS		 "ui\\ui_icons_npc"
#define MAP_ICONS		 "ui\\ui_icons_map"
#define MP_CHAR_ICONS	 "ui\\ui_models_multiplayer"

const LPCSTR relationsLtxSection	= "game_relations";
const LPCSTR ratingField			= "rating_names";
const LPCSTR reputationgField		= "reputation_names";
const LPCSTR goodwillField			= "goodwill_names";

ui_shader	*g_BuyMenuShader			= nullptr;
ui_shader	*g_EquipmentIconsShader		= nullptr;
ui_shader	*g_MPCharIconsShader		= nullptr;
ui_shader	*g_OutfitUpgradeIconsShader	= nullptr;
ui_shader	*g_WeaponUpgradeIconsShader	= nullptr;
ui_shader	*g_tmpWMShader				= nullptr;
static CUIStatic*	GetUIStatic				();

typedef				std::pair<CHARACTER_RANK_VALUE, shared_str>	CharInfoStringID;

using CharInfoStrings = xr_map<CHARACTER_RANK_VALUE, shared_str>;
using CharInfoStrings_it = CharInfoStrings::iterator;

CharInfoStrings		*charInfoReputationStrings	= nullptr;
CharInfoStrings		*charInfoRankStrings		= nullptr;
CharInfoStrings		*charInfoGoodwillStrings	= nullptr;

void InventoryUtilities::CreateShaders()
{
	g_tmpWMShader = new ui_shader();
	(*g_tmpWMShader)->create("effects\\wallmark",  "wm\\wm_grenade");
	//g_tmpWMShader.create("effects\\wallmark",  "wm\\wm_grenade");
}

void InventoryUtilities::DestroyShaders()
{
	xr_delete(g_BuyMenuShader);
	g_BuyMenuShader = 0;

	xr_delete(g_EquipmentIconsShader);
	g_EquipmentIconsShader = 0;

	xr_delete(g_MPCharIconsShader);
	g_MPCharIconsShader = 0;

	xr_delete(g_OutfitUpgradeIconsShader);
	g_OutfitUpgradeIconsShader = 0;

	xr_delete(g_WeaponUpgradeIconsShader);
	g_WeaponUpgradeIconsShader = 0;

	xr_delete(g_tmpWMShader);
	g_tmpWMShader = 0;
}

bool InventoryUtilities::GreaterRoomInRuck(PIItem item1, PIItem item2)
{
	Ivector2 r1,r2;
	r1.x			= item1->GetGridWidth();
	r1.y			= item1->GetGridHeight();
	r2.x			= item2->GetGridWidth();
	r2.y			= item2->GetGridHeight();

	if(r1.x > r2.x)			return true;
	
	if (r1.x == r2.x)
	{
		if(r1.y > r2.y)		return true;
		
		if (r1.y == r2.y){
			auto ammo1 = smart_cast<CWeaponAmmo*>(item1);
			auto ammo2 = smart_cast<CWeaponAmmo*>(item2);
			if(item1->object().cNameSect()==item2->object().cNameSect())
			{
				if (ammo1 && ammo2)
				{
					return ammo1->m_boxCurr > ammo2->m_boxCurr;
				}
				auto wpn1 = smart_cast<CWeaponMagazined*>(item1);
				auto wpn2 = smart_cast<CWeaponMagazined*>(item2);
				if (wpn1 && wpn2)
				{
					return wpn1->GetAmmoElapsed() > wpn2->GetAmmoElapsed();
				}
				if (!fsimilar(item1->GetCondition(), item2->GetCondition()))
				{
					return item1->GetCondition() > item2->GetCondition();
				}
				return (item1->object().ID() > item2->object().ID());
			}
			else
			{
				// ammo always goes to the left
				if (ammo1 && !ammo2 || !ammo1 && ammo2)
				{
					return ammo1 != nullptr;
				}
				return (item1->object().cNameSect() > item2->object().cNameSect());
			}
		}

		return				false;
	}
   	return					false;
}

bool InventoryUtilities::GreaterRoomInRuckCellItem(CUICellItem* cell1, CUICellItem* cell2)
{
	PIItem item1 = (PIItem)cell1->m_pData;
	PIItem item2 = (PIItem)cell2->m_pData;

	Ivector2 r1, r2;
	r1.x = item1->GetGridWidth();
	r1.y = item1->GetGridHeight();
	r2.x = item2->GetGridWidth();
	r2.y = item2->GetGridHeight();

	if (r1.x > r2.x)			return true;

	if (r1.x == r2.x)
	{
		if (r1.y > r2.y)		return true;

		if (r1.y == r2.y){
			if (item1->object().cNameSect() == item2->object().cNameSect())
				return (item1->object().ID() > item2->object().ID());
			else
				return (item1->object().cNameSect()>item2->object().cNameSect());

		}

		return				false;
	}
	return					false;
}

bool InventoryUtilities::FreeRoom_inBelt	(const TIItemContainer& src_item_list, PIItem _item, int width, int height)
{
	// make a copy of belt items
	TIItemContainer item_list = src_item_list;
	std::vector<bool> ruck_room(width * height, false);

	int		i,j,k,m;
	int		place_row = 0,  place_col = 0;
	bool	found_place;
	bool	can_place;

	item_list.push_back	(_item);
	std::sort			(item_list.begin(), item_list.end(),GreaterRoomInRuck);
	
	found_place			= true;

	for(xr_vector<PIItem>::iterator it = item_list.begin(); (item_list.end() != it) && found_place; ++it) 
	{
		PIItem pItem = *it;
		int iWidth	= pItem->GetGridWidth(); 
		int iHeight = pItem->GetGridHeight();
		//проверить можно ли разместить элемент,
		//проверяем последовательно каждую клеточку
		found_place = false;
	
		for(i=0; (i<height - iHeight +1) && !found_place; ++i)
		{
			for(j=0; (j<width - iWidth +1) && !found_place; ++j)
			{
				can_place = true;

				for(k=0; (k<iHeight) && can_place; ++k)
				{
					for(m=0; (m<iHeight) && can_place; ++m)
					{
						if(ruck_room[(i+k)*width + (j+m)])
								can_place =  false;
					}
				}
			
				if(can_place)
				{
					found_place=true;
					place_row = i;
					place_col = j;
				}

			}
		}

		//разместить элемент на найденном месте
		if(found_place)
		{
			for(k=0; k<iHeight; ++k)
			{
				for(m=0; m<iWidth; ++m)
				{
					ruck_room[(place_row+k)*width + place_col+m] = true;
				}
			}
		}
	}

	return found_place;
}

const ui_shader& InventoryUtilities::GetBuyMenuShader()
{	
	if(!g_BuyMenuShader)
	{
		g_BuyMenuShader = new ui_shader();
		(*g_BuyMenuShader)->create("hud\\default", BUY_MENU_TEXTURE);
	}

	return *g_BuyMenuShader;
}

const ui_shader& InventoryUtilities::GetEquipmentIconsShader()
{	
	if(!g_EquipmentIconsShader)
	{
		g_EquipmentIconsShader = new ui_shader();
		(*g_EquipmentIconsShader)->create("hud\\default", EQUIPMENT_ICONS);
	}

	return *g_EquipmentIconsShader;
}

const ui_shader&	InventoryUtilities::GetMPCharIconsShader()
{
	if(!g_MPCharIconsShader)
	{
		g_MPCharIconsShader = new ui_shader();
		(*g_MPCharIconsShader)->create("hud\\default",  MP_CHAR_ICONS);
	}

	return *g_MPCharIconsShader;
}

const ui_shader& InventoryUtilities::GetOutfitUpgradeIconsShader()
{	
	if(!g_OutfitUpgradeIconsShader)
	{
		g_OutfitUpgradeIconsShader = new ui_shader();
		(*g_OutfitUpgradeIconsShader)->create("hud\\default", "ui\\ui_actor_armor");
	}

	return *g_OutfitUpgradeIconsShader;
}

const ui_shader& InventoryUtilities::GetWeaponUpgradeIconsShader()
{	
	if(!g_WeaponUpgradeIconsShader)
	{
		g_WeaponUpgradeIconsShader = new ui_shader();
		(*g_WeaponUpgradeIconsShader)->create("hud\\default", "ui\\ui_actor_weapons");
	}

	return *g_WeaponUpgradeIconsShader;
}


void InventoryUtilities::UpdateWeaponUpgradeIconsShader(CUIStatic* item)
{
	item->InitTextureEx("ui\\ui_actor_weapons", "hud\\default");
}

void InventoryUtilities::UpdateOutfitUpgradeIconsShader(CUIStatic* item)
{
	item->InitTextureEx("ui\\ui_actor_armor", "hud\\default");
}

//////////////////////////////////////////////////////////////////////////

const shared_str InventoryUtilities::GetGameDateAsString(EDatePrecision datePrec, char dateSeparator)
{
	return GetDateAsString(Level().GetGameTime(), datePrec, dateSeparator);
}

//////////////////////////////////////////////////////////////////////////

const shared_str InventoryUtilities::GetGameTimeAsString(ETimePrecision timePrec, char timeSeparator)
{
	return GetTimeAsString(Level().GetGameTime(), timePrec, timeSeparator);
}

//////////////////////////////////////////////////////////////////////////

const shared_str InventoryUtilities::GetTimeAsString(ALife::_TIME_ID time, ETimePrecision timePrec, char timeSeparator)
{
	string64 bufTime;

	ZeroMemory(bufTime, sizeof(bufTime));

	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;

	split_time(time, year, month, day, hours, mins, secs, milisecs);

	// Time
	switch (timePrec)
	{
	case etpTimeToHours:
		xr_sprintf(bufTime, "%02i", hours);
		break;
	case etpTimeToMinutes:
		xr_sprintf(bufTime, "%02i%c%02i", hours, timeSeparator, mins);
		break;
	case etpTimeToSeconds:
		xr_sprintf(bufTime, "%02i%c%02i%c%02i", hours, timeSeparator, mins, timeSeparator, secs);
		break;
	case etpTimeToMilisecs:
		xr_sprintf(bufTime, "%02i%c%02i%c%02i%c%02i", hours, timeSeparator, mins, timeSeparator, secs, timeSeparator, milisecs);
		break;
	case etpTimeToSecondsAndDay:
		{
			int total_day = (int)( time/(1000*60*60*24) );
			xr_sprintf(bufTime, sizeof(bufTime), "%dd %02i%c%02i%c%02i", total_day, hours, timeSeparator, mins, timeSeparator, secs);
			break;
		}
	default:
		R_ASSERT(!"Unknown type of date precision");
	}

	return bufTime;
}

const shared_str InventoryUtilities::GetDateAsString(ALife::_TIME_ID date, EDatePrecision datePrec, char dateSeparator)
{
	string64 bufDate;

	ZeroMemory(bufDate, sizeof(bufDate));

	u32 year = 0, month = 0, day = 0, hours = 0, mins = 0, secs = 0, milisecs = 0;

	split_time(date, year, month, day, hours, mins, secs, milisecs);

	// Date
	switch (datePrec)
	{
	case edpDateToYear:
		xr_sprintf(bufDate, "%04i", year);
		break;
	case edpDateToMonth:
		xr_sprintf(bufDate, "%02i%c%04i", month, dateSeparator, year);
		break;
	case edpDateToDay:
		xr_sprintf(bufDate, "%02i%c%02i%c%04i", day, dateSeparator, month, dateSeparator, year);
		break;
	default:
		R_ASSERT(!"Unknown type of date precision");
	}

	return bufDate;
}

LPCSTR InventoryUtilities::GetTimePeriodAsString(LPSTR _buff, u32 buff_sz, ALife::_TIME_ID _from, ALife::_TIME_ID _to)
{
	u32 year1,month1,day1,hours1,mins1,secs1,milisecs1;
	u32 year2,month2,day2,hours2,mins2,secs2,milisecs2;
	
	split_time(_from, year1, month1, day1, hours1, mins1, secs1, milisecs1);
	split_time(_to, year2, month2, day2, hours2, mins2, secs2, milisecs2);
	
	int cnt		= 0;
	_buff[0]	= 0;

	if(month1!=month2)
		cnt = xr_sprintf(_buff+cnt,buff_sz-cnt,"%d %s ",month2-month1, *CStringTable().translate("ui_st_months"));

	if(!cnt && day1!=day2)
		cnt = xr_sprintf(_buff+cnt,buff_sz-cnt,"%d %s",day2-day1, *CStringTable().translate("ui_st_days"));

	if(!cnt && hours1!=hours2)
		cnt = xr_sprintf(_buff+cnt,buff_sz-cnt,"%d %s",hours2-hours1, *CStringTable().translate("ui_st_hours"));

	if(!cnt && mins1!=mins2)
		cnt = xr_sprintf(_buff+cnt,buff_sz-cnt,"%d %s",mins2-mins1, *CStringTable().translate("ui_st_mins"));

	if(!cnt && secs1!=secs2)
		cnt = xr_sprintf(_buff+cnt,buff_sz-cnt,"%d %s",secs2-secs1, *CStringTable().translate("ui_st_secs"));

	return _buff;
}

//////////////////////////////////////////////////////////////////////////

void InventoryUtilities::UpdateWeightContainer(CUIStatic &wnd, CInventory *pInventory, LPCSTR prefixStr)
{
	R_ASSERT(pInventory);
	string128 buf;

	float total = pInventory->CalcTotalWeight();
	float max	= pInventory->GetMaxWeight();

	string32 prefix;

	if (prefixStr && prefixStr[0])
	{
		xr_sprintf(prefix, "%s : ", prefixStr);
	}
	else
	{
		xr_strcpy(prefix, "");
	}

	xr_sprintf(buf, "%s%s%3.1f /%5.1f kg", prefix, "%c[UI_orange]", total, max);
	wnd.TextItemControl()->SetText(buf);
}

void InventoryUtilities::UpdateWeight(CUIStatic &wnd, bool withPrefix)
{
	CInventoryOwner *pInvOwner = smart_cast<CInventoryOwner*>(Level().CurrentEntity());
	R_ASSERT(pInvOwner);
	string128 buf;
	ZeroMemory(buf, sizeof(buf));

	float total = pInvOwner->inventory().CalcTotalWeight();
	float max	= pInvOwner->MaxCarryWeight();

	auto pActor = smart_cast<CActor*>(pInvOwner);
	float maxWalk = (pActor != nullptr)
		? pActor->MaxWalkWeight()
		: max;

	string16 cl;
	ZeroMemory(cl, sizeof(cl));

	if (total > max)
	{
		xr_strcpy(cl, "%c[red]");
	}
	else
	{
		xr_strcpy(cl, "%c[UI_orange]");
	}

	string32 prefix;
	ZeroMemory(prefix, sizeof(prefix));

	if (withPrefix)
	{
		xr_sprintf(prefix, "%%c[default]%s ", *CStringTable().translate("ui_inv_weight"));
	}
	else
	{
		xr_strcpy(prefix, "");
	}

	xr_sprintf(buf, "%s%s%3.1f %s/%5.1f (%.1f) kg", prefix, cl, total, "%c[UI_orange]", max, maxWalk);
	wnd.TextItemControl()->SetText(buf);
	//	UIStaticWeight.ClipperOff();
}

//////////////////////////////////////////////////////////////////////////

void LoadStrings(CharInfoStrings *container, LPCSTR section, LPCSTR field)
{
	R_ASSERT(container);

	LPCSTR				cfgRecord	= pSettings->r_string(section, field);
	u32					count		= _GetItemCount(cfgRecord);
	R_ASSERT3			(count%2, "there're must be an odd number of elements", field);
	string64			singleThreshold;
	ZeroMemory			(singleThreshold, sizeof(singleThreshold));
	int					upBoundThreshold	= 0;
	CharInfoStringID	id;

	for (u32 k = 0; k < count; k += 2)
	{
		_GetItem(cfgRecord, k, singleThreshold);
		id.second = singleThreshold;

		_GetItem(cfgRecord, k + 1, singleThreshold);
		if(k+1!=count)
			sscanf(singleThreshold, "%i", &upBoundThreshold);
		else
			upBoundThreshold	+= 1;

		id.first = upBoundThreshold;

		container->insert(id);
	}
}

//////////////////////////////////////////////////////////////////////////

void InitCharacterInfoStrings()
{
	if (charInfoReputationStrings && charInfoRankStrings) return;

	if (!charInfoReputationStrings)
	{
		// Create string->Id DB
		charInfoReputationStrings	= new CharInfoStrings();
		// Reputation
		LoadStrings(charInfoReputationStrings, relationsLtxSection, reputationgField);
	}

	if (!charInfoRankStrings)
	{
		// Create string->Id DB
		charInfoRankStrings			= new CharInfoStrings();
		// Ranks
		LoadStrings(charInfoRankStrings, relationsLtxSection, ratingField);
	}

	if (!charInfoGoodwillStrings)
	{
		// Create string->Id DB
		charInfoGoodwillStrings			= new CharInfoStrings();
		// Goodwills
		LoadStrings(charInfoGoodwillStrings, relationsLtxSection, goodwillField);
	}

}

//////////////////////////////////////////////////////////////////////////

void InventoryUtilities::ClearCharacterInfoStrings()
{
	xr_delete(charInfoReputationStrings);
	xr_delete(charInfoRankStrings);
	xr_delete(charInfoGoodwillStrings);
}

//////////////////////////////////////////////////////////////////////////

LPCSTR InventoryUtilities::GetRankAsText(CHARACTER_RANK_VALUE rankID)
{
	InitCharacterInfoStrings();
	CharInfoStrings::const_iterator cit = charInfoRankStrings->upper_bound(rankID);
	if(charInfoRankStrings->end() == cit)
		return charInfoRankStrings->rbegin()->second.c_str();
	return cit->second.c_str();
}

//////////////////////////////////////////////////////////////////////////

LPCSTR InventoryUtilities::GetReputationAsText(CHARACTER_REPUTATION_VALUE rankID)
{
	InitCharacterInfoStrings();

	CharInfoStrings::const_iterator cit = charInfoReputationStrings->upper_bound(rankID);
	if(charInfoReputationStrings->end() == cit)
		return charInfoReputationStrings->rbegin()->second.c_str();

	return cit->second.c_str();
}

//////////////////////////////////////////////////////////////////////////

LPCSTR InventoryUtilities::GetGoodwillAsText(CHARACTER_GOODWILL goodwill)
{
	InitCharacterInfoStrings();

	CharInfoStrings::const_iterator cit = charInfoGoodwillStrings->upper_bound(goodwill);
	if(charInfoGoodwillStrings->end() == cit)
		return charInfoGoodwillStrings->rbegin()->second.c_str();

	return cit->second.c_str();
}


//////////////////////////////////////////////////////////////////////////
// специальная функция для передачи info_portions при нажатии кнопок UI 
// (для tutorial)
void InventoryUtilities::SendInfoToActor(LPCSTR info_id)
{
	if (GameID() != GAME_SINGLE) return;
	
	CActor* actor = smart_cast<CActor*>(Level().CurrentEntity());
	if(actor)
	{
		actor->TransferInfo(info_id, true);
	}
}

//////////////////////////////////////////////////////////////////////////
// skyloader: специальная функция для получения info_portions при нажатии кнопок UI 
// (для tutorial)
bool InventoryUtilities::HasActorInfo(LPCSTR info_id)
{
	if (GameID() != GAME_SINGLE) return false;
	
	CActor* actor = smart_cast<CActor*>(Level().CurrentEntity());
	if(actor)
		return (actor->HasInfo(info_id));
	else
		return false;
}

u32 InventoryUtilities::GetGoodwillColor(CHARACTER_GOODWILL gw)
{
	u32 res = 0xffc0c0c0;
	if(gw==NEUTRAL_GOODWILL){
		res = 0xffc0c0c0;
	}else
	if(gw>1000){
		res = 0xff00ff00;
	}else
	if(gw<-1000){
		res = 0xffff0000;
	}
	return res;
}

u32 InventoryUtilities::GetReputationColor(CHARACTER_REPUTATION_VALUE rv)
{
	u32 res = 0xffc0c0c0;
	if(rv==NEUTAL_REPUTATION){
		res = 0xffc0c0c0;
	}else
	if(rv>50){
		res = 0xff00ff00;
	}else
	if(rv<-50){
		res = 0xffff0000;
	}
	return res;
}

u32	InventoryUtilities::GetRelationColor(ALife::ERelationType relation)
{
	switch(relation) {
	case ALife::eRelationTypeFriend:
		return 0xff00ff00;
		break;
	case ALife::eRelationTypeNeutral:
		return 0xffc0c0c0;
		break;
	case ALife::eRelationTypeEnemy:
		return  0xffff0000;
		break;
	default:
		NODEFAULT;
	}
#ifdef DEBUG
	return 0xffffffff;
#endif
}

//////////////////////////////////////////////////////////////////////

const shared_str InventoryUtilities::GetTimeAndDateAsString(ALife::_TIME_ID time)
{
	string256 buf;
	LPCSTR time_str = GetTimeAsString( time, etpTimeToMinutes ).c_str();
	LPCSTR date_str = GetDateAsString( time, edpDateToDay ).c_str();
	xr_strconcat( buf, time_str, ", ", date_str );
	return buf;
}