#pragma once

#include "../inventory_item.h"
#include "../character_info_defs.h"
#include "../../xrUI/ui_defs.h"
#include "UICellItem.h"

class CUIStatic;
class CInventory;

//ðàçìåðû ñåòêè â òåêñòóðå èíâåíòàðÿ
#define INV_GRID_WIDTH			50.0f
#define INV_GRID_HEIGHT			50.0f

//ðàçìåðû ñåòêè â òåêñòóðå èêîíîê ïåðñîíàæåé
#define ICON_GRID_WIDTH			64.0f
#define ICON_GRID_HEIGHT		64.0f
//ðàçìåð èêîíêè ïåðñîíàæà äëÿ èíâåíòîðÿ è òîðãîâëè
#define CHAR_ICON_WIDTH			2
#define CHAR_ICON_HEIGHT		2	

//ðàçìåð èêîíêè ïåðñîíàæà â ïîëíûé ðîñò
#define CHAR_ICON_FULL_WIDTH	2
#define CHAR_ICON_FULL_HEIGHT	5

#define TRADE_ICONS_SCALE		(4.f/5.f)

namespace InventoryUtilities
{

//ñðàâíèâàåò ýëåìåíòû ïî ïðîñòðàíñòâó çàíèìàåìîìó èìè â ðþêçàêå
//äëÿ ñîðòèðîâêè
bool GreaterRoomInRuck			(PIItem item1, PIItem item2);
bool GreaterRoomInRuckCellItem	(CUICellItem* cell1, CUICellItem* cell12);
//äëÿ ïðîâåðêè ñâîáîäíîãî ìåñòà
bool FreeRoom_inBelt	(const TIItemContainer& item_list, PIItem item, int width, int height);

// get shader for BuyWeaponWnd
const ui_shader&	GetBuyMenuShader();
//ïîëó÷èòü shader íà èêîíêè èíâåíòîðÿ
const ui_shader& GetEquipmentIconsShader();
// shader íà èêîíêè ïåðñîíàæåé â ìóëüòèïëååðå
const ui_shader&	GetMPCharIconsShader();
//get shader for outfit icons in upgrade menu
const ui_shader& GetOutfitUpgradeIconsShader();
//get shader for weapon icons in upgrade menu
const ui_shader& GetWeaponUpgradeIconsShader();
//óäàëÿåì âñå øåéäåðû
void DestroyShaders();
void CreateShaders();

void UpdateWeaponUpgradeIconsShader(CUIStatic* item);
void UpdateOutfitUpgradeIconsShader(CUIStatic* item);

// Ïîëó÷èòü çíà÷åíèå âðåìåíè â òåêñòîâîì âèäå

// Òî÷íîñòü âîçâðàùàåìîãî ôóíêöèåé GetGameDateTimeAsString çíà÷åíèÿ: äî ÷àñîâ, äî ìèíóò, äî ñåêóíä
enum ETimePrecision
{
	etpTimeToHours = 0,
	etpTimeToMinutes,
	etpTimeToSeconds,
	etpTimeToMilisecs,
	etpTimeToSecondsAndDay
};

// Òî÷íîñòü âîçâðàùàåìîãî ôóíêöèåé GetGameDateTimeAsString çíà÷åíèÿ: äî ãîäà, äî ìåñÿöà, äî äíÿ
enum EDatePrecision
{
	edpDateToDay,
	edpDateToMonth,
	edpDateToYear
};

const shared_str GetGameDateAsString(EDatePrecision datePrec, char dateSeparator = '/');
const shared_str GetTimeAndDateAsString(ALife::_TIME_ID time);
const shared_str GetGameTimeAsString(ETimePrecision timePrec, char timeSeparator = ':');
const shared_str GetDateAsString(ALife::_TIME_ID time, EDatePrecision datePrec, char dateSeparator = '/');
const shared_str GetTimeAsString(ALife::_TIME_ID time, ETimePrecision timePrec, char timeSeparator = ':');
LPCSTR GetTimePeriodAsString	(LPSTR _buff, u32 buff_sz, ALife::_TIME_ID _from, ALife::_TIME_ID _to);
// Îòîáðàçèòü âåñ êîíòåéíåðà
void UpdateWeightContainer(CUIStatic &wnd, CInventory *pInventory, LPCSTR prefixStr = nullptr);
// Îòîáðàçèòü âåñ, êîòîðûé íåñåò àêòåð
void UpdateWeight(CUIStatic &wnd, bool withPrefix = false);

// Ôóíêöèè ïîëó÷åíèÿ ñòðîêè-èäåíòèôèêàòîðà ðàíãà è îòíîøåíèÿ ïî èõ ÷èñëîâîìó èäåíòèôèêàòîðó
LPCSTR	GetRankAsText				(CHARACTER_RANK_VALUE		rankID);
LPCSTR	GetReputationAsText			(CHARACTER_REPUTATION_VALUE rankID);
LPCSTR	GetGoodwillAsText			(CHARACTER_GOODWILL			goodwill);

void	ClearCharacterInfoStrings	();

void	SendInfoToActor				(LPCSTR info_id);
bool	HasActorInfo				(LPCSTR info_id);
u32		GetGoodwillColor			(CHARACTER_GOODWILL gw);
u32		GetRelationColor			(ALife::ERelationType r);
u32		GetReputationColor			(CHARACTER_REPUTATION_VALUE rv);
};