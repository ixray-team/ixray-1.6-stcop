#pragma once

#include "../inventory_item.h"
#include "../character_info_defs.h"
#include "../ui_defs.h"
#include "UICellItem.h"

class CUIStatic;
class CInventory;

//размеры сетки в текстуре инвентаря
#define INV_GRID_WIDTH			50.0f
#define INV_GRID_HEIGHT			50.0f

//размеры сетки в текстуре иконок персонажей
#define ICON_GRID_WIDTH			64.0f
#define ICON_GRID_HEIGHT		64.0f
//размер иконки персонажа для инвенторя и торговли
#define CHAR_ICON_WIDTH			2
#define CHAR_ICON_HEIGHT		2	

//размер иконки персонажа в полный рост
#define CHAR_ICON_FULL_WIDTH	2
#define CHAR_ICON_FULL_HEIGHT	5

#define TRADE_ICONS_SCALE		(4.f/5.f)

namespace InventoryUtilities
{

//сравнивает элементы по пространству занимаемому ими в рюкзаке
//для сортировки
bool GreaterRoomInRuck			(PIItem item1, PIItem item2);
bool GreaterRoomInRuckCellItem	(CUICellItem* cell1, CUICellItem* cell12);
//для проверки свободного места
bool FreeRoom_inBelt	(const TIItemContainer& item_list, PIItem item, int width, int height);

// get shader for BuyWeaponWnd
const ui_shader&	GetBuyMenuShader();
//получить shader на иконки инвенторя
const ui_shader& GetEquipmentIconsShader();
// shader на иконки персонажей в мультиплеере
const ui_shader&	GetMPCharIconsShader();
//get shader for outfit icons in upgrade menu
const ui_shader& GetOutfitUpgradeIconsShader();
//get shader for weapon icons in upgrade menu
const ui_shader& GetWeaponUpgradeIconsShader();
//удаляем все шейдеры
void DestroyShaders();
void CreateShaders();

void UpdateWeaponUpgradeIconsShader(CUIStatic* item);
void UpdateOutfitUpgradeIconsShader(CUIStatic* item);

// Получить значение времени в текстовом виде

// Точность возвращаемого функцией GetGameDateTimeAsString значения: до часов, до минут, до секунд
enum ETimePrecision
{
	etpTimeToHours = 0,
	etpTimeToMinutes,
	etpTimeToSeconds,
	etpTimeToMilisecs,
	etpTimeToSecondsAndDay
};

// Точность возвращаемого функцией GetGameDateTimeAsString значения: до года, до месяца, до дня
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
// Отобразить вес контейнера
void UpdateWeightContainer(CUIStatic &wnd, CInventory *pInventory, LPCSTR prefixStr = nullptr);
// Отобразить вес, который несет актер
void UpdateWeight(CUIStatic &wnd, bool withPrefix = false);

// Функции получения строки-идентификатора ранга и отношения по их числовому идентификатору
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