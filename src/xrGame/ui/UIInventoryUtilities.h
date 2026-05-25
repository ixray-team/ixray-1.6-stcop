#pragma once

#include "../inventory_item.h"

#include "../../xrUI/ui_defs.h"

class CUIStatic;

//размеры сетки в текстуре инвентаря
#define INV_GRID_WIDTH(SCALE_ICON) (50.0f * SCALE_ICON)
#define INV_GRID_HEIGHT(SCALE_ICON) (50.0f * SCALE_ICON)

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

constexpr const char* BUY_MENU_TEXTURE = "ui\\ui_mp_buy_menu";
constexpr const char* CHAR_ICONS = "ui\\ui_icons_npc";
constexpr const char* MAP_ICONS = "ui\\ui_icons_map";
constexpr const char* MP_CHAR_ICONS = "ui\\ui_models_multiplayer";

namespace InventoryUtilities
{

//сравнивает элементы по пространству занимаемому ими в рюкзаке
//для сортировки
bool GreaterRoomInRuck	(PIItem item1, PIItem item2);
//для проверки свободного места
bool FreeRoom_inBelt	(TIItemContainer& item_list, PIItem item, int width, int height);


// get shader for BuyWeaponWnd
const ui_shader&	GetBuyMenuShader();
const ui_shader&	GetIconsShader(const char* name, const char* defaultName, xr_hash_map<xr_string, ui_shader*>& shaders);
//получить shader на иконки инвенторя
const ui_shader& GetEquipmentIconsShader(const char* name = nullptr);
// shader на иконки персонажей в мультиплеере
const ui_shader&	GetMPCharIconsShader();
//get shader for outfit icons in upgrade menu
const ui_shader& GetOutfitUpgradeIconsShader(const char* name);
//get shader for weapon icons in upgrade menu
const ui_shader& GetWeaponUpgradeIconsShader(const char* name);
//удаляем все шейдеры
void DestroyShaders();
void CreateShaders();

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

struct InventoryIconParams
{
	const char* _3d_static_visual;
	Fvector _3d_static_rotate;
	float _3d_static_scale;
	float scaleIcon;

	const char* icons_texture;
	float inv_grid_x;
	float inv_grid_y;
	float inv_grid_width;
	float inv_grid_height;
};

struct ConditionDisplayParams
{
	float state;
	bool usePortion;
	int portionCurrent;
	int portionMax;
	bool hideBackground;
	bool disableGradient;
};

const shared_str GetGameDateAsString(EDatePrecision datePrec, char dateSeparator = ',');
const shared_str GetGameTimeAsString(ETimePrecision timePrec, char timeSeparator = ':');
const shared_str GetDateAsString(ALife::_TIME_ID time, EDatePrecision datePrec, char dateSeparator = ',');
const shared_str GetDateAsStringLegacy(ALife::_TIME_ID time, EDatePrecision datePrec, char dateSeparator = '/');
const shared_str GetTimeAsString(ALife::_TIME_ID time, ETimePrecision timePrec, char timeSeparator = ':', bool full_mode = true);
const shared_str GetTimeAndDateAsString(ALife::_TIME_ID time, bool legacyMode = false);
const shared_str Get_GameTimeAndDate_AsString();

const char* GetTimePeriodAsString	(LPSTR _buff, u32 buff_sz, ALife::_TIME_ID _from, ALife::_TIME_ID _to);
// Отобразить вес, который несет (*pInvOwner)
void UpdateWeight(CUIStatic& wnd, bool withPrefix = false);
void UpdateWeightStr(CUIStatic* weightLabel, CUIStatic& wnd_max, CInventoryOwner* pInvOwner);
void UpdateVolumeStr(CUIStatic* volumeLabel, CUIStatic* volumeMax, CInventoryOwner* pInvOwner);

// Функции получения строки-идентификатора ранга и отношения по их числовому идентификатору
const char*	GetRankAsText				(s32		rankID);
const char*	GetReputationAsText			(s32 rankID);
const char*	GetGoodwillAsText			(s32			goodwill);

void	ClearCharacterInfoStrings	();

void	SendInfoToActor				(const char* info_id);
void	SendInfoToLuaScripts		(shared_str info);
u32		GetGoodwillColor			(s32 gw);
u32		GetRelationColor			(ALife::ERelationType r);
u32		GetReputationColor			(s32 rv);
InventoryIconParams	GetInventoryIconParams(const char* section);
ConditionDisplayParams GetConditionDisplayParams(CInventoryItem* item);
};
