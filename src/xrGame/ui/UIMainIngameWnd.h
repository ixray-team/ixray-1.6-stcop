#pragma once
#include "UIGameLog.h"

#include "../HudSound.h"
#include "../../xrEngine/AI/alife_space.h"
#include "../EntityCondition.h"
#include "UICarPanel.h"

class	CUIPdaMsgListItem;
class	CLAItem;
class	CUIZoneMap;
class	CUIScrollView;
struct	GAME_NEWS_DATA;
class	CMissile;
class	CInventoryItem;
class	CUIHudStatesWnd;
class	CUIMotionIcon;
class	CUIArtefactPanel;
class	CUIStackPanel;

class CUIMainIngameWnd final :
	public CUIWindow
{
public:
	CUIMainIngameWnd();
	virtual ~CUIMainIngameWnd();

	virtual void Init();
	virtual void Draw();
	virtual void Update();

	virtual CUIWindow* ui_cast_window() { return this; }

protected:

	CUIStatic* m_ind_sleepiness = nullptr;
	CUIStatic* m_ind_thirst = nullptr;

	CUIStatic* m_icon_microphone = nullptr;
	CUITextWnd* m_voice_distance = nullptr;
public:
	CUIStatic* m_ind_boost_psy;
	CUIStatic* m_ind_boost_radia;
	CUIStatic* m_ind_boost_chem;
	CUIStatic* m_ind_boost_wound;
	CUIStatic* m_ind_boost_weight;
	CUIStatic* m_ind_boost_health;
	CUIStatic* m_ind_boost_power;
	CUIStatic* m_ind_boost_rad;

	CUIStatic* m_ind_weapon_broken;
	CUIStatic* m_ind_helmet_broken;
	CUIStatic* m_ind_outfit_broken;
	CUIStatic* m_ind_overweight;

	CUIStatic* m_ind_bleeding;
	CUIStatic* m_ind_radiation;
	CUIStatic* m_ind_starvation;

	CUIStatic* UIStaticDiskIO;
	CUITextWnd* UIStaticQuickHelp;
	CUIMotionIcon* UIMotionIcon;
	CUIZoneMap* UIZoneMap;

	CUIStackPanel* UIStackPanelBoosters;
	CUIStackPanel* UIStackPanelIndicators;

	//иконка, показывающая количество активных PDA
	CUIStatic*			UIPdaOnline;

	CUIHudStatesWnd* m_ui_hud_states;

public:
	void			ShowZoneMap(bool status);
	void			DrawZoneMap();
	void			UpdateZoneMap();

	void			DrawMainIndicatorsForInventory();

	// Quick slots panel visibility / fade control
	void			ShowQuickSlotsPanel();
	void			HideQuickSlotsPanelImmediate();
	void			TickQuickSlotsPanelFade();
	void			SetQuickSlotsPanelVisible(bool visible);

	CUIHudStatesWnd* get_hud_states() { return m_ui_hud_states; } //temp
	void				OnSectorChanged(int sector);

	xr_vector<CUIStatic* > m_quick_slots_icons;
	CUITextWnd* m_QuickSlotText1;
	CUITextWnd* m_QuickSlotText2;
	CUITextWnd* m_QuickSlotText3;
	CUITextWnd* m_QuickSlotText4;

protected:
	// Panel fade state
	bool				m_quick_slots_visible = false; // Temporary visibility (auto-hides after delay)
	bool				m_quick_slots_force_visible = false; // Forced visibility (manual toggle, blocks auto-hide)
	bool				m_quick_slots_force_visible_by_key = false; // Track if force visibility was set by key press
	float				m_quick_slots_alpha = 0.0f; // Alpha value for fade effect (0.0 = transparent, 1.0 = opaque)
	float				m_quick_slots_last_interaction_time = 0.0f; // Timestamp of last interaction for auto-hide timer

protected:

	// 5 статиков для отображения иконок:
	// - сломанного оружия(only mp)
	// - радиации
	// - ранения
	// - голода
	// - усталости
	CUIStatic* 			UIWeaponJammedIcon;
	CUIStatic*			UIRadiaitionIcon;
	CUIStatic*			UIWoundIcon;
	CUIStatic*			UIStarvationIcon;
	CUIStatic*			UIPsyHealthIcon;
	CUIStatic* 			UIInvincibleIcon;
//	CUIStatic			UISleepIcon;
	CUIStatic* 			UIArtefactIcon;

	CUIScrollView* m_UIIcons;
	CUIWindow* m_pMPChatWnd;
	CUIWindow* m_pMPLogWnd;
	bool				useLegacyIndicators;

	// Car
	CUICarPanel UICarPanel;


	bool m_ind_bleeding_svg_inited;
	bool m_ind_weapon_broken_svg_inited;
	bool m_ind_helmet_broken_svg_inited;
	bool m_ind_outfit_broken_svg_inited;
	bool m_ind_overweight_svg_inited;
	bool m_ind_radiation_svg_inited;
	bool m_ind_starvation_svg_inited;

	bool m_ind_boost_psy_svg_inited;
	bool m_ind_boost_radia_svg_inited;
	bool m_ind_boost_chem_svg_inited;
	bool m_ind_boost_wound_svg_inited;
	bool m_ind_boost_weight_svg_inited;
	bool m_ind_boost_health_svg_inited;
	bool m_ind_boost_power_svg_inited;
	bool m_ind_boost_rad_svg_inited;
public:
	CUIArtefactPanel* m_artefactPanel;

	// Енумы соответсвующие предупреждающим иконкам 
	enum EWarningIcons
	{
		ewiAll = 0,
		ewiWeaponJammed,
		ewiRadiation,
		ewiWound,
		ewiStarvation,
		ewiPsyHealth,
//		ewiSleep,
		ewiInvincible,
		ewiArtefact,
	};

	void				SetMPChatLog(CUIWindow* pChat, CUIWindow* pLog);

	// Задаем цвет соответствующей иконке
	void				SetWarningIconColor(EWarningIcons icon, const u32 cl);
	void				TurnOffWarningIcon(EWarningIcons icon);

	// Пороги изменения цвета индикаторов, загружаемые из system.ltx
	typedef				xr_map<EWarningIcons, xr_vector<float> >	Thresholds;
	typedef				Thresholds::iterator						Thresholds_it;
	Thresholds			m_Thresholds;

	// Енум перечисления возможных мигающих иконок
	enum EFlashingIcons
	{
		efiPdaTask = 0,
		efiMail
	};

	void				SetFlashIconState_(EFlashingIcons type, bool enable);

	void				AnimateContacts(bool b_snd);
	HUD_SOUND_ITEM		m_contactSnd;

	void				ReceiveNews(GAME_NEWS_DATA* news);
	void				UpdateMainIndicators();
	void				UpdateBoosterIndicators(const xr_map<EBoostParams, SBooster> influences);

	void				SetActiveVoiceIcon(bool active);
	void				SetVoiceDistance(u8 distance);

protected:
	void				UpdateQuickSlots();
	void				SetWarningIconColorUI(CUIStatic* s, const u32 cl);
	void				InitFlashingIcons(CUIXml* node);
	void				DestroyFlashingIcons();
	void				UpdateFlashingIcons();
	//	void				UpdateActiveItemInfo			();

	//	void				SetAmmoIcon						(const shared_str& seсt_name);

		// first - иконка, second - анимация
	using FlashingIcons = xr_map<EFlashingIcons, CUIStatic*>;
	using FlashingIcons_it = FlashingIcons::iterator;

	FlashingIcons		m_FlashingIcons;

	//	CMissile*			m_pGrenade;
	//	CInventoryItem*		m_pItem;

		// Отображение подсказок при наведении прицела на объект
	void				RenderQuickInfos();

public:
	CUICarPanel& CarPanel() { return UICarPanel; };
	CUIMotionIcon* MotionIcon() { return UIMotionIcon; }
	void				OnConnected();
	void				reset_ui();

protected:
	CInventoryItem* m_pPickUpItem;
	CUIStatic* UIPickUpItemIcon;

	float				m_iPickUpItemIconX;
	float				m_iPickUpItemIconY;
	float				m_iPickUpItemIconWidth;
	float				m_iPickUpItemIconHeight;

	void				UpdatePickUpItem();
public:
	void				SetPickUpItem(CInventoryItem* PickUpItem);
#ifdef DEBUG
	void				draw_adjust_mode();
#endif
};
