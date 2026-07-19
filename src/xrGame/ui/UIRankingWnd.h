////////////////////////////////////////////////////////////////////////////
//	Module 		: UIRankingWnd.h
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Ranking window class
////////////////////////////////////////////////////////////////////////////

#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "UIRankFaction.h"
#include "UIAchievements.h"
#include "UIRankingsCoC.h"
#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"

class CUIStatic;
class CUIStackPanel;
class CUIWindow;
class CUIXml;
class CUIProgressBar;
class CUIFrameLineWnd;
class CUIFrameWindow;
class CUICharacterInfo;
class CUIScrollView;
class CUIGamepadLegend;
class CPdaUiSounds;

class CUIRankingWnd final :
	public CUIWindow,
	public CUIWndCallback
{
private:
	typedef CUIWindow	inherited;

	CUIFrameWindow*		m_background = nullptr;
	CUIFrameLineWnd*	m_background2 = nullptr;
	CUIStatic*			m_center_background = nullptr;
	CUIFrameWindow*		m_down_background = nullptr;
	CUIFrameWindow*		m_icon_overlay = nullptr;

	CUICharacterInfo*	m_actor_ch_info = nullptr;

	CUIStatic*			m_money_caption = nullptr;
	CUIStatic*			m_money_value = nullptr;

	CUIStatic*			m_center_caption = nullptr;
	CUIStatic*			m_faction_static = nullptr;
	CUIFrameLineWnd*	m_faction_line1 = nullptr;
	CUIFrameLineWnd*	m_faction_line2 = nullptr;

	CUIScrollView*		m_factions_list = nullptr;

	CUIScrollView*		m_achievements = nullptr;
	CUIFrameWindow*		m_achievements_background = nullptr;
	CUIFrameWindow*		m_monster_background = nullptr;
	CUIFrameWindow*		m_monster_over = nullptr;
	CUIFrameWindow*		m_favorite_weapon_ramka = nullptr;
	CUIFrameWindow*		m_favorite_weapon_over = nullptr;
	CUIStatic*			m_monster_icon_back = nullptr;
	CUIStatic*			m_monster_icon = nullptr;
	CUIStatic*			m_favorite_weapon_bckgrnd = nullptr;
	CUIStatic*			m_favorite_weapon_icon = nullptr;

	CUIStatic*			m_valuable_artifact_icon = nullptr;
	CUICharacterInfo*	m_ranking_actor_identity = nullptr;

	//Alundaio: CoC Rankings
	CUIScrollView*		m_coc_ranking = nullptr;
	CUIScrollView*		m_coc_ranking_actor_view = nullptr;
	CUIFrameWindow*		m_coc_ranking_background = nullptr;
	//-Alundaio 

	using ACHIEVES_VEC = xr_vector<CUIAchievements*>;
	using ACHIEVES_VEC_IT = ACHIEVES_VEC::iterator;

	ACHIEVES_VEC		m_achieves_vec;

	//Alundaio: CoC Rankings
	using RANKINGCOC_VEC = xr_vector<CUIRankingsCoC*>;
	using RANKINGCOC_VEC_IT = RANKINGCOC_VEC::iterator;
	RANKINGCOC_VEC		m_coc_ranking_vec;

	CUIRankingsCoC* m_coc_ranking_actor = nullptr;
	//-Alundaio

	struct StatItem final
	{
		enum class ELayout : u8
		{
			Legacy = 0,
			StackedRow = 1,
			SplitColumns = 2,
		};

		ELayout layout = ELayout::Legacy;
		shared_str statId;
		CUIStackPanel* rowStack = nullptr;
		CUIWindow* rowRoot = nullptr;
		CUIStatic* caption = nullptr;
		CUIStatic* value = nullptr;
		shared_str cachedValue;
	};

	xr_vector<StatItem> m_stat_items;
	CUIStackPanel* _statList = nullptr;
	CUIWindow* _statColumns = nullptr;
	CUIStackPanel* _statCaptionsStack = nullptr;
	CUIStackPanel* _statValuesStack = nullptr;

	u32					m_delay;
	u32					m_previous_time;
	u32					m_statDelay;
	u32					m_statPreviousTime;
	u32					m_actorStatRevision;
	u32					m_stat_count;
	const char*				m_last_monster_icon_back;
	const char*				m_last_monster_icon;
	const char*				m_last_weapon_icon;
	shared_str				m_last_valuable_artifact_icon;

	CPdaUiSounds*		m_pUiSounds = nullptr;

	bool m_isGetRankingsArraySize = false;
	const char* m_onGetRankingsArraySize = {};
	bool m_isGetPdaStatById = false;
	const char* m_onGetPdaStatById = {};

public:
						CUIRankingWnd			();
	virtual				~CUIRankingWnd			();

	virtual void 		Show					(bool status);
	virtual void		Update					();
	virtual void		DrawHint				();
	virtual void		ResetAll				();

			void		Init					();
			void		SetUiSounds				(CPdaUiSounds* uiSounds) { m_pUiSounds = uiSounds; }
			void		update_info				();

			bool		OnGamepadKeyAction		(int key, EUIMessages gamepad_action) override;
			bool		OnGamepadKeyHold		(int key) override;

	CUIGamepadLegend*	m_gamepad_legend = nullptr;

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
			void		add_faction				(CUIXml& xml, shared_str const& faction_id);
			void		clear_all_factions		();
			bool		SortingLessFunction		(CUIWindow* left, CUIWindow* right);
			void		RefreshStatItems		();
			void		RefreshStatItemsIfNeeded();
			void		update_ranking_heavy	();
			void		InitStatInfo			(CUIXml& xml);
			bool		InitLegacyStat			(CUIXml& xml, XML_NODE* statInfoNode, u32 index, u32 valueColor);
			bool		InitSplitStatColumns		(CUIXml& xml, XML_NODE* statInfoNode, u32 valueColor);
			bool		InitStackedStatRow		(CUIXml& xml, XML_NODE* statInfoNode, u32 index, u32 valueColor);

			void		add_achievement			(CUIXml& xml, shared_str const& faction_id);
			void		get_best_monster		();
			void		get_favorite_weapon		();
			void		get_valuable_artifact_icon();
			const char* GetStatValue		(const StatItem& item, const u32 index) const;

}; // class CUIRankingWnd
