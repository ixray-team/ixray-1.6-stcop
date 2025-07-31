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

class CUIStatic;
class CUIXml;
class CUIProgressBar;
class CUIFrameLineWnd;
class CUIFrameWindow;
class CUICharacterInfo;
class CUIScrollView;

class CUIRankingWnd : 
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

	CUITextWnd*			m_money_caption = nullptr;
	CUITextWnd*			m_money_value = nullptr;

	CUITextWnd*			m_center_caption = nullptr;
	CUIStatic*			m_faction_static = nullptr;
	CUIFrameLineWnd*	m_faction_line1= nullptr;
	CUIFrameLineWnd*	m_faction_line2= nullptr;

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

	enum { max_stat_info = 32 };
	CUITextWnd*			m_stat_caption[max_stat_info];
	CUITextWnd*			m_stat_info[max_stat_info];

	u32					m_delay;
	u32					m_previous_time;
	u32					m_stat_count;
	LPCSTR				m_last_monster_icon_back;
	LPCSTR				m_last_monster_icon;
	LPCSTR				m_last_weapon_icon;

	bool m_isGetRankingsArraySize = false;
	const char* m_onGetRankingsArraySize = {};

public:
						CUIRankingWnd			();
	virtual				~CUIRankingWnd			();

	virtual void 		Show					(bool status);
	virtual void		Update					();
	virtual void		DrawHint				();
	virtual void		ResetAll				();

			void		Init					();
			void		update_info				();

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
			void		add_faction				(CUIXml& xml, shared_str const& faction_id);
			void		clear_all_factions		();
			bool		SortingLessFunction		(CUIWindow* left, CUIWindow* right);
			void		get_value_from_script	();

			void		add_achievement			(CUIXml& xml, shared_str const& faction_id);
			void		get_statistic			();
			void		get_best_monster		();
			void		get_favorite_weapon		();

}; // class CUIRankingWnd
