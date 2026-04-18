#pragma once

#include "../../xrUI/Widgets/UIScrollView.h"
#include "UIStatsPlayerInfo.h"

class CUIXml;

typedef bool(*player_cmp_func)(LPVOID v1, LPVOID v2);

class CUIStatsPlayerList final : public CUIScrollView
{
public:
					CUIStatsPlayerList			();
	virtual			~CUIStatsPlayerList			();

			void 	Init						(CUIXml& xml_doc, const char* path);
			void 	SetSpectator				(bool f);
			void 	SetTeam						(int team);
			void 	AddField					(const char* name, float width);
	CUIStatic*	 	GetHeader					();
	CUIWindow*	 	GetTeamHeader				();
			void 	SetTextParams				(CGameFont* pF, u32 col);
			void 	SetHeaderHeight				(float h);
	virtual void 	AddWindow					(CUIWindow* pWnd, bool auto_delete = true);
	virtual void 	Update						();

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIScrollView* ui_cast_scroll_view() { return this; }

protected:
			void	InitHeader					(CUIXml& xml_doc, const char* path);
			void	InitTeamHeader				(CUIXml& xml_doc, const char* path);
	virtual void	RecalcSize					();
			void	ShowHeader					(bool bShow);
			const char*	GetST_entry					(const char* itm);

	int				m_CurTeam;
	bool			m_bSpectator;
	bool			m_bStatus_mode;
    
	xr_vector<PI_FIELD_INFO>		m_field_info;

	CUIStatic*						m_header;
    CUIWindow*						m_header_team;
	CUIStatic*						m_header_text;
	u32								m_prev_upd_time;

	typedef struct{
		u32			c; //color
		CGameFont*	f; //font
		float		h; //height
	} S_ELEMENT;

	S_ELEMENT		m_h;	// header
	S_ELEMENT		m_i;	// item
	S_ELEMENT		m_t;	// team header

};