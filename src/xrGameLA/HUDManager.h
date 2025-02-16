#pragma once

#include "../CustomHUD.h"
#include "HitMarker.h"

extern	u32	ui_hud_type;

class CHUDTarget;
class CUIGameCustom;

class CHUDManager :
	public CCustomHUD
{
	friend class CUI;
private:
	CUIGameCustom*			pUIGame;
	CHitMarker				HitMarker;
	CHUDTarget*				m_pHUDTarget;
	bool					b_online;
public:
							CHUDManager			();
	virtual					~CHUDManager		();
	virtual		void		OnEvent				(EVENT E, u64 P1, u64 P2);
	
	virtual		void		Render_First		();
	virtual		void		Render_Last			();	   
	virtual		void		OnFrame				();

	virtual		void		RenderUI			();

		CUIGameCustom*		GetGameUI			(){return pUIGame;}

				void		Hit					(int idx, float power, const Fvector& dir);
	//CFontManager&			Font				()							{return *(UI().Font());}
	//текущий предмет на который смотрит HUD
	collide::rq_result&		GetCurrentRayQuery	();


	//устанвка внешнего вида прицела в зависимости от текущей дисперсии
	void					SetCrosshairDisp	(float dispf, float disps = 0.f);
	void					ShowCrosshair		(bool show);

	void					SetHitmarkType		(LPCSTR tex_name);
	virtual void			OnScreenResolutionChanged();
	virtual	void			Load				();
	virtual void			OnDisconnected		();
	virtual void			OnConnected			();

	virtual	void			RenderActiveItemUI	();
	virtual	bool			RenderActiveItemUIQuery();

	virtual void			net_Relcase			(CObject *object);
};

IC CHUDManager&			HUD()		{ return *((CHUDManager*)g_hud);}