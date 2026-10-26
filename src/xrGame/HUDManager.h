#pragma once

#include "../xrEngine/CustomHUD.h"
#include "HitMarker.h"
#include "LevelInspector.h"

class CUIStatic;
class CHUDTarget;
class CUIGameCustom;

class CHUDManager :
	public CCustomHUD
{
	friend class CUI;
private:
//.	CUI*					pUI;
	CUIGameCustom*			pUIGame;
	CHitMarker				HitMarker;
	CHUDTarget*				m_pHUDTarget;
	CUIStatic*				m_pPauseStatic = nullptr;
	bool					b_online;
public:
	LevelInspector			world_prims = LevelInspector(false);
	IC void					draw_prims()
	{
		world_prims.OnRender();
	};
							CHUDManager			();
	virtual ~CHUDManager		() override;
	virtual void		OnEvent				(EVENT E, u64 P1, u64 P2) override;

	virtual void		Render_First		() override;
	virtual void		Render_Last			() override;
	virtual void		OnFrame				() override;

	virtual void		OnFrameMT			() override;

	virtual		void		RenderUI			();

//.				CUI*		GetUI				(){return pUI;}
		CUIGameCustom*		GetGameUI			(){return pUIGame;}

				void		HitMarked			(int idx, float power, const Fvector& dir);
				bool		AddGrenade_ForMark	( CGrenade* grn );
				void		Update_GrenadeView	( Fvector& pos_actor );
	virtual void		net_Relcase			( CObject* obj ) override;
				void		InitializePauseStatic();

	//текущий предмет на который смотрит HUD
	collide::rq_result&		GetCurrentRayQuery	();


	//устанвка внешнего вида прицела в зависимости от текущей дисперсии
	void					SetCrosshairDisp	(float dispf, float disps = 0.f);
#ifdef DEBUG
	void					SetFirstBulletCrosshairDisp(float fbdispf);
#endif
	void					ShowCrosshair		(bool show);

	void					SetHitmarkType		( const char* tex_name );
	void					SetGrenadeMarkType	( const char* tex_name );

	virtual void			OnScreenResolutionChanged() override;
	virtual void			Load				() override;
	virtual void			OnDisconnected		() override;
	virtual void			OnConnected			() override;

	virtual void			RenderActiveItemUI	() override;
	virtual bool			RenderActiveItemUIQuery() override;

	//Lain: added
				void		SetRenderable       (bool renderable) { psHUD_Flags.set(HUD_DRAW_RT2,renderable); }
};

IC CHUDManager&			HUD()		{ return *((CHUDManager*)g_hud);}

