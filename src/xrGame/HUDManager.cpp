#include "StdAfx.h"
#include "HUDManager.h"
#include "HUDTarget.h"
#include "Actor.h"
#include "../xrEngine/IGame_Level.h"
#include "../xrEngine/xr_input.h"
#include "GamePersistent.h"
#include "MainMenu.h"
#include "Grenade.h"
#include "Spectator.h"
#include "Car.h"
#include "UIGameCustom.h"
#include "../../xrUI/UICursor.h"
#include "../xrEngine/string_table.h"
#include "game_cl_base.h"
#ifdef	DEBUG
#include "PHDebug.h"
#endif
#include "../../xrUI/UIFontDefines.h"

extern CUIGameCustom* CurrentGameUI() {return HUD().GetGameUI();}

//--------------------------------------------------------------------
CHUDManager::CHUDManager() : pUIGame(nullptr), m_pHUDTarget(new CHUDTarget())
{ 
}
//--------------------------------------------------------------------
CHUDManager::~CHUDManager()
{
	OnDisconnected();

	if(pUIGame)		
		pUIGame->UnLoad	();

	xr_delete		(pUIGame);
	xr_delete		(m_pHUDTarget);
}
#include "GametaskManager.h"
//--------------------------------------------------------------------
void CHUDManager::OnFrame()
{
	if (!psHUD_Flags.is(HUD_DRAW_RT2))	
		return;

	if(!b_online)						
		return;

	PROF_EVENT("CHUDManager::OnFrame");
	m_pHUDTarget->CursorOnFrame();

	if (Device.IsEditorMode())
	{
		OnFrameMT();
	}
}

xrCriticalSection ui_lock;
void CHUDManager::OnFrameMT()
{
	if (!psHUD_Flags.is(HUD_DRAW_RT2))	
		return;

	if(!b_online)						
		return;
	PROF_EVENT("CHUDManager::OnFrameMT");

	if (Device.dwPrecacheFrame == 0)
		Level().GameTaskManager()->UpdateTasks();

	xrCriticalSectionGuard guard(&ui_lock);
	if (pUIGame) 
		pUIGame->OnFrame();
}
//--------------------------------------------------------------------

ENGINE_API extern float psHUD_FOV;

void CHUDManager::Render_First()
{
	if (!psHUD_Flags.is(HUD_WEAPON|HUD_WEAPON_RT|HUD_WEAPON_RT2|HUD_DRAW_RT2))return;
	if (0==pUIGame)					return;
	CObject*	O					= g_pGameLevel->CurrentViewEntity();
	if (0==O)						return;
	CActor*		A					= smart_cast<CActor*> (O);
	if (!A)							return;
	if (A && !A->HUDview())			return;

	// only shadow 
	::Render->set_Invisible			(TRUE);
	::Render->set_Object			(O->H_Root());
	O->renderable_Render			();
	::Render->set_Invisible			(FALSE);
}

bool need_render_hud()
{
	CObject*	O					= g_pGameLevel ? g_pGameLevel->CurrentViewEntity() : nullptr;
	if (0==O)						
		return false;

	CActor*		A					= smart_cast<CActor*> (O);
	if (A && (!A->HUDview() || !A->g_Alive()) ) 
		return false;

	if( smart_cast<CCar*>(O) || smart_cast<CSpectator*>(O) )
		return false;

	return true;
}

void CHUDManager::Render_Last()
{
	if (!psHUD_Flags.is(HUD_WEAPON|HUD_WEAPON_RT|HUD_WEAPON_RT2|HUD_DRAW_RT2))return;
	if (0==pUIGame)					return;

	if(!need_render_hud())			return;

	CObject*	O					= g_pGameLevel->CurrentViewEntity();
	// hud itself
	::Render->set_HUD				(TRUE);
	::Render->set_Object			(O->H_Root());
	O->OnHUDDraw					(this);
	::Render->set_HUD				(FALSE);
}

#include "player_hud.h"
bool   CHUDManager::RenderActiveItemUIQuery()
{
	if (!psHUD_Flags.is(HUD_DRAW_RT2))	
		return false;

	if (!psHUD_Flags.is(HUD_WEAPON|HUD_WEAPON_RT|HUD_WEAPON_RT2))return false;

	if(!need_render_hud())			return false;

	return (g_player_hud && g_player_hud->render_item_ui_query() );
}

void   CHUDManager::RenderActiveItemUI()
{
	if (!psHUD_Flags.is(HUD_DRAW_RT2))	
		return;

	g_player_hud->render_item_ui		();
}

extern ENGINE_API BOOL bShowPauseString;
//отрисовка элементов интерфейса
void  CHUDManager::RenderUI()
{
	if (!psHUD_Flags.is(HUD_DRAW_RT2))	
		return;

	if(!b_online)					return;
	PROF_EVENT("CHUDManager::RenderUI");
	if (true /*|| psHUD_Flags.is(HUD_DRAW | HUD_DRAW_RT)*/)
	{
		HitMarker.Render			();
		if(pUIGame)
		{
			xrCriticalSectionGuard guard(&ui_lock);
			pUIGame->Render();
		}

		UI().RenderFont				();
	}

		m_pHUDTarget->Render();


	if( Device.Paused() && bShowPauseString){
		CGameFont* pFont	= UI().Font().GetFont(GRAFFITI50_FONT_NAME);
		pFont->SetColor		(0x80FF0000	);
		LPCSTR _str			= g_pStringTable->translate("st_game_paused").c_str();
		
		Fvector2			_pos;
		_pos.set			(UI_BASE_WIDTH/2.0f, UI_BASE_HEIGHT/2.0f);
		UI().ClientToScreenScaled(_pos);
		pFont->SetAligment	(CGameFont::alCenter);
		pFont->Out			(_pos.x, _pos.y, _str);
		pFont->OnRender		();
	}

}

void CHUDManager::OnEvent(EVENT E, u64 P1, u64 P2)
{
}

collide::rq_result&	CHUDManager::GetCurrentRayQuery	() 
{
	return m_pHUDTarget->GetRQ();
}

void CHUDManager::SetCrosshairDisp	(float dispf, float disps)
{	
	m_pHUDTarget->GetHUDCrosshair().SetDispersion(psHUD_Flags.test(HUD_CROSSHAIR_DYNAMIC) ? dispf : disps);
}

#ifdef DEBUG
void CHUDManager::SetFirstBulletCrosshairDisp(float fbdispf)
{
	m_pHUDTarget->GetHUDCrosshair().SetFirstBulletDispertion(fbdispf);
}
#endif

void  CHUDManager::ShowCrosshair(bool show)
{
	m_pHUDTarget->ShowCrosshair	(show);
}

void CHUDManager::HitMarked( int idx, float power, const Fvector& dir )
{
	HitMarker.Hit			(dir);
	clamp					(power,0.0f,1.0f);
	pInput->feedback		(u16(iFloor(u16(-1)*power)), u16(iFloor(u16(-1)*power)), 0.5f);
}

bool CHUDManager::AddGrenade_ForMark( CGrenade* grn )
{
	return HitMarker.AddGrenade_ForMark( grn );
}

void CHUDManager::Update_GrenadeView( Fvector& pos_actor )
{
	HitMarker.Update_GrenadeView( pos_actor );
}

void CHUDManager::SetHitmarkType( LPCSTR tex_name )
{
	HitMarker.InitShader( tex_name );
}

void CHUDManager::SetGrenadeMarkType( LPCSTR tex_name )
{
	HitMarker.InitShader_Grenade( tex_name );
}

// ------------------------------------------------------------------------------------

#include "ui/UIMainIngameWnd.h"
extern CUIXml*			pWpnScopeXml;

void CHUDManager::Load()
{
	if (!pUIGame)
	{
		pUIGame				= Game().createGameUI();
	} else
	{
		pUIGame->SetClGame	(&Game());
	}
}

void CHUDManager::OnScreenResolutionChanged()
{
	pUIGame->HideShownDialogs			();

	xr_delete							(pWpnScopeXml);

	pUIGame->UnLoad						();
	pUIGame->Load						();

	pUIGame->OnConnected				();

	if (pUIGame)
		Game().OnScreenResolutionChanged();
}

void CHUDManager::OnDisconnected()
{
	b_online				= false;
}

void CHUDManager::OnConnected()
{
	if(b_online)			return;
	b_online				= true;
}

void CHUDManager::net_Relcase( CObject* obj )
{
	HitMarker.net_Relcase		( obj );
	
	VERIFY						( m_pHUDTarget );
	m_pHUDTarget->net_Relcase	( obj );
#ifdef	DEBUG
	DBG_PH_NetRelcase( obj );
#endif
}