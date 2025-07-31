#include "StdAfx.h"
#include "HUDTarget.h"
#include "../xrEngine/GameMtlLib.h"

#include "../xrEngine/Environment.h"
#include "../xrEngine/CustomHUD.h"
#include "Entity.h"
#include "Level.h"
#include "game_cl_base.h"
#include "../xrEngine/IGame_Persistent.h"

#include "../../xrUI/ui_base.h"
#include "InventoryOwner.h"
#include "relation_registry.h"
#include "character_info.h"

#include "../xrEngine/string_table.h"
#include "entity_alive.h"

#include "inventory_item.h"
#include "Inventory.h"
#include "../../xrUI/UIXmlInit.h"
#include <ai/monsters/poltergeist/poltergeist.h>
#include "../../xrUI/UIFontDefines.h"
#include "Actor.h"

u32 C_ON_ENEMY = color_rgba(0xff,0,0,0x80);
u32 C_ON_NEUTRAL = color_rgba(0xff,0xff,0x80,0x80);
u32 C_ON_FRIEND = color_rgba(0,0xff,0,0x80);
u32 C_DEFAULT = color_rgba(0xff, 0xff, 0xff, 0x80);

#define C_SIZE		0.025f
#define NEAR_LIM	0.5f

#define SHOW_INFO_SPEED		0.5f
#define HIDE_INFO_SPEED		10.f


IC	float	recon_mindist	()		{
	return 2.f;
}
IC	float	recon_maxdist	()		{
	return 50.f;
}
IC	float	recon_minspeed	()		{
	return 0.5f;
}
IC	float	recon_maxspeed	()		{
	return 10.f;
}

CHUDTarget::CHUDTarget	()
{    
	targetFont			= nullptr;
	bInitialized		= false;
	fuzzyShowInfo		= 0.f;
	PP.RQ.range			= 0.f;

	PP.RQ.set				(nullptr, 0.f, -1);

	Load				();
	m_bShowCrosshair	= false;
}

CHUDTarget::~CHUDTarget	()
{
}


void CHUDTarget::Load		()
{
	if (bInitialized)
		return;

	CUIXml xml;
	u32 color;
	xml.Load(CONFIG_PATH, UI_PATH, "hud_target.xml");

	LPCSTR texture = xml.Read("texture", 0, "ui\\cursor");
	LPCSTR shader = xml.Read("shader", 0, "hud\\cursor");

	colorEnemy = CUIXmlInit::GetColor(xml, "enemy_color", 0, color_rgba(0xff, 0, 0, 0x80));
	colorFriend = CUIXmlInit::GetColor(xml, "friend_color", 0, color_rgba(0, 0xff, 0, 0x80));
	colorNeutral = CUIXmlInit::GetColor(xml, "neutral_color", 0, color_rgba(0xff, 0xff, 0x80, 0x80));
	colorDefault = CUIXmlInit::GetColor(xml, "default_color", 0, color_rgba(0xff, 0xff, 0xff, 0x80));

	CUIXmlInit::InitFont(xml, "target_font", 0, color, targetFont);

	hShader->create		(shader,texture);

	HUDCrosshair.Load();
	bInitialized = true;
}

void CHUDTarget::ShowCrosshair(bool b)
{
	m_bShowCrosshair = b;
}
//. fVisTransparencyFactor
float fCurrentPickPower;
ICF static BOOL pick_trace_callback(collide::rq_result& result, LPVOID params)
{
	SPickParam*	pp			= (SPickParam*)params;
//	collide::rq_result* RQ	= pp->RQ;
	++pp->pass;

	if(result.O)
	{	
		pp->RQ				= result;
		return FALSE;
	}else
	{
		//получить треугольник и узнать его материал
		CDB::TRI* T		= Level().ObjectSpace.GetStaticTris()+result.element;
		
		SGameMtl* mtl = GMLib.GetMaterialByIdx(T->material);
		pp->power		*= mtl->fVisTransparencyFactor;
		if(pp->power>0.34f)
		{
			return TRUE;
		}
//.		if (mtl->Flags.is(SGameMtl::flPassable)) 
//.			return TRUE;
	}
	pp->RQ					= result;
	return					FALSE;
}

void CHUDTarget::CursorOnFrame ()
{
	PROF_EVENT("CHUDTarget::CursorOnFrame");
	// Render cursor
	if(Level().CurrentEntity())
	{
		PP.RQ.O			= 0; 
		PP.RQ.range		= g_pGamePersistent->Environment().CurrentEnv->far_plane*0.99f;
		PP.RQ.element		= -1;
		
		collide::ray_defs	RD(Device.vCameraPosition, Device.vCameraDirection, PP.RQ.range, CDB::OPT_CULL, collide::rqtBoth);
		RQR.r_clear			();
		VERIFY				(!fis_zero(RD.dir.square_magnitude()));
		
		PP.power			= 1.0f;
		PP.pass				= 0;

		if(Level().ObjectSpace.RayQuery(RQR,RD, pick_trace_callback, &PP, nullptr, Level().CurrentEntity()))
			clamp			(PP.RQ.range, NEAR_LIM, PP.RQ.range);
	}
}

extern ENGINE_API xr_atomic_bool g_bRendering; 
void CHUDTarget::Render()
{
	if (!bInitialized)
	{
		Load();
		return;
	}
	BOOL  b_do_rendering = ( psHUD_Flags.is(HUD_CROSSHAIR|HUD_CROSSHAIR_RT|HUD_CROSSHAIR_RT2) );
	
	if(!b_do_rendering)
		return;

	if (load_screen_renderer.IsActive())
		return;

	VERIFY				(g_bRendering);

	CActor* Actor = smart_cast<CActor*>(Level().CurrentEntity());
	if (!Actor)
		return;

	bool get_motions = Actor->active_cam() == eacLookAt && ((Actor->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcSprint) || (Actor->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcJumpSeq));

	if (get_motions)
		return;

	CObject*	O		= Level().CurrentEntity();
	if (0==O)			return;
	CEntity*	E		= smart_cast<CEntity*>(O);
	if (0==E)			return;

	Fvector p1			= Device.vCameraPosition;
	Fvector dir			= Device.vCameraDirection;
	
	// Render cursor
	u32 C				= colorDefault;
	
	Fvector				p2;
	p2.mad				(p1,dir,PP.RQ.range);
	Fvector4			pt;
	Device.mFullTransform.transform(pt, p2);
	pt.y = -pt.y;
	float				di_size = C_SIZE/powf(pt.w,.2f);

	targetFont->SetAligment		(CGameFont::alCenter);
	targetFont->OutSetI			(0.f,0.05f);

	if (psHUD_Flags.test(HUD_CROSSHAIR_DIST))
		targetFont->OutSkip		();

	if (psHUD_Flags.test(HUD_INFO))
	{ 
		bool const is_poltergeist	= PP.RQ.O && !!smart_cast<CPoltergeist*> (PP.RQ.O);

		if ((PP.RQ.O && PP.RQ.O->getVisible()) || is_poltergeist)
		{
			CEntityAlive* E_ = smart_cast<CEntityAlive*>(PP.RQ.O);
			CEntityAlive* pCurEnt = smart_cast<CEntityAlive*>(Level().CurrentEntity());
			PIItem l_pI = smart_cast<PIItem>(PP.RQ.O);
			CActor* pActor = smart_cast<CActor*>	(PP.RQ.O);
			CInventoryOwner* our_inv_owner = smart_cast<CInventoryOwner*>(pCurEnt);

			if (E_ && E_->g_Alive())
			{
				if (E_->cast_base_monster())
				{
					C = colorEnemy;
				}
				else if (!pActor || (pActor && IsGameTypeSingleCompatible()))
				{
					CInventoryOwner* others_inv_owner = smart_cast<CInventoryOwner*>(E_);

					if (our_inv_owner && others_inv_owner)
					{
						switch (RELATION_REGISTRY().GetRelationType(others_inv_owner, our_inv_owner))
						{
						case ALife::eRelationTypeEnemy:
							C = colorEnemy; break;
						case ALife::eRelationTypeNeutral:
							C = colorNeutral; break;
						case ALife::eRelationTypeFriend:
							C = colorFriend; break;
						}

						if (fuzzyShowInfo > 0.5f)
						{
							targetFont->SetColor(subst_alpha(C, u8(iFloor(255.f * (fuzzyShowInfo - 0.5f) * 2.f))));
							targetFont->OutNext("%s", *g_pStringTable->translate(others_inv_owner->Name()));
							targetFont->OutNext("%s", *g_pStringTable->translate(others_inv_owner->CharacterInfo().Community().id()));
						}
					}
				}
				fuzzyShowInfo += SHOW_INFO_SPEED * Device.fTimeDelta;
			}
			else if (l_pI && our_inv_owner && PP.RQ.range < 2.0f * 2.0f)
			{
				if (fuzzyShowInfo > 0.5f && l_pI->NameItem())
				{
					targetFont->SetColor(subst_alpha(C, u8(iFloor(255.f * (fuzzyShowInfo - 0.5f) * 2.f))));
					targetFont->OutNext("%s", l_pI->NameItem());
				}
				fuzzyShowInfo += SHOW_INFO_SPEED * Device.fTimeDelta;
			}
		}
		else
		{
			fuzzyShowInfo -= HIDE_INFO_SPEED*Device.fTimeDelta;
		}
		clamp(fuzzyShowInfo,0.f,1.f);
	}

	if (psHUD_Flags.test(HUD_CROSSHAIR_DIST))
	{
		targetFont->OutSetI		(0.f,0.05f);
		targetFont->SetColor		(C);
#ifdef DEBUG
		targetFont->OutNext		("%4.1f - %4.2f - %d", PP.RQ.range, PP.power, PP.pass);
#else
		targetFont->OutNext		("%4.1f", PP.RQ.range);
#endif
	}

	//отрендерить кружочек или крестик
	if(!m_bShowCrosshair)
	{
		
		UIRender->StartPrimitive	(6, IUIRender::ptTriList, UI().m_currentPointType);
		
		Fvector2		scr_size;
		scr_size.set	(float(Device.TargetWidth) ,float(Device.TargetHeight));
		float			size_x = scr_size.x	* di_size;
		float			size_y = scr_size.y * di_size;

		size_y			= size_x;

		float			w_2		= scr_size.x/2.0f;
		float			h_2		= scr_size.y/2.0f;

		// Convert to screen coords
		float cx		    = (pt.x+1)*w_2;
		float cy		    = (pt.y+1)*h_2;

		//	TODO: return code back to indexed rendering since we use quads
		//	Tri 1
		UIRender->PushPoint(cx - size_x, cy + size_y, 0, C, 0, 1);
		UIRender->PushPoint(cx - size_x, cy - size_y, 0, C, 0, 0);
		UIRender->PushPoint(cx + size_x, cy + size_y, 0, C, 1, 1);
		//	Tri 2
		UIRender->PushPoint(cx + size_x, cy + size_y, 0, C, 1, 1);
		UIRender->PushPoint(cx - size_x, cy - size_y, 0, C, 0, 0);
		UIRender->PushPoint(cx + size_x, cy - size_y, 0, C, 1, 0);

		// unlock VB and Render it as triangle LIST
		UIRender->SetShader(*hShader);
		UIRender->FlushPrimitive();

	}else{
		//отрендерить прицел
		HUDCrosshair.cross_color	= C;
		HUDCrosshair.OnRender		();
	}
}

void CHUDTarget::net_Relcase(CObject* O)
{
	if(PP.RQ.O == O)
		PP.RQ.O = nullptr;

	RQR.r_clear	();
}
