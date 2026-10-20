#include "StdAfx.h"
#include "HUDTarget.h"
#include "../xrEngine/GameMtlLib.h"

#include "../xrEngine/Environment.h"
#include "../xrEngine/CustomHUD.h"
#include "Entity.h"
#include "Level.h"
#include "game_cl_base.h"
#include "../xrEngine/IGame_Persistent.h"
#include "ui/UIGameCustom.h"
#include "../../xrUI/ui_base.h"
#include "InventoryOwner.h"
#include "relation_registry.h"
#include "character_info.h"
#include "ui/UIRadialMenuWeapon.h"
#include "../xrEngine/string_table.h"
#include "entity_alive.h"
#include "clsid_game.h"

#include "inventory_item.h"
#include "Inventory.h"
#include "Weapon.h"
#include "../../xrUI/UIXmlInit.h"
#include <ai/monsters/poltergeist/poltergeist.h>
#include "../../xrUI/UIFontDefines.h"
#include "Actor.h"
#include "game_cl_single.h"
#include "../xrEngine/CameraBase.h"

u32 C_ON_ENEMY = color_rgba(0xff,0,0,0x80);
u32 C_ON_NEUTRAL = color_rgba(0xff,0xff,0x80,0x80);
u32 C_ON_FRIEND = color_rgba(0,0xff,0,0x80);
u32 C_DEFAULT = color_rgba(0xff, 0xff, 0xff, 0x80);

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
	accumulatedTime		= 0.f;
	PP.RQ.range			= 0.f;

	PP.RQ.set				(nullptr, 0.f, -1);

	Load				();
	m_bShowCrosshair	= false;
	crosshair_params.zero();
}

CHUDTarget::~CHUDTarget	() = default;


void CHUDTarget::Load		()
{
	if (bInitialized)
		return;

	CUIXml xml;
	u32 color;
	xml.Load(CONFIG_PATH, UI_PATH, "backend\\hud_target.xml");

	const char* texture = xml.Read("texture", 0, "ui\\cursor");
	const char* shader = xml.Read("shader", 0, "hud\\cursor");

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
ICF static bool pick_trace_callback(collide::rq_result& result, LPVOID params)
{
	SPickParam*	pp			= (SPickParam*)params;
//	collide::rq_result* RQ	= pp->RQ;
	++pp->pass;

	if(result.O)
	{	
		pp->RQ				= result;
		return false;
	}else
	{
		//получить треугольник и узнать его материал
		CDB::TRI& T		= Level().ObjectSpace.GetStaticTris()[result.element];
		
		SGameMtl* mtl = GMLib.GetMaterialByIdx(T.material);
		pp->power		*= mtl->fVisTransparencyFactor;
		if(pp->power>0.34f)
		{
			return true;
		}
//.		if (mtl->Flags.is(SGameMtl::flPassable)) 
//.			return true;
	}
	pp->RQ					= result;
	return					false;
}

void CHUDTarget::CursorOnFrame ()
{
	PROF_EVENT("CHUDTarget::CursorOnFrame");

	is_extended_inertia = false;

	// Render cursor
	if(Level().CurrentEntity())
	{
		CActor* actor		= Level().CurrentEntity()->cast_actor();

		PP.RQ.O			= 0; 
		PP.RQ.range		= g_pGamePersistent->Environment().CurrentEnv->far_plane*0.99f;
		PP.RQ.element		= -1;

		float pick_range = PP.RQ.range;

		collide::ray_defs	RD(Device.vCameraPosition, Device.vCameraDirection, pick_range, CDB::OPT_CULL, collide::rqtBoth);
		RQR.r_clear			();
		VERIFY				(!fis_zero(RD.dir.square_magnitude()));
		
		PP.power			= 1.0f;
		PP.pass				= 0;

		if(Level().ObjectSpace.RayQuery(RQR,RD, pick_trace_callback, &PP, nullptr, Level().CurrentEntity()))
			clamp			(PP.RQ.range, NEAR_LIM, PP.RQ.range);

		if (crosshair_mode == EXTENDED && actor)
		{
			CWeapon* weapon = nullptr;
			if (CInventoryItem* item = actor->inventory().ActiveItem())
			{
				weapon = item->cast_weapon();
			}

			if (weapon)
			{
				attachable_hud_item* hud_item = weapon->HudItemData();
				const bool has_fire_point = hud_item == nullptr || hud_item->m_measures.m_prop_flags.test(hud_item_measures::e_fire_point);
				const bool is_barrel_weapon = !weapon->cast_weapon_binoculars() && !weapon->cast_weapon_knife();

				if (is_barrel_weapon && has_fire_point)
				{
					Fvector fire_pos = weapon->get_CurrentFirePoint();
					Fvector fire_dir = weapon->get_LastFD();

					if (fire_dir.square_magnitude() > EPS_S)
					{
						if ((weapon->GetHUDmode() || weapon->render_item_ui_query()) && hud_item)
						{
							Device.transform_hud2world(fire_pos, fire_dir);
						}

						collide::rq_result RQ;
						const float distance = Level().ObjectSpace.RayPick(fire_pos, fire_dir, pick_range, collide::rqtBoth, RQ, Level().CurrentEntity()) ? RQ.range : pick_range;
						extended_inertia_target.mad(fire_pos, fire_dir, distance);
						is_extended_inertia = true;
					}
				}
			}
		}
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
	const static bool noHUDonMaster = EngineExternal()[EEngineExternalUI::DisableHudRenderingOnMaster];
	bool renderHUDTarget = noHUDonMaster ? g_SingleGameDifficulty < egdVeteran : true;
	bool  b_do_rendering = ( psHUD_Flags.is(HUD_CROSSHAIR|HUD_CROSSHAIR_RT|HUD_CROSSHAIR_RT2) && renderHUDTarget );
	
	if(!b_do_rendering)
	{
		return;
	}
	
	if (load_screen_renderer.IsActive())
	{
		return;
	}

	if (CurrentGameUI() && CurrentGameUI()->RadialMenuWeapon() && CurrentGameUI()->RadialMenuWeapon()->IsShown())
	{
		return;
	}

	VERIFY(g_bRendering);

	CActor* Actor = Level().CurrentEntity() != nullptr ? Level().CurrentEntity()->cast_actor() : nullptr;
	if (!Actor)
	{
		return;
	}

	if (Actor->active_cam() == eacLookAt && ((Actor->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcSprint) || (Actor->GetMovementState(eReal) & ACTOR_DEFS::EMoveCommand::mcJumpSeq)))
	{
		return;
	}

	CObject* O = Level().CurrentEntity();
	if (nullptr == O)
	{
		return;
	}

	CEntity* E = O->cast_entity();
	if (nullptr == E)
	{
		return;
	}

	const bool actor_camera = crosshair_mode != DEFAULT;
	Fvector from = actor_camera ? Actor->cam_Active()->Position() : Device.vCameraPosition;
	Fvector dir = actor_camera ? Actor->cam_Active()->Direction() : Device.vCameraDirection;

	u32 C = colorDefault;

	Fvector picked;
	is_extended_inertia ? picked.set(extended_inertia_target) : picked.mad(from, dir, PP.RQ.range);

	Fvector4 projected;
	Device.mFullTransform.transform(projected, picked);
	projected.y = -projected.y;

	float di_size = C_SIZE / powf(projected.w, .2f);

	targetFont->SetAligment(CGameFont::alCenter);

	Fvector2 screen;
	screen.set(float(Device.TargetWidth), float(Device.TargetHeight));

	float size_x = screen.x * di_size;
	float size_y = screen.y * di_size;
	size_y = size_x;

	float x = (projected.x + 1) * screen.x / 2.0f;
	float y = (projected.y + 1) * screen.y / 2.0f;

	HUDCrosshair.dc.x = x;
	HUDCrosshair.dc.y = y;
	HUDCrosshair.cross_color = C;

	if (crosshair_mode != DEFAULT)
	{
		float t = powf(crosshair_inertia_factor, Device.fTimeDelta * 144.f);

		HUDCrosshair.dc.x = _inertion(HUDCrosshair.dc.x, x, t);
		HUDCrosshair.dc.y = _inertion(HUDCrosshair.dc.y, y, t);

		x = HUDCrosshair.dc.x;
		y = HUDCrosshair.dc.y;
	}

	crosshair_mode == EXTENDED ? targetFont->OutSet(x, y) : targetFont->OutSetI(0.f, .033f);

	// отрендерить кружочек или крестик
	if (!m_bShowCrosshair)
	{
		UIRender->StartPrimitive(6, IUIRender::ptTriList, UI().m_currentPointType);

		UIRender->PushPoint(x - size_x, y + size_y, 0, C, 0, 1);
		UIRender->PushPoint(x - size_x, y - size_y, 0, C, 0, 0);
		UIRender->PushPoint(x + size_x, y + size_y, 0, C, 1, 1);

		UIRender->PushPoint(x + size_x, y + size_y, 0, C, 1, 1);
		UIRender->PushPoint(x - size_x, y - size_y, 0, C, 0, 0);
		UIRender->PushPoint(x + size_x, y - size_y, 0, C, 1, 0);

		UIRender->SetShader(*hShader);
		UIRender->FlushPrimitive();
	}
	else
	{
		HUDCrosshair.OnRender();
	}

	if (psHUD_Flags.test(HUD_CROSSHAIR_DIST))
	{
		targetFont->SetColor(C);
#ifdef DEBUG
		targetFont->OutNext("%4.1f - %4.2f - %d", PP.RQ.range, PP.power, PP.pass);
#else
		targetFont->OutNext("%4.1f", PP.RQ.range);
#endif
	}

	if (psHUD_Flags.test(HUD_INFO))
	{ 
		bool const is_poltergeist	= PP.RQ.O && !!smart_cast<CPoltergeist*> (PP.RQ.O);

		if ((PP.RQ.O && PP.RQ.O->getVisible()) || is_poltergeist)
		{
			CEntityAlive* E_ = PP.RQ.O->cast_entity_alive();
			CEntityAlive* pCurEnt = Level().CurrentEntity() != nullptr ? Level().CurrentEntity()->cast_entity_alive() : nullptr;
			PIItem l_pI = PP.RQ.O->cast_inventory_item();
			CActor* pActor = PP.RQ.O->cast_actor();
			CInventoryOwner* our_inv_owner = pCurEnt != nullptr ? pCurEnt->cast_inventory_owner() : nullptr;

			if (E_ && E_->g_Alive())
			{
				if (auto BaseMonster = E_->cast_base_monster(); BaseMonster)
				{
					if (BaseMonster->ShouldMarkAsEnemy())
					{
						C = E_->CLS_ID == CLSID_AI_SCAVENGER_CROW ? colorNeutral : colorEnemy;
					}
				}
				else if (!pActor || (pActor && IsGameTypeSingleCompatible()))
				{
					CInventoryOwner* others_inv_owner = E_->cast_inventory_owner();

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

						targetFont->SetColor(subst_alpha(C, static_cast<u32>(lerp(0.f, 255.f, accumulatedTime))));
						targetFont->OutNext("%s", *g_pStringTable->translate(others_inv_owner->Name()));
						targetFont->OutNext("%s", *g_pStringTable->translate(others_inv_owner->CharacterInfo().Community().id()));
					}
				}
				accumulatedTime += SHOW_INFO_SPEED * Device.fTimeDelta;
			}
			else if (l_pI && our_inv_owner && PP.RQ.range < PICKUP_DISTANCE && l_pI->NameItem() && l_pI->CanTake())
			{
				targetFont->SetColor(subst_alpha(C, static_cast<u32>(lerp(0.f, 255.f, accumulatedTime))));
				targetFont->OutNext("%s", l_pI->NameItem());

				accumulatedTime += SHOW_INFO_SPEED * Device.fTimeDelta;
			}
		}
		else
		{
			accumulatedTime -= HIDE_INFO_SPEED * Device.fTimeDelta;
		}
		clamp(accumulatedTime,0.f,1.f);
	}

	// targetFont->OutSetI(0.f, .033f);
}

void CHUDTarget::net_Relcase(CObject* O)
{
	if(PP.RQ.O == O)
		PP.RQ.O = nullptr;

	RQR.r_clear	();
}
