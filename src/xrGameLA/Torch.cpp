#include "stdafx.h"
#include "torch.h"
#include "entity.h"
#include "actor.h"
#include "../LightAnimLibrary.h"
#include "PhysicsShell.h"
#include "xrserver_objects_alife_items.h"
#include "../xrSound/ai_sounds.h"
#include "HUDManager.h"
#include "level.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../camerabase.h"
#include "inventory.h"
#include "game_base_space.h"
#include "UIGameCustom.h"
#include "actorEffector.h"
#include "../xr_collide_form.h"
#include "CustomOutfit.h"

static const float		TIME_2_HIDE					= 5.f;
static const float		TORCH_INERTION_CLAMP		= PI_DIV_6;
static const float		TORCH_INERTION_SPEED_MAX	= 7.5f;
static const float		TORCH_INERTION_SPEED_MIN	= 0.5f;
static		 Fvector	TORCH_OFFSET				= {-0.2f,+0.1f,-0.3f};
static const Fvector	OMNI_OFFSET					= {-0.2f,+0.1f,-0.1f};
static const float		OPTIMIZATION_DISTANCE		= 100.f;

static bool stalker_use_dynamic_lights	= false;

ENGINE_API int g_current_renderer;

CTorch::CTorch(void) : m_current_battery_state(0)
{
	light_render				= ::Render->light_create();
	light_render->set_type		(IRender_Light::SPOT);
	light_render->set_shadow	(true);
	light_omni					= ::Render->light_create();
	light_omni->set_type		(IRender_Light::POINT);
	light_omni->set_shadow		(false);

	m_switched_on				= false;
	glow_render					= ::Render->glow_create();
	lanim						= 0;
	time2hide					= 0;
	fBrightness					= 1.f;

	m_RangeMax					= 20.f;
	m_RangeCurve				= 20.f;

	m_prev_hp.set				(0,0);
	m_delta_h					= 0;
	m_night_vision				= NULL;

	// Disabling shift by x and z axes for 1st render, 
	// because we don't have dynamic lighting in it. 
	if( g_current_renderer == 1 )
	{
		TORCH_OFFSET.x = 0;
		TORCH_OFFSET.z = 0;
	}

#pragma todo("lets skip AlifeItem stuff")
	m_battery_state = 0.f;
	fUnchanreRate = 0.f;
}

CTorch::~CTorch(void) 
{
	light_render.destroy	();
	light_omni.destroy	();
	glow_render.destroy		();
	HUD_SOUND_ITEM::DestroySound	(m_FlashlightSwitchSnd);
	sndBreaking.destroy();
	xr_delete				(m_night_vision);
}

inline bool CTorch::can_use_dynamic_lights	()
{
	if (!H_Parent())
		return				(true);

	CInventoryOwner			*owner = smart_cast<CInventoryOwner*>(H_Parent());
	if (!owner)
		return				(true);

	return					(owner->can_use_dynamic_lights());
}

void CTorch::Load(LPCSTR section) 
{
	inherited::Load			(section);
	light_trace_bone		= pSettings->r_string(section,"light_trace_bone");
	HUD_SOUND_ITEM::LoadSound(section, "snd_flashlight_switch_on", m_FlashlightSwitchSnd, SOUND_TYPE_ITEM_USING);
	m_battery_duration		= pSettings->r_u32(section, "battery_duration");
	m_bNightVisionEnabled = !!pSettings->r_bool(section,"night_vision");
	if(pSettings->line_exist(section, "break_sound"))
		sndBreaking.create(pSettings->r_string(section, "break_sound"),st_Effect,sg_SourceType);

	Settings_from_ltx = READ_IF_EXISTS(pSettings, r_u8, section, "settings_from_ltx", 0);

	//tatarinrafa: Добавил возможность прописать это в конфиге
	if (Settings_from_ltx == 1)
	{
		LoadLightSettings(pSettings, section);
	}

	fUnchanreRate = READ_IF_EXISTS(pSettings, r_float, section, "uncharge_rate", 20.f);
	//lets give some more realistics to batteries. Now found tourches will have 70 to 100 precent battery status 
	float rondo = ::Random.randF(0.7f, 1.0f);
	m_battery_state = m_battery_duration * rondo;
}

void CTorch::LoadLightSettings(CInifile* ini, LPCSTR section)
{
	bool isR2 = !!psDeviceFlags.test(rsR2) || !!psDeviceFlags.test(rsR4);

	IKinematics* K = smart_cast<IKinematics*>(Visual());
	lanim = LALib.FindItem(READ_IF_EXISTS(ini, r_string, section, "color_animator", "nill"));
	guid_bone = K->LL_BoneID(ini->r_string(section, "guide_bone"));
	VERIFY(guid_bone != BI_NONE);

	Fcolor clr = ini->r_fcolor(section, (isR2) ? "color_r2" : "color");
	fBrightness = clr.intensity();
	m_RangeMax = ini->r_float(section, (isR2) ? "range_max_r2" : "range_max");
	// m_RangeMin = ini->r_float(section, (isR2) ? "range_min_r2" : "range_min");
	m_RangeCurve = ini->r_float(section, "range_curve");

	light_render->set_color(clr);
	light_render->set_range(m_RangeMax);

	Fcolor clr_o = ini->r_fcolor(section, (isR2) ? "omni_color_r2" : "omni_color");
	float range_o = ini->r_float(section, (isR2) ? "omni_range_r2" : "omni_range");
	light_omni->set_color(clr_o);
	light_omni->set_range(range_o);

	light_render->set_cone(deg2rad(ini->r_float(section, "spot_angle")));
	light_render->set_texture(ini->r_string(section, "spot_texture"));

	glow_render->set_texture(ini->r_string(section, "glow_texture"));
	glow_render->set_color(clr);
	glow_render->set_radius(ini->r_float(section, "glow_radius"));
}

void CTorch::Switch()
{
	if (OnClient()) return;
	bool bActive			= !m_switched_on;
	Switch					(bActive);
}

void CTorch::Broke()
{
	if (OnClient()) return;
	if (m_switched_on) 
	{
		sndBreaking.play_at_pos(0, Position(), false);
		Switch(false);
	}
}

void CTorch::SetBatteryStatus(u32 val)
{
	//Msg("SetBatteryStatus=[%f], [%d], [%d], [%d], [%d], [%d], [%f]", condition, m_current_battery_state, val, m_battery_duration, m_current_battery_state - val, (m_current_battery_state - val)/m_battery_duration*100, (m_current_battery_state - val)/m_battery_duration);
	m_battery_state = val;//m_current_battery_state = val;
	float condition = 1.f * m_battery_state / m_battery_duration;
	SetCondition(condition);
}

void CTorch::Switch	(bool light_on)
{
	CActor *pA = smart_cast<CActor *>(H_Parent());
	m_actor_item = (pA) ? true : false;
	if (light_on && m_battery_state < 0 && m_actor_item)//!m_current_battery_state && m_actor_item)
	{
		light_on = false;

		SDrawStaticStruct* s	= HUD().GetGameUI()->AddCustomStatic("torch_battery_low", true);
		s->m_endTime		= Device.fTimeGlobal+3.0f;// 3sec
	}
	m_switched_on			= light_on;
	if (can_use_dynamic_lights())
	{
		light_render->set_active(light_on);
		if(!pA)light_omni->set_active(light_on);
	}
	glow_render->set_active					(light_on);

	if (*light_trace_bone) 
	{
		IKinematics* pVisual				= smart_cast<IKinematics*>(Visual()); VERIFY(pVisual);
		u16 bi								= pVisual->LL_BoneID(light_trace_bone);

		pVisual->LL_SetBoneVisible			(bi,	light_on,	TRUE);
		pVisual->CalculateBones				();

		pVisual->LL_SetBoneVisible			(bi,	light_on,	TRUE); //hack
	}
	if (m_switched_on && m_actor_item)
		HUD_SOUND_ITEM::PlaySound(m_FlashlightSwitchSnd, pA->Position(), pA, true, false);
}

void CTorch::SwitchNightVision()
{
	if (OnClient()) return;
	SwitchNightVision(!m_bNightVisionOn);
}

void CTorch::SwitchNightVision(bool vision_on, bool use_sounds)
{
	if(!m_bNightVisionEnabled) return;
	
	m_bNightVisionOn			= vision_on;

	CActor *pA = smart_cast<CActor *>(H_Parent());
	if(!pA)						return;
	
	if(!m_night_vision)
		m_night_vision			= new CNightVisionEffector(cNameSect());


	LPCSTR disabled_names	= pSettings->r_string(cNameSect(),"disabled_maps");
	LPCSTR curr_map			= *Level().name();
	u32 cnt					= _GetItemCount(disabled_names);
	bool b_allow			= true;
	string512				tmp;
	for(u32 i=0; i<cnt;++i){
		_GetItem(disabled_names, i, tmp);
		if(0==stricmp(tmp, curr_map)){
			b_allow = false;
			break;
		}
	}

//	CHelmet* pHelmet	= smart_cast<CHelmet*>(pA->inventory().ItemFromSlot(HELMET_SLOT));
	CInventoryItem * itemfromhelmetslot = pA->inventory().ItemFromSlot(HELMET_SLOT);
	CInventoryItem * itemfromPNVslot = pA->inventory().ItemFromSlot(PNV_SLOT);
	CCustomOutfit* pOutfit	= smart_cast<CCustomOutfit*>(pA->inventory().ItemFromSlot(OUTFIT_SLOT));

	/*if(pHelmet && pHelmet->m_NightVisionSect.size() && !b_allow)
	{
		m_night_vision->OnDisabled(pA, use_sounds);
		return;
	}
	else*/ 
	if (pOutfit && pOutfit->m_NightVisionSect.size() && !b_allow)
	{

		m_night_vision->OnDisabled(pA, use_sounds);

		return;
	}
	if (itemfromPNVslot && itemfromPNVslot->m_SectNightVision.size() && !b_allow)
	{

		m_night_vision->OnDisabled(pA, use_sounds);

		return;
	}
	if (itemfromhelmetslot && itemfromhelmetslot->m_SectNightVision.size() && !b_allow)
	{

		m_night_vision->OnDisabled(pA, use_sounds);

		return;
	}

	bool bIsActiveNow = m_night_vision->IsActive();

	if(m_bNightVisionOn)
	{

		if(!bIsActiveNow)
		{
			/*if(pHelmet && pHelmet->m_NightVisionSect.size())
			{
				m_night_vision->Start(pHelmet->m_NightVisionSect, pA, use_sounds);
				return;
			}
			else*/ 
			if(pOutfit && pOutfit->m_NightVisionSect.size())
			{
				m_night_vision->Start(pOutfit->m_NightVisionSect, pA, use_sounds);


				CurrentNightVisionItem = pA->inventory().ItemFromSlot(OUTFIT_SLOT);
				return;
			}

		//tatarinrafa: Добавил пнв для шлемов и слота ПНВ(набудущее)
			else if (itemfromhelmetslot && itemfromhelmetslot->m_SectNightVision.size())
			{
				m_night_vision->Start(itemfromhelmetslot->m_SectNightVision, pA, use_sounds);


				CurrentNightVisionItem = itemfromhelmetslot;
				return;
			}

			else if (itemfromPNVslot && itemfromPNVslot->m_SectNightVision.size())
			{
				m_night_vision->Start(itemfromPNVslot->m_SectNightVision, pA, use_sounds);


				CurrentNightVisionItem = itemfromPNVslot;
				return;
			}

			m_bNightVisionOn = false; // in case if there is no nightvision in helmet and outfit
		}
	}else
	{
		if(bIsActiveNow)
		{
			m_night_vision->Stop(100000.0f, use_sounds);
			CurrentNightVisionItem = NULL;
		}
	}
}


BOOL CTorch::net_Spawn(CSE_Abstract* DC) 
{
	CSE_Abstract			*e	= (CSE_Abstract*)(DC);
	CSE_ALifeItemTorch		*torch	= smart_cast<CSE_ALifeItemTorch*>(e);
	R_ASSERT				(torch);
	cNameVisual_set			(torch->get_visual());

	R_ASSERT				(!CFORM());
	R_ASSERT				(smart_cast<IKinematics*>(Visual()));
	collidable.model		= new CCF_Skeleton(this);

	if (!inherited::net_Spawn(DC))
		return				(FALSE);
	
	//tatarinrafa: Добавил возможность прописать это в конфиге
	if (Settings_from_ltx == 0)
	{
		IKinematics* K = smart_cast<IKinematics*>(Visual());
		CInifile* pUserData = K->LL_UserData();
		R_ASSERT3(pUserData, "Empty Torch user data!", torch->get_visual());

		LoadLightSettings(pUserData, "torch_definition");
	}

	//включить/выключить фонарик
	Switch					(torch->m_active);
	VERIFY					(!torch->m_active || (torch->ID_Parent != 0xffff));
	
	if(torch->ID_Parent == 0)		
		SwitchNightVision	(torch->m_nightvision_active, false);
	//else
	//	SwitchNightVision	(false, false);

	m_delta_h				= PI_DIV_2-atan((m_RangeMax*0.5f)/_abs(TORCH_OFFSET.x));

	m_current_battery_state = torch->m_battery_state;
	return					(TRUE);
}

void CTorch::net_Destroy() 
{
	Switch					(false);
	SwitchNightVision		(false);

	inherited::net_Destroy	();
}

void CTorch::OnH_A_Chield() 
{
	inherited::OnH_A_Chield			();
	m_focus.set						(Position());
}

void CTorch::OnH_B_Independent	(bool just_before_destroy) 
{
	inherited::OnH_B_Independent	(just_before_destroy);
	time2hide						= TIME_2_HIDE;

	Switch						(false);
}

void CTorch::Recharge(void)
{
	m_battery_state = m_battery_duration;
	SetCondition(1.f);
	//m_current_battery_state = m_battery_state;
}

void CTorch::UpdateBattery(void)
{
	if (m_switched_on)
	{
		//m_current_battery_state--;
		float minus = fUnchanreRate * Device.fTimeDelta;
		//Msg("UpdateBattery %f %f", m_battery_state, minus);
		m_battery_state -= minus;
		//Msg("UpdateBattery %f", m_battery_state);

		float condition = 1.f * m_battery_state / m_battery_duration;
		SetCondition(condition);
		//Msg("UpdateBattery condition %f", condition);

		float rangeCoef = atan(m_RangeCurve * m_battery_state / m_battery_duration) / PI_DIV_2;
		clamp(rangeCoef, 0.f, 1.f);
		float range = m_RangeMax * rangeCoef;
		// Msg("set_range [%f]", range);
		light_render->set_range	(range);
		m_delta_h	= PI_DIV_2-atan((range*0.5f)/_abs(TORCH_OFFSET.x));

		if (m_battery_state < 0)//!m_current_battery_state)
		{
			Switch(false);
			return;
		}
	}
}

void CTorch::UpdateCL() 
{
	inherited::UpdateCL			();

	if (m_night_vision && m_night_vision->IsActive())
	{
		if (CurrentNightVisionItem){
			CActor *pA = smart_cast<CActor *>(H_Parent());
			if (pA && CurrentNightVisionItem != pA->inventory().ItemFromSlot(PNV_SLOT) && CurrentNightVisionItem != pA->inventory().ItemFromSlot(HELMET_SLOT) && CurrentNightVisionItem != pA->inventory().ItemFromSlot(OUTFIT_SLOT)){
				CurrentNightVisionItem = NULL;
				SwitchNightVision();
			}
		}
	}
	if (!m_switched_on)		return;

	CBoneInstance			&BI = smart_cast<IKinematics*>(Visual())->LL_GetBoneInstance(guid_bone);
	Fmatrix					M;

	if (H_Parent()) 
	{
		CActor*			actor = smart_cast<CActor*>(H_Parent());
		if (actor)		smart_cast<IKinematics*>(H_Parent()->Visual())->CalculateBones_Invalidate	();

		if (H_Parent()->XFORM().c.distance_to_sqr(Device.vCameraPosition)<_sqr(OPTIMIZATION_DISTANCE) || GameID() != GAME_SINGLE) {
			// near camera
			smart_cast<IKinematics*>(H_Parent()->Visual())->CalculateBones	();
			M.mul_43				(XFORM(),BI.mTransform);
		} else {
			// approximately the same
			M		= H_Parent()->XFORM		();
			H_Parent()->Center				(M.c);
			M.c.y	+= H_Parent()->Radius	()*2.f/3.f;
		}

		if (actor) 
		{
			if (actor->IsLookAt())
			{
				m_prev_hp.x		= angle_inertion_var(m_prev_hp.x,-actor->cam_Active()->yaw,TORCH_INERTION_SPEED_MIN,TORCH_INERTION_SPEED_MAX,TORCH_INERTION_CLAMP,Device.fTimeDelta);
				m_prev_hp.y		= angle_inertion_var(m_prev_hp.y,-actor->cam_Active()->pitch,TORCH_INERTION_SPEED_MIN,TORCH_INERTION_SPEED_MAX,TORCH_INERTION_CLAMP,Device.fTimeDelta);
			} else {
				m_prev_hp.x		= angle_inertion_var(m_prev_hp.x,-actor->cam_FirstEye()->yaw,TORCH_INERTION_SPEED_MIN,TORCH_INERTION_SPEED_MAX,TORCH_INERTION_CLAMP,Device.fTimeDelta);
				m_prev_hp.y		= angle_inertion_var(m_prev_hp.y,-actor->cam_FirstEye()->pitch,TORCH_INERTION_SPEED_MIN,TORCH_INERTION_SPEED_MAX,TORCH_INERTION_CLAMP,Device.fTimeDelta);
			}

			Fvector			dir,right,up;	
			dir.setHP		(m_prev_hp.x+m_delta_h,m_prev_hp.y);
			Fvector::generate_orthonormal_basis_normalized(dir,up,right);


			if (true)
			{
				Fvector offset				= M.c; 
				offset.mad					(M.i,TORCH_OFFSET.x);
				offset.mad					(M.j,TORCH_OFFSET.y);
				offset.mad					(M.k,TORCH_OFFSET.z);
				light_render->set_position	(offset);

				if(true /*false*/)
				{
					offset						= M.c; 
					offset.mad					(M.i,OMNI_OFFSET.x);
					offset.mad					(M.j,OMNI_OFFSET.y);
					offset.mad					(M.k,OMNI_OFFSET.z);
					light_omni->set_position	(offset);
				}
			}//if (true)
			glow_render->set_position	(M.c);

			if (true)
			{
				light_render->set_rotation	(dir, right);
				
				if(true /*false*/)
				{
					light_omni->set_rotation	(dir, right);
				}
			}//if (true)
			glow_render->set_direction	(dir);

		}// if(actor)
		else 
		{
			if (can_use_dynamic_lights()) 
			{
				light_render->set_position	(M.c);
				light_render->set_rotation	(M.k,M.i);

				Fvector offset				= M.c; 
				offset.mad					(M.i,OMNI_OFFSET.x);
				offset.mad					(M.j,OMNI_OFFSET.y);
				offset.mad					(M.k,OMNI_OFFSET.z);
				light_omni->set_position	(M.c);
				light_omni->set_rotation	(M.k,M.i);
			}//if (can_use_dynamic_lights()) 

			glow_render->set_position	(M.c);
			glow_render->set_direction	(M.k);
		}
	}//if(HParent())
	else 
	{
		if (getVisible() && m_pPhysicsShell) 
		{
			M.mul						(XFORM(),BI.mTransform);

			m_switched_on			= false;
			light_render->set_active(false);
			light_omni->set_active(false);
			glow_render->set_active	(false);
		}//if (getVisible() && m_pPhysicsShell)  
	}

	if (!m_switched_on)					return;

	// calc color animator
	if (!lanim)							return;

	int						frame;
	// возвращает в формате BGR
	u32 clr					= lanim->CalculateBGR(Device.fTimeGlobal,frame); 

	Fcolor					fclr;
	fclr.set				((float)color_get_B(clr),(float)color_get_G(clr),(float)color_get_R(clr),1.f);
	fclr.mul_rgb			(fBrightness/255.f);
	if (can_use_dynamic_lights())
	{
		light_render->set_color	(fclr);
		light_omni->set_color	(fclr);
	}
	glow_render->set_color		(fclr);

}

void CTorch::create_physic_shell()
{
	CPhysicsShellHolder::create_physic_shell();
}

void CTorch::activate_physic_shell()
{
	CPhysicsShellHolder::activate_physic_shell();
}

void CTorch::setup_physic_shell	()
{
	CPhysicsShellHolder::setup_physic_shell();
}

void CTorch::net_Export			(NET_Packet& P)
{
	inherited::net_Export		(P);
//	P.w_u8						(m_switched_on ? 1 : 0);


	BYTE F = 0;
	F |= (m_switched_on ? eTorchActive : 0);
	F |= (m_bNightVisionOn ? eNightVisionActive : 0);
	const CActor *pA = smart_cast<const CActor *>(H_Parent());
	if (pA)
	{
		if (pA->attached(this))
			F |= eAttached;
	}
	P.w_u8(F);
	P.w_u16(m_current_battery_state);
}

void CTorch::net_Import			(NET_Packet& P)
{
	inherited::net_Import		(P);
	
	BYTE F = P.r_u8();
	bool new_m_switched_on				= !!(F & eTorchActive);
	bool new_m_bNightVisionOn			= !!(F & eNightVisionActive);

	if (new_m_switched_on != m_switched_on)			Switch						(new_m_switched_on);
	if (new_m_bNightVisionOn != m_bNightVisionOn)	
	{
//		Msg("CTorch::net_Import - NV[%d]", new_m_bNightVisionOn);

		const CActor *pA = smart_cast<const CActor *>(H_Parent());
		if (pA)
		{
			SwitchNightVision			(new_m_bNightVisionOn);
		}
	}

	m_current_battery_state = P.r_u16();
}


void CTorch::save(NET_Packet &output_packet)
{
	inherited::save(output_packet);
	save_data(m_battery_state, output_packet);

}

void CTorch::load(IReader &input_packet)
{
	inherited::load(input_packet);
	load_data(m_battery_state, input_packet);
}

bool CTorch::can_be_attached		() const
{
//	if( !inherited::can_be_attached() ) return false;

	const CActor *pA = smart_cast<const CActor *>(H_Parent());
	if (pA) 
	{
//		if(pA->inventory().Get(ID(), false))
		//check slot and belt
		bool b = false;
		int slot = BaseSlot();
		if (slot != NO_ACTIVE_SLOT)
			b = (const CTorch*)smart_cast<CTorch*>(pA->inventory().m_slots[slot].m_pIItem) == this;
		b = b || pA->inventory().InBelt(const_cast<CTorch*>(this));
		if(b)
			return true;
		else
			return false;
	}
	return true;
}
void CTorch::afterDetach			()
{
	inherited::afterDetach	();
	Switch					(false);
}

void CTorch::renderable_Render()
{
	inherited::renderable_Render();
}

u32 CTorch::GetBatteryLifetime() const
{
	return m_battery_duration;
}

u32 CTorch::GetBatteryStatus() const
{
	//return m_current_battery_state;
	return m_battery_state;
}

bool CTorch::IsSwitchedOn() const
{
	return m_switched_on;
}

CNightVisionEffector::CNightVisionEffector(const shared_str& section)
:m_pActor(NULL)
{
	m_sounds.LoadSound(section.c_str(),"snd_night_vision_on", "NightVisionOnSnd", false, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section.c_str(),"snd_night_vision_off", "NightVisionOffSnd", false, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section.c_str(),"snd_night_vision_idle", "NightVisionIdleSnd", false, SOUND_TYPE_ITEM_USING);
	m_sounds.LoadSound(section.c_str(),"snd_night_vision_broken", "NightVisionBrokenSnd", false, SOUND_TYPE_ITEM_USING);
}

void CNightVisionEffector::Start(const shared_str& sect, CActor* pA, bool play_sound)
{
	m_pActor			= pA;
	AddEffector			(m_pActor, effNightvision, sect);
	if(play_sound)
	{
		PlaySounds(eStartSound);
		PlaySounds(eIdleSound);
	}
}

void CNightVisionEffector::Stop(const float factor, bool play_sound)
{
	if(!m_pActor)		return;
	CEffectorPP* pp		= m_pActor->Cameras().GetPPEffector((EEffectorPPType)effNightvision);
	if(pp)
	{
		pp->Stop			(factor);
		if(play_sound)
			PlaySounds(eStopSound);

		m_sounds.StopSound("NightVisionIdleSnd");
	}
}

bool CNightVisionEffector::IsActive()
{
	//Msg("IsActive");
	if(!m_pActor)	return false;
	CEffectorPP* pp = m_pActor->Cameras().GetPPEffector((EEffectorPPType)effNightvision);
	return (pp!=NULL);
}

void CNightVisionEffector::OnDisabled(CActor* pA, bool play_sound)
{
	m_pActor					= pA;
	if(play_sound)
		PlaySounds(eBrokeSound);
}

void CNightVisionEffector::PlaySounds(EPlaySounds which)
{
	if(!m_pActor)
		return;

	bool bPlaySoundFirstPerson = !!m_pActor->HUDview();
	switch(which)
	{
	case eStartSound:
		{
			m_sounds.PlaySound("NightVisionOnSnd", m_pActor->Position(), NULL, bPlaySoundFirstPerson);
		}break;
	case eStopSound:
		{
			m_sounds.PlaySound("NightVisionOffSnd", m_pActor->Position(), NULL, bPlaySoundFirstPerson);
		}break;
	case eIdleSound:
		{
			m_sounds.PlaySound("NightVisionIdleSnd", m_pActor->Position(), NULL, bPlaySoundFirstPerson, true);
		}break;
	case eBrokeSound:
		{
			m_sounds.PlaySound("NightVisionBrokenSnd", m_pActor->Position(), NULL, bPlaySoundFirstPerson);
		}break;
	default: NODEFAULT;
	}
}
