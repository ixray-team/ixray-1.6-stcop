#include "StdAfx.h"
#include "Torch.h"
#include "Entity.h"
#include "Actor.h"
#include "../xrEngine/LightAnimLibrary.h"
#include "../xrPhysics/PhysicsShell.h"
#include "xrServer_Objects_ALife_Items.h"
#include "../xrSound/ai_sounds.h"

#include "Level.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrEngine/CameraBase.h"
#include "../xrEngine/xr_collide_form.h"
#include "Inventory.h"
#include "game_base_space.h"

static const float		TORCH_INERTION_CLAMP		= PI_DIV_6;
static const float		TORCH_INERTION_SPEED_MAX	= 7.5f;
static const float		TORCH_INERTION_SPEED_MIN	= 0.5f;
static		 Fvector	TORCH_OFFSET				= {-0.2f,+0.1f,-0.3f};
static const Fvector	OMNI_OFFSET					= {-0.2f,+0.1f,-0.1f};
static const float		OPTIMIZATION_DISTANCE		= 100.f;

static bool stalker_use_dynamic_lights	= false;

ENGINE_API int g_current_renderer;

CTorch::CTorch(void) 
{
	light_render				= ::Render->light_create();
	light_render->set_type		(IRender_Light::SPOT);
	light_render->set_shadow	(true);
	light_omni					= ::Render->light_create();
	light_omni->set_type		(IRender_Light::POINT);
	light_omni->set_shadow		(!!psDeviceFlags.test(rsR4));

	m_switched_on				= false;
	glow_render					= ::Render->glow_create();
	lanim						= 0;
	fBrightness					= 1.f;

	m_prev_hp.set				(0,0);
	m_delta_h					= 0;

	// Disabling shift by x and z axes for 1st render, 
	// because we don't have dynamic lighting in it. 
	if( g_current_renderer == 1 )
	{
		TORCH_OFFSET.x = 0;
		TORCH_OFFSET.z = 0;
	}
}

CTorch::~CTorch() 
{
	light_render.destroy	();
	light_omni.destroy		();
	glow_render.destroy		();
}

inline bool CTorch::can_use_dynamic_lights()
{
	if (!H_Parent())
	{
		return true;
	}

	CInventoryOwner* owner = H_Parent()->cast_inventory_owner();
	if (!owner)
	{
		return true;
	}

	return owner->can_use_dynamic_lights();
}

void CTorch::Load(LPCSTR section) 
{
	inherited::Load			(section);
	light_trace_bone		= pSettings->r_string(section,"light_trace_bone");

	if (pSettings->line_exist(section, "sound_activate"))
	{
		m_sounds.LoadSound(section, "sound_activate", "soundActivate", false, SOUND_TYPE_ITEM_USING);
	}
	if (pSettings->line_exist(section, "sound_deactivate"))
	{
		m_sounds.LoadSound(section, "sound_deactivate", "soundDeactivate", false, SOUND_TYPE_ITEM_USING);
	}
}

void CTorch::Switch()
{
	if (OnClient())			return;
	bool bActive			= !m_switched_on;
	Switch					(bActive);
	CActor* pA = H_Parent()->cast_actor();

	if (pA != nullptr)
	{
		if (!m_switched_on)
		{
			if (m_sounds.FindSoundItem("soundActivate", false))
				m_sounds.PlaySound("soundActivate", pA->Position(), nullptr, !!pA->HUDview());
		}
		else if (m_switched_on)
		{
			if (m_sounds.FindSoundItem("soundDeactivate", false))
				m_sounds.PlaySound("soundDeactivate", pA->Position(), nullptr, !!pA->HUDview());
		}
	}
}

void CTorch::Switch(bool light_on)
{
	m_switched_on			= light_on;

	if (can_use_dynamic_lights())
	{
		light_render->set_active(light_on);
		
		if(light_on && H_Parent() && H_Parent()->cast_actor())
		{
			m_prev_hp.x = -H_Parent()->cast_actor()->cam_Active()->yaw;
			m_prev_hp.y = -H_Parent()->cast_actor()->cam_Active()->pitch;
		}
		light_omni->set_decor_object(H_Parent());
		light_omni->set_active(light_on);
	}
	glow_render->set_active					(light_on);

	if (*light_trace_bone) 
	{
		IKinematics* pVisual				= PKinematics(Visual());
		VERIFY(pVisual);
		u16 bi								= pVisual->LL_BoneID(light_trace_bone);

		pVisual->LL_SetBoneVisible			(bi,	light_on,	TRUE);
		pVisual->CalculateBones				(TRUE);
	}
}
bool CTorch::torch_active					() const
{
	return (m_switched_on);
}

BOOL CTorch::net_Spawn(CSE_Abstract* DC) 
{
	CSE_Abstract			*e	= (CSE_Abstract*)(DC);
	CSE_ALifeItemTorch		*torch	= smart_cast<CSE_ALifeItemTorch*>(e);
	R_ASSERT				(torch);
	cNameVisual_set			(torch->get_visual());

	R_ASSERT				(!CFORM());
	R_ASSERT				(PKinematics(Visual()));
	collidable.model		= new CCF_Skeleton	(this);

	if (!inherited::net_Spawn(DC))
		return				(FALSE);
	
	bool b_r2				= !!psDeviceFlags.test(rsR2);
	b_r2					|= !!psDeviceFlags.test(rsR4);

	IKinematics* K			= PKinematics(Visual());
	CInifile* pUserData		= K->LL_UserData(); 

	if (pUserData != nullptr)
	{
		R_ASSERT3(pUserData, "Empty Torch user data!", torch->get_visual());
		lanim = LALib.FindItem(pUserData->r_string("torch_definition", "color_animator"));
		guid_bone = K->LL_BoneID(pUserData->r_string("torch_definition", "guide_bone"));	VERIFY(guid_bone != BI_NONE);

		Fcolor clr = pUserData->r_fcolor("torch_definition", (b_r2) ? "color_r2" : "color");
		fBrightness = clr.intensity();
		float range = pUserData->r_float("torch_definition", (b_r2) ? "range_r2" : "range");
		light_render->set_color(clr);
		light_render->set_range(range);

		Fcolor clr_o = pUserData->r_fcolor("torch_definition", (b_r2) ? "omni_color_r2" : "omni_color");
		float range_o = pUserData->r_float("torch_definition", (b_r2) ? "omni_range_r2" : "omni_range");
		light_omni->set_color(clr_o);
		light_omni->set_range(range_o);

		light_render->set_cone(deg2rad(pUserData->r_float("torch_definition", "spot_angle")));
		light_render->set_texture(pUserData->r_string("torch_definition", "spot_texture"));

		glow_render->set_texture(pUserData->r_string("torch_definition", "glow_texture"));
		glow_render->set_color(clr);
		glow_render->set_radius(pUserData->r_float("torch_definition", "glow_radius"));

		//включить/выключить фонарик
		Switch(torch->m_active);
		VERIFY(!torch->m_active || (torch->ID_Parent != 0xffff));

		m_delta_h = PI_DIV_2 - atan((range * 0.5f) / _abs(TORCH_OFFSET.x));
	}

	return					(TRUE);
}

void CTorch::net_Destroy() 
{
	Switch(false);

	inherited::net_Destroy	();
}

void CTorch::OnH_A_Chield() 
{
	inherited::OnH_A_Chield			();
	m_focus.set						(Position());
}

void CTorch::OnH_B_Independent(bool just_before_destroy) 
{
	inherited::OnH_B_Independent(just_before_destroy);

	Switch						(false);

	m_sounds.StopAllSounds();
}
static void dbg_text_renderer(const Fvector& pos, u32 color = color_rgba(0,255,100,255), shared_str str = "+")
{
    Fvector4		v_res;
    Device.mFullTransform.transform(v_res, pos);

    float x = (1.f + v_res.x) / 2.f * (Device.Width);
    float y = (1.f - v_res.y) / 2.f * (Device.Height);

    if (v_res.z < 0 || v_res.w < 0)
        return;

    if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y>1.f)
        return;

	g_FontManager->pFontSystem->SetAligment(CGameFont::alCenter);
	g_FontManager->pFontSystem->SetColor(color);
	g_FontManager->pFontSystem->Out(x, y, "%s", str.c_str());
}

void CTorch::Update()
{
	if (!m_switched_on)			return;

	Fmatrix M = PKinematics(Visual())->LL_GetTransform(guid_bone);

	if (H_Parent())
	{
		if (!AlwaysTheCrow())
			MakeMeCrow();

		CActor* actor = H_Parent()->cast_actor();
		//if (actor)		PKinematics(H_Parent()->Visual())->CalculateBones_Invalidate();

		if ((H_Parent()->XFORM().c.distance_to_sqr(Device.vCameraPosition) < _sqr(OPTIMIZATION_DISTANCE) || GameID() != eGameIDSingle))
		{
			// near camera
			//PKinematics(H_Parent()->Visual())->CalculateBones
			//(
			//	!!Render->ViewBase.testSphere_dirty(H_Parent()->SpatialComponent->spatial.sphere.P,
			//		H_Parent()->SpatialComponent->spatial.sphere.R + SpatialComponent->spatial.sphere.R + light_render->get_homdata().sphere.R)
			//);
			if(actor && actor->HUDview())
				PKinematics(H_Parent()->Visual())->CalculateBones(TRUE);

			M.mulA_43(XFORM());
		}
		else
		{
			// approximately the same
			M = H_Parent()->XFORM();
			H_Parent()->Center(M.c);
			M.c.y += H_Parent()->Radius() * 2.f / 3.f;
		}

		if (actor)
		{
			if (actor->HUDview())
			{
				m_prev_hp.x = angle_inertion_var(m_prev_hp.x, -actor->cam_Active()->yaw, TORCH_INERTION_SPEED_MIN, TORCH_INERTION_SPEED_MAX, TORCH_INERTION_CLAMP, Device.fTimeDelta);
				m_prev_hp.y = angle_inertion_var(m_prev_hp.y, -actor->cam_Active()->pitch, TORCH_INERTION_SPEED_MIN, TORCH_INERTION_SPEED_MAX, TORCH_INERTION_CLAMP, Device.fTimeDelta);

				light_render->set_ignore_object(H_Parent());
				light_omni->set_ignore_object(H_Parent());
				light_omni->set_decor_object(nullptr);

				Fvector			dir, right, up;
				dir.setHP(m_prev_hp.x + m_delta_h, m_prev_hp.y);
				Fvector::generate_orthonormal_basis_normalized(dir, up, right);

				Fvector offset = M.c;
				offset.mad(M.i, TORCH_OFFSET.x);
				offset.mad(M.j, TORCH_OFFSET.y);
				offset.mad(M.k, TORCH_OFFSET.z);
				light_render->set_position(offset);

				offset = M.c;
				offset.mad(M.i, OMNI_OFFSET.x);
				offset.mad(M.j, OMNI_OFFSET.y);
				offset.mad(M.k, OMNI_OFFSET.z);
				light_omni->set_position(offset);

				light_render->set_rotation(dir, right);
			}
			else
			{
				light_render->set_ignore_object(nullptr);
				light_render->set_position(M.c);
				light_render->set_rotation(M.k, M.i);

				light_omni->set_ignore_object(nullptr);
				light_omni->set_decor_object(H_Parent());
				light_omni->set_position(M.c);
			}

			glow_render->set_direction(M.k);
			glow_render->set_position(M.c);
		}
		else
		{
			if (can_use_dynamic_lights())
			{
				light_render->set_position(M.c);
				light_render->set_rotation(M.k, M.i);

				Fvector offset = M.c;
				offset.mad(M.i, OMNI_OFFSET.x);
				offset.mad(M.j, OMNI_OFFSET.y);
				offset.mad(M.k, OMNI_OFFSET.z);
				light_omni->set_position(M.c);
				light_omni->set_rotation(M.k, M.i);
			}

			glow_render->set_position(M.c);
			glow_render->set_direction(M.k);
		}
	}
	else
	{
		if (getVisible() && m_pPhysicsShell)
		{
			M.mulA_43(XFORM());

			m_switched_on = false;
			light_render->set_active(false);
			light_omni->set_active(false);
			glow_render->set_active(false);
		}
	}

	if (!m_switched_on)					return;

	// calc color animator
	if (!lanim)							return;

	int						frame;
	// возвращает в формате BGR
	u32 clr = lanim->CalculateBGR(Device.fTimeGlobal, frame);

	Fcolor					fclr;
	fclr.set((float)color_get_B(clr), (float)color_get_G(clr), (float)color_get_R(clr), 1.f);
	fclr.mul_rgb(fBrightness / 255.f);
	if (can_use_dynamic_lights())
	{
		light_render->set_color(fclr);
		light_omni->set_color(fclr);
	}
	glow_render->set_color(fclr);
}

void CTorch::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);
	//Update();

	if (H_Parent() && m_switched_on && !AlwaysTheCrow() && enabled())
	{
		MakeMeCrow();
		//dbg_text_renderer(XFORM().c);
	}
}

void CTorch::UpdateCL() 
{
	PROF_EVENT("CTorch::UpdateCL")
	inherited::UpdateCL			();
	
	Update();
}


void CTorch::create_physic_shell()
{
	CPhysicsShellHolder::create_physic_shell();
}

void CTorch::activate_physic_shell()
{
	CPhysicsShellHolder::activate_physic_shell();
}

void CTorch::setup_physic_shell()
{
	CPhysicsShellHolder::setup_physic_shell();
}

void CTorch::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);

	BYTE F = 0;
	F |= (m_switched_on ? eTorchActive : 0);
	const CActor* pA = H_Parent() ? H_Parent()->cast_actor() : nullptr;
	if (pA)
	{
		if (pA->attached(this))
		{
			F |= eAttached;
		}
	}

	P.w_u8(F);
}

void CTorch::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
	
	BYTE F = P.r_u8();
	bool new_m_switched_on = !!(F & eTorchActive);

	if (new_m_switched_on != m_switched_on)
		Switch(new_m_switched_on);
}
bool  CTorch::can_be_attached		() const
{
	const CActor *pA = smart_cast<const CActor *>(H_Parent());
	if (pA)
	{
		return pA->inventory().InSlot(this);
	}
	else
	{
		return true;
	}
}

void CTorch::afterDetach()
{
	inherited::afterDetach();
	Switch(false);
}

void CTorch::renderable_Render(IDSGraphManager* DM)
{
	if (IsGameTypeSingle())
	{
		inherited::renderable_Render(DM);
	}
	else
	{
		CActor* pActor = H_Parent() ? H_Parent()->cast_actor() : nullptr;
		if (m_switched_on && pActor)
			inherited::renderable_Render(DM);
	}
}

void CTorch::enable(bool value)
{
	inherited::enable(value);

	if (!enabled() && m_switched_on)
		Switch(false);

}