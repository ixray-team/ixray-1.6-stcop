#include "StdAfx.h"
#include "WeaponAmmo.h"
#include "../xrPhysics/PhysicsShell.h"
#include "xrServer_Objects_ALife_Items.h"
#include "Actor_Flags.h"
#include "Inventory.h"
#include "Weapon.h"
#include "Level_Bullet_Manager.h"
#include "ai_space.h"
#include "../xrEngine/GameMtlLib.h"
#include "Level.h"
#include "../xrEngine/string_table.h"

#define BULLET_MANAGER_SECTION "bullet_manager"

CCartridge::CCartridge() 
{
	m_flags.assign			(cfTracer | cfRicochet);
	m_ammoSect = nullptr;
	param_s.Init();
	bullet_material_idx = u16(-1);
}

void CCartridge::Load(const char* section, u8 LocalAmmoType) 
{
	m_ammoSect				= section;
	m_LocalAmmoType			= LocalAmmoType;
	param_s.kDist				= pSettings->r_float(section, "k_dist");
	param_s.kDisp				= pSettings->r_float(section, "k_disp");
	param_s.kHit				= pSettings->r_float(section, "k_hit");
	param_s.kImpulse			= pSettings->r_float(section, "k_impulse");

	if (pSettings->line_exist(section, "k_ap"))
	{
		param_s.kAP = pSettings->r_float(section, "k_ap");
	}
	else
	{
		param_s.kAP = pSettings->r_float(section, "k_pierce");
		param_s.kAP -= 1.0f;

		if (param_s.kAP < 0.f)
			param_s.kAP = 0.f;
	}
	param_s.u8ColorID			= READ_IF_EXISTS(pSettings, r_u8, section, "tracer_color_ID", 0);
	
	if (pSettings->line_exist(section, "k_air_resistance"))
		param_s.kAirRes			=  pSettings->r_float(section, "k_air_resistance");
	else
		param_s.kAirRes			= pSettings->r_float(BULLET_MANAGER_SECTION, "air_resistance_k");

	m_flags.set					(cfTracer, pSettings->r_bool(section, "tracer"));
	param_s.buckShot			= pSettings->r_s32(  section, "buck_shot");
	param_s.impair				= pSettings->r_float(section, "impair");
	param_s.fWallmarkSize		= pSettings->r_float(section, "wm_size");
	
	m_flags.set					(cfCanBeUnlimited | cfRicochet, true);
	m_flags.set					(cfMagneticBeam, false);

	if (pSettings->line_exist(section, "allow_ricochet"))
	{
		if (!pSettings->r_bool(section, "allow_ricochet"))
			m_flags.set(cfRicochet, false);
	}
	if (pSettings->line_exist(section, "magnetic_beam_shot"))
	{
		if (pSettings->r_bool(section, "magnetic_beam_shot"))
			m_flags.set(cfMagneticBeam, true);
	}

	if (pSettings->line_exist(section, "4to1_tracer"))
		m_4to1_tracer = !!pSettings->r_bool(section, "4to1_tracer");;

	if(pSettings->line_exist(section,"can_be_unlimited"))
		m_flags.set(cfCanBeUnlimited, pSettings->r_bool(section, "can_be_unlimited"));

	m_flags.set			(cfExplosive, READ_IF_EXISTS(pSettings, r_bool, section, "explosive", false));

	bullet_material_idx		=  GMLib.GetMaterialIdx(WEAPON_MATERIAL_NAME);
	VERIFY	(u16(-1)!=bullet_material_idx);
	VERIFY	(param_s.fWallmarkSize>0);

	m_InvShortName			= g_pStringTable->translate( pSettings->r_string(section, "inv_name_short"));
}

float CCartridge::Weight() const {
	auto s = m_ammoSect.c_str();
	float res = 0;
	if (s) {
		float box = pSettings->r_float(s, "box_size");
		if (box > 0) {
			float w = pSettings->r_float(s, "inv_weight");
			res = w / box;
		}
	}
	return res;
}

void CWeaponAmmo::Load(const char* section) 
{
	inherited::Load			(section);

	cartridge_param.kDist		= pSettings->r_float(section, "k_dist");
	cartridge_param.kDisp		= pSettings->r_float(section, "k_disp");
	cartridge_param.kHit		= pSettings->r_float(section, "k_hit");
	cartridge_param.kImpulse	= pSettings->r_float(section, "k_impulse");

	if (pSettings->line_exist(section, "k_ap"))
	{
		cartridge_param.kAP = pSettings->r_float(section, "k_ap");
	}
	else
	{
		cartridge_param.kAP = pSettings->r_float(section, "k_pierce");
		cartridge_param.kAP -= 1.0f;

		if (cartridge_param.kAP < 0.f)
			cartridge_param.kAP = 0.f;
	}

	cartridge_param.u8ColorID	= READ_IF_EXISTS(pSettings, r_u8, section, "tracer_color_ID", 0);

	if (pSettings->line_exist(section, "k_air_resistance"))
		cartridge_param.kAirRes		= pSettings->r_float(section, "k_air_resistance");
	else
		cartridge_param.kAirRes		= pSettings->r_float(BULLET_MANAGER_SECTION, "air_resistance_k");
	m_tracer				= !!pSettings->r_bool(section, "tracer");

	if (pSettings->line_exist(section, "4to1_tracer"))
		m_4to1_tracer = !!pSettings->r_bool(section, "4to1_tracer");;

	cartridge_param.buckShot		= pSettings->r_s32(  section, "buck_shot");
	cartridge_param.impair			= pSettings->r_float(section, "impair");
	cartridge_param.fWallmarkSize	= pSettings->r_float(section, "wm_size");
	R_ASSERT				(cartridge_param.fWallmarkSize>0);

	m_boxSize				= (u16)pSettings->r_s32(section, "box_size");
	m_boxCurr				= m_boxSize;	
}

bool CWeaponAmmo::net_Spawn(CSE_Abstract* DC) 
{
	bool bResult			= inherited::net_Spawn	(DC);
	CSE_Abstract	*e		= (CSE_Abstract*)(DC);
	CSE_ALifeItemAmmo* l_pW	= smart_cast<CSE_ALifeItemAmmo*>(e);
	m_boxCurr				= l_pW->a_elapsed;
	
	if(m_boxCurr > m_boxSize)
		l_pW->a_elapsed		= m_boxCurr = m_boxSize;

	return					bResult;
}

void CWeaponAmmo::net_Destroy() 
{
	inherited::net_Destroy	();
}

void CWeaponAmmo::OnH_B_Chield() 
{
	inherited::OnH_B_Chield	();
}

void CWeaponAmmo::OnH_B_Independent(bool just_before_destroy) 
{
	if(!Useful()) {
		
		if (Local()){
			DestroyObject	();
		}
		m_ready_to_destroy	= true;
	}
	inherited::OnH_B_Independent(just_before_destroy);
}


bool CWeaponAmmo::Useful() const
{
	// Если IItem еще не полностью использованый, вернуть true
	return !!m_boxCurr;
}

bool CWeaponAmmo::Get(CCartridge &cartridge) 
{
	if(!m_boxCurr) return false;
	cartridge.m_ammoSect = cNameSect();
	
	cartridge.param_s = cartridge_param;

	cartridge.m_flags.set(CCartridge::cfTracer ,m_tracer);
	cartridge.m_4to1_tracer = m_4to1_tracer;
	cartridge.bullet_material_idx = GMLib.GetMaterialIdx(WEAPON_MATERIAL_NAME);
	cartridge.m_InvShortName = NameShort();
	--m_boxCurr;
	if(m_pInventory)m_pInventory->InvalidateState();
	return true;
}

bool CWeaponAmmo::Repack(PIItem Other)
{
	CWeaponAmmo* OtherCasted = smart_cast<CWeaponAmmo*>(Other);
	VERIFY(OtherCasted);
	if (OtherCasted->m_boxCurr == OtherCasted->m_boxSize)
	{
		return true;
	}
	u32 Sum = OtherCasted->m_boxCurr + m_boxCurr;
	if (Sum > OtherCasted->m_boxSize)
	{
		m_boxCurr = Sum - OtherCasted->m_boxSize;
		OtherCasted->m_boxCurr = OtherCasted->m_boxSize;
		return true;
	}
	OtherCasted->m_boxCurr = Sum;
	if (OnServer()) {
		SetDropManual(TRUE);
	}
	return false;
}

bool CWeaponAmmo::IsValid() const
{
	return m_boxCurr;
}

void CWeaponAmmo::renderable_Render() 
{
	if(!m_ready_to_destroy)
		inherited::renderable_Render();
}

void CWeaponAmmo::UpdateCL() 
{
	PROF_EVENT("CWeaponAmmo::UpdateCL")
	VERIFY2								(_valid(renderable.xform),*cName());
	inherited::UpdateCL	();
	VERIFY2								(_valid(renderable.xform),*cName());
	
	if(!IsGameTypeSingle())
		make_Interpolation	();

	VERIFY2								(_valid(renderable.xform),*cName());

}

void CWeaponAmmo::net_Export(NET_Packet& P) 
{
	inherited::net_Export	(P);
	
	P.w_u16					(m_boxCurr);
}

void CWeaponAmmo::net_Import(NET_Packet& P) 
{
	inherited::net_Import	(P);

	P.r_u16					(m_boxCurr);
}

CInventoryItem* CWeaponAmmo::can_make_killing(const CInventory* inventory) const
{
	VERIFY(inventory);

	for (const PIItem item : inventory->m_all)
	{
		CWeapon* weapon = item->cast_weapon();

		if (!weapon)
		{
			continue;
		}

		xr_vector<shared_str>::const_iterator i = std::find(weapon->m_ammoTypes.begin(), weapon->m_ammoTypes.end(), cNameSect());
		if (i != weapon->m_ammoTypes.end())
		{
			return weapon;
		}
	}

	return 0;
}

float CWeaponAmmo::Weight() const
{
	if (m_boxSize > 0) {
		float res = inherited::Weight();
		res *= (float)m_boxCurr / (float)m_boxSize;
		return res;
	}
	return 0.f;
}

u32 CWeaponAmmo::Cost() const
{
	u32 res = inherited::Cost();

	res = iFloor(res*(float)m_boxCurr/(float)m_boxSize+0.5f);

	return res;
}
