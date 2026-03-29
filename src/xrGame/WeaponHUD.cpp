// WeaponHUD.cpp:	HUD для оружия и прочих предметов, которые
//					могут держать в руках персонажи, также используется
//					для синхронизации анимаций с видом от 3-го лица
//////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "WeaponHUD.h"
#include "Weapon.h"
#include "../Motion.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "level.h"
weapon_hud_container* g_pWeaponHUDContainer=0;

bool weapon_hud_value::load(const shared_str& section, CHudItem* owner)
{	
	// Geometry and transform
	Fvector						pos,ypr;
	pos							= pSettings->r_fvector3(section,"position");
	ypr							= pSettings->r_fvector3(section,"orientation");
	ypr.mul						(PI/180.f);

	m_offset.setHPB				(ypr.x,ypr.y,ypr.z);
	m_offset.translate_over		(pos);

	// Visual
	LPCSTR visual_name			= pSettings->r_string(section, "visual");
	m_animations				= ::Render->model_Create(visual_name)->dcast_PKinematicsAnimated();

	// fire bone	
	if(owner->cast_weapon())
	{
		LPCSTR fire_bone		= pSettings->r_string					(section,"fire_bone");
		m_fire_bone				= m_animations->dcast_PKinematics()->LL_BoneID	(fire_bone);
		if (m_fire_bone>=m_animations->dcast_PKinematics()->LL_BoneCount())	
			Debug.fatal	(DEBUG_INFO,"There is no '%s' bone for weapon '%s'.",fire_bone, *section);

		m_fp_offset				= pSettings->r_fvector3					(section,"fire_point");
		if(pSettings->line_exist(section,"fire_point2")) 
			m_fp2_offset		= pSettings->r_fvector3					(section,"fire_point2");
		else 
			m_fp2_offset		= m_fp_offset;

		m_sp_offset			= READ_IF_EXISTS(pSettings, r_fvector3, section, "shell_point", zero_vel);
	}
	else
	{
		m_fire_bone				= -1;
		m_fp_offset.set			(0,0,0);
		m_fp2_offset.set		(0,0,0);
		m_sp_offset.set			(0,0,0);
	}
	return true;
}

weapon_hud_value::~weapon_hud_value()
{
}

u32 shared_weapon_hud::motion_length(MotionID M)
{
	IKinematicsAnimated	*skeleton_animated = p_->m_animations;
	VERIFY				(skeleton_animated);
	CMotionDef			*motion_def = skeleton_animated->LL_GetMotionDef(M);
	VERIFY				(motion_def);

	if (motion_def->flags & esmStopAtEnd) 
	{
		CMotion*			motion		= skeleton_animated->LL_GetRootMotion(M);
		return				iFloor(0.5f + 1000.f*motion->GetLength()/ motion_def->Dequantize(motion_def->speed));
	}
	return				0;
}

MotionID shared_weapon_hud::motion_id(LPCSTR name)
{
	return p_->m_animations->ID_Cycle_Safe(name);
}

CWeaponHUD::CWeaponHUD			(CHudItem* pHudItem)
{
	m_bVisible					= false;
	m_pParentWeapon				= pHudItem;
	m_bHidden					= true;
	m_Transform.identity		();
}

CWeaponHUD::~CWeaponHUD()
{
}

void CWeaponHUD::Load(LPCSTR section)
{
	m_shared_data.create		(section,m_pParentWeapon);
}

void CWeaponHUD::UpdatePosition(const Fmatrix& trans)
{
	m_Transform.mul				(trans,m_shared_data.get_value()->m_offset);
	VERIFY						(!fis_zero(DET(m_Transform)));
}

MotionID CWeaponHUD::animGet(LPCSTR name)
{
	return m_shared_data.motion_id	(name);
}

void CWeaponHUD::animDisplay(MotionID M, BOOL bMixIn)
{
	if(m_bVisible)
	{
		Visual()->dcast_PKinematicsAnimated()->PlayCycle(M, bMixIn);
		Visual()->dcast_PKinematics()->CalculateBones_Invalidate();
		Visual()->dcast_PKinematics()->CalculateBones(TRUE);
	}
}

u32 CWeaponHUD::animPlay			(MotionID M,	BOOL bMixIn, CHudItem* W, u32 state, const CMotionDef*& md)
{
	md = m_shared_data.animations()->LL_GetMotionDef(M);
	Show							();
	animDisplay						(M, bMixIn);
	u32 anim_time					= m_shared_data.motion_length(M);
	return anim_time;
}

void CWeaponHUD::Update				()
{
	if (m_bVisible)
	{
		Visual()->dcast_PKinematicsAnimated()->UpdateTracks();
		Visual()->dcast_PKinematics()->CalculateBones_Invalidate();
		Visual()->dcast_PKinematics()->CalculateBones(TRUE);
	}
}

void CWeaponHUD::CreateSharedContainer	()
{
	VERIFY(!g_pWeaponHUDContainer);
	g_pWeaponHUDContainer	= new weapon_hud_container();
}

void CWeaponHUD::DestroySharedContainer	()
{
	xr_delete				(g_pWeaponHUDContainer);
}

void CWeaponHUD::CleanSharedContainer	()
{
	VERIFY(g_pWeaponHUDContainer);
	g_pWeaponHUDContainer->clean(false);
}

MotionID random_anim(MotionSVec& v)
{
	return v[Random.randI(v.size())];
}
