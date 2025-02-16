#include "stdafx.h"
#include "CustomZone.h"
#include "ZoneVisual.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../Include/xrRender/KinematicsAnimated.h"

CVisualZone::CVisualZone						()
{
}
CVisualZone::~CVisualZone						()
{

}

BOOL CVisualZone::	net_Spawn						(CSE_Abstract* DC)
{
	BOOL ret = inherited::net_Spawn(DC);

	CSE_Abstract* entity = (CSE_Abstract*)(DC);

	LPCSTR attack_anim_name = pSettings->r_string(cNameSect_str(), "attack_anim_name");
	LPCSTR idle_anim_name = pSettings->r_string(cNameSect_str(), "idle_anim_name");

	IKinematicsAnimated* animated = smart_cast<IKinematicsAnimated*>(Visual());

	R_ASSERT2(animated, make_string<const char*>("object is not animated %s", cNameVisual()));

	m_attack_animation = animated->ID_Cycle(attack_anim_name);
	m_idle_animation = animated->ID_Cycle(idle_anim_name);

	R_ASSERT2(m_attack_animation.valid(), make_string<const char*>("! Can't find anim cycle: %s %s %s", DC->s_name, cNameVisual(), attack_anim_name));
	R_ASSERT2(m_idle_animation.valid(), make_string<const char*>("! Can't find anim cycle: %s %s %s", DC->s_name, cNameVisual(), idle_anim_name));

	animated->PlayCycle(m_idle_animation);

	setVisible(TRUE);

	return ret;
}
void CVisualZone::net_Destroy()
{
	inherited::net_Destroy();

}
void CVisualZone:: AffectObjects					()		
{
	inherited::AffectObjects					();
//	smart_cast<CKinematicsAnimated*>(Visual())->PlayCycle(*m_attack_animation);
}
void CVisualZone::SwitchZoneState(EZoneState new_state)
{
	if(m_eZoneState==eZoneStateBlowout && new_state != eZoneStateBlowout)
	{
	//	CKinematicsAnimated*	SA=smart_cast<CKinematicsAnimated*>(Visual());
		smart_cast<IKinematicsAnimated*>(Visual())->PlayCycle(m_idle_animation);
	}

	inherited::SwitchZoneState(new_state);

}
void CVisualZone::Load(LPCSTR section)
{
	inherited::Load(section);
	m_dwAttackAnimaionStart		=pSettings->r_u32(section,"attack_animation_start");
	m_dwAttackAnimaionEnd		=pSettings->r_u32(section,"attack_animation_end");
	VERIFY2(m_dwAttackAnimaionStart<m_dwAttackAnimaionEnd,"attack_animation_start must be less then attack_animation_end");
}

void CVisualZone::UpdateBlowout()
{
	inherited::UpdateBlowout();
	if(m_dwAttackAnimaionStart >=(u32)m_iPreviousStateTime && 
		m_dwAttackAnimaionStart	<(u32)m_iStateTime)
				smart_cast<IKinematicsAnimated*>(Visual())->PlayCycle(m_attack_animation);
		
	if(m_dwAttackAnimaionEnd >=(u32)m_iPreviousStateTime && 
		m_dwAttackAnimaionEnd	<(u32)m_iStateTime)
				smart_cast<IKinematicsAnimated*>(Visual())->PlayCycle(m_idle_animation);
}