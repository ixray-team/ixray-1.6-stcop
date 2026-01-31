////////////////////////////////////////////////////////////////////////////
//	Module 		: damage_manager.h
//	Created 	: 02.10.2001
//  Modified 	: 19.11.2003
//	Author		: Dmitriy Iassenev
//	Description : Damage manager
////////////////////////////////////////////////////////////////////////////

#pragma once

struct TDamageManager
{
	float m_default_hit_factor;
	float m_default_wound_factor;
	CObject* m_object;

	void reload(LPCSTR section, CInifile const* ini);
	void reload(LPCSTR section, LPCSTR sub_section, CInifile const* ini);

	void HitScale(const int bone_num, float& hit_scale, float& wound_scale, bool aim_bullet = false);

private:
	void load_section(LPCSTR section, CInifile const* ini);
	void init_bones(LPCSTR section, CInifile const* ini);
};