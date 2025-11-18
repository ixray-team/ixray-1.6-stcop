#pragma once

class IKinematics;

struct SBoneProtections final
{
	struct BoneProtection
	{
		float koeff = 1.0f;
		float armor = 0.0f;
		BOOL BonePassBullet = FALSE;
	};

	using storage_type = xr_map<s16, BoneProtection>;
	using storage_it = storage_type::iterator;

	float m_fHitFracNpc = 0.0f;
	float m_fHitFracActor = 0.1f;

	BoneProtection m_default = {};
	storage_type m_bones_koeff;

	SBoneProtections() { m_default.koeff = 1.0f; m_default.armor = 0; m_fHitFracActor = 0.1f; }

	void reload(const shared_str& outfit_section, IKinematics* kinematics);
	void add(const shared_str& outfit_section, IKinematics* kinematics);

	float getBoneProtection(s16 bone_id);
	float getBoneArmor(s16 bone_id);

	BOOL getBonePassBullet(s16 bone_id);
};

