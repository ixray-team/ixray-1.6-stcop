#pragma once

class IKinematics;

struct SBoneProtections final
{
	static constexpr const char* HIT_FRACTION = "hit_fraction";
	static constexpr const char* HIT_FRACTION_NPC = "hit_fraction_npc";

	enum HitFractionType : u8
	{
		// Exists in SOC and CS
		HitFraction,

		// Introduced in CS, externally assigned
		HitFractionActorCS,

		// Introduced in COP
		HitFractionNPC,

		// Hit formula changed in COP, externally assigned
		HitFractionActorCOP,
	};

	struct BoneProtection 
	{
		float		koeff;
		float		armor;
		bool		BonePassBullet;
	};
    float m_fHitFrac{ 0.1f };
    HitFractionType m_hitFracType{ HitFractionNPC };

	using storage_type = xr_map<u16, BoneProtection>;
	using storage_it = storage_type::iterator;
						SBoneProtections	()								{m_default.koeff = 1.0f; m_default.armor = 0; m_fHitFrac = 0.1f; }
	BoneProtection		m_default;
	storage_type		m_bones_koeff;
	void				reload				(const shared_str& outfit_section, IKinematics* kinematics);
	void				add					(const shared_str& outfit_section, IKinematics* kinematics);
	float				getBoneProtection	(u16 bone_id);
	float				getBoneArmor		(u16 bone_id);
	bool				getBonePassBullet	(u16 bone_id);
};

