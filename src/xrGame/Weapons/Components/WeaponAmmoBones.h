#pragma once

class CWeapon;

struct TLiteAmmoBones final
{
private:
	struct
	{
		xr_hash_map<u32, shared_str> bullet_bones{};
		u32 bullet_cnt = 0;

	} m_ammo_bones_lite;

public:
	void BeginComponent(IECSOwner* O);
	void UpdateLiteAmmoBones(CWeapon* pWeapon, u32 idx);

private:
	ECS_COMPONENT(TLiteAmmoBones)
		ECS_VALUE(m_ammo_bones_lite.bullet_cnt, "Bullet Count");
	ECS_END
};

struct TMagAmmoBones final
{
private:
	xr_hash_map<u8, RStringVec> m_mag_bone_type{};

public:
	void BeginComponent(IECSOwner* O);
	void UpdateMagAmmoBones(CWeapon* pWeapon, u8 type);

private:
	ECS_COMPONENT(TMagAmmoBones)
	ECS_END
};

struct TGrenadeLauncherAmmoBones final
{
private:
	xr_hash_map<u8, RStringVec> m_grenade_launcher_bone_type{};

public:
	void BeginComponent(IECSOwner* O);
	void UpdateGLAmmoBones(CWeapon* pWeapon, u8 type);

private:
	ECS_COMPONENT(TGrenadeLauncherAmmoBones)
	ECS_END
};

struct TShellBones final
{
private:
	xr_hash_map<u8, RStringVec> m_shell_bone_type{};

public:
	void BeginComponent(IECSOwner* O);
	void UpdateShellBones(CWeapon* pWeapon, u8 type);

private:
	ECS_COMPONENT(TShellBones)
	ECS_END
};

struct TAmmoBones final
{
public:
	struct SAmmoBonesParams
	{
		u8 AmmoType = u8(-1);
		xr_hash_map<u32, std::pair<shared_str, RStringVec>> ConfigurationMap{};
		RStringVec AllBones{};

		SAmmoBonesParams() = default;
		SAmmoBonesParams(u32 type) : AmmoType(type) {}
		void Load(const shared_str& section, s32 base_node_count);
	};
private:
	xr_vector<SAmmoBonesParams> m_ammo_params{};
	shared_str m_current_section;
	u8 m_params_max_count = u8(-1);

public:
	void UpdateAmmoBones(CWeapon* pWeapon, u32 idx, u8 type);
	void Load(CWeapon* pWeapon, const shared_str& section);

private:
	ECS_COMPONENT(TAmmoBones)
		ECS_STRING(m_current_section.c_str(), "Current Section");
		ECS_VALUE(m_params_max_count, "Params Count");
	ECS_END
};