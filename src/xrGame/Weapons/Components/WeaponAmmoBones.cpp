#include "StdAfx.h"
#include "WeaponAmmoBones.h"
#include "../../Weapon.h"
#include "../WeaponBoneVisibility.h"

void TLiteAmmoBones::BeginComponent(IECSOwner* O)
{
	CWeapon* pWeapon = smart_cast<CWeapon*>(O);
	auto ReachInAllSections = [&](const char* param_name)
	{
		const char* reached_sect = *pWeapon->cNameSect();
		const shared_str hud_section = pWeapon->HudSection();
		if (pSettings->line_exist(hud_section, param_name))
		{
			reached_sect = hud_section.c_str();
		}
		return reached_sect;
	};

	if (pSettings->line_exist(ReachInAllSections("bullets_count"), "bullets_count"))
	{
		m_ammo_bones_lite.bullet_cnt = pSettings->r_u32(ReachInAllSections("bullets_count"), "bullets_count");
	}

	if (m_ammo_bones_lite.bullet_cnt > 0)
	{
		shared_str read_bullet_bone_name = pSettings->r_string(ReachInAllSections("bullet_bone_name"), "bullet_bone_name");

		for (u32 i = 1; i <= m_ammo_bones_lite.bullet_cnt; ++i)
		{
			shared_str bullet_bone_name;
			bullet_bone_name.printf("%s%d", *read_bullet_bone_name, i);

			m_ammo_bones_lite.bullet_bones[i] = bullet_bone_name;
		}
	}
}

void TLiteAmmoBones::UpdateLiteAmmoBones(CWeapon* pWeapon, u32 idx)
{
	if (pWeapon == nullptr)
	{
		return;
	}

	if (m_ammo_bones_lite.bullet_cnt == 0)
	{
		return;
	}

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, bool status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, false);
			}
		}
	};

	attachable_hud_item* HID = pWeapon->HudItemData();
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : nullptr;
	IKinematics* world_kin = pWeapon->Visual() != nullptr ? PKinematics(pWeapon->Visual()) : nullptr;

	for (const auto& it : m_ammo_bones_lite.bullet_bones)
	{
		u32 bullet_idx = it.first;
		const shared_str& node = it.second;

		bool visible = (bullet_idx <= idx);
		SetVisible(world_kin, node, visible);
		SetVisible(hud_kin, node, visible);
	}

	if (world_kin != nullptr)
	{
		world_kin->CalculateBones_Invalidate();
		world_kin->CalculateBones(true);
	}

	if (hud_kin != nullptr)
	{
		hud_kin->CalculateBones_Invalidate();
		hud_kin->CalculateBones(true);
	}
}

void TMagAmmoBones::BeginComponent(IECSOwner* O)
{
	CWeapon* pWeapon = smart_cast<CWeapon*>(O);
	auto ReachInAllSections = [&](const char* param_name)
	{
		const char* reached_sect = *pWeapon->cNameSect();
		const shared_str hud_section = pWeapon->HudSection();
		if (pSettings->line_exist(hud_section, param_name))
		{
			reached_sect = hud_section.c_str();
		}
		return reached_sect;
	};

	for (u8 i = 0; i < pWeapon->m_ammoTypes.size(); i++)
	{
		shared_str mag_bone_type = shared_str().printf("mag_bone_type_%d", i);

		if (pSettings->line_exist(ReachInAllSections(*mag_bone_type), *mag_bone_type))
		{
			RStringVec bones = {};
			const char* read_mag_bone_type = pSettings->r_string(ReachInAllSections(*mag_bone_type), *mag_bone_type);

			for (int i = 0, count = _GetItemCount(read_mag_bone_type); i < count; ++i)
			{
				string128 bone_name = {};
				_GetItem(read_mag_bone_type, i, bone_name);
				bones.push_back(bone_name);
			}

			m_mag_bone_type[i] = bones;
		}
	}
}

void TMagAmmoBones::UpdateMagAmmoBones(CWeapon* pWeapon, u8 type)
{
	if (pWeapon == nullptr)
	{
		return;
	}

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, bool status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, false);
			}
		}
	};

	attachable_hud_item* HID = pWeapon->HudItemData();
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : nullptr;
	IKinematics* world_kin = pWeapon->Visual() != nullptr ? PKinematics(pWeapon->Visual()) : nullptr;

	for (const auto& [ammotype, vec] : m_mag_bone_type)
	{
		bool status = !!(ammotype == type);
		for (const auto& bone : vec)
		{
			SetVisible(hud_kin, bone, status);
			SetVisible(world_kin, bone, status);
		}
	}

	if (world_kin != nullptr)
	{
		world_kin->CalculateBones_Invalidate();
		world_kin->CalculateBones(true);
	}

	if (hud_kin != nullptr)
	{
		hud_kin->CalculateBones_Invalidate();
		hud_kin->CalculateBones(true);
	}
}

void TGrenadeLauncherAmmoBones::BeginComponent(IECSOwner* O)
{
	CWeapon* pWeapon = smart_cast<CWeapon*>(O);
	auto ReachInAllSections = [&](const char* param_name)
	{
		const char* reached_sect = *pWeapon->cNameSect();
		const shared_str hud_section = pWeapon->HudSection();
		if (pSettings->line_exist(hud_section, param_name))
		{
			reached_sect = hud_section.c_str();
		}
		return reached_sect;
	};

	for (u8 i = 0; i < pWeapon->m_ammoTypes.size(); i++)
	{
		shared_str mag_bone_type = shared_str().printf("gl_bone_type_%d", i);

		if (pSettings->line_exist(ReachInAllSections(*mag_bone_type), *mag_bone_type))
		{
			RStringVec bones = {};
			const char* read_mag_bone_type = pSettings->r_string(ReachInAllSections(*mag_bone_type), *mag_bone_type);

			for (int i = 0, count = _GetItemCount(read_mag_bone_type); i < count; ++i)
			{
				string128 bone_name = {};
				_GetItem(read_mag_bone_type, i, bone_name);
				bones.push_back(bone_name);
			}

			m_grenade_launcher_bone_type[i] = bones;
		}
	}
}

void TGrenadeLauncherAmmoBones::UpdateGLAmmoBones(CWeapon* pWeapon, u8 type)
{
	if (pWeapon == nullptr)
	{
		return;
	}

	if (!pWeapon->IsGrenadeMode())
	{
		return;
	}

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, bool status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, false);
			}
		}
	};

	attachable_hud_item* HID = pWeapon->HudItemData();
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : nullptr;
	IKinematics* world_kin = pWeapon->Visual() != nullptr ? PKinematics(pWeapon->Visual()) : nullptr;

	for (const auto& [ammotype, vec] : m_grenade_launcher_bone_type)
	{
		bool status = !!(ammotype == type);
		for (const auto& bone : vec)
		{
			SetVisible(hud_kin, bone, status);
			SetVisible(world_kin, bone, status);
		}
	}

	if (world_kin != nullptr)
	{
		world_kin->CalculateBones_Invalidate();
		world_kin->CalculateBones(true);
	}

	if (hud_kin != nullptr)
	{
		hud_kin->CalculateBones_Invalidate();
		hud_kin->CalculateBones(true);
	}
}

void TShellBones::BeginComponent(IECSOwner* O)
{
	CWeapon* pWeapon = smart_cast<CWeapon*>(O);
	auto ReachInAllSections = [&](const char* param_name)
	{
		const char* reached_sect = *pWeapon->cNameSect();
		const shared_str hud_section = pWeapon->HudSection();
		if (pSettings->line_exist(hud_section, param_name))
		{
			reached_sect = hud_section.c_str();
		}
		return reached_sect;
	};

	for (u8 i = 0; i < pWeapon->m_ammoTypes.size(); i++)
	{
		shared_str mag_bone_type = shared_str().printf("shell_bone_type_%d", i);

		if (pSettings->line_exist(ReachInAllSections(*mag_bone_type), *mag_bone_type))
		{
			RStringVec bones = {};
			const char* read_mag_bone_type = pSettings->r_string(ReachInAllSections(*mag_bone_type), *mag_bone_type);

			for (int i = 0, count = _GetItemCount(read_mag_bone_type); i < count; ++i)
			{
				string128 bone_name = {};
				_GetItem(read_mag_bone_type, i, bone_name);
				bones.push_back(bone_name);
			}

			m_shell_bone_type[i] = bones;
		}
	}
}

void TShellBones::UpdateShellBones(CWeapon* pWeapon, u8 type)
{
	if (pWeapon == nullptr)
	{
		return;
	}

	auto SetVisible = [&](IKinematics* kin, const shared_str& bone_name, bool status)
	{
		if (kin != nullptr)
		{
			u16 bone_id = kin->LL_BoneID(bone_name);
			if (bone_id != BI_NONE)
			{
				kin->LL_SetBoneVisible(bone_id, status, false);
			}
		}
	};

	attachable_hud_item* HID = pWeapon->HudItemData();
	IKinematics* hud_kin = HID != nullptr ? HID->m_model : nullptr;
	IKinematics* world_kin = pWeapon->Visual() != nullptr ? PKinematics(pWeapon->Visual()) : nullptr;

	for (const auto& [ammotype, vec] : m_shell_bone_type)
	{
		bool status = !!(ammotype == type);
		for (const auto& bone : vec)
		{
			SetVisible(hud_kin, bone, status);
			SetVisible(world_kin, bone, status);
		}
	}

	if (world_kin != nullptr)
	{
		world_kin->CalculateBones_Invalidate();
		world_kin->CalculateBones(true);
	}

	if (hud_kin != nullptr)
	{
		hud_kin->CalculateBones_Invalidate();
		hud_kin->CalculateBones(true);
	}
}

void TAmmoBones::SAmmoBonesParams::Load(const shared_str& section, s32 base_node_count)
{
	if (!AllBones.empty())
	{
		AllBones.clear();
	}
	if (pSettings->line_exist(section, "all_bones"))
	{
		const char* S = pSettings->r_string(section, "all_bones");
		if (S && S[0])
		{
			string128 Item = {};
			u32 count = _GetItemCount(S);
			for (u32 it = 0; it < count; ++it)
			{
				_GetItem(S, it, Item);
				AllBones.push_back(Item);
			}
		}
	}

	shared_str configuration;

	if (ConfigurationMap.size() > 0)
	{
		for (auto& node : ConfigurationMap)
		{
			if (!node.second.second.empty())
			{
				node.second.second.clear();
			}
		}
	}

	ConfigurationMap.clear();

	s32 i = 0;

	for (; i <= base_node_count; ++i)
	{
		configuration.printf("configuration_%d", i);

		auto& node = ConfigurationMap[i];
		node.first = configuration;
		node.second = {};

		if (!pSettings->line_exist(section, configuration))
		{
			continue;
		}

		const char* S = pSettings->r_string(section, *configuration);
		if (S && S[0])
		{
			string128 Item = {};
			u32 count = _GetItemCount(S);
			for (u32 it = 0; it < count; ++it)
			{
				node.second.push_back(_GetItem(S, it, Item));
			}
		}
	}

	configuration.printf("configuration_%d", i);

	while (pSettings->line_exist(section, configuration))
	{
		auto& node = ConfigurationMap[i];
		node.first = configuration;
		node.second = {};

		const char* S = pSettings->r_string(section, *configuration);
		if (S && S[0])
		{
			string128 Item = {};
			u32 count = _GetItemCount(S);
			for (u32 it = 0; it < count; ++it)
			{
				node.second.push_back(_GetItem(S, it, Item));
			}
		}

		configuration.printf("configuration_%d", ++i);
	}
}

void TAmmoBones::Load(CWeapon* pWeapon, const shared_str& section)
{
	if (pWeapon == nullptr)
	{
		return;
	}

	bool new_section = !!(m_current_section.size() == 0 || m_current_section.size() > 0 && m_current_section != section);

	m_current_section = section;

	if (!new_section && m_params_max_count == pWeapon->m_ammoTypes.size())
	{
		return;
	}

	m_params_max_count = pWeapon->m_ammoTypes.size();

	if (pSettings->line_exist(section, "ammo_params_section") &&
		pSettings->section_exist(pSettings->r_string(section, "ammo_params_section")))
	{
		if (m_ammo_params.empty())
		{
			m_ammo_params.emplace_back();
		}

		m_ammo_params[0].Load(pSettings->r_string(section, "ammo_params_section"), pWeapon->iMagazineSize);
	}
	else
	{
		for (int i = 0; i < pWeapon->m_ammoTypes.size(); ++i)
		{
			shared_str params_section;
			params_section.printf("ammo_params_section_%d", i);

			if (pSettings->line_exist(section, *params_section))
			{
				if (i >= m_ammo_params.size())
				{
					m_ammo_params.emplace_back(i);
				}

				m_ammo_params[i].Load(pSettings->r_string(section, *params_section), pWeapon->iMagazineSize);
			}
		}
	}
}

void TAmmoBones::ApplyToModel(IKinematics* Model, xr_span<const SAmmoBonesParams> Parameters, u32 Count, u8 Type)
{
	if (!Model)
	{
		return;
	}
	for (const auto& Parameter : Parameters)
	{
		for (const auto& Bone : Parameter.AllBones)
		{
			WeaponBoneVisibility::SetBone(*Model, Bone, false);
		}
	}
	for (const auto& Parameter : Parameters)
	{
		if (Parameter.AmmoType == Type || Parameter.AmmoType == CWeapon::undefined_ammo_type)
		{
			const auto It = Parameter.ConfigurationMap.find(Count);
			if (It != Parameter.ConfigurationMap.end())
			{
				for (const auto& Bone : It->second.second)
				{
					WeaponBoneVisibility::SetBone(*Model, Bone, true);
				}
			}
		}
	}
	Model->CalculateBones_Invalidate();
	Model->CalculateBones(true);
}

void TAmmoBones::ApplyDefaultIconBones(IKinematics& Model, LPCSTR SectionName, xr_span<const shared_str> InstalledSections)
{
	LPCSTR HudSection = WeaponBoneVisibility::ReadString(SectionName, "hud");
	for (const auto& Parameters : InstalledSections)
	{
		HudSection = pSettings->read_if_exists<str_c>(Parameters, "hud", HudSection);
	}
	const auto ReadParameters = [&](LPCSTR Field)
	{
		LPCSTR Parameters = pSettings->section_exist(HudSection) && pSettings->line_exist(HudSection, Field) ? HudSection : SectionName;
		return WeaponBoneVisibility::ReadString(Parameters, Field);
	};
	xr_vector<SAmmoBonesParams> Parameters;
	const s32 MagazineSize = pSettings->read_if_exists<s32>(SectionName, "ammo_mag_size", 0);
	LPCSTR Common = ReadParameters("ammo_params_section");
	if (*Common && pSettings->section_exist(Common))
	{
		Parameters.emplace_back().Load(Common, MagazineSize);
	}
	else
	{
		LPCSTR Ammo = WeaponBoneVisibility::ReadString(SectionName, "ammo_class");
		for (int Index = 0, Count = _GetItemCount(Ammo); Index < Count; ++Index)
		{
			string64 Field;
			xr_sprintf(Field, "ammo_params_section_%d", Index);
			LPCSTR Section = ReadParameters(Field);
			if (*Section && pSettings->section_exist(Section))
			{
				Parameters.emplace_back(Index).Load(Section, MagazineSize);
			}
		}
	}
	// Newly spawned server items have an empty magazine and ammo type zero.
	ApplyToModel(&Model, Parameters, 0, 0);
}

void TAmmoBones::UpdateAmmoBones(CWeapon* pWeapon, u32 idx, u8 type)
{
	if (pWeapon == nullptr)
	{
		return;
	}
	if (pWeapon->m_bUseChamberInUpdateBones)
	{
		idx += pWeapon->GetAmmoChamberElapsed();
	}
	attachable_hud_item* HudItem = pWeapon->HudItemData();
	ApplyToModel(HudItem ? HudItem->m_model : nullptr, m_ammo_params, idx, type);
	ApplyToModel(pWeapon->Visual() ? PKinematics(pWeapon->Visual()) : nullptr, m_ammo_params, idx, type);
}
