#include "StdAfx.h"
#include "HudCartridges.h"
#include "../../Weapon.h"
#include "../../Magazine.h"
#include "../../../Include/xrRender/Kinematics.h"

void THudCartridges::BeginComponent(IECSOwner* O)
{
	const shared_str& Section = smart_cast<CGameObject*>(O)->cNameSect();

	for (const auto& Bone : xr_string(pSettings->r_string(Section, "hud_cartridge_bones")).RemoveWhitespaces().Split(','))
	{
		m_bone_names.push_back(Bone.c_str());
	}

	m_cartridge_visual = pSettings->read_if_exists<str_c>(Section, "hud_cartridge_visual", "");
	m_shell_visual = pSettings->read_if_exists<str_c>(Section, "hud_shell_visual", "");

	for (const auto& [Key, Value] : pSettings->r_section(Section).Data)
	{
		const xr_string Name = Key.c_str();
		if (Name.starts_with("hud_cartridge_visual_"))
		{
			m_cartridge_type_visuals[u8(atoi(Name.c_str() + xr_strlen("hud_cartridge_visual_")))] = Value;
		}
		else if (Name.starts_with("hud_shell_visual_"))
		{
			m_shell_type_visuals[u8(atoi(Name.c_str() + xr_strlen("hud_shell_visual_")))] = Value;
		}
	}
}

void THudCartridges::EndComponent()
{
	for (auto& Slots : m_slots)
	{
		for (SSlot& Slot : Slots)
		{
			for (auto& [Name, Visual] : Slot.Visuals)
			{
				::Render->model_Delete(Visual);
			}
		}
	}
	m_slots.clear();
}

const shared_str& THudCartridges::Visual(u8 Type, bool Shell) const
{
	const auto& TypeVisuals = Shell ? m_shell_type_visuals : m_cartridge_type_visuals;
	const auto It = TypeVisuals.find(Type);
	return It != TypeVisuals.end() ? It->second : Shell ? m_shell_visual : m_cartridge_visual;
}

void THudCartridges::Update(u32 Count, u8 Type, bool Mirror)
{
	m_states[Mirror] = { Count, Type };
}

void THudCartridges::Update(CWeapon* Weapon, u32 Count, u8 Type, bool Mirror)
{
	Apply(*Weapon, Mirror || !Weapon->m_bUseChamberInUpdateBones ? Count : Count + Weapon->GetAmmoChamberElapsed(), Type, Mirror);
}

void THudCartridges::Apply(CInventoryItem& Item, u32 Count, u8 Type, bool Mirror)
{
	if (smart_cast<CMagazine*>(&Item))
	{
		return;
	}

	if (THudCartridges* Cartridges = Item.object().GetComponent<THudCartridges>())
	{
		Cartridges->Update(Count, Type, Mirror);
	}

	for (const auto& [Id, Child] : Item.m_children)
	{
		Apply(*Child, Count, Type, Mirror);
	}
}

void THudCartridges::Render(const Fmatrix& XForm, IKinematics* Model, IDSGraphManager* DM, u32 Mirror)
{
	if (m_bones.empty())
	{
		for (const shared_str& Name : m_bone_names)
		{
			const u16 Bone = Model->LL_BoneID(Name);
			R_ASSERT3(Bone != BI_NONE, "cartridge bone not found in hud model", Name.c_str());
			m_bones.push_back(Bone);
		}
	}

	if (m_slots.size() <= Mirror)
	{
		m_slots.resize(Mirror + 1);
	}

	xr_vector<SSlot>& Slots = m_slots[Mirror];
	if (Slots.empty())
	{
		Slots.resize(m_bones.size());
	}

	const SState& State = m_states[Mirror != 0];
	for (u32 i = 0; i < m_bones.size(); ++i)
	{
		const shared_str& Name = Visual(State.Type, i >= State.Count);
		if (!Name.size() || !Model->LL_GetBoneVisible(m_bones[i]))
		{
			continue;
		}

		SSlot& Slot = Slots[i];
		IRenderVisual*& RenderVisual = Slot.Visuals[Name];
		if (!RenderVisual)
		{
			RenderVisual = ::Render->model_Create(Name.c_str());
			if (IKinematics* Kinematics = PKinematics(RenderVisual))
			{
				Kinematics->CalculateBones(true);
			}
		}

		Slot.Transform.mul_43(XForm, Model->LL_GetTransform(m_bones[i]));
		DM->add_Dynamic(RenderVisual, &Slot.Transform, true);
	}
}
