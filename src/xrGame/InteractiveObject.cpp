#include "StdAfx.h"
#include "pch_script.h"
#include "InteractiveObject.h"
#include "../Include/xrRender/Kinematics.h"
#include "Actor.h"
#include "ai_object_location.h"
#include "alife_simulator_base.h"
#include "alife_simulator.h"

CInteractiveObject::CInteractiveObject()
{

}

CInteractiveObject::~CInteractiveObject()
{
	DestroySoundsArray(m_use_sounds);
}

void CInteractiveObject::DestroySoundsArray(xr_vector<ref_sound>& soundsArray)
{
	for (ref_sound& sound : soundsArray)
	{
		sound.destroy();
	}

	soundsArray.clear();
}

void CInteractiveObject::OnEvent(NET_Packet& P, u16 type)
{
	inherited::OnEvent(P, type);
}

void CInteractiveObject::net_Relcase(CObject* O)
{
	inherited::net_Relcase(O);
}

void CInteractiveObject::UpdateCL()
{
	inherited::UpdateCL();
}

void CInteractiveObject::net_Destroy()
{
	inherited::net_Destroy();
}

bool CInteractiveObject::net_Spawn(CSE_Abstract* DC)
{
	inherited::net_Spawn(DC);
	setVisible(TRUE);
	setEnabled(TRUE);

	if (CSE_ALifeInteractiveObject* pSE_box = smart_cast<CSE_ALifeInteractiveObject*>(DC))
	{
		m_can_take = pSE_box->m_can_take;
		SetText();
	}

	return TRUE;
}

void CInteractiveObject::Load(const char* section)
{
	inherited::Load(section);

	ParseToVector(section, "items_bones_names", m_bone_names);
	left_uses = m_bone_names.size();
	ParseToVector(section, "spawn_section_name", m_spawn_sections);

	useSpawnRandomSections = READ_IF_EXISTS(pSettings, r_bool, section, "use_spawn_random_sections", false);
	useSpawnAtBoneIndexSections = READ_IF_EXISTS(pSettings, r_bool, section, "use_spawn_at_bone_index_sections", false);

	ParseRandomSounds(section, "use_sounds", m_use_sounds);
	m_tip_text_default = READ_IF_EXISTS(pSettings, r_string, section, "use_tip_text", "");
	m_tip_text = m_tip_text_default;

	SetText();
}

void CInteractiveObject::ParseToVector(const char* section, const char* bonesParameter, xr_vector<xr_string>& _array)
{
	_array.clear();

	if (pSettings->line_exist(section, bonesParameter))
	{
		xr_string unsplitted = pSettings->r_string(section, bonesParameter);
		if (!unsplitted.empty())
		{
			_array = unsplitted.RemoveWhitespaces().Split();
		}
	}
}

void CInteractiveObject::ParseRandomSounds(const char* section, const char* soundParameter, xr_vector<ref_sound>& soundsArray)
{
	soundsArray.clear();

	if (pSettings->line_exist(section, soundParameter))
	{
		xr_string unsplittedPaths = pSettings->r_string(section, soundParameter);
		if (!unsplittedPaths.empty())
		{
			xr_vector<xr_string> paths = unsplittedPaths.RemoveWhitespaces().Split();
			for (xr_string& sound_path : paths)
			{
				soundsArray.emplace_back().create(sound_path.c_str(), st_Effect, sg_SourceType);
			}
		}
	}
}

extern CSE_Abstract* CALifeSimulator__spawn_item2(
	CALifeSimulator* self_,
	const char* section,
	const Fvector& position,
	u32 level_vertex_id,
	GameGraph::_GRAPH_ID game_vertex_id,
	ALife::_OBJECT_ID id_parent
);

void CInteractiveObject::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	output_packet.w_u8(left_uses);
}

void CInteractiveObject::load(IReader& input_packet)
{
	inherited::load(input_packet);
	left_uses = input_packet.r_u8();

	if (!m_bone_names.empty())
	{
		u8 total_bones = m_bone_names.size();
		if (left_uses < total_bones)
		{
			for (u8 i = left_uses; i < total_bones; i++)
			{
				SetVisible(m_bone_names[i].c_str(), false);
			}
		}

		SetText();
	}
}

void CInteractiveObject::OnUse()
{
	if (left_uses > 0)
	{
		if (!m_spawn_sections.empty())
		{
			if (CActor* act = Actor())
			{
				if (CALifeSimulator* sim = const_cast<CALifeSimulator*>(&ai().alife()))
				{
					const char* section = m_spawn_sections[0].c_str();

					if (useSpawnRandomSections)
					{
						section = m_spawn_sections[::Random.randI(0, m_spawn_sections.size())].c_str();
					}

					if (useSpawnAtBoneIndexSections && !m_bone_names.empty() && left_uses - 1 >= 0)
					{
						section = m_spawn_sections[left_uses - 1].c_str();;
					}

					CALifeSimulator__spawn_item2(
						sim,
						section,
						act->Position(),
						act->cast_game_object()->ai_location().level_vertex_id(),
						act->cast_game_object()->ai_location().game_vertex_id(),
						act->ID()
					);
				}
			}
		}

		if (!m_bone_names.empty() && left_uses - 1 >= 0)
		{
			SetVisible(m_bone_names[left_uses - 1].c_str(), false);
		}

		if (!m_use_sounds.empty())
		{
			GetRandomSound(m_use_sounds).play_at_pos(0, Position());
		}

		left_uses--;
	}

	SetText();

	if (m_bone_names.size() == 0)
	{
		this->DestroyObject();
	}
}

void CInteractiveObject::SetText()
{
	if (m_bone_names.size() == 0)
	{
		m_tip_text = m_tip_text_default;
		
	}
	else
	{
		m_tip_text = left_uses > 0 ? m_tip_text_default : "";
	}

	set_tip_text(m_tip_text.c_str());
}

void CInteractiveObject::SetVisible(shared_str bone_name, bool bVisibility)
{
	IKinematics* KI = PKinematics(Visual());
	if (!KI)
	{
		return;
	}

	u16 boneId = KI->LL_BoneID(bone_name);
	if (boneId == BI_NONE)
	{
		return;
	}

	KI->CalculateBones(TRUE);
	KI->LL_SetBoneVisible(boneId, bVisibility, TRUE);
	KI->CalculateBones(TRUE);
	SetText();
}

using namespace luabind;
#pragma optimize("s",on)
void CInteractiveObject::script_register(lua_State* L)
{
    module(L)
        [
            class_<CInteractiveObject, CGameObject>("CInteractiveObject")
                .def(constructor<>())
        ];
}