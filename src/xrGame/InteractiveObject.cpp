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

BOOL CInteractiveObject::net_Spawn(CSE_Abstract* DC)
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

void CInteractiveObject::Load(LPCSTR section)
{
	inherited::Load(section);

	ParseBones(section, "items_bones_names", m_bone_names);
	left_uses = m_bone_names.size();

	m_spawn_section = READ_IF_EXISTS(pSettings, r_string, section, "spawn_section_name", "");
	ParseRandomSounds(section, "use_sounds", m_use_sounds);
	m_tip_text_default = READ_IF_EXISTS(pSettings, r_string, section, "use_tip_text", "");
	m_tip_text = m_tip_text_default;

	SetText();
}

void CInteractiveObject::ParseBones(LPCSTR section, LPCSTR bonesParameter, xr_vector<xr_string>& _array)
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

void CInteractiveObject::ParseRandomSounds(LPCSTR section, LPCSTR soundParameter, xr_vector<ref_sound>& soundsArray)
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
	LPCSTR section,
	const Fvector& position,
	u32 level_vertex_id,
	GameGraph::_GRAPH_ID game_vertex_id,
	ALife::_OBJECT_ID id_parent
);

void CInteractiveObject::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	output_packet.w_u8(left_uses);
	//output_packet.r_stringZ(m_tip_text);
}

void CInteractiveObject::load(IReader& input_packet)
{
	inherited::load(input_packet);
	left_uses = input_packet.r_u8();
	//input_packet.r_stringZ(m_tip_text);

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
		if (!m_spawn_section.empty())
		{
			if (CActor* act = Actor())
			{
				if (CALifeSimulator* sim = const_cast<CALifeSimulator*>(&ai().alife()))
				{
					CALifeSimulator__spawn_item2(
						sim,
						m_spawn_section.c_str(),
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

void CInteractiveObject::SetVisible(shared_str bone_name, BOOL bVisibility)
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