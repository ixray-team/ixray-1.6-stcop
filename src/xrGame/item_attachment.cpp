#include "StdAfx.h"
#include "item_attachment.h"
#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "GameObject.h"
#include "PhysicsShellHolder.h"
#include "inventory_item.h"
static bool try_to_load_attachments = true;
static xr_hash_map<shared_str, xr_vector<shared_str>> all_attachments;
void item_attachments_manager::load_attachments(IKinematics* parent_model)
{
	if (!try_to_load_attachments) return;

	if (all_attachments.empty())
	{
		auto& sections = pSettings->sections();
		size_t sections_size = sections.size();
		xr_string parent_sect, attachment_sect;
		attachment_sect.reserve(32);
		parent_sect.reserve(32);
		for (size_t i = 0; i < sections_size; i++)
		{
			parent_sect = sections[i].Name.c_str();
			if (!sections[i].line_exist("inv_name")) continue;
			for (size_t i2 = 0; i2 < sections_size; i2++)
			{
				if (!sections[i2].line_exist("inv_name")) continue;
				attachment_sect = parent_sect + "_" + sections[i2].Name.c_str();
				if (pSettings->section_exist(attachment_sect.c_str()) && pSettings->line_exist(attachment_sect.c_str(), "attachment_type"))
				{
					all_attachments[shared_str(parent_sect.c_str())].push_back(sections[i2].Name);
				}
			}
		}
	}

	if (all_attachments.empty())
	{
		try_to_load_attachments = false;
		return;
	}
	auto it = all_attachments.find(m_parent->object().cNameSect());
	if (it != all_attachments.end())
	{
		for (auto& attachment_name : it->second)
			load_attachment(attachment_name, parent_model);
	}
}

void item_attachments_manager::load_attachment(shared_str sect_name, IKinematics* parent_model)
{
	item_attachment& attachment = m_attachments[sect_name];
	if (attachment.state.test(eAStateFullyLoaded))
		return;

	xr_string attachment_modifiers_sect = xr_string(m_parent->object().cNameSect_str()) + '_' + xr_string(*sect_name);
	attachment.mod_sect_name = attachment_modifiers_sect.c_str();
	attachment.m_parent = m_parent;
	attachment.state.set(eAStateMCombined, !pSettings->line_exist(*attachment.mod_sect_name, "attachment_hud_visual"));
	attachment.place.m_model = PKinematics(::Render->model_Create(pSettings->line_exist(*attachment.mod_sect_name, "attachment_visual") ? pSettings->r_string(*attachment.mod_sect_name, "attachment_visual") : pSettings->r_string(sect_name, "visual")));
	attachment.hud_place.m_model = attachment.state.test(eAStateMCombined) ? attachment.place.m_model : PKinematics(::Render->model_Create(pSettings->r_string(*attachment.mod_sect_name, "attachment_hud_visual")));

	if (pSettings->section_exist(attachment_modifiers_sect.c_str()))
	{
		attachment.hud_place.position = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_hud_position") ? pSettings->r_fvector3(attachment_modifiers_sect.c_str(), "attachment_hud_position") : Fvector{0.f, 0.f, 0.f};
		attachment.hud_place.direction = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_hud_direction") ? pSettings->r_fvector3(attachment_modifiers_sect.c_str(), "attachment_hud_direction") : Fvector{0.f, 0.f, 0.f};
		attachment.hud_place.scale = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_hud_scale") ? pSettings->r_fvector3(attachment_modifiers_sect.c_str(), "attachment_hud_scale") : Fvector{1.f, 1.f, 1.f};
		if (parent_model)
		{
			attachment.hud_place.parent_bone_id = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_hud_bone_name") ? parent_model->LL_BoneID(pSettings->r_string(attachment_modifiers_sect.c_str(), "attachment_hud_bone_name")) : parent_model->LL_GetBoneRoot();
			attachment.state.set(eAStateFullyLoaded, true);
		}

		attachment.place.position = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_position") ? pSettings->r_fvector3(attachment_modifiers_sect.c_str(), "attachment_position") : Fvector{0.f, 0.f, 0.f};
		attachment.place.direction = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_direction") ? pSettings->r_fvector3(attachment_modifiers_sect.c_str(), "attachment_direction") : Fvector{0.f, 0.f, 0.f};
		attachment.place.scale = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_scale") ? pSettings->r_fvector3(attachment_modifiers_sect.c_str(), "attachment_scale") : Fvector{1.f, 1.f, 1.f};
		attachment.place.parent_bone_id = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_bone_name") ? PKinematics(m_parent->object().Visual())->LL_BoneID(pSettings->r_string(attachment_modifiers_sect.c_str(), "attachment_bone_name")) : PKinematics(m_parent->object().Visual())->LL_GetBoneRoot();

		attachment.attachment_type = pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_type") ? EattachmentType(pSettings->r_u8(attachment_modifiers_sect.c_str(), "attachment_type")) : EattachmentType::eTypeNone;
		attachment.state.set(eAStatePermanent, pSettings->line_exist(attachment_modifiers_sect.c_str(), "attachment_permanent") ? pSettings->r_bool(attachment_modifiers_sect.c_str(), "attachment_permanent") : false);
	}
}

void item_attachments_manager::unload_attachments()
{
	for (auto& pair : m_attachments)
	{
		if (pair.second.state.test(eAStateMCombined))
		{
			if (pair.second.place.m_model)
			{
				IRenderVisual* v = pair.second.place.m_model->dcast_RenderVisual();
				::Render->model_Delete(v);
				pair.second.place.m_model = nullptr;
				pair.second.hud_place.m_model = nullptr;
			}
		}
		else
		{
			if (pair.second.hud_place.m_model)
			{
				IRenderVisual* v = pair.second.hud_place.m_model->dcast_RenderVisual();
				::Render->model_Delete(v);
				pair.second.hud_place.m_model = nullptr;
			}
			if (pair.second.place.m_model)
			{
				IRenderVisual* v = pair.second.place.m_model->dcast_RenderVisual();
				::Render->model_Delete(v);
				pair.second.place.m_model = nullptr;
			}
		}
	}
	m_attachments.clear();
}

void item_attachments_manager::anim_play_attachment(const shared_str& item_anm_name, float speed, bool bMixIn)
{
	for (auto& pair : m_attachments)
	{
		if (IKinematicsAnimated* ka = pair.second.hud_place.m_model->dcast_PKinematicsAnimated())
		{
			MotionID M2 = ka->ID_Cycle_Safe(item_anm_name);
			if (!M2.valid())
			{
				M2 = ka->ID_Cycle_Safe("idle");
			}
			else if (bDebug)
			{
				Msg("playing item animation [%s]", item_anm_name.c_str());
			}

			R_ASSERT3(M2.valid(), "model has no motion [idle] ", pair.second.hud_place.m_model->getDebugName().c_str());

			if(CBlend* B = ka->PlayCycle(M2, bMixIn))
				B->speed *= speed;
			
			pair.second.hud_place.m_model->CalculateBones_Invalidate();
		}
	}
}

void item_attachments_manager::switch_attachment(shared_str sect_name)
{
	auto it = m_attachments.find(sect_name);
	if (it != m_attachments.end())
	{
		auto& state = it->second.state;
		state.set(eAStateVisible, !state.test(eAStateVisible));
	}
}

void item_attachments_manager::enable_attachment(shared_str sect_name)
{
	auto it = m_attachments.find(sect_name);
	if (it != m_attachments.end())
	{
		it->second.state.set(eAStateVisible, true);
	}
}

void item_attachments_manager::disable_attachment(shared_str sect_name)
{
	auto it = m_attachments.find(sect_name);
	if (it != m_attachments.end())
	{
		it->second.state.set(eAStateVisible, false);
	}
}

void item_attachments_manager::render_attachments(Fmatrix& xform, IKinematics* parent_model, bool hud_mode)
{
	Fmatrix attachment_offset, attachment_final_transform;
	for (auto& pair : m_attachments)
	{
		item_attachment& attachment = pair.second;
		if (!attachment.state.test(eAStatePermanent) && !attachment.state.test(eAStateVisible))
		{
			continue;
		}


		if (hud_mode)
		{
			if (IKinematicsAnimated* ka = attachment.hud_place.m_model->dcast_PKinematicsAnimated())
			{
				if (ka->LL_PartBlendsCount(0) != 0 ||
					ka->LL_PartBlendsCount(1) != 0 ||
					ka->LL_PartBlendsCount(2) != 0 ||
					ka->LL_PartBlendsCount(3) != 0 ||
					ka->LL_PartBlendsCount(4) != 0 ||
					ka->LL_PartBlendsCount(5) != 0 ||
					ka->LL_PartBlendsCount(6) != 0 ||
					ka->LL_PartBlendsCount(7) != 0)
				{
					ka->UpdateTracks();
					ka->dcast_PKinematics()->CalculateBones_Invalidate();
					ka->dcast_PKinematics()->CalculateBones(true);
				}
			}
			else
			{
				attachment.hud_place.m_model->CalculateBones(true);
			}
		}
		else
		{
			attachment.place.m_model->CalculateBones(true);
		}

		item_attachment::placement& place = hud_mode ? attachment.hud_place : attachment.place;

		attachment_offset.setXYZ(deg2rad(place.direction.x), deg2rad(place.direction.y), deg2rad(place.direction.z));
		attachment_offset.i.mul(place.scale.x);
		attachment_offset.j.mul(place.scale.y);
		attachment_offset.k.mul(place.scale.z);
		attachment_offset.translate_over(place.position);
		attachment_final_transform.mul(parent_model->LL_GetTransform(place.parent_bone_id), attachment_offset);
		attachment_final_transform.mulA_43(xform);

		::Render->set_Transform(&attachment_final_transform);
		::Render->add_Visual(place.m_model->dcast_RenderVisual());
	}
}