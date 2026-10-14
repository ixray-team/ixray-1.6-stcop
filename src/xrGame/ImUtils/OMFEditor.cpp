#include "StdAfx.h"
#include "ImUtils.h"
#include "ai_space.h"
#include "../alife_simulator.h"
#include "../Actor.h"
#include "../Inventory.h"
#include "../HudItem.h"
#include "../player_hud.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../xrEngine/SkeletonMotions.h"
#include "../../xrEngine/Fmesh.h"

namespace
{
	struct SOMFMotion
	{
		IKinematicsAnimated*	Model = nullptr;
		CMotionDef*				Def = nullptr;
		MotionID				Motion;
		xr_string				Name;
	};

	struct SOMFEditorState
	{
		attachable_hud_item*	Item = nullptr;
		IKinematicsAnimated*	HandsModel = nullptr;
		IKinematicsAnimated*	PartitionModel = nullptr;
		const CPartition*		Partition = nullptr;
		xr_vector<SOMFMotion>	Motions;
		string64				NewMark = {};
		MotionID				MarksMotion;
		int						SelectedMark = -1;
		int						SelectedInterval = -1;
		string512				SaveStatus = {};
		bool					Weapon = false;
	};

	struct SOMFMotionSnapshot
	{
		u16						Speed = 0;
		u16						Power = 0;
		u16						Accrue = 0;
		u16						Falloff = 0;
		u16						Flags = 0;
		xr_vector<motion_marks>	Marks;
	};

	SOMFEditorState g_omf_editor;
	xr_map<const CMotionDef*, SOMFMotionSnapshot> g_omf_snapshots;

	constexpr struct
	{
		u16			Bit;
		const char*	Name;
	}
	OMFMotionFlags[] =
	{
		{ 1 << 1, "Stop at end" },
		{ 1 << 2, "No mix" },
		{ 1 << 3, "Sync part" },
		{ 1 << 4, "Use foot steps" },
		{ 1 << 5, "Move XForm" },
		{ 1 << 6, "Idle" },
		{ 1 << 7, "Use weapon bone" },
	};
}

static void OMFEditorSnapshot(const CMotionDef* def, bool overwrite)
{
	if (!overwrite && g_omf_snapshots.contains(def))
	{
		return;
	}

	SOMFMotionSnapshot& snapshot = g_omf_snapshots[def];
	snapshot.Speed = def->speed;
	snapshot.Power = def->power;
	snapshot.Accrue = def->accrue;
	snapshot.Falloff = def->falloff;
	snapshot.Flags = def->flags;
	snapshot.Marks = def->marks;
}

static void OMFEditorReset(CMotionDef* def)
{
	const auto snapshot = g_omf_snapshots.find(def);
	if (snapshot == g_omf_snapshots.end())
	{
		return;
	}

	def->speed = snapshot->second.Speed;
	def->power = snapshot->second.Power;
	def->accrue = snapshot->second.Accrue;
	def->falloff = snapshot->second.Falloff;
	def->flags = snapshot->second.Flags;
	def->marks = snapshot->second.Marks;
}

static bool OMFEditorWriteParams(IReader& input, shared_motions& motions, IWriter& output, xr_vector<CMotionDef*>& defs)
{
	const u16 version = input.r_u16();
	output.w_u16(xrOGF_SMParamsVersion);

	string256 name;
	const u16 parts = input.r_u16();
	output.w_u16(parts);
	for (u16 part = 0; part < parts; ++part)
	{
		input.r_stringZ(name, sizeof(name));
		output.w_stringZ(name);

		const u16 bones = input.r_u16();
		output.w_u16(bones);
		for (u16 bone = 0; bone < bones; ++bone)
		{
			input.r_stringZ(name, sizeof(name));
			output.w_stringZ(name);
			output.w_u32(input.r_u32());
		}
	}

	const u16 count = input.r_u16();
	output.w_u16(count);
	for (u16 index = 0; index < count; ++index)
	{
		input.r_stringZ(name, sizeof(name));
		const u32 flags = input.r_u32();
		const u16 bone_or_part = input.r_u16();
		const u16 motion = input.r_u16();
		input.advance(sizeof(float) * 4);

		if (version >= 4)
		{
			for (u32 mark = 0, marks = input.r_u32(); mark < marks; ++mark)
			{
				motion_marks skipped;
				skipped.Load(&input);
			}
		}

		CMotionDef* def = motions.motion_def(index);
		if (def == nullptr)
		{
			return false;
		}

		output.w_stringZ(name);
		output.w_u32((flags & 0xffff0000) | def->flags);
		output.w_u16(bone_or_part);
		output.w_u16(motion);
		output.w_float(def->Dequantize(def->speed));
		output.w_float(def->Dequantize(def->power));
		output.w_float(def->Dequantize(def->accrue));
		output.w_float(def->Dequantize(def->falloff));
		output.w_u32(u32(def->marks.size()));
		for (motion_marks& mark : def->marks)
		{
			mark.Save(&output);
		}

		defs.push_back(def);
	}

	return true;
}

static bool OMFEditorSave(const SOMFMotion& motion)
{
	string512& status = g_omf_editor.SaveStatus;
	shared_motions motions = motion.Model->LL_MotionsSlot(motion.Motion.slot);
	const shared_str id = motions.id();

	string_path source = {};
	const char* alias = "$level$";
	const CLocatorAPI::file* desc = id.size() != 0 ? FS.exist(source, alias, id.c_str()) : nullptr;
	if (desc == nullptr)
	{
		alias = "$game_meshes$";
		desc = id.size() != 0 ? FS.exist(source, alias, id.c_str()) : nullptr;
		if (desc == nullptr)
		{
			xr_sprintf(status, "Not saved: [%s] is not an .omf file", id.size() ? id.c_str() : "?");
			return false;
		}
	}

	string_path target = {};
	if (desc->wrap != nullptr)
	{
		xr_strcpy(target, desc->wrap);
	}
	else if (desc->vfs == 0xffffffff)
	{
		xr_strcpy(target, source);
	}
	else
	{
		FS.update_path(target, alias, id.c_str());
	}

	IReader* reader = FS.r_open(source);
	if (reader == nullptr)
	{
		xr_sprintf(status, "Not saved: can't read [%s]", source);
		return false;
	}

	const u8* begin = static_cast<const u8*>(reader->pointer());
	xr_vector<u8> data(begin, begin + reader->length());
	FS.r_close(reader);

	IReader file(data.data(), data.size());
	CMemoryWriter output;
	xr_vector<CMotionDef*> defs;
	bool params = false;
	while (!file.eof())
	{
		const u32 chunk = file.r_u32();
		const u32 size = file.r_u32();
		if (size > u32(file.elapsed()))
		{
			xr_sprintf(status, "Not saved: [%s] is corrupted", source);
			return false;
		}

		u8* body = static_cast<u8*>(file.pointer());
		if (chunk == OGF_S_SMPARAMS)
		{
			IReader input(body, size);
			CMemoryWriter written;
			if (!OMFEditorWriteParams(input, motions, written, defs))
			{
				xr_sprintf(status, "Not saved: motions of [%s] differ from the file", source);
				return false;
			}

			output.w_u32(chunk);
			output.w_u32(written.size());
			output.w(written.pointer(), written.size());
			params = true;
		}
		else
		{
			output.w_u32(chunk);
			output.w_u32(size);
			output.w(body, size);
		}

		file.advance(size);
	}

	if (!params)
	{
		xr_sprintf(status, "Not saved: [%s] has no motion params", source);
		return false;
	}

	string_path backup = {};
	xr_strconcat(backup, target, ".bak");
	FS.file_copy(source, backup);

	IWriter* writer = FS.w_open(target);
	if (writer == nullptr)
	{
		xr_sprintf(status, "Not saved: can't write [%s]", target);
		return false;
	}

	writer->w(output.pointer(), output.size());
	FS.w_close(writer);

	for (const CMotionDef* def : defs)
	{
		OMFEditorSnapshot(def, true);
	}

	xr_sprintf(status, "Saved [%s]", target);
	Msg("OMF Editor: saved [%s], backup [%s]", target, backup);
	return true;
}

static void OMFEditorAddMotion(IKinematicsAnimated* model, const motion_descr& descr, bool bone_part)
{
	if (model == nullptr || !descr.mid.valid())
	{
		return;
	}

	for (const SOMFMotion& motion : g_omf_editor.Motions)
	{
		if (motion.Model == model && motion.Motion == descr.mid)
		{
			return;
		}
	}

	CMotionDef* def = model->LL_GetMotionDef(descr.mid);
	if (def == nullptr)
	{
		return;
	}

	OMFEditorSnapshot(def, false);

	SOMFMotion& motion = g_omf_editor.Motions.emplace_back();
	motion.Model = model;
	motion.Def = def;
	motion.Motion = descr.mid;
	motion.Name = descr.name.c_str();
	if (bone_part)
	{
		motion.Name += " [bp]";
	}
}

static shared_str OMFEditorWeaponMotionName(const player_hud_motion& motion)
{
	return motion.m_base_name != motion.m_additional_name ? motion.m_additional_name : motion.m_animations[0].name;
}

static MotionID OMFEditorWeaponMotion(attachable_hud_item* item, const player_hud_motion& motion)
{
	IKinematicsAnimated* model = item->m_model != nullptr ? item->m_model->dcast_PKinematicsAnimated() : nullptr;
	return model != nullptr && !motion.m_animations.empty() ? model->ID_Cycle_Safe(OMFEditorWeaponMotionName(motion)) : MotionID();
}

static void OMFEditorBind(attachable_hud_item* item, IKinematicsAnimated* hands)
{
	SOMFEditorState& state = g_omf_editor;
	state.Item = item;
	state.HandsModel = hands;
	state.Motions.clear();
	state.Partition = nullptr;

	IKinematicsAnimated* item_model = item->m_model != nullptr ? item->m_model->dcast_PKinematicsAnimated() : nullptr;
	IKinematicsAnimated* arms = item->m_model_combined ? item_model : hands;

	for (const player_hud_motion& motion : item->m_hand_motions.m_anims)
	{
		for (const motion_descr& descr : motion.m_animations)
		{
			OMFEditorAddMotion(arms, descr, false);
		}
	}

	for (const player_hud_motion& motion : item->m_hand_motions.m_anims)
	{
		const MotionID weapon = OMFEditorWeaponMotion(item, motion);
		if (!item->m_model_combined && weapon.valid())
		{
			OMFEditorAddMotion(item_model, motion_descr{ weapon, OMFEditorWeaponMotionName(motion) }, false);
		}
	}

	for (const attachable_hud_item_motion& motion : item->m_hand_motions.m_item_anims)
	{
		for (const motion_descr& descr : motion.m_animations)
		{
			OMFEditorAddMotion(item_model, descr, true);
		}
	}

	state.PartitionModel = arms != nullptr ? arms : item_model;
	for (u16 slot = 0, count = state.PartitionModel != nullptr ? state.PartitionModel->LL_MotionsSlotCount() : 0; slot < count && state.Partition == nullptr; ++slot)
	{
		shared_motions motions = state.PartitionModel->LL_MotionsSlot(slot);
		if (motions.partition() != nullptr && motions.partition()->count() > 0)
		{
			state.Partition = motions.partition();
		}
	}
}

static void OMFEditorMarks(SOMFMotion& motion, float length)
{
	SOMFEditorState& state = g_omf_editor;
	CMotionDef* def = motion.Def;
	const bool keys = HudMotionPlayerKeys();
	const float scale = keys ? SAMPLE_FPS : 1.f;
	float frame = 0.f;
	const bool has_frame = state.Item != nullptr && HudMotionPlayerCurrentMarkTime(state.Item, motion.Model, motion.Motion, frame);

	if (state.MarksMotion != motion.Motion)
	{
		state.MarksMotion = motion.Motion;
		state.SelectedMark = -1;
		state.SelectedInterval = -1;
	}

	if (state.SelectedMark >= int(def->marks.size()))
	{
		state.SelectedMark = -1;
	}

	motion_marks* mark = state.SelectedMark >= 0 ? &def->marks[state.SelectedMark] : nullptr;
	if (mark == nullptr || state.SelectedInterval >= int(mark->intervals.size()))
	{
		state.SelectedInterval = -1;
	}

	ImGui::SeparatorText("Motion Marks");

	const ImGuiStyle& style = ImGui::GetStyle();
	const float values = ImGui::CalcTextSize("Start:").x + ImGui::GetFontSize() * 6.f + ImGui::CalcTextSize("Frame").x + style.FramePadding.x * 2.f + style.ItemSpacing.x * 3.f;
	const float width = (ImGui::GetContentRegionAvail().x - values - style.ItemSpacing.x * 2.f) * 0.5f;
	const float button = (width - style.ItemSpacing.x) * 0.5f;
	const ImVec2 list_size(width, ImGui::GetTextLineHeightWithSpacing() * 6.f + style.FramePadding.y * 2.f);

	ImGui::BeginGroup();
	if (ImGui::BeginListBox("##marks", list_size))
	{
		for (int i = 0; i < int(def->marks.size()); ++i)
		{
			ImGui::PushID(i);
			if (ImGui::Selectable(def->marks[i].name.c_str(), i == state.SelectedMark))
			{
				state.SelectedMark = i;
				state.SelectedInterval = -1;
			}

			ImGui::PopID();
		}

		ImGui::EndListBox();
	}

	if (ImGui::Button("Add##mark", ImVec2(button, 0.f)))
	{
		state.NewMark[0] = 0;
		ImGui::OpenPopup("Add mark##omf");
	}

	ImGui::SameLine();
	ImGui::BeginDisabled(mark == nullptr);
	if (ImGui::Button("Del##mark", ImVec2(button, 0.f)))
	{
		def->marks.erase(def->marks.begin() + state.SelectedMark);
		state.SelectedMark = -1;
		state.SelectedInterval = -1;
		mark = nullptr;
	}
	ImGui::EndDisabled();
	ImGui::EndGroup();

	ImGui::SameLine();
	ImGui::BeginGroup();
	if (ImGui::BeginListBox("##intervals", list_size))
	{
		for (int i = 0; mark != nullptr && i < int(mark->intervals.size()); ++i)
		{
			const motion_marks::interval& interval = mark->intervals[i];
			ImGui::PushID(i);
			if (ImGui::Selectable(shared_str().printf("%.2f - %.2f", interval.first * scale, interval.second * scale).c_str(), i == state.SelectedInterval))
			{
				state.SelectedInterval = i;
			}

			ImGui::PopID();
		}

		ImGui::EndListBox();
	}

	ImGui::BeginDisabled(mark == nullptr);
	if (ImGui::Button("Add##interval", ImVec2(button, 0.f)))
	{
		const motion_marks::interval added(frame, std::max(length, frame));
		mark->intervals.push_back(added);
		std::sort(mark->intervals.begin(), mark->intervals.end(), [](const motion_marks::interval& l, const motion_marks::interval& r) { return l.first < r.first; });
		state.SelectedInterval = int(std::find(mark->intervals.begin(), mark->intervals.end(), added) - mark->intervals.begin());
	}
	ImGui::EndDisabled();

	ImGui::SameLine();
	ImGui::BeginDisabled(mark == nullptr || state.SelectedInterval < 0);
	if (ImGui::Button("Del##interval", ImVec2(button, 0.f)))
	{
		mark->intervals.erase(mark->intervals.begin() + state.SelectedInterval);
		state.SelectedInterval = -1;
	}
	ImGui::EndDisabled();
	ImGui::EndGroup();

	ImGui::SameLine();
	ImGui::BeginGroup();
	motion_marks::interval* interval = mark != nullptr && state.SelectedInterval >= 0 ? &mark->intervals[state.SelectedInterval] : nullptr;
	const float label = ImGui::CalcTextSize("Start:").x + style.ItemSpacing.x;
	const auto value_field = [&](const char* name, const char* id, bool start)
	{
		ImGui::AlignTextToFramePadding();
		ImGui::TextUnformatted(name);
		ImGui::SameLine(label);
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 6.f);

		float value = interval != nullptr ? (start ? interval->first : interval->second) * scale : 0.f;
		ImGui::BeginDisabled(interval == nullptr);
		if (ImGui::DragFloat(id, &value, keys ? 0.1f : 0.005f, 0.f, 0.f, "%.2f"))
		{
			(start ? interval->first : interval->second) = value / scale;
		}

		ImGui::SameLine();
		ImGui::BeginDisabled(!has_frame);
		if (ImGui::SmallButton(start ? "Frame##start" : "Frame##end"))
		{
			if (start)
			{
				interval->first = frame;
				interval->second = std::max(interval->second, frame);
			}
			else
			{
				interval->second = frame;
				interval->first = std::min(interval->first, frame);
			}
		}

		ImGui::SetItemTooltip("Set to the current frame of the motion player");
		ImGui::EndDisabled();
		ImGui::EndDisabled();
	};

	value_field("Start:", "##start", true);
	value_field("End:", "##end", false);
	ImGui::EndGroup();

	if (ImGui::BeginPopup("Add mark##omf"))
	{
		const auto has_mark = [def](const char* name) { return std::any_of(def->marks.begin(), def->marks.end(), [name](const motion_marks& it) { return xr_strcmp(it.name, name) == 0; }); };
		const auto add_mark = [&](const char* name)
		{
			motion_marks& added = def->marks.emplace_back();
			added.name = name;
			added.intervals.emplace_back(frame, std::max(length, frame));
			state.SelectedMark = int(def->marks.size()) - 1;
			state.SelectedInterval = 0;
			ImGui::CloseCurrentPopup();
		};

		xr_vector<shared_str> names = { HudAttachMarks::Ik, HudAttachMarks::Fingers, "mm_unpend", "Left" };
		if (state.Item != nullptr)
		{
			state.Item->UpdateAttachPose();
			size_t front = 0;
			for (const shared_str& mark : { state.Item->IkMark, state.Item->FingersMark })
			{
				if (std::find(names.begin(), names.end(), mark) == names.end())
				{
					names.insert(names.begin() + front++, mark);
				}
			}
		}

		const size_t fixed = names.size();
		for (const SOMFMotion& it : state.Motions)
		{
			for (const motion_marks& marks : it.Def->marks)
			{
				if (std::find(names.begin(), names.end(), marks.name) == names.end())
				{
					names.push_back(marks.name);
				}
			}
		}

		std::sort(names.begin() + fixed, names.end(), [](const shared_str& a, const shared_str& b) { return xr_strcmp(a, b) < 0; });

		for (const shared_str& name : names)
		{
			if (!has_mark(name.c_str()) && ImGui::Selectable(name.c_str()))
			{
				add_mark(name.c_str());
			}
		}

		ImGui::Separator();
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 11.f);
		const bool enter = ImGui::InputTextWithHint("##name", "custom mark", state.NewMark, sizeof(state.NewMark), ImGuiInputTextFlags_EnterReturnsTrue);
		const bool valid = state.NewMark[0] != 0 && !has_mark(state.NewMark);
		ImGui::SameLine();
		ImGui::BeginDisabled(!valid);
		if (ImGui::Button("OK") || enter && valid)
		{
			add_mark(state.NewMark);
		}
		ImGui::EndDisabled();
		ImGui::EndPopup();
	}
}

static void OMFEditorMotion(SOMFMotion& motion)
{
	SOMFEditorState& state = g_omf_editor;
	CMotionDef* def = motion.Def;

	CMotion* root = motion.Model->LL_GetRootMotion(motion.Motion);
	const float length = root != nullptr ? root->GetLength() : 0.f;
	const float speed = def->Speed();
	ImGui::Text("Length: %.0f keys = %.3f s", length * SAMPLE_FPS, speed > EPS ? length / speed : 0.f);
	ImGui::SetItemTooltip("Keys: frames in the motion (30 per second)\nSeconds: real duration in game at the OMF speed %.3f (keys / 30 / speed)", speed);

	if (state.Item != nullptr)
	{
		RenderHudMotionPlayerSpeed(state.Item);
	}

	if (ImGui::Button("Reset motion"))
	{
		OMFEditorReset(def);
		state.SelectedMark = -1;
		state.SelectedInterval = -1;
	}

	ImGui::SetItemTooltip("Restore flags and marks of this motion as they were loaded or last saved");
	ImGui::SameLine();
	if (ImGui::Button("Save OMF"))
	{
		OMFEditorSave(motion);
	}

	ImGui::SetItemTooltip("Write flags and marks of all motions of this .omf file to disk, the old file is kept as .bak");
	if (state.SaveStatus[0] != 0)
	{
		ImGui::SameLine();
		ImGui::TextDisabled("%s", state.SaveStatus);
	}

	if (ImGui::TreeNode("Flags"))
	{
		if (ImGui::BeginTable("##flags", 3))
		{
			for (const auto& flag : OMFMotionFlags)
			{
				ImGui::TableNextColumn();
				bool enabled = (def->flags & flag.Bit) != 0;
				if (ImGui::Checkbox(flag.Name, &enabled))
				{
					def->flags = enabled ? u16(def->flags | flag.Bit) : u16(def->flags & ~flag.Bit);
				}
			}

			ImGui::EndTable();
		}

		ImGui::TreePop();
	}

	OMFEditorMarks(motion, length);
}

static void OMFEditorBoneParts()
{
	SOMFEditorState& state = g_omf_editor;
	if (state.Partition == nullptr || state.PartitionModel == nullptr || !ImGui::CollapsingHeader("Bone parts"))
	{
		return;
	}

	IKinematics* kinematics = state.PartitionModel->dcast_PKinematics();
	for (u16 part = 0, count = state.Partition->count(); part < count; ++part)
	{
		const CPartDef& def = state.Partition->part(part);
		if (ImGui::TreeNode(def.Name.c_str(), "%s (%u)", def.Name.c_str(), u32(def.bones.size())))
		{
			for (u32 bone : def.bones)
			{
				ImGui::BulletText("%s", kinematics->LL_BoneName_dbg(u16(bone)));
			}

			ImGui::TreePop();
		}
	}
}

void OMFEditor_OnPressed(int key)
{
	if (key == SDL_SCANCODE_ESCAPE && Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)])
	{
		SRequestData req;
		req.editor_type = u32(eImGuiEditorType::kOMFEditor);
		req.request_type = u32(eRequestType_OMFEditor::kDeselectCurrentSelectedOrHideWindow);
		AllEditors_SendRequest(req);
	}
}

void OMFEditor_OnReleased(int key)
{
}

void RequestHandler_OMFEditor(const SRequestData& req)
{
	switch (static_cast<eRequestType_OMFEditor>(req.request_type))
	{
	case eRequestType_OMFEditor::kDeselectCurrentSelectedOrHideWindow:
		if (!ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId))
		{
			Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)] = false;
		}
		break;
	case eRequestType_OMFEditor::kShutdown:
		g_omf_editor = {};
		break;
	default:
		break;
	}
}

void RenderToolsOMFEditorWindow()
{
	SOMFEditorState& state = g_omf_editor;
	bool& open = Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)];
	if (!open)
	{
		HudMotionPlayerClose();
		state.Item = nullptr;
		return;
	}

	if (!ImGui::Begin("OMF Editor##ToolsInGameImGui", &open))
	{
		ImGui::End();
		return;
	}

	CActor* actor = g_pGameLevel != nullptr && ai().get_alife() != nullptr && g_actor != nullptr ? Actor() : nullptr;
	PIItem active = actor != nullptr && actor->GetfHealth() > 0.001f ? actor->inventory().ActiveItem() : nullptr;
	CHudItem* hud_item = active != nullptr ? active->cast_hud_item() : nullptr;
	attachable_hud_item* item = hud_item != nullptr && g_player_hud != nullptr ? hud_item->HudItemData() : nullptr;

	if (item == nullptr)
	{
		HudMotionPlayerClose();
		state.Item = nullptr;
		ImGui::TextDisabled("No active HUD item");
		ImGui::End();
		return;
	}

	if (state.Item != item || state.HandsModel != g_player_hud->GetModel())
	{
		OMFEditorBind(item, g_player_hud->GetModel());
	}

	if (ImGui::CollapsingHeader("Motion Player", ImGuiTreeNodeFlags_DefaultOpen))
	{
		RenderHudMotionPlayer();
	}

	if (const player_hud_motion* anm = HudMotionPlayerSelectedAnim(item))
	{
		ImGui::Separator();
		if (!item->m_model_combined)
		{
			if (ImGui::RadioButton("Hands", !state.Weapon))
			{
				state.Weapon = false;
			}

			ImGui::SameLine();
			if (ImGui::RadioButton("Weapon", state.Weapon))
			{
				state.Weapon = true;
			}
		}

		const bool weapon = state.Weapon && !item->m_model_combined;
		IKinematicsAnimated* model = weapon ? item->m_model->dcast_PKinematicsAnimated() : item->m_model_combined ? item->m_model->dcast_PKinematicsAnimated() : g_player_hud->GetModel();
		const MotionID selected = weapon ? OMFEditorWeaponMotion(item, *anm) : anm->m_animations[0].mid;
		const auto motion = std::find_if(state.Motions.begin(), state.Motions.end(), [&](const SOMFMotion& it) { return it.Model == model && it.Motion == selected; });
		if (motion != state.Motions.end())
		{
			ImGui::SameLine();
			ImGui::TextDisabled("%s", motion->Name.c_str());
			OMFEditorMotion(*motion);
		}
		else
		{
			ImGui::TextDisabled("The weapon has no motion [%s]", OMFEditorWeaponMotionName(*anm).c_str());
		}
	}

	OMFEditorBoneParts();

	ImGui::End();
}
