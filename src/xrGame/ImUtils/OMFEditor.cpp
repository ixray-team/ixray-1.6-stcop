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
		shared_str				MotionName;
		xr_string				File;
		xr_vector<xr_string>	Duplicates;
		u32						FileCopies = 1;
	};

	struct SOMFFileMotion
	{
		shared_str				Name;
		u32						Flags = 0;
		u32						Keys = 0;
	};

	struct SOMFNameStats
	{
		u32						Copies = 0;
		u32						First = 0;
	};

	struct SOMFChunk
	{
		u32						Id = 0;
		const u8*				Data = nullptr;
		u32						Size = 0;
	};

	struct SOMFParamsDef
	{
		string256				Name = {};
		u32						Flags = 0;
		u16						BoneOrPart = 0;
		u16						Motion = 0;
		float					Values[4] = {};
		u32						Marks = 0;
		const u8*				MarksBegin = nullptr;
		const u8*				MarksEnd = nullptr;
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
		int						EditField = 0;
		bool					EditFocus = false;
		xr_map<shared_str, xr_vector<SOMFFileMotion>>	FileMotions;
		xr_set<shared_str>		ChangedFiles;
		shared_str				ListFile;
		xr_vector<u8>			ListDelete;
		string512				ListStatus = {};
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

	constexpr u32 OMFMotionFx = 1 << 0;
	const ImVec4 OMFWarningColor(1.f, 0.75f, 0.2f, 1.f);
	const ImVec4 OMFSelectedColor(0.45f, 0.85f, 0.45f, 1.f);
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

static SOMFMotion* OMFEditorFindMotion(IKinematicsAnimated* model, MotionID id)
{
	if (model == nullptr || !id.valid())
	{
		return nullptr;
	}

	for (SOMFMotion& motion : g_omf_editor.Motions)
	{
		if (motion.Model == model && motion.Motion == id)
		{
			return &motion;
		}
	}

	return nullptr;
}

static u32 OMFEditorMarkedCount(const xr_vector<u8>& marks)
{
	return u32(std::count(marks.begin(), marks.end(), u8(1)));
}

static void OMFEditorWriteChunk(IWriter& output, u32 id, const void* data, u32 size)
{
	output.w_u32(id);
	output.w_u32(size);
	output.w(data, size);
}

static bool OMFEditorReadChunks(IReader& input, xr_vector<SOMFChunk>& chunks)
{
	while (!input.eof())
	{
		SOMFChunk& chunk = chunks.emplace_back();
		chunk.Id = input.r_u32();
		chunk.Size = input.r_u32();
		if (chunk.Size > u32(input.elapsed()))
		{
			return false;
		}

		chunk.Data = static_cast<const u8*>(input.pointer());
		input.advance(chunk.Size);
	}

	return true;
}

static void OMFEditorCopyPartitions(IReader& input, IWriter* output)
{
	string256 name;
	const u16 parts = input.r_u16();
	if (output != nullptr)
	{
		output->w_u16(parts);
	}

	for (u16 part = 0; part < parts; ++part)
	{
		input.r_stringZ(name, sizeof(name));
		const u16 bones = input.r_u16();
		if (output != nullptr)
		{
			output->w_stringZ(name);
			output->w_u16(bones);
		}

		for (u16 bone = 0; bone < bones; ++bone)
		{
			input.r_stringZ(name, sizeof(name));
			const u32 id = input.r_u32();
			if (output != nullptr)
			{
				output->w_stringZ(name);
				output->w_u32(id);
			}
		}
	}
}

static void OMFEditorReadDef(IReader& input, u16 version, SOMFParamsDef& def)
{
	input.r_stringZ(def.Name, sizeof(def.Name));
	def.Flags = input.r_u32();
	def.BoneOrPart = input.r_u16();
	def.Motion = input.r_u16();
	input.r(def.Values, sizeof(def.Values));
	if (version < 4)
	{
		return;
	}

	def.Marks = input.r_u32();
	def.MarksBegin = static_cast<const u8*>(input.pointer());
	for (u32 mark = 0; mark < def.Marks; ++mark)
	{
		motion_marks skipped;
		skipped.Load(&input);
	}

	def.MarksEnd = static_cast<const u8*>(input.pointer());
}

static bool OMFEditorWriteParams(IReader& input, shared_motions& motions, IWriter& output, xr_vector<CMotionDef*>& defs, const xr_vector<u8>* remove, bool from_file)
{
	const u16 version = input.r_u16();
	output.w_u16(xrOGF_SMParamsVersion);
	OMFEditorCopyPartitions(input, &output);

	const u16 count = input.r_u16();
	const MotionDefVec* loaded = motions.motion_defs();
	if ((!from_file && (loaded == nullptr || loaded->size() != count)) || (remove != nullptr && remove->size() != count))
	{
		return false;
	}

	output.w_u16(remove != nullptr ? u16(count - OMFEditorMarkedCount(*remove)) : count);
	u16 written = 0;
	for (u16 index = 0; index < count; ++index)
	{
		SOMFParamsDef def;
		OMFEditorReadDef(input, version, def);
		if (remove != nullptr && (*remove)[index] != 0)
		{
			continue;
		}

		const u16 motion = remove != nullptr ? written : def.Motion;
		++written;

		CMotionDef* edited = from_file ? nullptr : motions.motion_def(index);
		output.w_stringZ(def.Name);
		output.w_u32(edited != nullptr ? (def.Flags & 0xffff0000) | edited->flags : def.Flags);
		output.w_u16(def.BoneOrPart);
		output.w_u16(motion);

		if (edited == nullptr)
		{
			output.w(def.Values, sizeof(def.Values));
			output.w_u32(def.Marks);
			if (def.MarksEnd > def.MarksBegin)
			{
				output.w(def.MarksBegin, u32(def.MarksEnd - def.MarksBegin));
			}

			continue;
		}

		output.w_float(edited->Dequantize(edited->speed));
		output.w_float(edited->Dequantize(edited->power));
		output.w_float(edited->Dequantize(edited->accrue));
		output.w_float(edited->Dequantize(edited->falloff));
		output.w_u32(u32(edited->marks.size()));
		for (motion_marks& mark : edited->marks)
		{
			mark.Save(&output);
		}

		defs.push_back(edited);
	}

	return true;
}

static bool OMFEditorWriteMotions(IReader& input, const xr_vector<u8>& remove, IWriter& output)
{
	xr_vector<SOMFChunk> chunks;
	if (!OMFEditorReadChunks(input, chunks))
	{
		return false;
	}

	const auto find = [&](u32 id) -> const SOMFChunk*
	{
		const auto it = std::find_if(chunks.begin(), chunks.end(), [id](const SOMFChunk& chunk) { return chunk.Id == id; });
		return it != chunks.end() ? &*it : nullptr;
	};

	const SOMFChunk* header = find(0);
	if (header == nullptr || header->Size < sizeof(u32))
	{
		return false;
	}

	const u32 count = *reinterpret_cast<const u32*>(header->Data);
	if (count != remove.size())
	{
		return false;
	}

	for (const SOMFChunk& chunk : chunks)
	{
		const bool known = chunk.Id <= count || (chunk.Id >= count + 2 && chunk.Id < count * 2 + 2);
		if ((chunk.Id & CFS_CompressMark) != 0 || !known)
		{
			return false;
		}
	}

	const u32 kept = count - OMFEditorMarkedCount(remove);
	OMFEditorWriteChunk(output, 0, &kept, sizeof(kept));

	for (u32 index = 0, written = 0; index < count; ++index)
	{
		if (remove[index] != 0)
		{
			continue;
		}

		const SOMFChunk* motion = find(index + 1);
		if (motion == nullptr)
		{
			return false;
		}

		OMFEditorWriteChunk(output, ++written, motion->Data, motion->Size);
	}

	for (u32 index = 0, written = 0; index < count; ++index)
	{
		if (remove[index] != 0)
		{
			continue;
		}

		if (const SOMFChunk* notify = find(count + 2 + index))
		{
			OMFEditorWriteChunk(output, kept + 2 + written, notify->Data, notify->Size);
		}

		++written;
	}

	return true;
}

static const CLocatorAPI::file* OMFEditorFind(const shared_str& id, string_path& source, const char*& alias)
{
	if (id.size() == 0)
	{
		return nullptr;
	}

	alias = "$level$";
	const CLocatorAPI::file* desc = FS.exist(source, alias, id.c_str());
	if (desc == nullptr)
	{
		alias = "$game_meshes$";
		desc = FS.exist(source, alias, id.c_str());
	}

	return desc;
}

static xr_string OMFEditorShownPath(const shared_str& id)
{
	string_path source = {};
	const char* alias = nullptr;
	const CLocatorAPI::file* desc = OMFEditorFind(id, source, alias);
	if (desc == nullptr)
	{
		return id.size() != 0 ? id.c_str() : "?";
	}

	const xr_string full = desc->wrap != nullptr ? desc->wrap : source;
	xr_string lower = full;
	xr_strlwr(lower);

	size_t start = lower.find("ixr_addons");
	if (start == xr_string::npos)
	{
		start = lower.find("gamedata");
	}

	xr_string shown = full;
	if (start != xr_string::npos)
	{
		shown = full.substr(start).c_str();
	}
	if (desc->wrap == nullptr && desc->vfs != 0xffffffff)
	{
		shown += " (archive)";
	}

	return shown;
}

static const xr_vector<SOMFFileMotion>& OMFEditorFileMotions(const shared_str& id)
{
	auto [it, inserted] = g_omf_editor.FileMotions.try_emplace(id);
	xr_vector<SOMFFileMotion>& motions = it->second;
	string_path source = {};
	const char* alias = nullptr;
	IReader* reader = inserted && OMFEditorFind(id, source, alias) != nullptr ? FS.r_open(source) : nullptr;
	if (reader == nullptr)
	{
		return motions;
	}

	if (IReader* params = reader->open_chunk(OGF_S_SMPARAMS))
	{
		const u16 version = params->r_u16();
		OMFEditorCopyPartitions(*params, nullptr);
		for (u16 index = 0, count = params->r_u16(); index < count; ++index)
		{
			SOMFParamsDef def;
			OMFEditorReadDef(*params, version, def);

			SOMFFileMotion& motion = motions.emplace_back();
			motion.Name = _strlwr(def.Name);
			motion.Flags = def.Flags;
		}

		params->close();
	}

	if (IReader* keys = reader->open_chunk(OGF_S_MOTIONS))
	{
		string256 name;
		for (u32 index = 0; index < motions.size() && keys->find_chunk(index + 1) != 0; ++index)
		{
			keys->r_stringZ(name, sizeof(name));
			motions[index].Keys = keys->r_u32();
		}

		keys->close();
	}

	FS.r_close(reader);
	return motions;
}

static void OMFEditorTargetPath(const CLocatorAPI::file* desc, const char* source, const char* alias, const shared_str& id, string_path& target)
{
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
}

static bool OMFEditorRewrite(const SOMFMotion& motion, const xr_vector<u8>* remove, string512& status)
{
	shared_motions motions = motion.Model->LL_MotionsSlot(motion.Motion.slot);
	const shared_str id = motions.id();
	const bool from_file = g_omf_editor.ChangedFiles.contains(id);
	if (from_file && remove == nullptr)
	{
		xr_sprintf(status, "Not saved: motions were deleted from this file, restart the game first");
		return false;
	}

	string_path source = {};
	const char* alias = nullptr;
	const CLocatorAPI::file* desc = OMFEditorFind(id, source, alias);
	if (desc == nullptr)
	{
		xr_sprintf(status, "Not saved: [%s] is not an .omf file", id.size() ? id.c_str() : "?");
		return false;
	}

	string_path target = {};
	OMFEditorTargetPath(desc, source, alias, id, target);

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
	xr_vector<SOMFChunk> chunks;
	if (!OMFEditorReadChunks(file, chunks))
	{
		xr_sprintf(status, "Not saved: [%s] is corrupted", source);
		return false;
	}

	CMemoryWriter output;
	xr_vector<CMotionDef*> defs;
	bool params = false;
	bool keys = remove == nullptr;
	for (const SOMFChunk& chunk : chunks)
	{
		IReader input(const_cast<u8*>(chunk.Data), chunk.Size);
		CMemoryWriter written;
		if (chunk.Id == OGF_S_SMPARAMS)
		{
			if (!OMFEditorWriteParams(input, motions, written, defs, remove, from_file))
			{
				xr_sprintf(status, "Not saved: motions of [%s] differ from the file", source);
				return false;
			}

			params = true;
		}
		else if (chunk.Id == OGF_S_MOTIONS && remove != nullptr)
		{
			if (!OMFEditorWriteMotions(input, *remove, written))
			{
				xr_sprintf(status, "Not saved: keys of [%s] have an unknown layout", source);
				return false;
			}

			keys = true;
		}
		else
		{
			OMFEditorWriteChunk(output, chunk.Id, chunk.Data, chunk.Size);
			continue;
		}

		OMFEditorWriteChunk(output, chunk.Id, written.pointer(), written.size());
	}

	if (!params || !keys)
	{
		xr_sprintf(status, "Not saved: [%s] has no motion params or keys", source);
		return false;
	}

	IWriter* writer = FS.w_open(target);
	if (writer == nullptr)
	{
		xr_sprintf(status, "Not saved: can't write [%s]", target);
		return false;
	}

	writer->w(output.pointer(), output.size());
	FS.w_close(writer);

	if (remove != nullptr)
	{
		g_omf_editor.ChangedFiles.insert(id);
		g_omf_editor.FileMotions.erase(id);
		xr_sprintf(status, "Deleted %u motions, restart the game to reload the file", OMFEditorMarkedCount(*remove));
		Msg("OMF Editor: deleted motions from [%s]", target);
		return true;
	}

	for (const CMotionDef* def : defs)
	{
		OMFEditorSnapshot(def, true);
	}

	xr_sprintf(status, "Saved [%s]", target);
	Msg("OMF Editor: saved [%s]", target);
	return true;
}

static bool OMFEditorSave(const SOMFMotion& motion)
{
	return OMFEditorRewrite(motion, nullptr, g_omf_editor.SaveStatus);
}

static bool OMFEditorSlotHasMotion(shared_motions& motions, const shared_str& name)
{
	const accel_map* cycles = motions.cycle();
	const accel_map* fxs = motions.fx();
	return (cycles != nullptr && cycles->contains(name)) || (fxs != nullptr && fxs->contains(name));
}

static void OMFEditorAddMotion(IKinematicsAnimated* model, const motion_descr& descr, bool bone_part)
{
	if (model == nullptr || !descr.mid.valid() || OMFEditorFindMotion(model, descr.mid) != nullptr)
	{
		return;
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
	motion.MotionName = descr.name;
	motion.Name = descr.name.c_str();
	if (bone_part)
	{
		motion.Name += " [bp]";
	}

	const shared_str file = model->LL_MotionsSlot(descr.mid.slot).id();
	motion.File = OMFEditorShownPath(file);

	const xr_vector<SOMFFileMotion>& file_motions = OMFEditorFileMotions(file);
	motion.FileCopies = u32(std::count_if(file_motions.begin(), file_motions.end(), [&](const SOMFFileMotion& it) { return it.Name == descr.name; }));

	for (u16 slot = 0, count = model->LL_MotionsSlotCount(); slot < count; ++slot)
	{
		shared_motions motions = model->LL_MotionsSlot(slot);
		if (slot != descr.mid.slot && OMFEditorSlotHasMotion(motions, descr.name))
		{
			motion.Duplicates.push_back(OMFEditorShownPath(motions.id()));
		}
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
	state.FileMotions.clear();
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
	const float mark_speed = def->Speed();
	const float mark_length = mark_speed <= EPS || fsimilar(mark_speed, 1.f) ? length : length / mark_speed;

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
			ImGui::PushStyleColor(ImGuiCol_Text, HudMotionPlayerMarkColor(def->marks[i].name));
			if (ImGui::Selectable(def->marks[i].name.c_str(), i == state.SelectedMark))
			{
				state.SelectedMark = i;
				state.SelectedInterval = -1;
			}

			ImGui::PopStyleColor();
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
			ImGui::PushStyleColor(ImGuiCol_Text, HudMotionPlayerMarkColor(mark->name));
			if (ImGui::Selectable(shared_str().printf("%.2f - %.2f", interval.first * scale, interval.second * scale).c_str(), i == state.SelectedInterval))
			{
				state.SelectedInterval = i;
			}

			ImGui::PopStyleColor();
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
		const int field = start ? 1 : 2;
		if (interval == nullptr && state.EditField == field)
		{
			state.EditField = 0;
		}

		ImGui::AlignTextToFramePadding();
		if (mark != nullptr)
		{
			ImGui::TextColored(ImGui::ColorConvertU32ToFloat4(HudMotionPlayerMarkColor(mark->name)), "%s", name);
		}
		else
		{
			ImGui::TextUnformatted(name);
		}

		ImGui::SameLine(label);
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 6.f);

		float value = interval != nullptr ? (start ? interval->first : interval->second) * scale : 0.f;
		ImGui::BeginDisabled(interval == nullptr);
		if (state.EditField == field)
		{
			if (state.EditFocus)
			{
				ImGui::SetKeyboardFocusHere();
				state.EditFocus = false;
			}

			if (ImGui::InputFloat(id, &value, 0.f, 0.f, "%.2f", ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_AutoSelectAll))
			{
				(start ? interval->first : interval->second) = std::clamp(value / scale, 0.f, mark_length);
				state.EditField = 0;
			}
			else if (ImGui::IsItemDeactivated())
			{
				state.EditField = 0;
			}
		}
		else
		{
			if (ImGui::DragFloat(id, &value, keys ? 0.1f : 0.005f, 0.f, 0.f, "%.2f", ImGuiSliderFlags_NoInput))
			{
				(start ? interval->first : interval->second) = std::clamp(value / scale, 0.f, mark_length);
			}

			if (interval != nullptr && ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) && state.Item != nullptr)
			{
				HudMotionPlayerSeekMark(state.Item, motion.Model, motion.Motion, start ? interval->first : interval->second);
			}

			if (interval != nullptr && ImGui::IsItemClicked(ImGuiMouseButton_Right))
			{
				state.EditField = field;
				state.EditFocus = true;
			}

			ImGui::SetItemTooltip("Drag to change, double click to move the player here, right click to type the value");
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

	ImGui::SetItemTooltip("Write flags and marks of all motions of this .omf file to disk");
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

static void OMFEditorAppendUse(xr_map<shared_str, xr_string>& used, const shared_str& motion, const shared_str& alias)
{
	xr_string& list = used[motion];
	if (list.find(alias.c_str()) != xr_string::npos)
	{
		return;
	}

	if (!list.empty())
	{
		list += ", ";
	}

	list += alias.c_str();
}

static xr_map<shared_str, xr_string> OMFEditorMotionUses(attachable_hud_item* item, bool hands_file)
{
	xr_map<shared_str, xr_string> used;
	for (const player_hud_motion& anm : item->m_hand_motions.m_anims)
	{
		if (hands_file)
		{
			for (const motion_descr& descr : anm.m_animations)
			{
				OMFEditorAppendUse(used, descr.name, anm.m_alias_name);
			}
		}
		else if (!anm.m_animations.empty())
		{
			OMFEditorAppendUse(used, OMFEditorWeaponMotionName(anm), anm.m_alias_name);
		}
	}

	if (!hands_file || item->m_model_combined)
	{
		for (const attachable_hud_item_motion& anm : item->m_hand_motions.m_item_anims)
		{
			for (const motion_descr& descr : anm.m_animations)
			{
				OMFEditorAppendUse(used, descr.name, anm.m_alias_name);
			}
		}
	}

	return used;
}

static void OMFEditorFileTable(const SOMFMotion& motion, const xr_vector<SOMFFileMotion>& motions, const xr_map<shared_str, xr_string>& used, bool changed)
{
	SOMFEditorState& state = g_omf_editor;
	xr_map<shared_str, SOMFNameStats> stats;
	for (u32 i = 0; i < motions.size(); ++i)
	{
		SOMFNameStats& name = stats[motions[i].Name];
		name.First = name.Copies == 0 ? i : name.First;
		++name.Copies;
	}

	const ImGuiStyle& style = ImGui::GetStyle();
	const float row = ImGui::GetFrameHeight() + style.CellPadding.y * 2.f;
	const float height = row * float(std::min<size_t>(motions.size(), 14) + 1) + style.ScrollbarSize;
	const ImGuiTableFlags flags = ImGuiTableFlags_RowBg | ImGuiTableFlags_Borders | ImGuiTableFlags_PadOuterX | ImGuiTableFlags_ScrollY | ImGuiTableFlags_ScrollX | ImGuiTableFlags_SizingFixedFit;
	if (!ImGui::BeginTable("##file_motions", 6, flags, ImVec2(0.f, height)))
	{
		return;
	}

	ImGui::TableSetupScrollFreeze(0, 1);
	ImGui::TableSetupColumn("Del", ImGuiTableColumnFlags_WidthFixed, std::max(ImGui::GetFrameHeight(), ImGui::CalcTextSize("Del").x));
	ImGui::TableSetupColumn("#");
	ImGui::TableSetupColumn("Name");
	ImGui::TableSetupColumn("Type");
	ImGui::TableSetupColumn("Length");
	ImGui::TableSetupColumn("Used by");
	ImGui::TableHeadersRow();

	for (u32 i = 0; i < motions.size(); ++i)
	{
		const SOMFFileMotion& it = motions[i];
		const SOMFNameStats& name = stats[it.Name];
		const bool current = changed ? it.Name == motion.MotionName : i == motion.Motion.idx;

		ImGui::PushID(int(i));
		ImGui::TableNextRow();
		ImGui::TableNextColumn();
		bool remove = state.ListDelete[i] != 0;
		if (ImGui::Checkbox("##delete", &remove))
		{
			state.ListDelete[i] = remove ? 1 : 0;
		}

		ImGui::TableNextColumn();
		ImGui::Text("%u", i);

		ImGui::TableNextColumn();
		if (current || name.Copies > 1)
		{
			ImGui::TextColored(current ? OMFSelectedColor : OMFWarningColor, "%s", it.Name.c_str());
		}
		else
		{
			ImGui::TextUnformatted(it.Name.c_str());
		}

		if (name.Copies > 1 && name.First != i)
		{
			ImGui::SameLine();
			ImGui::TextDisabled("(not played)");
		}

		ImGui::TableNextColumn();
		ImGui::TextUnformatted((it.Flags & OMFMotionFx) != 0 ? "fx" : "cycle");

		ImGui::TableNextColumn();
		ImGui::Text("%u keys, %.2f s", it.Keys, float(it.Keys) / SAMPLE_FPS);

		ImGui::TableNextColumn();
		if (const auto use = used.find(it.Name); use != used.end())
		{
			ImGui::TextDisabled("%s", use->second.c_str());
		}

		ImGui::PopID();
	}

	ImGui::EndTable();
}

static void OMFEditorDeletePopup(SOMFMotion& motion, const xr_vector<SOMFFileMotion>& motions, const xr_map<shared_str, xr_string>& used, bool changed)
{
	SOMFEditorState& state = g_omf_editor;
	if (!ImGui::BeginPopupModal("Delete motions##omf", nullptr, ImGuiWindowFlags_AlwaysAutoResize))
	{
		return;
	}

	const auto kept = [&](const shared_str& name)
	{
		for (u32 i = 0; i < motions.size(); ++i)
		{
			if (state.ListDelete[i] == 0 && motions[i].Name == name)
			{
				return true;
			}
		}

		return false;
	};

	ImGui::Text("Delete %u motions from", OMFEditorMarkedCount(state.ListDelete));
	ImGui::TextDisabled("%s", motion.File.c_str());
	ImGui::Spacing();

	for (u32 i = 0; i < motions.size(); ++i)
	{
		if (state.ListDelete[i] == 0)
		{
			continue;
		}

		const SOMFFileMotion& it = motions[i];
		ImGui::BulletText("%u  %s", i, it.Name.c_str());

		const auto use = used.find(it.Name);
		if (use != used.end() && !kept(it.Name))
		{
			ImGui::SameLine();
			ImGui::TextColored(OMFWarningColor, "still used by %s", use->second.c_str());
		}
	}

	ImGui::Spacing();
	if (!changed)
	{
		ImGui::TextDisabled("Unsaved flag and mark edits of this file are written too.");
	}

	ImGui::TextDisabled("Restart the game to reload the file.");
	ImGui::Spacing();

	bool done = false;
	if (ImGui::Button("Delete"))
	{
		done = true;
		if (OMFEditorRewrite(motion, &state.ListDelete, state.ListStatus))
		{
			state.Item = nullptr;
		}
	}

	ImGui::SameLine();
	done = ImGui::Button("Cancel") || done;
	if (done)
	{
		ImGui::CloseCurrentPopup();
	}

	ImGui::EndPopup();
}

static void OMFEditorFileList(SOMFMotion& motion, attachable_hud_item* item)
{
	SOMFEditorState& state = g_omf_editor;
	const shared_str id = motion.Model->LL_MotionsSlot(motion.Motion.slot).id();
	const xr_vector<SOMFFileMotion>& motions = OMFEditorFileMotions(id);

	string128 header = {};
	xr_sprintf(header, "File motions (%u)###omf_file_motions", u32(motions.size()));
	if (!ImGui::CollapsingHeader(header))
	{
		return;
	}

	if (state.ListFile != id)
	{
		state.ListFile = id;
		state.ListStatus[0] = 0;
	}

	if (state.ListDelete.size() != motions.size())
	{
		state.ListDelete.assign(motions.size(), 0);
	}

	const bool changed = state.ChangedFiles.contains(id);
	ImGui::TextDisabled("File: %s", motion.File.c_str());
	if (changed)
	{
		ImGui::TextColored(OMFWarningColor, "Motions were deleted from this file, restart the game to reload it. Save OMF is off until then");
	}

	const xr_map<shared_str, xr_string> used = OMFEditorMotionUses(item, item->m_model_combined || motion.Model == state.HandsModel);
	OMFEditorFileTable(motion, motions, used, changed);

	const u32 selected = OMFEditorMarkedCount(state.ListDelete);
	string64 label = {};
	xr_sprintf(label, "Delete selected (%u)###omf_delete", selected);
	ImGui::BeginDisabled(selected == 0 || selected == motions.size());
	if (ImGui::Button(label))
	{
		ImGui::OpenPopup("Delete motions##omf");
	}

	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Remove the checked motions from the .omf file");

	ImGui::SameLine();
	ImGui::BeginDisabled(selected == 0);
	if (ImGui::Button("Clear selection"))
	{
		std::fill(state.ListDelete.begin(), state.ListDelete.end(), u8(0));
	}

	ImGui::EndDisabled();
	if (state.ListStatus[0] != 0)
	{
		ImGui::SameLine();
		ImGui::TextDisabled("%s", state.ListStatus);
	}

	OMFEditorDeletePopup(motion, motions, used, changed);
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

static void OMFEditorMotionInfo(const char* label, const SOMFMotion* motion, const char* name)
{
	if (motion == nullptr)
	{
		ImGui::TextDisabled("%s: none [%s]", label, name);
		return;
	}

	ImGui::Text("%s: %s", label, motion->Name.c_str());
	ImGui::TextDisabled("File: %s", motion->File.c_str());
}

static u32 OMFEditorWarningCount(const SOMFMotion* motion)
{
	return motion != nullptr ? u32(motion->FileCopies > 1) + u32(!motion->Duplicates.empty()) : 0;
}

static bool OMFEditorWarningBegin(const char* id, const char* title)
{
	ImGui::Spacing();
	ImGui::TextColored(OMFWarningColor, "(!) %s", title);
	ImGui::Indent();
	if (ImGui::BeginTable(id, 2, ImGuiTableFlags_SizingFixedFit))
	{
		return true;
	}

	ImGui::Unindent();
	return false;
}

static void OMFEditorWarningEnd()
{
	ImGui::EndTable();
	ImGui::Unindent();
}

static void OMFEditorWarningRow(const char* label, const char* value, bool dim = false)
{
	ImGui::TableNextRow();
	ImGui::TableNextColumn();
	ImGui::TextDisabled("%s", label);
	ImGui::TableNextColumn();
	if (dim)
	{
		ImGui::TextDisabled("%s", value);
	}
	else
	{
		ImGui::TextUnformatted(value);
	}
}

static void OMFEditorWarningRow(const char* label, u32 value)
{
	string16 text = {};
	xr_sprintf(text, "%u", value);
	OMFEditorWarningRow(label, text);
}

static void OMFEditorWarningList(const char* label, const xr_vector<xr_string>& values, bool dim)
{
	for (u32 i = 0; i < values.size(); ++i)
	{
		OMFEditorWarningRow(i == 0 ? label : "", values[i].c_str(), dim);
	}
}

static void OMFEditorMotionWarnings(const char* model, const SOMFMotion* motion)
{
	if (motion == nullptr)
	{
		return;
	}

	ImGui::PushID(model);
	if (motion->FileCopies > 1 && OMFEditorWarningBegin("##copies", "Motion is repeated in one .omf"))
	{
		OMFEditorWarningRow("Model", model);
		OMFEditorWarningRow("Motion", motion->Name.c_str());
		OMFEditorWarningRow("Times", motion->FileCopies);
		OMFEditorWarningRow("File", motion->File.c_str(), true);
		OMFEditorWarningRow("Plays", "the first one");
		OMFEditorWarningEnd();
	}

	if (!motion->Duplicates.empty() && OMFEditorWarningBegin("##other", "Motion is in several .omf"))
	{
		OMFEditorWarningRow("Model", model);
		OMFEditorWarningRow("Motion", motion->Name.c_str());
		OMFEditorWarningRow("Plays", motion->File.c_str(), true);
		OMFEditorWarningList("Also in", motion->Duplicates, true);
		OMFEditorWarningEnd();
	}

	ImGui::PopID();
}

static xr_vector<xr_string> OMFEditorSharedAliases(attachable_hud_item* item, const player_hud_motion& anm)
{
	xr_vector<xr_string> aliases;
	for (const player_hud_motion& other : item->m_hand_motions.m_anims)
	{
		if (&other == &anm || other.m_alias_name == anm.m_alias_name)
		{
			continue;
		}

		const bool shared = std::any_of(other.m_animations.begin(), other.m_animations.end(), [&](const motion_descr& descr)
		{
			return std::any_of(anm.m_animations.begin(), anm.m_animations.end(), [&](const motion_descr& own) { return own.mid == descr.mid; });
		});

		if (shared)
		{
			aliases.push_back(other.m_alias_name.c_str());
		}
	}

	return aliases;
}

static void OMFEditorWarnings(attachable_hud_item* item, const player_hud_motion& anm, const SOMFMotion* hands, const SOMFMotion* weapon)
{
	const u32 copies = pSettings->DuplicateLineCount(item->m_sect_name.c_str(), anm.m_alias_name.c_str());
	const xr_vector<xr_string> aliases = OMFEditorSharedAliases(item, anm);
	const u32 warnings = u32(copies > 1) + u32(!aliases.empty()) + OMFEditorWarningCount(hands) + OMFEditorWarningCount(weapon);
	if (warnings == 0)
	{
		return;
	}

	string64 header = {};
	xr_sprintf(header, "Warnings (%u)###omf_warnings", warnings);
	ImGui::PushStyleColor(ImGuiCol_Text, OMFWarningColor);
	const bool open = ImGui::CollapsingHeader(header, ImGuiTreeNodeFlags_DefaultOpen);
	ImGui::PopStyleColor();
	if (!open)
	{
		return;
	}

	if (copies > 1 && OMFEditorWarningBegin("##config_copies", "Key is set several times in the config"))
	{
		OMFEditorWarningRow("Key", anm.m_alias_name.c_str());
		OMFEditorWarningRow("Times", copies);
		OMFEditorWarningRow("Section", item->m_sect_name.c_str());
		OMFEditorWarningRow("Used", "the last one");
		OMFEditorWarningEnd();
	}

	if (!aliases.empty() && OMFEditorWarningBegin("##config_shared", "Keys use the same motion"))
	{
		OMFEditorWarningRow("Key", anm.m_alias_name.c_str());
		OMFEditorWarningRow("Motion", anm.m_animations[0].name.c_str());
		OMFEditorWarningRow("Section", item->m_sect_name.c_str());
		OMFEditorWarningList("Same as", aliases, false);
		OMFEditorWarningEnd();
	}

	OMFEditorMotionWarnings(item->m_model_combined ? "Hands and weapon" : "Hands", hands);
	OMFEditorMotionWarnings("Weapon", weapon);
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
		if (!ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId) && !ImGui::GetIO().WantTextInput)
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

	const player_hud_motion* anm = HudMotionPlayerSelectedAnim(item);
	const bool combined = item->m_model_combined;
	SOMFMotion* hands = nullptr;
	SOMFMotion* weapon = nullptr;
	if (anm != nullptr)
	{
		IKinematicsAnimated* item_model = item->m_model != nullptr ? item->m_model->dcast_PKinematicsAnimated() : nullptr;
		hands = OMFEditorFindMotion(combined ? item_model : g_player_hud->GetModel(), anm->m_animations[0].mid);
		weapon = combined ? nullptr : OMFEditorFindMotion(item_model, OMFEditorWeaponMotion(item, *anm));

		ImGui::Separator();
		OMFEditorMotionInfo(combined ? "Motion" : "Hand motion", hands, anm->m_animations[0].name.c_str());
		if (!combined)
		{
			OMFEditorMotionInfo("Weapon motion", weapon, OMFEditorWeaponMotionName(*anm).c_str());

			ImGui::Separator();
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
	}

	SOMFMotion* selected = state.Weapon && !combined ? weapon : hands;
	if (selected != nullptr)
	{
		OMFEditorMotion(*selected);
		OMFEditorFileList(*selected, item);
	}

	OMFEditorBoneParts();

	if (anm != nullptr)
	{
		OMFEditorWarnings(item, *anm, hands, weapon);
	}

	ImGui::End();
}
