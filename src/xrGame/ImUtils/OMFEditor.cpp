#include "StdAfx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"

#include "ai_space.h"

#include "ImUtils.h"
#include <fstream>
#include <memory_resource>

#if IXRAY_OMF_EDITOR_TAB_GAME == 1
#include "../Inventory.h"
#include "../Weapon.h"
#include "../player_hud.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#endif

constexpr unsigned int _kMaxStringFieldNameLength = 32;
constexpr const char* _kOMFEditorModalWindow_RenameAnimationParam = "Rename##ToolsInGameImGui_OMGEditor_AnimationParam";
constexpr const char* _kOMFEditorModalWindow_WarningRenameHasCollision = "Warning##ToolsInGameImGui_OMFEditor_AnimationParamFailedRenaming";
constexpr const char* _kOMFEditorModalWindow_BonePartsWasCopiedToClipboardSuccessful = "Successful!##ToolsInGameImGui_OMFEditor_BonePartsToClipboard";
constexpr const char* _kOMFEditorModalWindow_BonePartsWasCopiedToClipboardFailed = "Failed!##ToolsInGameImGui_OMFEditor_BonePartsToClipboard";
constexpr const char* _kOMFEditorModalWindow_BoneRenameHasCollion = "Warning!##ToolsInGameImGui_OMFEditor_BoneRenameHasCollision";
constexpr const char* _kOMFEditorModalWindow_AnimationParamMotionMarksCleared = "Warning!##ToolsOMFEditor_MotionMarksCleared";
constexpr const char* _kOMFEditorModalWindow_AddMotionMark = "Add##ToolsOMFEditor_MotionMarkAdd";
constexpr const char* _kOMFEditorModalWindow_DuplicateFoundMotionMark = "Warning!##ToolsOMFEditor_DuplicateFoundMotionMark";
constexpr const char* _kOMFEditorModalWindow_TryRepairApplied = "Info##ToolsOMFEditor_TryRepairModal";
constexpr const char* _kOMFEditorModalWindow_AddAnimsFrom = "Add anims from##ToolsOMFEditor_AddAnimsFrom";


struct OMFData
{
	using omf_name_t = xr_stack_string<_kMaxStringFieldNameLength>;

	struct AnimVector
	{
		int32_t section_id;
		uint32_t section_size;
		// dynamically allocated
		char* data;
		omf_name_t name;
	};

	struct BoneParts
	{
		struct Bone
		{
			uint32_t id;
			omf_name_t name;
		};

		int16_t count;
		omf_name_t name;

		xr_vector<Bone> bones;
	};

	struct BoneData
	{
		int16_t ogf_version;
		int16_t count;
		int32_t section_id;
		uint32_t section_size;
		xr_vector<BoneParts> parts;
	};

	struct AnimData
	{
		int32_t section_id;
		uint32_t section_size;

		int32_t section_id2;
		uint32_t section_size2;

		int32_t animations_count;
		short animations_params_count;

		xr_vector<AnimVector> anims;
	};

	struct AnimParamsData
	{
		struct AnimParams
		{
			struct MotionMark
			{
				struct Params
				{
					float t0;
					float t1;
				};

				int32_t count;
				omf_name_t name;
				xr_vector<Params> params;
			};

			int16_t bone_or_part;
			int16_t motion_id;
			int32_t flags;
			int32_t marks_count;
			float speed;
			float power;
			float accrue;
			float falloff;
			omf_name_t name;
			xr_vector<MotionMark> marks;
		};

		int16_t count;
		xr_vector<AnimParams> params;
	};

	AnimData data_anim;
	BoneData data_bone;
	AnimParamsData data_animparams;

	void destroy()
	{
		for (auto& anim : data_anim.anims)
		{
			if (anim.data)
			{
				delete[] anim.data;
				anim.data = nullptr;
			}
		}
	}
};

// ==============================================================
// Shared UI state.
// Everything the OMF editor UI needs that does NOT depend on where
// the edited data comes from (a loaded .omf file or live game data).
// CImGuiOMFEditor (editor tab) and CImGuiOMFGameState (game tab)
// both carry this state, so the same UI works on both data sources.
// ==============================================================
struct SOMFEditorUIState
{
	bool is_show_popup_marks_cleared{};
	bool is_show_popup_rename_animation_param{};
	bool is_show_popup_renamehascollision{};
	bool is_show_popup_boneparts_was_copied_to_clipboard_suc{};
	bool is_show_popup_boneparts_was_copied_to_clipboard_fail{};

	bool is_show_popup_add_motion_mark{};
	bool is_show_popup_duplicate_found_motion_mark{};

	bool is_motion_time_format_seconds_selected{};
	bool is_motion_time_format_keys_selected{};
	bool is_motion_time_format_radiobutton_changed{};
	bool is_motion_marks_enabled{};

	int current_selected_animation_param{};
	int current_selected_bone_rename{};
	int current_selected_mark{};
	int current_selected_mark_param{};

	OMFData::omf_name_t rename_temp;
	OMFData::omf_name_t rename_temp_bone;
	OMFData::omf_name_t temp_motion_mark_name;

	xr_vector<const char*> combo_animation_params_data;
	xr_vector<const char*> list_box_motion_marks_names;
	xr_vector<xr_stack_string16> list_box_motion_marks_params_names;

	void Reset()
	{
		current_selected_animation_param = 0;
		current_selected_mark = -1;
		current_selected_mark_param = -1;

	#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 0
		current_selected_bone_rename = 0;
	#else
		current_selected_bone_rename = -1;
	#endif

		is_motion_time_format_seconds_selected = true;
		is_motion_time_format_keys_selected = false;
		is_motion_time_format_radiobutton_changed = true;
		is_motion_marks_enabled = false;

		is_show_popup_marks_cleared = false;
		is_show_popup_rename_animation_param = false;
		is_show_popup_renamehascollision = false;
		is_show_popup_boneparts_was_copied_to_clipboard_suc = false;
		is_show_popup_boneparts_was_copied_to_clipboard_fail = false;
		is_show_popup_add_motion_mark = false;
		is_show_popup_duplicate_found_motion_mark = false;

		combo_animation_params_data.clear();
		list_box_motion_marks_names.clear();
		list_box_motion_marks_params_names.clear();
	}
};

// ==============================================================
// Data provider abstraction.
// The whole OMF editor UI is written once against this interface.
// COMFFileAnimProvider feeds it from a .omf file loaded on disk
// (editor tab), CGameAnimProvider feeds it from the live engine
// motions of the current hud item (game tab).
// ==============================================================
struct IOMFAnimDataProvider
{
	virtual ~IOMFAnimDataProvider() = default;

	// capabilities (UI hides/gates what a source can't do)
	virtual bool CanRenameAnimations() const = 0;
	virtual bool CanRenameBones() const = 0;
	virtual bool SupportsMotionMarks() const = 0;

	// animation params
	virtual int GetAnimParamsCount() const = 0;
	virtual const char* GetAnimParamName(int index) const = 0;
	virtual bool AnimParamNameExists(const char* name) const = 0;
	virtual void RenameAnimParam(int index, const char* new_name) = 0;

	virtual float GetSpeed(int index) const = 0;
	virtual float GetPower(int index) const = 0;
	virtual float GetAccrue(int index) const = 0;
	virtual float GetFalloff(int index) const = 0;
	virtual void SetSpeed(int index, float value) = 0;
	virtual void SetPower(int index, float value) = 0;
	virtual void SetAccrue(int index, float value) = 0;
	virtual void SetFalloff(int index, float value) = 0;

	virtual int GetFlags(int index) const = 0;
	virtual void SetFlags(int index, int flags) = 0;

	// raw motion length in seconds (speed = 1.0, i.e. keys_count / 30)
	virtual float GetAnimLengthSeconds(int index) const = 0;

	// motion marks
	virtual int GetMarksCount(int anim_index) const = 0;
	virtual const char* GetMarkName(int anim_index, int mark_index) const = 0;
	virtual void AddMark(int anim_index, const char* name) = 0;
	virtual void DeleteMark(int anim_index, int mark_index) = 0;
	virtual void ClearMarks(int anim_index) = 0;
	virtual int GetMarkParamsCount(int anim_index, int mark_index) const = 0;
	virtual void GetMarkParam(int anim_index, int mark_index, int param_index, float& t0, float& t1) const = 0;
	virtual void SetMarkParam(int anim_index, int mark_index, int param_index, float t0, float t1) = 0;
	virtual void AddMarkParam(int anim_index, int mark_index) = 0;
	virtual void DeleteMarkParam(int anim_index, int mark_index, int param_index) = 0;

	// bone parts
	virtual int GetBonePartsCount() const = 0;
	virtual const char* GetBonePartName(int part_index) const = 0;
	virtual int GetBonesCount(int part_index) const = 0;
	virtual const char* GetBoneName(int part_index, int bone_index) const = 0;
	virtual void RenameBone(int part_index, int bone_index, const char* new_name) = 0;
};

// editor tab: wraps the parsed .omf file data
struct COMFFileAnimProvider : IOMFAnimDataProvider
{
	void Bind(OMFData* pData) { m_data = pData; }
	bool IsValid() const { return m_data != nullptr; }

	bool CanRenameAnimations() const override { return true; }
	bool CanRenameBones() const override { return true; }
	bool SupportsMotionMarks() const override { return m_data && m_data->data_bone.ogf_version == 4; }

	int GetAnimParamsCount() const override { return m_data ? static_cast<int>(m_data->data_animparams.params.size()) : 0; }
	const char* GetAnimParamName(int index) const override { return Params(index).name.c_str(); }

	bool AnimParamNameExists(const char* name) const override
	{
		if (m_data == nullptr || name == nullptr)
		{
			return false;
		}

		for (const auto& param : m_data->data_animparams.params)
		{
			if (param.name == name)
			{
				return true;
			}
		}

		return false;
	}

	void RenameAnimParam(int index, const char* new_name) override { Params(index).name = new_name; }

	float GetSpeed(int index) const override { return Params(index).speed; }
	float GetPower(int index) const override { return Params(index).power; }
	float GetAccrue(int index) const override { return Params(index).accrue; }
	float GetFalloff(int index) const override { return Params(index).falloff; }
	void SetSpeed(int index, float value) override { Params(index).speed = value; }
	void SetPower(int index, float value) override { Params(index).power = value; }
	void SetAccrue(int index, float value) override { Params(index).accrue = value; }
	void SetFalloff(int index, float value) override { Params(index).falloff = value; }

	int GetFlags(int index) const override { return Params(index).flags; }
	void SetFlags(int index, int flags) override { Params(index).flags = flags; }

	float GetAnimLengthSeconds(int index) const override
	{
		if (m_data == nullptr || index < 0 || index >= static_cast<int>(m_data->data_anim.anims.size()))
		{
			return 0.0f;
		}

		const OMFData::AnimVector& anim = m_data->data_anim.anims[index];

		if (anim.data == nullptr)
		{
			return 0.0f;
		}

		// key count is stored at the beginning of the anim section
		int num_keys = 0;
		std::memcpy(&num_keys, anim.data, sizeof(num_keys));
		return static_cast<float>(num_keys) / 30.0f;
	}

	int GetMarksCount(int anim_index) const override { return Params(anim_index).marks_count; }
	const char* GetMarkName(int anim_index, int mark_index) const override { return Params(anim_index).marks[mark_index].name.c_str(); }

	void AddMark(int anim_index, const char* name) override
	{
		AnimParams& param = Params(anim_index);
		param.marks.push_back({});
		param.marks.back().name = name;
		param.marks_count = static_cast<int32_t>(param.marks.size());
	}

	void DeleteMark(int anim_index, int mark_index) override
	{
		AnimParams& param = Params(anim_index);
		param.marks.erase(param.marks.cbegin() + mark_index);
		param.marks_count = static_cast<int32_t>(param.marks.size());
	}

	void ClearMarks(int anim_index) override
	{
		AnimParams& param = Params(anim_index);
		param.marks.clear();
		param.marks_count = 0;
	}

	int GetMarkParamsCount(int anim_index, int mark_index) const override { return Params(anim_index).marks[mark_index].count; }

	void GetMarkParam(int anim_index, int mark_index, int param_index, float& t0, float& t1) const override
	{
		const auto& mark_param = Params(anim_index).marks[mark_index].params[param_index];
		t0 = mark_param.t0;
		t1 = mark_param.t1;
	}

	void SetMarkParam(int anim_index, int mark_index, int param_index, float t0, float t1) override
	{
		auto& mark_param = Params(anim_index).marks[mark_index].params[param_index];
		mark_param.t0 = t0;
		mark_param.t1 = t1;
	}

	void AddMarkParam(int anim_index, int mark_index) override
	{
		auto& mark = Params(anim_index).marks[mark_index];
		mark.params.push_back({});
		mark.count = static_cast<int32_t>(mark.params.size());
	}

	void DeleteMarkParam(int anim_index, int mark_index, int param_index) override
	{
		auto& mark = Params(anim_index).marks[mark_index];
		mark.params.erase(mark.params.cbegin() + param_index);
		mark.count = static_cast<int32_t>(mark.params.size());
	}

	int GetBonePartsCount() const override { return m_data ? static_cast<int>(m_data->data_bone.parts.size()) : 0; }
	const char* GetBonePartName(int part_index) const override { return m_data->data_bone.parts[part_index].name.c_str(); }
	int GetBonesCount(int part_index) const override { return static_cast<int>(m_data->data_bone.parts[part_index].bones.size()); }
	const char* GetBoneName(int part_index, int bone_index) const override { return m_data->data_bone.parts[part_index].bones[bone_index].name.c_str(); }
	void RenameBone(int part_index, int bone_index, const char* new_name) override { m_data->data_bone.parts[part_index].bones[bone_index].name = new_name; }

private:
	using AnimParams = OMFData::AnimParamsData::AnimParams;

	AnimParams& Params(int index)
	{
		R_ASSERT(m_data);
		R_ASSERT(index >= 0 && index < static_cast<int>(m_data->data_animparams.params.size()));
		return m_data->data_animparams.params[index];
	}

	const AnimParams& Params(int index) const
	{
		R_ASSERT(m_data);
		R_ASSERT(index >= 0 && index < static_cast<int>(m_data->data_animparams.params.size()));
		return m_data->data_animparams.params[index];
	}

	OMFData* m_data = nullptr;
};

#if IXRAY_OMF_EDITOR_TAB_GAME == 1
// game tab: wraps the live engine motions (CMotionDef) of a hud model.
// Editing writes directly to the shared motions_value, so changes apply
// to every model using the same OMF and take effect on the next played
// animation (marks are read per-frame and apply immediately).
struct CGameAnimProvider : IOMFAnimDataProvider
{
	struct SMotionEntry
	{
		IKinematicsAnimated* model; // model the MotionID resolves against
		CMotionDef* def;
		MotionID mid;
		xr_string display_name; // motion name (+ [bp] suffix for item bone-part motions)
	};

	// lists only the motions the given hud item actually uses:
	// hand motions (anm_*) resolve on the hands model (or on the item
	// model itself when it is a combined hands+item model), bone-part
	// motions (anm_bp_*) always resolve on the item model
	void Bind(attachable_hud_item* pItem, IKinematicsAnimated* pHandsModel)
	{
		m_entries.clear();
		m_model = nullptr;
		m_partition = nullptr;

		if (pItem == nullptr)
		{
			return;
		}

		IKinematicsAnimated* pItemModel = pItem->m_model ? pItem->m_model->dcast_PKinematicsAnimated() : nullptr;
		IKinematicsAnimated* pHands = pItem->m_model_combined ? pItemModel : pHandsModel;

		for (const player_hud_motion& motion : pItem->m_hand_motions.m_anims)
		{
			for (const motion_descr& descr : motion.m_animations)
			{
				AddEntry(pHands, descr, false);
			}
		}

		for (const attachable_hud_item_motion& motion : pItem->m_hand_motions.m_item_anims)
		{
			for (const motion_descr& descr : motion.m_animations)
			{
				AddEntry(pItemModel, descr, true);
			}
		}

		// bone parts are shown for the model that plays the hand motions
		m_model = pHands ? pHands : pItemModel;

		if (m_model)
		{
			const u16 slot_count = m_model->LL_MotionsSlotCount();

			for (u16 slot = 0; slot < slot_count && m_partition == nullptr; ++slot)
			{
				shared_motions motions = m_model->LL_MotionsSlot(slot);

				if (motions.partition() && motions.partition()->count() > 0)
				{
					m_partition = motions.partition();
				}
			}
		}
	}

	bool CanRenameAnimations() const override { return false; } // names are accel_map keys shared by all models using this OMF
	bool CanRenameBones() const override { return false; }      // bones belong to the skeleton, not to the motion data
	bool SupportsMotionMarks() const override { return true; }

	int GetAnimParamsCount() const override { return static_cast<int>(m_entries.size()); }
	const char* GetAnimParamName(int index) const override { return Entry(index).display_name.c_str(); }
	bool AnimParamNameExists(const char* name) const override { return false; }
	void RenameAnimParam(int index, const char* new_name) override { R_ASSERT(!"renaming animations is not supported for live game data"); }

	float GetSpeed(int index) const override { return Entry(index).def->Speed(); }
	float GetPower(int index) const override { return Entry(index).def->Power(); }
	float GetAccrue(int index) const override { return Entry(index).def->Accrue(); }
	float GetFalloff(int index) const override { return Entry(index).def->Falloff(); }

	void SetSpeed(int index, float value) override
	{
		CMotionDef* p_def = Entry(index).def;
		p_def->speed = p_def->Quantize(value);
	}

	void SetPower(int index, float value) override
	{
		CMotionDef* p_def = Entry(index).def;
		p_def->power = p_def->Quantize(value);
	}

	void SetAccrue(int index, float value) override
	{
		CMotionDef* p_def = Entry(index).def;
		p_def->accrue = p_def->Quantize(value / fQuantizerRangeExt);
	}

	void SetFalloff(int index, float value) override
	{
		CMotionDef* p_def = Entry(index).def;
		p_def->falloff = p_def->Quantize(value / fQuantizerRangeExt);
	}

	int GetFlags(int index) const override { return Entry(index).def->flags; }
	void SetFlags(int index, int flags) override { Entry(index).def->flags = static_cast<u16>(flags); }

	float GetAnimLengthSeconds(int index) const override
	{
		const SMotionEntry& entry = Entry(index);

		if (entry.model == nullptr)
		{
			return 0.0f;
		}

		// raw length (no speed division) to match the editor's keys/30 math
		CMotion* p_motion = entry.model->LL_GetRootMotion(entry.mid);
		return p_motion ? p_motion->GetLength() : 0.0f;
	}

	int GetMarksCount(int anim_index) const override { return static_cast<int>(Entry(anim_index).def->marks.size()); }
	const char* GetMarkName(int anim_index, int mark_index) const override { return Entry(anim_index).def->marks[mark_index].name.c_str(); }

	void AddMark(int anim_index, const char* name) override
	{
		CMotionDef* p_def = Entry(anim_index).def;
		motion_marks& mark = p_def->marks.emplace_back();
		mark.name = name;
	}

	void DeleteMark(int anim_index, int mark_index) override
	{
		CMotionDef* p_def = Entry(anim_index).def;
		p_def->marks.erase(p_def->marks.begin() + mark_index);
	}

	void ClearMarks(int anim_index) override
	{
		Entry(anim_index).def->marks.clear();
	}

	int GetMarkParamsCount(int anim_index, int mark_index) const override
	{
		return static_cast<int>(Entry(anim_index).def->marks[mark_index].intervals.size());
	}

	void GetMarkParam(int anim_index, int mark_index, int param_index, float& t0, float& t1) const override
	{
		const motion_marks::interval& mark_param = Entry(anim_index).def->marks[mark_index].intervals[param_index];
		t0 = mark_param.first;
		t1 = mark_param.second;
	}

	void SetMarkParam(int anim_index, int mark_index, int param_index, float t0, float t1) override
	{
		motion_marks::interval& mark_param = Entry(anim_index).def->marks[mark_index].intervals[param_index];
		mark_param.first = t0;
		mark_param.second = t1;
	}

	void AddMarkParam(int anim_index, int mark_index) override
	{
		Entry(anim_index).def->marks[mark_index].intervals.emplace_back(0.0f, 0.0f);
	}

	void DeleteMarkParam(int anim_index, int mark_index, int param_index) override
	{
		motion_marks& mark = Entry(anim_index).def->marks[mark_index];
		mark.intervals.erase(mark.intervals.begin() + param_index);
	}

	int GetBonePartsCount() const override { return m_partition ? m_partition->count() : 0; }
	const char* GetBonePartName(int part_index) const override { return m_partition->part(static_cast<u16>(part_index)).Name.c_str(); }
	int GetBonesCount(int part_index) const override { return static_cast<int>(m_partition->part(static_cast<u16>(part_index)).bones.size()); }

	const char* GetBoneName(int part_index, int bone_index) const override
	{
		// LL_BoneName_dbg lives on IKinematics, not on IKinematicsAnimated
		IKinematics* p_kinematics = m_model ? m_model->dcast_PKinematics() : nullptr;

		if (p_kinematics == nullptr)
		{
			return "";
		}

		u32 bone_id = m_partition->part(static_cast<u16>(part_index)).bones[bone_index];
		return p_kinematics->LL_BoneName_dbg(static_cast<u16>(bone_id));
	}

	void RenameBone(int part_index, int bone_index, const char* new_name) override { R_ASSERT(!"renaming bones is not supported for live game data"); }

private:
	void AddEntry(IKinematicsAnimated* pModel, const motion_descr& descr, bool is_bonepart)
	{
		if (pModel == nullptr || descr.mid.valid() == false)
		{
			return;
		}

		for (const SMotionEntry& entry : m_entries)
		{
			if (entry.model == pModel && entry.mid == descr.mid)
			{
				return; // already listed (several aliases can share one motion)
			}
		}

		CMotionDef* p_def = pModel->LL_GetMotionDef(descr.mid);

		if (p_def == nullptr)
		{
			return;
		}

		SMotionEntry& entry = m_entries.emplace_back();
		entry.model = pModel;
		entry.def = p_def;
		entry.mid = descr.mid;
		entry.display_name = descr.name.c_str();

		if (is_bonepart)
		{
			entry.display_name += " [bp]";
		}
	}

	const SMotionEntry& Entry(int index) const
	{
		R_ASSERT(index >= 0 && index < static_cast<int>(m_entries.size()));
		return m_entries[index];
	}

	xr_vector<SMotionEntry> m_entries;
	IKinematicsAnimated* m_model = nullptr;
	const CPartition* m_partition = nullptr;
};
#endif

// editor tab state = shared UI state + file-specific data
struct CImGuiOMFEditor : SOMFEditorUIState
{
	~CImGuiOMFEditor()
	{
		if (omf)
		{
			xr_delete(omf);
		}
	}

	bool is_show_popup_boneparts_rename_has_collision{};
	bool is_show_popup_try_repair_applied{};
	bool is_show_popup_add_anims_from{};

	bool is_file_loaded{};
	bool is_input_text_addanimsfrom_updated_preview{};
	bool is_input_text_addanimsfrom_was_edited{};

	OMFData* omf{};
	OMFData* temp_omf{};
	COMFFileAnimProvider provider{};

	xr_vector<const char*> combo_bones_data;
	xr_set<size_t> combo_bones_name_hashes;

	xr_vector<OMFData::omf_name_t> addanimsfrom_animation_list;

	xr_stack_string<512> input_text_add_anims_from_buffer;
	xr_stack_string<sizeof(string_path) * 2> path;
};

CImGuiOMFEditor* g_pOMFEditor = nullptr;

#if IXRAY_OMF_EDITOR_TAB_GAME == 1
// game tab state = shared UI state + live data binding.
// Exists alongside g_pOMFEditor, so a file can stay open in the
// editor tab while the game tab edits the current weapon's data.
struct CImGuiOMFGameState : SOMFEditorUIState
{
	CGameAnimProvider provider{};
	attachable_hud_item* bound_item = nullptr;
	IKinematicsAnimated* bound_model = nullptr;
};

CImGuiOMFGameState* g_pOMFGame = nullptr;
#endif


void OMFEditor_OnPressed(int key)
{
	switch (key)
	{
		case SDL_Scancode::SDL_SCANCODE_ESCAPE:
		{
			if (Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)])
			{
				if (g_pOMFEditor)
				{
					SRequestData req;
					req.editor_type = (u32)eImGuiEditorType::kOMFEditor;
					req.request_type = (u32)eRequestType_OMFEditor::kDeselectCurrentSelectedOrHideWindow;

					AllEditors_SendRequest(req);
				}
			}
			break;
		}
	}
}

void OMFEditor_OnReleased(int key)
{
}

void OMFEditor_ReadString(OMFData::omf_name_t& str, std::ifstream& file)
{
	char symbol = -1;
	uint32_t str_length = 0;
	do
	{
		R_ASSERT2(str_length < str.max_size(), "report to developers you have too long serialized string");

		file.read(&symbol, 1);
		str += symbol;
		++str_length;
	} while (symbol != '\0');
}

void OMFEditor_ReadStringMotionMark(OMFData::omf_name_t& str, std::ifstream& file)
{
	char symbol = -1;
	uint32_t str_length = 0;
	do
	{
		R_ASSERT2(str_length < str.max_size(), "report to developers you have too long serialized string");

		file.read(&symbol, 1);
		str += symbol;
		++str_length;
	} while (symbol != 0xA);
}

void OMFEditor_WriteMotionMarkName(
	const char* pStr,
	unsigned int Size,
	std::ofstream& file
)
{
	if (
		pStr &&
		Size > 0 &&
		file.is_open() &&
		file.good()
	)
	{
		for (unsigned int i = 0; i < Size; ++i)
		{
			char symbol = pStr[i];

			if (symbol == '\0')
			{
				char val = 0xA;
				file.write(&val, sizeof(char));
				return;
			}
			else
			{
				file.write(&symbol, sizeof(char));
			}
		}
	}
}


bool OMFEditor_LoadOMF_AnimData(OMFData::AnimData& data, std::ifstream& file)
{
	file.read(reinterpret_cast<char*>(&data.section_id), sizeof(data.section_id));
	file.read(reinterpret_cast<char*>(&data.section_size), sizeof(data.section_size));
	file.read(reinterpret_cast<char*>(&data.section_id2), sizeof(data.section_id2));
	file.read(reinterpret_cast<char*>(&data.section_size2), sizeof(data.section_size2));
	file.read(reinterpret_cast<char*>(&data.animations_count), sizeof(data.animations_count));

	for (int i = 0; i < data.animations_count; ++i)
	{
		data.anims.push_back({});
		OMFData::AnimVector& av = data.anims.back();

		file.read(reinterpret_cast<char*>(&av.section_id), sizeof(av.section_id));
		file.read(reinterpret_cast<char*>(&av.section_size), sizeof(av.section_size));

		// GSC style of reading data refering to r_stringZ implementation
		OMFEditor_ReadString(av.name, file);

		uint32_t data_size = av.section_size - (av.name.size() + 1);
		av.data = new char[data_size];

		file.read(&av.data[0], data_size);
	}

	return true;
}

bool OMFEditor_SaveOMF_AnimData(
	const OMFData::AnimData& data,
	std::ofstream& file
)
{
	R_ASSERT(file.good());

	if (file.good() == false)
	{
		return false;
	}

	uint32_t section_size = 0;

	for (const auto& anim : data.anims)
	{
		section_size += anim.name.size() + 1 + (anim.section_size - (anim.name.size() + 1));
		section_size += sizeof(anim.section_id);
		section_size += sizeof(anim.section_size);
	}

	section_size += 12;

	file.write(reinterpret_cast<const char*>(&data.section_id), sizeof(data.section_id));
	file.write(reinterpret_cast<char*>(&section_size), sizeof(section_size));

	file.write(reinterpret_cast<const char*>(&data.section_id2), sizeof(data.section_id2));
	file.write(reinterpret_cast<const char*>(&data.section_size2), sizeof(data.section_size2));

	file.write(reinterpret_cast<const char*>(&data.animations_count), sizeof(data.animations_count));

	for (const auto& anim : data.anims)
	{
		file.write(reinterpret_cast<const char*>(&anim.section_id), sizeof(anim.section_id));
		file.write(reinterpret_cast<const char*>(&anim.section_size), sizeof(anim.section_size));
		file.write(anim.name.c_str(), anim.name.size() + 1);
		file.write(anim.data, (anim.section_size - (anim.name.size() + 1)));
	}

	return file.good();
}

bool OMFEditor_LoadOMF_BoneData(OMFData::BoneData& data, std::ifstream& file)
{
	file.read(reinterpret_cast<char*>(&data.section_id), sizeof(data.section_id));
	file.read(reinterpret_cast<char*>(&data.section_size), sizeof(data.section_size));
	file.read(reinterpret_cast<char*>(&data.ogf_version), sizeof(data.ogf_version));
	file.read(reinterpret_cast<char*>(&data.count), sizeof(data.count));

	R_ASSERT2(data.count <= data.parts.max_size(), "report to developers!");

	for (int16_t i = 0; i < data.count; ++i)
	{
		data.parts.push_back({});
		OMFData::BoneParts& bp = data.parts.back();

		OMFEditor_ReadString(bp.name, file);
		file.read(reinterpret_cast<char*>(&bp.count), sizeof(bp.count));

		R_ASSERT2(bp.count <= bp.bones.max_size(), "report to developers!");

		for (int j = 0; j < bp.count; ++j)
		{
			bp.bones.push_back({});
			OMFData::BoneParts::Bone& bone = bp.bones.back();

			OMFEditor_ReadString(bone.name, file);
			file.read(reinterpret_cast<char*>(&bone.id), sizeof(bone.id));
		}
	}

	return true;
}

bool OMFEditor_SaveOMF_BoneData(
	const OMFData::BoneData& data,
	const OMFData::AnimParamsData& data_ap,
	std::ofstream& file
)
{
	R_ASSERT(file.good());

	if (file.good() == false)
	{
		return false;
	}

	file.write(reinterpret_cast<const char*>(&data.section_id), sizeof(data.section_id));

	unsigned int section_size = sizeof(data.section_id);

	for (const auto& part : data.parts)
	{
		section_size += part.name.size() + 1;
		section_size += sizeof(part.count);

		for (const auto& bone : part.bones)
		{
			section_size += bone.name.size() + 1;
			section_size += sizeof(bone.id);
		}
	}

	section_size += sizeof(data.ogf_version);

	for (const auto& param : data_ap.params)
	{
		section_size += param.name.size() + 1;

		section_size += sizeof(param.flags);
		section_size += sizeof(param.bone_or_part);
		section_size += sizeof(param.motion_id);
		section_size += sizeof(param.speed);
		section_size += sizeof(param.power);
		section_size += sizeof(param.accrue);
		section_size += sizeof(param.falloff);

		if (data.ogf_version >= 4)
		{
			section_size += 4;

			for (const auto& mark : param.marks)
			{
				section_size += mark.name.size() + 1;
				section_size += sizeof(mark.count);

				for (const auto& mark_param : mark.params)
				{
					section_size += sizeof(mark_param.t0);
					section_size += sizeof(mark_param.t1);
				}
			}
		}
	}

	file.write(reinterpret_cast<char*>(&section_size), sizeof(section_size));
	file.write(reinterpret_cast<const char*>(&data.ogf_version), sizeof(data.ogf_version));
	file.write(reinterpret_cast<const char*>(&data.count), sizeof(data.count));

	for (const auto& part : data.parts)
	{
		file.write(part.name.c_str(), part.name.size() + 1);
		file.write(reinterpret_cast<const char*>(&part.count), sizeof(part.count));

		for (const auto& bone : part.bones)
		{
			file.write(bone.name.c_str(), bone.name.size() + 1);
			file.write(reinterpret_cast<const char*>(&bone.id), sizeof(bone.id));
		}
	}

	return file.good();
}

bool OMFEditor_LoadOMF_AnimParamsData_MotionMark(
	OMFData::AnimParamsData::AnimParams::MotionMark& mark,
	std::ifstream& file
)
{
	bool status = true;

	OMFEditor_ReadStringMotionMark(mark.name, file);
	file.read(reinterpret_cast<char*>(&mark.count), sizeof(mark.count));

	for (int32_t i = 0; i < mark.count; ++i)
	{
		mark.params.push_back({});
		OMFData::AnimParamsData::AnimParams::MotionMark::Params& mark_param = mark.params.back();

		file.read(reinterpret_cast<char*>(&mark_param.t0), sizeof(mark_param.t0));
		file.read(reinterpret_cast<char*>(&mark_param.t1), sizeof(mark_param.t1));
	}

	return status;
}

bool OMFEditor_LoadOMF_AnimParamsData(int16_t ogf_version, int32_t animation_count, OMFData::AnimParamsData& data, std::ifstream& file)
{
	file.read(reinterpret_cast<char*>(&data.count), sizeof(data.count));

	if (animation_count != data.count)
	{
		ShowMessageBox(_eMessageBoxStatus::kWarning, "Invalid OMF", "Animation count IS NOT equal to anim params count!");
		return false;
	}

	for (int16_t i = 0; i < data.count; ++i)
	{
		data.params.push_back({});
		OMFData::AnimParamsData::AnimParams& param = data.params.back();
		OMFEditor_ReadString(param.name, file);

		file.read(reinterpret_cast<char*>(&param.flags), sizeof(param.flags));
		file.read(reinterpret_cast<char*>(&param.bone_or_part), sizeof(param.bone_or_part));
		file.read(reinterpret_cast<char*>(&param.motion_id), sizeof(param.motion_id));
		file.read(reinterpret_cast<char*>(&param.speed), sizeof(param.speed));
		file.read(reinterpret_cast<char*>(&param.power), sizeof(param.power));
		file.read(reinterpret_cast<char*>(&param.accrue), sizeof(param.accrue));
		file.read(reinterpret_cast<char*>(&param.falloff), sizeof(param.falloff));

		if (ogf_version == 4)
		{
			file.read(reinterpret_cast<char*>(&param.marks_count), sizeof(param.marks_count));

			if (param.marks_count > 0)
			{
				for (int16_t mark_id = 0; mark_id < param.marks_count; ++mark_id)
				{
					param.marks.push_back({});
					OMFData::AnimParamsData::AnimParams::MotionMark& mark = param.marks.back();

					bool status_mark = OMFEditor_LoadOMF_AnimParamsData_MotionMark(mark, file);

					if (!status_mark)
					{
						char msg[64]{};
						std::sprintf(msg, "failed to load motion mark: %d", mark_id);
						ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", msg);
					}
				}
			}
		}
	}

	return true;
}

bool OMFEditor_SaveOMF_AnimParamsData(
	int16_t ogf_version,
	const OMFData::AnimParamsData& data,
	std::ofstream& file
)
{
	R_ASSERT(file.good());

	if (file.good() == false)
	{
		return false;
	}

	file.write(reinterpret_cast<const char*>(&data.count), sizeof(data.count));

	for (int i = 0; i < data.count; ++i)
	{
		auto& param = data.params[i];

		file.write(param.name.c_str(), param.name.size() + 1);
		file.write(reinterpret_cast<const char*>(&param.flags), sizeof(param.flags));
		file.write(reinterpret_cast<const char*>(&param.bone_or_part), sizeof(param.bone_or_part));
		file.write(reinterpret_cast<const char*>(&param.motion_id), sizeof(param.motion_id));
		file.write(reinterpret_cast<const char*>(&param.speed), sizeof(param.speed));
		file.write(reinterpret_cast<const char*>(&param.power), sizeof(param.power));
		file.write(reinterpret_cast<const char*>(&param.accrue), sizeof(param.accrue));
		file.write(reinterpret_cast<const char*>(&param.falloff), sizeof(param.falloff));

		if (ogf_version != 4)
		{
			continue;
		}

		file.write(reinterpret_cast<const char*>(&param.marks_count), sizeof(param.marks_count));

		for (int j = 0; j < param.marks_count; ++j)
		{
			auto& mark = param.marks[j];

			R_ASSERT(mark.name[mark.name.size()] == '\0' && "invalid string you got!!!!");
			OMFEditor_WriteMotionMarkName(
				mark.name.c_str(),
				mark.name.size() + 1,
				file
			);

			file.write(reinterpret_cast<const char*>(&mark.count), sizeof(mark.count));

			for (int y = 0; y < mark.count; ++y)
			{
				auto& param_mark = mark.params[y];
				file.write(reinterpret_cast<const char*>(&param_mark.t0), sizeof(param_mark.t0));
				file.write(reinterpret_cast<const char*>(&param_mark.t1), sizeof(param_mark.t1));
			}
		}
	}

	return file.good();
}

// ==============================================================
// Shared UI logic. All functions work against SOMFEditorUIState +
// IOMFAnimDataProvider, so editor (file) and game (live) tabs share
// the exact same behavior.
// ==============================================================

void OMFEditorUI_RebuildComboCache(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	state.combo_animation_params_data.clear();

	const int count = data.GetAnimParamsCount();
	state.combo_animation_params_data.reserve(count);

	for (int i = 0; i < count; ++i)
	{
		state.combo_animation_params_data.push_back(data.GetAnimParamName(i));
	}
}

void OMFEditorUI_RebuildMotionMarkParamLabels(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	state.list_box_motion_marks_params_names.clear();

	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount())
	{
		return;
	}

	const int mark_index = state.current_selected_mark;

	if (mark_index < 0 || mark_index >= data.GetMarksCount(anim_index))
	{
		return;
	}

	xr_stack_string16 temp;
	const int params_count = data.GetMarkParamsCount(anim_index, mark_index);

	for (int i = 0; i < params_count; ++i)
	{
		std::sprintf(temp.data(), "%d_mark%d", mark_index, i);
		state.list_box_motion_marks_params_names.push_back(temp);
	}
}

void OMFEditorUI_RebuildMotionMarkCaches(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	state.list_box_motion_marks_names.clear();
	state.list_box_motion_marks_params_names.clear();

	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount())
	{
		return;
	}

	const int marks_count = data.GetMarksCount(anim_index);

	for (int mark_index = 0; mark_index < marks_count; ++mark_index)
	{
		state.list_box_motion_marks_names.push_back(data.GetMarkName(anim_index, mark_index));
	}

	OMFEditorUI_RebuildMotionMarkParamLabels(state, data);
}

void OMFEditorUI_OnAnimParamSelected(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	state.current_selected_mark = -1;
	state.current_selected_mark_param = -1;
	OMFEditorUI_RebuildMotionMarkCaches(state, data);
}

bool OMFEditorUI_HasDuplicateMotionMark(
	SOMFEditorUIState& state,
	IOMFAnimDataProvider& data,
	const OMFData::omf_name_t& mark_name
)
{
	R_ASSERT(mark_name.empty() == false);

	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount() || mark_name.empty())
	{
		return false;
	}

	OMFData::omf_name_t lower_left;
	OMFData::omf_name_t lower_right = mark_name;

	xr_strlwr(lower_right);

	const int marks_count = data.GetMarksCount(anim_index);

	for (int mark_index = 0; mark_index < marks_count; ++mark_index)
	{
		lower_left = data.GetMarkName(anim_index, mark_index);
		xr_strlwr(lower_left);

		if (lower_left == lower_right)
		{
			return true;
		}
	}

	return false;
}

void OMFEditorUI_AddMotionMark(
	SOMFEditorUIState& state,
	IOMFAnimDataProvider& data,
	const OMFData::omf_name_t& mark_name
)
{
	R_ASSERT(mark_name.empty() == false);

	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount() || mark_name.empty())
	{
		return;
	}

	data.AddMark(anim_index, mark_name.c_str());
	OMFEditorUI_RebuildMotionMarkCaches(state, data);
}

void OMFEditorUI_DeleteMotionMark(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount())
	{
		return;
	}

	const int mark_index = state.current_selected_mark;

	if (mark_index < 0 || mark_index >= data.GetMarksCount(anim_index))
	{
		return;
	}

	data.DeleteMark(anim_index, mark_index);

	if (state.current_selected_mark >= data.GetMarksCount(anim_index))
	{
		state.current_selected_mark = -1;
	}

	if (state.current_selected_mark < 0)
	{
		state.current_selected_mark_param = -1;
	}

	OMFEditorUI_RebuildMotionMarkCaches(state, data);
}

void OMFEditorUI_AddMotionMarkParam(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount())
	{
		return;
	}

	const int mark_index = state.current_selected_mark;

	if (mark_index < 0 || mark_index >= data.GetMarksCount(anim_index))
	{
		return;
	}

	data.AddMarkParam(anim_index, mark_index);
	OMFEditorUI_RebuildMotionMarkParamLabels(state, data);
}

void OMFEditorUI_DeleteMotionMarkParam(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount())
	{
		return;
	}

	const int mark_index = state.current_selected_mark;

	if (mark_index < 0 || mark_index >= data.GetMarksCount(anim_index))
	{
		return;
	}

	const int param_index = state.current_selected_mark_param;

	if (param_index < 0 || param_index >= data.GetMarkParamsCount(anim_index, mark_index))
	{
		return;
	}

	data.DeleteMarkParam(anim_index, mark_index, param_index);

	if (state.current_selected_mark_param >= data.GetMarkParamsCount(anim_index, mark_index))
	{
		state.current_selected_mark_param = -1;
	}

	OMFEditorUI_RebuildMotionMarkParamLabels(state, data);
}

// returns true when something was deselected/closed (window must stay open)
bool OMFEditorUI_Deselect(SOMFEditorUIState& state)
{
	bool consumed = false;

#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 1
	if (state.current_selected_bone_rename != -1)
	{
		state.current_selected_bone_rename = -1;
		consumed = true;
	}
#endif

	if (ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId))
	{
		state.is_show_popup_boneparts_was_copied_to_clipboard_fail = false;
		state.is_show_popup_boneparts_was_copied_to_clipboard_suc = false;
		state.is_show_popup_marks_cleared = false;
		state.is_show_popup_renamehascollision = false;
		state.is_show_popup_rename_animation_param = false;
		state.is_show_popup_add_motion_mark = false;
		state.is_show_popup_duplicate_found_motion_mark = false;

		consumed = true;
	}

	if (state.current_selected_mark_param >= 0)
	{
		state.current_selected_mark_param = -1;
		consumed = true;
	}

	if (state.current_selected_mark >= 0)
	{
		state.current_selected_mark = -1;
		consumed = true;
	}

	return consumed;
}

bool OMFEditor_CopyBonePartsToClipboard(IOMFAnimDataProvider& data)
{
	xr_stack_string<1024 * 64> output;

	const int parts_count = data.GetBonePartsCount();

	for (int part_index = 0; part_index < parts_count; ++part_index)
	{
		output += "[";
		output += data.GetBonePartName(part_index);
		output += "]";
		output += "\n";

		const int bones_count = data.GetBonesCount(part_index);

		for (int bone_index = 0; bone_index < bones_count; ++bone_index)
		{
			output += data.GetBoneName(part_index, bone_index);
			output += "\n";
		}

		output += "\n";
		output += "\n";
	}

	bool result = false;

	if (xr_EFS)
	{
		result = xr_EFS->CopyTextToClipboard(output);
	}

	return result;
}

void OMFEditor_Init_ComboBones(CImGuiOMFEditor* p_state, OMFData& data)
{
	R_ASSERT2(p_state->combo_bones_data.empty(), "did you clear data before init?");
	R_ASSERT2(p_state->combo_bones_name_hashes.empty(), "did you clear data before init?");

	if (data.data_bone.count > 0)
	{
		for (int16_t i = 0; i < data.data_bone.count; ++i)
		{
			const auto& part = data.data_bone.parts[i];

			for (int16_t j = 0; j < part.count; ++j)
			{
				std::string_view view = part.bones[j].name.c_str();
				p_state->combo_bones_data.push_back(part.bones[j].name.c_str());
				p_state->combo_bones_name_hashes.insert(std::hash<std::string_view>()(view));
			}
		}
	}
}

void OMFEditor_Init(CImGuiOMFEditor* p_state)
{
	if (!p_state || !p_state->omf)
	{
		return;
	}

	p_state->provider.Bind(p_state->omf);
	p_state->Reset();

	p_state->combo_bones_data.clear();
	p_state->combo_bones_name_hashes.clear();

	p_state->is_show_popup_boneparts_rename_has_collision = false;
	p_state->is_show_popup_try_repair_applied = false;
	p_state->is_show_popup_add_anims_from = false;

	p_state->list_box_motion_marks_names.reserve(128);
	p_state->list_box_motion_marks_params_names.reserve(128);
	p_state->addanimsfrom_animation_list.reserve(64);

	p_state->is_input_text_addanimsfrom_updated_preview = false;
	p_state->is_input_text_addanimsfrom_was_edited = false;

	OMFEditor_Init_ComboBones(p_state, *p_state->omf);

	OMFEditorUI_RebuildComboCache(*p_state, p_state->provider);
	OMFEditorUI_RebuildMotionMarkCaches(*p_state, p_state->provider);

	if (p_state->omf->data_bone.count > 0)
	{
		R_ASSERT2(p_state->combo_bones_data.size(), "No bones detected");
		p_state->rename_temp_bone = p_state->combo_bones_data[0];
	}
}

bool OMFEditor_LoadOMF(OMFData& data, std::ifstream& file)
{
	R_ASSERT2(file.good(), "Invalid file passed");
	R_ASSERT2(file.is_open(), "Unable to open file!");

	bool status = false;
	if (file.is_open() && file.good())
	{
		status = OMFEditor_LoadOMF_AnimData(data.data_anim, file);

		if (!status)
		{
			return status;
		}

		status = OMFEditor_LoadOMF_BoneData(data.data_bone, file);

		if (!status)
		{
			return status;
		}

		status = OMFEditor_LoadOMF_AnimParamsData(data.data_bone.ogf_version, data.data_anim.animations_count, data.data_animparams, file);

		if (!status)
		{
			return status;
		}
	}

	return status;
}

void OMFEditor_LoadFile(CImGuiOMFEditor* p_state)
{
	if (p_state)
	{
		if (xr_EFS)
		{
			xr_stack_tstring<sizeof(string_path)> local_path;
			bool status = xr_EFS->GetOpenName(local_path, XR_TEXT("OMF file\0*.omf\0"));
			p_state->is_file_loaded = status;

			if (p_state->is_file_loaded)
			{
#ifdef IXR_WINDOWS
				status = Platform::WCHAR_TO_CHAR(local_path, p_state->path);
				R_ASSERT2(status, "report to developers! Unable to convert your path to multibyte string");


				std::ifstream file_omf(p_state->path.c_str(), std::ios::binary);

				if (file_omf.is_open())
				{
					if (p_state->omf)
					{
						p_state->omf->destroy();

						delete p_state->omf;
						p_state->omf = new OMFData();
					}
					else
					{
						p_state->omf = new OMFData();
					}

					p_state->list_box_motion_marks_names.clear();
					p_state->list_box_motion_marks_params_names.clear();

					OMFEditor_LoadOMF(*p_state->omf, file_omf);
				}
				else
				{
					ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", "failed to load file!");
				}

				file_omf.close();

				p_state->is_file_loaded = status;

				OMFEditor_Init(p_state);
#endif
			}
		}
	}
}

void OMFEditor_RenameBone(int bone_id, const OMFData::omf_name_t& new_name, OMFData& data)
{
	if (data.data_bone.count > 0)
	{
		bool was_found = false;
		int global_index = 0;
		for (int16_t i = 0; i < data.data_bone.count; ++i)
		{
			auto& part = data.data_bone.parts[i];

			for (int16_t j = 0; j < part.count; ++j)
			{
				auto& bone = part.bones[j];

				if (global_index == bone_id)
				{
					was_found = true;
					bone.name = new_name;
					break;
				}

				++global_index;
			}

			if (was_found)
			{
				break;
			}
		}

		R_ASSERT(was_found && "unable to find it means something is corrupted!");
	}
}

void OMFEditor_SaveOMF(
	CImGuiOMFEditor* pState,
	xr_stack_tstring<sizeof(string_path)>& path_where_to_save_file,
	bool silent = false
)
{
	R_ASSERT(pState);
	R_ASSERT(pState->omf);
	R_ASSERT(pState->is_file_loaded);
	R_ASSERT(path_where_to_save_file.empty() == false);

	if (
		pState &&
		pState->omf &&
		pState->is_file_loaded &&
		path_where_to_save_file.empty() == false
	)
	{
		xr_strlwr(path_where_to_save_file);

		if (path_where_to_save_file.find(XR_TEXT(".omf")) == xr_stack_tstring<1>::npos)
		{
			path_where_to_save_file.append(XR_TEXT(".omf"));
		}

		std::ofstream file(path_where_to_save_file.c_str(), std::ios_base::binary);

		R_ASSERT(file.good());

		if (file.good() == false)
		{
			ShowMessageBox(_eMessageBoxStatus::kError, "Check writing policy for your disk", "Failed to create file for writing");
			return;
		}

		bool status = OMFEditor_SaveOMF_AnimData(pState->omf->data_anim, file);
		R_ASSERT(status);

		if (status)
		{
			status = OMFEditor_SaveOMF_BoneData(pState->omf->data_bone, pState->omf->data_animparams, file);
			R_ASSERT(status);

			if (status)
			{
				status = OMFEditor_SaveOMF_AnimParamsData(
					pState->omf->data_bone.ogf_version,
					pState->omf->data_animparams,
					file
				);
				R_ASSERT(status);

				if (status)
				{
					if (silent == false)
					{
						ShowMessageBox(_eMessageBoxStatus::kSuccess, "", "File is saved successfully!");
					}
				}
				else
				{
					ShowMessageBox(_eMessageBoxStatus::kError, "ERROR", "Failed to save anim params data, can't save file!");
					file.close();
					std::filesystem::remove(path_where_to_save_file.c_str());
				}
			}
			else
			{
				ShowMessageBox(_eMessageBoxStatus::kError, "ERROR", "Failed to save bone data, can't save file");
				file.close();
				std::filesystem::remove(path_where_to_save_file.c_str());
			}
		}
		else
		{
			ShowMessageBox(_eMessageBoxStatus::kError, "ERROR", "Failed to save anim data, can't save file");
			file.close();
			std::filesystem::remove(path_where_to_save_file.c_str());
		}

		if (file.is_open())
		{
			file.close();
		}
	}
}

void OMFEditor_TryRepair(
	CImGuiOMFEditor* pState
)
{
	R_ASSERT(pState);

	if (
		pState &&
		pState->omf &&
		pState->is_file_loaded &&
		pState->omf->data_animparams.params.empty() == false &&
		pState->omf->data_anim.anims.empty() == false
	)
	{
		short i = 0;
		for (auto& param : pState->omf->data_animparams.params)
		{
			param.motion_id = i;
			++i;
		}

		i = 1;

		for (auto& anim : pState->omf->data_anim.anims)
		{
			anim.name = pState->omf->data_animparams.params[i - 1].name;
			anim.section_id = i;
			++i;
		}

		xr_stack_tstring<sizeof(string_path)> path = Platform::ANSI_TO_TCHAR(g_pOMFEditor->path.c_str());

		OMFEditor_SaveOMF(
			g_pOMFEditor,
			path,
			true
		);

		g_pOMFEditor->is_show_popup_try_repair_applied = true;
	}
}

void OMFEditor_SwapAnimMarks(
	CImGuiOMFEditor* pState
)
{
	R_ASSERT(pState);

	if (
		pState &&
		pState->omf &&
		pState->is_file_loaded
	)
	{
		if (pState->temp_omf)
		{
			pState->temp_omf->destroy();
			delete pState->temp_omf;
			pState->temp_omf = nullptr;
		}

		if (pState->temp_omf == nullptr)
		{
			pState->temp_omf = pState->omf;
			pState->omf = nullptr;
		}

		R_ASSERT(pState->omf == nullptr);

		if (pState->omf == nullptr)
		{
			xr_stack_tstring<sizeof(string_path)> local_path;
			bool status = xr_EFS->GetOpenName(local_path, XR_TEXT("OMF file\0*.omf\0"));

			if (status)
			{
#ifdef IXR_WINDOWS
				status = Platform::WCHAR_TO_CHAR(local_path, pState->path);
				R_ASSERT(status);

				R_ASSERT(std::filesystem::exists(pState->path.c_str()));

				std::ifstream file_omf(local_path.c_str(), std::ios::binary);

				if (file_omf.is_open())
				{
					pState->current_selected_animation_param = 0;
					pState->current_selected_mark = -1;
					pState->current_selected_mark_param = -1;
					pState->omf = new OMFData();

					status = OMFEditor_LoadOMF(*pState->omf, file_omf);
					R_ASSERT(status);

					if (status)
					{
						bool dlg_showed = false;
						bool dlg_option_overwrite_enabled = false;


						OMFData::omf_name_t name_from_temp;
						OMFData::omf_name_t name_from_current;

						for (int i = 0; i < pState->temp_omf->data_anim.animations_count; ++i)
						{
							auto& param_temp = pState->temp_omf->data_animparams.params[i];

							if (param_temp.marks_count > 0)
							{
								for (int j = 0; j < pState->omf->data_anim.animations_count; ++j)
								{
									name_from_temp = pState->temp_omf->data_anim.anims[i].name;
									name_from_current = pState->omf->data_anim.anims[j].name;

									// we must gurantee how we compare names...
									xr_strlwr(name_from_temp);
									xr_strlwr(name_from_current);

									if (name_from_temp == name_from_current)
									{
										if (!dlg_showed)
										{
											dlg_option_overwrite_enabled = ShowMessageBox(_eMessageBoxStatus::kYesOrNo, "Info", "Overwrite existing motion marks?") == 1;
											dlg_showed = true;
										}

										if (dlg_option_overwrite_enabled)
										{
											auto& param = pState->omf->data_animparams.params[j];

											param.marks.clear();
											param.marks_count = param_temp.marks_count;
											param.marks = param_temp.marks;
										}
									}
								}
							}
						}
					}
					else
					{
						delete pState->omf;
						pState->omf = pState->temp_omf;
						pState->temp_omf = nullptr;
						ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", "failed to load file!");
					}
				}
				else
				{
					R_ASSERT(pState->omf == nullptr);
					pState->omf = pState->temp_omf;
					pState->temp_omf = nullptr;
					ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", "failed to load file!");
				}
#endif
			}
			else
			{
				R_ASSERT(pState->omf == nullptr);
				pState->omf = pState->temp_omf;
				pState->temp_omf = nullptr;
			}

			R_ASSERT(pState->omf);
			if (pState->omf)
			{
				pState->provider.Bind(pState->omf);
				pState->current_selected_animation_param = 0;
				pState->current_selected_mark = -1;
				pState->current_selected_mark_param = -1;

				OMFEditorUI_RebuildComboCache(*pState, pState->provider);
				OMFEditorUI_RebuildMotionMarkCaches(*pState, pState->provider);
			}
		}
	}
}

void OMFEditor_MergeWith(
	CImGuiOMFEditor* pState
)
{
	R_ASSERT(pState);
	R_ASSERT(pState->omf);

	if (
		pState &&
		pState->is_file_loaded &&
		pState->omf
	)
	{
		xr_stack_tstring<sizeof(string_path)> local_path;
		bool status = xr_EFS->GetOpenName(local_path, XR_TEXT("OMF file\0*.omf\0"));

		if (
			status &&
			local_path.empty() == false &&
			std::filesystem::exists(local_path.c_str())
		)
		{
			OMFData from;
			std::ifstream file(local_path.c_str(), std::ios::binary);

			if (file.good())
			{
				status = OMFEditor_LoadOMF(from, file);

				if (status)
				{
					int overwrite = ShowMessageBox(_eMessageBoxStatus::kYesOrNo, "", "Overwrite existed?");

					if (overwrite != 1 && overwrite != 0)
					{
						overwrite = 0;
					}

					OMFData::omf_name_t current_omf_name;
					OMFData::omf_name_t from_omf_name;

					unsigned char raw_mem[sizeof(int) * 64];
					std::pmr::monotonic_buffer_resource mbr{raw_mem, sizeof(raw_mem)};
					std::pmr::polymorphic_allocator<int> pmr_al{&mbr};
					std::pmr::vector<int> elements_to_remove_by_lookupids{pmr_al};


					for (int i = 0; i <
									pState->omf->data_anim.animations_count;
						 ++i)
					{
						for (int j = 0; j < from.data_anim.animations_count; ++j)
						{
							current_omf_name = pState->omf->data_anim.anims[i].name;
							from_omf_name = from.data_anim.anims[j].name;

							xr_strlwr(current_omf_name);
							xr_strlwr(from_omf_name);

							if (current_omf_name == from_omf_name)
							{
								if (overwrite == 1)
								{
									elements_to_remove_by_lookupids.push_back(i);
								}
								else
								{
									elements_to_remove_by_lookupids.push_back(j);
								}
							}
						}
					}

					std::sort(
						elements_to_remove_by_lookupids.begin(),
						elements_to_remove_by_lookupids.end(),
						std::greater<int>()
					);

#ifdef DEBUG
					if (overwrite == 1)
					{
						R_ASSERT(pState->omf->data_anim.anims.size() == pState->omf->data_animparams.params.size());
					}
					else
					{
						R_ASSERT(from.data_anim.anims.size() == from.data_animparams.params.size());
					}
#endif

					for (int id : elements_to_remove_by_lookupids)
					{
						if (overwrite == 1)
						{
							pState->omf->data_anim.anims.erase(pState->omf->data_anim.anims.begin() + id);
							pState->omf->data_animparams.params.erase(pState->omf->data_animparams.params.begin() + id);
						}
						else
						{
							from.data_anim.anims.erase(from.data_anim.anims.begin() + id);
							from.data_animparams.params.erase(from.data_animparams.params.begin() + id);
						}
					}

					int temp_id = 0;

					for (const auto& anim : pState->omf->data_anim.anims)
					{
						pState->omf->data_animparams.params[temp_id].motion_id = temp_id;
						++temp_id;
					}

					temp_id = 0;

					for (const auto& anim : from.data_anim.anims)
					{
						pState->omf->data_anim.anims.push_back(anim);

						from.data_animparams.params[temp_id].motion_id = static_cast<decltype(OMFData::AnimParamsData::AnimParams::motion_id)>(pState->omf->data_animparams.params.size());
						pState->omf->data_animparams.params.push_back(from.data_animparams.params[temp_id]);
						++temp_id;
					}

					pState->omf->data_anim.animations_count = static_cast<decltype(OMFData::AnimData::animations_count)>(pState->omf->data_anim.anims.size());
					pState->omf->data_animparams.count = static_cast<decltype(OMFData::AnimParamsData::count)>(pState->omf->data_animparams.params.size());

					temp_id = 0;

					for (auto& param : pState->omf->data_animparams.params)
					{
						param.motion_id = temp_id;
						++temp_id;
					}

					temp_id = 1;

					for (auto& anim : pState->omf->data_anim.anims)
					{
						anim.section_id = temp_id;
						++temp_id;
					}

					OMFEditorUI_RebuildComboCache(*pState, pState->provider);
					OMFEditorUI_RebuildMotionMarkCaches(*pState, pState->provider);
				}
			}

			file.close();
		}
	}
}

void OMFEditor_AddAnimsFrom_Popup(
	CImGuiOMFEditor* pState
)
{
	R_ASSERT(pState);
	R_ASSERT(pState->omf);

	if (
		pState &&
		pState->is_file_loaded &&
		pState->omf
	)
	{
		pState->is_show_popup_add_anims_from = true;
	}
}

void RequestHandler_OMFEditor(const SRequestData& req)
{
	R_ASSERT2(static_cast<eImGuiEditorType>(req.editor_type) == eImGuiEditorType::kOMFEditor, "mistaken workload calling! that means data was corrupted or some error occurred");

	eRequestType_OMFEditor req_type = static_cast<eRequestType_OMFEditor>(req.request_type);

	switch (req_type)
	{
		case eRequestType_OMFEditor::kReadSettings:
		{
			break;
		}
		case eRequestType_OMFEditor::kWriteSettings:
		{
			break;
		}
		case eRequestType_OMFEditor::kLoadFile:
		{
			break;
		}
		case eRequestType_OMFEditor::kDeselectCurrentSelectedOrHideWindow:
		{
			bool can_hide_window = true;

			if (g_pOMFEditor)
			{
				if (ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId))
				{
					g_pOMFEditor->is_show_popup_boneparts_rename_has_collision = false;
					g_pOMFEditor->is_show_popup_try_repair_applied = false;
					g_pOMFEditor->is_show_popup_add_anims_from = false;
				}

				if (OMFEditorUI_Deselect(*g_pOMFEditor))
				{
					can_hide_window = false;
				}
			}

#if IXRAY_OMF_EDITOR_TAB_GAME == 1
			if (g_pOMFGame && OMFEditorUI_Deselect(*g_pOMFGame))
			{
				can_hide_window = false;
			}
#endif

			if (can_hide_window)
			{
				Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)] = false;
			}

			break;
		}
		case eRequestType_OMFEditor::kShutdown:
		{
			if (g_pOMFEditor)
			{
				if (g_pOMFEditor->omf)
				{
					g_pOMFEditor->omf->destroy();

					delete g_pOMFEditor->omf;
					g_pOMFEditor->omf = nullptr;
				}

				if (g_pOMFEditor->temp_omf)
				{
					g_pOMFEditor->temp_omf->destroy();

					delete g_pOMFEditor->temp_omf;
					g_pOMFEditor->temp_omf = nullptr;
				}

				delete g_pOMFEditor;
				g_pOMFEditor = nullptr;
			}

#if IXRAY_OMF_EDITOR_TAB_GAME == 1
			if (g_pOMFGame)
			{
				delete g_pOMFGame;
				g_pOMFGame = nullptr;
			}
#endif

			break;
		}
		default:
		{
			R_ASSERT(!"invalid request type or request type of different editor");
			break;
		}
	}
}

void RenderOMFEditor_Draw_TableHeader()
{
	if (ImGui::BeginMenuBar())
	{
		if (ImGui::BeginMenu("File##OMFEditor"))
		{
			if (ImGui::MenuItem("Load"))
			{
				OMFEditor_LoadFile(g_pOMFEditor);
			}

			if (g_pOMFEditor->is_file_loaded)
			{
				if (ImGui::MenuItem("Close"))
				{
					g_pOMFEditor->is_file_loaded = false;
					g_pOMFEditor->path[0] = 0;
				}
			}

			ImGui::EndMenu();
		}


		if (g_pOMFEditor->is_file_loaded)
		{
			if (ImGui::MenuItem("Save##ToolsInGameImGui_OMFEditor"))
			{
				if (xr_EFS)
				{
					xr_stack_tstring<sizeof(string_path)> local_path;
					bool status = xr_EFS->GetSaveName(local_path, XR_TEXT("OMF file\0*.omf\0"));

					if (status)
					{
						R_ASSERT(local_path.empty() == false);

						if (local_path.empty() == false)
						{
							OMFEditor_SaveOMF(
								g_pOMFEditor,
								local_path
							);
						}
					}
				}
			}

			if (ImGui::MenuItem("Merge with##ToolsInGameImGui_OMFEditor"))
			{
				OMFEditor_MergeWith(g_pOMFEditor);
			}

			if (ImGui::MenuItem("Add anims from##ToolsInGameImGui_OMFEditor"))
			{
				OMFEditor_AddAnimsFrom_Popup(g_pOMFEditor);
			}

			if (ImGui::MenuItem("Try repair##ToolsInGameImGui_OMFEditor"))
			{
				OMFEditor_TryRepair(g_pOMFEditor);
			}

			if (ImGui::MenuItem("Swap anim marks##ToolsInGameImGui_OMFEditor"))
			{
				OMFEditor_SwapAnimMarks(g_pOMFEditor);
			}
		}

		ImGui::EndMenuBar();
	}
}

// ==============================================================
// Shared UI rendering. Same widgets for the editor (file) tab and
// the game (live data) tab; the provider decides what is editable.
// ==============================================================

void RenderOMFEditorUI_BoneRenaming(
	SOMFEditorUIState& state,
	IOMFAnimDataProvider& data,
	int bone_id,
	int part_index,
	int bone_index
)
{
#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 1
	ImGui::PushID(bone_id);

	if (state.current_selected_bone_rename == bone_id)
	{
		if (ImGui::InputText(
				"##ToolsOMFEditor_DirectRenamingOfBone",
				state.rename_temp_bone.data(),
				state.rename_temp_bone.max_size(),
				ImGuiInputTextFlags_EnterReturnsTrue
			) &&
			state.rename_temp_bone.size() > 0)
		{
			data.RenameBone(part_index, bone_index, state.rename_temp_bone.c_str());
			state.current_selected_bone_rename = -1;
		}
	}
	else
	{
		if (ImGui::Selectable(data.GetBoneName(part_index, bone_index)))
		{
			// Optional: handle selection

			state.current_selected_bone_rename = -1;
		}

		// Activate editing on double-click
		if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(0))
		{
			state.current_selected_bone_rename = bone_id;
			state.rename_temp_bone = data.GetBoneName(part_index, bone_index);
		}
	}

	ImGui::PopID();
#endif
}

void RenderOMFEditorUI_BonePartsToolbar(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	if (ImGui::Button("copy to clipboard##ToolsInGameImGui_OMFEditor_ShowBoneParts"))
	{
		bool status = OMFEditor_CopyBonePartsToClipboard(data);

		if (status)
		{
			state.is_show_popup_boneparts_was_copied_to_clipboard_suc = true;
		}
		else
		{
			state.is_show_popup_boneparts_was_copied_to_clipboard_fail = true;
		}
	}
}

void RenderOMFEditorUI_BonePartsTabs(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	ImGui::SeparatorText("Bone Parts");

	const int parts_count = data.GetBonePartsCount();

	if (parts_count == 0)
	{
		ImGui::Text("No bones!");
		return;
	}

	if (ImGui::BeginTabBar("##ToolsOMFEditor_TableMain_BonesPartSection"))
	{
		for (int part_index = 0; part_index < parts_count; ++part_index)
		{
			if (ImGui::BeginTabItem(data.GetBonePartName(part_index)))
			{
				const int bones_count = data.GetBonesCount(part_index);

				ImGui::Text("bone count: %d", bones_count);
				ImGui::Separator();

				// if (ImGui::CollapsingHeader("Bones"))
				{
					if (ImGui::BeginChild("##ToolsOMFEditor_BonesScrollableRegion"))
					{
#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 0
						for (int bone_index = 0; bone_index < bones_count; ++bone_index)
						{
							ImGui::Text(data.GetBoneName(part_index, bone_index));
						}
#else
						for (int bone_index = 0; bone_index < bones_count; ++bone_index)
						{
							if (data.CanRenameBones())
							{
								RenderOMFEditorUI_BoneRenaming(state, data, bone_index, part_index, bone_index);
							}
							else
							{
								ImGui::Text(data.GetBoneName(part_index, bone_index));
							}
						}
#endif
					}

					ImGui::EndChild();
				}

				ImGui::EndTabItem();
			}
		}

		ImGui::EndTabBar();
	}
}

void RenderOMFEditor_Draw_TableMain_BonesRenaming_Section()
{
#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 0
	if (ImGui::CollapsingHeader("Rename bones##ToolsInGameImGui_OMFEditor_Data_Body"))
	{
		if (g_pOMFEditor->omf->data_bone.count > 0)
		{
			ImGui::SeparatorText("Select bone");

			if (ImGui::Combo("Bones##ToolsInGameImGui_OMFEditor_RenameBones", &g_pOMFEditor->current_selected_bone_rename, g_pOMFEditor->combo_bones_data.data(), g_pOMFEditor->combo_bones_data.size()))
			{
				g_pOMFEditor->rename_temp_bone = g_pOMFEditor->combo_bones_data[g_pOMFEditor->current_selected_bone_rename];
			}

			ImGui::SeparatorText("Edit");

			ImGui::Text("bone id: %d", g_pOMFEditor->current_selected_bone_rename);
			ImGui::InputText("##ToolsInGameImGui_OMFEditor_RenameBoneIT", g_pOMFEditor->rename_temp_bone.data(), g_pOMFEditor->rename_temp_bone.max_size());
			ImGui::SameLine();
			if (ImGui::Button("apply##ToolsInGameImGui_OMFEditor_RenameBone"))
			{
				size_t hash_temp = std::hash<xr_string_view>()(xr_string_view(g_pOMFEditor->rename_temp_bone.c_str()));

				if (g_pOMFEditor->combo_bones_name_hashes.find(hash_temp) != g_pOMFEditor->combo_bones_name_hashes.end() && g_pOMFEditor->combo_bones_data[g_pOMFEditor->current_selected_bone_rename] != g_pOMFEditor->rename_temp_bone)
				{
					ImGui::OpenPopup(_kOMFEditorModalWindow_BoneRenameHasCollion);
				}
				else
				{
					size_t hash_current = std::hash<std::string_view>()(g_pOMFEditor->combo_bones_data[g_pOMFEditor->current_selected_bone_rename]);
					if (g_pOMFEditor->combo_bones_name_hashes.find(hash_current) != g_pOMFEditor->combo_bones_name_hashes.end())
					{
						g_pOMFEditor->combo_bones_name_hashes.erase(hash_current);
					}

					g_pOMFEditor->combo_bones_name_hashes.insert(hash_temp);
					OMFEditor_RenameBone(g_pOMFEditor->current_selected_bone_rename, g_pOMFEditor->rename_temp_bone, *g_pOMFEditor->omf);
				}
			}

			bool cross = true;
			if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_BoneRenameHasCollion, &cross, ImGuiWindowFlags_AlwaysAutoResize))
			{
				ImGui::Text("You have already same name, can't rename current bone!");
				ImGui::EndPopup();
			}
		}
		else
		{
			ImGui::Text("you don't have any bones for renaming!");
		}
	}
#endif
}

void RenderOMFEditorUI_MotionMarks(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount())
	{
		return;
	}

	bool has_motion_marks_selected = data.SupportsMotionMarks();

	has_motion_marks_selected &= state.is_motion_marks_enabled;

	if (!has_motion_marks_selected && state.is_motion_marks_enabled)
	{
		ImGui::Text("Motion marks are not supported by this data (requires OGF version 4)");
	}

	ImGui::BeginDisabled(has_motion_marks_selected == false);

	if (ImGui::CollapsingHeader("Motion marks"))
	{
		if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body3", 2))
		{
			ImGui::TableNextRow();

			ImGui::TableSetColumnIndex(0);

			ImGui::SeparatorText("Mark");

			bool reselected = ImGui::ListBox(
				"##ToolsOMFEditor_MarkGroupLB",
				&state.current_selected_mark,
				state.list_box_motion_marks_names.data(),
				state.list_box_motion_marks_names.size()
			);

			if (reselected)
			{
				OMFEditorUI_RebuildMotionMarkParamLabels(state, data);
			}

			if (
				ImGui::Button("Add##ToolsOMFEditor_MarkAdd") &&
				state.is_show_popup_add_motion_mark == false
			)
			{
				state.is_show_popup_add_motion_mark = true;
				state.temp_motion_mark_name.clear();
				state.temp_motion_mark_name = "NewGroup";
			}

			ImGui::SameLine();

			if (ImGui::Button("Delete##ToolsOMFEditor_MarkDelete"))
			{
				OMFEditorUI_DeleteMotionMark(state, data);
			}

			ImGui::SeparatorText("Mark Param");

			ImGui::BeginDisabled(state.current_selected_mark == -1);

			ImGui::ListBox(
				"##ToolsOMFEditor_MarkParamLB",
				&state.current_selected_mark_param,
				[](void* user_data, int idx) -> const char*
				{
					R_ASSERT(user_data);

					xr_vector<xr_stack_string16>* pCasted = static_cast<xr_vector<xr_stack_string16>*>(user_data);

					R_ASSERT(idx <= pCasted->size() - 1);

					return pCasted->operator[](idx).c_str();
				},
				&state.list_box_motion_marks_params_names,
				state.list_box_motion_marks_params_names.size()
			);

			if (ImGui::Button("Add##ToolsOMFEditor_MarkParamAdd"))
			{
				OMFEditorUI_AddMotionMarkParam(state, data);
			}

			ImGui::SameLine();

			if (ImGui::Button("Delete##ToolsOMFEditor_MarkParamDelete"))
			{
				OMFEditorUI_DeleteMotionMarkParam(state, data);
			}

			ImGui::EndDisabled();

			ImGui::TableSetColumnIndex(1);

			bool is_mark_settings_disabled = (has_motion_marks_selected) && (state.current_selected_mark_param == -1);

			ImGui::BeginDisabled(is_mark_settings_disabled);


			ImGui::SeparatorText("Mark settings");

			const int mark_index = state.current_selected_mark;
			const int mark_param_index = state.current_selected_mark_param;

			if (
				mark_param_index == -1 ||
				mark_index == -1 ||
				mark_index >= data.GetMarksCount(anim_index) ||
				mark_param_index >= data.GetMarkParamsCount(anim_index, mark_index)
			)
			{
				float fStart{};
				ImGui::DragFloat("Start##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fStart);


				float fEnd{};
				ImGui::DragFloat("End##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fEnd);
			}
			else
			{
				float fStart{};
				float fEnd{};
				data.GetMarkParam(anim_index, mark_index, mark_param_index, fStart, fEnd);

				bool changed = ImGui::DragFloat("Start##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fStart);
				changed |= ImGui::DragFloat("End##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fEnd);

				if (changed)
				{
					data.SetMarkParam(anim_index, mark_index, mark_param_index, fStart, fEnd);
				}
			}


			ImGui::EndTable();
			ImGui::EndDisabled();
		}
	}

	ImGui::EndDisabled();
}

void RenderOMFEditorUI_SharedModals(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	unsigned char modal_opened = 0;

	modal_opened += state.is_show_popup_marks_cleared;
	modal_opened += state.is_show_popup_rename_animation_param;
	modal_opened += state.is_show_popup_boneparts_was_copied_to_clipboard_suc;
	modal_opened += state.is_show_popup_boneparts_was_copied_to_clipboard_fail;
	modal_opened += state.is_show_popup_renamehascollision;
	modal_opened += state.is_show_popup_add_motion_mark;
	modal_opened += state.is_show_popup_duplicate_found_motion_mark;

	R_ASSERT(modal_opened <= 1);

	if (modal_opened == 1)
	{
		if (state.is_show_popup_marks_cleared)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_AnimationParamMotionMarksCleared);
		}

		if (state.is_show_popup_rename_animation_param && data.CanRenameAnimations())
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_RenameAnimationParam);
		}

		if (state.is_show_popup_boneparts_was_copied_to_clipboard_suc)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardSuccessful);
		}

		if (state.is_show_popup_boneparts_was_copied_to_clipboard_fail)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardFailed);
		}

		if (state.is_show_popup_add_motion_mark)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_AddMotionMark);
		}

		if (state.is_show_popup_duplicate_found_motion_mark)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_DuplicateFoundMotionMark);
		}
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_AnimationParamMotionMarksCleared, &state.is_show_popup_marks_cleared))
	{
		ImGui::Text("Motion marks are cleared!");

		if (ImGui::Button("Ok##ToolsOMFEditor_MotionMarksCleared"))
		{
			state.is_show_popup_marks_cleared = false;
		}

		ImGui::EndPopup();
	}

	if (data.CanRenameAnimations())
	{
		if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_RenameAnimationParam, &state.is_show_popup_rename_animation_param, ImGuiWindowFlags_AlwaysAutoResize))
		{
			ImGui::InputText("##ToolsInGameImGui_OMFEditor_RenameAnimationParamInputText", state.rename_temp.data(), state.rename_temp.max_size());

			ImGui::SetItemDefaultFocus();
			if (ImGui::Button("Save##ToolsInGameImGui_OMFEditor_RenameAnimationParam"))
			{
				const char* current_name = data.GetAnimParamName(state.current_selected_animation_param);

				if (data.AnimParamNameExists(state.rename_temp.c_str()) && !(state.rename_temp == current_name))
				{
					state.is_show_popup_renamehascollision = true;
					state.is_show_popup_rename_animation_param = false;
				}
				else
				{
					data.RenameAnimParam(state.current_selected_animation_param, state.rename_temp.c_str());
					state.is_show_popup_rename_animation_param = false;
				}
			}

			ImGui::SameLine();

			if (ImGui::Button("Cancel##ToolsInGameImGui_OMFEditor_RenameAnimationParam"))
			{
				state.is_show_popup_rename_animation_param = false;
			}

			ImGui::EndPopup();
		}
	}

	if (state.is_show_popup_renamehascollision)
	{
		ImGui::OpenPopup(_kOMFEditorModalWindow_WarningRenameHasCollision);
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_WarningRenameHasCollision, &state.is_show_popup_renamehascollision, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Failed to rename because you have already same name!");
		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardFailed, &state.is_show_popup_boneparts_was_copied_to_clipboard_fail, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Text wasn't copied to your clipboard! Try again or report to developers!");

		if (ImGui::Button("OK##ToolsInGameImGui_OMFEditor_ClipBoard"))
		{
			state.is_show_popup_boneparts_was_copied_to_clipboard_fail = false;
		}

		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardSuccessful, &state.is_show_popup_boneparts_was_copied_to_clipboard_suc, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Text was successfully copied to your clipboard!");

		if (ImGui::Button("OK##ToolsInGameImGui_OMFEditor_ClipBoard"))
		{
			state.is_show_popup_boneparts_was_copied_to_clipboard_suc = false;
		}

		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(
			_kOMFEditorModalWindow_AddMotionMark,
			&state.is_show_popup_add_motion_mark,
			ImGuiWindowFlags_AlwaysAutoResize
		))
	{
		if (ImGui::InputText(
				"##ToolsOMFEditor_MotionMarkIT",
				state.temp_motion_mark_name.data(),
				state.temp_motion_mark_name.max_size()
			))
		{
		}

		if (ImGui::Button("Ok##ToolsOMFEditor_MotionMarkITOK"))
		{
			if (OMFEditorUI_HasDuplicateMotionMark(
					state,
					data,
					state.temp_motion_mark_name
				))
			{
				state.is_show_popup_duplicate_found_motion_mark = true;
			}
			else
			{
				OMFEditorUI_AddMotionMark(state, data, state.temp_motion_mark_name);
			}

			state.is_show_popup_add_motion_mark = false;
		}

		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(
			_kOMFEditorModalWindow_DuplicateFoundMotionMark
		))
	{
		ImGui::Text("failed to add motion mark because it is already added!");

		if (ImGui::Button("Ok##ToolsOMFEditor_DuplicateFoundMM"))
		{
			state.is_show_popup_add_motion_mark = true;
			state.is_show_popup_duplicate_found_motion_mark = false;
		}

		ImGui::EndPopup();
	}
}

void RenderOMFEditor_Draw_EditorModals()
{
	unsigned char modal_opened = 0;

	modal_opened += g_pOMFEditor->is_show_popup_try_repair_applied;
	modal_opened += g_pOMFEditor->is_show_popup_add_anims_from;

	R_ASSERT(modal_opened <= 1);

	if (modal_opened == 1)
	{
		if (g_pOMFEditor->is_show_popup_try_repair_applied)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_TryRepairApplied);
		}

		if (g_pOMFEditor->is_show_popup_add_anims_from)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_AddAnimsFrom);
		}
	}

	if (ImGui::BeginPopupModal(
			_kOMFEditorModalWindow_TryRepairApplied,
			&g_pOMFEditor->is_show_popup_try_repair_applied,
			ImGuiWindowFlags_AlwaysAutoResize
		))
	{
		ImGui::Text("Repair was applied!");

		if (ImGui::Button("Ok##ToolsOMFEditor_TryRepair"))
		{
			g_pOMFEditor->is_show_popup_try_repair_applied = false;
		}

		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(
			_kOMFEditorModalWindow_AddAnimsFrom,
			&g_pOMFEditor->is_show_popup_add_anims_from,
			ImGuiWindowFlags_AlwaysAutoResize
		))
	{
		ImGui::Text("Insert text:");

		ImGui::SameLine();

		ImGui::BeginDisabled();

		ImGui::Text("(?)");

		ImGui::SetItemTooltip("You need to insert animation list using newline ('\\n') separator like this:\nanim1\nanim2\nanim3");

		ImGui::EndDisabled();

		if (g_pOMFEditor->input_text_add_anims_from_buffer.empty())
		{
			if (ImGui::InputTextMultiline(
					"##AddAnimsFrom_ITM",
					g_pOMFEditor->input_text_add_anims_from_buffer.data(),
					g_pOMFEditor->input_text_add_anims_from_buffer.max_size()
				))
			{
				g_pOMFEditor->is_input_text_addanimsfrom_updated_preview = true;
			}
		}
		else
		{
			if (ImGui::BeginTable("##AddAnimsFrom_Table", 2))
			{
				ImGui::TableNextRow();

				ImGui::TableSetColumnIndex(0);

				g_pOMFEditor->is_input_text_addanimsfrom_was_edited = false;

				if (ImGui::InputTextMultiline(
						"##AddAnimsFrom_ITM",
						g_pOMFEditor->input_text_add_anims_from_buffer.data(),
						g_pOMFEditor->input_text_add_anims_from_buffer.max_size()
					))
				{
					g_pOMFEditor->is_input_text_addanimsfrom_was_edited = true;
					g_pOMFEditor->is_input_text_addanimsfrom_updated_preview = true;
				}

				if (
					g_pOMFEditor->is_input_text_addanimsfrom_updated_preview &&
					g_pOMFEditor->is_input_text_addanimsfrom_was_edited == false
				)
				{
					g_pOMFEditor->addanimsfrom_animation_list.clear();

					if (g_pOMFEditor->input_text_add_anims_from_buffer.empty() == false)
					{
					}

					g_pOMFEditor->is_input_text_addanimsfrom_updated_preview = false;
				}

				ImGui::TableSetColumnIndex(1);


				ImGui::EndTable();
			}
		}


		if (ImGui::Button("Ok##AddAnimsFrom"))
		{
		}

		ImGui::SameLine();

		if (ImGui::Button("Cancel##AddAnimsFrom"))
		{
			g_pOMFEditor->is_show_popup_add_anims_from = false;
		}


		ImGui::EndPopup();
	}
}

void RenderOMFEditorUI_Params(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	const int anim_index = state.current_selected_animation_param;

	if (anim_index < 0 || anim_index >= data.GetAnimParamsCount())
	{
		return;
	}

	if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body_Params", 2))
	{
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);

		constexpr float _kMinSpeed = 0.001f;

		float fSpeed = data.GetSpeed(anim_index);

		if (ImGui::DragFloat("Speed", &fSpeed, _kMinSpeed))
		{
			data.SetSpeed(anim_index, fSpeed);
		}

		float fPower = data.GetPower(anim_index);

		if (ImGui::DragFloat("Power", &fPower, _kMinSpeed))
		{
			data.SetPower(anim_index, fPower);
		}

		float fAccrue = data.GetAccrue(anim_index);

		if (ImGui::DragFloat("Accrue", &fAccrue, _kMinSpeed))
		{
			data.SetAccrue(anim_index, fAccrue);
		}

		float fFalloff = data.GetFalloff(anim_index);

		if (ImGui::DragFloat("Falloff", &fFalloff, _kMinSpeed))
		{
			data.SetFalloff(anim_index, fFalloff);
		}

		ImGui::BeginDisabled(true);

		float unit_time = state.is_motion_time_format_seconds_selected ? 30.0f : 1.0f;

		// raw seconds (speed=1.0) back to keys, so both time formats match the file editor
		float num_keys = data.GetAnimLengthSeconds(anim_index) * 30.0f;
		float current_speed = data.GetSpeed(anim_index);

		float length_with_current_speed = (num_keys / unit_time) / current_speed;
		float length_with_rt = (num_keys / unit_time) / 1.0f;

		const char* pPrintOutTemplate = "Length: %.4f | %.4f";

		if (state.is_motion_time_format_seconds_selected == false)
		{
			pPrintOutTemplate = "Length: %.0f | %.0f";
		}

		ImGui::Text(pPrintOutTemplate, length_with_current_speed, length_with_rt);
		ImGui::SetItemTooltip("Length: [value_uses_current_speed] | [value_uses_real_time_speed] \nReal time means when speed=1.0 so animation plays as it would take normal time speed in game");

		ImGui::EndDisabled();

		ImGui::SeparatorText("Motion time format");

		if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_MotionTimeFormat", 2))
		{
			ImGui::TableNextRow();
			ImGui::TableSetColumnIndex(0);

			if (ImGui::RadioButton("Keys##ToolsInGameImGui_OMFEditor_KeysRB", state.is_motion_time_format_keys_selected))
			{
				state.is_motion_time_format_radiobutton_changed = true;
				state.is_motion_time_format_seconds_selected = false;
				state.is_motion_time_format_keys_selected = !state.is_motion_time_format_keys_selected;
			}

			ImGui::TableSetColumnIndex(1);

			if (ImGui::RadioButton("Seconds##ToolsInGameImGui_OMFEditor_SecondsRB", state.is_motion_time_format_seconds_selected))
			{
				state.is_motion_time_format_radiobutton_changed = true;
				state.is_motion_time_format_keys_selected = false;
				state.is_motion_time_format_seconds_selected = !state.is_motion_time_format_seconds_selected;
			}

			if (state.is_motion_time_format_radiobutton_changed)
			{
				R_ASSERT2(!(state.is_motion_time_format_keys_selected && state.is_motion_time_format_seconds_selected), "You can't select both keys and seconds format at the same time!");

				if (state.is_motion_time_format_seconds_selected == false && state.is_motion_time_format_keys_selected == false)
				{
					state.is_motion_time_format_seconds_selected = true;
				}

				state.is_motion_time_format_radiobutton_changed = false;
			}

			ImGui::EndTable();
		}


		ImGui::TableSetColumnIndex(1);

		int flags = data.GetFlags(anim_index);
		bool flags_changed = false;
		bool check_box_changed = false;

		bool stop_at_end = (flags & (1 << 1)) == (1 << 1);
		check_box_changed = ImGui::Checkbox("Stop at end", &stop_at_end);

		if (check_box_changed)
		{
			if (stop_at_end)
			{
				flags |= (1 << 1);
			}
			else
			{
				flags &= ~(1 << 1);
			}

			flags_changed = true;
		}

		bool no_mix_selected = (flags & (1 << 2)) == (1 << 2);
		check_box_changed = ImGui::Checkbox("No mix", &no_mix_selected);

		if (check_box_changed)
		{
			if (no_mix_selected)
			{
				flags |= (1 << 2);
			}
			else
			{
				flags &= ~(1 << 2);
			}

			flags_changed = true;
		}

		bool sync_part = (flags & (1 << 3)) == (1 << 3);
		check_box_changed = ImGui::Checkbox("Sync part", &sync_part);

		if (check_box_changed)
		{
			if (sync_part)
			{
				flags |= (1 << 3);
			}
			else
			{
				flags &= ~(1 << 3);
			}

			flags_changed = true;
		}

		bool use_foot_steps = (flags & (1 << 4)) == (1 << 4);
		check_box_changed = ImGui::Checkbox("Use foot steps", &use_foot_steps);

		if (check_box_changed)
		{
			if (use_foot_steps)
			{
				flags |= (1 << 4);
			}
			else
			{
				flags &= ~(1 << 4);
			}

			flags_changed = true;
		}

		bool move_xform = (flags & (1 << 5)) == (1 << 5);
		check_box_changed = ImGui::Checkbox("Move XForm", &move_xform);

		if (check_box_changed)
		{
			if (move_xform)
			{
				flags |= (1 << 5);
			}
			else
			{
				flags &= ~(1 << 5);
			}

			flags_changed = true;
		}

		bool idle = (flags & (1 << 6)) == (1 << 6);
		check_box_changed = ImGui::Checkbox("Idle", &idle);

		if (check_box_changed)
		{
			if (idle)
			{
				flags |= (1 << 6);
			}
			else
			{
				flags &= ~(1 << 6);
			}

			flags_changed = true;
		}

		bool use_weapon_bone = (flags & (1 << 7)) == (1 << 7);
		check_box_changed = ImGui::Checkbox("Use weapon bone", &use_weapon_bone);

		if (check_box_changed)
		{
			if (use_weapon_bone)
			{
				flags |= (1 << 7);
			}
			else
			{
				flags &= ~(1 << 7);
			}

			flags_changed = true;
		}

		if (flags_changed)
		{
			data.SetFlags(anim_index, flags);
		}

		check_box_changed = ImGui::Checkbox("Has motion marks", &state.is_motion_marks_enabled);

		if (check_box_changed)
		{
			if (state.is_motion_marks_enabled == false)
			{
				data.ClearMarks(anim_index);

				state.list_box_motion_marks_names.clear();
				state.list_box_motion_marks_params_names.clear();

				state.is_show_popup_marks_cleared = true;
			}
		}

		ImGui::EndTable();
	}
}

void RenderOMFEditorUI_AnimParamsHeader(SOMFEditorUIState& state, IOMFAnimDataProvider& data)
{
	const int count = data.GetAnimParamsCount();

	if (count <= 0)
	{
		ImGui::Text("No animation params!");
		return;
	}

	if (state.current_selected_animation_param < 0 || state.current_selected_animation_param >= count)
	{
		state.current_selected_animation_param = 0;
	}

	if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Header", 2))
	{
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);

		if (ImGui::Combo("Animation params##ToolsInGameImGui_OMFEditor_Data_Header_Combo", &state.current_selected_animation_param, state.combo_animation_params_data.data(), count))
		{
			OMFEditorUI_OnAnimParamSelected(state, data);
		}

		ImGui::TableSetColumnIndex(1);

		ImGui::Text("Selected: [%s]", data.GetAnimParamName(state.current_selected_animation_param));

		if (data.CanRenameAnimations())
		{
			ImGui::SameLine();

			if (ImGui::Button("Rename##ToolsInGameImGui_OMFEditor"))
			{
				state.is_show_popup_rename_animation_param = true;
				state.rename_temp = data.GetAnimParamName(state.current_selected_animation_param);
			}
		}

		ImGui::EndTable();
	}
	ImGui::Separator();
}

void RenderOMFEditor_Draw_TableMain()
{
	if (g_pOMFEditor->is_file_loaded == false)
	{
		return;
	}

	R_ASSERT2(g_pOMFEditor->omf, "must be initialized");

	if (g_pOMFEditor->provider.IsValid() == false)
	{
		return;
	}

	ImGui::TextWrapped("Loaded file: [%s]", g_pOMFEditor->path.c_str());
	ImGui::Separator();

	constexpr const char* _kColumnOfMainTableNames[] = {
		"Editing",
#if IXRAY_OMF_EDITOR_ENABLE_VIEWER == 1
		"Viewer"
#endif
	};
	constexpr u8 _kColumnOfMainTableSize = sizeof(_kColumnOfMainTableNames) / sizeof(_kColumnOfMainTableNames[0]);

	SOMFEditorUIState& state = *g_pOMFEditor;
	IOMFAnimDataProvider& data = g_pOMFEditor->provider;

	RenderOMFEditorUI_SharedModals(state, data);
	RenderOMFEditor_Draw_EditorModals();

	if (ImGui::BeginTable("##TII_OE_Main", _kColumnOfMainTableSize, ImGuiTableFlags_SizingStretchProp))
	{
		for (u8 i = 0; i < static_cast<u8>(_kColumnOfMainTableSize); ++i)
		{
			ImGui::TableSetupColumn(_kColumnOfMainTableNames[i]);
		}

		ImGui::TableHeadersRow();

		ImGui::TableNextRow();

		for (u8 column = 0; column < _kColumnOfMainTableSize; ++column)
		{
			ImGui::TableSetColumnIndex(static_cast<int>(column));

			switch (column)
			{
				case 0:
				{
					RenderOMFEditorUI_AnimParamsHeader(state, data);

					if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body", 2))
					{
						ImGui::TableNextRow();
						ImGui::TableSetColumnIndex(0);

						RenderOMFEditor_Draw_TableMain_BonesRenaming_Section();

						if (ImGui::CollapsingHeader("Bones##ToolsInGameImGui_OMFEditor_Data_Body"))
						{
							RenderOMFEditorUI_BonePartsToolbar(state, data);

							ImGui::SameLine();

							if (ImGui::Button("save as file##ToolsInGameImGui_OMFEditor_ShowBoneParts"))
							{
								R_ASSERT(false && "todo: impl");
							}

							RenderOMFEditorUI_BonePartsTabs(state, data);
						}

						ImGui::TableSetColumnIndex(1);

						RenderOMFEditorUI_Params(state, data);
						RenderOMFEditorUI_MotionMarks(state, data);

						ImGui::EndTable();
					}

					break;
				}
#if IXRAY_OMF_EDITOR_ENABLE_VIEWER == 1
				case 1:
				{
					if (ImGui::CollapsingHeader("Viewer"))
					{
						ImGui::Text("______________________________________________________________________");
					}

					break;
				}
#endif
			}
		}


		ImGui::EndTable();
	}
}


#if IXRAY_OMF_EDITOR_TAB_GAME == 1
inline const std::string_view& convert_EHudStates_to_string(u8 state) noexcept
{
	if (state <= CHUDState::EHudStates::eLastBaseState)
	{
		return magic_enum::enum_name(static_cast<CHUDState::EHudStates>(state));
	}
	else
	{
		return magic_enum::enum_name(static_cast<CWeapon::EWeaponStates>(state));
	}
}
#endif

void RenderOMFEditor_Draw_Game_Editing(
	CActor* pPlayer
)
{
#if IXRAY_OMF_EDITOR_TAB_GAME == 1
	if (pPlayer == nullptr || g_player_hud == nullptr)
	{
		return;
	}

	PIItem pItem = pPlayer->inventory().ActiveItem();
	CHudItem* pHI = (pItem && pItem->cast_hud_item()) ? pItem->cast_hud_item() : nullptr;
	attachable_hud_item* pAHI = pHI ? pHI->HudItemData() : nullptr;

	// resolve the model exactly like the game does: combined models play
	// hand motions on their own model, otherwise on the global hands model
	IKinematicsAnimated* pModel = nullptr;

	if (pAHI)
	{
		pModel = pAHI->m_model_combined
			? (pAHI->m_model ? pAHI->m_model->dcast_PKinematicsAnimated() : nullptr)
			: g_player_hud->GetModel();
	}

	if (g_pOMFGame && (pAHI == nullptr || pModel == nullptr))
	{
		// force rebind on the next valid frame (pointers may be stale
		// after a level change or hud reload)
		g_pOMFGame->bound_item = nullptr;
		g_pOMFGame->bound_model = nullptr;
	}

	if (pAHI == nullptr)
	{
		ImGui::Text("Withdraw weapon/item! Can't edit data of hud item!");
		return;
	}

	if (pModel == nullptr)
	{
		ImGui::Text("No animated model!");
		return;
	}

	if (pAHI->m_hand_motions.m_anims.empty())
	{
		ImGui::Text("No anims!");
		return;
	}

	if (g_pOMFGame == nullptr)
	{
		g_pOMFGame = new CImGuiOMFGameState();
	}

	SOMFEditorUIState& state = *g_pOMFGame;
	CGameAnimProvider& provider = g_pOMFGame->provider;

	if (g_pOMFGame->bound_item != pAHI || g_pOMFGame->bound_model != pModel)
	{
		provider.Bind(pAHI, g_player_hud->GetModel());
		state.Reset();
		OMFEditorUI_RebuildComboCache(state, provider);
		OMFEditorUI_RebuildMotionMarkCaches(state, provider);

		g_pOMFGame->bound_item = pAHI;
		g_pOMFGame->bound_model = pModel;
	}

	ImGui::Text(
		"Source: [%s] | slots: %d | motions: %d",
		pAHI->m_model_combined ? "item (combined)" : "hands",
		static_cast<int>(pModel->LL_MotionsSlotCount()),
		provider.GetAnimParamsCount()
	);

	ImGui::SameLine();

	if (ImGui::Button("Refresh##ToolsOMFEditor_GameRebind"))
	{
		provider.Bind(pAHI, g_player_hud->GetModel());
		state.Reset();
		OMFEditorUI_RebuildComboCache(state, provider);
		OMFEditorUI_RebuildMotionMarkCaches(state, provider);
	}

	ImGui::SetItemTooltip("Re-read all motions from the model (use after OMF files were reloaded/appended at runtime)");

	ImGui::TextWrapped("WARNING: you are editing LIVE engine data shared by all models using the same OMF files. Changes apply to the next played animation (motion marks apply immediately) and are NOT saved to disk.");
	ImGui::Separator();

	RenderOMFEditorUI_SharedModals(state, provider);
	RenderOMFEditorUI_AnimParamsHeader(state, provider);

	if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body", 2))
	{
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);

		if (ImGui::CollapsingHeader("Bones##ToolsInGameImGui_OMFEditor_Data_Body"))
		{
			ImGui::TextWrapped("Bones are read-only in game: runtime partitions store bone ids, names belong to the skeleton model.");

			RenderOMFEditorUI_BonePartsToolbar(state, provider);
			RenderOMFEditorUI_BonePartsTabs(state, provider);
		}

		ImGui::TableSetColumnIndex(1);

		RenderOMFEditorUI_Params(state, provider);
		RenderOMFEditorUI_MotionMarks(state, provider);

		ImGui::EndTable();
	}
#endif
}

void RenderOMFEditor_Draw_Game_Info(
	CActor* pPlayer
)
{
#if IXRAY_OMF_EDITOR_TAB_GAME == 1
	if (pPlayer && g_player_hud)
	{
		PIItem pItem = pPlayer->inventory().ActiveItem();

		if (pItem && pItem->cast_hud_item())
		{
			ImGui::Text("active item:\n\t%s (%s)", pItem->m_section_id.c_str(), pItem->cast_hud_item()->HudSection().c_str());
			ImGui::Text("ogf:\n\t[%s]", pItem->m_3d_static_visual_name);

			CHudItem* pHI = pItem->cast_hud_item();

			if (pHI)
			{
				if (pHI->HudItemData())
				{
					attachable_hud_item* pAHI = pHI->HudItemData();

					if (pAHI->m_hand_motions.m_anims.empty() == false)
					{
						ImGui::Text("Current anim:\n\t[%s]\n\tt=[%d]/[%d]\n\tstartedMotionState=[%s (%d)]", pHI->m_current_motion.c_str(), pHI->m_dwMotionCurrTm, pHI->m_dwMotionEndTm, convert_EHudStates_to_string((pHI->m_startedMotionState)).data(), pHI->m_startedMotionState);

						// playback progress in the time format selected in the Editing column
						bool is_seconds_selected = true;

						if (g_pOMFGame)
						{
							is_seconds_selected = g_pOMFGame->is_motion_time_format_seconds_selected;
						}

						u32 timing_current_ms = (pHI->m_dwMotionCurrTm >= pHI->m_dwMotionStartTm) ? (pHI->m_dwMotionCurrTm - pHI->m_dwMotionStartTm) : 0;
						u32 timing_total_ms = (pHI->m_dwMotionEndTm >= pHI->m_dwMotionStartTm) ? (pHI->m_dwMotionEndTm - pHI->m_dwMotionStartTm) : 0;

						if (is_seconds_selected)
						{
							ImGui::Text("time:\n\t[%.2f]/[%.2f] sec", float(timing_current_ms) / 1000.0f, float(timing_total_ms) / 1000.0f);
						}
						else
						{
							ImGui::Text("time:\n\t[%.0f]/[%.0f] keys", float(timing_current_ms) * 0.03f, float(timing_total_ms) * 0.03f);
						}

						if (pAHI->m_hand_motions.m_banned_bone_parts.empty() == false)
						{
							if (ImGui::CollapsingHeader("banned bone parts:"))
							{
								for (const shared_str& str : pAHI->m_hand_motions.m_banned_bone_parts)
								{
									ImGui::Text("\t[%s]", str.c_str());
								}
							}
						}

						char ch_name[32];
						std::sprintf(ch_name, "Anims=%zu", pAHI->m_hand_motions.m_anims.size());
						if (ImGui::CollapsingHeader(ch_name))
						{
							u16 i = 0;
							for (const player_hud_motion& phm : pAHI->m_hand_motions.m_anims)
							{
								ImGui::PushID(i);
								if (ImGui::CollapsingHeader(phm.m_alias_name.c_str()))
								{
									ImGui::Text("additional name: [%s]", phm.m_additional_name.c_str());
									ImGui::Text("base name: [%s]", phm.m_base_name.c_str());
									ImGui::Text("speed: %.2f", phm.m_anim_speed);

									if (phm.m_bone_parts.empty() == false)
									{
										ImGui::Text("bone parts:");

										for (const shared_str& bone_part_name : phm.m_bone_parts)
										{
											ImGui::Text("\t[%s]", bone_part_name.c_str());
										}
									}
								}
								ImGui::PopID();		
								++i;
							}
						}
					}
					else
					{
						ImGui::Text("No anims!");
					}
				}
				else
				{
					ImGui::Text("No HudItemData!");
				}
			}
			else
			{
				ImGui::Text("Withdraw weapon/item! Can't preview data of hud item!");
			}
		}
	}
#endif
}

void RenderOMFEditor_Draw_Game(
	CActor* pPlayer
)
{
#if IXRAY_OMF_EDITOR_TAB_GAME == 1
	constexpr const char* _kTableColumnNames[] = {
		"Info",
		"Editing"
	};
	constexpr u8 _kTableColumnNamesCount = sizeof(_kTableColumnNames) / sizeof(_kTableColumnNames[0]);

	if (ImGui::BeginTable("##ToolsInGame_OMFEditor_GameTable", 2))
	{
		for (u8 i = 0; i < _kTableColumnNamesCount; ++i)
		{
			ImGui::TableSetupColumn(_kTableColumnNames[i]);
		}

		ImGui::TableHeadersRow();

		ImGui::TableNextRow();

		ImGui::TableSetColumnIndex(0);

		RenderOMFEditor_Draw_Game_Info(pPlayer);

		ImGui::TableSetColumnIndex(1);

		RenderOMFEditor_Draw_Game_Editing(pPlayer);

		ImGui::EndTable();
	}
#endif
}

// ==============================================================
// Help tab: user manual
// ==============================================================

#if IXRAY_OMF_EDITOR_TAB_HELP == 1
void RenderOMFEditor_Draw_HelpTab()
{
	ImGui::SeparatorText("About this tool");

	ImGui::TextWrapped("This editor views and edits .omf animation files, and the live animation data of the weapon you are holding. An .omf file stores skeletal animations for a model: the keyframes, the bone groups they play on, and a parameter block for every motion. Editor tab: load and edit a file, then save it. Game tab: edit the data of the weapon in your hands while the game is running.");

	ImGui::SeparatorText("How the engine uses .omf files");

	ImGuiEditorUI_HelpBullet("Every model (OGF) lists its .omf files. At load, the engine reads the bone partition, one parameter block per motion, and the keyframes for every bone.");
	ImGuiEditorUI_HelpBullet("All loaded .omf files live in one global cache, shared by file path. Every model using the same file shares one copy in memory.");
	ImGuiEditorUI_HelpBullet("Motions are found by name. A hidden flag (FX) decides if a motion is a cycle or a one-shot effect.");
	ImGuiEditorUI_HelpBullet("When a motion starts, the game creates a blend: a copy of the motion's speed, power, accrue and falloff. Editing these values affects the NEXT started motion.");
	ImGuiEditorUI_HelpBullet("Motion marks are different: the game reads them every frame, so mark edits apply immediately.");

	ImGui::SeparatorText("Animation params combo");

	ImGui::TextWrapped("Each entry is one motion: its name, its parameters and its flags. The entry points to the motion's keyframes by its motion id. The 'Try repair' tool re-links these ids when a file is inconsistent.");

	ImGui::SeparatorText("Speed / Power / Accrue / Falloff");

	ImGuiEditorUI_HelpBullet("Speed: playback rate. 1.0 = normal, 2.0 = twice as fast, 0.5 = half speed.");
	ImGuiEditorUI_HelpBullet("Power: the strength (weight) of the motion when it mixes with others. 1.0 = full strength, lower values show the motion only partly.");
	ImGuiEditorUI_HelpBullet("Accrue: how fast the motion fades in when it starts. Higher = faster.");
	ImGuiEditorUI_HelpBullet("Falloff: how fast the motion fades out when it is replaced. Higher = faster. The engine keeps falloff below accrue for cycle motions.");
	ImGuiEditorUI_HelpBullet("Note: the engine stores these four values as 16-bit numbers. The game tab snaps to that grid (about 0.0015 steps), the file editor keeps full float precision.");

	ImGui::SeparatorText("Flags");

	ImGuiEditorUI_HelpSection("Stop at end", "The motion plays once and freezes at the last frame instead of looping. The game also uses this flag to know the animation length and to fire the end-of-animation event.");
	ImGuiEditorUI_HelpSection("No mix", "The motion starts without blending into the previous one (a hard switch). The runtime here does not read it; it is used by tools and AI code when starting motions.");
	ImGuiEditorUI_HelpSection("Sync part", "When switching between two motions that both have this flag, the new one continues from the same time as the old one. This keeps body parts in sync, for example in walk cycles.");
	ImGuiEditorUI_HelpSection("Use foot steps", "The motion is expected to produce footsteps (through motion marks). The legs controller warns in debug when a flagged motion has no marks.");
	ImGuiEditorUI_HelpSection("Move XForm", "The root bone translation moves the object itself: the character really walks forward instead of walking in place.");
	ImGuiEditorUI_HelpSection("Idle", "Marks the motion as an idle animation. The IK system treats idle motions specially.");
	ImGuiEditorUI_HelpSection("Use weapon bone", "Part of the format for tools. The current runtime does not read it.");

	ImGui::SeparatorText("Motion marks");

	ImGuiEditorUI_HelpBullet("Only for OGF version 4 files. A mark is a name plus time intervals in seconds (start, end).");
	ImGuiEditorUI_HelpBullet("While a motion plays, the game checks every frame if playback crosses an interval and fires an event. Used for sounds, shell ejection, footsteps and so on.");
	ImGuiEditorUI_HelpBullet("The 'Has motion marks' checkbox clears all marks of the selected motion when you turn it off.");

	ImGui::SeparatorText("Bone parts");

	ImGuiEditorUI_HelpBullet("A partition is a set of named bone groups (for example left_hand, right_hand). A motion can play on the whole skeleton or only on one part.");
	ImGuiEditorUI_HelpBullet("Bone names are resolved to bone ids when the model loads. That is why the game tab shows them read-only: at runtime there are no names left to rename, only ids.");

	ImGui::SeparatorText("Game tab");

	ImGuiEditorUI_HelpBullet("You edit the live engine data of the weapon in your hands. The combo lists only the motions that weapon actually uses: its anm_* config entries with their random variants, plus anm_bp_* bone-part motions.");
	ImGuiEditorUI_HelpBullet("Changes are runtime-only, they are NOT saved to disk, and they affect every model that uses the same .omf file (the engine shares one copy).");
	ImGuiEditorUI_HelpBullet("Renaming is disabled here: motion names are lookup keys inside the engine.");
	ImGuiEditorUI_HelpBullet("Switching weapons re-reads the data automatically. Refresh re-reads it manually, for example after OMF files were appended at runtime.");

	ImGui::SeparatorText("File tools (Editor tab)");

	ImGuiEditorUI_HelpBullet("Save / Merge with / Add anims from: write the file or extend it with motions from other files.");
	ImGuiEditorUI_HelpBullet("Try repair: re-links motion ids and names when a file is inconsistent.");
	ImGuiEditorUI_HelpBullet("Swap anim marks: copies motion marks from another .omf file into motions with matching names.");

	ImGui::SeparatorText("Shortcuts");

	ImGuiEditorUI_HelpBullet("Escape: closes popups and selections first, then the window.");
}
#endif

void RenderToolsOMFEditorWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)])
	{
		return;
	}

	if (g_pOMFEditor == nullptr)
	{
		g_pOMFEditor = new CImGuiOMFEditor();
	}

	if (g_pOMFEditor)
	{
		if (ImGui::Begin("Editor - [OMF]##ToolsInGameImGui", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)], ImGuiWindowFlags_MenuBar))
		{
			if (ImGui::BeginTabBar("##OMFToolsIII"))
			{
#if IXRAY_OMF_EDITOR_TAB_EDITOR == 1
				if (ImGui::BeginTabItem("Editor"))
				{
					RenderOMFEditor_Draw_TableHeader();

					RenderOMFEditor_Draw_TableMain();

					ImGui::EndTabItem();
				}
#endif

#if IXRAY_OMF_EDITOR_TAB_GAME == 1

				bool is_in_game = false;

				CActor* pPlayer = nullptr;
				if (
					g_pGameLevel &&
					ai().get_alife() &&
					g_actor
				)
				{
					pPlayer = Actor();

					if (pPlayer && pPlayer->GetfHealth() > 0.001f)
					{
						is_in_game = true;
					}
				}

				ImGui::BeginDisabled(!is_in_game);
				if (ImGui::BeginTabItem("Game"))
				{
					if (is_in_game)
					{
						RenderOMFEditor_Draw_Game(pPlayer);
					}

					ImGui::EndTabItem();
				}

				ImGui::EndDisabled();
#endif

#if IXRAY_OMF_EDITOR_TAB_HELP == 1
				if (ImGui::BeginTabItem("Help"))
				{
					RenderOMFEditor_Draw_HelpTab();

					ImGui::EndTabItem();
				}
#endif

				ImGui::EndTabBar();
			}
		}
		ImGui::End();
	}
}