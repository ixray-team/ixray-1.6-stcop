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

enum class _eMessageBoxStatus
{
	kSuccess,
	kWarning,
	kError,
	kYesOrNo
};

int ShowMessageBox(_eMessageBoxStatus status, std::string_view title, std::string_view message)
{
	const SDL_MessageBoxButtonData buttons[] =
	{
		{ 0, 0, "Ok" }
	};

	const SDL_MessageBoxButtonData buttons_yesorno[] = 
	{
		{SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT, 1, "Yes"},
		{0, 0, "No"},
	};

	u32 type = SDL_MESSAGEBOX_INFORMATION;

	switch (status)
	{
	case _eMessageBoxStatus::kWarning:
	{
		type = SDL_MESSAGEBOX_WARNING;
		break;
	}
	case _eMessageBoxStatus::kError:
	{
		type = SDL_MESSAGEBOX_ERROR;
		break;
	}
	}

	const SDL_MessageBoxButtonData* pButtons = buttons;
	int size_buttons = std::size(buttons);

	if (status == _eMessageBoxStatus::kYesOrNo)
	{
		pButtons = buttons_yesorno;
		size_buttons = std::size(buttons_yesorno);
	}

	const SDL_MessageBoxData messageboxdata =
	{
		type | SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT,		/* .flags */
		nullptr,					/* .window */
		title.data(),				/* .title */
		message.data(),			/* .message */
		size_buttons,			/* .numbuttons */
		pButtons,					/* .buttons */
		nullptr						/* .colorScheme */
	};

	int button_id = -1;

	int ret = SDL_ShowMessageBox(&messageboxdata, &button_id);

	if (ret < 0)
		button_id = -1;

	return button_id;
}


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

struct CImGuiOMFEditor
{
	~CImGuiOMFEditor()
	{
		if (omf)
		{
			xr_delete(omf);
		}
	}


	bool is_show_popup_marks_cleared{};
	bool is_show_popup_rename_animation_param{};
	bool is_show_popup_renamehascollision{};
	bool is_show_popup_boneparts_was_copied_to_clipboard_suc{};
	bool is_show_popup_boneparts_was_copied_to_clipboard_fail{};
	bool is_show_popup_boneparts_rename_has_collision{};

	bool is_show_popup_add_motion_mark{};
	bool is_show_popup_duplicate_found_motion_mark{};

	bool is_show_popup_try_repair_applied{};

	bool is_file_loaded{};
	bool animation_param_was_changed{};
	bool is_motion_time_format_seconds_selected{};
	bool is_motion_time_format_keys_selected{};
	bool is_motion_time_format_radiobutton_changed{};
	bool is_motion_marks_enabled{};
	int current_selected_animation_param{};
	int current_selected_bone_rename{};
	int current_selected_mark{};
	int current_selected_mark_param{};

	OMFData* omf{};
	OMFData* temp_omf{};
	OMFData::omf_name_t rename_temp;
	OMFData::omf_name_t rename_temp_bone;
	OMFData::omf_name_t temp_motion_mark_name;
	xr_vector<const char*> combo_animation_params_data;
	xr_set<size_t> combo_animation_params_name_hashes;
	xr_vector<const char*> combo_bones_data;
	xr_set<size_t> combo_bones_name_hashes;

	xr_vector<const char*> list_box_motion_marks_names;
	xr_vector<xr_stack_string16> list_box_motion_marks_params_names;

	xr_stack_string<sizeof(string_path)*2> path;
};

CImGuiOMFEditor* g_pOMFEditor = nullptr;


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

template<typename T>
void OMFEditor_ReadString(T& str, std::ifstream& file)
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

template<typename T>
void OMFEditor_ReadStringMotionMark(T& str, std::ifstream& file)
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
		return false;

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
	std::ofstream& file)
{
	R_ASSERT(file.good());

	if (file.good() == false)
		return false;

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
	int16_t mark_id,
	std::ifstream& file
)
{
	bool status = true;

	OMFEditor_ReadStringMotionMark(mark.name, file);
	file.read(reinterpret_cast<char*>(&mark.count), sizeof(mark.count));

	if (mark.name.empty() == false)
	{
		if (g_pOMFEditor && mark_id == 0)
		{
			g_pOMFEditor->list_box_motion_marks_names.push_back(mark.name.c_str());
		}
	}

	for (int32_t i = 0; i < mark.count; ++i)
	{
		mark.params.push_back({});
		OMFData::AnimParamsData::AnimParams::MotionMark::Params& mark_param = mark.params.back();

		file.read(reinterpret_cast<char*>(&mark_param.t0), sizeof(mark_param.t0));
		file.read(reinterpret_cast<char*>(&mark_param.t1), sizeof(mark_param.t1));

		if (g_pOMFEditor && mark_id == 0)
		{
			xr_stack_string16 temp;
			std::sprintf(temp.data(), "%hd_mark%d", mark_id, i);
			g_pOMFEditor->list_box_motion_marks_params_names.push_back(temp);
		}
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
		if (g_pOMFEditor)
		{
			g_pOMFEditor->current_selected_animation_param = 0;
		}

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

					bool status_mark = OMFEditor_LoadOMF_AnimParamsData_MotionMark(mark, mark_id, file);

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

	if (g_pOMFEditor)
	{
		if (g_pOMFEditor->current_selected_animation_param != 0)
		{
			// invalidate it means serialized data didn't contain any animation param
			g_pOMFEditor->current_selected_animation_param = -1;
		}

		g_pOMFEditor->current_selected_mark = -1;
		g_pOMFEditor->current_selected_mark_param = -1;
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
		return false;

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
			continue;

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

void OMFEditor_Init_ComboAnimationParams(CImGuiOMFEditor* p_state, OMFData& data)
{
	R_ASSERT2(p_state->combo_animation_params_data.empty(), "did you clear data before init?");
	R_ASSERT2(p_state->combo_animation_params_name_hashes.empty(), "did you clear data before init?");

	if (data.data_animparams.count > 0)
	{
		for (int16_t i = 0; i < data.data_animparams.count; ++i)
		{
			std::string_view view = data.data_animparams.params[i].name.c_str();
			p_state->combo_animation_params_data.push_back(data.data_animparams.params[i].name.c_str());
			p_state->combo_animation_params_name_hashes.insert(std::hash<std::string_view>()(data.data_animparams.params[i].name.c_str()));
		}
	}
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

void OMFEditor_Init(CImGuiOMFEditor* p_state, OMFData& data)
{
	if (!p_state)
		return;

	p_state->current_selected_animation_param = 0;
	p_state->current_selected_mark = -1;
	p_state->current_selected_mark_param = -1;

#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 0
	p_state->current_selected_bone_rename = 0;
#else
	p_state->current_selected_bone_rename = -1;
#endif

	p_state->animation_param_was_changed = false;

	p_state->is_motion_time_format_seconds_selected = true;
	p_state->is_motion_time_format_radiobutton_changed = true;
	p_state->is_motion_marks_enabled = false;
	p_state->combo_animation_params_data.clear();
	p_state->combo_animation_params_name_hashes.clear();
	p_state->combo_bones_data.clear();
	p_state->combo_bones_name_hashes.clear();

	p_state->is_show_popup_marks_cleared = false;
	p_state->is_show_popup_rename_animation_param = false;
	p_state->is_show_popup_renamehascollision = false;
	p_state->is_show_popup_boneparts_was_copied_to_clipboard_suc = false;
	p_state->is_show_popup_boneparts_was_copied_to_clipboard_fail = false;
	p_state->is_show_popup_boneparts_rename_has_collision = false;
	p_state->is_show_popup_add_motion_mark = false;
	p_state->is_show_popup_duplicate_found_motion_mark = false;
	p_state->is_show_popup_try_repair_applied = false;

	p_state->list_box_motion_marks_names.reserve(128);
	p_state->list_box_motion_marks_params_names.reserve(128);

	OMFEditor_Init_ComboAnimationParams(p_state, data);
	OMFEditor_Init_ComboBones(p_state, data);

	if (data.data_bone.count > 0)
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
			return status;

		status = OMFEditor_LoadOMF_BoneData(data.data_bone, file);

		if (!status)
			return status;

		status = OMFEditor_LoadOMF_AnimParamsData(data.data_bone.ogf_version, data.data_anim.animations_count, data.data_animparams, file);

		if (!status)
			return status;
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

				OMFEditor_Init(p_state, *p_state->omf);
			}
		}
	}
}

bool OMFEditor_CopyBonePartsToClipboard(CImGuiOMFEditor* p_state)
{
	bool result{};
	if (p_state)
	{
		xr_stack_string<1024 * 64> output;
		for (const auto& bone_part : p_state->omf->data_bone.parts)
		{
			output += "[";
			output += bone_part.name;
			output += "]";
			output += "\n";

			for (const auto& bone : bone_part.bones)
			{
				output += bone.name;
				output += "\n";
			}

			output += "\n";
			output += "\n";
		}

		if (xr_EFS)
		{
			result = xr_EFS->CopyTextToClipboard(output);
		}
	}

	return result;
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
				break;
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
			file.close();
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
		pState->omf->data_animparams.params.empty()==false &&
		pState->omf->data_anim.anims.empty()==false
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

		R_ASSERT(pState->omf==nullptr);

		if (pState->omf == nullptr)
		{
			xr_stack_tstring<sizeof(string_path)> local_path;
			bool status = xr_EFS->GetOpenName(local_path, XR_TEXT("OMF file\0*.omf\0"));

			if (status)
			{
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
				auto& selected_param = pState->omf->data_animparams.params[pState->current_selected_animation_param];

				xr_stack_string16 temp_mark_param_name;
				pState->list_box_motion_marks_names.clear();
				pState->list_box_motion_marks_params_names.clear();

				int i = 0;
				for (auto& mark : selected_param.marks)
				{
					pState->list_box_motion_marks_names.push_back(mark.name.c_str());

					if (i == 0)
					{
						int mark_param_id = 0;
						for (auto& mark_param : mark.params)
						{
							std::sprintf(temp_mark_param_name.data(), "%d_mark%d", i, mark_param_id);
							pState->list_box_motion_marks_params_names.push_back(temp_mark_param_name);
							++mark_param_id;
						}
					}
					++i;
				}
			}
		}
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
		if (g_pOMFEditor)
		{
			bool can_hide_window = false;

#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 1
			can_hide_window = g_pOMFEditor->current_selected_bone_rename == -1;

			if (can_hide_window == false)
			{
				g_pOMFEditor->current_selected_bone_rename = -1;
			}
#else
			can_hide_window = true;
#endif

			if (ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId))
			{
				g_pOMFEditor->is_show_popup_boneparts_rename_has_collision = false;
				g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_fail = false;
				g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_suc = false;
				g_pOMFEditor->is_show_popup_marks_cleared = false;
				g_pOMFEditor->is_show_popup_renamehascollision = false;
				g_pOMFEditor->is_show_popup_rename_animation_param = false;
				g_pOMFEditor->is_show_popup_add_motion_mark = false;
				g_pOMFEditor->is_show_popup_try_repair_applied = false;

				can_hide_window = false;
			}

			if (g_pOMFEditor->current_selected_mark_param >= 0)
			{
				g_pOMFEditor->current_selected_mark_param = -1;
				can_hide_window = false;
			}

			if (can_hide_window && g_pOMFEditor->current_selected_mark >= 0)
			{
				g_pOMFEditor->current_selected_mark = -1;
				can_hide_window = false;
			}


			if (can_hide_window)
			{
				Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)] = false;
			}
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
			}

			if (ImGui::MenuItem("Add anims from##ToolsInGameImGui_OMFEditor"))
			{
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

void RenderOMFEditor_Draw_TableMain_Bone_Renaming(int bone_id, OMFData::BoneParts::Bone& bone)
{
#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 1
	ImGui::PushID(bone_id);

	if (g_pOMFEditor->current_selected_bone_rename == bone_id)
	{
		if (ImGui::InputText(
			"##ToolsOMFEditor_DirectRenamingOfBone",
			g_pOMFEditor->rename_temp_bone.data(),
			g_pOMFEditor->rename_temp_bone.max_size(),
			ImGuiInputTextFlags_EnterReturnsTrue
		) && g_pOMFEditor->rename_temp_bone.size() > 0)
		{
			bone.name = g_pOMFEditor->rename_temp_bone;
			g_pOMFEditor->current_selected_bone_rename = -1;
		}
	}
	else
	{
		if (ImGui::Selectable(bone.name.c_str())) {
			// Optional: handle selection

			g_pOMFEditor->current_selected_bone_rename = -1;

		}

		// Activate editing on double-click
		if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(0)) {
			g_pOMFEditor->current_selected_bone_rename = bone_id;
			g_pOMFEditor->rename_temp_bone = bone.name;
		}
	}

	ImGui::PopID();
#endif
}

void RenderOMFEditor_Draw_TableMain_Bones_Section()
{
	ImGui::SeparatorText("Bone Parts");

	if (g_pOMFEditor == nullptr)
		return;

	if (g_pOMFEditor->is_file_loaded == false)
		return;

	if (g_pOMFEditor->omf == nullptr)
		return;

	xr_vector<OMFData::BoneParts>& bone_parts = g_pOMFEditor->omf->data_bone.parts;

	if (bone_parts.empty())
	{
		ImGui::Text("No bones!");
	}
	else
	{
		if (ImGui::BeginTabBar("##ToolsOMFEditor_TableMain_BonesPartSection"))
		{
			for (OMFData::BoneParts& bone_part : bone_parts)
			{
				if (ImGui::BeginTabItem(bone_part.name.c_str()))
				{
					ImGui::Text("bone count: %d", bone_part.bones.size());
					ImGui::Separator();

					//if (ImGui::CollapsingHeader("Bones"))
					{
						if (ImGui::BeginChild("##ToolsOMFEditor_BonesScrollableRegion"))
						{
#if IXRAY_OMF_EDITOR_ENABLE_DIRECT_BONE_RENAMING == 0
							for (OMFData::BoneParts::Bone& bone : bone_part.bones)
							{
								ImGui::Text(bone.name.c_str());
							}
#else
							for (int bone_id = 0; bone_id < bone_part.bones.size(); ++bone_id)
							{
								OMFData::BoneParts::Bone& bone = bone_part.bones[bone_id];
								RenderOMFEditor_Draw_TableMain_Bone_Renaming(bone_id, bone);
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

void OMFEditor_AddMotionMark(
	CImGuiOMFEditor* pState,
	const OMFData::omf_name_t& mark_name
)
{
	R_ASSERT(pState);
	R_ASSERT(mark_name.empty() == false);

	if (
		pState &&
		pState->omf &&
		pState->current_selected_animation_param >= 0 &&
		pState->omf->data_animparams.count > 0
		)
	{
		OMFData::AnimParamsData::AnimParams& param = pState->omf->data_animparams.params[pState->current_selected_animation_param];

		param.marks.push_back(OMFData::AnimParamsData::AnimParams::MotionMark());
		param.marks.back().name = mark_name;

		g_pOMFEditor->list_box_motion_marks_names.push_back(param.marks.back().name.c_str());

		param.marks_count = static_cast<int32_t>(param.marks.size());
	}
}

void OMFEditor_DeleteMotionMark(
	CImGuiOMFEditor* pState,
	int index
)
{
	R_ASSERT(pState);
	R_ASSERT(index >= 0);

	if (
		pState &&
		pState->omf &&
		pState->current_selected_animation_param >= 0 &&
		index >= 0 &&
		pState->omf->data_animparams.count > 0
		)
	{
		OMFData::AnimParamsData::AnimParams& param = pState->omf->data_animparams.params[pState->current_selected_animation_param];

		if (param.marks_count > 0)
		{
			auto& mark = param.marks[index];

			if (mark.count > 0)
			{
				mark.params.clear();
				pState->list_box_motion_marks_params_names.clear();
			}

			param.marks.erase(param.marks.cbegin() + index);
			--param.marks_count;

			g_pOMFEditor->list_box_motion_marks_names.erase(g_pOMFEditor->list_box_motion_marks_names.cbegin() + index);

			if (param.marks_count == 0)
			{
				g_pOMFEditor->current_selected_mark = -1;
			}
		}
	}
}

void OMFEditor_AddMotionMarkParam(
	CImGuiOMFEditor* pState,
	int index_selected_animation_param,
	int index_selected_mark
)
{
	R_ASSERT(pState);

	if (
		pState &&
		pState->omf &&
		index_selected_animation_param >= 0 &&
		index_selected_mark >= 0 &&
		pState->omf->data_animparams.count > 0
		)
	{
		OMFData::AnimParamsData::AnimParams& param = pState->omf->data_animparams.params[index_selected_animation_param];

		R_ASSERT(param.marks_count > 0 && "if triggered something is wrong and state handling of UI state is broken or memory corruption from outside code execution");

		if (param.marks_count > 0)
		{
			OMFData::AnimParamsData::AnimParams::MotionMark& mark = param.marks[index_selected_mark];

			mark.params.push_back({});
			++mark.count;
			xr_stack_string16 param_name;

			std::sprintf(
				param_name.data(),
				"%d_mark%zu",
				index_selected_mark,
				mark.params.size() - 1
			);

			pState->list_box_motion_marks_params_names.push_back(param_name);
		}
	}
}

void OMFEditor_DeleteMotionMarkParam(
	CImGuiOMFEditor* pState,
	int index_selected_animation_param,
	int index_selected_mark,
	int index_selected_mark_param
)
{
	R_ASSERT(pState);

	if (
		pState &&
		pState->omf &&
		index_selected_animation_param >= 0 &&
		index_selected_mark >= 0 &&
		index_selected_mark_param >= 0 &&
		pState->omf->data_animparams.count > 0
		)
	{
		OMFData::AnimParamsData::AnimParams& param = pState->omf->data_animparams.params[index_selected_animation_param];

		if (param.marks_count > 0)
		{
			OMFData::AnimParamsData::AnimParams::MotionMark& mark = param.marks[index_selected_mark];

			mark.params.erase(mark.params.cbegin() + index_selected_mark_param);
			--mark.count;
			pState->list_box_motion_marks_params_names.erase(pState->list_box_motion_marks_params_names.cbegin() + index_selected_mark_param);

			if (pState->list_box_motion_marks_params_names.empty() == false)
			{
				xr_stack_string16 temp;
				int i = 0;
				for (xr_stack_string16& param_name : pState->list_box_motion_marks_params_names)
				{
					std::sprintf(temp.data(), "%d_mark%d", index_selected_mark, i);
					param_name = temp;
					++i;
				}
			}
			else
			{
				pState->current_selected_mark_param = -1;
			}
		}
	}
}

bool OMFEditor_CheckDuplicateMotionMark(
	CImGuiOMFEditor* pState,
	const OMFData::omf_name_t& mark_name
)
{
	R_ASSERT(pState);
	R_ASSERT(mark_name.empty() == false);

	if (
		pState &&
		pState->omf &&
		pState->current_selected_animation_param >= 0 &&
		pState->omf->data_animparams.count > 0
		)
	{
		OMFData::omf_name_t lower_left;
		OMFData::omf_name_t lower_right = mark_name;

		xr_strlwr(lower_right);

		const xr_vector<OMFData::AnimParamsData::AnimParams::MotionMark>& marks = g_pOMFEditor->omf->data_animparams.params[g_pOMFEditor->current_selected_animation_param].marks;


		auto it = std::find_if(
			marks.begin(),
			marks.end(),
			[&lower_left, lower_right](
				const OMFData::AnimParamsData::AnimParams::MotionMark& left
				)->bool {
					lower_left = left.name;

					xr_strlwr(lower_left);

					return lower_left == lower_right;
			}
		);

		return (it != marks.end());
	}

	return false;
}

void OMFEditor_ComboAnimationParamWasChanged(
	CImGuiOMFEditor* pState,
	int selected_animation_param_id
)
{
	R_ASSERT(pState);
	R_ASSERT(pState->omf);

	if (pState &&
		pState->omf
		)
	{
		pState->list_box_motion_marks_names.clear();
		pState->list_box_motion_marks_params_names.clear();
		pState->current_selected_mark = -1;
		pState->current_selected_mark_param = -1;
		if (selected_animation_param_id >= 0)
		{
			auto& param = pState->omf->data_animparams.params[selected_animation_param_id];

			xr_stack_string16 temp;
			int mark_id = 0;
			int mark_param_id = 0;
			for (const auto& mark : param.marks)
			{
				pState->list_box_motion_marks_names.push_back(mark.name.c_str());
				
				for (const auto& mark_param : mark.params)
				{
					std::sprintf(temp.data(), "%d_mark%d", mark_id, mark_param_id);
					pState->list_box_motion_marks_params_names.push_back(temp);
					++mark_param_id; 
				}

				mark_param_id = 0;
				++mark_id;
			}
		}
	}
}

void RenderOMFEditor_Draw_TableMain_MotionMarks()
{
	if (
		g_pOMFEditor == nullptr ||
		g_pOMFEditor->omf == nullptr ||
		g_pOMFEditor->omf->data_animparams.count <= 0 ||
		g_pOMFEditor->current_selected_animation_param < 0
		)
		return;


	OMFData::AnimParamsData::AnimParams& param = g_pOMFEditor->omf->data_animparams.params[g_pOMFEditor->current_selected_animation_param];

	bool has_motion_marks_selected = (g_pOMFEditor->omf->data_bone.ogf_version == 4);

	has_motion_marks_selected &= g_pOMFEditor->is_motion_marks_enabled;

	if (!has_motion_marks_selected && g_pOMFEditor->is_motion_marks_enabled)
	{
		ImGui::Text("Motion marks are only for OGF Version == 4 yours is %d", g_pOMFEditor->omf->data_bone.ogf_version);
	}

	ImGui::BeginDisabled(has_motion_marks_selected == false);

	if (ImGui::CollapsingHeader("Motion marks"))
	{
		if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body3", 2))
		{
			ImGui::TableNextRow();

			ImGui::TableSetColumnIndex(0);

			ImGui::SeparatorText("Mark");

			OMFData::AnimParamsData::AnimParams& param_anim = g_pOMFEditor->omf->data_animparams.params[g_pOMFEditor->current_selected_animation_param];

			/*
			for (int i = 0; i < param_anim.marks_count; ++i)
			{
				g_pOMFEditor->list_box_motion_marks_names.push_back(param_anim.marks[i].name.c_str());
			}
			*/

			bool reselected = ImGui::ListBox(
				"##ToolsOMFEditor_MarkGroupLB",
				&g_pOMFEditor->current_selected_mark,
				g_pOMFEditor->list_box_motion_marks_names.data(),
				g_pOMFEditor->list_box_motion_marks_names.size()
			);

			if (reselected)
			{
				g_pOMFEditor->list_box_motion_marks_params_names.clear();

				R_ASSERT(g_pOMFEditor->current_selected_animation_param >= 0);
				R_ASSERT(g_pOMFEditor->current_selected_mark >= 0);

				if (
					g_pOMFEditor->current_selected_animation_param >= 0 &&
					g_pOMFEditor->current_selected_mark >= 0 &&
					g_pOMFEditor->omf
					)
				{
					auto& mark = g_pOMFEditor->omf->data_animparams.params[g_pOMFEditor->current_selected_animation_param].marks[g_pOMFEditor->current_selected_mark];

					if (mark.count > 0)
					{
						xr_stack_string16 temp;
						for (int i = 0; i < mark.count; ++i)
						{
							std::sprintf(temp.data(), "%d_mark%d", g_pOMFEditor->current_selected_mark, i);
							g_pOMFEditor->list_box_motion_marks_params_names.push_back(temp);
						}
					}
				}
			}

			if (
				ImGui::Button("Add##ToolsOMFEditor_MarkAdd") &&
				g_pOMFEditor->is_show_popup_add_motion_mark == false
				)
			{
				g_pOMFEditor->is_show_popup_add_motion_mark = true;
				g_pOMFEditor->temp_motion_mark_name.clear();
				g_pOMFEditor->temp_motion_mark_name = "NewGroup";
			}

			ImGui::SameLine();

			if (ImGui::Button("Delete##ToolsOMFEditor_MarkDelete"))
			{
				OMFEditor_DeleteMotionMark(g_pOMFEditor, g_pOMFEditor->current_selected_mark);
			}

			ImGui::SeparatorText("Mark Param");

			ImGui::BeginDisabled(g_pOMFEditor->current_selected_mark == -1);

			ImGui::ListBox(
				"##ToolsOMFEditor_MarkParamLB",
				&g_pOMFEditor->current_selected_mark_param,
				[](void* user_data, int idx) -> const char* {
					R_ASSERT(user_data);

					xr_vector<xr_stack_string16>* pCasted = static_cast<xr_vector<xr_stack_string16>*>(user_data);

					R_ASSERT(idx <= pCasted->size() - 1);

					return pCasted->operator[](idx).c_str();
				},
				&g_pOMFEditor->list_box_motion_marks_params_names,
				g_pOMFEditor->list_box_motion_marks_params_names.size()
			);

			if (ImGui::Button("Add##ToolsOMFEditor_MarkParamAdd"))
			{
				OMFEditor_AddMotionMarkParam(
					g_pOMFEditor,
					g_pOMFEditor->current_selected_animation_param,
					g_pOMFEditor->current_selected_mark
				);
			}

			ImGui::SameLine();

			if (ImGui::Button("Delete##ToolsOMFEditor_MarkParamDelete"))
			{
				OMFEditor_DeleteMotionMarkParam(
					g_pOMFEditor,
					g_pOMFEditor->current_selected_animation_param,
					g_pOMFEditor->current_selected_mark,
					g_pOMFEditor->current_selected_mark_param
				);
			}

			ImGui::EndDisabled();

			ImGui::TableSetColumnIndex(1);

			bool is_mark_settings_disabled = (has_motion_marks_selected) && (g_pOMFEditor->current_selected_mark_param == -1);

			ImGui::BeginDisabled(is_mark_settings_disabled);


			ImGui::SeparatorText("Mark settings");

			if (
				g_pOMFEditor->current_selected_mark_param == -1 ||
				g_pOMFEditor->current_selected_mark == -1 ||
				g_pOMFEditor->current_selected_animation_param == -1
				)
			{
				float fStart{};
				ImGui::DragFloat("Start##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fStart);


				float fEnd{};
				ImGui::DragFloat("End##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fEnd);
			}
			else
			{
				auto& mark = g_pOMFEditor->omf->data_animparams.params[g_pOMFEditor->current_selected_animation_param].marks[g_pOMFEditor->current_selected_mark];

				if (mark.params.empty() == false)
				{
					auto& mark_param = mark.params[g_pOMFEditor->current_selected_mark_param];
					ImGui::DragFloat("Start##ToolsInGameImGui_OMFEditor_MotionMarksMark", &mark_param.t0);
					ImGui::DragFloat("End##ToolsInGameImGui_OMFEditor_MotionMarksMark", &mark_param.t1);
				}
				else
				{
					ImGui::BeginDisabled(true);

					float fStart{};
					ImGui::DragFloat("Start##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fStart);


					float fEnd{};
					ImGui::DragFloat("End##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fEnd);

					ImGui::EndDisabled();
				}
			}


			ImGui::EndTable();
			ImGui::EndDisabled();
		}
	}

	ImGui::EndDisabled();
}

void RenderOMFEditor_Draw_ModalPopups()
{
	unsigned char modal_opened = 0;

	modal_opened += g_pOMFEditor->is_show_popup_marks_cleared;
	modal_opened += g_pOMFEditor->is_show_popup_rename_animation_param;
	modal_opened += g_pOMFEditor->is_show_popup_boneparts_rename_has_collision;
	modal_opened += g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_suc;
	modal_opened += g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_fail;
	modal_opened += g_pOMFEditor->is_show_popup_renamehascollision;
	modal_opened += g_pOMFEditor->is_show_popup_add_motion_mark;
	modal_opened += g_pOMFEditor->is_show_popup_duplicate_found_motion_mark;
	modal_opened += g_pOMFEditor->is_show_popup_try_repair_applied;

	R_ASSERT(modal_opened <= 1);

	if (modal_opened == 1)
	{
		if (g_pOMFEditor->is_show_popup_marks_cleared)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_AnimationParamMotionMarksCleared);
		}

		if (g_pOMFEditor->is_show_popup_rename_animation_param)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_RenameAnimationParam);
		}

		if (g_pOMFEditor->is_show_popup_boneparts_rename_has_collision)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_WarningRenameHasCollision);
		}

		if (g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_suc)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardSuccessful);
		}

		if (g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_fail)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardFailed);
		}

		if (g_pOMFEditor->is_show_popup_add_motion_mark)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_AddMotionMark);
		}

		if (g_pOMFEditor->is_show_popup_duplicate_found_motion_mark)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_DuplicateFoundMotionMark);
		}

		if (g_pOMFEditor->is_show_popup_try_repair_applied)
		{
			ImGui::OpenPopup(_kOMFEditorModalWindow_TryRepairApplied);
		}
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_AnimationParamMotionMarksCleared, &g_pOMFEditor->is_show_popup_marks_cleared))
	{
		ImGui::Text("Motion marks are cleared!");

		if (ImGui::Button("Ok##ToolsOMFEditor_MotionMarksCleared"))
		{
			g_pOMFEditor->is_show_popup_marks_cleared = false;
		}

		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_RenameAnimationParam, &g_pOMFEditor->is_show_popup_rename_animation_param, ImGuiWindowFlags_AlwaysAutoResize))
	{
		auto& current_param = g_pOMFEditor->omf->data_animparams.params[g_pOMFEditor->current_selected_animation_param];
		ImGui::InputText("##ToolsInGameImGui_OMFEditor_RenameAnimationParamInputText", g_pOMFEditor->rename_temp.data(), g_pOMFEditor->rename_temp.max_size());

		ImGui::SetItemDefaultFocus();
		if (ImGui::Button("Save##ToolsInGameImGui_OMFEditor_RenameAnimationParam"))
		{
			size_t hash_temp = std::hash<std::string_view>()(g_pOMFEditor->rename_temp.c_str());

			if (g_pOMFEditor->combo_animation_params_name_hashes.find(hash_temp) != g_pOMFEditor->combo_animation_params_name_hashes.end() && g_pOMFEditor->rename_temp != current_param.name)
			{
				g_pOMFEditor->is_show_popup_renamehascollision = true;
				g_pOMFEditor->is_show_popup_rename_animation_param = false;
			}
			else
			{
				OMFData::omf_name_t previous = current_param.name;
				size_t previous_temp = std::hash<std::string_view>()(previous.c_str());
				if (g_pOMFEditor->combo_animation_params_name_hashes.find(previous_temp) != g_pOMFEditor->combo_animation_params_name_hashes.end())
				{
					g_pOMFEditor->combo_animation_params_name_hashes.erase(previous_temp);
				}

				current_param.name = g_pOMFEditor->rename_temp;
				g_pOMFEditor->combo_animation_params_name_hashes.insert(std::hash<std::string_view>()(current_param.name.c_str()));
				g_pOMFEditor->is_show_popup_rename_animation_param = false;
			}
		}

		ImGui::SameLine();

		if (ImGui::Button("Cancel##ToolsInGameImGui_OMFEditor_RenameAnimationParam"))
		{
			g_pOMFEditor->is_show_popup_rename_animation_param = false;
		}

		ImGui::EndPopup();
	}

	if (g_pOMFEditor->is_show_popup_renamehascollision)
	{
		ImGui::OpenPopup(_kOMFEditorModalWindow_WarningRenameHasCollision);
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_WarningRenameHasCollision, &g_pOMFEditor->is_show_popup_renamehascollision, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Failed to rename because you have already same name!");
		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardFailed, &g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_fail, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Text wasn't copied to your clipboard! Try again or report to developers!");

		if (ImGui::Button("OK##ToolsInGameImGui_OMFEditor_ClipBoard"))
		{
			g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_fail = false;
		}

		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardSuccessful, &g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_suc, ImGuiWindowFlags_AlwaysAutoResize))
	{
		ImGui::Text("Text was successfully copied to your clipboard!");

		if (ImGui::Button("OK##ToolsInGameImGui_OMFEditor_ClipBoard"))
		{
			g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_suc = false;
		}

		ImGui::EndPopup();
	}

	if (ImGui::BeginPopupModal(
		_kOMFEditorModalWindow_AddMotionMark,
		&g_pOMFEditor->is_show_popup_add_motion_mark,
		ImGuiWindowFlags_AlwaysAutoResize
	))
	{

		if (ImGui::InputText(
			"##ToolsOMFEditor_MotionMarkIT",
			g_pOMFEditor->temp_motion_mark_name.data(),
			g_pOMFEditor->temp_motion_mark_name.max_size()
		))
		{

		}

		if (ImGui::Button("Ok##ToolsOMFEditor_MotionMarkITOK"))
		{
			if (OMFEditor_CheckDuplicateMotionMark(
				g_pOMFEditor,
				g_pOMFEditor->temp_motion_mark_name
			))
			{
				g_pOMFEditor->is_show_popup_duplicate_found_motion_mark = true;
			}
			else
			{
				OMFEditor_AddMotionMark(g_pOMFEditor, g_pOMFEditor->temp_motion_mark_name);
			}

			g_pOMFEditor->is_show_popup_add_motion_mark = false;
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
			g_pOMFEditor->is_show_popup_add_motion_mark = true;
			g_pOMFEditor->is_show_popup_duplicate_found_motion_mark = false;
		}

		ImGui::EndPopup();
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
}

void RenderOMFEditor_Draw_TableMain_Params()
{
	if (
		g_pOMFEditor == nullptr ||
		g_pOMFEditor->omf == nullptr ||
		g_pOMFEditor->omf->data_animparams.count <= 0 ||
		g_pOMFEditor->current_selected_animation_param < 0
		)
		return;

	bool is_disabled = false;

	ImGui::BeginDisabled(is_disabled);

	OMFData::AnimParamsData::AnimParams& param = g_pOMFEditor->omf->data_animparams.params[g_pOMFEditor->current_selected_animation_param];

	if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body_Params", 2))
	{
		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);

		constexpr float _kMinSpeed = 0.001f;

		ImGui::DragFloat("Speed", &param.speed, _kMinSpeed);
		ImGui::DragFloat("Power", &param.power, _kMinSpeed);
		ImGui::DragFloat("Accrue", &param.accrue, _kMinSpeed);
		ImGui::DragFloat("Falloff", &param.falloff, _kMinSpeed);

		ImGui::BeginDisabled(true);

		R_ASSERT(g_pOMFEditor->omf->data_anim.animations_count > 0);

		const auto& anim = g_pOMFEditor->omf->data_anim.anims[g_pOMFEditor->current_selected_animation_param];

		R_ASSERT(anim.data);

		int num_keys;
		std::memcpy(&num_keys, anim.data, sizeof(num_keys));

		float unit_time = g_pOMFEditor->is_motion_time_format_seconds_selected ? 30.0f : 1.0f;

		float length_with_current_speed = (float(num_keys) / unit_time) / param.speed;
		float length_with_rt = (float(num_keys) / unit_time) / 1.0f;

		const char* pPrintOutTemplate = "Length: %.4f | %.4f";

		if (g_pOMFEditor->is_motion_time_format_seconds_selected == false)
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

			if (ImGui::RadioButton("Keys##ToolsInGameImGui_OMFEditor_KeysRB", g_pOMFEditor->is_motion_time_format_keys_selected))
			{
				g_pOMFEditor->is_motion_time_format_radiobutton_changed = true;
				g_pOMFEditor->is_motion_time_format_seconds_selected = false;
				g_pOMFEditor->is_motion_time_format_keys_selected = !g_pOMFEditor->is_motion_time_format_keys_selected;
			}

			ImGui::TableSetColumnIndex(1);

			if (ImGui::RadioButton("Seconds##ToolsInGameImGui_OMFEditor_SecondsRB", g_pOMFEditor->is_motion_time_format_seconds_selected))
			{
				g_pOMFEditor->is_motion_time_format_radiobutton_changed = true;
				g_pOMFEditor->is_motion_time_format_keys_selected = false;
				g_pOMFEditor->is_motion_time_format_seconds_selected = !g_pOMFEditor->is_motion_time_format_seconds_selected;
			}

			if (g_pOMFEditor->is_motion_time_format_radiobutton_changed)
			{
				R_ASSERT2(!(g_pOMFEditor->is_motion_time_format_keys_selected && g_pOMFEditor->is_motion_time_format_seconds_selected), "You can't select both keys and seconds format at the same time!");

				if (g_pOMFEditor->is_motion_time_format_seconds_selected == false && g_pOMFEditor->is_motion_time_format_keys_selected == false)
				{
					g_pOMFEditor->is_motion_time_format_seconds_selected = true;
				}

				g_pOMFEditor->is_motion_time_format_radiobutton_changed = false;
			}

			ImGui::EndTable();
		}




		ImGui::TableSetColumnIndex(1);


		bool stop_at_end = (param.flags & (1 << 1)) == (1 << 1);
		bool check_box_changed = ImGui::Checkbox("Stop at end", &stop_at_end);

		if (check_box_changed)
		{
			if (stop_at_end)
			{
				param.flags |= (1 << 1);
			}
			else
			{
				param.flags &= ~(1 << 1);
			}
		}

		bool no_mix_selected = (param.flags & (1 << 2)) == (1 << 2);
		check_box_changed = ImGui::Checkbox("No mix", &no_mix_selected);

		if (check_box_changed)
		{
			if (no_mix_selected)
			{
				param.flags |= (1 << 2);
			}
			else
			{
				param.flags &= ~(1 << 2);
			}
		}

		bool sync_part = (param.flags & (1 << 3)) == (1 << 3);
		check_box_changed = ImGui::Checkbox("Sync part", &sync_part);

		if (check_box_changed)
		{
			if (sync_part)
			{
				param.flags |= (1 << 3);
			}
			else
			{
				param.flags &= ~(1 << 3);
			}
		}

		bool use_foot_steps = (param.flags & (1 << 4)) == (1 << 4);
		check_box_changed = ImGui::Checkbox("Use foot steps", &use_foot_steps);

		if (check_box_changed)
		{
			if (use_foot_steps)
			{
				param.flags |= (1 << 4);
			}
			else
			{
				param.flags &= ~(1 << 4);
			}
		}

		bool move_xform = (param.flags & (1 << 5)) == (1 << 5);
		check_box_changed = ImGui::Checkbox("Move XForm", &move_xform);

		if (check_box_changed)
		{
			if (move_xform)
			{
				param.flags |= (1 << 5);
			}
			else
			{
				param.flags &= ~(1 << 5);
			}
		}

		bool idle = (param.flags & (1 << 6)) == (1 << 6);
		check_box_changed = ImGui::Checkbox("Idle", &idle);

		if (check_box_changed)
		{
			if (idle)
			{
				param.flags |= (1 << 6);
			}
			else
			{
				param.flags &= ~(1 << 6);
			}
		}

		bool use_weapon_bone = (param.flags & (1 << 7)) == (1 << 7);
		check_box_changed = ImGui::Checkbox("Use weapon bone", &use_weapon_bone);

		if (check_box_changed)
		{
			if (use_weapon_bone)
			{
				param.flags |= (1 << 7);
			}
			else
			{
				param.flags &= ~(1 << 7);
			}
		}

		check_box_changed = ImGui::Checkbox("Has motion marks", &g_pOMFEditor->is_motion_marks_enabled);

		if (check_box_changed)
		{
			if (g_pOMFEditor->is_motion_marks_enabled == false)
			{
				param.marks.clear();
				param.marks_count = 0;

				g_pOMFEditor->list_box_motion_marks_names.clear();
				g_pOMFEditor->list_box_motion_marks_params_names.clear();

				g_pOMFEditor->is_show_popup_marks_cleared = true;
			}
		}

		ImGui::EndTable();
	}

	ImGui::EndDisabled();
}

void RenderOMFEditor_Draw_TableMain()
{
	if (g_pOMFEditor->is_file_loaded)
	{
		R_ASSERT2(g_pOMFEditor->omf, "must be initialized");
		ImGui::TextWrapped("Loaded file: [%s]", g_pOMFEditor->path.c_str());
		ImGui::Separator();

		constexpr const char* _kColumnOfMainTableNames[] = {
			"Editing",
#if IXRAY_OMF_EDITOR_ENABLE_VIEWER == 1
			"Viewer"
#endif
		};
		constexpr u8 _kColumnOfMainTableSize = sizeof(_kColumnOfMainTableNames) / sizeof(_kColumnOfMainTableNames[0]);

		RenderOMFEditor_Draw_ModalPopups();

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
					if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Header", 2))
					{
						ImGui::TableNextRow();
						ImGui::TableSetColumnIndex(0);

						constexpr const char* _kEmptyAnimationParams = "";

						bool is_empty = g_pOMFEditor->omf->data_animparams.count > 0;

						if (ImGui::Combo("Animation params##ToolsInGameImGui_OMFEditor_Data_Header_Combo", &g_pOMFEditor->current_selected_animation_param, g_pOMFEditor->combo_animation_params_data.data(), g_pOMFEditor->omf->data_animparams.count))
						{
							OMFEditor_ComboAnimationParamWasChanged(
								g_pOMFEditor,
								g_pOMFEditor->current_selected_animation_param
							);
						}

						ImGui::TableSetColumnIndex(1);

						ImGui::Text("Selected: [%s]", g_pOMFEditor->combo_animation_params_data[g_pOMFEditor->current_selected_animation_param]);
						ImGui::SameLine();

						if (ImGui::Button("Rename##ToolsInGameImGui_OMFEditor"))
						{
							g_pOMFEditor->is_show_popup_rename_animation_param = true;
							g_pOMFEditor->rename_temp = g_pOMFEditor->omf->data_animparams.params[g_pOMFEditor->current_selected_animation_param].name;
						}

						ImGui::EndTable();
					}
					ImGui::Separator();
					if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body", 2))
					{
						ImGui::TableNextRow();
						ImGui::TableSetColumnIndex(0);

						RenderOMFEditor_Draw_TableMain_BonesRenaming_Section();

						if (ImGui::CollapsingHeader("Bones##ToolsInGameImGui_OMFEditor_Data_Body"))
						{
							if (ImGui::Button("copy to clipboard##ToolsInGameImGui_OMFEditor_ShowBoneParts"))
							{
								bool status = OMFEditor_CopyBonePartsToClipboard(g_pOMFEditor);

								if (status)
									g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_suc = true;
								else
									g_pOMFEditor->is_show_popup_boneparts_was_copied_to_clipboard_fail = true;
							}

							ImGui::SameLine();

							if (ImGui::Button("save as file##ToolsInGameImGui_OMFEditor_ShowBoneParts"))
							{

							}

							RenderOMFEditor_Draw_TableMain_Bones_Section();
						}

						ImGui::TableSetColumnIndex(1);

						RenderOMFEditor_Draw_TableMain_Params();
						RenderOMFEditor_Draw_TableMain_MotionMarks();

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
}

void RenderToolsOMFEditorWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)])
		return;

	if (g_pOMFEditor == nullptr)
	{
		g_pOMFEditor = new CImGuiOMFEditor();
	}

	if (g_pOMFEditor)
	{
		if (ImGui::Begin("Editor - [OMF]##ToolsInGameImGui", nullptr, ImGuiWindowFlags_MenuBar))
		{
			RenderOMFEditor_Draw_TableHeader();

			RenderOMFEditor_Draw_TableMain();

			ImGui::End();
		}
	}
}