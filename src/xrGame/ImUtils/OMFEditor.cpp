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
	kError
};

void ShowMessageBox(_eMessageBoxStatus status, std::string_view title, std::string_view message)
{
	const SDL_MessageBoxButtonData buttons[] =
	{
		{ 0, 0, "Ok" }
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

	const SDL_MessageBoxData messageboxdata =
	{
		type | SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT,		/* .flags */
		nullptr,					/* .window */
		title.data(),				/* .title */
		message.data(),			/* .message */
		std::size(buttons),			/* .numbuttons */
		buttons,					/* .buttons */
		nullptr						/* .colorScheme */
	};

	int button_id = -1;

	int ret = SDL_ShowMessageBox(&messageboxdata, &button_id);
}


constexpr unsigned int _kMaxStringFieldNameLength = sizeof(string128);
constexpr const char* _kOMFEditorModalWindow_RenameAnimationParam = "Rename##ToolsInGameImGui_OMGEditor_AnimationParam";
constexpr const char* _kOMFEditorModalWindow_WarningRenameHasCollision = "Warning##ToolsInGameImGui_OMFEditor_AnimationParamFailedRenaming";
constexpr const char* _kOMFEditorModalWindow_BonePartsWasCopiedToClipboardSuccessful = "Successful!##ToolsInGameImGui_OMFEditor_BonePartsToClipboard";
constexpr const char* _kOMFEditorModalWindow_BonePartsWasCopiedToClipboardFailed = "Failed!##ToolsInGameImGui_OMFEditor_BonePartsToClipboard";
constexpr const char* _kOMFEditorModalWindow_BoneRenameHasCollion = "Warning!##ToolsInGameImGui_OMFEditor_BoneRenameHasCollision";
constexpr const char* _kOMFEditorModalWindow_AnimationParamMotionMarksCleared = "Warning!##ToolsOMFEditor_MotionMarksCleared";

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
	bool is_show_popup_boneparts_was_copied_to_clipboard_suc;
	bool is_show_popup_boneparts_was_copied_to_clipboard_fail;
	bool is_show_popup_boneparts_rename_has_collision{};


	bool is_file_loaded{};
	bool animation_param_was_changed{};
	bool is_motion_time_format_seconds_selected{};
	bool is_motion_time_format_keys_selected{};
	bool is_motion_time_format_radiobutton_changed{};
	bool is_motion_marks_enabled{};
	int current_selected_animation_param{};
	int current_selected_bone_rename{};
	OMFData* omf{};
	OMFData::omf_name_t rename_temp;
	OMFData::omf_name_t rename_temp_bone;
	xr_vector<const char*> combo_animation_params_data;
	xr_set<size_t> combo_animation_params_name_hashes;
	xr_vector<const char*> combo_bones_data;
	xr_set<size_t> combo_bones_name_hashes;

	xr_stack_string<sizeof(string_path) * 2> path;
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

bool OMFEditor_LoadOMF_AnimParamsData_MotionMark(OMFData::AnimParamsData::AnimParams::MotionMark& mark, std::ifstream& file)
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
						delete p_state->omf;
						p_state->omf = new OMFData();
					}
					else
					{
						p_state->omf = new OMFData();
					}

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
	if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_MainTable", 10))
	{
		ImGui::TableNextRow();

		ImGui::TableSetColumnIndex(0);
		if (ImGui::Button("Load##ToolsInGameImGui_OMFEditor"))
		{
			OMFEditor_LoadFile(g_pOMFEditor);
		}


		if (g_pOMFEditor->is_file_loaded)
		{
			ImGui::TableSetColumnIndex(1);
			if (ImGui::Button("Close##ToolsInGameImGui_OMFEditor"))
			{
				g_pOMFEditor->is_file_loaded = false;
				g_pOMFEditor->path[0] = 0;
			}

			ImGui::TableSetColumnIndex(2);
			if (ImGui::Button("Save##ToolsInGameImGui_OMFEditor"))
			{

			}

			ImGui::TableSetColumnIndex(3);
			if (ImGui::Button("Save As...##ToolsInGameImGui_OMFEditor"))
			{

			}

			ImGui::TableSetColumnIndex(4);
			if (ImGui::Button("Merge with##ToolsInGameImGui_OMFEditor"))
			{

			}

			ImGui::TableSetColumnIndex(5);
			if (ImGui::Button("Add anims from##ToolsInGameImGui_OMFEditor"))
			{
			}

			ImGui::TableSetColumnIndex(6);
			if (ImGui::Button("Try repair##ToolsInGameImGui_OMFEditor"))
			{

			}

			ImGui::TableSetColumnIndex(7);
			if (ImGui::Button("Swap anim marks##ToolsInGameImGui_OMFEditor"))
			{

			}

			//ImGui::TableSetColumnIndex(8);
			//if (ImGui::Button("Rename bones##ToolsInGameImGui_OMFEditor"))
			//{
			//}

			//	ImGui::TableSetColumnIndex(9);
			//	if (ImGui::Button("Show bone parts##ToolsInGameImGui_OMFEditor"))
			//	{
			//	}

		}

		ImGui::EndTable();
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

			ImGui::TableSetColumnIndex(1);

			ImGui::SeparatorText("Motion marks");

			ImGui::BeginDisabled(!has_motion_marks_selected);
			if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_MotionMarksTable", 3))
			{
				ImGui::TableNextRow();

				ImGui::TableSetColumnIndex(0);

				ImGui::SeparatorText("Mark Group");
				//	ImGui::ListBox("##ToolsInGameImGui_OMFEditor_MarkGroupLB", 0, 0, 0);

				ImGui::Button("Add##ToolsInGameImGui_OMFEditor_MotionMarksGroup");
				ImGui::SameLine();
				ImGui::Button("Delete##ToolsInGameImGui_OMFEditor_MotionMarksGroup");

				ImGui::TableSetColumnIndex(1);

				ImGui::SeparatorText("Marks");
				//	ImGui::ListBox("##ToolsInGameImGui_OMFEditor_MarksLB", 0, 0, 0);

				ImGui::Button("Add##ToolsInGameImGui_OMFEditor_MotionMarksMarks");
				ImGui::SameLine();
				ImGui::Button("Delete##ToolsInGameImGui_OMFEditor_MotionMarksMarks");

				ImGui::TableSetColumnIndex(2);

				ImGui::SeparatorText("Mark settings");

				float fStart{};
				ImGui::DragFloat("Start##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fStart);


				float fEnd{};
				ImGui::DragFloat("End##ToolsInGameImGui_OMFEditor_MotionMarksMark", &fEnd);

				ImGui::EndTable();
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
		if (ImGui::Begin("Editor - [OMF]##ToolsInGameImGui"))
		{
			RenderOMFEditor_Draw_TableHeader();

			RenderOMFEditor_Draw_TableMain();

			ImGui::End();
		}
	}
}