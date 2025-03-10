#include "stdafx.h"
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

struct OMFEditorState
{
	~OMFEditorState()
	{
		if (omf)
		{
			xr_delete(omf);
		}
	}

	bool is_file_loaded{};
	bool animation_param_was_changed{};
	bool stop_at_end_selected{};
	bool no_mix_selected{};
	bool sync_part_selected{};
	bool use_foot_steps_selected{};
	bool move_xform_selected{};
	bool idle_selected{};
	bool use_weapon_bone_selected{};
	bool has_motion_marks_selected{};
	bool is_motion_time_format_seconds_selected{};
	bool is_motion_time_format_keys_selected{};
	bool is_motion_time_format_radiobutton_changed{};
	int current_selected_animation_param{};
	int current_selected_bone_rename{};
	float speed{};
	float power{};
	float accrue{};
	float falloff{};
	float length{};
	OMFData* omf{};
	OMFData::omf_name_t rename_temp;
	OMFData::omf_name_t rename_temp_bone;
	xr_vector<const char*> combo_animation_params_data;
	xr_set<size_t> combo_animation_params_name_hashes;
	xr_vector<const char*> combo_bones_data;
	xr_set<size_t> combo_bones_name_hashes;

	xr_stack_string<sizeof(string_path) * 2> path;
} g_omf_editor;

OMFEditorState* pEditor = &g_omf_editor;

template<std::size_t Size>
void OMFEditor_ReadString(xr_stack_string<Size>& str, std::ifstream& file)
{
	char symbol = -1;
	uint32_t str_length = 0;
	do
	{
		R_ASSERT(!(str_length > str.max_size()) && "report to developers you have too long serialized string");

		file.read(&symbol, 1);
		str += symbol;
		++str_length;
	} while (symbol != '\0');
}

template<std::size_t Size>
void OMFEditor_ReadStringMotionMark(xr_stack_string<Size>& str, std::ifstream& file)
{
	char symbol = -1;
	uint32_t str_length = 0;
	do
	{
		R_ASSERT(!(str_length > str.max_size()) && "report to developers you have too long serialized string");

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

	R_ASSERT(data.count <= data.parts.max_size() && "report to developers!");

	for (int16_t i = 0; i < data.count; ++i)
	{
		data.parts.push_back({});
		OMFData::BoneParts& bp = data.parts.back();

		OMFEditor_ReadString(bp.name, file);
		file.read(reinterpret_cast<char*>(&bp.count), sizeof(bp.count));

		R_ASSERT(bp.count <= bp.bones.max_size() && "report to developers!");

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

void OMFEditor_Init_ComboAnimationParams(OMFEditorState* p_state, OMFData& data)
{
	R_ASSERT(p_state->combo_animation_params_data.empty() && "did you clear data before init?");
	R_ASSERT(p_state->combo_animation_params_name_hashes.empty() && "did you clear data before init?");

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

void OMFEditor_Init_ComboBones(OMFEditorState* p_state, OMFData& data)
{
	R_ASSERT(p_state->combo_bones_data.empty() && "did you clear data before init?");
	R_ASSERT(p_state->combo_bones_name_hashes.empty() && "did you clear data before init?");

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

void OMFEditor_Init_CurrentAnimationParams(int animation_param_id, OMFData& data, OMFEditorState* p_state)
{
	if (p_state)
	{
		auto& param = data.data_animparams.params[animation_param_id];
		p_state->speed = param.speed;
		p_state->accrue = param.accrue;
		p_state->falloff = param.falloff;
		p_state->power = param.power;

		// probably you could replace with for-loop but for now it is just for simplicity
		p_state->stop_at_end_selected = (param.flags & (1 << 1)) == (1 << 1);
		p_state->no_mix_selected = (param.flags & (1 << 2)) == (1 << 2);
		p_state->sync_part_selected = (param.flags & (1 << 3)) == (1 << 3);
		p_state->use_foot_steps_selected = (param.flags & (1 << 4)) == (1 << 4);
		p_state->move_xform_selected = (param.flags & (1 << 5)) == (1 << 5);
		p_state->idle_selected = (param.flags & (1 << 6)) == (1 << 6);
		p_state->use_weapon_bone_selected = (param.flags & (1 << 7)) == (1 << 7);
		p_state->has_motion_marks_selected = (data.data_bone.ogf_version == 4 && param.marks_count > 0);
	}
}

void OMFEditor_Init(OMFEditorState* p_state, OMFData& data)
{
	if (!p_state)
		return;

	p_state->current_selected_animation_param = 0;
	p_state->current_selected_bone_rename = 0;
	p_state->animation_param_was_changed = false;

	p_state->is_motion_time_format_seconds_selected = true;
	p_state->is_motion_time_format_radiobutton_changed = true;
	p_state->combo_animation_params_data.clear();
	p_state->combo_animation_params_name_hashes.clear();
	p_state->combo_bones_data.clear();
	p_state->combo_bones_name_hashes.clear();

	OMFEditor_Init_ComboAnimationParams(p_state, data);
	OMFEditor_Init_ComboBones(p_state, data);

	if (data.data_bone.count > 0)
	{
		R_ASSERT(p_state->combo_bones_data.empty() == false && "can't be!");
		p_state->rename_temp_bone = p_state->combo_bones_data[0];
	}

	if (data.data_animparams.count > 0)
	{
		OMFEditor_Init_CurrentAnimationParams(p_state->current_selected_animation_param, data, p_state);
	}
}

bool OMFEditor_LoadOMF(OMFData& data, std::ifstream& file)
{
	R_ASSERT(file.good() && "lol, pass valid file here please");
	R_ASSERT(file.is_open() && "obviously file must be opened before reading");

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

void OMFEditor_LoadFile(OMFEditorState* p_state)
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

bool OMFEditor_CopyBonePartsToClipboard(OMFEditorState* p_state)
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

void RenderToolsOMFEditorWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)])
		return;


	if (ImGui::Begin("Editor - [OMF]##ToolsInGameImGui", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)]))
	{
		if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_MainTable", 10))
		{
			ImGui::TableNextRow();

			ImGui::TableSetColumnIndex(0);
			if (ImGui::Button("Load##ToolsInGameImGui_OMFEditor"))
			{
				OMFEditor_LoadFile(&g_omf_editor);
			}


			if (g_omf_editor.is_file_loaded)
			{
				ImGui::TableSetColumnIndex(1);
				if (ImGui::Button("Close##ToolsInGameImGui_OMFEditor"))
				{
					g_omf_editor.is_file_loaded = false;
					pEditor->path[0] = 0;
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


		if (g_omf_editor.is_file_loaded)
		{
			R_ASSERT(g_omf_editor.omf && "must be initialized");
			ImGui::TextWrapped("Loaded file: [%s]", g_omf_editor.path);
			ImGui::Separator();

			if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Header", 2))
			{
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(0);

				constexpr const char* _kEmptyAnimationParams = "";

				bool is_empty = g_omf_editor.omf->data_animparams.count > 0;

				if (ImGui::Combo("Animation params##ToolsInGameImGui_OMFEditor_Data_Header_Combo", &g_omf_editor.current_selected_animation_param, g_omf_editor.combo_animation_params_data.data(), g_omf_editor.omf->data_animparams.count))
				{
					if (g_omf_editor.current_selected_animation_param > -1)
					{
						OMFEditor_Init_CurrentAnimationParams(g_omf_editor.current_selected_animation_param, *g_omf_editor.omf, &g_omf_editor);
					}
				}

				ImGui::TableSetColumnIndex(1);

				ImGui::Text("Selected: [%s]", g_omf_editor.combo_animation_params_data[g_omf_editor.current_selected_animation_param]);
				ImGui::SameLine();

				if (ImGui::Button("Rename##ToolsInGameImGui_OMFEditor"))
				{
					ImGui::OpenPopup(_kOMFEditorModalWindow_RenameAnimationParam);
					g_omf_editor.rename_temp = g_omf_editor.omf->data_animparams.params[g_omf_editor.current_selected_animation_param].name;
				}

				if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_RenameAnimationParam, nullptr, ImGuiWindowFlags_AlwaysAutoResize))
				{
					auto& current_param = g_omf_editor.omf->data_animparams.params[g_omf_editor.current_selected_animation_param];
					ImGui::InputText("##ToolsInGameImGui_OMFEditor_RenameAnimationParamInputText", g_omf_editor.rename_temp.data(), g_omf_editor.rename_temp.max_size());

					ImGui::SetItemDefaultFocus();
					if (ImGui::Button("Save##ToolsInGameImGui_OMFEditor_RenameAnimationParam"))
					{
						size_t hash_temp = std::hash<std::string_view>()(g_omf_editor.rename_temp.c_str());

						if (g_omf_editor.combo_animation_params_name_hashes.find(hash_temp) != g_omf_editor.combo_animation_params_name_hashes.end() && g_omf_editor.rename_temp != current_param.name)
						{
							ImGui::OpenPopup(_kOMFEditorModalWindow_WarningRenameHasCollision);
						}
						else
						{
							OMFData::omf_name_t previous = current_param.name;
							size_t previous_temp = std::hash<std::string_view>()(previous.c_str());
							if (g_omf_editor.combo_animation_params_name_hashes.find(previous_temp) != g_omf_editor.combo_animation_params_name_hashes.end())
							{
								g_omf_editor.combo_animation_params_name_hashes.erase(previous_temp);
							}

							current_param.name = g_omf_editor.rename_temp;
							g_omf_editor.combo_animation_params_name_hashes.insert(std::hash<std::string_view>()(current_param.name.c_str()));
							ImGui::CloseCurrentPopup();
						}
					}

					ImGui::SameLine();

					if (ImGui::Button("Cancel##ToolsInGameImGui_OMFEditor_RenameAnimationParam"))
					{
						ImGui::CloseCurrentPopup();
					}

					bool cross = true;
					if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_WarningRenameHasCollision, &cross, ImGuiWindowFlags_AlwaysAutoResize))
					{
						ImGui::Text("Failed to rename because you have already same name!");
						ImGui::EndPopup();
					}

					ImGui::EndPopup();
				}

				ImGui::EndTable();
			}
			ImGui::Separator();
			if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body", 2))
			{
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(0);

				if (ImGui::CollapsingHeader("Rename bones##ToolsInGameImGui_OMFEditor_Data_Body"))
				{
					if (g_omf_editor.omf->data_bone.count > 0)
					{
						ImGui::SeparatorText("Select bone");

						if (ImGui::Combo("Bones##ToolsInGameImGui_OMFEditor_RenameBones", &g_omf_editor.current_selected_bone_rename, g_omf_editor.combo_bones_data.data(), g_omf_editor.combo_bones_data.size()))
						{
							g_omf_editor.rename_temp_bone = g_omf_editor.combo_bones_data[g_omf_editor.current_selected_bone_rename];
						}

						ImGui::SeparatorText("Edit");

						ImGui::Text("bone id: %d", g_omf_editor.current_selected_bone_rename);
						ImGui::InputText("##ToolsInGameImGui_OMFEditor_RenameBoneIT", g_omf_editor.rename_temp_bone.data(), g_omf_editor.rename_temp_bone.max_size());
						ImGui::SameLine();
						if (ImGui::Button("apply##ToolsInGameImGui_OMFEditor_RenameBone"))
						{
							size_t hash_temp = std::hash<std::string_view>()(std::string_view(g_omf_editor.rename_temp_bone.c_str()));

							if (g_omf_editor.combo_bones_name_hashes.find(hash_temp) != g_omf_editor.combo_bones_name_hashes.end() && g_omf_editor.combo_bones_data[g_omf_editor.current_selected_bone_rename] != g_omf_editor.rename_temp_bone)
							{
								ImGui::OpenPopup(_kOMFEditorModalWindow_BoneRenameHasCollion);
							}
							else
							{
								size_t hash_current = std::hash<std::string_view>()(g_omf_editor.combo_bones_data[g_omf_editor.current_selected_bone_rename]);
								if (g_omf_editor.combo_bones_name_hashes.find(hash_current) != g_omf_editor.combo_bones_name_hashes.end())
								{
									g_omf_editor.combo_bones_name_hashes.erase(hash_current);
								}

								g_omf_editor.combo_bones_name_hashes.insert(hash_temp);
								OMFEditor_RenameBone(g_omf_editor.current_selected_bone_rename, g_omf_editor.rename_temp_bone, *g_omf_editor.omf);
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

				if (ImGui::CollapsingHeader("Show bone parts##ToolsInGameImGui_OMFEditor_Data_Body"))
				{
					if (ImGui::Button("copy to clipboard##ToolsInGameImGui_OMFEditor_ShowBoneParts"))
					{
						bool status = OMFEditor_CopyBonePartsToClipboard(&g_omf_editor);
						
						if (status)
							ImGui::OpenPopup(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardSuccessful);
						else
							ImGui::OpenPopup(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardFailed);
					}

					ImGui::SameLine();

					if (ImGui::Button("save as file##ToolsInGameImGui_OMFEditor_ShowBoneParts"))
					{

					}

					ImGui::SeparatorText("Bones");
					for (const auto& bone_part : g_omf_editor.omf->data_bone.parts)
					{
						if (ImGui::TreeNode(bone_part.name.c_str()))
						{
							ImGui::Text("bone count: %d", bone_part.bones.size());
							ImGui::Separator();
							for (const auto& bone : bone_part.bones)
							{
								ImGui::Text(bone.name.c_str());
							}

							ImGui::TreePop();
						}
					}

					if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardFailed, nullptr, ImGuiWindowFlags_AlwaysAutoResize))
					{
						ImGui::Text("Text wasn't copied to your clipboard! Try again or report to developers!");

						if (ImGui::Button("OK##ToolsInGameImGui_OMFEditor_ClipBoard"))
						{
							ImGui::CloseCurrentPopup();
						}

						ImGui::EndPopup();
					}

					if (ImGui::BeginPopupModal(_kOMFEditorModalWindow_BonePartsWasCopiedToClipboardSuccessful, nullptr, ImGuiWindowFlags_AlwaysAutoResize))
					{
						ImGui::Text("Text was successfully copied to your clipboard!");

						if (ImGui::Button("OK##ToolsInGameImGui_OMFEditor_ClipBoard"))
						{
							ImGui::CloseCurrentPopup();
						}

						ImGui::EndPopup();
					}
				}

				ImGui::TableSetColumnIndex(1);

				if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body_Params", 2))
				{
					ImGui::TableNextRow();
					ImGui::TableSetColumnIndex(0);

					ImGui::DragFloat("Speed", &g_omf_editor.speed);
					ImGui::DragFloat("Power", &g_omf_editor.power);
					ImGui::DragFloat("Accrue", &g_omf_editor.accrue);
					ImGui::DragFloat("Falloff", &g_omf_editor.falloff);

					ImGui::TableSetColumnIndex(1);

					ImGui::Checkbox("Stop at end", &g_omf_editor.stop_at_end_selected);
					ImGui::Checkbox("No mix", &g_omf_editor.no_mix_selected);
					ImGui::Checkbox("Sync part", &g_omf_editor.sync_part_selected);
					ImGui::Checkbox("Use foot steps", &g_omf_editor.use_foot_steps_selected);
					ImGui::Checkbox("Move XForm", &g_omf_editor.move_xform_selected);
					ImGui::Checkbox("Idle", &g_omf_editor.idle_selected);
					ImGui::Checkbox("Use weapon bone", &g_omf_editor.use_weapon_bone_selected);
					ImGui::Checkbox("Has motion marks", &g_omf_editor.has_motion_marks_selected);

					ImGui::EndTable();
				}

				ImGui::EndTable();
			}

			ImGui::Separator();

			if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body2", 2))
			{
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(0);

				ImGui::TableSetColumnIndex(1);

				ImGui::SeparatorText("Motion time format");

				if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_MotionTimeFormat", 2))
				{
					ImGui::TableNextRow();
					ImGui::TableSetColumnIndex(0);

					ImGui::BeginDisabled(!g_omf_editor.has_motion_marks_selected);
					if (ImGui::RadioButton("Keys##ToolsInGameImGui_OMFEditor_KeysRB", g_omf_editor.is_motion_time_format_keys_selected))
					{
						g_omf_editor.is_motion_time_format_radiobutton_changed = true;
						g_omf_editor.is_motion_time_format_seconds_selected = false;
						g_omf_editor.is_motion_time_format_keys_selected = !g_omf_editor.is_motion_time_format_keys_selected;
					}
					ImGui::EndDisabled();

					ImGui::TableSetColumnIndex(1);

					ImGui::BeginDisabled(!g_omf_editor.has_motion_marks_selected);
					if (ImGui::RadioButton("Seconds##ToolsInGameImGui_OMFEditor_SecondsRB", g_omf_editor.is_motion_time_format_seconds_selected))
					{
						g_omf_editor.is_motion_time_format_radiobutton_changed = true;
						g_omf_editor.is_motion_time_format_keys_selected = false;
						g_omf_editor.is_motion_time_format_seconds_selected = !g_omf_editor.is_motion_time_format_seconds_selected;
					}
					ImGui::EndDisabled();

					if (g_omf_editor.has_motion_marks_selected && g_omf_editor.is_motion_time_format_radiobutton_changed)
					{
						R_ASSERT(!(g_omf_editor.is_motion_time_format_keys_selected && g_omf_editor.is_motion_time_format_seconds_selected) && "can't be both selected at same time!");

						// todo: add implemenetation here

						g_omf_editor.is_motion_time_format_radiobutton_changed = false;
					}

					ImGui::EndTable();
				}

				ImGui::EndTable();
			}

			ImGui::Separator();

			if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body3", 2))
			{
				ImGui::TableNextRow();

				ImGui::TableSetColumnIndex(0);

				ImGui::TableSetColumnIndex(1);

				ImGui::SeparatorText("Motion marks");

				ImGui::BeginDisabled(!g_omf_editor.has_motion_marks_selected);
				if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_MotionMarksTable", 3))
				{
					ImGui::TableNextRow();

					ImGui::TableSetColumnIndex(0);

					ImGui::SeparatorText("Mark Group");
					ImGui::ListBox("##ToolsInGameImGui_OMFEditor_MarkGroupLB", 0, 0, 0);

					ImGui::Button("Add##ToolsInGameImGui_OMFEditor_MotionMarksGroup");
					ImGui::SameLine();
					ImGui::Button("Delete##ToolsInGameImGui_OMFEditor_MotionMarksGroup");

					ImGui::TableSetColumnIndex(1);

					ImGui::SeparatorText("Marks");
					ImGui::ListBox("##ToolsInGameImGui_OMFEditor_MarksLB", 0, 0, 0);

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

		ImGui::End();
	}
}