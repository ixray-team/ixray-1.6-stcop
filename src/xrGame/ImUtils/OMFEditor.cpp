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

struct OMFData
{
	struct AnimVector
	{
		int32_t section_id;
		uint32_t section_size;
		// dynamically allocated
		char* data;
		xr_stack_string<32> name;
	};

	struct BoneParts
	{
		struct Bone
		{
			uint32_t id;
			xr_stack_string<32> name;
		};

		int16_t count;
		xr_stack_string<32> name;

		std::array<Bone, 32> bones;
	};

	struct BoneData
	{
		int16_t ogf_version;
		int16_t count;
		int32_t section_id;
		uint32_t section_size;
		std::array<BoneParts, 64> parts;
	};

	struct AnimData
	{
		int32_t section_id;
		uint32_t section_size;

		int32_t section_id2;
		uint32_t section_size2;

		int32_t animations_count;
		short animations_params_count;

		std::array<AnimVector, 128> anims;
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
				xr_stack_string<32> name;
				std::array<Params, 64> params;
			};

			int16_t bone_or_part;
			int16_t motion_id;
			int32_t flags;
			int32_t marks_count;
			float speed;
			float power;
			float accrue;
			float falloff;
			xr_stack_string<32> name;
			std::array<MotionMark, 32> marks;
		};

		int16_t count;
		std::array<AnimParams, 16> params;
	};

	AnimData data_anim;
	BoneData data_bone;
	AnimParamsData data_animparams;
};

struct OMFEditorState
{
	bool is_file_loaded;
	bool animation_param_was_changed;
	int current_selected_animation_param;
	float speed;
	float power;
	float accrue;
	float falloff;
	float length;
	const char* combo_animation_params_data[256];
	xr_stack_string<sizeof(string_path) * 2> path;
	OMFData omf;
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
		OMFData::AnimVector& av = data.anims[i];

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
		OMFData::BoneParts& bp = data.parts[i];

		OMFEditor_ReadString(bp.name, file);
		file.read(reinterpret_cast<char*>(&bp.count), sizeof(bp.count));

		R_ASSERT(bp.count <= bp.bones.max_size() && "report to developers!");

		for (int j = 0; j < bp.count; ++j)
		{
			OMFData::BoneParts::Bone& bone = bp.bones[j];

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
		OMFData::AnimParamsData::AnimParams::MotionMark::Params& mark_param = mark.params[i];

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

	constexpr int16_t _kSize = sizeof(data.params) / sizeof(data.params[0]);

	if (_kSize <= data.count)
	{
		R_ASSERT(false && "report to developers!");
		ShowMessageBox(_eMessageBoxStatus::kWarning, "Report to developers", "too many anim params!");
		return false;
	}

	for (int16_t i = 0; i < data.count; ++i)
	{
		OMFData::AnimParamsData::AnimParams& param = data.params[i];
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
					OMFData::AnimParamsData::AnimParams::MotionMark& mark = param.marks[mark_id];

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
	if (data.data_animparams.count > 0)
	{
		constexpr int16_t _kSize = sizeof(p_state->combo_animation_params_data) / sizeof(p_state->combo_animation_params_data[0]);

		R_ASSERT(_kSize >= data.data_animparams.count && "report to developers!");

		for (int16_t i = 0; i < data.data_animparams.count; ++i)
		{
			p_state->combo_animation_params_data[i] = data.data_animparams.params[i].name.c_str();
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
	}
}

void OMFEditor_Init(OMFEditorState* p_state, OMFData& data)
{
	if (!p_state)
		return;

	p_state->current_selected_animation_param = 0;
	p_state->animation_param_was_changed = false;
	OMFEditor_Init_ComboAnimationParams(p_state, data);

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
					OMFEditor_LoadOMF(p_state->omf, file_omf);
				}
				else
				{
					ShowMessageBox(_eMessageBoxStatus::kWarning, "Warning", "failed to load file!");
				}

				file_omf.close();

				p_state->is_file_loaded = status;

				OMFEditor_Init(p_state, p_state->omf);
			}
		}
	}
}

void RenderToolsOMFEditorWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)])
		return;


	if (ImGui::Begin("Editor - [OMF]##ToolsInGameImGui", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)]))
	{
		if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_MainTable", 5))
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
			}

			ImGui::EndTable();
		}


		if (g_omf_editor.is_file_loaded)
		{
			ImGui::TextWrapped("Loaded file: [%s]", g_omf_editor.path);
			ImGui::Separator();

			if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Header", 2))
			{
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(0);

				constexpr const char* _kEmptyAnimationParams = "";

				bool is_empty = g_omf_editor.omf.data_animparams.count > 0;

				if (ImGui::Combo("Animation params##ToolsInGameImGui_OMFEditor_Data_Header_Combo", &g_omf_editor.current_selected_animation_param, g_omf_editor.combo_animation_params_data, g_omf_editor.omf.data_animparams.count))
				{
					bool changed = false;
				}

				ImGui::TableSetColumnIndex(1);

				ImGui::Text("Selected: [%s]", g_omf_editor.combo_animation_params_data[g_omf_editor.current_selected_animation_param]);


				ImGui::EndTable();
			}
			ImGui::Separator();
			if (ImGui::BeginTable("##ToolsInGameImGui_OMFEditor_Data_Body", 2))
			{
				ImGui::TableNextRow();
				ImGui::TableSetColumnIndex(0);

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

					bool temp = false;
					ImGui::Checkbox("Stop at end", &temp);
					ImGui::Checkbox("No mix", &temp);
					ImGui::Checkbox("Sync part", &temp);
					ImGui::Checkbox("Use foot steps", &temp);
					ImGui::Checkbox("Move XForm", &temp);
					ImGui::Checkbox("Idle", &temp);
					ImGui::Checkbox("Use weapon bone", &temp);
					ImGui::Checkbox("Has motion marks", &temp);

					ImGui::EndTable();
				}

				ImGui::EndTable();
			}

			ImGui::Separator();

		}

		ImGui::End();
	}
}