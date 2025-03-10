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

	int type = SDL_MESSAGEBOX_INFORMATION;

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
			float accue;
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
	OMFData omf;
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


void OMFEditor_LoadOMF_AnimData(OMFData::AnimData& data, std::ifstream& file)
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
}

void OMFEditor_LoadOMF_BoneData(OMFData::BoneData& data, std::ifstream& file)
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
}

void OMFEditor_LoadOMF_AnimParamsData_MotionMark(OMFData::AnimParamsData::AnimParams::MotionMark& mark, std::ifstream& file)
{

}

void OMFEditor_LoadOMF_AnimParamsData(OMFData::AnimParamsData& data, std::ifstream& file)
{

}

void OMFEditor_LoadOMF(OMFData& data, std::ifstream& file)
{
	R_ASSERT(file.good() && "lol, pass valid file here please");
	R_ASSERT(file.is_open() && "obviously file must be opened before reading");

	if (file.is_open() && file.good())
	{
		OMFEditor_LoadOMF_AnimData(data.data_anim, file);
		OMFEditor_LoadOMF_BoneData(data.data_bone, file);


	}
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
			for (unsigned char row = 0; row < 1; ++row)
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
			}

			ImGui::EndTable();
		}


		if (g_omf_editor.is_file_loaded)
		{

			ImGui::TextWrapped("Loaded file: [%s]", g_omf_editor.path);
			ImGui::Separator();
		}

		ImGui::End();
	}
}