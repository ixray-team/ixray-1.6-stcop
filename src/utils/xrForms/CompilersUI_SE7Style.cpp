#include "CompilersUI.h"
#include <imgui.h>

#include "../../xrCore/xrCore.h"

static bool ShowMainUI = true;
struct LevelFileData
{
	xr_string Name;
	bool Select = false;
};
static xr_vector<LevelFileData> Files;
extern CompilersMode gCompilerMode;

void InitializeUIData()
{
	string_path LevelsDir = {};
	FS.update_path(LevelsDir, "$game_levels$", "");

	for (const xr_path& Dir : std::filesystem::directory_iterator{ LevelsDir })
	{
		if (!std::filesystem::is_directory(Dir))
			continue;

		auto& LevelInfo = Files.emplace_back();
		LevelInfo.Name = Dir.xfilename();

	}
}

void DrawCompilerConfig();
void DrawAIConfig();
void DrawDOConfig();
void DrawLCConfig();

void RenderMainUI()
{
	if (!ShowMainUI)
		return;

	int Size[2] = {};

	SDL_GetWindowSize(g_AppInfo.Window, &Size[0], &Size[1]);

	ImGui::SetNextWindowPos({ 0, 0 });
	ImGui::SetNextWindowSize({ (float)Size[0], (float)Size[1]});

	if (ImGui::Begin("MainForm", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus))
	{
		 
		//ImGui::Text("Levels:");

		ImVec2 ListBoxSize = { float(Size[0] - 20), float ( Size[1] - 75) };
		if (ImGui::BeginTable("##Levels", 5, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Borders | ImGuiTableFlags_ScrollY, ListBoxSize))
		{
			// 
			ImGui::TableSetupColumn("Levels");
			ImGui::TableSetupColumn("Settings");
			ImGui::TableSetupColumn("xrLC");
			ImGui::TableSetupColumn("xrAI");
			ImGui::TableSetupColumn("xrDO");

			ImGui::TableHeadersRow();

			ImGui::TableNextRow();
			ImGui::TableSetColumnIndex(0);
			
			ImVec2 ListBoxSize2 = { 200, float(Size[1] - 115) };
			if (  ImGui::BeginTable("##Levels", 2, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Borders | ImGuiTableFlags_ScrollY, ListBoxSize2)  )
			{
				ImGui::TableSetupColumn("Name");
				ImGui::TableSetupColumn("Prop");
				ImGui::TableHeadersRow();

				size_t Iter = 1;
				for (auto& [File, Selected] : Files)
				{
					ImGui::TableNextColumn();
					ImGui::Selectable(File.c_str());

					ImGui::TableNextColumn();
					ImGui::Checkbox(("##check" + File).c_str(), &Selected);
					Iter++;

					if (Iter < Files.size())
					{
						ImGui::TableNextRow();
					}
				}
				ImGui::EndTable();
			}

			ImGui::TableSetColumnIndex(1);
			
			DrawCompilerConfig();
 			
			ImGui::TableSetColumnIndex(2);
			
			DrawLCConfig();

			ImGui::TableSetColumnIndex(3);

			DrawAIConfig();

			ImGui::TableSetColumnIndex(4);

			DrawDOConfig();

			ImGui::EndTable();  
		}
	}
	auto BSize = ImGui::GetContentRegionAvail();

	if (ImGui::Button("Run Compiler", { BSize.x, 50 }))
	{
		bool isReady = false;
		if (gCompilerMode.LC)
		{
			for (auto& FILE : Files)
			{
				if (FILE.Select)
				{
					strcpy(gCompilerMode.level_name, FILE.Name.c_str());
					break;
				}
			}
			isReady = true;
			Msg("Level For Building : %s", gCompilerMode.level_name);
		}

		if (gCompilerMode.AI)
		{
			xr_string temp_maps;

			int Size = 0;
			for (auto& FILE : Files)
			{
				if (FILE.Select)
				{
					if (!temp_maps.empty())
					{
						temp_maps += ",";
					}
					
					temp_maps += FILE.Name.c_str();
					Size++;
					//break;
				}
			}

			if (Size > 1)
				temp_maps.pop_back();


			if (gCompilerMode.AI_BuildLevel)
			{
				if (Size > 1)
				{
					Msg("Dont Correct Level Size > 1");
					return;
				}

				strcpy(gCompilerMode.level_name, temp_maps.c_str());
				isReady = true;
			}

			if (gCompilerMode.AI_BuildSpawn)
			{
				strcpy(gCompilerMode.level_name, temp_maps.c_str());
				isReady = true;
			}



			Msg("Level For Building : %s", temp_maps.c_str());
		}

		if (isReady)
		{
			extern void StartCompile();
			StartCompile();
		}

	}

	ImGui::End();
}

int item_current_selected = 2;
int item_current_jitter = 2;
int item_current_jitter_mu = 5;

const char* items[] = { "1024", "2048", "4096", "8192" };
const char* itemsJitter[] = { "1", "4", "9" };
const char* itemsJitterMU[] = { "1", "2", "3", "4", "5", "6"};

void DrawLCConfig()
{
	if (ImGui::BeginChild("LC", { 200, 370 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
	{
		ImGui::Checkbox("Lighting Compiler", &gCompilerMode.LC);
		ImGui::Separator();

		ImGui::BeginDisabled(!gCompilerMode.LC);
		ImGui::Checkbox("GI (Radiosity)", &gCompilerMode.LC_GI);
		ImGui::Checkbox("No Sun", &gCompilerMode.LC_NoSun);
		ImGui::Checkbox("No Smooth Group", &gCompilerMode.LC_NoSMG);
		ImGui::Checkbox("Noise", &gCompilerMode.LC_Noise);
		ImGui::Checkbox("Tesselation", &gCompilerMode.LC_Tess);
		ImGui::Checkbox("Skip invalid faces", &gCompilerMode.LC_SkipInvalidFaces);
		ImGui::Checkbox("Texture RGBA", &gCompilerMode.LC_tex_rgba);
		ImGui::Checkbox("Skip Subdivide", &gCompilerMode.LC_NoSubdivide);
		ImGui::Checkbox("Skip Welding", &gCompilerMode.LC_skipWeld);

		ImGui::SetNextItemWidth(100);
		if (ImGui::Combo("lmaps", &item_current_selected, items, 4))
		{
			gCompilerMode.LC_sizeLmaps = atoi(items[item_current_selected]);
		}

		ImGui::Separator();
		ImGui::Checkbox("Overload Prebuild", &gCompilerMode.IsOverloadedSettings);

		ImGui::BeginDisabled(!gCompilerMode.IsOverloadedSettings);
		ImGui::SetNextItemWidth(100);
		ImGui::Combo("JitterMU", &item_current_jitter_mu, itemsJitterMU, 6);
		ImGui::SetNextItemWidth(100);
		ImGui::Combo("Jitter", &item_current_jitter, itemsJitter, 3);

		ImGui::SetNextItemWidth(100);
		ImGui::InputFloat("Pixels", &gCompilerMode.LC_Pixels);

		gCompilerMode.LC_JSample   = atoi(itemsJitter[item_current_jitter]);
		gCompilerMode.LC_JSampleMU = atoi(itemsJitterMU[item_current_jitter_mu]);
 
		ImGui::EndDisabled();
		ImGui::EndDisabled();
		
		
		ImGui::EndChild();
	}


}

void DrawDOConfig()
{
	if (ImGui::BeginChild("DO", { 200, 370 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
	{
		ImGui::Checkbox("Details Compiler", &gCompilerMode.DO);
		ImGui::Separator();

		ImGui::BeginDisabled(!gCompilerMode.DO);
		ImGui::Checkbox("No Sun", &gCompilerMode.LC_NoSun);
		ImGui::EndDisabled();
		ImGui::EndChild();
	}
	
}

void DrawAIConfig()
{
	if (ImGui::BeginChild("AI", { 200, 370 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
	{
			ImGui::Checkbox("AI Compiler", &gCompilerMode.AI);
		
			ImGui::BeginDisabled(!gCompilerMode.AI);

			ImGui::Separator();

			ImGui::Checkbox("AI Compiler ai.level", &gCompilerMode.AI_BuildLevel);
		 
				ImGui::BeginDisabled(!gCompilerMode.AI_BuildLevel);

 				ImGui::Checkbox("Draft AI-Map", &gCompilerMode.AI_Draft);
				ImGui::Checkbox("Pure Covers", &gCompilerMode.AI_PureCovers);
				ImGui::Checkbox("Verify", &gCompilerMode.AI_Verify);
				ImGui::Checkbox("Verbose", &gCompilerMode.AI_Verbose);
 	  
				ImGui::EndDisabled();
		
			ImGui::Separator();

			ImGui::Checkbox("AI Compiler all.spawn", &gCompilerMode.AI_BuildSpawn);
				ImGui::BeginDisabled(!gCompilerMode.AI_BuildSpawn);
 				
 				ImGui::Checkbox("No Separator Check", &gCompilerMode.AI_NoSeparatorCheck);

				ImGui::Text("Name all.spawn :");
				ImGui::InputText("#1", gCompilerMode.AI_spawn_name, sizeof(gCompilerMode.AI_spawn_name));
				ImGui::Text("Name level start:");
				ImGui::InputText("#2", gCompilerMode.AI_StartActor, sizeof(gCompilerMode.AI_StartActor));
 
				ImGui::EndDisabled();

			ImGui::EndDisabled();
	}
	ImGui::EndChild();
}

extern bool SaveCForm;

void DrawCompilerConfig()
{
	if (ImGui::BeginChild("Settings", { 170, 370 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
	{
		ImGui::Checkbox("Silent mode", &gCompilerMode.Silent);
		ImGui::Checkbox("Use IntelEmbree", &gCompilerMode.Embree);
		ImGui::Checkbox("Clear temp files", &gCompilerMode.ClearTemp);
		ImGui::Checkbox("Save cform to obj", &SaveCForm);
		ImGui::EndChild();
	}
	
}
