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
		 
		ImGui::Text("Levels:");

		ImVec2 ListBoxSize = { float(Size[0] - 20), float ( Size[1] - 100) };
		if (ImGui::BeginTable("##Levels", 5, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Borders | ImGuiTableFlags_ScrollY, ListBoxSize))
		{
			// 
			ImGui::TableSetupColumn("Levels");
			ImGui::TableSetupColumn("Settings");
			ImGui::TableSetupColumn("xrLC");
			ImGui::TableSetupColumn("xrDO");
			ImGui::TableSetupColumn("xrAI");

			ImGui::TableHeadersRow();

			ImGui::TableNextRow();
			ImGui::TableSetColumnIndex(0);
			
			ImVec2 ListBoxSize2 = { 168, float(Size[1] - 175) };
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

			auto BSize = ImGui::GetContentRegionAvail();

			if (ImGui::Button("Run Compiler", { BSize.x, 50 }))
			{
				for (auto& FILE : Files)
				{
					if (FILE.Select)
					{
						strcpy(gCompilerMode.level_name, FILE.Name.c_str());
						break;
					}
				}


				extern void StartCompile();
				StartCompile();

				Msg("Level For Building : %s", gCompilerMode.level_name);
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
	ImGui::End();
}

int item_current_selected = 0;
int item_current_jitter = 0;
int item_current_jitter_mu = 0;

const char* items[] = { "1024", "2048", "4096", "8192" };
const char* itemsJitter[] = { "1", "4", "9" };
const char* itemsJitterMU[] = { "1", "2", "3", "4", "5", "6"};

void DrawLCConfig()
{
	if (ImGui::BeginChild("LC", { 200, 350 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
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
		
 
 		ImGui::Combo("lmaps", &item_current_selected, items, 4);
		ImGui::Combo("JitterMU", &item_current_jitter_mu, itemsJitterMU, 6);
		ImGui::Combo("Jitter", &item_current_jitter, itemsJitter, 3);

		ImGui::InputFloat("Pixels", &gCompilerMode.LC_Pixels);

		gCompilerMode.LC_sizeLmaps = atoi(items[item_current_selected]);
		gCompilerMode.LC_JSample   = atoi(itemsJitter[item_current_jitter]);
		gCompilerMode.LC_JSampleMU = atoi(itemsJitter[item_current_jitter_mu]);
 
		ImGui::EndDisabled();
		
		
		ImGui::EndChild();
	}


}

void DrawDOConfig()
{
	if (ImGui::BeginChild("DO", { 200, 60 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
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
	if (ImGui::BeginChild("AI", { 200, 130 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
	{
		ImGui::Checkbox("AI Compiler", &gCompilerMode.AI);
		ImGui::Separator();

		ImGui::BeginDisabled(!gCompilerMode.AI);
		ImGui::Checkbox("Build level", &gCompilerMode.AI_BuildLevel);
		ImGui::Checkbox("Build global spawn", &gCompilerMode.AI_Spawn);
		ImGui::Checkbox("No Separator Check", &gCompilerMode.AI_NoSeparatorCheck);
		ImGui::Checkbox("Draft AI-Map", &gCompilerMode.AI_Draft);


		ImGui::EndDisabled();
		ImGui::EndChild();
	}

}


void DrawCompilerConfig()
{
	if (ImGui::BeginChild("Settings", { 150, 80 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
	{
		ImGui::Checkbox("Silent mode", &gCompilerMode.Silent);
		ImGui::Checkbox("Use IntelEmbree", &gCompilerMode.Embree);
		ImGui::Checkbox("Clear temp files", &gCompilerMode.ClearTemp);
		ImGui::EndChild();
	}
	
}
