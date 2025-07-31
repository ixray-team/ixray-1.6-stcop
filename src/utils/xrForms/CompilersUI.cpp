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
	ImGui::SetNextWindowSize({ (float)Size[0], (float)Size[1] });
	if (ImGui::Begin("MainForm"), nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus)
	{
		DrawCompilerConfig();
		DrawLCConfig();
		DrawAIConfig();
		DrawDOConfig();

		ImGui::SetCursorPosY(ImGui::GetWindowSize().y - 355);
		ImGui::Text("Levels:");
		ImVec2 ListBoxSize = { 220, 330};
		if (ImGui::BeginTable("##Levels", 2, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Borders | ImGuiTableFlags_ScrollY, ListBoxSize))
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
	}

	ImGui::End();
}

void DrawLCConfig()
{
	if (ImGui::BeginChild("LC", { 200, 175 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
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
		ImGui::EndDisabled();

	}
	ImGui::EndChild();
	ImGui::SameLine();
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

	}
	ImGui::EndChild();
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

	}
	ImGui::EndChild();
	ImGui::SameLine();
}


void DrawCompilerConfig()
{
	if (ImGui::BeginChild("Settings", { 200, 80 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
	{
		ImGui::Checkbox("Silent mode", &gCompilerMode.Silent);
		ImGui::Checkbox("Use IntelEmbree", &gCompilerMode.Embree);
		ImGui::Checkbox("Clear temp files", &gCompilerMode.ClearTemp);

	}
	ImGui::EndChild();
	ImGui::SameLine();
}
