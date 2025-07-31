#include <imgui.h>

#include "../../xrCore/xrCore.h"
#include "CompilersUI.h"
#include "cl_log.h"
#include <timeapi.h>

//Ex: 25, 200, 50, 255 -> 0.0980392, 0.784314, 0.196078, 1
#define RGBAColor(r,g,b,a) r/(float)255, g/(float)255, b/(float)255, a/(float)255

bool ShowMainUI = true;

extern CompilersMode gCompilerMode;

void InitializeUIData()
{
	string_path LevelsDir = {};
	FS.update_path(LevelsDir, "$game_levels$", "");

	for (const xr_path& Dir : std::filesystem::directory_iterator{ LevelsDir })
	{
		if (!std::filesystem::is_directory(Dir))
			continue;

		auto& LevelInfo = gCompilerMode.Files.emplace_back();
		LevelInfo.Name = Dir.xfilename();

	}
}

void DrawCompilerConfig();
void DrawAIConfig();
void DrawDOConfig();
void DrawLCConfig();

void RenderMainUI()
{
	int Size[2] = {};
 	SDL_GetWindowSize(g_AppInfo.Window, &Size[0], &Size[1]);

	ImGui::SetNextWindowPos({ 0, 0 });
	ImGui::SetNextWindowSize({ (float)Size[0], (float)Size[1] });

	if (!ShowMainUI) 
	{
		RenderCompilerUI(Size[0], Size[1]);
		return;
	}

	if ( Size[0] != 1000 || Size[1] != 560 )
	{
		SDL_SetWindowSize(g_AppInfo.Window, 1000, 560);
	}

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
				for (auto& [File, Selected] : gCompilerMode.Files)
				{
					ImGui::TableNextColumn();
					xr_string U8Str = Platform::ANSI_TO_UTF8(File);
					ImGui::Selectable(U8Str.c_str());

					ImGui::TableNextColumn();
					ImGui::Checkbox(("##check" + File).c_str(), &Selected);
					Iter++;

					if (Iter < gCompilerMode.Files.size())
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
		if (gCompilerMode.LC || gCompilerMode.DO)
		{
			isReady = true;
		}

		if (gCompilerMode.AI)
		{

			if (gCompilerMode.AI_BuildLevel)
			{
				isReady = true;
			}

			if (gCompilerMode.AI_BuildSpawn)
			{
				isReady = true;
			}
		}

		if (isReady)
		{
			for (auto& FILE : gCompilerMode.Files)
			{
				if (FILE.Select)
				{
					Msg("Level For Building : %s", FILE.Name);
					break;
				}
			}

			ShowMainUI = false;
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
	if (ImGui::BeginChild("LC", { 200, 390 }, ImGuiChildFlags_Border, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings))
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
 			ImGui::SetNextItemWidth(100);
			ImGui::InputFloat("Dist Weld", &gCompilerMode.MergeDistance);

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

void getStatusInfo(IterationStatus status, xr_string& text, ImVec4& textCol, char& icon)
{
	switch (status)
	{
	case Complited:
		text = "Complited";
		textCol = { 0, 0.9, 0, 1 };

		icon = 'C';
		break;
	case InProgress:
		text = "In Progress";
		textCol = { 0.9, 0.9, 0, 1 };

		icon = 'B';
		break;
	case Pending:
		text = "Pending";
		textCol = { 0.8, 0.8, 0.8, 0.8 };

		icon = 'A';
		break;
	case Skip:
		text = "Skip";
		textCol = { 0.9, 0.9, 0.9, 0.6 };

		icon = 'D';
		break;
	default:
		text = "";
		textCol = { 1,1,1,1 };

		icon = 'A';
		break;
	}
}

const ImVec4 getLogColor(const char& c)
{
	switch (c)
	{
	case '~': return ImVec4(RGBAColor(248, 248,  49, 255));
	case '!': return ImVec4(RGBAColor(204, 102, 102, 255));
	case '@': return ImVec4(RGBAColor(125, 125, 241, 255));
	case '#': return ImVec4(RGBAColor(  0, 222, 205, 155));
	case '$': return ImVec4(RGBAColor(172, 172, 255, 255));
	case '%': return ImVec4(RGBAColor(202,  85, 219, 155));
	case '^': return ImVec4(RGBAColor(100, 246, 121, 255));
	case '&': return ImVec4(RGBAColor(255, 255,   0, 255));
	case '*': return ImVec4(RGBAColor(187, 187, 187, 255));
	case '-': return ImVec4(RGBAColor(  0, 255,   0, 255));
	case '+': return ImVec4(RGBAColor( 84, 255, 255, 255));
	case '=': return ImVec4(RGBAColor(205, 205, 105, 255));
	case '/': return ImVec4(RGBAColor(146, 146, 252, 255));
	default: return ImVec4(RGBAColor(230, 230, 230, 255));
	}
}

void RenderCompilerUI(int X, int Y)
{
	//static const char* levelName = "LevelTextName";
	static bool autoScroll = true;
	static bool hideLogSection = false;
	static bool ResizeMaximal = false;

	// Set up the window
	ImGui::Begin("Compile Split Screen", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus);

	// Calculate sizes for the top and bottom parts
	ImVec2 windowSize = ImGui::GetContentRegionAvail();
	float topHeight = hideLogSection ? windowSize.y - 58.f : windowSize.y * 0.5f;

	// Top section
	ImGui::BeginChild("TopSection", ImVec2(windowSize.x, topHeight), true);

	// Level name
	xr_string Levels;

	for (auto& [Name, Selected] : gCompilerMode.Files)
	{
		if (Selected)
			Levels += (!Levels.empty() ? ", " : "") + Name;
	}
	ImGui::Text("%s", Levels.c_str());
	ImGui::Separator();

	ImVec4 phaseTextCol = { 78, 178, 98, 0.78 };


	if (ResizeMaximal)
	{
		if (X != 1250 || Y != 800)
		{
			SDL_SetWindowSize(g_AppInfo.Window, 1250, 800);
		}
	}
	else
	{
		if (X != 1000 || Y != 560)
		{
			SDL_SetWindowSize(g_AppInfo.Window, 1000, 560);
		}
	}
	


		// Table
	if (ImGui::BeginTable("IterationsTable", 10, ImGuiTableFlags_ScrollY | ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg | ImGuiTableFlags_Resizable)) {
		ImGui::TableSetupColumn(" ", ImGuiTableColumnFlags_WidthFixed, 15.0f);
		ImGui::TableSetupColumn("Task", ImGuiTableColumnFlags_WidthFixed, 15.f);
		ImGui::TableSetupColumn("Phase", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn("Phase %", ImGuiTableColumnFlags_WidthFixed, 50.f);
		ImGui::TableSetupColumn("Elapsed Time", ImGuiTableColumnFlags_WidthFixed, 80.0f);
		ImGui::TableSetupColumn("Remain Time", ImGuiTableColumnFlags_WidthFixed, 80.0f);
		ImGui::TableSetupColumn("Warnings", ImGuiTableColumnFlags_WidthFixed, 80.0f);
		ImGui::TableSetupColumn("Status", ImGuiTableColumnFlags_WidthFixed, 100.f);
		ImGui::TableSetupColumn("Memory", ImGuiTableColumnFlags_WidthFixed, 100.f);
		ImGui::TableSetupColumn("Status Description", ImGuiTableColumnFlags_WidthFixed, 300.f);

		ImGui::TableHeadersRow();


		for (auto& row : GetIterationData()) {

			xr_string rowStatus;
			ImVec4 rowStatusColor;

			char rowIcon;

			getStatusInfo(row.status, rowStatus, rowStatusColor, rowIcon);

			ImGui::TableNextRow();

			// Status icon

			ImGui::TableSetColumnIndex(0);
			ImGui::PushFont(gCompilerMode.CompilerIconsFont);
			ImGui::TextColored(rowStatusColor, "%c", rowIcon);
			ImGui::PopFont();

			// TASK
			ImGui::TableSetColumnIndex(1);
			ImGui::Text("%s", row.iterationName.c_str());

			// 
			ImGui::TableSetColumnIndex(3);
			ImGui::Text("%0.f", row.Persent * 100);

			ImGui::TableSetColumnIndex(6);
			ImGui::Text("%d", row.warnings);
			// Status text
			ImGui::TableSetColumnIndex(7);


			ImGui::TextColored(rowStatusColor, rowStatus.c_str());

			for (auto& phase : row.phases)
			{
				xr_string status;
				ImVec4 statusColor;
				char phaseIcon;

				getStatusInfo(phase.status, status, statusColor, phaseIcon);

				ImGui::TableNextRow();

				ImGui::TableSetColumnIndex(1);
				ImGui::PushFont(gCompilerMode.CompilerIconsFont);

				float column_width = ImGui::GetColumnWidth();
				float text_size = ImGui::CalcTextSize("A").x;
				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + column_width - text_size);

				ImGui::TextColored(statusColor, "%c", phaseIcon);

				ImGui::PopFont();

				ImGui::TableSetColumnIndex(2);
				ImGui::TextColored(phaseTextCol, phase.PhaseName.c_str());
				//PHASE %
				auto pers = phase.PhasePersent;

				if (phase.status != Complited) {
					u32 dwCurrentTime = timeGetTime();
					u32 dwTimeDiff = dwCurrentTime - GetPhaseStartTime();
					u32 secElapsed = dwTimeDiff / 1000;
					u32 secRemain = u32(float(secElapsed) / pers) - secElapsed;

					phase.elapsed_time = secElapsed;
					if (pers > 0.005f)
						phase.remain_time = secRemain;
				}

				//
				if (phase.status == Complited) pers = 1;
				else if (pers > 1.f)	pers = 1;
				else if (pers < 0.f)	pers = 0;

				ImGui::TableSetColumnIndex(3);
				ImGui::TextColored(phaseTextCol, "%0.f", pers * 100);

				ImGui::TableSetColumnIndex(4);
				ImGui::TextColored(phaseTextCol, "%s", make_time(phase.elapsed_time).c_str());

				ImGui::TableSetColumnIndex(5);
				if (phase.status != Complited)
					ImGui::TextColored(phaseTextCol, "%s", (phase.remain_time == 0 ? "Calculating..." : make_time(phase.remain_time).c_str()));

				ImGui::TableSetColumnIndex(7);

				ImGui::TextColored(statusColor, status.c_str());

				ImGui::TableSetColumnIndex(8);
				ImGui::Text("%u MB", u32 ( size_t( phase.used_memory/ 1024/ 1024) ) );

				ImGui::TableSetColumnIndex(9);
				ImGui::Text("%s", phase.AdditionalData.c_str() );
			}
		}

		if (autoScroll)
			ImGui::SetScrollY(ImGui::GetScrollMaxY());
		ImGui::EndTable();
	}

	ImGui::EndChild();
	 

	ImGui::Separator();
	ImGui::Text("Log");

	ImGui::SameLine();

	const char* buttonText = (hideLogSection) ? "+" : "-";
	ImVec2 textSize = ImGui::CalcTextSize(buttonText);

	ImVec2 buttonSize = ImVec2(textSize.x + ImGui::GetStyle().FramePadding.x * 2,
		textSize.y + ImGui::GetStyle().FramePadding.y * 2);

	auto ZSize = ImGui::GetContentRegionAvail();

	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + ZSize.x - buttonSize.x);

	if (ImGui::Button(buttonText))
		hideLogSection = !hideLogSection;

	if (!hideLogSection && ImGui::BeginChild("LogSection", ImVec2(windowSize.x, windowSize.y - topHeight - (buttonSize.y * 2)-30), true))
	{
		ImGuiListClipper clipper;

		xrCriticalSectionGuard LogGuard(&csLog);

		clipper.Begin(GetLogVector().size());

		while (clipper.Step())
		{
			for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; ++i)
			{
				auto& line = GetLogVector()[i];
				char cLine = (line.size() > 0 ? line[0] : ' ');
				ImGui::TextColored(getLogColor(cLine), "%s", line.c_str());
			}
		}


		if (autoScroll)
			ImGui::SetScrollY(ImGui::GetScrollMaxY());

		ImGui::EndChild();
	}

	ImGui::Separator();

	if (ImGui::Button(autoScroll ? "Disable Auto-Scroll" : "Enable Auto-Scroll"))
	{
		autoScroll = !autoScroll;
	}

	ImGui::SameLine();

	if (ImGui::Button(!ResizeMaximal ? "Maximal resize" : "Minimal resize"))
	{
		ResizeMaximal = !ResizeMaximal;
	}

	ImGui::SameLine();

	size_t  w_free, w_reserved, w_committed;
	vminfo(&w_free, &w_reserved, &w_committed);
 
	ImGui::TextColored( ImVec4{ 0, 0.9, 0, 1 }, "Memory: %u mb", w_committed / 1024 / 1024);

	ImGui::End();
}