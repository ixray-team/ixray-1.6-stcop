#include "xrCore.h"
#include "ConverterUI.h"

#include <imgui.h>

#include "../xrForms/cl_log.h"
#include <timeapi.h>
#include <algorithm>
#ifdef IXR_WINDOWS
#include <psapi.h>
#endif

using namespace CFormConverter;

const char* texture_formats[] = 
{
	"RGBA (No compression)",
	"BC7 (DX11 Only)",
	"BC5 (Original)"
};

//Ex: 25, 200, 50, 255 -> 0.0980392, 0.784314, 0.196078, 1
#define RGBAColor(r,g,b,a) r/(float)255, g/(float)255, b/(float)255, a/(float)255

size_t GetHeapMemory()
{
#ifdef IXR_WINDOWS
	PROCESS_MEMORY_COUNTERS_EX pmc;
	if (GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS*)&pmc, sizeof(pmc)))
	{
		return pmc.PrivateUsage;
	}
#endif

	return 0;
}

bool ShowMainUI = true;

void CFormConverter::InitializeUIData()
{
	string_path LevelsDir = {};
	FS.update_path(LevelsDir, "$game_levels$", "");

	for (const xr_path& Dir : std::filesystem::directory_iterator{ LevelsDir })
	{
		if (!std::filesystem::is_directory(Dir))
			continue;

		auto& LevelInfo = GetConverterSettings().Files.emplace_back();
		LevelInfo.Name = Dir.xfilename();

	}
}

int current_format = 0;
static bool autoScroll = true;
static bool hideLogSection = true;
static bool ResizeMaximal = false;

int item_current_cform = 0;
int item_current_geom = 0;

const char* cform_types[] = {
	magic_enum::enum_name<CFormVersions>(CFormVersions::Vanilla).data(),
	magic_enum::enum_name<CFormVersions>(CFormVersions::VanillaChunked).data()
};
constexpr int cform_types_num = sizeof(cform_types) / sizeof(cform_types[0]);

const char* geom_types[] = {
	magic_enum::enum_name<GeomVanillaType>(GeomVanillaType::Vanilla).data(),
	magic_enum::enum_name<GeomVanillaType>(GeomVanillaType::Chunked).data()
};
constexpr int geom_types_num = sizeof(geom_types) / sizeof(geom_types[0]);

void CFormConverter::RenderMainUI()
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


	if (Size[0] != 1200 || Size[1] != 675)
	{
		SDL_SetWindowSize(g_AppInfo.Window, 1200, 675);
	}
	

	if (ImGui::Begin("MainForm", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus))
	{
		ImVec2 ListBoxSize = { float(Size[0] - 20), float ( Size[1] - 115) };
		if (ImGui::BeginTable("##Levels", 4, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Borders | ImGuiTableFlags_ScrollY, ListBoxSize))
		{
			// 
			ImGui::TableSetupColumn("Levels");
			ImGui::TableSetupColumn("Geom");
			ImGui::TableSetupColumn("CForm");
			ImGui::TableSetupColumn("Spawn");

			ImGui::TableHeadersRow();

			ImGui::TableNextRow();
			ImGui::TableNextColumn();
			
			ImVec2 ListBoxSize2 = { 200, float(Size[1] - 155) };
			if (  ImGui::BeginTable("##Levels", 2, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Borders | ImGuiTableFlags_ScrollY, ListBoxSize2)  )
			{
				ImGui::TableSetupColumn("Name");
				ImGui::TableSetupColumn("Prop");
				ImGui::TableHeadersRow();

				size_t Iter = 1;
				for (auto& [File, Selected] : GetConverterSettings().Files)
				{
					ImGui::TableNextColumn();
					xr_string U8Str = Platform::ANSI_TO_UTF8(File);
					ImGui::Selectable(U8Str.c_str());

					ImGui::TableNextColumn();
					ImGui::Checkbox(("##check" + File).c_str(), &Selected);
					Iter++;

					if (Iter < GetConverterSettings().Files.size())
					{
						ImGui::TableNextRow();
					}
				}
				ImGui::EndTable();
			}

			ImGui::TableNextColumn();
			
			{
				ImGui::PushID("Geom");
				ImGui::Checkbox("Geom Converter", &GetConverterSettings().Geom);
				ImGui::Separator();
				ImGui::Text("Geom format:");
				if (ImGui::Combo("##geom", &item_current_geom, geom_types, geom_types_num))
				{
					auto type = magic_enum::enum_cast<GeomVanillaType>(geom_types[item_current_geom]);
					VERIFY(type.has_value());
					GetConverterSettings().LC_GeomType = type.value();
				}
		
				ImGui::BeginDisabled(GetConverterSettings().LC_GeomType != GeomVanillaType::Chunked);
				ImGui::InputInt("Chunk size (MB)", &GetConverterSettings().LC_GeomChunkSize);
				GetConverterSettings().LC_GeomChunkSize = std::max(GetConverterSettings().LC_GeomChunkSize, 1);
				ImGui::EndDisabled();
				ImGui::PopID();
			}

			ImGui::TableNextColumn();
			
			{
				ImGui::PushID("CForm");
				ImGui::Checkbox("CForm Converter", &GetConverterSettings().CForm);
				ImGui::Separator();
				ImGui::Text("CForm format:");
				if (ImGui::Combo("##cform", &item_current_cform, cform_types, cform_types_num))
				{
					auto type = magic_enum::enum_cast<CFormVersions>(cform_types[item_current_cform]);
					VERIFY(type.has_value());
					GetConverterSettings().LC_CformType = type.value();
				}
		
				ImGui::BeginDisabled(GetConverterSettings().LC_CformType != CFormVersions::VanillaChunked);
				ImGui::InputInt("Chunk size (MB)", &GetConverterSettings().LC_CFormChunkSize);
				GetConverterSettings().LC_CFormChunkSize = std::max(GetConverterSettings().LC_CFormChunkSize, 1);
				ImGui::EndDisabled();
				ImGui::PopID();
			}
			
			ImGui::TableNextColumn();
			
			{
				ImGui::PushID("Spawn");
				ImGui::Checkbox("Spawn Converter", &GetConverterSettings().Spawn);
				ImGui::Separator();
				ImGui::InputText("Spawn from", GetConverterSettings().SpawnOrig.data(), GetConverterSettings().SpawnOrig.Length);
				ImGui::InputText("Spawn to", GetConverterSettings().SpawnDest.data(), GetConverterSettings().SpawnDest.Length);
				ImGui::PopID();
			}

			ImGui::EndTable();  
		}
	}
	 
	auto BSize = ImGui::GetContentRegionAvail();

	if (ImGui::Button("Run Converter", { BSize.x, 50 }))
	{
		for (auto& FILE : GetConverterSettings().Files)
		{
			if (FILE.Select)
			{
				Msg("Level For Building : %s", FILE.Name.c_str());
				break;
			}
		}

		ShowMainUI = false;
		StartCompile();
	}
	 
	if (true)
	{
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
		ImGui::TextColored(ImVec4{ 0, 0.9, 0, 1 }, "Memory: %u mb", GetHeapMemory() / 1024 / 1024);
		ImGui::SameLine();
		ImGui::Checkbox("ShowMain", &ShowMainUI);
	}
 
	ImGui::End();
}

void getStatusInfo(IterationStatus status, xr_string& text, ImVec4& textCol, char& icon)
{
	switch (status)
	{
	case Complete:
		text = "Complete";
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

const ImVec4 getLogColor(char* text)
{
	if (text == nullptr || xr_strlen(text) == 0)
		return ImVec4(RGBAColor(230, 230, 230, 255));

	xr_string TextEx = text;
	TextEx = TextEx.RemoveWhitespaces();
	size_t Pos = TextEx.find('|');

	while (Pos != xr_string::npos)
	{
		TextEx.erase(Pos, 1);
		Pos = TextEx.find('|');
	}

	char Word = TextEx[0];

	switch (Word)
	{
	case '~': return ImVec4(RGBAColor(248, 248, 49, 255));
	case '!': return ImVec4(RGBAColor(204, 102, 102, 255));
	case '@': return ImVec4(RGBAColor(125, 125, 241, 255));
	case '#': return ImVec4(RGBAColor(0, 222, 205, 155));
	case '%': return ImVec4(RGBAColor(202, 85, 219, 155));
	case '$': return ImVec4(RGBAColor(172, 172, 255, 255));
	case '*': return ImVec4(RGBAColor(248, 248, 49, 255));
	case '^': return ImVec4(RGBAColor(100, 246, 121, 255));
	case '&': return ImVec4(RGBAColor(255, 255, 0, 255));
	case '-': return ImVec4(RGBAColor(0, 255, 0, 255));
	case '+': return ImVec4(RGBAColor(84, 255, 255, 255));
	case '=': return ImVec4(RGBAColor(205, 205, 105, 255));
	case '/': return ImVec4(RGBAColor(146, 146, 252, 255));
	}

	return ImVec4(RGBAColor(230, 230, 230, 255));
}

void CFormConverter::RenderCompilerUI(int X, int Y)
{
	//static const char* levelName = "LevelTextName";

	// Set up the window
	ImGui::Begin("Compile Split Screen", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus);

	// Calculate sizes for the top and bottom parts
	ImVec2 windowSize = ImGui::GetContentRegionAvail();
	float topHeight = hideLogSection ? windowSize.y - 58.f : windowSize.y * 0.5f;

	// Top section
	ImGui::BeginChild("TopSection", ImVec2(windowSize.x, topHeight), true);

	// Level name
	xr_string Levels;

	for (auto& [Name, Selected] : GetConverterSettings().Files)
	{
		if (Selected)
			Levels += (!Levels.empty() ? ", " : "") + Name;
	}
	ImGui::Text("%s", Levels.c_str());
	ImGui::Separator();

	ImVec4 phaseTextCol = { 78, 178, 98, 0.78 };

	int MAX_TRABS = 9;
	if (ResizeMaximal)
		MAX_TRABS = 7;

	auto RenderRowIcon = [](IterationStatus& status, int Index)
	{
 		xr_string rowStatus;
		ImVec4 rowStatusColor;
		char rowIcon;
		getStatusInfo(status, rowStatus, rowStatusColor, rowIcon);
 		ImGui::TableSetColumnIndex(Index);
		ImGui::PushFont(GetConverterSettings().CompilerIconsFont);
		ImGui::TextColored(rowStatusColor, "%c", rowIcon);
		ImGui::PopFont();
	};

	auto GetPhaseTime = [](IterationPhase& phase)
	{
 		if (phase.status != Complete)
		{
			u32 dwCurrentTime = timeGetTime();
			u32 dwTimeDiff = dwCurrentTime - GetPhaseStartTime();
			u32 secElapsed = dwTimeDiff / 1000;
 			phase.elapsed_time = secElapsed;
 		}
	};

	int Flags = ImGuiTableFlags_ScrollY | ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg | ImGuiTableFlags_Resizable | ImGuiTableFlags_SizingFixedSame;

 	if (ResizeMaximal && ImGui::BeginTable("DebugTable", MAX_TRABS, Flags))
	{
		ImGui::TableSetupColumn(" ", ImGuiTableColumnFlags_WidthFixed, 15.f);
		ImGui::TableSetupColumn("Task", ImGuiTableColumnFlags_WidthFixed, 15.f);
		ImGui::TableSetupColumn("Phase", ImGuiTableColumnFlags_WidthFixed, 250.0f);
		ImGui::TableSetupColumn("Phase %", ImGuiTableColumnFlags_WidthFixed, 40.f);
		ImGui::TableSetupColumn("Elapsed Time", ImGuiTableColumnFlags_WidthFixed, 80.0f);		
		ImGui::TableSetupColumn("Memory", ImGuiTableColumnFlags_WidthFixed, 100.f);
		ImGui::TableSetupColumn("Status Description", ImGuiTableColumnFlags_WidthStretch);

		ImGui::TableHeadersRow();

		for (auto& row : GetIterationData())
		{
			ImGui::TableNextRow();
 			RenderRowIcon(row.status, 0);

			// TASK
			ImGui::TableSetColumnIndex(1);
			ImGui::Text("%s", row.iterationName.c_str());

			ImGui::TableSetColumnIndex(3);
			ImGui::Text("%0.f", row.Persent * 100);

			for (auto& phase : row.phases)
			{
				GetPhaseTime(phase);

  				ImGui::TableNextRow();
				RenderRowIcon(phase.status, 1);
				 
 				ImGui::TableSetColumnIndex(2);
				ImGui::TextColored(phaseTextCol, phase.PhaseName.c_str());

				ImGui::TableSetColumnIndex(4);
				ImGui::TextColored(phaseTextCol, "%s", make_time(phase.elapsed_time).c_str());

				ImGui::TableSetColumnIndex(5);
				ImGui::Text("%u MB", u32(size_t(phase.used_memory / 1024 / 1024)));

				ImGui::TableSetColumnIndex(6);
				ImGui::Text("%s", phase.AdditionalData.c_str());
			}
		}

		if (autoScroll)
			ImGui::SetScrollY(ImGui::GetScrollMaxY());
		ImGui::EndTable();
	}

	// Table
 	if (!ResizeMaximal && ImGui::BeginTable("IterationsTable", MAX_TRABS, Flags))
	{
 		ImGui::TableSetupColumn(" ", ImGuiTableColumnFlags_WidthFixed, 15.0f);
		ImGui::TableSetupColumn("Task", ImGuiTableColumnFlags_WidthFixed, 50.f);
		ImGui::TableSetupColumn("Phase", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn("Phase %", ImGuiTableColumnFlags_WidthFixed, 50.f);
		ImGui::TableSetupColumn("Elapsed Time", ImGuiTableColumnFlags_WidthFixed, 80.0f);
		ImGui::TableSetupColumn("Remain Time", ImGuiTableColumnFlags_WidthFixed, 80.0f);
		ImGui::TableSetupColumn("Warnings", ImGuiTableColumnFlags_WidthFixed, 80.0f);
		ImGui::TableSetupColumn("Status", ImGuiTableColumnFlags_WidthFixed, 100.f);
		ImGui::TableSetupColumn("Memory", ImGuiTableColumnFlags_WidthFixed, 100.f);

		ImGui::TableHeadersRow();

		for (auto& row : GetIterationData())
		{
 			xr_string rowStatus;
			ImVec4 rowStatusColor;
 			char rowIcon;
 			getStatusInfo(row.status, rowStatus, rowStatusColor, rowIcon);
 			ImGui::TableNextRow();

			// Status icon

			ImGui::TableSetColumnIndex(0);
			ImGui::PushFont(GetConverterSettings().CompilerIconsFont);
			ImGui::TextColored(rowStatusColor, "%c", rowIcon);
			ImGui::PopFont();

			// TASK
			ImGui::TableSetColumnIndex(1);
			ImGui::Text("%s", row.iterationName.c_str());

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
				ImGui::PushFont(GetConverterSettings().CompilerIconsFont);
 				float column_width = ImGui::GetColumnWidth();
				float text_size = ImGui::CalcTextSize("A").x;
				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + column_width - text_size);
 				ImGui::TextColored(statusColor, "%c", phaseIcon);
 				ImGui::PopFont();

				ImGui::TableSetColumnIndex(2);
				ImGui::TextColored(phaseTextCol, phase.PhaseName.c_str());
				//PHASE %
				auto pers = phase.PhasePersent;

				if (phase.status != Complete)
				{
					u32 dwCurrentTime = timeGetTime();
					u32 dwTimeDiff = dwCurrentTime - GetPhaseStartTime();
					u32 secElapsed = dwTimeDiff / 1000;
					u32 secRemain = u32(float(secElapsed) / pers) - secElapsed;

					phase.elapsed_time = secElapsed;
					if (pers > 0.005f)
						phase.remain_time = secRemain;
				}

				//
				if (phase.status == Complete) pers = 1;
				else if (pers > 1.f)	pers = 1;
				else if (pers < 0.f)	pers = 0;
			 
				ImGui::TableSetColumnIndex(3);
				ImGui::TextColored(phaseTextCol, "%0.f", pers * 100);

				ImGui::TableSetColumnIndex(4);
				ImGui::TextColored(phaseTextCol, "%s", make_time(phase.elapsed_time).c_str());

				ImGui::TableSetColumnIndex(5);
				if (phase.status != Complete)
					ImGui::TextColored(phaseTextCol, "%s", (phase.remain_time == 0 ? "Calculating..." : make_time(phase.remain_time).c_str()));
 
				 
				ImGui::TableSetColumnIndex(7);

				ImGui::TextColored(statusColor, status.c_str());

				ImGui::TableSetColumnIndex(8);
				ImGui::Text("%u MB", u32(size_t(phase.used_memory / 1024 / 1024)));
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
				ImGui::TextColored(getLogColor((char*)line.c_str()), "%s", Platform::UTF8_to_CP1251(line.c_str()).c_str() );
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

	ImGui::TextColored( ImVec4{ 0, 0.9, 0, 1 }, "Memory usage: %u mb", GetHeapMemory() / 1024 / 1024);

	ImGui::SameLine();
	ImGui::Checkbox("Show Main", &ShowMainUI);

	ImGui::End();
}