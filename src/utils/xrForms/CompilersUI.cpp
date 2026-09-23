#include <imgui.h>

#include "../../xrCore/xrCore.h"
#include "CompilersUI.h"
#include "cl_log.h"
#include <timeapi.h>
#include <algorithm>

#include "xrLevel.h"

const char* texture_formats[] =
	{
		"RGBA (No compression)",
		"BC7 (DX11 Only)",
		"BC5 (Original)"
};

// Ex: 25, 200, 50, 255 -> 0.0980392, 0.784314, 0.196078, 1
#define RGBAColor(r, g, b, a) r / (float)255, g / (float)255, b / (float)255, a / (float)255
extern size_t GetHeapMemory();

bool ShowMainUI = true;
bool ShowLightPreview = false;

extern CompilersMode gCompilerMode;

static xrCriticalSection LightPreviewCS;
static xr_vector<Fvector> LightPreviewVertices;
static xr_vector<u32> LightPreviewIndices;
static Fvector LightPreviewCenter;
static float LightPreviewRadius = 1.f;
static u32 LightPreviewSceneGeneration = 0;
static xr_vector<u8> LightPreviewColors;
static u32 LightPreviewColorGeneration = 0;
static xr_vector<float> LightPreviewUv;
static xr_vector<u32> LightPreviewLayerId;
static xr_vector<LightPreviewMap> LightPreviewMaps;
static u32 LightPreviewMapGeneration = 0;

struct LightPreviewLock
{
	LightPreviewLock()
	{
		LightPreviewCS.Enter();
	}

	~LightPreviewLock()
	{
		LightPreviewCS.Leave();
	}
};

static bool HasNewGeneration(u32 Current, u32 Known)
{
	return Current != 0 && Current != Known;
}

void PublishLightPreviewScene(const xr_vector<Fvector>& Vertices, const xr_vector<u32>& Indices, const Fvector& Center, float Radius)
{
	LightPreviewLock Lock;
	LightPreviewVertices = Vertices;
	LightPreviewIndices = Indices;
	LightPreviewCenter = Center;
	LightPreviewRadius = Radius > 1.f ? Radius : 1.f;
	LightPreviewSceneGeneration++;
}

bool TakeLightPreviewScene(u32 KnownGeneration, u32& Generation, xr_vector<Fvector>& Vertices, xr_vector<u32>& Indices, Fvector& Center, float& Radius)
{
	LightPreviewLock Lock;
	if (!HasNewGeneration(LightPreviewSceneGeneration, KnownGeneration))
	{
		return false;
	}

	Vertices = LightPreviewVertices;
	Indices = LightPreviewIndices;
	Center = LightPreviewCenter;
	Radius = LightPreviewRadius;
	Generation = LightPreviewSceneGeneration;
	return true;
}

void PublishLightPreviewColors(const xr_vector<u8>& CornerRgb)
{
	LightPreviewLock Lock;
	LightPreviewColors = CornerRgb;
	LightPreviewColorGeneration++;
}

bool TakeLightPreviewColors(u32 KnownGeneration, u32& Generation, xr_vector<u8>& CornerRgb)
{
	LightPreviewLock Lock;
	if (!HasNewGeneration(LightPreviewColorGeneration, KnownGeneration))
	{
		return false;
	}

	CornerRgb = LightPreviewColors;
	Generation = LightPreviewColorGeneration;
	return true;
}

void PublishLightPreviewMaps(const xr_vector<float>& Uv, const xr_vector<u32>& Layers, const xr_vector<LightPreviewMap>& Maps)
{
	LightPreviewLock Lock;
	LightPreviewUv = Uv;
	LightPreviewLayerId = Layers;
	LightPreviewMaps = Maps;
	LightPreviewMapGeneration++;
}

bool TakeLightPreviewMaps(u32 KnownGeneration, u32& Generation, xr_vector<float>& Uv, xr_vector<u32>& Layers, xr_vector<LightPreviewMap>& Maps)
{
	LightPreviewLock Lock;
	if (!HasNewGeneration(LightPreviewMapGeneration, KnownGeneration))
	{
		return false;
	}

	Uv = LightPreviewUv;
	Layers = LightPreviewLayerId;
	Maps = LightPreviewMaps;
	Generation = LightPreviewMapGeneration;
	return true;
}

void InitializeUIData()
{
	string_path LevelsDir = {};
	FS.update_path(LevelsDir, "$game_levels$", "");

	for (const xr_path& Dir : std::filesystem::directory_iterator{LevelsDir})
	{
		if (!std::filesystem::is_directory(Dir))
		{
			continue;
		}

		auto& LevelInfo = gCompilerMode.Files.emplace_back();
		LevelInfo.Name = Dir.xfilename();
	}
}

int current_format = 0;
static bool autoScroll = true;
static bool hideLogSection = false;

void DrawCompilerConfig();
void DrawAIConfig();
void DrawDOConfig();
void DrawLCConfig();

void DrawDownUI()
{
	ImGui::Separator();

	ImGui::Checkbox("SwitchUI", &ShowMainUI);
	ImGui::SameLine();
	ImGui::Checkbox("Preview", &ShowLightPreview);
	ImGui::SameLine();
	ImGui::Checkbox("auto-scroll", &autoScroll);
	ImGui::SameLine();
	ImGui::SameLine();
	ImGui::TextColored(ImVec4{0, 0.9, 0, 1}, "Memory: %u mb", GetHeapMemory() / 1024 / 1024);
}

void RenderMainUI()
{
	// Считаем выбранные уровни для счётчика в тулбаре.
	auto CountSelected = []() -> int
	{
		int n = 0;
		for (auto& [Name, Sel] : gCompilerMode.Files)
		{
			if (Sel)
			{
				++n;
			}
		}
		return n;
	};

	// Действия над списком уровней — используются и в кнопках, и в горячих клавишах.
	auto SelectAll = []()
	{
		for (auto& [Name, Sel] : gCompilerMode.Files)
		{
			Sel = true;
		}
	};

	auto SelectNone = []()
	{
		for (auto& [Name, Sel] : gCompilerMode.Files)
		{
			Sel = false;
		}
	};

	auto SelectInvert = []()
	{
		for (auto& [Name, Sel] : gCompilerMode.Files)
		{
			Sel = !Sel;
		}
	};

	int Size[2] = {};
	if (Size[0] != 1100 || Size[1] != 700)
	{
		SDL_SetWindowSize(g_AppInfo.Window, 1100, 700);
	}
	SDL_GetWindowSize(g_AppInfo.Window, &Size[0], &Size[1]);

	ImGui::SetNextWindowPos({0, 0});
	ImGui::SetNextWindowSize({(float)Size[0], (float)Size[1]});

	if (ShowLightPreview)
	{
		ImGui::SetNextWindowPos({0, 0});
		ImGui::SetNextWindowSize({(float)Size[0], (float)Size[1]});
		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(8, 8));
		if (ImGui::Begin("LightPreview", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus))
		{
			const float Bar = ImGui::GetFrameHeightWithSpacing() + 6.f;
			DrawLightPreview((float)Size[0] - 16.f, (float)Size[1] - 16.f - Bar);
			DrawDownUI();
		}
		ImGui::End();
		ImGui::PopStyleVar();
		return;
	}

	if (!ShowMainUI)
	{
		RenderCompilerUI(Size[0], Size[1]);
		return;
	}

	if (ImGui::Begin("MainForm", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus))
	{
		ImVec2 ListBoxSize = {float(Size[0] - 20), float(Size[1] - 115)};
		if (ImGui::BeginTable("##Levels", 5, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Borders, ListBoxSize))
		{
			// >>> FIX: фиксируем ширину первой колонки.
			//     Без этого счётчик "N / M" внутри тулбара растягивал
			//     ячейку до всей ширины окна и выдавливал остальные колонки вправо.
			ImGui::TableSetupColumn("Levels", ImGuiTableColumnFlags_WidthFixed, 260.f);
			ImGui::TableSetupColumn("Settings");
			ImGui::TableSetupColumn("xrLC");
			ImGui::TableSetupColumn("xrAI");
			ImGui::TableSetupColumn("xrDO");

			ImGui::TableHeadersRow();

			ImGui::TableNextRow();
			ImGui::TableNextColumn();

			// >>> SELECT-TOOLBAR
			{
				const float spacing = ImGui::GetStyle().ItemSpacing.x;

				if (ImGui::SmallButton("All"))
				{
					SelectAll();
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetTooltip("Select all levels (Ctrl+A)");
				}

				ImGui::SameLine(0.f, spacing);
				if (ImGui::SmallButton("None"))
				{
					SelectNone();
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetTooltip("Deselect all levels (Ctrl+D)");
				}

				ImGui::SameLine(0.f, spacing);
				if (ImGui::SmallButton("Invert"))
				{
					SelectInvert();
				}
				if (ImGui::IsItemHovered())
				{
					ImGui::SetTooltip("Invert selection (Ctrl+I)");
				}

				// Счётчик справа — теперь точно внутри 260px-колонки.
				{
					char counter[64];
					snprintf(counter, sizeof(counter), "%d / %d", CountSelected(), (int)gCompilerMode.Files.size());

					ImVec2 tsz = ImGui::CalcTextSize(counter);
					float avail = ImGui::GetContentRegionAvail().x;
					if (avail > tsz.x + 8.f)
					{
						ImGui::SameLine();
						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + avail - tsz.x);
						ImGui::TextColored(ImVec4(0.66f, 0.66f, 0.66f, 1.f), "%s", counter);
					}
				}
			}
			// <<< SELECT-TOOLBAR

			// Тулбар отъел по высоте — уменьшаем таблицу ровно на его размер.
			const float toolbarH = ImGui::GetFrameHeightWithSpacing();

			ImVec2 ListBoxSize2 = {250, float(Size[1] - 155) - toolbarH};
			if (ImGui::BeginTable("##LevelsList", 2, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_Borders | ImGuiTableFlags_ScrollY, ListBoxSize2))
			{
				ImGui::TableSetupColumn("Name", ImGuiTableColumnFlags_WidthFixed, 195);
				ImGui::TableSetupColumn("Prop");

				size_t Iter = 1;
				for (auto& [File, Selected] : gCompilerMode.Files)
				{
					ImGui::TableNextColumn();
					xr_string U8Str = Platform::ANSI_TO_UTF8(File);

					// Клик по имени — тоже переключает выбор.
					if (ImGui::Selectable(U8Str.c_str(), Selected))
					{
						Selected = !Selected;
					}

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

			ImGui::TableNextColumn();
			DrawCompilerConfig();

			ImGui::TableNextColumn();
			DrawLCConfig();

			ImGui::TableNextColumn();
			DrawAIConfig();

			ImGui::TableNextColumn();
			DrawDOConfig();

			ImGui::EndTable();
		}

		// >>> HOTKEYS: Ctrl+A / Ctrl+D / Ctrl+I
		if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows) && !ImGui::GetIO().WantTextInput)
		{
			const bool ctrl = ImGui::GetIO().KeyCtrl;

			if (ctrl && ImGui::IsKeyPressed(ImGuiKey_A, false))
			{
				SelectAll();
			}

			if (ctrl && ImGui::IsKeyPressed(ImGuiKey_D, false))
			{
				SelectNone();
			}

			if (ctrl && ImGui::IsKeyPressed(ImGuiKey_I, false))
			{
				SelectInvert();
			}
		}
		// <<< HOTKEYS
	}

	auto BSize = ImGui::GetContentRegionAvail();

	if (ImGui::Button("Run Compiler", {BSize.x, 50}))
	{
		GetIterationData().clear();

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
			// >>> FIX: убрано static — иначе второй запуск без выбранных
			//          уровней не показывал предупреждение.
			bool levelsEmpty = true;
			for (auto& FILE : gCompilerMode.Files)
			{
				if (FILE.Select)
				{
					levelsEmpty = false;
					Msg("Level For Building : %s", FILE.Name.c_str());
					break;
				}
			}

			extern void StartCompile();
			if (!levelsEmpty)
			{
				ShowMainUI = false;
				ClearLogVector();
				StartCompile();
			}
			else
			{
				SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_WARNING, "Warning!", "No levels selected.", nullptr);
			}
		}
	}

	DrawDownUI();
	ImGui::End();
}

int item_current_lightmap = 2;
int item_current_cform = 0;
int item_current_geom = 0;
int item_current_jitter = 2;
int item_current_jitter_mu = 6;

// Index Add (changed list)
int max_resolution = 5;
const char* lightmap_resolution[] = {"1024", "2048", "4096", "8192", "16384"};

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

// Jitters
const char* itemsJitter[] = {"1", "4", "9"};
const char* itemsJitterMU[] = {"0", "1", "2", "3", "4", "5", "6"};

void DrawLCConfig()
{
	{
		ImGui::PushID("xrLC");
		ImGui::Checkbox("Lighting Compiler", &gCompilerMode.LC);
		ImGui::SetItemTooltip("Compile Game Level (Collision, Lighting).");

		ImGui::Separator();

		ImGui::BeginDisabled(!gCompilerMode.LC);
		ImGui::Checkbox("No Smooth Group", &gCompilerMode.LC_NoSMG);
		ImGui::SetItemTooltip("Ignore smoothing groups.");

		ImGui::Checkbox("Tessellation", &gCompilerMode.LC_Tess);
		ImGui::SetItemTooltip("Geometric tessellation.");

		ImGui::Checkbox("Skip Invalid Faces", &gCompilerMode.LC_SkipInvalidFaces);
		ImGui::SetItemTooltip("Skip invalid faces.");

		ImGui::Checkbox("Skip Welding", &gCompilerMode.LC_skipWeld);
		ImGui::SetItemTooltip("Skip welding adjacent vertices.");

		ImGui::Separator();

		ImGui::Text("OGF Optimize: ");
		ImGui::Checkbox("Make TangentBasis", &gCompilerMode.LC_OGF_TANGENT);
		ImGui::Checkbox("Make Progressive", &gCompilerMode.LC_OGF_PROGRESSIVE);
		ImGui::Checkbox("Make Striptify", &gCompilerMode.LC_OGF_STRIPTIFY);
		ImGui::Separator();

		ImGui::PushID("geom");
		ImGui::Text("Geom format:");
		ImGui::SetNextItemWidth(180);
		if (ImGui::Combo("##geom", &item_current_geom, geom_types, geom_types_num))
		{
			auto type = magic_enum::enum_cast<GeomVanillaType>(geom_types[item_current_geom]);
			VERIFY(type.has_value());
			gCompilerMode.LC_GeomType = type.value();
		}

		ImGui::BeginDisabled(gCompilerMode.LC_GeomType != GeomVanillaType::Chunked);
		ImGui::SetNextItemWidth(100);
		ImGui::InputInt("Chunk size (MB)", &gCompilerMode.LC_GeomChunkSize);
		gCompilerMode.LC_GeomChunkSize = std::max(gCompilerMode.LC_GeomChunkSize, 1);
		ImGui::EndDisabled();
		ImGui::PopID();

		ImGui::Separator();

		ImGui::PushID("CForm");
		ImGui::Text("CForm format:");
		ImGui::SetNextItemWidth(180);
		if (ImGui::Combo("##cform", &item_current_cform, cform_types, cform_types_num))
		{
			auto type = magic_enum::enum_cast<CFormVersions>(cform_types[item_current_cform]);
			VERIFY(type.has_value());
			gCompilerMode.LC_CformType = type.value();
		}

		ImGui::BeginDisabled(gCompilerMode.LC_CformType != CFormVersions::VanillaChunked);
		ImGui::SetNextItemWidth(100);
		ImGui::InputInt("Chunk size (MB)", &gCompilerMode.LC_CFormChunkSize);
		gCompilerMode.LC_CFormChunkSize = std::max(gCompilerMode.LC_CFormChunkSize, 1);
		ImGui::EndDisabled();
		ImGui::PopID();

		ImGui::Separator();

		ImGui::Spacing();
		ImGui::TextColored(ImVec4(1.0f, 0.8f, 0.0f, 1.0f), "Lightmaps");
		ImGui::Spacing();

		ImGui::Text("Border:");
		ImGui::SameLine(0, 20);
		ImGui::InputInt("##LM-Border", &gCompilerMode.LC_BORDER, 1, 1);

		ImGui::Text("Size:");
		ImGui::SameLine(0, 35);
		ImGui::SetNextItemWidth(135);
		if (ImGui::Combo("##lmaps", &item_current_lightmap, lightmap_resolution, max_resolution))
		{
			gCompilerMode.LC_sizeLmaps = atoi(lightmap_resolution[item_current_lightmap]);
			ImGui::SetItemTooltip("Lightmap size.");
		}

		ImGui::Text("Format:");
		ImGui::SameLine(0, 20);
		ImGui::SetNextItemWidth(135);
		if (ImGui::Combo("##Texture Format", &current_format, texture_formats, IM_ARRAYSIZE(texture_formats)))
		{
			gCompilerMode.LmapsFormat = static_cast<LCLightmapFormat>(current_format);
		}

		ImGui::Checkbox("Fast LMaps", &gCompilerMode.LC_fast_way);
		ImGui::Checkbox("SoC LMaps", &gCompilerMode.LC_legacyLM);
		ImGui::Checkbox("Skip Static map", &gCompilerMode.LC_SkipStaticMap);
		ImGui::Checkbox("Skip Sun", &gCompilerMode.LC_NoSun);
		ImGui::SetItemTooltip("Disable sunlight calculation.");

		ImGui::EndDisabled();

		ImGui::PopID();
		// ImGui::EndChild();
	}
}

void DrawDOConfig()
{
	ImGui::PushID("xrDO");
	ImGui::Checkbox("Details Compiler", &gCompilerMode.DO);
	ImGui::SetItemTooltip("Compile Detail Objects.");

	ImGui::Separator();

	ImGui::BeginDisabled(!gCompilerMode.DO);
	ImGui::Checkbox("No Sun", &gCompilerMode.LC_NoSun);
	ImGui::SetItemTooltip("Disable sunlight calculation.");
	ImGui::EndDisabled();

	ImGui::PopID();
}

void DrawAIConfig()
{
	ImGui::Checkbox("AI Compiler", &gCompilerMode.AI);
	ImGui::SetItemTooltip("Compile AI-map.");

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

	ImGui::Checkbox("FreeMP Build", &gCompilerMode.AI_FreeMPBuild);

	ImGui::BeginDisabled(gCompilerMode.AI_FreeMPBuild);
	ImGui::Text("Name all.spawn :");
	ImGui::InputText("#1", gCompilerMode.AI_spawn_name, sizeof(gCompilerMode.AI_spawn_name));
	ImGui::Text("Name level start:");
	ImGui::InputText("#2", gCompilerMode.AI_StartActor, sizeof(gCompilerMode.AI_StartActor));
	ImGui::EndDisabled();

	ImGui::EndDisabled();
	ImGui::EndDisabled();
}

extern bool SaveCForm;

void DrawCompilerConfig()
{
	ImGui::Checkbox("Silent mode", &gCompilerMode.Silent);

	ImGui::PushID("LightPreset");
	{
		static int RadioID = -1;
		if (RadioID < 0)
		{
			RadioID = 0;
			RadioID += 1 * (int)gCompilerMode.Embree;
			RadioID += 2 * (int)gCompilerMode.CUDA;
		}

		ImGui::RadioButton("Use Intel Embree", &RadioID, 1);
		ImGui::SetItemTooltip("Use Intel Embree for ray tracing");
#ifdef LCCUDA_BUILD
		ImGui::RadioButton("Use Nvidia CUDA", &RadioID, 2);
#endif

		switch (RadioID)
		{
			case 1:
				gCompilerMode.CUDA = false;
				gCompilerMode.Embree = true;
				break;
			case 2:
				gCompilerMode.CUDA = true;
				gCompilerMode.Embree = false;
				break;
			default:
				break;
		}
	}
	ImGui::PopID();
	ImGui::Separator();


	ImGui::BeginDisabled(!gCompilerMode.Embree);
	ImGui::Checkbox("Embree Compacted", &gCompilerMode.EmbreeBVHCompact); // Замедляет скорость Траверсера
	ImGui::Checkbox("Embree Robust", &gCompilerMode.EmbreeBVHRobust);	  // Замедляет скорость Траверсера
	ImGui::Checkbox("Embree Instaces MU", &gCompilerMode.EmbreeInstaces); // Замедляет скорость Траверсера
	ImGui::Checkbox("Embree RayPack8", &gCompilerMode.EmbreeRays8);		  // x2 скорость Траверсера (AVX2)
	ImGui::SetItemTooltip("AVX2 Batch Ray Tracing. Speeds up BVH traversal by approximately a factor of 2.");

	ImGui::EndDisabled();

	ImGui::Separator();

	ImGui::Checkbox("Clear Temporary Files", &gCompilerMode.ClearTemp);
	ImGui::SetItemTooltip("Delete temporary files after compilation.");
	ImGui::Checkbox("Skip THM", &gCompilerMode.SkipTHM);
	ImGui::Checkbox("Save CForm to obj", &SaveCForm);

	ImGui::Separator();
	ImGui::Text("Threads Max");
	ImGui::SetItemTooltip("Maximum number of threads.");

	if (ImGui::InputInt("##Threads Max", &gCompilerMode.ThreadsPerWork))
	{
		gCompilerMode.ThreadsPerWork = std::min((u32)gCompilerMode.ThreadsPerWork, CPU::ID().n_threads);
	}

	ImGui::Separator();
	ImGui::Checkbox("Overload Prebuild", &gCompilerMode.IsOverloadedSettings);
	ImGui::SetItemTooltip("Overrides the settings specified in the Level Editor.");

	ImGui::BeginDisabled(!gCompilerMode.IsOverloadedSettings);

	ImGui::SetNextItemWidth(100);
	ImGui::Combo("JitterMU", &item_current_jitter_mu, itemsJitterMU, 7);

	ImGui::SetNextItemWidth(100);
	ImGui::Combo("Jitter", &item_current_jitter, itemsJitter, 3);

	ImGui::SetNextItemWidth(100);
	ImGui::InputFloat("Pixels", &gCompilerMode.LC_Pixels);
	ImGui::SetItemTooltip("Pixels per meter of the lightmap.");

	ImGui::SetNextItemWidth(100);
	ImGui::InputFloat("Welding Distance", &gCompilerMode.WeldDistance);
	ImGui::SetItemTooltip("Vertices welding distance.");

	gCompilerMode.LC_JSample = atoi(itemsJitter[item_current_jitter]);
	gCompilerMode.LC_JSampleMU = atoi(itemsJitterMU[item_current_jitter_mu]);

	ImGui::EndDisabled();
}

void getStatusInfo(IterationStatus status, xr_string& text, ImVec4& textCol, char& icon)
{
	switch (status)
	{
		case Complete:
			text = "Complete";
			textCol = {0.42f, 0.85f, 0.5f, 1.f};
			icon = 'C';
			break;
		case InProgress:
			text = "In Progress";
			textCol = {0.97f, 0.85f, 0.38f, 1.f};
			icon = 'B';
			break;
		case Pending:
			text = "Pending";
			textCol = {0.65f, 0.65f, 0.65f, 0.85f};
			icon = 'A';
			break;
		case Skip:
			text = "Skip";
			textCol = {0.55f, 0.55f, 0.55f, 0.6f};
			icon = 'D';
			break;
		default:
			text = "";
			textCol = {1, 1, 1, 1};
			icon = 'A';
			break;
	}
}

// =========================================================================
// >>> PRETTY: палитра лога + контекстные правила
// =========================================================================
const ImVec4 getLogColor(char* text)
{
	if (text == nullptr || xr_strlen(text) == 0)
	{
		return ImVec4(RGBAColor(230, 230, 230, 255));
	}

	xr_string TextEx = text;
	TextEx = TextEx.RemoveWhitespaces();
	size_t Pos = TextEx.find('|');

	while (Pos != xr_string::npos)
	{
		TextEx.erase(Pos, 1);
		Pos = TextEx.find('|');
	}

	if (TextEx.empty())
	{
		return ImVec4(RGBAColor(230, 230, 230, 255));
	}

	char Word = TextEx[0];

	switch (Word)
	{
		case '~':
			return ImVec4(RGBAColor(248, 248, 49, 255));
		case '!':
			return ImVec4(RGBAColor(235, 100, 100, 255));
		case '@':
			return ImVec4(RGBAColor(160, 160, 255, 255));
		case '#':
			return ImVec4(RGBAColor(0, 222, 205, 235));
		case '%':
			return ImVec4(RGBAColor(202, 85, 219, 235));
		case '$':
			return ImVec4(RGBAColor(172, 172, 255, 255));
		case '*':
			return ImVec4(RGBAColor(248, 216, 96, 255));
		case '^':
			return ImVec4(RGBAColor(100, 246, 121, 255));
		case '&':
			return ImVec4(RGBAColor(255, 255, 0, 255));
		case '-':
			return ImVec4(RGBAColor(120, 220, 120, 255));
		case '+':
			return ImVec4(RGBAColor(84, 255, 255, 255));
		case '=':
			return ImVec4(RGBAColor(205, 205, 105, 255));
		case '/':
			return ImVec4(RGBAColor(146, 146, 252, 255));
	}

	// Контекстная подсветка для строк без спец-префикса
	if (TextEx.find("New phase started") != xr_string::npos)
	{
		return ImVec4(RGBAColor(122, 168, 232, 255));
	}

	if (TextEx.find("Compilation is Ended") != xr_string::npos)
	{
		return ImVec4(RGBAColor(100, 246, 121, 255));
	}

	if (TextEx.find("Validate errors") != xr_string::npos)
	{
		return ImVec4(RGBAColor(248, 216, 96, 255));
	}

	return ImVec4(RGBAColor(230, 230, 230, 255));
}
// <<< PRETTY
// =========================================================================

void DrawGpuGraph(const float* values, int count, float maxValue = 100.0f)
{
	if (count == 0)
	{
		return;
	}

	ImVec2 size = ImVec2(500, 100);
	ImVec2 p = ImGui::GetCursorScreenPos();
	ImDrawList* draw = ImGui::GetWindowDrawList();

	// background
	draw->AddRectFilled(p, ImVec2(p.x + size.x, p.y + size.y), IM_COL32(20, 20, 20, 255));

	// grid
	for (int i = 0; i < 5; i++)
	{
		float y = p.y + (size.y / 4) * i;
		draw->AddLine(ImVec2(p.x, y), ImVec2(p.x + size.x, y), IM_COL32(50, 50, 50, 120));
	}

	// graph line
	float step = size.x / (float)(count - 1);
	for (int i = 1; i < count; i++)
	{
		float v0 = values[i - 1] / maxValue;
		float v1 = values[i] / maxValue;

		ImVec2 a = ImVec2(
			p.x + step * (i - 1),
			p.y + size.y - (v0 * size.y)
		);

		ImVec2 b = ImVec2(
			p.x + step * i,
			p.y + size.y - (v1 * size.y)
		);

		draw->AddLine(a, b, IM_COL32(0, 200, 255, 255), 2.0f);
	}

	// fill (like MSI Afterburner)
	for (int i = 1; i < count; i++)
	{
		float v0 = values[i - 1] / maxValue;
		float v1 = values[i] / maxValue;

		ImVec2 a = ImVec2(p.x + step * (i - 1), p.y + size.y);
		ImVec2 b = ImVec2(p.x + step * (i - 1), p.y + size.y - (v0 * size.y));
		ImVec2 c = ImVec2(p.x + step * i, p.y + size.y - (v1 * size.y));
		ImVec2 d = ImVec2(p.x + step * i, p.y + size.y);

		draw->AddQuadFilled(a, b, c, d, IM_COL32(0, 120, 255, 40));
	}

	ImGui::Dummy(size);
}


// =========================================================================
// >>> PRETTY: хелперы для отрисовки строк таблицы
// =========================================================================

namespace UITheme
{
const ImVec4 Text = ImVec4(RGBAColor(230, 230, 230, 255));
const ImVec4 TextDim = ImVec4(RGBAColor(170, 170, 170, 220));
const ImVec4 Accent = ImVec4(RGBAColor(239, 88, 88, 255));
const ImVec4 Green = ImVec4(RGBAColor(104, 210, 122, 255));
const ImVec4 Yellow = ImVec4(RGBAColor(248, 216, 96, 255));
const ImVec4 Red = ImVec4(RGBAColor(228, 96, 96, 255));
const ImVec4 Gray = ImVec4(RGBAColor(150, 150, 150, 200));
const ImVec4 RowBg = ImVec4(RGBAColor(30, 30, 34, 120));
const ImVec4 PhaseText = ImVec4(RGBAColor(200, 200, 210, 235));
} // namespace UITheme

// Маленькое цветное «поло» + текст — быстро сканируется взглядом
static void DrawStatusBadge(const char* status, const ImVec4& color)
{
	const float h = ImGui::GetTextLineHeight();
	ImVec2 p = ImGui::GetCursorScreenPos();

	ImGui::GetWindowDrawList()->AddCircleFilled(
		ImVec2(p.x + 5.f, p.y + h * 0.5f), 4.f, ImGui::ColorConvertFloat4ToU32(color), 16
	);

	ImGui::Dummy({14.f, h});
	ImGui::SameLine(0.f, 0.f);
	ImGui::TextColored(color, "%s", status);
}

// Компактный прогресс-бар с процентом поверх
static void DrawPhaseProgressBar(float percent, const ImVec4& color, ImVec2 size)
{
	percent = std::clamp(percent, 0.f, 1.f);

	char overlay[16];
	if (percent >= 0.999f)
	{
		snprintf(overlay, sizeof(overlay), "done");
	}
	else
	{
		snprintf(overlay, sizeof(overlay), "%d%%", (int)(percent * 100.f));
	}

	ImGui::PushStyleColor(ImGuiCol_PlotHistogram, color);
	ImGui::PushStyleColor(ImGuiCol_FrameBg, UITheme::RowBg);
	ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 3.f);
	ImGui::ProgressBar(percent, size, overlay);
	ImGui::PopStyleVar();
	ImGui::PopStyleColor(2);
}

// <<< PRETTY
// =========================================================================


void RenderCompilerUI(int X, int Y)
{
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(8, 8));

	ImGui::Begin("Compile Split Screen", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoNavFocus);

	// Calculate sizes for the top and bottom parts
	ImVec2 windowSize = ImGui::GetContentRegionAvail();
	float topHeight = hideLogSection ? windowSize.y - 58.f : windowSize.y * 0.5f;

	// Top section
	ImGui::BeginChild("TopSection", ImVec2(windowSize.x, topHeight), true);

	// >>> PRETTY: заголовок с акцентом
	{
		xr_string Levels;
		for (auto& [Name, Selected] : gCompilerMode.Files)
		{
			if (Selected)
			{
				Levels += (!Levels.empty() ? ", " : "") + Name;
			}
		}
		ImGui::TextColored(UITheme::Accent, "Building:");
		ImGui::SameLine();
		ImGui::TextUnformatted(Levels.empty() ? "<none>" : Levels.c_str());
	}
	// <<< PRETTY
	ImGui::Separator();

	ImVec4 phaseTextCol = UITheme::PhaseText;

	int Flags = ImGuiTableFlags_ScrollY | ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg | ImGuiTableFlags_Resizable | ImGuiTableFlags_SizingFixedSame;

	// Table
	if (ImGui::BeginTable("IterationsTable", 8, Flags))
	{
		ImGui::TableSetupColumn(" ", ImGuiTableColumnFlags_WidthFixed, 20.0f);
		ImGui::TableSetupColumn("Task", ImGuiTableColumnFlags_WidthFixed, 50.f);
		ImGui::TableSetupColumn("Phase", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn("Phase %", ImGuiTableColumnFlags_WidthFixed, 100.f);
		ImGui::TableSetupColumn("Elapsed Time", ImGuiTableColumnFlags_WidthFixed, 80.0f);
		ImGui::TableSetupColumn("Status", ImGuiTableColumnFlags_WidthFixed, 110.f);
		ImGui::TableSetupColumn("Memory", ImGuiTableColumnFlags_WidthFixed, 70.f);
		ImGui::TableSetupColumn("Info", ImGuiTableColumnFlags_WidthFixed, 350.0f);

		ImGui::TableHeadersRow();

		for (auto& row : GetIterationData())
		{
			xr_string rowStatus;
			ImVec4 rowStatusColor;
			char rowIcon;
			getStatusInfo(row.status, rowStatus, rowStatusColor, rowIcon);

			ImGui::TableNextRow(0, ImGui::GetTextLineHeight() + 8.f);

			// Иконка статуса
			ImGui::TableSetColumnIndex(0);
			ImGui::PushFont(gCompilerMode.CompilerIconsFont);
			ImGui::TextColored(rowStatusColor, "%c", rowIcon);
			ImGui::PopFont();

			// Название итерации
			ImGui::TableSetColumnIndex(1);
			ImGui::TextColored(UITheme::Text, "%s", row.iterationName.c_str());

			// Прогресс итерации (%)
			ImGui::TableSetColumnIndex(3);
			{
				float p = std::clamp(row.Persent, 0.f, 1.f);
				DrawPhaseProgressBar(p, rowStatusColor, ImVec2(-1.f, ImGui::GetTextLineHeight() * 0.85f));
			}

			// Общий статус
			ImGui::TableSetColumnIndex(5);
			DrawStatusBadge(rowStatus.c_str(), rowStatusColor);
			// <<< PRETTY

			for (auto& phase : row.phases)
			{
				xr_string status;
				ImVec4 statusColor;
				char phaseIcon;
				getStatusInfo(phase.status, status, statusColor, phaseIcon);
				ImGui::TableNextRow();

				// Иконка фазы
				ImGui::TableSetColumnIndex(1);
				ImGui::PushFont(gCompilerMode.CompilerIconsFont);
				float column_width = ImGui::GetColumnWidth();
				float text_size = ImGui::CalcTextSize("A").x;
				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + column_width - text_size - 6.f);
				ImGui::TextColored(statusColor, "%c", phaseIcon);
				ImGui::PopFont();

				// Название фазы
				ImGui::TableSetColumnIndex(2);
				ImGui::TextColored(phaseTextCol, "%s", phase.PhaseName.c_str());

				// Обновляем таймеры активной фазы
				auto PerS = phase.PhasePersent;

				if (phase.status != Complete)
				{
					u32 dwCurrentTime = timeGetTime();
					u32 dwTimeDiff = dwCurrentTime - GetPhaseStartTime();
					u32 secElapsed = dwTimeDiff / 1000;
					u32 secRemain = u32(float(secElapsed) / PerS) - secElapsed;

					phase.elapsed_time = secElapsed;
					if (PerS > 0.005f)
					{
						phase.remain_time = secRemain;
					}
				}

				if (phase.status == Complete)
				{
					PerS = 1;
				}
				else
				{
					PerS = std::clamp(PerS, 0.f, 1.f);
				}

				// >>> PRETTY: прогресс-бар вместо текста
				ImGui::TableSetColumnIndex(3);
				DrawPhaseProgressBar(PerS, statusColor, ImVec2(-1.f, ImGui::GetTextLineHeight() * 1.1f));

				// Время
				ImGui::TableSetColumnIndex(4);
				ImGui::TextColored(phaseTextCol, "%s", make_time(phase.elapsed_time).c_str());

				// Статус
				ImGui::TableSetColumnIndex(5);
				ImGui::TextColored(statusColor, "%s", status.c_str());

				// Память
				ImGui::TableSetColumnIndex(6);
				ImGui::TextColored(UITheme::TextDim, "%u MB", u32(size_t(phase.used_memory / 1024 / 1024)));

				// Доп. данные
				ImGui::TableSetColumnIndex(7);
				ImGui::TextColored(UITheme::TextDim, "%s", phase.AdditionalData.c_str());
			}
		}

		if (autoScroll)
		{
			ImGui::SetScrollY(ImGui::GetScrollMaxY());
		}
		ImGui::EndTable();


		ImGui::EndChild();
	}

	ImGui::Separator();

	// Окно лога
	{
		ImGui::TextColored(UITheme::Accent, "Log");
		ImGui::SameLine();

		const char* buttonText = (hideLogSection) ? "+" : "-";
		ImVec2 textSize = ImGui::CalcTextSize(buttonText);
		ImVec2 buttonSize = ImVec2(textSize.x + ImGui::GetStyle().FramePadding.x * 2, textSize.y + ImGui::GetStyle().FramePadding.y * 2);

		auto ZSize = ImGui::GetContentRegionAvail();

		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + ZSize.x - buttonSize.x);

		if (ImGui::Button(buttonText))
		{
			hideLogSection = !hideLogSection;
		}

		u32 nSize = windowSize.x / 4;

		if (!hideLogSection)
		{
#ifdef LCCUDA_BUILD
			if (ImGui::BeginChild("LogSection", ImVec2(nSize * 3, windowSize.y - topHeight - (buttonSize.y * 2) - 30), true))
#else
			if (ImGui::BeginChild("LogSection", ImVec2(windowSize.x, windowSize.y - topHeight - (buttonSize.y * 2) - 30), true))
#endif
			{
				ImGuiListClipper clipper;

				xrCriticalSectionGuard LogGuard(&csLog);

				clipper.Begin(GetLogVector().size());

				while (clipper.Step())
				{
					for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; ++i)
					{
						auto& line = GetLogVector()[i];
						ImGui::TextColored(getLogColor((char*)line.c_str()), "%s", Platform::UTF8_to_CP1251(line.c_str()).c_str());
					}
				}

				if (autoScroll)
				{
					ImGui::SetScrollY(ImGui::GetScrollMaxY());
				}

				ImGui::EndChild();
			}

#ifdef LCCUDA_BUILD
			ImGui::SameLine();

			if (ImGui::BeginChild("GPU USAGE", ImVec2(nSize, windowSize.y - topHeight - (buttonSize.y * 2) - 30), ImGuiWindowFlags_NoScrollWithMouse | ImGuiWindowFlags_NoScrollbar))
			{
				extern void CudaUsage(unsigned int& UsageCuda, unsigned int& UsageMemory);
				extern void CudaStatisticThread();
				extern xr_vector<float> get_cuda_usage();
				extern xr_vector<float> get_mem_usage();

				static bool isGpuStarted = false;
				if (!isGpuStarted)
				{
					isGpuStarted = true;
					CudaStatisticThread();
				}

				unsigned int UsageCuda = 0, UsageMemory = 0;
				CudaUsage(UsageCuda, UsageMemory);

				ImGui::Text("CUDA Usage: %u", UsageCuda);
				auto data = get_cuda_usage();
				DrawGpuGraph(data.data(), data.size(), 100.0f);

				ImGui::Separator();


				ImGui::Text("VRAM Usage: %u", UsageMemory);
				auto data_mem = get_mem_usage();
				DrawGpuGraph(data_mem.data(), data_mem.size(), 100.0f);

				ImGui::EndChild();
			}
#endif
		}
	}

	DrawDownUI();

	ImGui::End();
	ImGui::PopStyleVar();
}