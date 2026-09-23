#include "../../xrCore/xrCore.h"
#include "../../xrCore/FormatParsers/json/JsonSerialize.h"
#include "cl_log.h"
#include "CompilersUI.h"

#include <imgui.h>
#include <timeapi.h>

#include "../../Editors/xrEUI/imgui_impl_sdl3.h"
#include "imgui_impl_sdlrenderer3.h"
#include <SDL3/SDL.h>

#include "CompilerIcons.h"

// >>> UX-PROGRESS
#include "TaskbarProgress.h"
#include "ToastNotify.h"
// <<< UX-PROGRESS


extern int item_current_lightmap;
extern int item_current_cform;
extern int item_current_geom;
extern int item_current_jitter;
extern int item_current_jitter_mu;
extern int current_format;

void StartupAI();
void StartupLC();
void StartupDO();

CompilersMode gCompilerMode;
CJsonSerializer* Serializer = nullptr;

extern bool ShowMainUI;

static SDL_Texture* LightPreviewTex = nullptr;
static int LightPreviewTexW = 0;
static int LightPreviewTexH = 0;
static SDL_Renderer* LightPreviewRenderer = nullptr;

static void RefreshLightPreview(SDL_Renderer* Renderer)
{
	LightPreviewRenderer = Renderer;
}

static float Edge(float Ax, float Ay, float Bx, float By, float Px, float Py)
{
	return (Px - Ax) * (By - Ay) - (Py - Ay) * (Bx - Ax);
}

struct ClipVert
{
	float Cx = 0.f;
	float Cy = 0.f;
	float Cz = 0.f;
	float U = 0.f;
	float V = 0.f;
	float R = 0.f;
	float G = 0.f;
	float B = 0.f;
};

static ClipVert LerpVert(const ClipVert& A, const ClipVert& B, float T)
{
	ClipVert Out;
	Out.Cx = A.Cx + (B.Cx - A.Cx) * T;
	Out.Cy = A.Cy + (B.Cy - A.Cy) * T;
	Out.Cz = A.Cz + (B.Cz - A.Cz) * T;
	Out.U = A.U + (B.U - A.U) * T;
	Out.V = A.V + (B.V - A.V) * T;
	Out.R = A.R + (B.R - A.R) * T;
	Out.G = A.G + (B.G - A.G) * T;
	Out.B = A.B + (B.B - A.B) * T;
	return Out;
}

static void RasterPreviewScene(
	const xr_vector<Fvector>& Vertices,
	const xr_vector<u32>& Indices,
	const Fvector& Target,
	float Yaw,
	float Pitch,
	float Distance,
	int Width,
	int Height,
	const xr_vector<u8>* Baked,
	const xr_vector<float>* MapUv,
	const xr_vector<u32>* MapLayer,
	const xr_vector<LightPreviewMap>* Maps,
	xr_vector<u8>& Rgba)
{
	Rgba.assign((size_t)Width * (size_t)Height * 4, 0);
	for (int Index = 0; Index < Width * Height; ++Index)
	{
		u8* Pixel = &Rgba[(size_t)Index * 4];
		Pixel[0] = 214;
		Pixel[1] = 216;
		Pixel[2] = 218;
		Pixel[3] = 255;
	}

	xr_vector<float> ZBuffer((size_t)Width * (size_t)Height, 1e20f);

	const float CosPitch = cosf(Pitch);
	const float SinPitch = sinf(Pitch);
	const float CosYaw = cosf(Yaw);
	const float SinYaw = sinf(Yaw);

	Fvector Eye;
	Eye.x = Target.x + CosPitch * SinYaw * Distance;
	Eye.y = Target.y + SinPitch * Distance;
	Eye.z = Target.z + CosPitch * CosYaw * Distance;

	Fvector Forward;
	Forward.sub(Target, Eye);
	Forward.normalize_safe();

	Fvector WorldUp;
	WorldUp.set(0.f, 1.f, 0.f);
	if (std::abs(Forward.dotproduct(WorldUp)) > 0.95f)
	{
		WorldUp.set(0.f, 0.f, 1.f);
	}

	Fvector Right;
	Fvector Up;
	Right.crossproduct(Forward, WorldUp);
	Right.normalize_safe();
	Up.crossproduct(Right, Forward);
	Up.normalize_safe();

	Fvector Light;
	Light.set(0.35f, 0.86f, 0.28f);
	Light.normalize_safe();

	const float Focal = (float(Height) * 0.5f) / tanf(deg2rad(50.f) * 0.5f);
	const float ZNear = 0.05f;

	auto DrawTri = [&](ClipVert V0, ClipVert V1, ClipVert V2, const LightPreviewMap* Map, u8 Shade)
	{
		float X0 = (V0.Cx / V0.Cz) * Focal + float(Width) * 0.5f;
		float Y0 = (-V0.Cy / V0.Cz) * Focal + float(Height) * 0.5f;
		float X1 = (V1.Cx / V1.Cz) * Focal + float(Width) * 0.5f;
		float Y1 = (-V1.Cy / V1.Cz) * Focal + float(Height) * 0.5f;
		float X2 = (V2.Cx / V2.Cz) * Focal + float(Width) * 0.5f;
		float Y2 = (-V2.Cy / V2.Cz) * Focal + float(Height) * 0.5f;

		float Area = Edge(X0, Y0, X1, Y1, X2, Y2);
		if (Area < 0.f)
		{
			std::swap(X1, X2);
			std::swap(Y1, Y2);
			std::swap(V1, V2);
			Area = -Area;
		}
		if (Area < 1.f)
		{
			return;
		}

		const float Z0 = V0.Cz;
		const float Z1 = V1.Cz;
		const float Z2 = V2.Cz;

		int MinX = (int)floorf(std::min(X0, std::min(X1, X2)));
		int MaxX = (int)ceilf(std::max(X0, std::max(X1, X2)));
		int MinY = (int)floorf(std::min(Y0, std::min(Y1, Y2)));
		int MaxY = (int)ceilf(std::max(Y0, std::max(Y1, Y2)));
		if (MaxX < 0 || MaxY < 0 || MinX >= Width || MinY >= Height)
		{
			return;
		}

		MinX = std::clamp(MinX, 0, Width - 1);
		MaxX = std::clamp(MaxX, 0, Width - 1);
		MinY = std::clamp(MinY, 0, Height - 1);
		MaxY = std::clamp(MaxY, 0, Height - 1);

		const float W0Dx = Y2 - Y1;
		const float W1Dx = Y0 - Y2;
		const float W2Dx = Y1 - Y0;
		const float W0Dy = X1 - X2;
		const float W1Dy = X2 - X0;
		const float W2Dy = X0 - X1;
		const float StartX = float(MinX) + 0.5f;
		const float StartY = float(MinY) + 0.5f;
		float Row0 = Edge(X1, Y1, X2, Y2, StartX, StartY);
		float Row1 = Edge(X2, Y2, X0, Y0, StartX, StartY);
		float Row2 = Edge(X0, Y0, X1, Y1, StartX, StartY);
		const float InvZ0 = 1.f / Z0;
		const float InvZ1 = 1.f / Z1;
		const float InvZ2 = 1.f / Z2;

		for (int Y = MinY; Y <= MaxY; ++Y)
		{
			float W0 = Row0;
			float W1 = Row1;
			float W2 = Row2;
			float* ZRow = &ZBuffer[(size_t)Y * (size_t)Width];
			u8* PixelRow = &Rgba[((size_t)Y * (size_t)Width) * 4];
			for (int X = MinX; X <= MaxX; ++X)
			{
				if (W0 >= 0.f && W1 >= 0.f && W2 >= 0.f)
				{
					const float InvZ = (W0 * InvZ0 + W1 * InvZ1 + W2 * InvZ2) / Area;
					float Depth = 1.f / InvZ;
					if (Map)
					{
						Depth *= 0.999f;
					}
					if (Depth < ZRow[X])
					{
						ZRow[X] = Depth;
						u8* Pixel = PixelRow + X * 4;
						const float B0 = W0 * InvZ0;
						const float B1 = W1 * InvZ1;
						const float B2 = W2 * InvZ2;
						const float InvB = 1.f / (B0 + B1 + B2);
						if (Map)
						{
							const float SampleU = (B0 * V0.U + B1 * V1.U + B2 * V2.U) * InvB;
							const float SampleV = (B0 * V0.V + B1 * V1.V + B2 * V2.V) * InvB;
							const int TexX = (int)(std::clamp(SampleU, 0.f, 0.999f) * float(Map->Width));
							const int TexY = (int)(std::clamp(SampleV, 0.f, 0.999f) * float(Map->Height));
							const u8* Src = &Map->Rgb[((size_t)TexY * Map->Width + TexX) * 3];
							Pixel[0] = Src[0];
							Pixel[1] = Src[1];
							Pixel[2] = Src[2];
						}
						else
						{
							Pixel[0] = u8((B0 * V0.R + B1 * V1.R + B2 * V2.R) * InvB);
							Pixel[1] = u8((B0 * V0.G + B1 * V1.G + B2 * V2.G) * InvB);
							Pixel[2] = u8((B0 * V0.B + B1 * V1.B + B2 * V2.B) * InvB);
							if (Pixel[0] == 0 && Pixel[1] == 0 && Pixel[2] == 0)
							{
								Pixel[0] = Shade;
								Pixel[1] = Shade;
								Pixel[2] = Shade;
							}
						}
						Pixel[3] = 255;
					}
				}
				W0 += W0Dx;
				W1 += W1Dx;
				W2 += W2Dx;
			}
			Row0 += W0Dy;
			Row1 += W1Dy;
			Row2 += W2Dy;
		}
	};

	for (size_t Index = 0; Index + 2 < Indices.size(); Index += 3)
	{
		const u32 I0 = Indices[Index];
		const u32 I1 = Indices[Index + 1];
		const u32 I2 = Indices[Index + 2];
		if (I0 >= Vertices.size() || I1 >= Vertices.size() || I2 >= Vertices.size())
		{
			continue;
		}

		const Fvector& A = Vertices[I0];
		const Fvector& B = Vertices[I1];
		const Fvector& C = Vertices[I2];

		Fvector Edge0;
		Fvector Edge1;
		Fvector Normal;
		Edge0.sub(B, A);
		Edge1.sub(C, A);
		Normal.crossproduct(Edge0, Edge1);
		if (Normal.square_magnitude() < EPS_S)
		{
			continue;
		}
		Normal.normalize();

		const size_t Triangle = Index / 3;
		const u8* BakedTri = (Baked && Baked->size() >= (Triangle + 1) * 9) ? &(*Baked)[Triangle * 9] : nullptr;
		const LightPreviewMap* Map = nullptr;
		float Uv0 = 0.f;
		float Uv1 = 0.f;
		float Uv2 = 0.f;
		float Uv3 = 0.f;
		float Uv4 = 0.f;
		float Uv5 = 0.f;
		if (MapUv && MapLayer && Maps && MapUv->size() >= (Triangle + 1) * 6 && MapLayer->size() > Triangle)
		{
			const u32 Layer = (*MapLayer)[Triangle];
			if (Layer != u32(-1) && Layer < Maps->size() && !(*Maps)[Layer].Rgb.empty())
			{
				const float* SrcUv = &(*MapUv)[Triangle * 6];
				Uv0 = SrcUv[0];
				Uv1 = SrcUv[1];
				Uv2 = SrcUv[2];
				Uv3 = SrcUv[3];
				Uv4 = SrcUv[4];
				Uv5 = SrcUv[5];
				Map = &(*Maps)[Layer];
			}
		}

		auto ToCam = [&](const Fvector& Point, float U, float V, float R, float G, float Bch) -> ClipVert
		{
			Fvector Rel;
			Rel.sub(Point, Eye);
			ClipVert Out;
			Out.Cx = Rel.dotproduct(Right);
			Out.Cy = Rel.dotproduct(Up);
			Out.Cz = Rel.dotproduct(Forward);
			Out.U = U;
			Out.V = V;
			Out.R = R;
			Out.G = G;
			Out.B = Bch;
			return Out;
		};

		const float R0 = BakedTri ? BakedTri[0] : 196.f;
		const float G0 = BakedTri ? BakedTri[1] : 196.f;
		const float B0 = BakedTri ? BakedTri[2] : 196.f;
		const float R1 = BakedTri ? BakedTri[3] : 196.f;
		const float G1 = BakedTri ? BakedTri[4] : 196.f;
		const float B1 = BakedTri ? BakedTri[5] : 196.f;
		const float R2 = BakedTri ? BakedTri[6] : 196.f;
		const float G2 = BakedTri ? BakedTri[7] : 196.f;
		const float B2 = BakedTri ? BakedTri[8] : 196.f;

		ClipVert In[3] =
		{
			ToCam(A, Uv0, Uv1, R0, G0, B0),
			ToCam(B, Uv2, Uv3, R1, G1, B1),
			ToCam(C, Uv4, Uv5, R2, G2, B2)
		};

		ClipVert Clipped[8];
		int OutCount = 0;
		for (int Corner = 0; Corner < 3; ++Corner)
		{
			const ClipVert& From = In[Corner];
			const ClipVert& To = In[(Corner + 1) % 3];
			const bool FromIn = From.Cz >= ZNear;
			const bool ToIn = To.Cz >= ZNear;
			if (FromIn && ToIn)
			{
				Clipped[OutCount++] = To;
			}
			else if (FromIn && !ToIn)
			{
				const float TClip = (ZNear - From.Cz) / (To.Cz - From.Cz);
				Clipped[OutCount++] = LerpVert(From, To, TClip);
			}
			else if (!FromIn && ToIn)
			{
				const float TClip = (ZNear - From.Cz) / (To.Cz - From.Cz);
				Clipped[OutCount++] = LerpVert(From, To, TClip);
				Clipped[OutCount++] = To;
			}
		}
		if (OutCount < 3)
		{
			continue;
		}

		const float NDotL = std::clamp(Normal.dotproduct(Light), 0.f, 1.f);
		const u8 Shade = u8((0.28f + 0.72f * NDotL) * 255.f);
		for (int Fan = 1; Fan + 1 < OutCount; ++Fan)
		{
			DrawTri(Clipped[0], Clipped[Fan], Clipped[Fan + 1], Map, Shade);
		}
	}
}

void DrawLightPreview(float Width, float Height)
{
	static xr_vector<Fvector> Vertices;
	static xr_vector<u32> Indices;
	static Fvector Target;
	static float Radius = 1.f;
	static u32 SceneGeneration = 0;
	static float Yaw = 0.f;
	static float Pitch = 0.4f;
	static float Distance = 10.f;
	static bool CameraReady = false;
	static xr_vector<u8> Frame;
	static xr_vector<u8> Baked;
	static xr_vector<float> MapUv;
	static xr_vector<u32> MapLayer;
	static xr_vector<LightPreviewMap> Maps;
	static u32 ColorGeneration = 0;
	static u32 MapGeneration = 0;
	static bool FrameDirty = true;

	u32 NewSceneGeneration = SceneGeneration;
	if (TakeLightPreviewScene(SceneGeneration, NewSceneGeneration, Vertices, Indices, Target, Radius))
	{
		SceneGeneration = NewSceneGeneration;
		const float Len = sqrtf(0.85f * 0.85f + 0.55f * 0.55f + 0.85f * 0.85f);
		Yaw = atan2f(0.85f, 0.85f);
		Pitch = asinf(std::clamp(0.55f / Len, -1.f, 1.f));
		Distance = Radius * 2.6f;
		CameraReady = true;
		FrameDirty = true;
	}

	u32 NewColorGeneration = ColorGeneration;
	if (TakeLightPreviewColors(ColorGeneration, NewColorGeneration, Baked))
	{
		ColorGeneration = NewColorGeneration;
		FrameDirty = true;
	}

	u32 NewMapGeneration = MapGeneration;
	if (TakeLightPreviewMaps(MapGeneration, NewMapGeneration, MapUv, MapLayer, Maps))
	{
		MapGeneration = NewMapGeneration;
		FrameDirty = true;
	}

	if (!CameraReady || Vertices.empty() || Indices.empty())
	{
		ImGui::TextUnformatted("3D preview is not ready");
		return;
	}

	const int ViewW = std::max(8, (int)Width);
	const int ViewH = std::max(8, (int)Height);
	const ImVec2 Origin = ImGui::GetCursorScreenPos();
	ImGui::InvisibleButton("##light_preview", ImVec2((float)ViewW, (float)ViewH));

	if (ImGui::IsItemHovered())
	{
		const float Wheel = ImGui::GetIO().MouseWheel;
		if (Wheel != 0.f)
		{
			Distance = std::clamp(Distance * powf(0.88f, Wheel), Radius * 0.05f, Radius * 20.f);
			FrameDirty = true;
		}
	}

	if (ImGui::IsItemActive() && ImGui::IsMouseDragging(ImGuiMouseButton_Left))
	{
		Yaw += ImGui::GetIO().MouseDelta.x * 0.01f;
		Pitch = std::clamp(Pitch - ImGui::GetIO().MouseDelta.y * 0.01f, -1.35f, 1.35f);
		FrameDirty = true;
	}

	if (LightPreviewTexW != ViewW || LightPreviewTexH != ViewH)
	{
		FrameDirty = true;
	}

	if (FrameDirty && LightPreviewRenderer)
	{
		RasterPreviewScene(
			Vertices, Indices, Target, Yaw, Pitch, Distance, ViewW, ViewH,
			Baked.empty() ? nullptr : &Baked,
			MapUv.empty() ? nullptr : &MapUv,
			MapLayer.empty() ? nullptr : &MapLayer,
			Maps.empty() ? nullptr : &Maps,
			Frame);
		if (!LightPreviewTex || LightPreviewTexW != ViewW || LightPreviewTexH != ViewH)
		{
			if (LightPreviewTex)
			{
				SDL_DestroyTexture(LightPreviewTex);
			}
			LightPreviewTex = SDL_CreateTexture(LightPreviewRenderer, SDL_PIXELFORMAT_ABGR8888, SDL_TEXTUREACCESS_STATIC, ViewW, ViewH);
			LightPreviewTexW = ViewW;
			LightPreviewTexH = ViewH;
		}
		if (LightPreviewTex)
		{
			SDL_UpdateTexture(LightPreviewTex, nullptr, Frame.data(), ViewW * 4);
		}
		FrameDirty = false;
	}

	if (!LightPreviewTex)
	{
		return;
	}

	ImGui::GetWindowDrawList()->AddImage(
		(ImTextureID)LightPreviewTex,
		Origin,
		ImVec2(Origin.x + (float)ViewW, Origin.y + (float)ViewH));
}


// >>> UX-PROGRESS
static xr_string GetBuildingLevelName()
{
	xr_string result;

	for (const auto& FILE : gCompilerMode.Files)
	{
		if (FILE.Select)
		{
			if (!result.empty())
			{
				result += ", ";
			}

			result += FILE.Name.c_str();
		}
	}

	if (result.empty())
	{
		result = "<unknown>";
	}

	return result;
}

// =========================================================================
// ������� ���� �������� � �������-������ � ������ �����.
// =========================================================================
static TaskbarOverlay GetOverlayForIteration(LCBuildingType type)
{
	switch (type)
	{
		case LCBuildingType::eLC:
			return TaskbarOverlay::XrLC;
		case LCBuildingType::eAI:
			return TaskbarOverlay::XrAI;
		case LCBuildingType::eDO:
			return TaskbarOverlay::XrDO;
		default:
			return TaskbarOverlay::None;
	}
}

// ������: ������� wide-string �� ���� (LPCSTR).
static std::wstring WidenPhase(LPCSTR phase)
{
	if (!phase)
	{
		return std::wstring();
	}

	int len = (int)xr_strlen(phase);
	return std::wstring(phase, phase + len);
}
// <<< UX-PROGRESS


void Startup(LPSTR lpCmdLine)
{
	xrLogger::EnableFastDebugLog();

	SaveCompilerCfg();

	GetIterationData().push_back({"xrLC"});
	GetIterationData().push_back({"xrAI"});
	GetIterationData().push_back({"xrDO"});

	// >>> UX-PROGRESS
	const xr_string levelName = GetBuildingLevelName();
	const std::wstring wLevel(levelName.begin(), levelName.end());

	clMsg("=== Startup: initializing ToastNotify and TaskbarProgress ===");
	clMsg("* Startup: building level = '%s'", levelName.c_str());

	CToastNotify::Instance().Initialize();

	// Tooltip ��� ������� � ������ ����� (�� ������ ������ ������).
	{
		std::wstring tooltip = L"IX-Ray Level Builder - building: ";
		tooltip += wLevel;
		CTaskbarProgress::Instance().SetTooltip(tooltip);
	}

	// ��������� ���� � ������ ������.
	CToastNotify::Instance().ShowInfo(
		L"IX-Ray Level Builder",
		std::wstring(L"Compilation started: ") + wLevel
	);
	// <<< UX-PROGRESS

	// >>> UX-PROGRESS
	int totalActive = 0;
	if (gCompilerMode.LC)
	{
		++totalActive;
	}
	if (gCompilerMode.AI)
	{
		++totalActive;
	}
	if (gCompilerMode.DO)
	{
		++totalActive;
	}
	int completed = 0;
	clMsg("* Startup: totalActive = %d", totalActive);
	// <<< UX-PROGRESS

	auto InitilizeIteration = [&](LCBuildingType Type, bool active, LPCSTR phase)
	{
		SetActiveIteration(&(GetIterationData()[(int)Type]));
		gCompilerMode.builder_type = Type;

		if (active)
		{
			clMsg("* Startup: iteration %d (%s) - starting", (int)Type, phase);

			// >>> UX-PROGRESS: indeterminate + ����������� ������ ������
			CTaskbarProgress::Instance().SetState(TBPF_INDETERMINATE);

			{
				const TaskbarOverlay overlay = GetOverlayForIteration(Type);
				const std::wstring wPhase = WidenPhase(phase);

				std::wstring desc = L"IX-Ray Level Builder - ";
				desc += wPhase;
				CTaskbarProgress::Instance().SetOverlayIcon(overlay, desc);

				std::wstring tip = L"IX-Ray Level Builder - ";
				tip += wLevel;
				tip += L" (";
				tip += wPhase;
				tip += L")";
				CTaskbarProgress::Instance().SetTooltip(tip);
			}
			// <<< UX-PROGRESS

			GetActiveIteration()->status = InProgress;
			u32 dwTime = timeGetTime();
			Phase(phase);

			if (Type == LCBuildingType::eLC)
			{
				StartupLC();
			}
			else if (Type == LCBuildingType::eDO)
			{
				StartupDO();
			}
			else if (Type == LCBuildingType::eAI)
			{
				StartupAI();
			}

			dwTime = (timeGetTime() - dwTime) / 1000;

			GetActiveIteration()->status = Complete;
			GetActiveIteration()->elapsed_time = dwTime;

			clMsg("* Startup: iteration %d (%s) - complete, elapsed = %u sec", (int)Type, phase, dwTime);
		}
		else
		{
			GetActiveIteration()->status = Skip;
			clMsg("* Startup: iteration %d (%s) - skipped", (int)Type, phase);
		}

		PhaseEnd();

		// >>> UX-PROGRESS: �������� �� ������ ����� �� �������
		++completed;
		if (totalActive > 0)
		{
			CTaskbarProgress::Instance().SetState(TBPF_NORMAL);
			CTaskbarProgress::Instance().SetProgress((ULONGLONG)completed, (ULONGLONG)totalActive);
		}
		// <<< UX-PROGRESS
	};

	InitilizeIteration(LCBuildingType::eLC, gCompilerMode.LC, "xrLC Startup");
	InitilizeIteration(LCBuildingType::eAI, gCompilerMode.AI, "xrAI Startup");
	InitilizeIteration(LCBuildingType::eDO, gCompilerMode.DO, "xrDO Startup");

	// Show statistic
	extern xr_string make_time(u32 sec);
	for (auto& I : GetIterationData())
	{
		clMsg("* Compiler (%s) : Time elapsed: %s ", I.iterationName.c_str(), make_time(I.elapsed_time));
	}

	// Close log
	xrLogger::FlushLog();

	// >>> UX-PROGRESS: ��������� ������� + ����� ���������
	CTaskbarProgress::Instance().Reset();
	CTaskbarProgress::Instance().SetTooltip(L"IX-Ray Level Builder");

	{
		xr_string summary;
		for (auto& I : GetIterationData())
		{
			if (I.status == Complete)
			{
				summary += I.iterationName.c_str();
				summary += ": ";
				summary += make_time(I.elapsed_time).c_str();
				summary += "  ";
			}
		}

		// ���������, ���� �� ������� (�� Complete � �� Skip).
		bool anyFailed = false;
		for (auto& I : GetIterationData())
		{
			if (I.status != Complete && I.status != Skip)
			{
				anyFailed = true;
				break;
			}
		}

		const TaskbarOverlay finalOverlay = anyFailed
												? TaskbarOverlay::Error
												: TaskbarOverlay::Success;

		const wchar_t* finalDesc = anyFailed
									   ? L"Compilation failed"
									   : L"Compilation complete";

		CTaskbarProgress::Instance().SetOverlayIcon(finalOverlay, finalDesc);

		// ��������� ����� � ������ ������.
		std::wstring title = anyFailed
								 ? L"Compilation failed: "
								 : L"Compilation complete: ";
		title += wLevel;

		std::wstring body(summary.begin(), summary.end());
		clMsg("* Startup: showing completion toast, level = '%s'", levelName.c_str());

		if (anyFailed)
		{
			CToastNotify::Instance().ShowError(title, body);
		}
		else
		{
			CToastNotify::Instance().ShowSuccess(title, body);
		}
	}
	// <<< UX-PROGRESS

	ShowMainUI = true;
	Sleep(200);
}


void SDL_Application()
{
	if (!SDL_Init(SDL_INIT_EVENTS) != 0)
	{
		printf("Error: SDL_Init(): %s\n", SDL_GetError());
		return;
	}

	SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
	g_AppInfo.Window = SDL_CreateWindow("IX-Ray Level Builder", 1000, 560, window_flags);

	if (!g_AppInfo.Window)
	{
		clMsg("! SDL_Application: SDL_CreateWindow failed: %s", SDL_GetError());
		return;
	}

	SDL_Renderer* renderer = SDL_CreateRenderer(g_AppInfo.Window, NULL);

	if (!renderer)
	{
		clMsg("! SDL_Application: SDL_CreateRenderer failed: %s", SDL_GetError());
		return;
	}

	SDL_SetWindowPosition(g_AppInfo.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
	SDL_ShowWindow(g_AppInfo.Window);

	// >>> UX-PROGRESS: �������� HWND �� SDL3 � ���������������� taskbar/toast
	{
		HWND hwnd = nullptr;
		SDL_PropertiesID props = SDL_GetWindowProperties(g_AppInfo.Window);
		clMsg("* SDL_Application: SDL_GetWindowProperties returned props = %u", (unsigned)props);

		if (props != 0)
		{
			hwnd = static_cast<HWND>(
				SDL_GetPointerProperty(props, SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr)
			);
		}

		clMsg("* SDL_Application: extracted HWND = 0x%p", hwnd);

		if (hwnd)
		{
			if (!CTaskbarProgress::Instance().Initialize(hwnd))
			{
				clMsg("! SDL_Application: TaskbarProgress initialization FAILED");
			}
			else
			{
				clMsg("* SDL_Application: TaskbarProgress initialization OK");
			}
		}
		else
		{
			clMsg("! SDL_Application: HWND not found - taskbar progress unavailable");
		}

		CToastNotify::Instance().Initialize();
	}
	// <<< UX-PROGRESS

	ImGui::CreateContext();
	ImGuiIO& io = ImGui::GetIO();
	(void)io;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;

	XRay::ImGui::MakeEditorTheme();

	ImGui_ImplSDL3_InitForSDLRenderer(g_AppInfo.Window, renderer);
	ImGui_ImplSDLRenderer3_Init(renderer);

	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);

	ImFont* defaultFont = io.Fonts->AddFontDefault();

	ImFontConfig config;
	config.FontDataOwnedByAtlas = false;

	gCompilerMode.CompilerIconsFont = io.Fonts->AddFontFromMemoryTTF(
		IconsFont, sizeof(IconsFont), 16.f, &config, io.Fonts->GetGlyphRangesDefault()
	);

	gCompilerMode.ThreadsPerWork = CPU::ID().n_threads - 1;

	bool done = false;

	while (!done)
	{
		SDL_Event event;
		while (SDL_PollEvent(&event))
		{
			ImGui_ImplSDL3_ProcessEvent(&event);
			if (event.type == SDL_EVENT_QUIT)
			{
				done = true;
			}
			if (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED &&
				event.window.windowID == SDL_GetWindowID(g_AppInfo.Window))
			{
				done = true;
			}
		}

		RefreshLightPreview(renderer);

		ImGui_ImplSDLRenderer3_NewFrame();
		ImGui_ImplSDL3_NewFrame();
		ImGui::NewFrame();

		{
			RenderMainUI();
		}

		ImGui::Render();

		SDL_SetRenderDrawColor(renderer, (Uint8)(clear_color.x * 255), (Uint8)(clear_color.y * 255), (Uint8)(clear_color.z * 255), (Uint8)(clear_color.w * 255));
		SDL_RenderClear(renderer);
		ImGui_ImplSDLRenderer3_RenderDrawData(ImGui::GetDrawData());
		SDL_RenderPresent(renderer);

		Sleep(41);
	}

	// >>> UX-PROGRESS: ���������� ����������
	clMsg("=== SDL_Application: shutting down UX-PROGRESS ===");
	CToastNotify::Instance().Shutdown();
	CTaskbarProgress::Instance().Release();
	// <<< UX-PROGRESS

	if (LightPreviewTex)
	{
		SDL_DestroyTexture(LightPreviewTex);
		LightPreviewTex = nullptr;
	}

	ImGui_ImplSDLRenderer3_Shutdown();
	ImGui_ImplSDL3_Shutdown();
	ImGui::DestroyContext();

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(g_AppInfo.Window);
	SDL_Quit();
}


void StartCompile()
{
	Sleep(150);
	thread_spawn(logThread, "log-update", 1024 * 1024, 0);
}


void SaveCompilerCfg()
{
	Serializer->Write("ai", gCompilerMode.AI);
	Serializer->Write("lc", gCompilerMode.LC);
	Serializer->Write("do", gCompilerMode.DO);
	Serializer->Write("Silent", gCompilerMode.Silent);
	Serializer->Write("Embree", gCompilerMode.Embree);
	Serializer->Write("CUDA", gCompilerMode.CUDA);
	Serializer->Write("EmbreeBVHCompact", gCompilerMode.EmbreeBVHCompact);
	Serializer->Write("EmbreeBVHRobust", gCompilerMode.EmbreeBVHRobust);
	Serializer->Write("ClearTemp", gCompilerMode.ClearTemp);
	Serializer->Write("SkipTHM", gCompilerMode.SkipTHM);

	Serializer->Write("LC_SkipStaticMap", gCompilerMode.LC_SkipStaticMap);
	Serializer->Write("LC_NoSun", gCompilerMode.LC_NoSun);
	Serializer->Write("LC_NoSMG", gCompilerMode.LC_NoSMG);
	Serializer->Write("LC_Tess", gCompilerMode.LC_Tess);
	Serializer->Write("LC_SkipInvalidFaces", gCompilerMode.LC_SkipInvalidFaces);
	Serializer->Write("LC_tex_format", current_format);
	Serializer->Write("LC_skipWeld", gCompilerMode.LC_skipWeld);
	Serializer->Write("IsOverloadedSettings", gCompilerMode.IsOverloadedSettings);
	Serializer->Write("LC_sizeLmaps", gCompilerMode.LC_sizeLmaps);
	Serializer->Write("LC_JSampleMU", gCompilerMode.LC_JSampleMU);
	Serializer->Write("LC_JSample", gCompilerMode.LC_JSample);
	Serializer->Write("ThreadsPerWork", gCompilerMode.ThreadsPerWork);
	Serializer->Write("LC_Pixels", gCompilerMode.LC_Pixels);
	Serializer->Write("WeldDistance", gCompilerMode.WeldDistance);
	Serializer->Write("AI_BuildSpawn", gCompilerMode.AI_BuildSpawn);
	Serializer->Write("AI_NoSeparatorCheck", gCompilerMode.AI_NoSeparatorCheck);
	Serializer->Write("AI_FreeMPBuild", gCompilerMode.AI_FreeMPBuild);
	Serializer->Write("AI_StartActor", gCompilerMode.AI_StartActor);
	Serializer->Write("AI_spawn_name", gCompilerMode.AI_spawn_name);
	Serializer->Write("AI_BuildLevel", gCompilerMode.AI_BuildLevel);
	Serializer->Write("AI_PureCovers", gCompilerMode.AI_PureCovers);
	Serializer->Write("AI_Draft", gCompilerMode.AI_Draft);
	Serializer->Write("AI_Verify", gCompilerMode.AI_Verify);
	Serializer->Write("AI_Verbose", gCompilerMode.AI_Verbose);

	Serializer->Write("item_current_selected", item_current_lightmap);
	Serializer->Write("item_current_cform", item_current_cform);
	Serializer->Write("item_current_geom", item_current_geom);
	Serializer->Write("item_current_jitter", item_current_jitter);
	Serializer->Write("item_current_jitter_mu", item_current_jitter_mu);

	Serializer->Write("LC_fast_way", gCompilerMode.LC_fast_way);
	Serializer->Write("LC_legacyLM", gCompilerMode.LC_legacyLM);
	Serializer->Write("LC_CformType", gCompilerMode.LC_CformType);
	Serializer->Write("LC_CFormChunkSize", gCompilerMode.LC_CFormChunkSize);
	Serializer->Write("LC_GeomType", gCompilerMode.LC_GeomType);
	Serializer->Write("LC_GeomChunkSize", gCompilerMode.LC_GeomChunkSize);

	Serializer->Write("LC_Skip_Progressive", gCompilerMode.LC_OGF_PROGRESSIVE);
	Serializer->Write("LC_Skip_Striptify", gCompilerMode.LC_OGF_STRIPTIFY);
	Serializer->Write("LC_Skip_Tangents", gCompilerMode.LC_OGF_TANGENT);

	Serializer->Save();
}


int APIENTRY WinMain(
	HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	LPSTR lpCmdLine,
	int nCmdShow
)
{
	Debug._initialize(false);

	const char* fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(lpCmdLine, fsgame_ltx_name))
	{
		int sz = xr_strlen(fsgame_ltx_name);
		sscanf(strstr(lpCmdLine, fsgame_ltx_name) + sz, "%[^ ] ", fsgame);
	}
	Core._initialize("IX-Ray Compilers", nullptr, true, fsgame[0] ? fsgame : nullptr);

	Serializer = new CJsonSerializer("xrlevelbuilder.json");
	Serializer->Read("ai", gCompilerMode.AI);
	Serializer->Read("lc", gCompilerMode.LC);
	Serializer->Read("do", gCompilerMode.DO);
	Serializer->Read("Silent", gCompilerMode.Silent);
	Serializer->Read("Embree", gCompilerMode.Embree);
	Serializer->Read("CUDA", gCompilerMode.CUDA);
	Serializer->Read("EmbreeBVHCompact", gCompilerMode.EmbreeBVHCompact);
	Serializer->Read("EmbreeBVHRobust", gCompilerMode.EmbreeBVHRobust);
	Serializer->Read("ClearTemp", gCompilerMode.ClearTemp);
	Serializer->Read("SkipTHM", gCompilerMode.SkipTHM);
	Serializer->Read("LC_SkipStaticMap", gCompilerMode.LC_SkipStaticMap);
	Serializer->Read("LC_NoSun", gCompilerMode.LC_NoSun);
	Serializer->Read("LC_NoSMG", gCompilerMode.LC_NoSMG);
	Serializer->Read("LC_Tess", gCompilerMode.LC_Tess);
	Serializer->Read("LC_SkipInvalidFaces", gCompilerMode.LC_SkipInvalidFaces);
	Serializer->Read("LC_tex_format", current_format);
	Serializer->Read("LC_skipWeld", gCompilerMode.LC_skipWeld);
	Serializer->Read("IsOverloadedSettings", gCompilerMode.IsOverloadedSettings);
	Serializer->Read("LC_sizeLmaps", gCompilerMode.LC_sizeLmaps);
	Serializer->Read("LC_JSampleMU", gCompilerMode.LC_JSampleMU);
	Serializer->Read("LC_JSample", gCompilerMode.LC_JSample);
	Serializer->Read("ThreadsPerWork", gCompilerMode.ThreadsPerWork);
	Serializer->Read("LC_Pixels", gCompilerMode.LC_Pixels);
	Serializer->Read("WeldDistance", gCompilerMode.WeldDistance);
	Serializer->Read("AI_BuildSpawn", gCompilerMode.AI_BuildSpawn);
	Serializer->Read("AI_NoSeparatorCheck", gCompilerMode.AI_NoSeparatorCheck);
	Serializer->Read("AI_FreeMPBuild", gCompilerMode.AI_FreeMPBuild);
	Serializer->Read("AI_StartActor", gCompilerMode.AI_StartActor);
	Serializer->Read("AI_spawn_name", gCompilerMode.AI_spawn_name);
	Serializer->Read("AI_BuildLevel", gCompilerMode.AI_BuildLevel);
	Serializer->Read("AI_PureCovers", gCompilerMode.AI_PureCovers);
	Serializer->Read("AI_Draft", gCompilerMode.AI_Draft);
	Serializer->Read("AI_Verify", gCompilerMode.AI_Verify);
	Serializer->Read("AI_Verbose", gCompilerMode.AI_Verbose);
	Serializer->Read("item_current_selected", item_current_lightmap);
	Serializer->Read("item_current_cform", item_current_cform);
	Serializer->Read("item_current_geom", item_current_geom);
	Serializer->Read("item_current_jitter", item_current_jitter);
	Serializer->Read("item_current_jitter_mu", item_current_jitter_mu);

	Serializer->Read("LC_fast_way", gCompilerMode.LC_fast_way);
	Serializer->Read("LC_legacyLM", gCompilerMode.LC_legacyLM);
	Serializer->Read("LC_CformType", gCompilerMode.LC_CformType);
	Serializer->Read("LC_CFormChunkSize", gCompilerMode.LC_CFormChunkSize);
	Serializer->Read("LC_GeomType", gCompilerMode.LC_GeomType);
	Serializer->Read("LC_GeomChunkSize", gCompilerMode.LC_GeomChunkSize);

	Serializer->Read("LC_Skip_Progressive", gCompilerMode.LC_OGF_PROGRESSIVE);
	Serializer->Read("LC_Skip_Striptify", gCompilerMode.LC_OGF_STRIPTIFY);
	Serializer->Read("LC_Skip_Tangents", gCompilerMode.LC_OGF_TANGENT);

	gCompilerMode.LmapsFormat = (LCLightmapFormat)current_format;

	InitializeUIData();
	SDL_Application();

	SaveCompilerCfg();

	xr_delete(Serializer);

	return 0;
}