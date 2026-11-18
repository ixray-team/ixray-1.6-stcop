#include "stdafx.h"
#include "Terrain.h"
#include "../Road/Road.h"
#include <RedImage/RedImage.hpp>

using XRay::Editor::HeightmapUtils::SHeightMap;
using XRay::Editor::TerrainMask::SCellWeights;
using XRay::Editor::TerrainMask::LAYER_COUNT;

enum class ETerrainChunkIDs
{
	HMap,
	Asset,
	Mask
};

CTerrain::CTerrain(LPVOID data, const char* name)
	: inherited(data, name ? name : "terrain"), TerrainObject(new CEditableObject(name ? name : "terrain"))
{
	Construct(data);
	FScale.set(1, 1, 1);
	m_RT_Flags.set(flRT_Visible, true);
}

void CTerrain::Construct(LPVOID data)
{
	FClassID = OBJCLASS_TERRAIN;

	SurfaceShader = "levels\\zaton_earth";
	SurfaceShaderXRLC = "default";
	SurfaceGameMtl = "materials\\earth";
	SurfaceTexture = "terrain\\terrain_mp_atp";

	// Same defaults as the BmmD (terrain) blender details: R grass, G asphalt, B earth, A yantar
	Mask.Layers[0].Texture = "detail\\detail_grnd_grass";
	Mask.Layers[1].Texture = "detail\\detail_grnd_asphalt";
	Mask.Layers[2].Texture = "detail\\detail_grnd_earth";
	Mask.Layers[3].Texture = "detail\\detail_grnd_yantar";
}

CTerrain::~CTerrain()
{
	if (TerrainObject)
	{
		for (auto _M : TerrainObject->m_Meshes)
		{
			_M->RemoveColor(this);
		}
		TerrainObject->EvictObject();
	}

	xr_delete(TerrainObject);
}

void CTerrain::OnUpdateTransform()
{
	inherited::OnUpdateTransform();

	// update bounding volume
	if (TerrainObject)
	{
		m_TBBox.set(TerrainObject->GetBox());
		m_TBBox.xform(_Transform());
	}
}

void CTerrain::InitializeHeightmap(u32 w, u32 h, float fill)
{
	HMap.Create(w, h, fill);
	XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(HMap, TerrainObject, ScaleY, SurfaceTemplate());
	HMap.MarkDirty();
	OnUpdateTransform();
}

void CTerrain::RebuildMesh()
{
	CEditableObject* Old = TerrainObject;
	TerrainObject = new CEditableObject(GetName());
	XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(HMap, TerrainObject, ScaleY, SurfaceTemplate());

	EContext.UI->CommandList[TUI::ECommandListID::NextFrame].push_back
	(
		[Old]()
		{
			Old->EvictObject();
			xr_delete(Old);
		}
	);
	OnUpdateTransform();
}

bool CTerrain::PickHeightmap(const Fvector& Start, const Fvector& Dir, float& Dist, Fvector& WorldPoint)
{
	if (!HMap.IsValid())
	{
		return false;
	}

	Fvector LocalStart, LocalDir;
	_ITransform().transform_tiny(LocalStart, Start);
	_ITransform().transform_dir(LocalDir, Dir);
	const float DirScale = LocalDir.magnitude();
	if (DirScale < EPS_S)
	{
		return false;
	}
	LocalDir.div(DirScale);

	float LocalDist = (Dist < flt_max * 0.5f) ? Dist * DirScale : flt_max;
	Fvector LocalPoint;
	if (!HMap.RayPickLocal(LocalDist, LocalStart, LocalDir, float(ScaleY), &LocalPoint))
	{
		return false;
	}

	_Transform().transform_tiny(WorldPoint, LocalPoint);
	const float WorldDist = Start.distance_to(WorldPoint);
	if (WorldDist > Dist)
	{
		return false;
	}

	Dist = WorldDist;
	return true;
}

void CTerrain::OnHeightmapChanged(int x0, int z0, int x1, int z1)
{
	HMap.MarkDirtyRect(x0, z0, x1, z1);

	// Height / slope rules depend on the relief: refresh the touched area of the mask
	if (HMap.PreviewMode == SHeightMap::pmColors && !Mask.Dirty && Mask.Width == HMap.Width && Mask.Height == HMap.Height)
	{
		Mask.RebuildFinal(HMap, _Transform(), float(ScaleY), x0 - 1, z0 - 1, x1 + 1, z1 + 1);
		UpdateMaskPreview(x0 - 1, z0 - 1, x1 + 1, z1 + 1);
	}
	else
	{
		Mask.Dirty = true;
	}
}

void CTerrain::OnHeightmapChanged()
{
	HMap.UpdateMinMax();
	HMap.MarkDirty();
	Mask.Dirty = true;
	if (IsPreview)
	{
		RebuildMesh();
	}
}

void CTerrain::SetPreviewMode(u32 Mode)
{
	const auto NewMode = SHeightMap::EPreviewMode(clampr(Mode, 0u, 2u));
	if (HMap.PreviewMode != NewMode)
	{
		HMap.PreviewMode = NewMode;
		Mask.Dirty = true;
		HMap.MarkDirty();
	}
}

Fvector CTerrain::CellWorld(int x, int z)
{
	Fvector L;
	L.x = -(x * HMap.StepX() - HMap.HalfX());
	L.z = z * HMap.StepZ() - HMap.HalfZ();
	L.y = HMap.ToLocalY(HMap.GetHeightClamped(x, z), float(ScaleY));
	Fvector W;
	_Transform().transform_tiny(W, L);
	return W;
}

void CTerrain::MarkMaskDirty()
{
	Mask.Dirty = true;
}

void CTerrain::OnRoadsChanged()
{
	if (!Scene)
	{
		return;
	}

	for (CCustomObject* Obj : Scene->ListObj(OBJCLASS_TERRAIN))
	{
		if (CTerrain* T = smart_cast<CTerrain*>(Obj))
		{
			T->Mask.RoadsDirty = true;
		}
	}
}

void CTerrain::OnChangeMaskProp(PropValue* sender)
{
	for (auto& L : Mask.Layers)
	{
		L.EnsureLoaded();
	}
	Mask.Dirty = true;
}

void CTerrain::EnsureMask()
{
	if (!HMap.IsValid())
	{
		return;
	}

	Mask.Resize(HMap.Width, HMap.Height);
	for (auto& L : Mask.Layers)
	{
		L.EnsureLoaded();
	}

	HMap.OverlayColor = &Mask.SurfaceColor;
	HMap.OverlayCoverage = &Mask.SurfaceCoverage;

	if (Mask.RoadsDirty)
	{
		RebuildRoadMask();
		Mask.RoadsDirty = false;
		Mask.Dirty = true;
	}

	if (Mask.Dirty)
	{
		Mask.RebuildFinal(HMap, _Transform(), float(ScaleY));
		UpdateMaskPreview(0, 0, int(HMap.Width) - 1, int(HMap.Height) - 1);
	}
}

void CTerrain::OnMaskPainted(int x0, int z0, int x1, int z1)
{
	Mask.RebuildFinal(HMap, _Transform(), float(ScaleY), x0, z0, x1, z1);
	UpdateMaskPreview(x0, z0, x1, z1);

	// The surface texture preview shows the surface painting as an overlay
	if (HMap.PreviewMode == SHeightMap::pmTexture)
	{
		HMap.MarkDirtyRect(x0, z0, x1, z1);
	}
}

void CTerrain::UpdateMaskPreview(int x0, int z0, int x1, int z1)
{
	if (HMap.PreviewMode != SHeightMap::pmColors)
	{
		return;
	}

	const size_t N = size_t(HMap.Width) * HMap.Height;
	if (HMap.VertexColors.size() != N)
	{
		HMap.VertexColors.assign(N, 0xFF808080);
		x0 = z0 = 0;
		x1 = int(HMap.Width) - 1;
		z1 = int(HMap.Height) - 1;
	}

	x0 = std::max(x0, 0);
	z0 = std::max(z0, 0);
	x1 = std::min(x1, int(HMap.Width) - 1);
	z1 = std::min(z1, int(HMap.Height) - 1);

	xr_parallel_for(z0, z1 + 1, [&](int z)
	{
		for (int x = x0; x <= x1; ++x)
		{
			const Fvector W = CellWorld(x, z);
			HMap.VertexColors[size_t(z) * HMap.Width + x] = Mask.PreviewColor(u32(x), u32(z), W.x, W.z);
		}
	});

	if (x0 == 0 && z0 == 0 && x1 == int(HMap.Width) - 1 && z1 == int(HMap.Height) - 1)
	{
		HMap.MarkDirty();
	}
	else
	{
		HMap.MarkDirtyRect(x0, z0, x1, z1);
	}
}

void CTerrain::RebuildRoadMask()
{
	std::fill(Mask.RoadLayer.begin(), Mask.RoadLayer.end(), u8(255));
	std::fill(Mask.RoadWeight.begin(), Mask.RoadWeight.end(), u8(0));
	std::fill(Mask.ShoulderLayer.begin(), Mask.ShoulderLayer.end(), u8(255));
	std::fill(Mask.ShoulderWeight.begin(), Mask.ShoulderWeight.end(), u8(0));

	const Fmatrix& IM = _ITransform();

	auto Smooth = [](float E0, float E1, float X)
	{
		if (E1 <= E0)
		{
			return X >= E1 ? 1.f : 0.f;
		}
		const float T = clampr((X - E0) / (E1 - E0), 0.f, 1.f);
		return T * T * (3.f - 2.f * T);
	};

	auto GridRect = [&](const Fvector& A, const Fvector& B, float Expand, int& X0, int& Z0, int& X1, int& Z1)
	{
		float MinGx = flt_max, MinGz = flt_max, MaxGx = -flt_max, MaxGz = -flt_max;
		const float Xs[2] = {std::min(A.x, B.x) - Expand, std::max(A.x, B.x) + Expand};
		const float Zs[2] = {std::min(A.z, B.z) - Expand, std::max(A.z, B.z) + Expand};
		for (float X : Xs)
		{
			for (float Z : Zs)
			{
				Fvector W = {X, (A.y + B.y) * 0.5f, Z};
				Fvector L;
				IM.transform_tiny(L, W);
				float Gx, Gz;
				HMap.LocalToGrid(L, Gx, Gz);
				MinGx = std::min(MinGx, Gx);
				MaxGx = std::max(MaxGx, Gx);
				MinGz = std::min(MinGz, Gz);
				MaxGz = std::max(MaxGz, Gz);
			}
		}
		X0 = std::max(0, iFloor(MinGx));
		Z0 = std::max(0, iFloor(MinGz));
		X1 = std::min(int(HMap.Width) - 1, iCeil(MaxGx));
		Z1 = std::min(int(HMap.Height) - 1, iCeil(MaxGz));
	};

	for (CCustomObject* Obj : Scene->ListObj(OBJCLASS_ROAD))
	{
		CRoad* Road = smart_cast<CRoad*>(Obj);
		if (!Road || !Road->Visible() || !Road->Spline.IsValid())
		{
			continue;
		}

		const bool HasRoad = Road->MaskRoadLayer < LAYER_COUNT;
		const bool HasShoulder = Road->MaskShoulderLayer < LAYER_COUNT && Road->ShoulderWidth > 0.01f;
		if (!HasRoad && !HasShoulder)
		{
			continue;
		}

		xr_vector<XRay::Editor::Spline::SSplineSample> Samples;
		Road->Spline.Sample(std::max(Road->SampleStep * 0.5f, 0.25f), Samples);

		const Fmatrix& RM = Road->_Transform();
		xr_vector<Fvector> Pos(Samples.size());
		for (size_t i = 0; i < Samples.size(); ++i)
		{
			RM.transform_tiny(Pos[i], Samples[i].Pos);
		}

		const float HalfBlend = std::max(Road->MaskBlend, 0.f) * 0.5f;
		const float Shoulder = HasShoulder ? Road->ShoulderWidth : 0.f;

		for (size_t s = 0; s + 1 < Samples.size(); ++s)
		{
			const Fvector& A = Pos[s];
			const Fvector& B = Pos[s + 1];
			const float HwA = Samples[s].Width * 0.5f;
			const float HwB = Samples[s + 1].Width * 0.5f;

			int X0, Z0, X1, Z1;
			GridRect(A, B, std::max(HwA, HwB) + Shoulder + HalfBlend + 1.f, X0, Z0, X1, Z1);

			const float Dx = B.x - A.x;
			const float Dz = B.z - A.z;
			const float Len2 = Dx * Dx + Dz * Dz;

			for (int z = Z0; z <= Z1; ++z)
			{
				for (int x = X0; x <= X1; ++x)
				{
					const Fvector W = CellWorld(x, z);
					float T = 0.f;
					if (Len2 > EPS_S)
					{
						T = clampr(((W.x - A.x) * Dx + (W.z - A.z) * Dz) / Len2, 0.f, 1.f);
					}
					const float Px = A.x + Dx * T;
					const float Pz = A.z + Dz * T;
					const float D = _sqrt((W.x - Px) * (W.x - Px) + (W.z - Pz) * (W.z - Pz));
					const float Hw = HwA + (HwB - HwA) * T;
					const size_t Idx = size_t(z) * HMap.Width + x;

					if (HasRoad)
					{
						const float Wr = 1.f - Smooth(Hw - HalfBlend, Hw + HalfBlend, D);
						const u8 V = u8(iFloor(Wr * 255.f + 0.5f));
						if (V > Mask.RoadWeight[Idx])
						{
							Mask.RoadWeight[Idx] = V;
							Mask.RoadLayer[Idx] = u8(Road->MaskRoadLayer);
						}
					}

					if (HasShoulder)
					{
						const float Ws = 1.f - Smooth(Hw + Shoulder - HalfBlend, Hw + Shoulder + HalfBlend, D);
						const u8 V = u8(iFloor(Ws * 255.f + 0.5f));
						if (V > Mask.ShoulderWeight[Idx])
						{
							Mask.ShoulderWeight[Idx] = V;
							Mask.ShoulderLayer[Idx] = u8(Road->MaskShoulderLayer);
						}
					}
				}
			}
		}
	}
}

xr_string CTerrain::MaskBaseName() const
{
	// Own texture per terrain object, never overwrite a stock texture
	xr_string Name = GetName();
	for (char& C : Name)
	{
		if (C == ' ' || C == '\\' || C == '/')
		{
			C = '_';
		}
	}
	xr_strlwr(Name);
	if (Name.rfind("terrain", 0) != 0)
	{
		Name = "terrain_" + Name;
	}
	return "terrain\\" + Name;
}

bool CTerrain::BakeMask(u32 Size, xr_string& OutColor, xr_string& OutMask)
{
	if (!HMap.IsValid() || Size < 2)
	{
		return false;
	}

	Mask.RoadsDirty = true;
	EnsureMask();

	RedImageTool::RedImage ColorImg(Size, Size);
	RedImageTool::RedImage MaskImg(Size, Size);

	const float Last = float(Size - 1);
	for (u32 py = 0; py < Size; ++py)
	{
		for (u32 px = 0; px < Size; ++px)
		{
			// Same UV as the terrain mesh: u = 1 - x / (W - 1), v = 1 - z / (H - 1)
			const float U = float(px) / Last;
			const float V = float(py) / Last;
			const float Gx = (1.f - U) * float(HMap.Width - 1);
			const float Gz = (1.f - V) * float(HMap.Height - 1);

			const SCellWeights C = Mask.SampleFinal(Gx, Gz);
			u8 Packed[4];
			XRay::Editor::TerrainMask::PackWeights(C, Packed);
			MaskImg.SetPixel(RedImageTool::RedColor(Packed[0] / 255.f, Packed[1] / 255.f, Packed[2] / 255.f, Packed[3] / 255.f), px, py);

			// Color map: layer average colors (the details add the high frequencies in game)
			float R = 0, G = 0, B = 0;
			for (u32 i = 0; i < LAYER_COUNT; ++i)
			{
				const u32 Avg = Mask.Layers[i].AvgColor;
				R += color_get_R(Avg) * C.W[i];
				G += color_get_G(Avg) * C.W[i];
				B += color_get_B(Avg) * C.W[i];
			}
			const u32 LayerColor = color_rgba(u32(clampr(R, 0.f, 255.f)), u32(clampr(G, 0.f, 255.f)), u32(clampr(B, 0.f, 255.f)), 255);
			float SurfaceCov;
			const u32 Final = Mask.SampleSurface(Gx, Gz, LayerColor, SurfaceCov);
			ColorImg.SetPixel(RedImageTool::RedColor(color_get_R(Final) / 255.f, color_get_G(Final) / 255.f, color_get_B(Final) / 255.f, 1.f), px, py);
		}
	}

	// Write the sources straight into the texture library (rawdata	extures)
	const xr_string ColorName = MaskBaseName();
	const xr_string MaskName = ColorName + "_mask";
	string_path ColorPath, MaskPath;
	FS.update_path(ColorPath, "$textures$", (ColorName + ".tga").c_str());
	FS.update_path(MaskPath, "$textures$", (MaskName + ".tga").c_str());

	std::error_code Ec;
	std::filesystem::create_directories(std::filesystem::path(ColorPath).parent_path(), Ec);

	if (!ColorImg.SaveToTga(ColorPath) || !MaskImg.SaveToTga(MaskPath))
	{
		Msg("! Terrain [%s]: failed to write [%s] / [%s]", GetName(), ColorPath, MaskPath);
		return false;
	}
	OutColor = ColorName;
	OutMask = MaskName;

	// Thumbnails with the right parameters, then build the game .dds and reload them in the renderer
	const time_t Age = time(nullptr);
	auto MakeTexture = [&](const xr_string& Name, const char* Path, bool IsMask)
	{
		FS.TryLoad(Path);
		FS.set_file_age(Path, Age);

		ETextureThumbnail* THM = new ETextureThumbnail(Name.c_str(), false);
		ImageLib.CreateTextureThumbnail(THM, Name, nullptr, true);
		if (IsMask)
		{
			// Exact layer weights: no compression
			THM->_Format().type = STextureParams::ttImage;
			THM->_Format().fmt = STextureParams::tfRGBA;
		}
		else
		{
			THM->_Format().type = STextureParams::ttTerrain;
			THM->_Format().fmt = STextureParams::tfDXT1;
		}
		THM->_Format().flags.set(STextureParams::flGenerateMipMaps, true);
		THM->Save(int(Age));
		xr_delete(THM);

		ImageLib.SynchronizeTexture(Name.c_str(), Age);
	};
	MakeTexture(ColorName, ColorPath, false);
	MakeTexture(MaskName, MaskPath, true);

	// Use the baked color map as the terrain surface: the mesh shader picks <texture>_mask by itself
	SurfaceTexture = ColorName.c_str();
	ApplySurfaceTemplate();
	RebuildMesh();
	ReloadPreviewTexture();

	Msg("Terrain [%s]: baked and applied [%s] + [%s]", GetName(), ColorName.c_str(), MaskName.c_str());
	return true;
}

void CTerrain::ReloadPreviewTexture()
{
	m_PreviewTexLoaded = nullptr;
	m_PreviewTexTried = false;
	UpdatePreviewTexture();
}

void CTerrain::UpdatePreviewTexture()
{
	if (HMap.PreviewMode != SHeightMap::pmTexture)
	{
		return;
	}

	if (m_PreviewTexTried && m_PreviewTexLoaded == SurfaceTexture)
	{
		return;
	}

	m_PreviewTexTried = true;
	m_PreviewTexLoaded = SurfaceTexture;

	xr_vector<u32> Pixels;
	u32 W = 0, H = 0;
	if (!SurfaceTexture.size() || !ImageLib.LoadTextureData(*SurfaceTexture, Pixels, W, H))
	{
		if (SurfaceTexture.size())
		{
			Msg("! Terrain [%s]: can't load preview texture [%s]", GetName(), *SurfaceTexture);
		}
		HMap.SetPreviewTexture({}, 0, 0);
		return;
	}

	HMap.SetPreviewTexture(std::move(Pixels), W, H);
}

void CTerrain::OnChangeTexturePreview(PropValue* sender)
{
	Mask.Dirty = true;
	HMap.MarkDirty();
}

void CTerrain::OnChangePreview(PropValue* sender)
{
	if (IsPreview)
	{
		RebuildMesh();
	}
}

void CTerrain::SaveStream(IWriter& F)
{
	F.open_chunk(ETerrainChunkIDs::HMap);
	HMap.SaveSteam(&F);
	F.close_chunk();

	F.open_chunk(ETerrainChunkIDs::Asset);
	F.w_stringZ(*SurfaceShader);
	F.w_stringZ(*SurfaceShaderXRLC);
	F.w_stringZ(*SurfaceGameMtl);
	F.w_stringZ(*SurfaceTexture);
	F.close_chunk();

	F.open_chunk(ETerrainChunkIDs::Mask);
	Mask.Save(F);
	F.close_chunk();

	inherited::SaveStream(F);
}

bool CTerrain::LoadStream(IReader& F)
{
	if (F.length() == 0)
	{
		return false;
	}

	if (IReader* HMapChunk = F.open_chunk(ETerrainChunkIDs::HMap))
	{
		HMap.LoadSteam(HMapChunk);
		HMapChunk->close();
	}

	if (IReader* AssetChunk = F.open_chunk(ETerrainChunkIDs::Asset))
	{
		char Buf[512];
		AssetChunk->r_stringZ(Buf, sizeof(Buf));
		SurfaceShader = Buf;
		AssetChunk->r_stringZ(Buf, sizeof(Buf));
		SurfaceShaderXRLC = Buf;
		AssetChunk->r_stringZ(Buf, sizeof(Buf));
		SurfaceGameMtl = Buf;
		AssetChunk->r_stringZ(Buf, sizeof(Buf));
		SurfaceTexture = Buf;
		AssetChunk->close();
	}

	if (IReader* MaskChunk = F.open_chunk(ETerrainChunkIDs::Mask))
	{
		Mask.Load(*MaskChunk);
		MaskChunk->close();
	}

	inherited::LoadStream(F);

	if (!HMap.IsValid())
	{
		return false;
	}

	// A mask saved for another resolution can't be reused
	Mask.Resize(HMap.Width, HMap.Height);
	Mask.Dirty = true;
	Mask.RoadsDirty = true;

	if (TerrainObject && !TerrainObject->m_Meshes.empty())
	{
		RebuildMesh();
	}
	else
	{
		XRay::Editor::HeightmapUtils::GenerateMeshByHeightmap(HMap, TerrainObject, ScaleY, SurfaceTemplate());
		OnUpdateTransform();
	}

	return true;
}

XRay::Editor::HeightmapUtils::STerrainSurfaceTemplate CTerrain::SurfaceTemplate() const
{
	return {*SurfaceShader, *SurfaceShaderXRLC, *SurfaceGameMtl, *SurfaceTexture};
}

void CTerrain::OnDeviceDestroy()
{
	HMap.RenderData.InvalidateGpu();
	inherited::OnDeviceDestroy();
}

void CTerrain::OnFrame()
{
	CCustomObject::OnFrame();

	auto fequal = [](float a, float b, float eps = EPS)
	{
		return fabs(a - b) < eps;
	};

	if (!fequal(HMap.Pos.x, FPosition.x) || !fequal(HMap.Pos.y, FPosition.y) || !fequal(HMap.Pos.z, FPosition.z))
	{
		HMap.Pos = FPosition;
		HMap.MarkDirty();
	}

	if (!fequal(HMap.Size.x, FScale.x) || !fequal(HMap.Size.y, FScale.y) || !fequal(HMap.Size.z, FScale.z))
	{
		HMap.Size = FScale;
		HMap.MarkDirty();
	}
}

bool CTerrain::RayPick(float& dist, const Fvector& S, const Fvector& D, SRayPickInfo* pinf)
{
	if (!IsLoaded && (!pinf || !pinf->IsForcePickup))
	{
		return false;
	}

	if (LTools->GetTarget() == OBJCLASS_TERRAIN)
	{
		Fvector Point;
		if (PickHeightmap(S, D, dist, Point))
		{
			if (pinf)
			{
				pinf->s_obj = this;
				pinf->pt = Point;
				pinf->inf.range = dist;
			}
			return true;
		}
	}
	else
	{
		if (TerrainObject->RayPick(dist, S, D, _ITransform(), pinf))
		{
			if (pinf)
			{
				pinf->s_obj = this;
			}
			return true;
		}
	}

	return false;
}

void CTerrain::Render(int priority, bool strictB2F)
{
	if (LTools->GetTarget() == OBJCLASS_TERRAIN && !IsPreview)
	{
		if (priority == 1 && !strictB2F)
		{
			if (HMap.PreviewMode == SHeightMap::pmColors)
			{
				EnsureMask();
			}
			else
			{
				UpdatePreviewTexture();
				if (HMap.PreviewMode == SHeightMap::pmTexture && Mask.Width == HMap.Width && Mask.Height == HMap.Height)
				{
					HMap.OverlayColor = &Mask.SurfaceColor;
					HMap.OverlayCoverage = &Mask.SurfaceCoverage;
				}
			}
			HMap.Draw(_Transform(), float(ScaleY));

			if (Selected())
			{
				EDevice->SetShader(GEditorDraw.WireShader);
				RCache.set_xform_world(_Transform());
				u32 clr = Locked() ? 0xFFFF0000 : 0xFFFFFFFF;
				DU_impl.DrawSelectionBoxB(TerrainObject->GetBox(), &clr);
			}
		}
	}
	else
	{
		TerrainObject->Render(this, _Transform(), priority, strictB2F);
	}
}

void CTerrain::Move(Fvector& amount)
{
	inherited::Move(amount);
	HMap.MarkDirty();
	HMap.Pos = FPosition;
}

void CTerrain::Scale(Fvector& amount)
{
	inherited::Scale(amount);
	HMap.MarkDirty();
	HMap.Size = FScale;
}

void CTerrain::FillProp(const char* pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);

	PHelper().CreateCaption(items, "Height Map\\Resolution", (xr_string::ToString(HMap.Width) + " x " + xr_string::ToString(HMap.Height)).c_str());
	PHelper().CreateCaption(items, "Height Map\\World Size", (xr_string::ToString(int((HMap.Width - 1) * HMap.StepX())) + " x " + xr_string::ToString(int((HMap.Height - 1) * HMap.StepZ()))).c_str());

	static xr_token PreviewModeTokens[] =
	{
		{"Height", SHeightMap::pmHeight},
		{"Surface Texture", SHeightMap::pmTexture},
		{"Material Mask", SHeightMap::pmColors},
		{nullptr, 0},
	};
	PropValue* TexPreviewVal = PHelper().CreateToken(items, "Height Map\\Preview Mode", (u8*)&HMap.PreviewMode, PreviewModeTokens);
	TexPreviewVal->OnChangeEvent.bind(this, &CTerrain::OnChangeTexturePreview);

	// Material mask layers (R, G, B, A of <texture>_mask)
	static const char* LayerNames[LAYER_COUNT] = {"Layer R (base)", "Layer G", "Layer B", "Layer A"};
	for (u32 i = 0; i < LAYER_COUNT; ++i)
	{
		auto& L = Mask.Layers[i];
		const xr_string Key = xr_string("Material Mask\\") + LayerNames[i];
		PropValue* V;
		V = PHelper().CreateChoose(items, PrepareKey(Key.c_str(), "Texture"), &L.Texture, smTexture);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Tile Size"), &L.TileSize, 0.1f, 200.f, 0.5f, 2);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		if (i == 0)
		{
			continue; // The base layer fills what the other layers leave
		}
		V = PHelper().CreateBool(items, PrepareKey(Key.c_str(), "Auto\\Enabled"), &L.AutoEnabled);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Auto\\Height Min"), &L.HeightMin, -10000.f, 10000.f, 0.5f, 2);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Auto\\Height Max"), &L.HeightMax, -10000.f, 10000.f, 0.5f, 2);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Auto\\Height Blend"), &L.HeightBlend, 0.f, 100.f, 0.1f, 2);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Auto\\Slope Min (deg)"), &L.SlopeMin, 0.f, 90.f, 0.5f, 1);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Auto\\Slope Max (deg)"), &L.SlopeMax, 0.f, 90.f, 0.5f, 1);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Auto\\Slope Blend (deg)"), &L.SlopeBlend, 0.f, 45.f, 0.5f, 1);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Auto\\Noise"), &L.Noise, 0.f, 1.f, 0.01f, 2);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
		V = PHelper().CreateFloat(items, PrepareKey(Key.c_str(), "Auto\\Noise Size"), &L.NoiseScale, 0.5f, 500.f, 0.5f, 1);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeMaskProp);
	}

	PropValue* PreviewVal = PHelper().CreateBool(items, "Height Map\\Preview", &IsPreview);
	PreviewVal->OnChangeEvent.bind(this, &CTerrain::OnChangePreview);

	S32Value* ScaleEdit = PHelper().CreateNumeric(items, "Height Map\\Multiply Y", &ScaleY);
	ScaleEdit->OnAfterEditEvent.bind(this, &CTerrain::OnChangeHMData);

	shared_str Pref1 = PrepareKey(pref, "Surfaces").c_str();
	shared_str Pref2 = PrepareKey(Pref1.c_str(), "terrain").c_str();
	{
		PropValue* V;
		V = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Texture"), &SurfaceTexture, smTexture);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeSurfaceProp);
		V = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Shader"), &SurfaceShader, smEShader);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeSurfaceProp);
		V = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Compile"), &SurfaceShaderXRLC, smCShader);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeSurfaceProp);
		V = PHelper().CreateChoose(items, PrepareKey(Pref2.c_str(), "Game Mtl"), &SurfaceGameMtl, smGameMaterial);
		V->OnChangeEvent.bind(this, &CTerrain::OnChangeSurfaceProp);
	}
}

void CTerrain::OnChangeSurfaceProp(PropValue* sender)
{
	ApplySurfaceTemplate();
}

void CTerrain::ApplySurfaceTemplate()
{
	if (!TerrainObject || TerrainObject->m_Surfaces.empty())
	{
		return;
	}

	CSurface* S = TerrainObject->m_Surfaces.front();
	if (SurfaceTexture[0])
	{
		S->SetTexture(*SurfaceTexture);
	}
	if (SurfaceShader[0])
	{
		S->SetShader(*SurfaceShader);
	}
	if (SurfaceShaderXRLC[0])
	{
		S->SetShaderXRLC(*SurfaceShaderXRLC);
	}
	if (SurfaceGameMtl[0])
	{
		S->SetGameMtl(*SurfaceGameMtl);
	}
}

bool CTerrain::OnChangeHMData(PropValue* sender, int& NewValue)
{
	if (NewValue < 0)
	{
		return false;
	}

	if (NewValue == m_AppliedHMScale)
	{
		return true;
	}

	m_AppliedHMScale = NewValue;
	ScaleY = NewValue;

	HMap.MarkDirty();

	if (IsPreview)
	{
		RebuildMesh();
	}

	return true;
}

void CTerrain::BoxQuery(SPickQuery& pinf)
{
	if (!TerrainObject)
	{
		return;
	}

	TerrainObject->BoxQuery(_Transform(), _ITransform(), pinf);
}

void CTerrain::RayQuery(SPickQuery& pinf)
{
	if (!TerrainObject)
	{
		return;
	}

	TerrainObject->RayQuery(_Transform(), _ITransform(), pinf);
}

bool CTerrain::GetBox(Fbox& box)
{
	if (!TerrainObject)
	{
		return false;
	}
	box.set(m_TBBox);
	return true;
}

bool CTerrain::BoxPick(const Fbox& box, SBoxPickInfoVec& pinf)
{
	if (!TerrainObject)
	{
		return false;
	}

	return TerrainObject->BoxPick(this, box, _ITransform(), pinf);
}

bool CTerrain::GetUTBox(Fbox& box)
{
	if (!TerrainObject)
	{
		return false;
	}
	box.set(TerrainObject->GetBox());

	return true;
}
