#include "stdafx.h"
#include "ESceneTerrainTools.h"
#include "ESceneTerrainControls.h"
#include "UI/Tools/UITerrainTool.h"

using XRay::Editor::HeightmapUtils::SHeightMap;

namespace TerrainBrush
{
	float Hash(int X, int Z)
	{
		u32 N = u32(X) * 374761393u + u32(Z) * 668265263u;
		N = (N ^ (N >> 13)) * 1274126177u;
		N ^= N >> 16;
		return float(N & 0xFFFF) / 65535.f * 2.f - 1.f;
	}

	float ValueNoise(float X, float Z)
	{
		const int X0 = iFloor(X);
		const int Z0 = iFloor(Z);
		float Fx = X - X0;
		float Fz = Z - Z0;
		Fx = Fx * Fx * (3.f - 2.f * Fx);
		Fz = Fz * Fz * (3.f - 2.f * Fz);

		const float A = Hash(X0, Z0) + (Hash(X0 + 1, Z0) - Hash(X0, Z0)) * Fx;
		const float B = Hash(X0, Z0 + 1) + (Hash(X0 + 1, Z0 + 1) - Hash(X0, Z0 + 1)) * Fx;
		return A + (B - A) * Fz;
	}

	float Fbm(float X, float Z)
	{
		float Sum = 0.f;
		float Amp = 0.5f;
		for (int i = 0; i < 4; ++i)
		{
			Sum += ValueNoise(X, Z) * Amp;
			X *= 2.03f;
			Z *= 2.03f;
			Amp *= 0.5f;
		}
		return Sum;
	}

	float TransformScale(const Fmatrix& M)
	{
		const float S = M.i.magnitude();
		return S > EPS_S ? S : 1.f;
	}

	u32 ModeColor(ESceneTerrainTool::ETerrainBrushMode Mode, bool Invert)
	{
		switch (Mode)
		{
		case ESceneTerrainTool::bmRaise:
			return Invert ? 0xFFFF6040 : 0xFF60FF60;
		case ESceneTerrainTool::bmLower:
			return Invert ? 0xFF60FF60 : 0xFFFF6040;
		case ESceneTerrainTool::bmSmooth:
			return 0xFF60B0FF;
		case ESceneTerrainTool::bmFlatten:
			return 0xFFFFE040;
		case ESceneTerrainTool::bmNoise:
			return 0xFFD070FF;
		case ESceneTerrainTool::bmHole:
			return Invert ? 0xFFFFFFFF : 0xFF202020;
		case ESceneTerrainTool::bmPaint:
			return Invert ? 0xFFFF8080 : 0xFFFFFFFF;
		default:
			return 0xFFFFFFFF;
		}
	}
}

const char* ESceneTerrainTool::BrushModeName(ETerrainBrushMode Mode)
{
	switch (Mode)
	{
	case bmRaise:
		return "Raise";
	case bmLower:
		return "Lower";
	case bmSmooth:
		return "Smooth";
	case bmFlatten:
		return "Flatten";
	case bmNoise:
		return "Noise";
	case bmHole:
		return "Hole";
	case bmPaint:
		return "Paint";
	default:
		return "?";
	}
}

ESceneTerrainTool::ESceneTerrainTool() :
	ESceneCustomOTool(OBJCLASS_TERRAIN)
{
	m_BrushSize = 10.f;
	m_BrushStrength = 0.02f;
	m_BrushHardness = 0.3f;
	m_BrushSpacing = 0.25f;
	m_NoiseScale = 16.f;
	m_BrushMode = bmRaise;
	m_BrushInvert = false;
	m_FlattenPickOnStroke = true;
	m_FlattenTarget = 0.5f;
	m_PaintLayer = 1;
	m_PaintMask = true;
	m_PaintSurface = true;
	m_SurfaceSource = 0;
	m_SurfaceColor.set(0.45f, 0.38f, 0.28f, 1.f);
	m_BrushActive = false;
	m_BrushPos.set(0, 0, 0);
	m_EditedTerrain = nullptr;
	m_HoverTerrain = nullptr;
	m_LastStampPos.set(0, 0, 0);
	m_HasLastStamp = false;
	m_StrokeInvert = false;
	m_StrokePlaneY = 0.f;
	Clear();
}

ESceneTerrainTool::~ESceneTerrainTool()
{
}

void ESceneTerrainTool::Clear(bool bSpecific)
{
	inherited::Clear(bSpecific);

	lcontrol_last_idx	= 0;
	lcontrols.clear		();
	m_BrushActive = false;
	m_EditedTerrain = nullptr;
	m_HoverTerrain = nullptr;
	m_HasLastStamp = false;
}

void ESceneTerrainTool::BeforeRender()
{

}

void ESceneTerrainTool::AfterRender()
{
}

void ESceneTerrainTool::OnRender(int priority, bool strictB2F)
{
	for (ObjectIt it=m_Objects.begin(); it!=m_Objects.end(); it++)
    	(*it)->Render(priority,strictB2F);

	if (priority == 1 && !strictB2F && GetSubTarget(0) == estTerrainSculpt)
		RenderBrush();
}

void ESceneTerrainTool::OnControlAppendClick(ButtonValue* sender, bool& bDataModified, bool& bSafe)
{
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	bDataModified = true;
}

void ESceneTerrainTool::OnControlRenameRemoveClick(ButtonValue* V, bool& bDataModified, bool& bSafe)
{
}

void ESceneTerrainTool::OnDeactivate()
{
	EndSculpt();

	// The heightmap is the source of truth. The generated mesh can be stale
	// when sculpting without Preview, so rebuild it when leaving Terrain mode.
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
	{
		CTerrain* t = (CTerrain*)(*it);
		if (t && t->TerrainObject)
		{
			t->RebuildMesh();
		}
	}

	m_HoverTerrain = nullptr;
	m_BrushActive = false;
	inherited::OnDeactivate();
}

void ESceneTerrainTool::FillProp(const char* pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);
}

bool ESceneTerrainTool::_AppendObject(CCustomObject* object)
{
	CTerrain* terr = (CTerrain*)object;

	// The heightmap may already exist (e.g. a freshly created plane)
	if (!terr->HMap.IsValid())
	{
		bool loaded = false;
		if (object->GetName())
		{
			string_path Path = {};
			FS.update_path(Path, "$server_data_root$", object->GetName());
			xr_strcat(Path, ".r16");

			if (FS.TryLoad(Path))
			{
				IReader* Stream = FS.r_open(Path);
				if (Stream)
				{
					loaded = terr->LoadStream(*Stream);
					FS.r_close(Stream);
				}
			}
		}

		if (!loaded)
		{
			// No external .r16, create a default plane
			terr->InitializeHeightmap(129, 129, 0.5f);
		}
	}

	return inherited::_AppendObject(object);
}

bool ESceneTerrainTool::Validate(bool full_test)
{
	return true;
}

void ESceneTerrainTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	AddControl(new TUI_ControlTerrainSculpt(estTerrainSculpt, etaAdd, this));
	pForm = new UITerrainTool();
	((UITerrainTool*)pForm)->tool = this;
}

void ESceneTerrainTool::RemoveControls()
{
	inherited::RemoveControls();
}

CTerrain* ESceneTerrainTool::PickTerrain(float& dist, Fvector& point)
{
	CTerrain* best = nullptr;
	float bestDist = EContext.UI->ZFar();

	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
	{
		CTerrain* t = (CTerrain*)(*it);
		if (!t->Visible() || t->Locked())
		{
			continue;
		}

		float d = bestDist;
		Fvector p;
		if (t->PickHeightmap(EContext.UI->m_CurrentRStart, EContext.UI->m_CurrentRDir, d, p) && d < bestDist)
		{
			bestDist = d;
			best = t;
			point = p;
		}
	}

	dist = bestDist;
	return best;
}

void ESceneTerrainTool::PickFlattenTarget(CTerrain* obj, const Fvector& worldPoint)
{
	SHeightMap& H = obj->HMap;
	Fvector Local;
	obj->_ITransform().transform_tiny(Local, worldPoint);
	float Gx, Gz;
	H.LocalToGrid(Local, Gx, Gz);
	const float Value = H.Sample(Gx, Gz);
	if (Value > 0.f)
	{
		m_FlattenTarget = Value;
	}
}

void ESceneTerrainTool::BeginSculpt(CTerrain* obj, const Fvector& worldPoint, bool invert)
{
	m_EditedTerrain = obj;
	m_BrushPos = worldPoint;
	m_BrushActive = true;
	m_HasLastStamp = false;
	m_StrokePlaneY = worldPoint.y;

	if (m_BrushMode == bmFlatten && m_FlattenPickOnStroke)
	{
		PickFlattenTarget(obj, worldPoint);
	}

	if (m_BrushMode == bmPaint)
	{
		// Painting needs an up-to-date mask and should be visible right away:
		// the height tint shows neither, the surface texture view shows the surface paint as an overlay
		if (obj->GetPreviewMode() == XRay::Editor::HeightmapUtils::SHeightMap::pmHeight)
		{
			obj->SetPreviewMode(XRay::Editor::HeightmapUtils::SHeightMap::pmColors);
		}
		obj->EnsureMask();
	}

	StrokeTo(obj, worldPoint, invert);
}

void ESceneTerrainTool::StrokeTo(CTerrain* obj, const Fvector& worldPoint, bool invert)
{
	if (!m_HasLastStamp)
	{
		SculptTerrain(obj, worldPoint, invert);
		m_LastStampPos = worldPoint;
		m_HasLastStamp = true;
		return;
	}

	// Evenly spaced stamps along the stroke: the result no longer depends on mouse speed
	const float Spacing = std::max(m_BrushSize * m_BrushSpacing, 0.05f);
	Fvector Delta;
	Delta.sub(worldPoint, m_LastStampPos);
	const float Length = Delta.magnitude();
	if (Length < Spacing)
	{
		return;
	}

	// The cursor jumped far (e.g. the pick ray fell through a hole onto distant terrain):
	// start a new stroke segment instead of painting along the whole gap
	if (Length > std::max(m_BrushSize * 4.f, Spacing * 8.f))
	{
		SculptTerrain(obj, worldPoint, invert);
		m_LastStampPos = worldPoint;
		return;
	}

	Delta.div(Length);
	const int Count = std::min(iFloor(Length / Spacing), 256);
	for (int i = 1; i <= Count; ++i)
	{
		Fvector P;
		P.mad(m_LastStampPos, Delta, Spacing * i);
		SculptTerrain(obj, P, invert);
	}
	m_LastStampPos.mad(m_LastStampPos, Delta, Spacing * Count);
}

void ESceneTerrainTool::EndSculpt()
{
	if (!m_EditedTerrain)
	{
		return;
	}

	CTerrain* Obj = m_EditedTerrain;
	m_EditedTerrain = nullptr;
	m_HasLastStamp = false;

	Obj->HMap.UpdateMinMax();
	if (Obj->IsMeshPreview())
	{
		Obj->RebuildMesh();
	}
	Scene->UndoSave();
}

float ESceneTerrainTool::BrushFalloff(float distance) const
{
	if (distance >= m_BrushSize)
	{
		return 0.f;
	}

	const float D = distance / m_BrushSize;
	const float Hard = clampr(m_BrushHardness, 0.f, 0.99f);
	if (D <= Hard)
	{
		return 1.f;
	}

	float T = (D - Hard) / (1.f - Hard);
	T = 1.f - T;
	return T * T * (3.f - 2.f * T);
}

void ESceneTerrainTool::SculptTerrain(CTerrain* obj, const Fvector& worldPoint, bool invert)
{
	SHeightMap& H = obj->HMap;
	if (!H.IsValid())
	{
		return;
	}

	Fvector Local;
	obj->_ITransform().transform_tiny(Local, worldPoint);

	// Brush radius is in world units, the grid lives in local units
	const float WorldScale = TerrainBrush::TransformScale(obj->_Transform());
	const float StepX = H.StepX() * WorldScale;
	const float StepZ = H.StepZ() * WorldScale;

	float Gx, Gz;
	H.LocalToGrid(Local, Gx, Gz);

	const int MinX = std::max(0, iFloor(Gx - m_BrushSize / StepX));
	const int MaxX = std::min(int(H.Width) - 1, iCeil(Gx + m_BrushSize / StepX));
	const int MinZ = std::max(0, iFloor(Gz - m_BrushSize / StepZ));
	const int MaxZ = std::min(int(H.Height) - 1, iCeil(Gz + m_BrushSize / StepZ));
	if (MinX > MaxX || MinZ > MaxZ)
	{
		return;
	}

	// Smooth reads from a snapshot so the result does not depend on iteration order
	const int SX0 = std::max(0, MinX - 1);
	const int SZ0 = std::max(0, MinZ - 1);
	const int SX1 = std::min(int(H.Width) - 1, MaxX + 1);
	const int SZ1 = std::min(int(H.Height) - 1, MaxZ + 1);
	const int SW = SX1 - SX0 + 1;
	if (m_BrushMode == bmSmooth)
	{
		m_SmoothScratch.resize(size_t(SW) * (SZ1 - SZ0 + 1));
		for (int z = SZ0; z <= SZ1; ++z)
		{
			for (int x = SX0; x <= SX1; ++x)
			{
				m_SmoothScratch[size_t(z - SZ0) * SW + (x - SX0)] = H.GetHeight(x, z);
			}
		}
	}

	if (m_BrushMode == bmPaint)
	{
		auto& Mask = obj->Mask;
		if (Mask.Width != H.Width || Mask.Height != H.Height)
		{
			obj->EnsureMask();
		}

		const float PaintBlend = clampr(m_BrushStrength * 10.f, 0.f, 1.f);
		const auto& Layer = Mask.Layers[std::min(m_PaintLayer, XRay::Editor::TerrainMask::LAYER_COUNT - 1)];
		const u32 SolidColor = m_SurfaceColor.get();
		for (int z = MinZ; z <= MaxZ; ++z)
		{
			for (int x = MinX; x <= MaxX; ++x)
			{
				const float Dx = (x - Gx) * StepX;
				const float Dz = (z - Gz) * StepZ;
				const float Falloff = BrushFalloff(_sqrt(Dx * Dx + Dz * Dz));
				if (Falloff <= 0.f)
				{
					continue;
				}

				if (m_PaintMask)
				{
					Mask.PaintCell(u32(x), u32(z), m_PaintLayer, PaintBlend * Falloff, invert);
				}

				if (m_PaintSurface)
				{
					u32 Color = SolidColor;
					if (m_SurfaceSource == 0)
					{
						// The layer texture, tiled in world space like in game
						const Fvector W = obj->CellWorld(x, z);
						Color = Layer.SampleTiled(W.x, W.z);
					}
					Mask.PaintSurfaceCell(u32(x), u32(z), Color, PaintBlend * Falloff, invert);
				}
			}
		}
		obj->OnMaskPainted(MinX, MinZ, MaxX, MaxZ);
		return;
	}

	const float Sign = invert ? -1.f : 1.f;
	const float Blend = clampr(m_BrushStrength * 10.f, 0.f, 1.f);
	const float NoiseFreq = 1.f / std::max(m_NoiseScale, 0.1f);

	for (int z = MinZ; z <= MaxZ; ++z)
	{
		for (int x = MinX; x <= MaxX; ++x)
		{
			const float Dx = (x - Gx) * StepX;
			const float Dz = (z - Gz) * StepZ;
			const float Falloff = BrushFalloff(_sqrt(Dx * Dx + Dz * Dz));
			if (Falloff <= 0.f)
			{
				continue;
			}

			float& Hgt = H.Data[size_t(z) * H.Width + x];

			if (m_BrushMode == bmHole)
			{
				if (!invert && Falloff >= 0.5f)
				{
					Hgt = 0.f;
				}
				else if (invert && Hgt <= 0.f && Falloff >= 0.5f)
				{
					Hgt = std::max(m_FlattenTarget, SHeightMap::MIN_SOLID);
				}
				continue;
			}

			// Regular brushes never touch holes and never create them
			if (Hgt <= 0.f)
			{
				continue;
			}

			switch (m_BrushMode)
			{
			case bmRaise:
				Hgt += m_BrushStrength * Falloff * Sign;
				break;
			case bmLower:
				Hgt -= m_BrushStrength * Falloff * Sign;
				break;
			case bmFlatten:
				Hgt += (m_FlattenTarget - Hgt) * Blend * Falloff;
				break;
			case bmNoise:
			{
				const float Wx = x * StepX * NoiseFreq;
				const float Wz = z * StepZ * NoiseFreq;
				Hgt += TerrainBrush::Fbm(Wx, Wz) * m_BrushStrength * Falloff * Sign;
				break;
			}
			case bmSmooth:
			{
				float Avg = 0.f;
				int Cnt = 0;
				for (int nz = -1; nz <= 1; ++nz)
				{
					for (int nx = -1; nx <= 1; ++nx)
					{
						const int Sx = x + nx;
						const int Sz = z + nz;
						if (Sx < SX0 || Sz < SZ0 || Sx > SX1 || Sz > SZ1)
						{
							continue;
						}
						const float V = m_SmoothScratch[size_t(Sz - SZ0) * SW + (Sx - SX0)];
						if (V > 0.f)
						{
							Avg += V;
							++Cnt;
						}
					}
				}
				if (Cnt)
				{
					Hgt += (Avg / float(Cnt) - Hgt) * Blend * Falloff;
				}
				break;
			}
			default:
				break;
			}

			Hgt = clampr(Hgt, SHeightMap::MIN_SOLID, 1.f);
			H.MinH = std::min(H.MinH, Hgt);
			H.MaxH = std::max(H.MaxH, Hgt);
		}
	}

	obj->OnHeightmapChanged(MinX, MinZ, MaxX, MaxZ);
}

void ESceneTerrainTool::RenderBrush()
{
	CTerrain* Obj = m_EditedTerrain ? m_EditedTerrain : m_HoverTerrain;
	if (!m_BrushActive || !Obj || !Obj->HMap.IsValid())
	{
		return;
	}

	const SHeightMap& H = Obj->HMap;
	const Fmatrix& M = Obj->_Transform();
	const float ScaleY = Obj->GetScaleY();
	const float LocalRadius = m_BrushSize / TerrainBrush::TransformScale(M);
	const bool Invert = m_BrushInvert || (EContext.UI->GetShiftState() & ssCtrl);
	const u32 Color = TerrainBrush::ModeColor(m_BrushMode, Invert);
	const u32 InnerColor = subst_alpha(Color, 0x80);

	Fvector Center;
	Obj->_ITransform().transform_tiny(Center, m_BrushPos);

	auto SurfacePoint = [&](float Lx, float Lz) -> Fvector
	{
		Fvector L = {Lx, 0.f, Lz};
		float Gx, Gz;
		H.LocalToGrid(L, Gx, Gz);
		L.y = H.ToLocalY(H.Sample(Gx, Gz), ScaleY) + 0.15f;
		Fvector W;
		M.transform_tiny(W, L);
		return W;
	};

	auto DrawRing = [&](float Radius, u32 Clr)
	{
		const int Segments = 64;
		Fvector Prev = SurfacePoint(Center.x + Radius, Center.z);
		for (int i = 1; i <= Segments; ++i)
		{
			const float A = i * PI_MUL_2 / Segments;
			const Fvector Cur = SurfacePoint(Center.x + cosf(A) * Radius, Center.z + sinf(A) * Radius);
			DU_impl.DrawLine(Prev, Cur, Clr);
			Prev = Cur;
		}
	};

	RCache.set_xform_world(Fidentity);
	DrawRing(LocalRadius, Color);

	const float Hard = clampr(m_BrushHardness, 0.f, 1.f);
	if (Hard > 0.05f && Hard < 0.95f)
	{
		DrawRing(LocalRadius * Hard, InnerColor);
	}

	// Center marker
	const Fvector Base = SurfacePoint(Center.x, Center.z);
	Fvector Top = Base;
	Top.y += std::max(m_BrushSize * 0.2f, 0.5f);
	DU_impl.DrawLine(Base, Top, Color);

	// Flatten target plane indicator
	if (m_BrushMode == bmFlatten)
	{
		Fvector L = {Center.x, H.ToLocalY(m_FlattenTarget, ScaleY), Center.z};
		Fvector W;
		M.transform_tiny(W, L);
		const float R = m_BrushSize * 0.5f;
		DU_impl.DrawLine(Fvector().set(W.x - R, W.y, W.z), Fvector().set(W.x + R, W.y, W.z), InnerColor);
		DU_impl.DrawLine(Fvector().set(W.x, W.y, W.z - R), Fvector().set(W.x, W.y, W.z + R), InnerColor);
	}
}

void ESceneTerrainTool::CreateTerrain(LPCSTR name, u32 w, u32 h, float fill)
{
	string256 namebuffer;
	Scene->GenObjectName(OBJCLASS_TERRAIN, namebuffer, name);
	CTerrain* obj = new CTerrain(nullptr, namebuffer);
	obj->InitializeHeightmap(w, h, fill);
	obj->SetLoadedState();
	Scene->SelectObjects(false, OBJCLASS_TERRAIN);
	Scene->AppendObject(obj);
	obj->Select(true);
	Scene->UndoSave();
}

CTerrain* ESceneTerrainTool::GetActiveTerrain()
{
	CTerrain* Single = nullptr;
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
	{
		CTerrain* t = (CTerrain*)(*it);
		if (t->Selected())
		{
			return t;
		}
		Single = t;
	}

	return (m_Objects.size() == 1) ? Single : nullptr;
}

void ESceneTerrainTool::FinishOp(CTerrain* obj)
{
	obj->OnHeightmapChanged();
	Scene->UndoSave();
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void ESceneTerrainTool::OpFill(CTerrain* obj, float height)
{
	if (!obj)
	{
		return;
	}

	height = clampr(height, SHeightMap::MIN_SOLID, 1.f);
	for (float& Hgt : obj->HMap.Data)
	{
		if (Hgt > 0.f)
		{
			Hgt = height;
		}
	}
	FinishOp(obj);
}

void ESceneTerrainTool::OpSmooth(CTerrain* obj, int passes)
{
	if (!obj)
	{
		return;
	}

	SHeightMap& H = obj->HMap;
	xr_vector<float> Src;
	for (int Pass = 0; Pass < passes; ++Pass)
	{
		Src = H.Data;
		xr_parallel_for(u32(0), H.Height, [&](u32 z)
		{
			for (u32 x = 0; x < H.Width; ++x)
			{
				float& Hgt = H.Data[size_t(z) * H.Width + x];
				if (Hgt <= 0.f)
				{
					continue;
				}

				float Sum = 0.f;
				int Cnt = 0;
				for (int nz = -1; nz <= 1; ++nz)
				{
					for (int nx = -1; nx <= 1; ++nx)
					{
						const int Sx = int(x) + nx;
						const int Sz = int(z) + nz;
						if (Sx < 0 || Sz < 0 || Sx >= int(H.Width) || Sz >= int(H.Height))
						{
							continue;
						}
						const float V = Src[size_t(Sz) * H.Width + Sx];
						if (V > 0.f)
						{
							Sum += V;
							++Cnt;
						}
					}
				}
				Hgt = clampr(Sum / float(Cnt), SHeightMap::MIN_SOLID, 1.f);
			}
		});
	}
	FinishOp(obj);
}

void ESceneTerrainTool::OpNormalize(CTerrain* obj)
{
	if (!obj)
	{
		return;
	}

	float Min = FLT_MAX, Max = -FLT_MAX;
	for (float Hgt : obj->HMap.Data)
	{
		if (Hgt > 0.f)
		{
			Min = std::min(Min, Hgt);
			Max = std::max(Max, Hgt);
		}
	}

	if (Max - Min < EPS_S)
	{
		return;
	}

	for (float& Hgt : obj->HMap.Data)
	{
		if (Hgt > 0.f)
		{
			Hgt = clampr((Hgt - Min) / (Max - Min), SHeightMap::MIN_SOLID, 1.f);
		}
	}
	FinishOp(obj);
}

void ESceneTerrainTool::OpInvert(CTerrain* obj)
{
	if (!obj)
	{
		return;
	}

	for (float& Hgt : obj->HMap.Data)
	{
		if (Hgt > 0.f)
		{
			Hgt = clampr(1.f - Hgt, SHeightMap::MIN_SOLID, 1.f);
		}
	}
	FinishOp(obj);
}

void ESceneTerrainTool::OpFillHoles(CTerrain* obj)
{
	if (!obj)
	{
		return;
	}

	SHeightMap& H = obj->HMap;
	bool Changed = true;
	bool Any = false;
	xr_vector<float> Src;
	for (u32 Pass = 0; Changed && Pass < std::max(H.Width, H.Height); ++Pass)
	{
		Changed = false;
		Src = H.Data;
		for (u32 z = 0; z < H.Height; ++z)
		{
			for (u32 x = 0; x < H.Width; ++x)
			{
				float& Hgt = H.Data[size_t(z) * H.Width + x];
				if (Hgt > 0.f)
				{
					continue;
				}

				float Sum = 0.f;
				int Cnt = 0;
				for (int nz = -1; nz <= 1; ++nz)
				{
					for (int nx = -1; nx <= 1; ++nx)
					{
						const int Sx = int(x) + nx;
						const int Sz = int(z) + nz;
						if (Sx < 0 || Sz < 0 || Sx >= int(H.Width) || Sz >= int(H.Height))
						{
							continue;
						}
						const float V = Src[size_t(Sz) * H.Width + Sx];
						if (V > 0.f)
						{
							Sum += V;
							++Cnt;
						}
					}
				}

				if (Cnt)
				{
					Hgt = Sum / float(Cnt);
					Changed = true;
					Any = true;
				}
			}
		}
	}

	if (Any)
	{
		FinishOp(obj);
	}
}

void ESceneTerrainTool::OpResize(CTerrain* obj, u32 size)
{
	if (!obj || size < 2 || (obj->HMap.Width == size && obj->HMap.Height == size))
	{
		return;
	}

	obj->HMap.Resample(size, size);
	obj->RebuildMesh();
	FinishOp(obj);
}

bool ESceneTerrainTool::OpImportR16(CTerrain* obj, const char* fileName)
{
	if (!obj || !fileName || !fileName[0])
	{
		return false;
	}

	IReader* F = FS.r_open(fileName);
	if (!F)
	{
		Msg("! Terrain: can't open [%s]", fileName);
		return false;
	}

	const bool Result = obj->HMap.LoadSteam(F);
	FS.r_close(F);
	if (!Result)
	{
		return false;
	}

	obj->RebuildMesh();
	FinishOp(obj);
	Msg("Terrain: imported [%s] %ux%u", fileName, obj->HMap.Width, obj->HMap.Height);
	return true;
}

bool ESceneTerrainTool::OpExportR16(CTerrain* obj, const char* fileName)
{
	if (!obj || !fileName || !fileName[0])
	{
		return false;
	}

	IWriter* F = FS.w_open(fileName);
	if (!F)
	{
		Msg("! Terrain: can't write [%s]", fileName);
		return false;
	}

	const bool Result = obj->HMap.SaveSteam(F);
	FS.w_close(F);
	Msg("Terrain: exported [%s] %ux%u", fileName, obj->HMap.Width, obj->HMap.Height);
	return Result;
}

void ESceneTerrainTool::OpClearSurfacePaint(CTerrain* obj)
{
	if (!obj)
	{
		return;
	}
	obj->Mask.ClearSurfacePaint();
	Scene->UndoSave();
}

void ESceneTerrainTool::OpClearPaint(CTerrain* obj)
{
	if (!obj)
	{
		return;
	}
	obj->Mask.ClearPaint();
	Scene->UndoSave();
}

bool ESceneTerrainTool::OpBakeMask(CTerrain* obj, u32 size)
{
	if (!obj)
	{
		return false;
	}
	xr_string ColorPath, MaskPath;
	return obj->BakeMask(size, ColorPath, MaskPath);
}

void ESceneTerrainTool::OpApplyMesh(CTerrain* obj)
{
	if (obj)
	{
		obj->RebuildMesh();
	}
}

CCustomObject* ESceneTerrainTool::CreateObject(LPVOID data, const char* name)
{
	CCustomObject* O = new CTerrain(data, name);
	O->FParentTools = this;
	return O;
}

void ESceneTerrainTool::OnDrawUI()
{
	xr_string result;
	bool ok = false;
	if (UITextForm::GetResult(ok,result))
	{
		if (ok)
		{
		}
	}

	UITextForm::Update();
}

void ESceneTerrainTool::GetStaticDesc(int& v_cnt, int& f_cnt, bool b_selected_only, bool b_cform)
{
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); it++)
	{
		CTerrain* obj = (CTerrain*)(*it);

		if (b_selected_only && !obj->Selected())
		{
			continue;
		}

		f_cnt += obj->TerrainObject->GetFaceCount();
		v_cnt += obj->TerrainObject->GetVertexCount();
	}
}
