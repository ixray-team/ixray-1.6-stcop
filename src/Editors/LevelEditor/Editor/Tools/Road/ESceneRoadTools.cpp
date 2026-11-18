#include "stdafx.h"
#include "ESceneRoadTools.h"
#include "ESceneRoadControls.h"
#include "UI/Tools/UIRoadTool.h"
#include "../../Entry/Terrain/Terrain.h"

using XRay::Editor::HeightmapUtils::SHeightMap;
using XRay::Editor::Spline::SSplineSample;

ESceneRoadTool::ESceneRoadTool() :
	ESceneCustomOTool(OBJCLASS_ROAD)
{
}

ESceneRoadTool::~ESceneRoadTool()
{
}

void ESceneRoadTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	// Point mode: standard Select / Move act on control points (see CRoad), Add places new points
	inherited::CreateDefaultControls(estRoadEdit);
	AddControl(new TUI_ControlRoadEdit(estRoadEdit, etaAdd, this));
	pForm = new UIRoadTool();
	((UIRoadTool*)pForm)->tool = this;
}

void ESceneRoadTool::RemoveControls()
{
	inherited::RemoveControls();
}

void ESceneRoadTool::OnRender(int priority, bool strictB2F)
{
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
	{
		(*it)->Render(priority, strictB2F);
	}
}

void ESceneRoadTool::OnDeactivate()
{
	inherited::OnDeactivate();
}

CCustomObject* ESceneRoadTool::CreateObject(LPVOID data, const char* name)
{
	CCustomObject* O = new CRoad(data, name);
	O->FParentTools = this;
	return O;
}

CRoad* ESceneRoadTool::GetActiveRoad()
{
	CRoad* Single = nullptr;
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
	{
		CRoad* R = (CRoad*)(*it);
		if (R->Selected())
		{
			return R;
		}
		Single = R;
	}
	return (m_Objects.size() == 1) ? Single : nullptr;
}

CRoad* ESceneRoadTool::CreateRoad(const char* name)
{
	string256 NameBuffer;
	Scene->GenObjectName(OBJCLASS_ROAD, NameBuffer, name);
	CRoad* Road = new CRoad(nullptr, NameBuffer);
	Road->FParentTools = this;
	Road->SetLoadedState();
	Scene->SelectObjects(false, OBJCLASS_ROAD);
	Scene->AppendObject(Road);
	Road->Select(true);
	Scene->UndoSave();
	return Road;
}

void ESceneRoadTool::DeletePoint(CRoad* Road, int Index)
{
	if (!Road || Index < 0 || Index >= int(Road->Spline.Points.size()))
	{
		return;
	}

	Road->Spline.Points.erase(Road->Spline.Points.begin() + Index);
	Road->ClearPointSelection();
	Road->RebuildMesh();
	Scene->UndoSave();
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
}

void ESceneRoadTool::SnapPointsToTerrain(CRoad* Road)
{
	if (!Road)
	{
		return;
	}

	bool Any = false;
	for (u32 i = 0; i < u32(Road->Spline.Points.size()); ++i)
	{
		Fvector W = Road->PointWorld(i);
		float Y;
		if (CRoad::SampleTerrainHeight(W.x, W.z, Y))
		{
			W.y = Y;
			Road->SetPointWorld(i, W);
			Any = true;
		}
	}

	if (Any)
	{
		Road->RebuildMesh();
		Scene->UndoSave();
	}
}

void ESceneRoadTool::ConformTerrain(CRoad* Road)
{
	if (!Road || !Road->Spline.IsValid())
	{
		return;
	}

	// Road centerline in world space
	xr_vector<SSplineSample> Samples;
	Road->Spline.Sample(std::max(Road->SampleStep * 0.5f, 0.25f), Samples);
	const Fmatrix& RM = Road->_Transform();
	struct SWorldSample
	{
		Fvector Pos;
		float HalfWidth;
	};
	xr_vector<SWorldSample> Path(Samples.size());
	for (size_t i = 0; i < Samples.size(); ++i)
	{
		RM.transform_tiny(Path[i].Pos, Samples[i].Pos);
		Path[i].HalfWidth = Samples[i].Width * 0.5f + Road->ShoulderWidth;
	}

	const float Falloff = std::max(Road->ConformFalloff, 0.f);
	// The terrain goes slightly under the road surface
	const float Sink = std::max(Road->HeightOffset, 0.f) + 0.02f;

	ObjectList& Terrains = Scene->ListObj(OBJCLASS_TERRAIN);
	bool Changed = false;

	for (CCustomObject* Obj : Terrains)
	{
		CTerrain* T = smart_cast<CTerrain*>(Obj);
		if (!T || !T->GetHeightmap().IsValid())
		{
			continue;
		}

		SHeightMap& H = T->GetHeightmap();
		const Fmatrix& M = T->_Transform();
		const Fmatrix& IM = T->_ITransform();
		const float ScaleY = T->GetScaleY();

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
					H.LocalToGrid(L, Gx, Gz);
					MinGx = std::min(MinGx, Gx);
					MaxGx = std::max(MaxGx, Gx);
					MinGz = std::min(MinGz, Gz);
					MaxGz = std::max(MaxGz, Gz);
				}
			}
			X0 = std::max(0, iFloor(MinGx));
			Z0 = std::max(0, iFloor(MinGz));
			X1 = std::min(int(H.Width) - 1, iCeil(MaxGx));
			Z1 = std::min(int(H.Height) - 1, iCeil(MaxGz));
		};

		// Region touched by the whole road
		Fbox RoadBox;
		RoadBox.invalidate();
		float MaxHalf = 0.f;
		for (const SWorldSample& S : Path)
		{
			RoadBox.modify(S.Pos);
			MaxHalf = std::max(MaxHalf, S.HalfWidth);
		}

		int RX0, RZ0, RX1, RZ1;
		GridRect(RoadBox.min, RoadBox.max, MaxHalf + Falloff, RX0, RZ0, RX1, RZ1);
		if (RX0 > RX1 || RZ0 > RZ1)
		{
			continue;
		}

		const int RW = RX1 - RX0 + 1;
		const int RH = RZ1 - RZ0 + 1;
		// Per cell: distance beyond the road core (<= 0 inside), and the road height
		xr_vector<float> BestOut(size_t(RW) * RH, flt_max);
		xr_vector<float> TargetY(size_t(RW) * RH, 0.f);

		auto CellWorld = [&](int X, int Z) -> Fvector
		{
			Fvector L;
			L.x = -(X * H.StepX() - H.HalfX());
			L.z = Z * H.StepZ() - H.HalfZ();
			L.y = H.ToLocalY(H.GetHeight(X, Z), ScaleY);
			Fvector W;
			M.transform_tiny(W, L);
			return W;
		};

		for (size_t s = 0; s + 1 < Path.size(); ++s)
		{
			const SWorldSample& A = Path[s];
			const SWorldSample& B = Path[s + 1];
			const float Expand = std::max(A.HalfWidth, B.HalfWidth) + Falloff;

			int X0, Z0, X1, Z1;
			GridRect(A.Pos, B.Pos, Expand, X0, Z0, X1, Z1);

			const float Dx = B.Pos.x - A.Pos.x;
			const float Dz = B.Pos.z - A.Pos.z;
			const float Len2 = Dx * Dx + Dz * Dz;

			for (int z = Z0; z <= Z1; ++z)
			{
				for (int x = X0; x <= X1; ++x)
				{
					const Fvector W = CellWorld(x, z);
					float Tp = 0.f;
					if (Len2 > EPS_S)
					{
						Tp = clampr(((W.x - A.Pos.x) * Dx + (W.z - A.Pos.z) * Dz) / Len2, 0.f, 1.f);
					}
					const float Px = A.Pos.x + Dx * Tp;
					const float Pz = A.Pos.z + Dz * Tp;
					const float Dist = _sqrt((W.x - Px) * (W.x - Px) + (W.z - Pz) * (W.z - Pz));
					const float Half = A.HalfWidth + (B.HalfWidth - A.HalfWidth) * Tp;
					const float Out = Dist - Half;

					const size_t Idx = size_t(z - RZ0) * RW + (x - RX0);
					if (Out < BestOut[Idx])
					{
						BestOut[Idx] = Out;
						TargetY[Idx] = A.Pos.y + (B.Pos.y - A.Pos.y) * Tp - Sink;
					}
				}
			}
		}

		bool TerrainChanged = false;
		for (int z = RZ0; z <= RZ1; ++z)
		{
			for (int x = RX0; x <= RX1; ++x)
			{
				const size_t Idx = size_t(z - RZ0) * RW + (x - RX0);
				const float Out = BestOut[Idx];
				if (Out > Falloff)
				{
					continue;
				}

				float& Hgt = H.Data[size_t(z) * H.Width + x];
				if (Hgt <= 0.f)
				{
					continue;
				}

				float Weight = 1.f;
				if (Out > 0.f)
				{
					float Tf = 1.f - Out / std::max(Falloff, EPS_S);
					Weight = Tf * Tf * (3.f - 2.f * Tf);
				}

				Fvector W = CellWorld(x, z);
				W.y = TargetY[Idx];
				Fvector L;
				IM.transform_tiny(L, W);
				const float Target = clampr(H.FromLocalY(L.y, ScaleY), SHeightMap::MIN_SOLID, 1.f);
				Hgt += (Target - Hgt) * Weight;
				TerrainChanged = true;
			}
		}

		if (TerrainChanged)
		{
			T->OnHeightmapChanged();
			T->RebuildMesh();
			Changed = true;
		}
	}

	if (Changed)
	{
		// Re-drape all roads over the new terrain
		for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
		{
			((CRoad*)(*it))->RebuildMesh();
		}
		Scene->UndoSave();
	}
	else
	{
		Msg("! Road: no terrain under [%s]", Road->GetName());
	}
}

void ESceneRoadTool::GetStaticDesc(int& v_cnt, int& f_cnt, bool b_selected_only, bool b_cform)
{
	for (ObjectIt it = m_Objects.begin(); it != m_Objects.end(); ++it)
	{
		CRoad* R = (CRoad*)(*it);
		if ((b_selected_only && !R->Selected()) || !R->HasMesh())
		{
			continue;
		}
		f_cnt += R->RoadObject->GetFaceCount();
		v_cnt += R->RoadObject->GetVertexCount();
	}
}
