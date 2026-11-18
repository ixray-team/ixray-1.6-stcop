#include "stdafx.h"
#include "Road.h"
#include "../Terrain/Terrain.h"
#include "../../Tools/Road/ESceneRoadTools.h"

using XRay::Editor::Spline::SSplinePoint;
using XRay::Editor::Spline::SSplineSample;

namespace
{
	enum class ERoadChunkIDs : u32
	{
		Version = 0x1000,
		Spline,
		Params,
		Surfaces,
	};

	constexpr u16 ROAD_VERSION = 1;

	void WriteSurface(IWriter& F, const CRoad::SSurface& S)
	{
		F.w_stringZ(*S.Shader);
		F.w_stringZ(*S.ShaderXRLC);
		F.w_stringZ(*S.GameMtl);
		F.w_stringZ(*S.Texture);
	}

	void ReadSurface(IReader& F, CRoad::SSurface& S)
	{
		shared_str Str;
		F.r_stringZ(Str);
		S.Shader = Str;
		F.r_stringZ(Str);
		S.ShaderXRLC = Str;
		F.r_stringZ(Str);
		S.GameMtl = Str;
		F.r_stringZ(Str);
		S.Texture = Str;
	}

	void ApplySurface(CSurface* Surface, const char* Name, const CRoad::SSurface& S)
	{
		Surface->SetName(Name);
		if (S.Shader.size())
		{
			Surface->SetShader(*S.Shader);
		}
		if (S.ShaderXRLC.size())
		{
			Surface->SetShaderXRLC(*S.ShaderXRLC);
		}
		if (S.GameMtl.size())
		{
			Surface->SetGameMtl(*S.GameMtl);
		}
		if (S.Texture.size())
		{
			Surface->SetTexture(*S.Texture);
		}
		Surface->m_VMap = "Texture";
		Surface->m_dwFVF = (D3DFVF_XYZ | D3DFVF_NORMAL | D3DFVF_TEX1);
		Surface->OnDeviceCreate();
	}
}

CRoad::CRoad(LPVOID data, const char* name)
	: inherited(data, name ? name : "road"), RoadObject(new CEditableObject(name ? name : "road"))
{
	FClassID = OBJCLASS_ROAD;
	FScale.set(1, 1, 1);
	m_RT_Flags.set(flRT_Visible, true);
	m_TBBox.invalidate();

	RoadSurface.Shader = "default";
	RoadSurface.ShaderXRLC = "default";
	RoadSurface.GameMtl = "materials\\asphalt";
	RoadSurface.Texture = "terrain\\terrain_mp_atp";

	ShoulderSurface.Shader = "default";
	ShoulderSurface.ShaderXRLC = "default";
	ShoulderSurface.GameMtl = "materials\\earth";
	ShoulderSurface.Texture = "terrain\\terrain_mp_atp";
}

CRoad::~CRoad()
{
	if (RoadObject)
	{
		RoadObject->EvictObject();
	}
	xr_delete(RoadObject);
}

bool CRoad::HasMesh() const
{
	return RoadObject && !RoadObject->m_Meshes.empty();
}

float CRoad::GetLength() const
{
	xr_vector<SSplineSample> Samples;
	Spline.Sample(1.f, Samples);
	return Samples.empty() ? 0.f : Samples.back().Distance;
}

Fvector CRoad::PointWorld(u32 Index)
{
	Fvector W;
	_Transform().transform_tiny(W, Spline.Points[Index].Pos);
	return W;
}

void CRoad::SetPointWorld(u32 Index, const Fvector& World)
{
	_ITransform().transform_tiny(Spline.Points[Index].Pos, World);
}

u32 CRoad::InsertPointWorld(u32 Index, const Fvector& World, float Width)
{
	// The first point defines the object pivot
	if (Spline.Points.empty())
	{
		FPosition = World;
		UpdateTransform(true);
	}

	SSplinePoint P;
	_ITransform().transform_tiny(P.Pos, World);
	P.Width = Width;

	Index = std::min(Index, u32(Spline.Points.size()));
	if (!Spline.Points.empty())
	{
		// Inherit width/roll from the neighbour
		const SSplinePoint& N = Spline.Points[std::min(Index, u32(Spline.Points.size() - 1))];
		P.Roll = N.Roll;
	}
	Spline.Points.insert(Spline.Points.begin() + Index, P);
	return Index;
}

bool CRoad::IsPointMode()
{
	return LTools && LTools->GetTarget() == OBJCLASS_ROAD && LTools->GetSubTarget() == ESceneRoadTool::estRoadEdit;
}

int CRoad::PickPointRay(const Fvector& Start, const Fvector& Dir, float* OutT)
{
	int Best = -1;
	float BestDist = flt_max;
	for (u32 i = 0; i < u32(Spline.Points.size()); ++i)
	{
		const Fvector P = PointWorld(i);
		Fvector D;
		D.sub(P, Start);
		const float T = D.dotproduct(Dir);
		if (T <= 0.f)
		{
			continue;
		}

		Fvector Closest;
		Closest.mad(Start, Dir, T);
		const float Dist = Closest.distance_to(P);
		// Pick radius grows with the distance to keep a constant on-screen size
		if (Dist < std::max(0.5f, T * 0.015f) && Dist < BestDist)
		{
			BestDist = Dist;
			Best = int(i);
			if (OutT)
			{
				*OutT = T;
			}
		}
	}
	return Best;
}

void CRoad::SetPointSelected(u32 Index, int Flag)
{
	const bool Now = IsPointSelected(Index);
	const bool Want = (Flag == -1) ? !Now : (Flag != 0);
	if (Now != Want)
	{
		TogglePoint(Index);
	}
}

void CRoad::Select(int flag)
{
	if (IsPointMode())
	{
		// Select all / none / invert for the points of the selected road
		if (Selected())
		{
			for (u32 i = 0; i < u32(Spline.Points.size()); ++i)
			{
				SetPointSelected(i, flag);
			}
			ExecCommand(COMMAND_UPDATE_PROPERTIES);
		}
		return;
	}
	inherited::Select(flag);
}

bool CRoad::RaySelect(int flag, const Fvector& start, const Fvector& dir, bool bRayTest)
{
	if (!IsPointMode())
	{
		return inherited::RaySelect(flag, start, dir, bRayTest);
	}

	if (!Visible() || Locked())
	{
		return false;
	}

	const int Hit = PickPointRay(start, dir);
	if (Hit < 0)
	{
		return false;
	}
	if (bRayTest)
	{
		return true;
	}

	// Picking a point also makes its road the selected object, so Properties show it
	if (!Selected())
	{
		inherited::Select(1);
	}
	SetPointSelected(u32(Hit), flag);
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	return true;
}

bool CRoad::FrustumSelect(int flag, const CFrustum& frustum)
{
	if (!IsPointMode())
	{
		return inherited::FrustumSelect(flag, frustum);
	}

	if (!Visible() || Locked())
	{
		return false;
	}

	bool Any = false;
	for (u32 i = 0; i < u32(Spline.Points.size()); ++i)
	{
		Fvector P = PointWorld(i);
		if (frustum.testSphere_dirty(P, 0.35f))
		{
			SetPointSelected(i, flag);
			Any = true;
		}
	}

	if (Any)
	{
		if (!Selected() && flag != 0)
		{
			inherited::Select(1);
		}
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
	}
	return Any;
}

Fmatrix CRoad::GetPointsTransform()
{
	Fmatrix M = _Transform();
	if (!IsPointMode() || SelectedPoints.empty())
	{
		return M;
	}

	Fvector Center = {0, 0, 0};
	u32 Count = 0;
	for (u32 I : SelectedPoints)
	{
		if (I < Spline.Points.size())
		{
			Center.add(PointWorld(I));
			++Count;
		}
	}
	if (Count)
	{
		Center.div(float(Count));
		M.c = Center;
	}
	return M;
}

void CRoad::Move(Fvector& amount)
{
	if (!IsPointMode())
	{
		inherited::Move(amount);
		m_MeshDirty = true;
		return;
	}

	for (u32 I : SelectedPoints)
	{
		if (I < Spline.Points.size())
		{
			Fvector W = PointWorld(I);
			W.add(amount);
			SetPointWorld(I, W);
		}
	}
	m_MeshDirty = true;
	UpdateTransform(true);
}

void CRoad::RotateParent(Fvector& axis, float angle)
{
	if (!IsPointMode())
	{
		inherited::RotateParent(axis, angle);
		m_MeshDirty = true;
	}
}

void CRoad::RotateLocal(Fvector& axis, float angle)
{
	if (!IsPointMode())
	{
		inherited::RotateLocal(axis, angle);
		m_MeshDirty = true;
	}
}

void CRoad::Scale(Fvector& amount)
{
	if (!IsPointMode())
	{
		inherited::Scale(amount);
		m_MeshDirty = true;
	}
}

bool CRoad::OnSelectionRemove()
{
	if (!IsPointMode())
	{
		return true;
	}

	// Delete removes the selected points, the road object stays
	if (!SelectedPoints.empty())
	{
		DeleteSelectedPoints();
		RebuildMesh();
		ExecCommand(COMMAND_UPDATE_PROPERTIES);
	}
	return false;
}

void CRoad::OnFrame()
{
	inherited::OnFrame();

	// Rebuild once the gizmo drag is over (draping the mesh every frame is too slow)
	if (m_MeshDirty && !ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		m_MeshDirty = false;
		RebuildMesh();
	}
}

bool CRoad::IsPointSelected(u32 Index) const
{
	return std::binary_search(SelectedPoints.begin(), SelectedPoints.end(), Index);
}

void CRoad::SelectPoint(u32 Index, bool Add)
{
	if (!Add)
	{
		SelectedPoints.clear();
	}
	if (!IsPointSelected(Index))
	{
		SelectedPoints.insert(std::upper_bound(SelectedPoints.begin(), SelectedPoints.end(), Index), Index);
	}
}

void CRoad::TogglePoint(u32 Index)
{
	auto It = std::lower_bound(SelectedPoints.begin(), SelectedPoints.end(), Index);
	if (It != SelectedPoints.end() && *It == Index)
	{
		SelectedPoints.erase(It);
	}
	else
	{
		SelectedPoints.insert(It, Index);
	}
}

void CRoad::DeleteSelectedPoints()
{
	for (auto It = SelectedPoints.rbegin(); It != SelectedPoints.rend(); ++It)
	{
		if (*It < Spline.Points.size())
		{
			Spline.Points.erase(Spline.Points.begin() + *It);
		}
	}
	SelectedPoints.clear();
}

void CRoad::FillPointProps(const char* pref, PropItemVec& items)
{
	// Drop stale indices
	SelectedPoints.erase(std::remove_if(SelectedPoints.begin(), SelectedPoints.end(), [&](u32 I) { return I >= Spline.Points.size(); }), SelectedPoints.end());

	const shared_str Pref = PrepareKey(pref, "Road\\Selected Points").c_str();
	PHelper().CreateCaption(items, PrepareKey(Pref.c_str(), "Count"), xr_string::ToString(u32(SelectedPoints.size())).c_str());
	if (SelectedPoints.empty())
	{
		return;
	}

	const u32 First = SelectedPoints.front();
	EditWidth = Spline.Points[First].Width;
	EditBank = rad2deg(Spline.Points[First].Roll);
	EditPos = PointWorld(First);
	EditHeight = EditPos.y;

	PropValue* V;
	V = PHelper().CreateFloat(items, PrepareKey(Pref.c_str(), "Width"), &EditWidth, 0.5f, 100.f, 0.1f, 2);
	V->OnChangeEvent.bind(this, &CRoad::OnPointWidthChange);
	V = PHelper().CreateFloat(items, PrepareKey(Pref.c_str(), "Bank (deg)"), &EditBank, -45.f, 45.f, 0.5f, 1);
	V->OnChangeEvent.bind(this, &CRoad::OnPointBankChange);
	V = PHelper().CreateFloat(items, PrepareKey(Pref.c_str(), "Height"), &EditHeight, -10000.f, 10000.f, 0.05f, 2);
	V->OnChangeEvent.bind(this, &CRoad::OnPointHeightChange);
	if (SelectedPoints.size() == 1)
	{
		V = PHelper().CreateVector(items, PrepareKey(Pref.c_str(), "Position"), &EditPos, -100000.f, 100000.f, 0.05f, 2);
		V->OnChangeEvent.bind(this, &CRoad::OnPointPosChange);
	}

	ButtonValue* B = PHelper().CreateButton(items, PrepareKey(Pref.c_str(), "Delete"), "Delete Selected", 0);
	B->OnBtnClickEvent.bind(this, &CRoad::OnDeletePointsClick);
}

void CRoad::AfterPointEdit()
{
	RebuildMesh();
	Scene->UndoSave();
}

void CRoad::OnPointWidthChange(PropValue* sender)
{
	for (u32 I : SelectedPoints)
	{
		Spline.Points[I].Width = EditWidth;
	}
	AfterPointEdit();
}

void CRoad::OnPointBankChange(PropValue* sender)
{
	for (u32 I : SelectedPoints)
	{
		Spline.Points[I].Roll = deg2rad(EditBank);
	}
	AfterPointEdit();
}

void CRoad::OnPointHeightChange(PropValue* sender)
{
	for (u32 I : SelectedPoints)
	{
		Fvector W = PointWorld(I);
		W.y = EditHeight;
		SetPointWorld(I, W);
	}
	AfterPointEdit();
}

void CRoad::OnPointPosChange(PropValue* sender)
{
	if (SelectedPoints.size() == 1)
	{
		SetPointWorld(SelectedPoints.front(), EditPos);
		AfterPointEdit();
	}
}

void CRoad::OnDeletePointsClick(ButtonValue* sender, bool& bModif, bool& bSafe)
{
	DeleteSelectedPoints();
	AfterPointEdit();
	ExecCommand(COMMAND_UPDATE_PROPERTIES);
	bModif = true;
}

bool CRoad::SampleTerrainHeight(float X, float Z, float& Y)
{
	bool Found = false;
	float BestY = -flt_max;

	ObjectList& Terrains = Scene->ListObj(OBJCLASS_TERRAIN);
	for (CCustomObject* Obj : Terrains)
	{
		CTerrain* T = smart_cast<CTerrain*>(Obj);
		if (!T || !T->Visible())
		{
			continue;
		}

		const Fvector Start = {X, 10000.f, Z};
		const Fvector Dir = {0.f, -1.f, 0.f};
		float Dist = 20000.f;
		Fvector P;
		if (T->PickHeightmap(Start, Dir, Dist, P) && P.y > BestY)
		{
			BestY = P.y;
			Found = true;
		}
	}

	if (Found)
	{
		Y = BestY;
	}
	return Found;
}

void CRoad::RebuildMesh()
{
	CEditableObject* Old = RoadObject;
	RoadObject = new CEditableObject(GetName());
	EContext.UI->CommandList[TUI::ECommandListID::NextFrame].push_back
	(
		[Old]()
		{
			Old->EvictObject();
			xr_delete(Old);
		}
	);

	xr_vector<SSplineSample> Samples;
	Spline.Sample(SampleStep, Samples);
	if (Samples.size() < 2)
	{
		OnUpdateTransform();
		CTerrain::OnRoadsChanged();
		return;
	}

	const Fmatrix& M = _Transform();
	const Fmatrix& IM = _ITransform();
	const bool HasTerrain = FollowTerrain && Scene->ObjCount(OBJCLASS_TERRAIN) > 0;

	auto Drape = [&](Fvector& Local, float Lift)
	{
		if (!HasTerrain)
		{
			return;
		}
		Fvector W;
		M.transform_tiny(W, Local);
		float Y;
		if (SampleTerrainHeight(W.x, W.z, Y))
		{
			W.y = Y + Lift;
			IM.transform_tiny(Local, W);
		}
	};

	const u32 C = u32(clampr(CrossSegments, 1, 32));
	const bool Shoulders = ShoulderWidth > 0.01f;
	const u32 RoadRow = C + 1;
	const u32 Row = RoadRow + (Shoulders ? 4 : 0);
	const float VScale = 1.f / std::max(UVLength, 0.1f);
	const float ShoulderVScale = 1.f / std::max(ShoulderUVLength, 0.1f);

	// Seamless tiling: U from the lateral distance to the centerline, so road and shoulders
	// with the same texture and tile size continue each other without a seam
	auto RoadU = [&](float Offset, float F) { return RoadTileWidth > 0.01f ? Offset / RoadTileWidth : F; };
	auto ShoulderU = [&](float Offset, float Stretch) { return ShoulderTileWidth > 0.01f ? Offset / ShoulderTileWidth : Stretch; };

	xr_vector<Fvector> Vertices;
	xr_vector<Fvector2> UVs;
	Vertices.reserve(Samples.size() * Row);
	UVs.reserve(Samples.size() * Row);

	for (const SSplineSample& S : Samples)
	{
		const float HW = S.Width * 0.5f;
		const float V = S.Distance * VScale;
		const size_t RowStart = Vertices.size();

		for (u32 j = 0; j <= C; ++j)
		{
			const float F = float(j) / float(C);
			const float Offset = -HW + 2.f * HW * F;
			Fvector P;
			P.mad(S.Pos, S.Right, Offset);
			Drape(P, HeightOffset);
			Vertices.push_back(P);
			UVs.push_back({RoadU(Offset, F), V});
		}

		if (Shoulders)
		{
			Fvector OuterL, OuterR;
			OuterL.mad(S.Pos, S.Right, -(HW + ShoulderWidth));
			OuterR.mad(S.Pos, S.Right, HW + ShoulderWidth);
			Drape(OuterL, HeightOffset * 0.5f);
			Drape(OuterR, HeightOffset * 0.5f);

			const Fvector EdgeL = Vertices[RowStart];
			const Fvector EdgeR = Vertices[RowStart + C];
			const float SV = S.Distance * ShoulderVScale;
			const float Outer = HW + ShoulderWidth;

			Vertices.push_back(OuterL);
			UVs.push_back({ShoulderU(-Outer, 0.f), SV});
			Vertices.push_back(EdgeL);
			UVs.push_back({ShoulderU(-HW, 1.f), SV});
			Vertices.push_back(EdgeR);
			UVs.push_back({ShoulderU(HW, 1.f), SV});
			Vertices.push_back(OuterR);
			UVs.push_back({ShoulderU(Outer, 0.f), SV});
		}
	}

	xr_vector<st_Face> RoadFaces;
	xr_vector<st_Face> ShoulderFaces;

	auto AddQuad = [&](xr_vector<st_Face>& Out, u32 A, u32 B, u32 Cc, u32 D, const Fvector& Up)
	{
		auto AddTri = [&](u32 I0, u32 I1, u32 I2)
		{
			Fvector E1, E2, N;
			E1.sub(Vertices[I1], Vertices[I0]);
			E2.sub(Vertices[I2], Vertices[I0]);
			N.crossproduct(E1, E2);
			if (N.dotproduct(Up) < 0.f)
			{
				std::swap(I1, I2);
			}

			st_Face Face;
			Face.pv[0].pindex = I0;
			Face.pv[1].pindex = I1;
			Face.pv[2].pindex = I2;
			Out.push_back(Face);
		};

		AddTri(A, B, Cc);
		AddTri(B, D, Cc);
	};

	for (u32 i = 0; i + 1 < u32(Samples.size()); ++i)
	{
		const u32 Base = i * Row;
		const u32 Next = (i + 1) * Row;
		const Fvector& Up = Samples[i].Up;

		for (u32 j = 0; j < C; ++j)
		{
			AddQuad(RoadFaces, Base + j, Base + j + 1, Next + j, Next + j + 1, Up);
		}

		if (Shoulders)
		{
			AddQuad(ShoulderFaces, Base + RoadRow, Base + RoadRow + 1, Next + RoadRow, Next + RoadRow + 1, Up);
			AddQuad(ShoulderFaces, Base + RoadRow + 2, Base + RoadRow + 3, Next + RoadRow + 2, Next + RoadRow + 3, Up);
		}
	}

	xr_vector<st_Face> Faces = RoadFaces;
	Faces.insert(Faces.end(), ShoulderFaces.begin(), ShoulderFaces.end());

	CEditableMesh* Mesh = new CEditableMesh(RoadObject);
	RoadObject->m_Meshes.push_back(Mesh);
	Mesh->Create(Faces.data(), u32(Faces.size()), Vertices.data(), u32(Vertices.size()), nullptr, 0);

	Mesh->m_VMaps.push_back(xr_make_unique<st_VMap>("Texture", vmtUV, false));
	st_VMap* UVMap = Mesh->m_VMaps.back().get();
	for (const Fvector2& UV : UVs)
	{
		UVMap->appendUV(const_cast<Fvector2&>(UV));
	}

	Mesh->m_VMRefs.resize(Vertices.size());
	for (u32 i = 0; i < u32(Vertices.size()); ++i)
	{
		st_VMapPtLst& Ref = Mesh->m_VMRefs[i];
		Ref.resize(1);
		Ref[0].vmap_index = 0;
		Ref[0].index = i;
	}

	for (u32 f = 0; f < u32(Faces.size()); ++f)
	{
		for (u32 j = 0; j < 3; ++j)
		{
			Mesh->m_Faces[f].pv[j].vmref = Faces[f].pv[j].pindex;
		}
	}

	// Create() made a default surface for all faces: reuse it for the road, add one for shoulders
	CSurface* RoadSurf = Mesh->m_SurfFaces.begin()->first;
	ApplySurface(RoadSurf, "road", RoadSurface);
	Mesh->m_SurfFaces.clear();

	IntVec RoadIds(RoadFaces.size());
	for (u32 i = 0; i < u32(RoadFaces.size()); ++i)
	{
		RoadIds[i] = int(i);
	}
	Mesh->m_SurfFaces[RoadSurf] = RoadIds;

	if (!ShoulderFaces.empty())
	{
		CSurface* ShoulderSurf = new CSurface();
		ShoulderSurf->SetSharedData(nullptr);
		RoadObject->m_Surfaces.push_back(ShoulderSurf);
		ApplySurface(ShoulderSurf, "road_shoulder", ShoulderSurface);

		IntVec ShoulderIds(ShoulderFaces.size());
		for (u32 i = 0; i < u32(ShoulderFaces.size()); ++i)
		{
			ShoulderIds[i] = int(RoadFaces.size() + i);
		}
		Mesh->m_SurfFaces[ShoulderSurf] = ShoulderIds;
	}

	Mesh->GenerateFNormals();
	Mesh->GenerateVNormals(nullptr, true);
	Mesh->GenerateAdjacency();
	RoadObject->UpdateBox();

	OnUpdateTransform();
	CTerrain::OnRoadsChanged();
}

void CRoad::OnUpdateTransform()
{
	inherited::OnUpdateTransform();

	m_TBBox.invalidate();
	if (HasMesh())
	{
		m_TBBox.set(RoadObject->GetBox());
		m_TBBox.xform(_Transform());
	}
	else
	{
		for (u32 i = 0; i < u32(Spline.Points.size()); ++i)
		{
			m_TBBox.modify(PointWorld(i));
		}
		if (!Spline.Points.empty())
		{
			m_TBBox.grow(0.5f);
		}
	}
}

void CRoad::OnDeviceDestroy()
{
	inherited::OnDeviceDestroy();
}

void CRoad::SaveStream(IWriter& F)
{
	F.open_chunk(u32(ERoadChunkIDs::Version));
	F.w_u16(ROAD_VERSION);
	F.close_chunk();

	F.open_chunk(u32(ERoadChunkIDs::Spline));
	Spline.Save(F);
	F.close_chunk();

	F.open_chunk(u32(ERoadChunkIDs::Params));
	F.w_float(SampleStep);
	F.w_s32(CrossSegments);
	F.w_float(ShoulderWidth);
	F.w_float(HeightOffset);
	F.w_float(UVLength);
	F.w_float(ConformFalloff);
	F.w_u8(FollowTerrain ? 1 : 0);
	F.w_float(RoadTileWidth);
	F.w_float(ShoulderUVLength);
	F.w_float(ShoulderTileWidth);
	// Old channel fields are kept as a marker, the material names follow
	F.w_u32(0xFFFFFFFF);
	F.w_u32(0);
	F.w_float(0.f);
	F.w_stringZ(MaskRoadMaterial.size() ? *MaskRoadMaterial : "");
	F.w_stringZ(MaskShoulderMaterial.size() ? *MaskShoulderMaterial : "");
	F.close_chunk();

	F.open_chunk(u32(ERoadChunkIDs::Surfaces));
	WriteSurface(F, RoadSurface);
	WriteSurface(F, ShoulderSurface);
	F.close_chunk();

	inherited::SaveStream(F);
}

bool CRoad::LoadStream(IReader& F)
{
	if (IReader* Chunk = F.open_chunk(u32(ERoadChunkIDs::Version)))
	{
		const u16 Version = Chunk->r_u16();
		Chunk->close();
		if (Version != ROAD_VERSION)
		{
			Msg("! Road [%s]: unsupported version %u", GetName(), Version);
			return false;
		}
	}

	if (IReader* Chunk = F.open_chunk(u32(ERoadChunkIDs::Spline)))
	{
		Spline.Load(*Chunk);
		Chunk->close();
	}

	if (IReader* Chunk = F.open_chunk(u32(ERoadChunkIDs::Params)))
	{
		SampleStep = Chunk->r_float();
		CrossSegments = Chunk->r_s32();
		ShoulderWidth = Chunk->r_float();
		HeightOffset = Chunk->r_float();
		UVLength = Chunk->r_float();
		ConformFalloff = Chunk->r_float();
		FollowTerrain = Chunk->r_u8() != 0;
		if (!Chunk->eof())
		{
			RoadTileWidth = Chunk->r_float();
			ShoulderUVLength = Chunk->r_float();
			ShoulderTileWidth = Chunk->r_float();
		}
		if (!Chunk->eof())
		{
			const u32 Marker = Chunk->r_u32();
			Chunk->r_u32();
			Chunk->r_float();
			if (Marker == 0xFFFFFFFF && !Chunk->eof())
			{
				shared_str Str;
				Chunk->r_stringZ(Str);
				MaskRoadMaterial = Str;
				Chunk->r_stringZ(Str);
				MaskShoulderMaterial = Str;
			}
		}
		Chunk->close();
	}

	if (IReader* Chunk = F.open_chunk(u32(ERoadChunkIDs::Surfaces)))
	{
		ReadSurface(*Chunk, RoadSurface);
		ReadSurface(*Chunk, ShoulderSurface);
		Chunk->close();
	}

	if (!inherited::LoadStream(F))
	{
		return false;
	}

	UpdateTransform(true);
	RebuildMesh();
	return true;
}

bool CRoad::RayPick(float& dist, const Fvector& S, const Fvector& D, SRayPickInfo* pinf)
{
	if (!HasMesh())
	{
		return false;
	}

	if (RoadObject->RayPick(dist, S, D, _ITransform(), pinf))
	{
		if (pinf)
		{
			pinf->s_obj = this;
		}
		return true;
	}
	return false;
}

void CRoad::RenderSpline()
{
	if (!Spline.IsValid() && Spline.Points.empty())
	{
		return;
	}

	const Fmatrix& M = _Transform();
	RCache.set_xform_world(Fidentity);

	const u32 LineColor = Selected() ? 0xFFFFD040 : 0xFF909090;
	const u32 EdgeColor = Selected() ? 0x80FFD040 : 0x60909090;
	const u32 ShoulderColor = 0x8040C0FF;

	xr_vector<SSplineSample> Samples;
	Spline.Sample(std::max(SampleStep, 1.f), Samples);
	for (size_t i = 1; i < Samples.size(); ++i)
	{
		const SSplineSample& A = Samples[i - 1];
		const SSplineSample& B = Samples[i];

		Fvector P0, P1;
		M.transform_tiny(P0, A.Pos);
		M.transform_tiny(P1, B.Pos);
		DU_impl.DrawLine(P0, P1, LineColor);

		if (Selected())
		{
			for (float Side : {-1.f, 1.f})
			{
				// Road edge
				Fvector E0, E1;
				E0.mad(A.Pos, A.Right, A.Width * 0.5f * Side);
				E1.mad(B.Pos, B.Right, B.Width * 0.5f * Side);
				M.transform_tiny(E0, Fvector(E0));
				M.transform_tiny(E1, Fvector(E1));
				DU_impl.DrawLine(E0, E1, EdgeColor);

				// Outer shoulder edge
				if (ShoulderWidth > 0.01f)
				{
					Fvector O0, O1;
					O0.mad(A.Pos, A.Right, (A.Width * 0.5f + ShoulderWidth) * Side);
					O1.mad(B.Pos, B.Right, (B.Width * 0.5f + ShoulderWidth) * Side);
					M.transform_tiny(O0, Fvector(O0));
					M.transform_tiny(O1, Fvector(O1));
					DU_impl.DrawLine(O0, O1, ShoulderColor);
				}
			}
		}
	}

	for (u32 i = 0; i < u32(Spline.Points.size()); ++i)
	{
		const Fvector P = PointWorld(i);
		u32 Color = 0xFF40A0FF;
		float Size = 0.35f;
		if (Selected() && IsPointSelected(i))
		{
			Color = 0xFFFF4040;
			Size = 0.55f;
		}
		else if (i == 0)
		{
			Color = 0xFF40FF40;
		}
		DU_impl.DrawRomboid(P, Size, Color);
	}
}

void CRoad::Render(int priority, bool strictB2F)
{
	if (HasMesh())
	{
		RoadObject->Render(this, _Transform(), priority, strictB2F);
	}

	if (priority == 1 && !strictB2F)
	{
		const bool RoadMode = LTools && LTools->GetTarget() == OBJCLASS_ROAD;
		if (RoadMode || Selected())
		{
			RenderSpline();
		}

		if (Selected() && m_TBBox.is_valid())
		{
			EDevice->SetShader(GEditorDraw.WireShader);
			RCache.set_xform_world(Fidentity);
			u32 Clr = Locked() ? 0xFFFF0000 : 0xFFFFFFFF;
			DU_impl.DrawSelectionBoxB(m_TBBox, &Clr);
		}
	}
}

void CRoad::FillProp(const char* pref, PropItemVec& items)
{
	inherited::FillProp(pref, items);

	PHelper().CreateCaption(items, PrepareKey(pref, "Road\\Points"), xr_string::ToString(u32(Spline.Points.size())).c_str());
	PHelper().CreateCaption(items, PrepareKey(pref, "Road\\Length"), (xr_string::ToString(GetLength()) + " m").c_str());

	PropValue* V;
	V = PHelper().CreateBool(items, PrepareKey(pref, "Road\\Closed Loop"), &Spline.Closed);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateBool(items, PrepareKey(pref, "Road\\Follow Terrain"), &FollowTerrain);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateFloat(items, PrepareKey(pref, "Road\\Height Offset"), &HeightOffset, -1.f, 1.f, 0.01f, 3);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateFloat(items, PrepareKey(pref, "Road\\Shoulder Width"), &ShoulderWidth, 0.f, 20.f, 0.1f, 2);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateFloat(items, PrepareKey(pref, "Road\\Conform Falloff"), &ConformFalloff, 0.f, 100.f, 0.5f, 1);
	V = PHelper().CreateFloat(items, PrepareKey(pref, "Road\\Mesh\\Sample Step"), &SampleStep, 0.25f, 20.f, 0.25f, 2);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateNumeric(items, PrepareKey(pref, "Road\\Mesh\\Cross Segments"), &CrossSegments, 1, 32);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateFloat(items, PrepareKey(pref, "Road\\Tiling\\Road Tile Length"), &UVLength, 0.1f, 200.f, 0.5f, 2);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateFloat(items, PrepareKey(pref, "Road\\Tiling\\Road Tile Width (0 = stretch)"), &RoadTileWidth, 0.f, 200.f, 0.5f, 2);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateFloat(items, PrepareKey(pref, "Road\\Tiling\\Shoulder Tile Length"), &ShoulderUVLength, 0.1f, 200.f, 0.5f, 2);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateFloat(items, PrepareKey(pref, "Road\\Tiling\\Shoulder Tile Width (0 = stretch)"), &ShoulderTileWidth, 0.f, 200.f, 0.5f, 2);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);

	// Names of terrain materials (Terrain Properties > Material Mask > Materials)
	V = PHelper().CreateRText(items, PrepareKey(pref, "Road\\Terrain Mask\\Road Material"), &MaskRoadMaterial);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	V = PHelper().CreateRText(items, PrepareKey(pref, "Road\\Terrain Mask\\Shoulder Material"), &MaskShoulderMaterial);
	V->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);

	auto AddSurface = [&](const char* Key, SSurface& S)
	{
		shared_str Pref = PrepareKey(pref, Key).c_str();
		PropValue* P;
		P = PHelper().CreateChoose(items, PrepareKey(Pref.c_str(), "Texture"), &S.Texture, smTexture);
		P->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
		P = PHelper().CreateChoose(items, PrepareKey(Pref.c_str(), "Shader"), &S.Shader, smEShader);
		P->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
		P = PHelper().CreateChoose(items, PrepareKey(Pref.c_str(), "Compile"), &S.ShaderXRLC, smCShader);
		P->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
		P = PHelper().CreateChoose(items, PrepareKey(Pref.c_str(), "Game Mtl"), &S.GameMtl, smGameMaterial);
		P->OnChangeEvent.bind(this, &CRoad::OnChangeMeshProp);
	};

	AddSurface("Surfaces\\road", RoadSurface);
	AddSurface("Surfaces\\shoulder", ShoulderSurface);

	FillPointProps(pref, items);
}

void CRoad::OnChangeMeshProp(PropValue* sender)
{
	RebuildMesh();
}

void CRoad::BoxQuery(SPickQuery& pinf)
{
	if (HasMesh())
	{
		RoadObject->BoxQuery(_Transform(), _ITransform(), pinf);
	}
}

void CRoad::RayQuery(SPickQuery& pinf)
{
	if (HasMesh())
	{
		RoadObject->RayQuery(_Transform(), _ITransform(), pinf);
	}
}

bool CRoad::GetBox(Fbox& box)
{
	if (!m_TBBox.is_valid())
	{
		return false;
	}
	box.set(m_TBBox);
	return true;
}

bool CRoad::GetUTBox(Fbox& box)
{
	if (!HasMesh())
	{
		return false;
	}
	box.set(RoadObject->GetBox());
	return true;
}

bool CRoad::BoxPick(const Fbox& box, SBoxPickInfoVec& pinf)
{
	if (!HasMesh())
	{
		return false;
	}
	return RoadObject->BoxPick(this, box, _ITransform(), pinf);
}
