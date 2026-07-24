#include "stdafx.h"
#include "TiramisuEditorLegacySceneBridge.h"
#include "TiramisuEditorNativeScene.h"

#include "../../../xrECore/Editor/EditorRenderBackend.h"
#include "../../../xrECore/Editor/UI_ToolsCustom.h"
#include "../../Editor/Entry/StaticObject/SceneObject.h"
#include "../../Editor/Scene/scene.h"

#include <algorithm>
#include <bit>
#include <cmath>
#include <cstdint>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace
{
constexpr u64 FnvOffset = 14695981039346656037ull;
constexpr u64 FnvPrime = 1099511628211ull;
constexpr u64 FallbackMaterialSlot = 0x746972616d697375ull;

struct FViewportBridgeState
{
	xr_hash_map<u64, u64> MeshRevisions;
	u64 SceneRevision = 0;
};

struct FMeshPayload
{
	FEditorStaticMeshId MeshId;
	u64 Revision = 0;
	xr_vector<FEditorStaticMeshVertex> Vertices;
	xr_vector<u32> Indices;
	xr_vector<FEditorStaticMeshSection> Sections;

	[[nodiscard]] FEditorStaticMeshUpload MakeUpload() const
	{
		return {MeshId, Revision, Vertices, Indices, Sections};
	}
};

xr_hash_map<u32, FViewportBridgeState> ViewportStates;

void HashBytes(u64& Hash, const void* Data, const size_t Size)
{
	const auto* Bytes = static_cast<const u8*>(Data);
	for (size_t Index = 0; Index < Size; ++Index)
	{
		Hash ^= Bytes[Index];
		Hash *= FnvPrime;
	}
}

void HashString(u64& Hash, const char* Text)
{
	if (Text)
		HashBytes(Hash, Text, xr_strlen(Text));
	const u8 Separator = 0;
	HashBytes(Hash, &Separator, sizeof(Separator));
}

[[nodiscard]] u64 MakePointerId(const void* Pointer)
{
	u64 Value = static_cast<u64>(
		reinterpret_cast<std::uintptr_t>(Pointer));
	// Zero is reserved for invalid handles. Pointer identity is only used for
	// the lifetime of this editor process and never serialized into assets.
	return Value == 0 ? 1 : Value;
}

[[nodiscard]] FEditorMaterialSlotId MakeMaterialSlotId(const CSurface* Surface)
{
	if (!Surface)
		return {FallbackMaterialSlot};
	u64 Hash = FnvOffset;
	HashString(Hash, Surface->_ShaderName());
	HashString(Hash, Surface->_Texture());
	HashString(Hash, Surface->_Name());
	const bool TwoSided = Surface->m_Flags.is(CSurface::sf2Sided);
	HashBytes(Hash, &TwoSided, sizeof(TwoSided));
	if (Hash == 0)
		Hash = 1;
	return {Hash};
}

[[nodiscard]] FEditorMaterialSlotSource MakeMaterialSlotSource(
	const CSurface* Surface)
{
	if (!Surface)
	{
		return {{FallbackMaterialSlot}, "default",
			"textures/default/default_white", "Unassigned",
			EEditorMaterialSlotFlags::None};
	}
	return {MakeMaterialSlotId(Surface), Surface->_ShaderName(),
		Surface->_Texture(), Surface->_Name(),
		Surface->m_Flags.is(CSurface::sf2Sided)
			? EEditorMaterialSlotFlags::TwoSided
			: EEditorMaterialSlotFlags::None};
}

[[nodiscard]] FEditorMaterialSlotId MakeMaterialOverrideSlotId(
	const FEditorMaterialSlotId BaseMaterialSlot,
	const xr_string_view MaterialAsset)
{
	u64 Hash = FnvOffset;
	HashString(Hash, "legacy-scene-material-override");
	HashBytes(Hash, &BaseMaterialSlot.Value,
		sizeof(BaseMaterialSlot.Value));
	HashBytes(Hash, MaterialAsset.data(), MaterialAsset.size());
	if (Hash == 0)
		Hash = 1;
	return {Hash};
}

[[nodiscard]] const CSurface* FindObjectSurface(
	const CSceneObject& SceneObject, const char* SurfaceName)
{
	if (!SurfaceName)
		return nullptr;
	for (const CSurface* Surface : SceneObject.m_Surfaces)
	{
		if (Surface && xr_strcmp(Surface->_Name(), SurfaceName) == 0)
			return Surface;
	}
	return nullptr;
}

void AppendLegacyMaterialOverrides(CSceneObject& SceneObject,
	CEditableMesh& Mesh, FEditorStaticMeshInstance& Instance,
	xr_hash_map<u64, FEditorMaterialSlotSource>&
		MaterialSlotsById)
{
	for (const auto& [BaseSurface, Faces] : Mesh.GetSurfFaces())
	{
		(void)Faces;
		if (!BaseSurface)
			continue;
		const char* MaterialAsset =
			SceneObject.GetRenderMaterialAsset(BaseSurface->_Name());
		if (!MaterialAsset || !MaterialAsset[0])
			continue;

		const FEditorMaterialSlotId BaseSlot =
			MakeMaterialSlotId(BaseSurface);
		const FEditorMaterialSlotId OverrideSlot =
			MakeMaterialOverrideSlotId(BaseSlot, MaterialAsset);
		const CSurface* ObjectSurface =
			FindObjectSurface(SceneObject, BaseSurface->_Name());
		FEditorMaterialSlotSource Source =
			MakeMaterialSlotSource(ObjectSurface
				? ObjectSurface : BaseSurface);
		Source.MaterialSlot = OverrideSlot;
		Source.MaterialAsset = MaterialAsset;
		MaterialSlotsById.insert_or_assign(OverrideSlot.Value, Source);
		Instance.MaterialOverrides.push_back({BaseSlot, OverrideSlot});
	}
}

void CopyMatrix(const Fmatrix& Source, xr_array<float, 16>& Destination)
{
	std::copy_n(Source.mm, Destination.size(), Destination.begin());
}

[[nodiscard]] FEditorDebugVertex MakeDebugVertex(const Fvector& Position,
	const u32 Color)
{
	FEditorDebugVertex Vertex;
	Vertex.Position = {Position.x, Position.y, Position.z};
	const float Scale = 1.0f / 255.0f;
	const u32 Alpha = Color >> 24u;
	Vertex.Color = {
		static_cast<float>((Color >> 16u) & 0xffu) * Scale,
		static_cast<float>((Color >> 8u) & 0xffu) * Scale,
		static_cast<float>(Color & 0xffu) * Scale,
		Alpha == 0 ? 1.0f : static_cast<float>(Alpha) * Scale};
	return Vertex;
}

void AppendDebugLine(xr_vector<FEditorDebugLine>& Lines,
	const Fvector& Start, const Fvector& End, const u32 Color)
{
	FEditorDebugLine& Line = Lines.emplace_back();
	Line.Vertices[0] = MakeDebugVertex(Start, Color);
	Line.Vertices[1] = MakeDebugVertex(End, Color);
}

[[nodiscard]] u64 BuildDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles,
	xr_vector<FEditorOverlayLine>& OverlayLines,
	xr_vector<FEditorOverlayTriangle>& OverlayTriangles,
	xr_vector<FEditorOverlayText>& OverlayText,
	xr_vector<FEditorTransientMeshCapture>& TransientMeshes)
{
	Lines.clear();
	Triangles.clear();
	OverlayLines.clear();
	OverlayTriangles.clear();
	OverlayText.clear();
	TransientMeshes.clear();
	const bool CaptureActive = IsEditorDebugDrawCaptureActive();
	xr_vector<FEditorDebugLine> CapturedLines;
	xr_vector<FEditorDebugTriangle> CapturedTriangles;
	xr_vector<FEditorOverlayLine> CapturedOverlayLines;
	xr_vector<FEditorOverlayTriangle> CapturedOverlayTriangles;
	xr_vector<FEditorOverlayText> CapturedOverlayText;
	xr_vector<FEditorTransientMeshCapture> CapturedTransientMeshes;
	EndEditorDebugDrawCapture(CapturedLines, CapturedTriangles,
		CapturedOverlayLines, CapturedOverlayTriangles, CapturedOverlayText,
		CapturedTransientMeshes);
	if (CaptureActive)
	{
		Lines = std::move(CapturedLines);
		Triangles = std::move(CapturedTriangles);
		OverlayLines = std::move(CapturedOverlayLines);
		OverlayTriangles = std::move(CapturedOverlayTriangles);
		OverlayText = std::move(CapturedOverlayText);
		TransientMeshes = std::move(CapturedTransientMeshes);
	}
	else if (Tools)
	{
		const CToolCustom::SDebugDraw& Source = Tools->m_DebugDraw;
		for (const CToolCustom::SDebugDraw::Point& Point : Source.m_Points)
		{
			constexpr float Radius = 0.025f;
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				Fvector Start = Point.p[0];
				Fvector End = Point.p[0];
				Start[Axis] -= Radius;
				End[Axis] += Radius;
				AppendDebugLine(Lines, Start, End, Point.c);
			}
		}
		for (const CToolCustom::SDebugDraw::Line& Line : Source.m_Lines)
			AppendDebugLine(Lines, Line.p[0], Line.p[1], Line.c);
		for (const CToolCustom::SDebugDraw::Face& Face : Source.m_WireFaces)
		{
			AppendDebugLine(Lines, Face.p[0], Face.p[1], Face.c);
			AppendDebugLine(Lines, Face.p[1], Face.p[2], Face.c);
			AppendDebugLine(Lines, Face.p[2], Face.p[0], Face.c);
		}
		for (const CToolCustom::SDebugDraw::Face& Face : Source.m_SolidFaces)
		{
			FEditorDebugTriangle& Triangle = Triangles.emplace_back();
			for (u32 Corner = 0; Corner < 3; ++Corner)
				Triangle.Vertices[Corner] = MakeDebugVertex(Face.p[Corner], Face.c);
		}
		for (const Fobb& Box : Source.m_OBB)
		{
			xr_array<Fvector, 8> Corners;
			for (u32 Index = 0; Index < Corners.size(); ++Index)
			{
				Fvector& Corner = Corners[Index];
				Corner = Box.m_translate;
				Corner.mad(Box.m_rotate.i,
					(Index & 1u) ? Box.m_halfsize.x : -Box.m_halfsize.x);
				Corner.mad(Box.m_rotate.j,
					(Index & 2u) ? Box.m_halfsize.y : -Box.m_halfsize.y);
				Corner.mad(Box.m_rotate.k,
					(Index & 4u) ? Box.m_halfsize.z : -Box.m_halfsize.z);
			}
			constexpr xr_array<xr_array<u32, 2>, 12> Edges = {{
				{{0, 1}}, {{2, 3}}, {{4, 5}}, {{6, 7}},
				{{0, 2}}, {{1, 3}}, {{4, 6}}, {{5, 7}},
				{{0, 4}}, {{1, 5}}, {{2, 6}}, {{3, 7}}}};
			for (const auto& Edge : Edges)
				AppendDebugLine(Lines, Corners[Edge[0]], Corners[Edge[1]],
					0xff00ff00u);
		}
	}

	u64 Revision = FnvOffset;
	auto HashVertex = [&](const FEditorDebugVertex& Vertex)
	{
		HashBytes(Revision, Vertex.Position.data(),
			Vertex.Position.size() * sizeof(float));
		HashBytes(Revision, Vertex.Color.data(),
			Vertex.Color.size() * sizeof(float));
	};
	for (const FEditorDebugLine& Line : Lines)
		for (const FEditorDebugVertex& Vertex : Line.Vertices) HashVertex(Vertex);
	for (const FEditorDebugTriangle& Triangle : Triangles)
		for (const FEditorDebugVertex& Vertex : Triangle.Vertices) HashVertex(Vertex);
	auto HashOverlayVertex = [&](const FEditorOverlayVertex& Vertex)
	{
		HashBytes(Revision, Vertex.Position.data(),
			Vertex.Position.size() * sizeof(float));
		HashBytes(Revision, Vertex.Color.data(),
			Vertex.Color.size() * sizeof(float));
	};
	for (const FEditorOverlayLine& Line : OverlayLines)
		for (const FEditorOverlayVertex& Vertex : Line.Vertices)
			HashOverlayVertex(Vertex);
	for (const FEditorOverlayTriangle& Triangle : OverlayTriangles)
		for (const FEditorOverlayVertex& Vertex : Triangle.Vertices)
			HashOverlayVertex(Vertex);
	for (const FEditorOverlayText& Text : OverlayText)
	{
		HashBytes(Revision, Text.Position.data(),
			Text.Position.size() * sizeof(float));
		HashBytes(Revision, Text.Color.data(), Text.Color.size() * sizeof(float));
		HashBytes(Revision, Text.ShadowColor.data(),
			Text.ShadowColor.size() * sizeof(float));
		HashString(Revision, Text.Text.c_str());
	}
	return Revision == 0 ? 1 : Revision;
}

[[nodiscard]] bool ComputeMeshRevision(CEditableMesh& Mesh,
	u64& OutRevision)
{
	const Fvector* Vertices = Mesh.GetVertices();
	const st_Face* Faces = Mesh.GetFaces();
	const u32 VertexCount = Mesh.GetVCount();
	const u32 FaceCount = Mesh.GetFCount();
	if (!Vertices || !Faces || VertexCount == 0 || FaceCount == 0)
		return false;

	u64 Hash = FnvOffset;
	HashBytes(Hash, &VertexCount, sizeof(VertexCount));
	HashBytes(Hash, &FaceCount, sizeof(FaceCount));
	for (u32 VertexIndex = 0; VertexIndex < VertexCount; ++VertexIndex)
	{
		HashBytes(Hash, &Vertices[VertexIndex].x, sizeof(float));
		HashBytes(Hash, &Vertices[VertexIndex].y, sizeof(float));
		HashBytes(Hash, &Vertices[VertexIndex].z, sizeof(float));
	}
	for (u32 FaceIndex = 0; FaceIndex < FaceCount; ++FaceIndex)
	{
		for (const st_FaceVert& FaceVertex : Faces[FaceIndex].pv)
		{
			if (FaceVertex.pindex < 0 ||
				static_cast<u32>(FaceVertex.pindex) >= VertexCount)
			{
				return false;
			}
			HashBytes(Hash, &FaceVertex.pindex, sizeof(FaceVertex.pindex));
		}
	}
	for (const auto& [Surface, SurfaceFaces] : Mesh.GetSurfFaces())
	{
		const FEditorMaterialSlotId Slot = MakeMaterialSlotId(Surface);
		HashBytes(Hash, &Slot.Value, sizeof(Slot.Value));
		for (const int FaceIndex : SurfaceFaces)
			HashBytes(Hash, &FaceIndex, sizeof(FaceIndex));
	}
	OutRevision = Hash == 0 ? 1 : Hash;
	return true;
}

void AppendFace(CEditableMesh& Mesh, const u32 FaceIndex,
	FMeshPayload& Payload)
{
	const st_Face& Face = Mesh.GetFaces()[FaceIndex];
	const Fvector* SourceVertices = Mesh.GetVertices();
	const Fvector& A = SourceVertices[Face.pv[0].pindex];
	const Fvector& B = SourceVertices[Face.pv[1].pindex];
	const Fvector& C = SourceVertices[Face.pv[2].pindex];
	Fvector Normal;
	Normal.mknormal(A, B, C);

	const Fvector* Triangle[] = {&A, &B, &C};
	const VMRefsVec& References = Mesh.GetVMRefs();
	const VMapVec& VertexMaps = Mesh.GetVMaps();
	for (u32 Corner = 0; Corner < 3; ++Corner)
	{
		FEditorStaticMeshVertex Vertex;
		const Fvector* Position = Triangle[Corner];
		Vertex.Position = {Position->x, Position->y, Position->z};
		Vertex.Normal = {Normal.x, Normal.y, Normal.z};
		const int ReferenceIndex = Face.pv[Corner].vmref;
		if (ReferenceIndex >= 0 &&
			static_cast<size_t>(ReferenceIndex) < References.size())
		{
			const st_VMapPtLst& Reference = References[ReferenceIndex];
			for (u32 Layer = 0; Layer < Reference.count; ++Layer)
			{
				const st_VMapPt& Point = Reference.pts[Layer];
				if (Point.vmap_index < 0 || Point.index < 0 ||
					static_cast<size_t>(Point.vmap_index) >= VertexMaps.size())
				{
					continue;
				}
				const st_VMap* VertexMap = VertexMaps[Point.vmap_index];
				if (!VertexMap || VertexMap->type != vmtUV ||
					Point.index >= VertexMap->size())
				{
					continue;
				}
				const Fvector2& TexCoord = VertexMap->getUV(Point.index);
				Vertex.TexCoord = {TexCoord.x, TexCoord.y};
				break;
			}
		}
		Payload.Indices.push_back(
			static_cast<u32>(Payload.Vertices.size()));
		Payload.Vertices.push_back(Vertex);
	}
}

[[nodiscard]] bool BuildMeshPayload(CEditableMesh& Mesh,
	const FEditorStaticMeshId MeshId, const u64 Revision,
	FMeshPayload& OutPayload)
{
	OutPayload = {};
	OutPayload.MeshId = MeshId;
	OutPayload.Revision = Revision;
	const u32 FaceCount = Mesh.GetFCount();
	xr_vector<bool> AddedFaces(FaceCount, false);

	for (const auto& [Surface, SurfaceFaces] : Mesh.GetSurfFaces())
	{
		FEditorStaticMeshSection Section;
		Section.FirstIndex = static_cast<u32>(OutPayload.Indices.size());
		Section.MaterialSlot = MakeMaterialSlotId(Surface);
		for (const int SignedFaceIndex : SurfaceFaces)
		{
			if (SignedFaceIndex < 0)
				continue;
			const u32 FaceIndex =
				static_cast<u32>(SignedFaceIndex);
			if (FaceIndex >= FaceCount || AddedFaces[FaceIndex])
				continue;
			AppendFace(Mesh, FaceIndex, OutPayload);
			AddedFaces[FaceIndex] = true;
		}
		Section.IndexCount = static_cast<u32>(OutPayload.Indices.size()) -
			Section.FirstIndex;
		if (Section.IndexCount != 0)
			OutPayload.Sections.push_back(Section);
	}

	FEditorStaticMeshSection Unassigned;
	Unassigned.FirstIndex = static_cast<u32>(OutPayload.Indices.size());
	for (u32 FaceIndex = 0; FaceIndex < FaceCount; ++FaceIndex)
	{
		if (!AddedFaces[FaceIndex])
			AppendFace(Mesh, FaceIndex, OutPayload);
	}
	Unassigned.IndexCount = static_cast<u32>(OutPayload.Indices.size()) -
		Unassigned.FirstIndex;
	if (Unassigned.IndexCount != 0)
		OutPayload.Sections.push_back(Unassigned);

	return !OutPayload.Vertices.empty() && !OutPayload.Indices.empty();
}

[[nodiscard]] FEditorMaterialSlotId MakeNativeMaterialSlotId(
	const Tiramisu::Scene::FStaticMeshAsset& Mesh,
	const u32 SlotIndex)
{
	xr_string Identity = Mesh.Id;
	Identity += ":material-slot:";
	Identity += std::to_string(SlotIndex);
	return {Tiramisu::Scene::StableSceneIdHash(Identity)};
}

[[nodiscard]] FEditorMaterialSlotId MakeNativeMaterialOverrideId(
	const Tiramisu::Scene::FStaticMeshComponent& Component,
	const Tiramisu::Scene::FStaticMeshMaterialOverride& Override)
{
	xr_string Identity = Component.Id;
	Identity += ":material-override:";
	Identity += std::to_string(Override.MaterialSlot);
	Identity += ':';
	Identity += Override.Material;
	return {Tiramisu::Scene::StableSceneIdHash(Identity)};
}

[[nodiscard]] bool BuildNativeScenePayload(
	const TiramisuEditorNativeSceneDocument& Document,
	const Tiramisu::Scene::FResolvedRenderScene& NativeScene,
	const FViewportBridgeState& State,
	xr_hash_map<u64, u64>& CurrentMeshRevisions,
	xr_hash_map<u64, FEditorMaterialSlotSource>&
		MaterialSlotsById,
	xr_vector<FMeshPayload>& ChangedPayloads,
	xr_vector<FEditorStaticMeshInstance>& Instances,
	xr_vector<FEditorSceneLight>& Lights)
{
	for (const auto& [Reference, Mesh] : NativeScene.StaticMeshes)
	{
		(void)Reference;
		const FEditorStaticMeshId MeshId{
			Tiramisu::Scene::StableSceneIdHash(Mesh.Id)};
		const u64 Revision =
			Tiramisu::Scene::CalculateStaticMeshRevision(Mesh);
		if (!CurrentMeshRevisions.emplace(MeshId.Value, Revision).second)
			return false;

		for (u32 SlotIndex = 0;
			SlotIndex < Mesh.MaterialSlots.size(); ++SlotIndex)
		{
			const Tiramisu::Scene::FStaticMeshMaterialSlot& Slot =
				Mesh.MaterialSlots[SlotIndex];
			const FEditorMaterialSlotId SlotId =
				MakeNativeMaterialSlotId(Mesh, SlotIndex);
			const EEditorMaterialSlotFlags Flags = Slot.TwoSided
				? EEditorMaterialSlotFlags::TwoSided
				: EEditorMaterialSlotFlags::None;
			MaterialSlotsById.insert_or_assign(SlotId.Value,
				FEditorMaterialSlotSource{SlotId, {}, {}, Slot.Name,
					Flags, Slot.Material});
		}

		const auto Existing = State.MeshRevisions.find(MeshId.Value);
		if (Existing != State.MeshRevisions.end() &&
			Existing->second == Revision)
		{
			continue;
		}
		FMeshPayload Payload;
		Payload.MeshId = MeshId;
		Payload.Revision = Revision;
		Payload.Vertices.reserve(Mesh.Vertices.size());
		for (const Tiramisu::Scene::FStaticMeshVertex& Source :
			Mesh.Vertices)
		{
			FEditorStaticMeshVertex Vertex;
			Vertex.Position = Source.Position;
			Vertex.Normal = Source.Normal;
			Vertex.Tangent = Source.Tangent;
			Vertex.TexCoord = Source.TexCoord0;
			Vertex.TexCoord1 = Source.TexCoord1;
			Vertex.Color = Source.Color;
			Payload.Vertices.push_back(Vertex);
		}
		Payload.Indices = Mesh.Indices;
		Payload.Sections.reserve(Mesh.Sections.size());
		for (const Tiramisu::Scene::FStaticMeshSection& Source :
			Mesh.Sections)
		{
			if (Source.MaterialSlot >= Mesh.MaterialSlots.size())
				return false;
			Payload.Sections.push_back({Source.FirstIndex,
				Source.IndexCount,
				MakeNativeMaterialSlotId(Mesh, Source.MaterialSlot)});
		}
		ChangedPayloads.push_back(std::move(Payload));
	}

	Instances.reserve(NativeScene.Scene.StaticMeshComponents.size());
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		NativeScene.Scene.StaticMeshComponents)
	{
		if (!Component.Visible)
			continue;
		const auto Mesh = NativeScene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == NativeScene.StaticMeshes.end())
			return false;
		FEditorStaticMeshInstance Instance;
		Instance.ObjectId = {
			Tiramisu::Scene::StableSceneIdHash(Component.Id)};
		Instance.MeshId = {
			Tiramisu::Scene::StableSceneIdHash(Mesh->second.Id)};
		Instance.LocalToWorld = Component.LocalToWorld;
		if (Document.IsComponentSelected(Component.Id))
			Instance.Flags = EEditorSceneInstanceFlags::Selected;
		for (const Tiramisu::Scene::FStaticMeshMaterialOverride& Override :
			Component.MaterialOverrides)
		{
			if (Override.MaterialSlot >=
				Mesh->second.MaterialSlots.size())
			{
				return false;
			}
			const FEditorMaterialSlotId BaseSlot =
				MakeNativeMaterialSlotId(
					Mesh->second, Override.MaterialSlot);
			const FEditorMaterialSlotId OverrideSlot =
				MakeNativeMaterialOverrideId(Component, Override);
			const Tiramisu::Scene::FStaticMeshMaterialSlot& Base =
				Mesh->second.MaterialSlots[Override.MaterialSlot];
			const EEditorMaterialSlotFlags Flags = Override.TwoSided
				? EEditorMaterialSlotFlags::TwoSided
				: EEditorMaterialSlotFlags::None;
			MaterialSlotsById.insert_or_assign(OverrideSlot.Value,
				FEditorMaterialSlotSource{OverrideSlot, {}, {},
					Base.Name, Flags, Override.Material});
			Instance.MaterialOverrides.push_back(
				{BaseSlot, OverrideSlot});
		}
		Instances.push_back(Instance);
	}
	Lights.reserve(NativeScene.Scene.LightComponents.size());
	for (const Tiramisu::Scene::FLightComponent& Source :
		NativeScene.Scene.LightComponents)
	{
		if (!Source.Visible)
			continue;
		FEditorSceneLight Light;
		Light.ObjectId = {
			Tiramisu::Scene::StableSceneIdHash(Source.Id)};
		switch (Source.Type)
		{
		case Tiramisu::Scene::ELightType::Directional:
			Light.Type = EEditorSceneLightType::Directional;
			break;
		case Tiramisu::Scene::ELightType::Point:
			Light.Type = EEditorSceneLightType::Point;
			break;
		case Tiramisu::Scene::ELightType::Spot:
			Light.Type = EEditorSceneLightType::Spot;
			break;
		}
		Light.LocalToWorld = Source.LocalToWorld;
		Light.Color = Source.Color;
		Light.Intensity = Source.Intensity;
		Light.Range = Source.Range;
		Light.InnerConeAngleDegrees =
			Source.InnerConeAngleDegrees;
		Light.OuterConeAngleDegrees =
			Source.OuterConeAngleDegrees;
		u32 Flags = Source.CastShadows
			? static_cast<u32>(
				EEditorSceneLightFlags::CastShadows)
			: 0u;
		if (Document.IsComponentSelected(Source.Id))
		{
			Flags |= static_cast<u32>(
				EEditorSceneLightFlags::Selected);
		}
		Light.Flags = static_cast<EEditorSceneLightFlags>(Flags);
		Lights.push_back(Light);
	}
	return true;
}

[[nodiscard]] FEditorViewportCamera BuildCamera()
{
	FEditorViewportCamera Camera;
	CopyMatrix(EDevice->mView, Camera.View);
	CopyMatrix(EDevice->mProject, Camera.Projection);
	CopyMatrix(EDevice->mFullTransform, Camera.ViewProjection);
	const Fvector& Position = UI->CurrentView().m_Camera.GetPosition();
	Camera.WorldPosition = {Position.x, Position.y, Position.z};
	Camera.NearPlane = UI->CurrentView().m_Camera._Znear();
	Camera.FarPlane = UI->CurrentView().m_Camera._Zfar();
	return Camera;
}

void AppendNativeLightDebugDraw(
	const xr_vector<FEditorSceneLight>& Lights,
	xr_vector<FEditorDebugLine>& Lines,
	u64& Revision)
{
	const size_t FirstLine = Lines.size();
	for (const FEditorSceneLight& Light : Lights)
	{
		const xr_array<float, 3> Position = {
			Light.LocalToWorld[12],
			Light.LocalToWorld[13],
			Light.LocalToWorld[14]};
		xr_array<float, 4> Color = {
			std::clamp(Light.Color[0], 0.1f, 1.0f),
			std::clamp(Light.Color[1], 0.1f, 1.0f),
			std::clamp(Light.Color[2], 0.1f, 1.0f), 1.0f};
		if ((static_cast<u32>(Light.Flags) &
				static_cast<u32>(
					EEditorSceneLightFlags::Selected)) != 0)
		{
			Color = {1.0f, 0.55f, 0.08f, 1.0f};
		}
		const auto AddLine =
			[&](const xr_array<float, 3>& Start,
				const xr_array<float, 3>& End)
			{
				FEditorDebugLine& Line = Lines.emplace_back();
				Line.Vertices[0] = {Start, Color};
				Line.Vertices[1] = {End, Color};
			};
		constexpr float IconExtent = 0.25f;
		for (size_t Axis = 0; Axis < 3; ++Axis)
		{
			xr_array<float, 3> Start = Position;
			xr_array<float, 3> End = Position;
			Start[Axis] -= IconExtent;
			End[Axis] += IconExtent;
			AddLine(Start, End);
		}
		if (Light.Type != EEditorSceneLightType::Point)
		{
			xr_array<float, 3> Direction = {
				Light.LocalToWorld[8],
				Light.LocalToWorld[9],
				Light.LocalToWorld[10]};
			const float Length = std::sqrt(
				Direction[0] * Direction[0] +
				Direction[1] * Direction[1] +
				Direction[2] * Direction[2]);
			if (Length > 1.0e-6f)
			{
				const float Scale =
					Light.Type == EEditorSceneLightType::Spot
					? std::min(Light.Range, 2.0f) : 2.0f;
				xr_array<float, 3> End = Position;
				for (size_t Axis = 0; Axis < 3; ++Axis)
					End[Axis] += Direction[Axis] / Length * Scale;
				AddLine(Position, End);
			}
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(Revision, Vertex.Position.data(),
				Vertex.Position.size() * sizeof(float));
			HashBytes(Revision, Vertex.Color.data(),
				Vertex.Color.size() * sizeof(float));
		}
	}
	if (Revision == 0)
		Revision = 1;
}
} // namespace

bool SubmitEditorSceneToEditorRenderer(const u32 ViewportId)
{
	if (!Scene || !UI || !EDevice)
		return false;
	IEditorRenderBackend& Renderer = GetEditorRenderBackend();
	if (Renderer.GetKind() != EEditorRenderBackendKind::Tiramisu)
		return true;

	FViewportBridgeState& State = ViewportStates[ViewportId];
	const TiramisuEditorNativeSceneDocument& NativeDocument =
		GetEditorNativeSceneDocument();
	const Tiramisu::Scene::FResolvedRenderScene* NativeScene =
		NativeDocument.GetScene();
	xr_vector<xr_pair<CSceneObject*, CEditableMesh*>> VisibleMeshInstances;
	xr_hash_set<CEditableMesh*> UniqueMeshes;
	if (!NativeScene)
	{
		for (CCustomObject* CustomObject :
			Scene->ListObj(OBJCLASS_SCENEOBJECT))
		{
			if (!CustomObject || !CustomObject->Visible())
				continue;
			auto* SceneObject = static_cast<CSceneObject*>(CustomObject);
			// Resolve all visible legacy objects in one migration transaction.
			// The cache makes subsequent viewport submissions read-only.
			SceneObject->ResolveRenderMaterials(true);
			EditMeshVec* Meshes = SceneObject->Meshes();
			if (!Meshes)
				continue;
			for (CEditableMesh* Mesh : *Meshes)
			{
				if (!Mesh || !Mesh->Visible())
					continue;
				VisibleMeshInstances.emplace_back(SceneObject, Mesh);
				UniqueMeshes.insert(Mesh);
			}
		}
		if (!CSceneObject::FlushRenderMaterialMigration())
			Msg("! Tiramisu could not publish legacy scene material migration database.");
	}

	xr_hash_map<u64, u64> CurrentMeshRevisions;
	xr_hash_map<u64, FEditorMaterialSlotSource>
		MaterialSlotsById;
	if (!NativeScene)
	{
		const FEditorMaterialSlotSource FallbackMaterial =
			MakeMaterialSlotSource(nullptr);
		MaterialSlotsById.emplace(FallbackMaterial.MaterialSlot.Value,
			FallbackMaterial);
		for (CEditableMesh* Mesh : UniqueMeshes)
		{
			for (const auto& [Surface, Faces] : Mesh->GetSurfFaces())
			{
				(void)Faces;
				const FEditorMaterialSlotSource Material =
					MakeMaterialSlotSource(Surface);
				MaterialSlotsById.insert_or_assign(
					Material.MaterialSlot.Value, Material);
			}
		}
	}
	xr_vector<FMeshPayload> ChangedPayloads;
	xr_vector<FEditorStaticMeshInstance> Instances;
	xr_vector<FEditorSceneLight> Lights;
	if (NativeScene)
	{
		if (!BuildNativeScenePayload(NativeDocument, *NativeScene, State,
				CurrentMeshRevisions, MaterialSlotsById,
				ChangedPayloads, Instances, Lights))
		{
			return false;
		}
	}
	else
	{
		for (CEditableMesh* Mesh : UniqueMeshes)
		{
			const FEditorStaticMeshId MeshId{MakePointerId(Mesh)};
			u64 Revision = 0;
			if (!ComputeMeshRevision(*Mesh, Revision))
				continue;
			CurrentMeshRevisions.emplace(MeshId.Value, Revision);
			const auto Existing = State.MeshRevisions.find(MeshId.Value);
			if (Existing != State.MeshRevisions.end() &&
				Existing->second == Revision)
				continue;
			FMeshPayload Payload;
			if (BuildMeshPayload(*Mesh, MeshId, Revision, Payload))
				ChangedPayloads.push_back(std::move(Payload));
			else
				CurrentMeshRevisions.erase(MeshId.Value);
		}

		Instances.reserve(VisibleMeshInstances.size());
		for (const auto& [SceneObject, Mesh] : VisibleMeshInstances)
		{
			const FEditorStaticMeshId MeshId{MakePointerId(Mesh)};
			if (!CurrentMeshRevisions.contains(MeshId.Value))
				continue;
			FEditorStaticMeshInstance Instance;
			Instance.ObjectId = {MakePointerId(SceneObject)};
			Instance.MeshId = MeshId;
			Fmatrix LocalToWorld;
			SceneObject->GetFullTransformToWorld(LocalToWorld);
			CopyMatrix(LocalToWorld, Instance.LocalToWorld);
			if (SceneObject->Selected())
				Instance.Flags = EEditorSceneInstanceFlags::Selected;
			AppendLegacyMaterialOverrides(*SceneObject, *Mesh, Instance,
				MaterialSlotsById);
			Instances.push_back(Instance);
		}
	}
	xr_vector<FEditorDebugLine> DebugLines;
	xr_vector<FEditorDebugTriangle> DebugTriangles;
	xr_vector<FEditorOverlayLine> OverlayLines;
	xr_vector<FEditorOverlayTriangle> OverlayTriangles;
	xr_vector<FEditorOverlayText> OverlayText;
	xr_vector<FEditorTransientMeshCapture> TransientMeshes;
	u64 DebugDrawRevision =
		BuildDebugDraw(DebugLines, DebugTriangles, OverlayLines,
			OverlayTriangles, OverlayText, TransientMeshes);
	AppendNativeLightDebugDraw(
		Lights, DebugLines, DebugDrawRevision);

	for (const FEditorTransientMeshCapture& Capture : TransientMeshes)
	{
		MaterialSlotsById.insert_or_assign(Capture.MaterialSlot.Value,
			FEditorMaterialSlotSource{Capture.MaterialSlot, Capture.ShaderName,
				Capture.TextureName, Capture.SurfaceName, Capture.MaterialFlags});

		const auto [RevisionIt, Inserted] = CurrentMeshRevisions.emplace(
			Capture.MeshId.Value, Capture.Revision);
		if (Inserted)
		{
			const auto Existing = State.MeshRevisions.find(Capture.MeshId.Value);
			if (Existing == State.MeshRevisions.end() ||
				Existing->second != Capture.Revision)
			{
				FMeshPayload Payload;
				Payload.MeshId = Capture.MeshId;
				Payload.Revision = Capture.Revision;
				Payload.Vertices = Capture.Vertices;
				Payload.Indices = Capture.Indices;
				Payload.Sections.push_back({0,
					static_cast<u32>(Payload.Indices.size()),
					Capture.MaterialSlot});
				ChangedPayloads.push_back(std::move(Payload));
			}
		}
		else if (RevisionIt->second != Capture.Revision)
		{
			// A stable mesh ID must describe one immutable geometry revision per
			// redraw. Ignore the conflicting capture instead of corrupting the
			// backend cache transaction.
			continue;
		}

		FEditorStaticMeshInstance Instance;
		Instance.ObjectId = Capture.ObjectId;
		Instance.MeshId = Capture.MeshId;
		Instance.LocalToWorld = Capture.LocalToWorld;
		Instance.Flags = Capture.InstanceFlags;
		Instances.push_back(Instance);
	}

	xr_vector<FEditorMaterialSlotSource> MaterialSlots;
	MaterialSlots.reserve(MaterialSlotsById.size());
	for (const auto& [Slot, Material] : MaterialSlotsById)
	{
		(void)Slot;
		MaterialSlots.push_back(Material);
	}
	std::ranges::sort(MaterialSlots, {},
		[](const FEditorMaterialSlotSource& Material)
		{
			return Material.MaterialSlot.Value;
		});

	xr_vector<FEditorStaticMeshUpload> ChangedMeshes;
	ChangedMeshes.reserve(ChangedPayloads.size());
	for (const FMeshPayload& Payload : ChangedPayloads)
		ChangedMeshes.push_back(Payload.MakeUpload());

	xr_vector<FEditorStaticMeshId> RemovedMeshes;
	for (const auto& [MeshId, Revision] : State.MeshRevisions)
	{
		(void)Revision;
		if (!CurrentMeshRevisions.contains(MeshId))
			RemovedMeshes.push_back({MeshId});
	}

	FEditorViewportSceneSnapshot Snapshot;
	Snapshot.Camera = BuildCamera();
	Snapshot.MaterialSlots = MaterialSlots;
	Snapshot.StaticMeshes = ChangedMeshes;
	Snapshot.RemovedStaticMeshes = RemovedMeshes;
	Snapshot.Instances = Instances;
	Snapshot.Lights = Lights;
	Snapshot.DebugLines = DebugLines;
	Snapshot.DebugTriangles = DebugTriangles;
	Snapshot.OverlayLines = OverlayLines;
	Snapshot.OverlayTriangles = OverlayTriangles;
	Snapshot.OverlayText = OverlayText;
	Snapshot.DebugDrawRevision = DebugDrawRevision;
	Snapshot.Revision = ++State.SceneRevision;
	if (!Renderer.SubmitViewportScene(ViewportId, Snapshot))
		return false;
	State.MeshRevisions = std::move(CurrentMeshRevisions);
	return true;
}

bool SubmitLegacySceneToEditorRenderer(const u32 ViewportId)
{
	return SubmitEditorSceneToEditorRenderer(ViewportId);
}
