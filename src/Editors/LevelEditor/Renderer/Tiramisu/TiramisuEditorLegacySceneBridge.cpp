#include "stdafx.h"
#include "TiramisuEditorLegacySceneBridge.h"
#include "TiramisuEditorNativeScene.h"

#include "../../../xrECore/Editor/EditorRenderBackend.h"
#include "../../../xrECore/Editor/EDetailModel.h"
#include "../../../xrECore/Editor/UI_ToolsCustom.h"
#include "../../../xrECore/Editor/du_box.h"
#include "../../../xrECore/Editor/du_sphere.h"
#include "../../Editor/Entry/Glow/glow.h"
#include "../../Editor/Entry/Group/GroupObject.h"
#include "../../Editor/Entry/Light/ELight.h"
#include "../../Editor/Entry/Particles/EParticlesObject.h"
#include "../../Editor/Entry/Portal/portal.h"
#include "../../Editor/Entry/Sector/sector.h"
#include "../../Editor/Entry/Shape/EShape.h"
#include "../../Editor/Entry/Sound/ESound_Source.h"
#include "../../Editor/Entry/Spawn/SpawnPoint.h"
#include "../../Editor/Entry/StaticObject/SceneObject.h"
#include "../../Editor/Entry/Terrain/Terrain.h"
#include "../../Editor/Entry/WayPoint/WayPoint.h"
#include "../../Editor/Scene/scene.h"
#include "../../Editor/Tools/AIMap/ESceneAIMapTools.h"
#include "../../Editor/Tools/Details/ESceneDOTools.h"
#include "../../Editor/Tools/Light/ESceneLightTools.h"
#include "../../Editor/Tools/Wallmark/ESceneWallmarkTools.h"

#include <LegacyDecalProjection.h>

#include <algorithm>
#include <bit>
#include <chrono>
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
constexpr size_t MaxLegacyShapeDebugLines = 1u << 18;
constexpr size_t MaxLegacyShapeDebugTriangles = 1u << 18;
constexpr size_t MaxLegacyAiNodeCount = 1u << 17;
constexpr size_t MaxLegacyDetailInstanceCount = 1u << 15;

struct FViewportBridgeState
{
	xr_hash_map<u64, u64> MeshRevisions;
	xr_vector<FEditorDebugLine> AiMapDebugLines;
	xr_vector<FEditorDebugTriangle> AiMapDebugTriangles;
	u64 AiMapDebugSourceRevision = 0;
	u64 SubmittedDebugDrawRevision = 0;
	u64 SceneRevision = 0;
	u32 SlowSubmissionLogCount = 0;
	u32 LegacyDetailModelCount = 0;
	u32 LegacyDetailInstanceCount = 0;
};

struct FLegacyDetailBridgeResult
{
	u32 ModelCount = 0;
	u32 InstanceCount = 0;
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

// Описывает один экземпляр legacy-сетки без привязки к конкретному типу
// объекта редактора. SceneObject нужен только для material overrides.
struct FLegacyMeshInstanceSource
{
	CCustomObject* Object = nullptr;
	CSceneObject* SceneObject = nullptr;
	CEditableMesh* Mesh = nullptr;
};

xr_hash_map<u32, FViewportBridgeState> ViewportStates;

[[nodiscard]] bool IsLegacyObjectVisibleForTiramisu(
	CCustomObject* Object
)
{
	for (CCustomObject* Current = Object;
		 Current;
		 Current = Current->GetOwner())
	{
		if (!Current->Visible())
		{
			return false;
		}
		ESceneToolBase* Tool = Scene->GetTool(Current->FClassID);
		if (Tool && !Tool->IsVisible())
		{
			return false;
		}
	}
	return Object != nullptr;
}

[[nodiscard]] bool IsLegacyObjectInsideEditorFrustum(
	CCustomObject& Object
)
{
	Fbox Box;
	if (!Object.GetBox(Box) || !Box.is_valid())
	{
		// Объект без bounds не скрывается: bridge остаётся консервативным.
		return true;
	}
	u32 PlaneMask = 0xff;
	return ::Render->ViewBase.testAABB(Box.data(), PlaneMask) != fcvNone;
}

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
	{
		HashBytes(Hash, Text, xr_strlen(Text));
	}
	const u8 Separator = 0;
	HashBytes(Hash, &Separator, sizeof(Separator));
}

[[nodiscard]] u64 MakePointerId(const void* Pointer)
{
	u64 Value = static_cast<u64>(
		reinterpret_cast<std::uintptr_t>(Pointer)
	);
	// Zero is reserved for invalid handles. Pointer identity is only used for
	// the lifetime of this editor process and never serialized into assets.
	return Value == 0 ? 1 : Value;
}

[[nodiscard]] FEditorMaterialSlotId MakeMaterialSlotId(const CSurface* Surface)
{
	if (!Surface)
	{
		return {FallbackMaterialSlot};
	}
	u64 Hash = FnvOffset;
	HashString(Hash, Surface->_ShaderName());
	HashString(Hash, Surface->_Texture());
	HashString(Hash, Surface->_Name());
	const bool TwoSided = Surface->m_Flags.is(CSurface::sf2Sided);
	HashBytes(Hash, &TwoSided, sizeof(TwoSided));
	if (Hash == 0)
	{
		Hash = 1;
	}
	return {Hash};
}

[[nodiscard]] FEditorMaterialSlotSource MakeMaterialSlotSource(
	const CSurface* Surface
)
{
	if (!Surface)
	{
		return {{FallbackMaterialSlot}, "default", "textures/default/default_white", "Unassigned", EEditorMaterialSlotFlags::None};
	}
	return {MakeMaterialSlotId(Surface), Surface->_ShaderName(), Surface->_Texture(), Surface->_Name(), Surface->m_Flags.is(CSurface::sf2Sided) ? EEditorMaterialSlotFlags::TwoSided : EEditorMaterialSlotFlags::None};
}

[[nodiscard]] FEditorMaterialSlotId MakeMaterialOverrideSlotId(
	const FEditorMaterialSlotId BaseMaterialSlot,
	const xr_string_view MaterialAsset
)
{
	u64 Hash = FnvOffset;
	HashString(Hash, "legacy-scene-material-override");
	HashBytes(Hash, &BaseMaterialSlot.Value, sizeof(BaseMaterialSlot.Value));
	HashBytes(Hash, MaterialAsset.data(), MaterialAsset.size());
	if (Hash == 0)
	{
		Hash = 1;
	}
	return {Hash};
}

[[nodiscard]] const CSurface* FindObjectSurface(
	const CSceneObject& SceneObject, const char* SurfaceName
)
{
	if (!SurfaceName)
	{
		return nullptr;
	}
	for (const CSurface* Surface : SceneObject.m_Surfaces)
	{
		if (Surface && xr_strcmp(Surface->_Name(), SurfaceName) == 0)
		{
			return Surface;
		}
	}
	return nullptr;
}

void AppendLegacyMaterialOverrides(CSceneObject& SceneObject, CEditableMesh& Mesh, FEditorStaticMeshInstance& Instance, xr_hash_map<u64, FEditorMaterialSlotSource>& MaterialSlotsById)
{
	for (const auto& [BaseSurface, Faces] : Mesh.GetSurfFaces())
	{
		(void)Faces;
		if (!BaseSurface)
		{
			continue;
		}
		const char* MaterialAsset =
			SceneObject.GetRenderMaterialAsset(BaseSurface->_Name());
		if (!MaterialAsset || !MaterialAsset[0])
		{
			continue;
		}

		const FEditorMaterialSlotId BaseSlot =
			MakeMaterialSlotId(BaseSurface);
		const FEditorMaterialSlotId OverrideSlot =
			MakeMaterialOverrideSlotId(BaseSlot, MaterialAsset);
		const CSurface* ObjectSurface =
			FindObjectSurface(SceneObject, BaseSurface->_Name());
		FEditorMaterialSlotSource Source =
			MakeMaterialSlotSource(ObjectSurface ? ObjectSurface : BaseSurface);
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

void SetEditorLightDirection(
	FEditorSceneLight& Light,
	const Fvector& SourceDirection
)
{
	Fvector Direction = SourceDirection;
	Direction.normalize_safe();
	if (Direction.square_magnitude() < EPS_S)
	{
		Direction.set(0.0f, -1.0f, 0.0f);
	}
	Fvector Up = {0.0f, 1.0f, 0.0f};
	if (std::abs(Up.dotproduct(Direction)) > 0.99f)
	{
		Up.set(0.0f, 0.0f, 1.0f);
	}
	Fvector Right;
	Right.crossproduct(Up, Direction).normalize_safe();
	Fvector CorrectedUp;
	CorrectedUp.crossproduct(Direction, Right).normalize_safe();
	Fmatrix Transform = Fidentity;
	Transform.i.set(Right);
	Transform.j.set(CorrectedUp);
	Transform.k.set(Direction);
	CopyMatrix(Transform, Light.LocalToWorld);
}

void AppendLegacyEditorLights(xr_vector<FEditorSceneLight>& Lights)
{
	Lights.clear();
	Lights.reserve(EditorViewportMaxLightCount);
	auto* LightTools = smart_cast<ESceneLightTool*>(
		Scene->GetOTool(OBJCLASS_LIGHT)
	);
	if (LightTools)
	{
		FEditorSceneLight Sun;
		Sun.ObjectId = {MakePointerId(LightTools)};
		Sun.Type = EEditorSceneLightType::Directional;
		Fvector Direction;
		const Fvector2& ShadowDirection =
			LightTools->GetSunShadowDirection();
		Direction.setHP(ShadowDirection.y, ShadowDirection.x);
		SetEditorLightDirection(Sun, Direction);
		Sun.Flags = EEditorSceneLightFlags::CastShadows;
		Lights.push_back(Sun);
	}

	for (CCustomObject* CustomObject : Scene->ListObj(OBJCLASS_LIGHT))
	{
		if (Lights.size() >= EditorViewportMaxLightCount)
		{
			break;
		}
		auto* LegacyLight = smart_cast<CLight*>(CustomObject);
		if (!LegacyLight ||
			!IsLegacyObjectVisibleForTiramisu(LegacyLight) ||
			!LegacyLight->m_UseInD3D)
		{
			continue;
		}
		FEditorSceneLight Light;
		Light.ObjectId = {MakePointerId(LegacyLight)};
		switch (LegacyLight->m_Type)
		{
			case ELight::ltPoint:
				Light.Type = EEditorSceneLightType::Point;
				break;
			case ELight::ltSpot:
				Light.Type = EEditorSceneLightType::Spot;
				break;
			case ELight::ltDirect:
				Light.Type = EEditorSceneLightType::Directional;
				break;
			default:
				continue;
		}
		CopyMatrix(LegacyLight->_Transform(), Light.LocalToWorld);
		Light.Color = {
			std::max(LegacyLight->m_Color.r, 0.0f),
			std::max(LegacyLight->m_Color.g, 0.0f),
			std::max(LegacyLight->m_Color.b, 0.0f)
		};
		Light.Intensity = std::max(LegacyLight->m_Brightness, 0.0f);
		Light.Range = std::max(LegacyLight->m_Range, EPS_S);
		if (Light.Type == EEditorSceneLightType::Spot)
		{
			Light.OuterConeAngleDegrees = std::clamp(
				rad2deg(LegacyLight->m_Cone) * 0.5f,
				0.1f,
				89.0f
			);
			Light.InnerConeAngleDegrees =
				Light.OuterConeAngleDegrees * 0.8f;
		}
		u32 Flags = LegacyLight->Selected()
			? static_cast<u32>(EEditorSceneLightFlags::Selected)
			: 0;
		if (LegacyLight->m_Flags.is(ELight::flCastShadow))
		{
			Flags |= static_cast<u32>(
				EEditorSceneLightFlags::CastShadows
			);
		}
		Light.Flags = static_cast<EEditorSceneLightFlags>(Flags);
		Lights.push_back(Light);
	}
}

[[nodiscard]] FEditorMaterialSlotId MakeLegacyDecalMaterialSlotId(
	const ESceneWallmarkTool::wm_slot& Slot
)
{
	u64 Hash = FnvOffset;
	HashString(Hash, "legacy-projective-decal");
	HashString(Hash, Slot.sh_name.c_str());
	HashString(Hash, Slot.tx_name.c_str());
	return {Hash == 0 ? 1 : Hash};
}

[[nodiscard]] bool BuildLegacyDecalTransform(
	const ESceneWallmarkTool::wallmark& Wallmark,
	xr_array<float, 16>& OutTransform
)
{
	xr_vector<Tiramisu::Scene::FLegacyDecalVertex> Vertices;
	Vertices.reserve(Wallmark.verts.size());
	for (const FVF::LIT& Vertex : Wallmark.verts)
	{
		Vertices.push_back({
			{Vertex.p.x, Vertex.p.y, Vertex.p.z},
			{Vertex.t.x, Vertex.t.y}
		});
	}
	const Tiramisu::Scene::FLegacyDecalProjectionResult Projection =
		Tiramisu::Scene::BuildLegacyDecalProjection(
			Vertices,
			Wallmark.w,
			Wallmark.h
		);
	if (!Projection.Succeeded())
	{
		return false;
	}
	OutTransform = Projection.LocalToWorld;
	return true;
}

void AppendLegacyEditorDecals(
	xr_hash_map<u64, FEditorMaterialSlotSource>& MaterialSlotsById,
	xr_vector<FEditorDecalInstance>& Decals
)
{
	auto* Tool = smart_cast<ESceneWallmarkTool*>(
		Scene->GetTool(OBJCLASS_WM)
	);
	if (!Tool || !Tool->IsVisible() ||
		!Tool->m_Flags.is(ESceneWallmarkTool::flDrawWallmark))
	{
		return;
	}
	for (const ESceneWallmarkTool::wm_slot* Slot : Tool->marks)
	{
		if (!Slot)
		{
			continue;
		}
		const FEditorMaterialSlotId MaterialSlot =
			MakeLegacyDecalMaterialSlotId(*Slot);
		MaterialSlotsById.insert_or_assign(
			MaterialSlot.Value,
			FEditorMaterialSlotSource{
				MaterialSlot,
				Slot->sh_name.c_str(),
				Slot->tx_name.c_str(),
				"Legacy Wallmark -> Decal",
				EEditorMaterialSlotFlags::None,
				{}
			}
		);
		for (const ESceneWallmarkTool::wallmark* Wallmark : Slot->items)
		{
			if (!Wallmark ||
				Wallmark->flags.test(
					ESceneWallmarkTool::wallmark::flRemoved
				))
			{
				continue;
			}
			FEditorDecalInstance Decal;
			Decal.ObjectId = {MakePointerId(Wallmark)};
			Decal.MaterialSlot = MaterialSlot;
			if (!BuildLegacyDecalTransform(
					*Wallmark,
					Decal.LocalToWorld
				))
			{
				continue;
			}
			Decal.SortOrder = static_cast<s32>(Decals.size());
			if (Wallmark->flags.test(
					ESceneWallmarkTool::wallmark::flSelected
				))
			{
				Decal.Flags = EEditorDecalInstanceFlags::Selected;
			}
			Decals.push_back(Decal);
			if (Decals.size() >= EditorViewportMaxDecalCount)
			{
				return;
			}
		}
	}
}

[[nodiscard]] FEditorDebugVertex MakeDebugVertex(const Fvector& Position, const u32 Color)
{
	FEditorDebugVertex Vertex;
	Vertex.Position = {Position.x, Position.y, Position.z};
	const float Scale = 1.0f / 255.0f;
	const u32 Alpha = Color >> 24u;
	Vertex.Color = {
		static_cast<float>((Color >> 16u) & 0xffu) * Scale,
		static_cast<float>((Color >> 8u) & 0xffu) * Scale,
		static_cast<float>(Color & 0xffu) * Scale,
		Alpha == 0 ? 1.0f : static_cast<float>(Alpha) * Scale
	};
	return Vertex;
}

void AppendDebugLine(xr_vector<FEditorDebugLine>& Lines, const Fvector& Start, const Fvector& End, const u32 Color)
{
	FEditorDebugLine& Line = Lines.emplace_back();
	Line.Vertices[0] = MakeDebugVertex(Start, Color);
	Line.Vertices[1] = MakeDebugVertex(End, Color);
}

void AppendWireBox(
	xr_vector<FEditorDebugLine>& Lines,
	Fbox Box,
	const u32 Color
)
{
	if (!Box.is_valid())
	{
		return;
	}
	const xr_array<Fvector, 8> Corners = {{
		{Box.min.x, Box.min.y, Box.min.z},
		{Box.max.x, Box.min.y, Box.min.z},
		{Box.max.x, Box.max.y, Box.min.z},
		{Box.min.x, Box.max.y, Box.min.z},
		{Box.min.x, Box.min.y, Box.max.z},
		{Box.max.x, Box.min.y, Box.max.z},
		{Box.max.x, Box.max.y, Box.max.z},
		{Box.min.x, Box.max.y, Box.max.z}
	}};
	constexpr xr_array<xr_array<size_t, 2>, 12> Edges = {{
		{{0, 1}}, {{1, 2}}, {{2, 3}}, {{3, 0}},
		{{4, 5}}, {{5, 6}}, {{6, 7}}, {{7, 4}},
		{{0, 4}}, {{1, 5}}, {{2, 6}}, {{3, 7}}
	}};
	for (const auto& Edge : Edges)
	{
		if (Lines.size() >= MaxLegacyShapeDebugLines)
		{
			break;
		}
		AppendDebugLine(
			Lines,
			Corners[Edge[0]],
			Corners[Edge[1]],
			Color
		);
	}
}

void AppendLegacyDetailSlotDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	u64& Revision
)
{
	auto* DetailManager = static_cast<EDetailManager*>(
		Scene->GetTool(OBJCLASS_DO)
	);
	if (!DetailManager || !DetailManager->IsVisible() ||
		!DetailManager->ShouldDrawSlotBoxes() ||
		!DetailManager->dtSlots)
	{
		return;
	}

	const size_t FirstLine = Lines.size();
	const Fvector& CameraPosition =
		UI->CurrentView().m_Camera.GetPosition();
	constexpr float SlotDrawDistanceSquared = 75.0f * 75.0f;
	for (u32 Z = 0; Z < DetailManager->dtH.size_z; ++Z)
	{
		for (u32 X = 0; X < DetailManager->dtH.size_x; ++X)
		{
			const size_t SlotIndex =
				static_cast<size_t>(Z) * DetailManager->dtH.size_x + X;
			DetailSlot& Slot = DetailManager->dtSlots[SlotIndex];
			Fvector Center = {
				(static_cast<s32>(X) - DetailManager->dtH.offs_x) *
					DETAIL_SLOT_SIZE + DETAIL_SLOT_SIZE_2,
				Slot.r_ybase() + Slot.r_yheight() * 0.5f,
				(static_cast<s32>(Z) - DetailManager->dtH.offs_z) *
					DETAIL_SLOT_SIZE + DETAIL_SLOT_SIZE_2
			};
			if (CameraPosition.distance_to_sqr(Center) >=
				SlotDrawDistanceSquared)
			{
				continue;
			}
			Fbox Box;
			Box.min.set(
				Center.x - DETAIL_SLOT_SIZE_2,
				Slot.r_ybase(),
				Center.z - DETAIL_SLOT_SIZE_2
			);
			Box.max.set(
				Center.x + DETAIL_SLOT_SIZE_2,
				Slot.r_ybase() + Slot.r_yheight(),
				Center.z + DETAIL_SLOT_SIZE_2
			);
			Box.shrink(0.05f);
			const bool Selected = SlotIndex <
				DetailManager->m_Selected.size() &&
				DetailManager->m_Selected[SlotIndex] != 0;
			AppendWireBox(
				Lines,
				Box,
				Selected ? 0xffffffffu : 0xff808080u
			);
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(
				Revision,
				Vertex.Position.data(),
				Vertex.Position.size() * sizeof(float)
			);
			HashBytes(
				Revision,
				Vertex.Color.data(),
				Vertex.Color.size() * sizeof(float)
			);
		}
	}
}

[[nodiscard]] FEditorMaterialSlotId MakeDetailBaseMaterialSlotId(
	const CCustom2DProjector& Projector,
	const bool Blended
)
{
	u64 Hash = FnvOffset;
	HashString(Hash, "legacy-detail-base-texture");
	HashString(Hash, Projector.name.c_str());
	HashBytes(Hash, &Blended, sizeof(Blended));
	return {Hash == 0 ? 1 : Hash};
}

void AppendLegacyDetailBaseTexture(
	FViewportBridgeState& State,
	xr_hash_map<u64, u64>& CurrentMeshRevisions,
	xr_hash_map<u64, FEditorMaterialSlotSource>& MaterialSlotsById,
	xr_vector<FMeshPayload>& ChangedPayloads,
	xr_vector<FEditorStaticMeshInstance>& Instances
)
{
	auto* DetailManager = static_cast<EDetailManager*>(
		Scene->GetTool(OBJCLASS_DO)
	);
	if (!DetailManager || !DetailManager->IsVisible() ||
		!DetailManager->ShouldDrawBaseTexture() ||
		!DetailManager->m_Base.Valid())
	{
		return;
	}
	const xr_span<const FVF::V> SourceMesh =
		DetailManager->m_Base.GetRenderMesh();
	if (SourceMesh.empty() || SourceMesh.size() % 3 != 0)
	{
		return;
	}

	const bool Blended = DetailManager->IsBaseTextureBlended();
	const FEditorMaterialSlotId MaterialSlot =
		MakeDetailBaseMaterialSlotId(DetailManager->m_Base, Blended);
	MaterialSlotsById.insert_or_assign(
		MaterialSlot.Value,
		FEditorMaterialSlotSource{
			MaterialSlot,
			Blended ? "editor\\particle_translucent" : "default",
			DetailManager->m_Base.GetName(),
			"Legacy Detail Base Texture",
			EEditorMaterialSlotFlags::TwoSided
		}
	);

	u64 Revision = FnvOffset;
	HashString(Revision, DetailManager->m_Base.GetName());
	HashBytes(Revision, &Blended, sizeof(Blended));
	for (const FVF::V& Vertex : SourceMesh)
	{
		HashBytes(Revision, &Vertex.p.x, sizeof(Vertex.p.x));
		HashBytes(Revision, &Vertex.p.y, sizeof(Vertex.p.y));
		HashBytes(Revision, &Vertex.p.z, sizeof(Vertex.p.z));
		HashBytes(Revision, &Vertex.t.x, sizeof(Vertex.t.x));
		HashBytes(Revision, &Vertex.t.y, sizeof(Vertex.t.y));
	}
	if (Revision == 0)
	{
		Revision = 1;
	}
	const FEditorStaticMeshId MeshId{
		MakePointerId(&DetailManager->m_Base)
	};
	CurrentMeshRevisions.insert_or_assign(MeshId.Value, Revision);
	const auto Existing = State.MeshRevisions.find(MeshId.Value);
	if (Existing == State.MeshRevisions.end() ||
		Existing->second != Revision)
	{
		FMeshPayload Payload;
		Payload.MeshId = MeshId;
		Payload.Revision = Revision;
		Payload.Vertices.reserve(SourceMesh.size());
		Payload.Indices.reserve(SourceMesh.size());
		for (size_t Index = 0; Index < SourceMesh.size(); Index += 3)
		{
			Fvector Normal;
			Normal.mknormal(
				SourceMesh[Index].p,
				SourceMesh[Index + 1].p,
				SourceMesh[Index + 2].p
			);
			for (size_t Corner = 0; Corner < 3; ++Corner)
			{
				const FVF::V& Source = SourceMesh[Index + Corner];
				FEditorStaticMeshVertex Vertex;
				Vertex.Position = {Source.p.x, Source.p.y, Source.p.z};
				Vertex.Normal = {Normal.x, Normal.y, Normal.z};
				Vertex.TexCoord = {Source.t.x, Source.t.y};
				Payload.Indices.push_back(
					static_cast<u32>(Payload.Vertices.size())
				);
				Payload.Vertices.push_back(Vertex);
			}
		}
		Payload.Sections.push_back({
			0,
			static_cast<u32>(Payload.Indices.size()),
			MaterialSlot
		});
		ChangedPayloads.push_back(std::move(Payload));
	}

	FEditorStaticMeshInstance Instance;
	Instance.ObjectId = {MakePointerId(&DetailManager->m_Base)};
	Instance.MeshId = MeshId;
	Instance.Flags = static_cast<EEditorSceneInstanceFlags>(
		static_cast<u32>(EEditorSceneInstanceFlags::TwoSided) |
		static_cast<u32>(EEditorSceneInstanceFlags::DepthBias)
	);
	Instances.push_back(std::move(Instance));
}

[[nodiscard]] u64 BuildDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles,
	xr_vector<FEditorOverlayLine>& OverlayLines,
	xr_vector<FEditorOverlayTriangle>& OverlayTriangles,
	xr_vector<FEditorOverlayText>& OverlayText,
	xr_vector<FEditorTransientMeshCapture>& TransientMeshes
)
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
	EndEditorDebugDrawCapture(CapturedLines, CapturedTriangles, CapturedOverlayLines, CapturedOverlayTriangles, CapturedOverlayText, CapturedTransientMeshes);
	if (CaptureActive)
	{
		Lines = std::move(CapturedLines);
		Triangles = std::move(CapturedTriangles);
		OverlayLines = std::move(CapturedOverlayLines);
		OverlayTriangles = std::move(CapturedOverlayTriangles);
		OverlayText = std::move(CapturedOverlayText);
		TransientMeshes = std::move(CapturedTransientMeshes);
	}
	if (Tools)
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
		{
			AppendDebugLine(Lines, Line.p[0], Line.p[1], Line.c);
		}
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
			{
				Triangle.Vertices[Corner] = MakeDebugVertex(Face.p[Corner], Face.c);
			}
		}
		for (const Fobb& Box : Source.m_OBB)
		{
			xr_array<Fvector, 8> Corners;
			for (u32 Index = 0; Index < Corners.size(); ++Index)
			{
				Fvector& Corner = Corners[Index];
				Corner = Box.m_translate;
				Corner.mad(Box.m_rotate.i, (Index & 1u) ? Box.m_halfsize.x : -Box.m_halfsize.x);
				Corner.mad(Box.m_rotate.j, (Index & 2u) ? Box.m_halfsize.y : -Box.m_halfsize.y);
				Corner.mad(Box.m_rotate.k, (Index & 4u) ? Box.m_halfsize.z : -Box.m_halfsize.z);
			}
			constexpr xr_array<xr_array<u32, 2>, 12> Edges = {{{{0, 1}}, {{2, 3}}, {{4, 5}}, {{6, 7}}, {{0, 2}}, {{1, 3}}, {{4, 6}}, {{5, 7}}, {{0, 4}}, {{1, 5}}, {{2, 6}}, {{3, 7}}}};
			for (const auto& Edge : Edges)
			{
				AppendDebugLine(Lines, Corners[Edge[0]], Corners[Edge[1]], 0xff00ff00u);
			}
		}
	}

	u64 Revision = FnvOffset;
	auto HashVertex = [&](const FEditorDebugVertex& Vertex)
	{
		HashBytes(Revision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
		HashBytes(Revision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
	};
	for (const FEditorDebugLine& Line : Lines)
	{
		for (const FEditorDebugVertex& Vertex : Line.Vertices)
		{
			HashVertex(Vertex);
		}
	}
	for (const FEditorDebugTriangle& Triangle : Triangles)
	{
		for (const FEditorDebugVertex& Vertex : Triangle.Vertices)
		{
			HashVertex(Vertex);
		}
	}
	auto HashOverlayVertex = [&](const FEditorOverlayVertex& Vertex)
	{
		HashBytes(Revision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
		HashBytes(Revision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
	};
	for (const FEditorOverlayLine& Line : OverlayLines)
	{
		for (const FEditorOverlayVertex& Vertex : Line.Vertices)
		{
			HashOverlayVertex(Vertex);
		}
	}
	for (const FEditorOverlayTriangle& Triangle : OverlayTriangles)
	{
		for (const FEditorOverlayVertex& Vertex : Triangle.Vertices)
		{
			HashOverlayVertex(Vertex);
		}
	}
	for (const FEditorOverlayText& Text : OverlayText)
	{
		HashBytes(Revision, Text.Position.data(), Text.Position.size() * sizeof(float));
		HashBytes(Revision, Text.Color.data(), Text.Color.size() * sizeof(float));
		HashBytes(Revision, Text.ShadowColor.data(), Text.ShadowColor.size() * sizeof(float));
		HashString(Revision, Text.Text.c_str());
	}
	return Revision == 0 ? 1 : Revision;
}

[[nodiscard]] bool ComputeMeshRevision(CEditableMesh& Mesh, u64& OutRevision)
{
	const Fvector* Vertices = Mesh.GetVertices();
	const st_Face* Faces = Mesh.GetFaces();
	const u32 VertexCount = Mesh.GetVCount();
	const u32 FaceCount = Mesh.GetFCount();
	if (!Vertices || !Faces || VertexCount == 0 || FaceCount == 0)
	{
		return false;
	}

	// Полное хеширование вершин и граней каждого кадра превращало Zaton в
	// несколько секунд CPU-работы. CEditableMesh публикует revision при
	// изменении geometry/UV/surface layout, поэтому проверка теперь имеет O(1).
	u64 Hash = FnvOffset;
	const u64 GeometryRevision = Mesh.GetRenderGeometryRevision();
	HashBytes(Hash, &GeometryRevision, sizeof(GeometryRevision));
	HashBytes(Hash, &VertexCount, sizeof(VertexCount));
	HashBytes(Hash, &FaceCount, sizeof(FaceCount));
	OutRevision = Hash == 0 ? 1 : Hash;
	return true;
}

void AppendFace(CEditableMesh& Mesh, const u32 FaceIndex, FMeshPayload& Payload)
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
			static_cast<u32>(Payload.Vertices.size())
		);
		Payload.Vertices.push_back(Vertex);
	}
}

[[nodiscard]] bool BuildMeshPayload(CEditableMesh& Mesh, const FEditorStaticMeshId MeshId, const u64 Revision, FMeshPayload& OutPayload)
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
			{
				continue;
			}
			const u32 FaceIndex =
				static_cast<u32>(SignedFaceIndex);
			if (FaceIndex >= FaceCount || AddedFaces[FaceIndex])
			{
				continue;
			}
			AppendFace(Mesh, FaceIndex, OutPayload);
			AddedFaces[FaceIndex] = true;
		}
		Section.IndexCount = static_cast<u32>(OutPayload.Indices.size()) -
							 Section.FirstIndex;
		if (Section.IndexCount != 0)
		{
			OutPayload.Sections.push_back(Section);
		}
	}

	FEditorStaticMeshSection Unassigned;
	Unassigned.FirstIndex = static_cast<u32>(OutPayload.Indices.size());
	for (u32 FaceIndex = 0; FaceIndex < FaceCount; ++FaceIndex)
	{
		if (!AddedFaces[FaceIndex])
		{
			AppendFace(Mesh, FaceIndex, OutPayload);
		}
	}
	Unassigned.IndexCount = static_cast<u32>(OutPayload.Indices.size()) -
							Unassigned.FirstIndex;
	if (Unassigned.IndexCount != 0)
	{
		OutPayload.Sections.push_back(Unassigned);
	}

	return !OutPayload.Vertices.empty() && !OutPayload.Indices.empty();
}

[[nodiscard]] bool BuildDetailTransform(
	const CDetail::SlotItem& Item,
	Fmatrix& OutTransform
)
{
	if (!std::isfinite(Item.scale) || Item.scale <= 0.0f ||
		!std::isfinite(Item.pos.x) || !std::isfinite(Item.pos.y) ||
		!std::isfinite(Item.pos.z))
	{
		return false;
	}
	const float QuaternionLengthSquared =
		Item.quat.x * Item.quat.x + Item.quat.y * Item.quat.y +
		Item.quat.z * Item.quat.z;
	if (!std::isfinite(QuaternionLengthSquared) ||
		QuaternionLengthSquared > 1.0f + EPS_L)
	{
		return false;
	}
	Fquaternion Rotation;
	Rotation.set(
		std::sqrt(std::max(0.0f, 1.0f - QuaternionLengthSquared)),
		Item.quat.x,
		Item.quat.y,
		Item.quat.z
	);
	Rotation.normalize();
	OutTransform.mk_xform(Rotation, Item.pos);
	OutTransform.i.mul(Item.scale);
	OutTransform.j.mul(Item.scale);
	OutTransform.k.mul(Item.scale);
	return true;
}

[[nodiscard]] bool ComputeDetailModelRevision(
	const EDetail& Detail,
	const FEditorMaterialSlotId MaterialSlot,
	const xr_vector<const CDetail::SlotItem*>& Items,
	u64& OutRevision
)
{
	if (!Detail.vertices || !Detail.indices || Items.empty() ||
		Detail.number_vertices == 0 || Detail.number_indices == 0 ||
		Detail.number_indices % 3 != 0)
	{
		return false;
	}

	u64 Hash = FnvOffset;
	HashBytes(Hash, &Detail.number_vertices, sizeof(Detail.number_vertices));
	HashBytes(Hash, &Detail.number_indices, sizeof(Detail.number_indices));
	HashBytes(Hash, &MaterialSlot.Value, sizeof(MaterialSlot.Value));
	for (u32 Index = 0; Index < Detail.number_vertices; ++Index)
	{
		const IRender_DetailModel::fvfVertexIn& Vertex =
			Detail.vertices[Index];
		HashBytes(Hash, &Vertex.P.x, sizeof(Vertex.P.x));
		HashBytes(Hash, &Vertex.P.y, sizeof(Vertex.P.y));
		HashBytes(Hash, &Vertex.P.z, sizeof(Vertex.P.z));
		HashBytes(Hash, &Vertex.u, sizeof(Vertex.u));
		HashBytes(Hash, &Vertex.v, sizeof(Vertex.v));
	}
	for (u32 Index = 0; Index < Detail.number_indices; ++Index)
	{
		if (Detail.indices[Index] >= Detail.number_vertices)
		{
			return false;
		}
		HashBytes(Hash, &Detail.indices[Index], sizeof(Detail.indices[Index]));
	}
	for (const CDetail::SlotItem* Item : Items)
	{
		HashBytes(Hash, &Item->quat.x, sizeof(Item->quat.x));
		HashBytes(Hash, &Item->quat.y, sizeof(Item->quat.y));
		HashBytes(Hash, &Item->quat.z, sizeof(Item->quat.z));
		HashBytes(Hash, &Item->scale, sizeof(Item->scale));
		HashBytes(Hash, &Item->pos.x, sizeof(Item->pos.x));
		HashBytes(Hash, &Item->pos.y, sizeof(Item->pos.y));
		HashBytes(Hash, &Item->pos.z, sizeof(Item->pos.z));
		HashBytes(Hash, &Item->c_hemi, sizeof(Item->c_hemi));
	}
	OutRevision = Hash == 0 ? 1 : Hash;
	return true;
}

[[nodiscard]] bool BuildDetailModelPayload(
	const EDetail& Detail,
	const FEditorStaticMeshId MeshId,
	const FEditorMaterialSlotId MaterialSlot,
	const xr_vector<const CDetail::SlotItem*>& Items,
	const u64 Revision,
	FMeshPayload& OutPayload
)
{
	OutPayload = {};
	OutPayload.MeshId = MeshId;
	OutPayload.Revision = Revision;
	const size_t VertexCount =
		static_cast<size_t>(Detail.number_indices) * Items.size();
	OutPayload.Vertices.reserve(VertexCount);
	OutPayload.Indices.reserve(VertexCount);

	for (const CDetail::SlotItem* Item : Items)
	{
		Fmatrix Transform;
		if (!Item || !BuildDetailTransform(*Item, Transform))
		{
			continue;
		}
		for (u32 Index = 0; Index < Detail.number_indices; Index += 3)
		{
			const u16 AIndex = Detail.indices[Index];
			const u16 BIndex = Detail.indices[Index + 1];
			const u16 CIndex = Detail.indices[Index + 2];
			if (AIndex >= Detail.number_vertices ||
				BIndex >= Detail.number_vertices ||
				CIndex >= Detail.number_vertices)
			{
				return false;
			}
			const IRender_DetailModel::fvfVertexIn* Triangle[] = {
				&Detail.vertices[AIndex],
				&Detail.vertices[BIndex],
				&Detail.vertices[CIndex]
			};
			Fvector LocalNormal;
			LocalNormal.mknormal(
				Triangle[0]->P,
				Triangle[1]->P,
				Triangle[2]->P
			);
			Fvector WorldNormal;
			Transform.transform_dir(WorldNormal, LocalNormal);
			WorldNormal.normalize_safe();
			for (const IRender_DetailModel::fvfVertexIn* Source : Triangle)
			{
				Fvector WorldPosition;
				Transform.transform_tiny(WorldPosition, Source->P);
				FEditorStaticMeshVertex Vertex;
				Vertex.Position = {
					WorldPosition.x,
					WorldPosition.y,
					WorldPosition.z
				};
				Vertex.Normal = {
					WorldNormal.x,
					WorldNormal.y,
					WorldNormal.z
				};
				Vertex.TexCoord = {Source->u, Source->v};
				OutPayload.Indices.push_back(
					static_cast<u32>(OutPayload.Vertices.size())
				);
				OutPayload.Vertices.push_back(Vertex);
			}
		}
	}
	OutPayload.Sections.push_back({
		0,
		static_cast<u32>(OutPayload.Indices.size()),
		MaterialSlot
	});
	return !OutPayload.Vertices.empty();
}

[[nodiscard]] FLegacyDetailBridgeResult AppendLegacyDetailObjects(
	FViewportBridgeState& State,
	xr_hash_map<u64, u64>& CurrentMeshRevisions,
	xr_hash_map<u64, FEditorMaterialSlotSource>& MaterialSlotsById,
	xr_vector<FMeshPayload>& ChangedPayloads,
	xr_vector<FEditorStaticMeshInstance>& Instances
)
{
	FLegacyDetailBridgeResult Result;
	auto* DetailManager = static_cast<EDetailManager*>(
		Scene->GetTool(OBJCLASS_DO)
	);
	if (!DetailManager || !DetailManager->IsVisible() ||
		!DetailManager->HasTiramisuRenderData() ||
		!DetailManager->task_finished.load())
	{
		return Result;
	}

	DetailManager->cache_Update(
		UI->CurrentView().m_Camera.GetPosition()
	);
	xr_vector<xr_vector<const CDetail::SlotItem*>> VisibleItems(
		DetailManager->objects.size()
	);
	size_t VisibleItemCount = 0;
	for (const CDetailManager::Slot& Slot : DetailManager->cache_pool)
	{
		if (Slot.empty || Slot.type != CDetailManager::stReady)
		{
			continue;
		}
		for (const CDetailManager::SlotPart& Part : Slot.G)
		{
			if (Part.id >= VisibleItems.size())
			{
				continue;
			}
			for (const auto& Variant : Part.items)
			{
				for (const CDetail::SlotItem* Item : Variant)
				{
					Fmatrix Transform;
					if (!Item || !BuildDetailTransform(*Item, Transform))
					{
						continue;
					}
					VisibleItems[Part.id].push_back(Item);
					if (++VisibleItemCount >= MaxLegacyDetailInstanceCount)
					{
						break;
					}
				}
				if (VisibleItemCount >= MaxLegacyDetailInstanceCount)
				{
					break;
				}
			}
			if (VisibleItemCount >= MaxLegacyDetailInstanceCount)
			{
				break;
			}
		}
		if (VisibleItemCount >= MaxLegacyDetailInstanceCount)
		{
			break;
		}
	}

	for (size_t ObjectIndex = 0;
		 ObjectIndex < DetailManager->objects.size();
		 ++ObjectIndex)
	{
		const xr_vector<const CDetail::SlotItem*>& Items =
			VisibleItems[ObjectIndex];
		if (Items.empty())
		{
			continue;
		}
		auto* Detail = static_cast<EDetail*>(
			DetailManager->objects[ObjectIndex]
		);
		if (!Detail || !Detail->m_pRefs ||
			Detail->m_pRefs->SurfaceCount() == 0)
		{
			continue;
		}
		CSurface* Surface = *Detail->m_pRefs->FirstSurface();
		if (!Surface)
		{
			continue;
		}
		FEditorMaterialSlotSource Material =
			MakeMaterialSlotSource(Surface);
		Material.Flags = EEditorMaterialSlotFlags::TwoSided;
		MaterialSlotsById.insert_or_assign(
			Material.MaterialSlot.Value,
			Material
		);

		const FEditorStaticMeshId MeshId{MakePointerId(Detail)};
		u64 Revision = 0;
		if (!ComputeDetailModelRevision(
				*Detail,
				Material.MaterialSlot,
				Items,
				Revision
			))
		{
			continue;
		}
		++Result.ModelCount;
		Result.InstanceCount += static_cast<u32>(Items.size());
		CurrentMeshRevisions.insert_or_assign(MeshId.Value, Revision);
		FEditorStaticMeshInstance Instance;
		Instance.ObjectId = {MakePointerId(Detail)};
		Instance.MeshId = MeshId;
		Instance.Flags = EEditorSceneInstanceFlags::TwoSided;
		Instances.push_back(std::move(Instance));
		const auto Existing = State.MeshRevisions.find(MeshId.Value);
		if (Existing != State.MeshRevisions.end() &&
			Existing->second == Revision)
		{
			continue;
		}
		FMeshPayload Payload;
		if (BuildDetailModelPayload(
				*Detail,
				MeshId,
				Material.MaterialSlot,
				Items,
				Revision,
				Payload
			))
		{
			ChangedPayloads.push_back(std::move(Payload));
		}
		else
		{
			Instances.pop_back();
			--Result.ModelCount;
			Result.InstanceCount -= static_cast<u32>(Items.size());
			CurrentMeshRevisions.erase(MeshId.Value);
		}
	}
	return Result;
}

[[nodiscard]] FEditorMaterialSlotId MakeNativeMaterialSlotId(
	const Tiramisu::Scene::FStaticMeshAsset& Mesh,
	const u32 SlotIndex
)
{
	xr_string Identity = Mesh.Id;
	Identity += ":material-slot:";
	Identity += std::to_string(SlotIndex);
	return {Tiramisu::Scene::StableSceneIdHash(Identity)};
}

[[nodiscard]] FEditorMaterialSlotId MakeNativeMaterialOverrideId(
	const Tiramisu::Scene::FStaticMeshComponent& Component,
	const Tiramisu::Scene::FStaticMeshMaterialOverride& Override
)
{
	xr_string Identity = Component.Id;
	Identity += ":material-override:";
	Identity += std::to_string(Override.MaterialSlot);
	Identity += ':';
	Identity += Override.Material;
	return {Tiramisu::Scene::StableSceneIdHash(Identity)};
}

[[nodiscard]] FEditorMaterialSlotId MakeNativeDecalMaterialSlotId(
	const Tiramisu::Scene::FDecalComponent& Decal
)
{
	xr_string Identity = Decal.Id;
	Identity += ":decal-material:";
	Identity += Decal.Material;
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
	xr_vector<FEditorSceneLight>& Lights,
	xr_vector<FEditorDecalInstance>& Decals
)
{
	for (const auto& [Reference, Mesh] : NativeScene.StaticMeshes)
	{
		(void)Reference;
		const FEditorStaticMeshId MeshId{
			Tiramisu::Scene::StableSceneIdHash(Mesh.Id)
		};
		const u64 Revision =
			Tiramisu::Scene::CalculateStaticMeshRevision(Mesh);
		if (!CurrentMeshRevisions.emplace(MeshId.Value, Revision).second)
		{
			return false;
		}

		for (u32 SlotIndex = 0;
			 SlotIndex < Mesh.MaterialSlots.size();
			 ++SlotIndex)
		{
			const Tiramisu::Scene::FStaticMeshMaterialSlot& Slot =
				Mesh.MaterialSlots[SlotIndex];
			const FEditorMaterialSlotId SlotId =
				MakeNativeMaterialSlotId(Mesh, SlotIndex);
			const EEditorMaterialSlotFlags Flags = Slot.TwoSided
													   ? EEditorMaterialSlotFlags::TwoSided
													   : EEditorMaterialSlotFlags::None;
			MaterialSlotsById.insert_or_assign(SlotId.Value, FEditorMaterialSlotSource{SlotId, {}, {}, Slot.Name, Flags, Slot.Material});
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
			{
				return false;
			}
			Payload.Sections.push_back({Source.FirstIndex, Source.IndexCount, MakeNativeMaterialSlotId(Mesh, Source.MaterialSlot)});
		}
		ChangedPayloads.push_back(std::move(Payload));
	}

	Instances.reserve(NativeScene.Scene.StaticMeshComponents.size());
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 NativeScene.Scene.StaticMeshComponents)
	{
		if (!Component.Visible)
		{
			continue;
		}
		const auto Mesh = NativeScene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == NativeScene.StaticMeshes.end())
		{
			return false;
		}
		FEditorStaticMeshInstance Instance;
		Instance.ObjectId = {
			Tiramisu::Scene::StableSceneIdHash(Component.Id)
		};
		Instance.MeshId = {
			Tiramisu::Scene::StableSceneIdHash(Mesh->second.Id)
		};
		Instance.LocalToWorld = Component.LocalToWorld;
		if (Document.IsComponentSelected(Component.Id))
		{
			Instance.Flags = EEditorSceneInstanceFlags::Selected;
		}
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
					Mesh->second, Override.MaterialSlot
				);
			const FEditorMaterialSlotId OverrideSlot =
				MakeNativeMaterialOverrideId(Component, Override);
			const Tiramisu::Scene::FStaticMeshMaterialSlot& Base =
				Mesh->second.MaterialSlots[Override.MaterialSlot];
			const EEditorMaterialSlotFlags Flags = Override.TwoSided
													   ? EEditorMaterialSlotFlags::TwoSided
													   : EEditorMaterialSlotFlags::None;
			MaterialSlotsById.insert_or_assign(OverrideSlot.Value, FEditorMaterialSlotSource{OverrideSlot, {}, {}, Base.Name, Flags, Override.Material});
			Instance.MaterialOverrides.push_back(
				{BaseSlot, OverrideSlot}
			);
		}
		Instances.push_back(Instance);
	}
	Lights.reserve(NativeScene.Scene.LightComponents.size());
	for (const Tiramisu::Scene::FLightComponent& Source :
		 NativeScene.Scene.LightComponents)
	{
		if (!Source.Visible)
		{
			continue;
		}
		FEditorSceneLight Light;
		Light.ObjectId = {
			Tiramisu::Scene::StableSceneIdHash(Source.Id)
		};
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
							  EEditorSceneLightFlags::CastShadows
						  )
						: 0u;
		if (Document.IsComponentSelected(Source.Id))
		{
			Flags |= static_cast<u32>(
				EEditorSceneLightFlags::Selected
			);
		}
		Light.Flags = static_cast<EEditorSceneLightFlags>(Flags);
		Lights.push_back(Light);
	}
	Decals.reserve(NativeScene.Scene.DecalComponents.size());
	for (const Tiramisu::Scene::FDecalComponent& Source :
		 NativeScene.Scene.DecalComponents)
	{
		if (!Source.Visible)
		{
			continue;
		}
		const FEditorMaterialSlotId MaterialSlot =
			MakeNativeDecalMaterialSlotId(Source);
		MaterialSlotsById.insert_or_assign(
			MaterialSlot.Value,
			FEditorMaterialSlotSource{
				MaterialSlot,
				{},
				{},
				Source.Name,
				EEditorMaterialSlotFlags::None,
				Source.Material
			}
		);
		FEditorDecalInstance Decal;
		Decal.ObjectId = {
			Tiramisu::Scene::StableSceneIdHash(Source.Id)
		};
		Decal.MaterialSlot = MaterialSlot;
		Decal.LocalToWorld = Source.LocalToWorld;
		Decal.SortOrder = Source.SortOrder;
		if (Document.IsComponentSelected(Source.Id))
		{
			Decal.Flags = EEditorDecalInstanceFlags::Selected;
		}
		Decals.push_back(Decal);
	}
	std::ranges::stable_sort(
		Decals,
		{},
		[](const FEditorDecalInstance& Decal)
		{
			return Decal.SortOrder;
		}
	);
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
	u64& Revision
)
{
	const size_t FirstLine = Lines.size();
	for (const FEditorSceneLight& Light : Lights)
	{
		const xr_array<float, 3> Position = {
			Light.LocalToWorld[12],
			Light.LocalToWorld[13],
			Light.LocalToWorld[14]
		};
		xr_array<float, 4> Color = {
			std::clamp(Light.Color[0], 0.1f, 1.0f),
			std::clamp(Light.Color[1], 0.1f, 1.0f),
			std::clamp(Light.Color[2], 0.1f, 1.0f),
			1.0f
		};
		if ((static_cast<u32>(Light.Flags) &
			 static_cast<u32>(
				 EEditorSceneLightFlags::Selected
			 )) != 0)
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
				Light.LocalToWorld[10]
			};
			const float Length = std::sqrt(
				Direction[0] * Direction[0] +
				Direction[1] * Direction[1] +
				Direction[2] * Direction[2]
			);
			if (Length > 1.0e-6f)
			{
				const float Scale =
					Light.Type == EEditorSceneLightType::Spot
						? std::min(Light.Range, 2.0f)
						: 2.0f;
				xr_array<float, 3> End = Position;
				for (size_t Axis = 0; Axis < 3; ++Axis)
				{
					End[Axis] += Direction[Axis] / Length * Scale;
				}
				AddLine(Position, End);
			}
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(Revision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
			HashBytes(Revision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
		}
	}
	if (Revision == 0)
	{
		Revision = 1;
	}
}

void AppendLegacyShapeObjectDebugDraw(
	CEditShape& ShapeObject,
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles
)
{
	const u32 EdgeColor = ShapeObject.m_DrawEdgeColor;
	const u32 FillColor = ShapeObject.Selected()
		? subst_alpha(
			ShapeObject.m_DrawTranspColor,
			std::min(
				color_get_A(ShapeObject.m_DrawTranspColor) * 2,
				255u
			)
		)
		: ShapeObject.m_DrawTranspColor;
	for (const auto& Shape : ShapeObject.GetShapes())
	{
		Fmatrix LocalToWorld;
		const Fvector* LineVertices = nullptr;
		const Fvector* FaceVertices = nullptr;
		const WORD* SourceLines = nullptr;
		const WORD* SourceFaces = nullptr;
		size_t LineCount = 0;
		size_t FaceCount = 0;
		if (Shape.type == 0)
		{
			LocalToWorld.scale(
				Shape.data.sphere.R,
				Shape.data.sphere.R,
				Shape.data.sphere.R
			);
			LocalToWorld.translate_over(Shape.data.sphere.P);
			LocalToWorld.mulA_43(ShapeObject._Transform());
			LineVertices = du_sphere_verticesl;
			FaceVertices = du_sphere_vertices;
			SourceLines = du_sphere_lines;
			SourceFaces = du_sphere_faces;
			LineCount = DU_SPHERE_NUMLINES;
			FaceCount = DU_SPHERE_NUMFACES;
		}
		else if (Shape.type == 1)
		{
			LocalToWorld = Shape.data.box;
			LocalToWorld.mulA_43(ShapeObject._Transform());
			LineVertices = du_box_vertices;
			FaceVertices = du_box_vertices;
			SourceLines = du_box_lines;
			SourceFaces = du_box_faces;
			LineCount = DU_BOX_NUMLINES;
			FaceCount = DU_BOX_NUMFACES;
		}
		else
		{
			continue;
		}
		for (size_t LineIndex = 0;
			 LineIndex < LineCount &&
			 Lines.size() < MaxLegacyShapeDebugLines;
			 ++LineIndex)
		{
			Fvector Start;
			Fvector End;
			LocalToWorld.transform_tiny(
				Start,
				LineVertices[SourceLines[LineIndex * 2]]
			);
			LocalToWorld.transform_tiny(
				End,
				LineVertices[SourceLines[LineIndex * 2 + 1]]
			);
			AppendDebugLine(Lines, Start, End, EdgeColor);
		}
		for (size_t FaceIndex = 0;
			 FaceIndex < FaceCount &&
			 Triangles.size() < MaxLegacyShapeDebugTriangles;
			 ++FaceIndex)
		{
			FEditorDebugTriangle& Triangle = Triangles.emplace_back();
			for (size_t Corner = 0; Corner < 3; ++Corner)
			{
				Fvector Position;
				LocalToWorld.transform_tiny(
					Position,
					FaceVertices[SourceFaces[FaceIndex * 3 + Corner]]
				);
				Triangle.Vertices[Corner] =
					MakeDebugVertex(Position, FillColor);
			}
		}
	}
}

void AppendLegacyShapeDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles,
	u64& Revision
)
{
	const size_t FirstLine = Lines.size();
	const size_t FirstTriangle = Triangles.size();
	const xr_array<ObjClassID, 4> ShapeClasses = {
		OBJCLASS_SHAPE,
		OBJCLASS_SOUND_ENV,
		OBJCLASS_PUDDLES,
		OBJCLASS_FOG_VOL
	};
	for (const ObjClassID ShapeClass : ShapeClasses)
	{
		for (CCustomObject* CustomObject : Scene->ListObj(ShapeClass))
		{
			auto* ShapeObject = smart_cast<CEditShape*>(CustomObject);
			if (!ShapeObject ||
				!IsLegacyObjectVisibleForTiramisu(ShapeObject) ||
				!IsLegacyObjectInsideEditorFrustum(*ShapeObject))
			{
				continue;
			}
			AppendLegacyShapeObjectDebugDraw(
				*ShapeObject,
				Lines,
				Triangles
			);
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(
				Revision,
				Vertex.Position.data(),
				Vertex.Position.size() * sizeof(float)
			);
			HashBytes(
				Revision,
				Vertex.Color.data(),
				Vertex.Color.size() * sizeof(float)
			);
		}
	}
	for (size_t Index = FirstTriangle;
		 Index < Triangles.size();
		 ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Triangles[Index].Vertices)
		{
			HashBytes(
				Revision,
				Vertex.Position.data(),
				Vertex.Position.size() * sizeof(float)
			);
			HashBytes(
				Revision,
				Vertex.Color.data(),
				Vertex.Color.size() * sizeof(float)
			);
		}
	}
}

void AppendWireSphere(
	xr_vector<FEditorDebugLine>& Lines,
	const Fvector& Position,
	const float Radius,
	const u32 Color
)
{
	if (!std::isfinite(Radius) || Radius <= 0.0f)
	{
		return;
	}
	Fmatrix LocalToWorld;
	LocalToWorld.scale(Radius, Radius, Radius);
	LocalToWorld.translate_over(Position);
	for (size_t LineIndex = 0;
		 LineIndex < DU_SPHERE_NUMLINES &&
		 Lines.size() < MaxLegacyShapeDebugLines;
		 ++LineIndex)
	{
		Fvector Start;
		Fvector End;
		LocalToWorld.transform_tiny(
			Start,
			du_sphere_verticesl[du_sphere_lines[LineIndex * 2]]
		);
		LocalToWorld.transform_tiny(
			End,
			du_sphere_verticesl[du_sphere_lines[LineIndex * 2 + 1]]
		);
		AppendDebugLine(Lines, Start, End, Color);
	}
}

void AppendLegacySoundSourceDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	u64& Revision
)
{
	const size_t FirstLine = Lines.size();
	for (CCustomObject* CustomObject :
		 Scene->ListObj(OBJCLASS_SOUND_SRC))
	{
		auto* SoundSource = smart_cast<ESoundSource*>(CustomObject);
		if (!SoundSource ||
			!IsLegacyObjectVisibleForTiramisu(SoundSource) ||
			!IsLegacyObjectInsideEditorFrustum(*SoundSource))
		{
			continue;
		}
		const u32 PrimaryColor = SoundSource->Locked()
			? 0x00ff0000u
			: SoundSource->Selected()
				? 0x00a0a0f0u
				: 0x000000ffu;
		const u32 SecondaryColor = SoundSource->Locked()
			? 0x00ff0000u
			: SoundSource->Selected()
				? 0x00ffffffu
				: 0x000000ffu;
		if (SoundSource->Selected())
		{
			AppendWireSphere(
				Lines,
				SoundSource->GetPosition(),
				SoundSource->GetMaxDistance(),
				SecondaryColor
			);
			AppendWireSphere(
				Lines,
				SoundSource->GetPosition(),
				SoundSource->GetMinDistance(),
				PrimaryColor
			);
		}
		else
		{
			AppendWireSphere(
				Lines,
				SoundSource->GetPosition(),
				0.25f,
				SecondaryColor
			);
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(
				Revision,
				Vertex.Position.data(),
				Vertex.Position.size() * sizeof(float)
			);
			HashBytes(
				Revision,
				Vertex.Color.data(),
				Vertex.Color.size() * sizeof(float)
			);
		}
	}
}

void AppendLegacyPortalDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles,
	u64& Revision
)
{
	const size_t FirstLine = Lines.size();
	const size_t FirstTriangle = Triangles.size();
	for (CCustomObject* CustomObject : Scene->ListObj(OBJCLASS_PORTAL))
	{
		auto* Portal = smart_cast<CPortal*>(CustomObject);
		if (!Portal || !IsLegacyObjectVisibleForTiramisu(Portal) ||
			!IsLegacyObjectInsideEditorFrustum(*Portal))
		{
			continue;
		}
		const FvectorVec& Vertices = Portal->Vertices();
		if (Vertices.size() < 3)
		{
			continue;
		}
		Fvector Center = {};
		for (const Fvector& Vertex : Vertices)
		{
			Center.add(Vertex);
		}
		Center.div(static_cast<float>(Vertices.size()));
		const u32 EdgeColor = Portal->Selected()
			? 0xffffffffu
			: 0xff808080u;
		const u32 FillColor = Portal->Selected()
			? 0x80ff9a30u
			: 0x40707070u;
		for (size_t Index = 0;
			 Index < Vertices.size() &&
			 Lines.size() < MaxLegacyShapeDebugLines;
			 ++Index)
		{
			AppendDebugLine(
				Lines,
				Vertices[Index],
				Vertices[(Index + 1) % Vertices.size()],
				EdgeColor
			);
		}
		for (size_t Index = 0;
			 Index < Vertices.size() &&
			 Triangles.size() + 1 < MaxLegacyShapeDebugTriangles;
			 ++Index)
		{
			const Fvector& First = Vertices[Index];
			const Fvector& Second =
				Vertices[(Index + 1) % Vertices.size()];
			FEditorDebugTriangle& Front = Triangles.emplace_back();
			Front.Vertices[0] = MakeDebugVertex(Center, FillColor);
			Front.Vertices[1] = MakeDebugVertex(First, FillColor);
			Front.Vertices[2] = MakeDebugVertex(Second, FillColor);
			FEditorDebugTriangle& Back = Triangles.emplace_back();
			Back.Vertices[0] = MakeDebugVertex(Center, FillColor);
			Back.Vertices[1] = MakeDebugVertex(Second, FillColor);
			Back.Vertices[2] = MakeDebugVertex(First, FillColor);
		}
		Fvector Normal;
		Normal.mknormal(Vertices[0], Vertices[1], Vertices[2]);
		if (Normal.square_magnitude() > EPS_S &&
			Lines.size() < MaxLegacyShapeDebugLines)
		{
			Normal.normalize_safe().mul(0.75f).add(Center);
			AppendDebugLine(Lines, Center, Normal, 0xffffff00u);
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(Revision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
			HashBytes(Revision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
		}
	}
	for (size_t Index = FirstTriangle; Index < Triangles.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Triangles[Index].Vertices)
		{
			HashBytes(Revision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
			HashBytes(Revision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
		}
	}
}

void AppendLegacyGlowPayload(
	FViewportBridgeState& State,
	xr_hash_map<u64, u64>& CurrentMeshRevisions,
	xr_hash_map<u64, FEditorMaterialSlotSource>& MaterialSlotsById,
	xr_vector<FMeshPayload>& ChangedPayloads,
	xr_vector<FEditorStaticMeshInstance>& Instances,
	xr_vector<FEditorDebugLine>& Lines,
	u64& DebugDrawRevision
)
{
	const size_t FirstLine = Lines.size();
	for (CCustomObject* CustomObject : Scene->ListObj(OBJCLASS_GLOW))
	{
		auto* Glow = smart_cast<CGlow*>(CustomObject);
		if (!Glow || !IsLegacyObjectVisibleForTiramisu(Glow) || !EDevice ||
			!IsLegacyObjectInsideEditorFrustum(*Glow) ||
			!std::isfinite(Glow->m_fRadius) || Glow->m_fRadius <= 0.0f)
		{
			continue;
		}
		const char* TextureName = Glow->m_TexName.size()
			? *Glow->m_TexName
			: "textures/default/default_white";
		u64 MaterialHash = FnvOffset;
		HashString(MaterialHash, "editor\\glow_sprite");
		HashString(MaterialHash, TextureName);
		if (MaterialHash == 0)
		{
			MaterialHash = 1;
		}
		const FEditorMaterialSlotId MaterialSlot = {MaterialHash};
		MaterialSlotsById.insert_or_assign(
			MaterialSlot.Value,
			FEditorMaterialSlotSource{
				MaterialSlot,
				"editor\\glow_sprite",
				TextureName,
				"Legacy glow",
				EEditorMaterialSlotFlags::TwoSided
			}
		);

		u64 MeshHash = FnvOffset;
		HashString(MeshHash, "legacy-editor-glow-billboard-v2");
		HashBytes(MeshHash, &MaterialHash, sizeof(MaterialHash));
		if (MeshHash == 0)
		{
			MeshHash = 1;
		}
		const FEditorStaticMeshId MeshId = {MeshHash};
		const auto [RevisionIt, Inserted] =
			CurrentMeshRevisions.emplace(MeshId.Value, MeshHash);
		if ((Inserted &&
			 (State.MeshRevisions.find(MeshId.Value) ==
				  State.MeshRevisions.end() ||
			  State.MeshRevisions.at(MeshId.Value) != MeshHash)))
		{
			FMeshPayload Payload;
			Payload.MeshId = MeshId;
			Payload.Revision = MeshHash;
			Payload.Vertices.resize(4);
			Payload.Vertices[0].Position = {-1.0f, -1.0f, 0.0f};
			Payload.Vertices[1].Position = {-1.0f, 1.0f, 0.0f};
			Payload.Vertices[2].Position = {1.0f, -1.0f, 0.0f};
			Payload.Vertices[3].Position = {1.0f, 1.0f, 0.0f};
			Payload.Vertices[0].TexCoord = {0.0f, 1.0f};
			Payload.Vertices[1].TexCoord = {0.0f, 0.0f};
			Payload.Vertices[2].TexCoord = {1.0f, 1.0f};
			Payload.Vertices[3].TexCoord = {1.0f, 0.0f};
			for (FEditorStaticMeshVertex& Vertex : Payload.Vertices)
			{
				Vertex.Normal = {0.0f, 0.0f, 1.0f};
				Vertex.Tangent = {1.0f, 0.0f, 0.0f, 1.0f};
			}
			Payload.Indices = {0, 1, 2, 3, 2, 1};
			Payload.Sections.push_back({0, 6, MaterialSlot});
			ChangedPayloads.push_back(std::move(Payload));
		}
		else if (!Inserted && RevisionIt->second != MeshHash)
		{
			continue;
		}

		FVF::TL Projected;
		Projected.transform(Glow->GetPosition(), EDevice->mFullTransform);
		const float ProjectionScale = std::abs(EDevice->mProject._11);
		if (!std::isfinite(Projected.p.w) || Projected.p.w <= 0.0f ||
			ProjectionScale <= EPS_S)
		{
			continue;
		}
		const float HalfSize = Glow->m_Flags.is(CGlow::gfFixedSize)
			? Glow->m_fRadius * Projected.p.w / ProjectionScale
			: 2.0f * Glow->m_fRadius / ProjectionScale;
		if (!std::isfinite(HalfSize) || HalfSize <= 0.0f)
		{
			continue;
		}
		Fmatrix LocalToWorld;
		LocalToWorld.set(
			Fvector(EDevice->vCameraRight).mul(HalfSize),
			Fvector(EDevice->vCameraTop).mul(HalfSize),
			Fvector(EDevice->vCameraDirection).invert(),
			Glow->GetPosition()
		);
		FEditorStaticMeshInstance Instance;
		Instance.ObjectId = {MakePointerId(Glow)};
		Instance.MeshId = MeshId;
		CopyMatrix(LocalToWorld, Instance.LocalToWorld);
		u32 Flags = static_cast<u32>(
			EEditorSceneInstanceFlags::TwoSided
		);
		if (Glow->Selected())
		{
			Flags |= static_cast<u32>(
				EEditorSceneInstanceFlags::Selected
			);
		}
		Instance.Flags = static_cast<EEditorSceneInstanceFlags>(Flags);
		Instances.push_back(Instance);

		if (Glow->Selected())
		{
			const Fvector Position = Glow->GetPosition();
			const float Radius = Glow->m_fRadius;
			const xr_array<Fvector, 8> Corners = {{
				{Position.x - Radius, Position.y - Radius, Position.z - Radius},
				{Position.x + Radius, Position.y - Radius, Position.z - Radius},
				{Position.x + Radius, Position.y + Radius, Position.z - Radius},
				{Position.x - Radius, Position.y + Radius, Position.z - Radius},
				{Position.x - Radius, Position.y - Radius, Position.z + Radius},
				{Position.x + Radius, Position.y - Radius, Position.z + Radius},
				{Position.x + Radius, Position.y + Radius, Position.z + Radius},
				{Position.x - Radius, Position.y + Radius, Position.z + Radius}
			}};
			constexpr xr_array<xr_array<size_t, 2>, 12> Edges = {{
				{{0, 1}}, {{1, 2}}, {{2, 3}}, {{3, 0}},
				{{4, 5}}, {{5, 6}}, {{6, 7}}, {{7, 4}},
				{{0, 4}}, {{1, 5}}, {{2, 6}}, {{3, 7}}
			}};
			const u32 Color = Glow->Locked()
				? 0xffff0000u
				: 0xffffffffu;
			for (const auto& Edge : Edges)
			{
				if (Lines.size() >= MaxLegacyShapeDebugLines)
				{
					break;
				}
				AppendDebugLine(
					Lines,
					Corners[Edge[0]],
					Corners[Edge[1]],
					Color
				);
			}
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(DebugDrawRevision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
			HashBytes(DebugDrawRevision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
		}
	}
}

void AppendLegacySpawnDebugDraw(
	xr_vector<FEditorModelInstance>& ModelInstances,
	xr_vector<FEditorParticleInstance>& ParticleInstances,
	const FEditorParticleLibrarySnapshot& ParticleLibrary,
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles,
	xr_vector<FEditorOverlayText>& OverlayText,
	u64& Revision
)
{
	const size_t FirstLine = Lines.size();
	const size_t FirstTriangle = Triangles.size();
	const size_t FirstText = OverlayText.size();
	for (CCustomObject* CustomObject :
		 Scene->ListObj(OBJCLASS_SPAWNPOINT))
	{
		auto* Spawn = smart_cast<CSpawnPoint*>(CustomObject);
		if (!Spawn || !IsLegacyObjectVisibleForTiramisu(Spawn) ||
			!IsLegacyObjectInsideEditorFrustum(*Spawn))
		{
			continue;
		}
		const Fvector Position = Spawn->GetPosition();
		if (const char* VisualName = Spawn->GetEditorVisualName())
		{
			FEditorModelInstance& Model = ModelInstances.emplace_back();
			Model.ObjectId = {MakePointerId(Spawn)};
			Model.AssetName = VisualName;
			if (const char* AnimationName =
					Spawn->GetEditorAnimationName())
			{
				Model.AnimationName = AnimationName;
			}
			CopyMatrix(Spawn->_Transform(), Model.LocalToWorld);
			if (Spawn->Selected())
			{
				Model.Flags = EEditorSceneInstanceFlags::Selected;
			}
		}
		if (const char* ParticleName =
				Spawn->GetEditorIdleParticleName())
		{
			const auto Asset = std::ranges::find_if(
				ParticleLibrary.Assets,
				[ParticleName](const FEditorParticleAssetInfo& Candidate)
				{
					return Candidate.Type !=
							EEditorParticleAssetType::AnimationCurve &&
						xr_strcmp(Candidate.Name.c_str(), ParticleName) == 0;
				}
			);
			if (Asset != ParticleLibrary.Assets.end())
			{
				FEditorParticleInstance& Particle =
					ParticleInstances.emplace_back();
				Particle.ObjectId = {
					MakePointerId(Spawn) ^ 0x69646c6570617274ull
				};
				Particle.AssetName = ParticleName;
				Particle.AssetType = Asset->Type;
				CopyMatrix(Spawn->_Transform(), Particle.LocalToWorld);
				u32 Flags = static_cast<u32>(
					EEditorParticleInstanceFlags::Playing
				);
				if (Spawn->Selected())
				{
					Flags |= static_cast<u32>(
						EEditorParticleInstanceFlags::Selected
					);
				}
				Particle.Flags =
					static_cast<EEditorParticleInstanceFlags>(Flags);
			}
		}
		if (auto* Shape =
				smart_cast<CEditShape*>(Spawn->m_AttachedObject))
		{
			AppendLegacyShapeObjectDebugDraw(
				*Shape,
				Lines,
				Triangles
			);
		}
		constexpr float IconRadius = 0.3f;
		const xr_array<Fvector, 6> IconVertices = {{
			{Position.x, Position.y + IconRadius, Position.z},
			{Position.x, Position.y - IconRadius, Position.z},
			{Position.x - IconRadius, Position.y, Position.z},
			{Position.x + IconRadius, Position.y, Position.z},
			{Position.x, Position.y, Position.z - IconRadius},
			{Position.x, Position.y, Position.z + IconRadius}
		}};
		constexpr xr_array<xr_array<size_t, 2>, 12> IconEdges = {{
			{{0, 2}}, {{0, 3}}, {{0, 4}}, {{0, 5}},
			{{1, 2}}, {{1, 3}}, {{1, 4}}, {{1, 5}},
			{{2, 4}}, {{4, 3}}, {{3, 5}}, {{5, 2}}
		}};
		const u32 IconColor = Spawn->Locked()
			? 0xffff0000u
			: Spawn->Selected()
				? 0xffffb030u
				: 0xff40c0ffu;
		for (const auto& Edge : IconEdges)
		{
			if (Lines.size() >= MaxLegacyShapeDebugLines)
			{
				break;
			}
			AppendDebugLine(
				Lines,
				IconVertices[Edge[0]],
				IconVertices[Edge[1]],
				IconColor
			);
		}
		if (Spawn->Selected())
		{
			Fbox Box;
			if (Spawn->GetBox(Box))
			{
				const xr_array<Fvector, 8> Corners = {{
					{Box.min.x, Box.min.y, Box.min.z},
					{Box.max.x, Box.min.y, Box.min.z},
					{Box.max.x, Box.max.y, Box.min.z},
					{Box.min.x, Box.max.y, Box.min.z},
					{Box.min.x, Box.min.y, Box.max.z},
					{Box.max.x, Box.min.y, Box.max.z},
					{Box.max.x, Box.max.y, Box.max.z},
					{Box.min.x, Box.max.y, Box.max.z}
				}};
				constexpr xr_array<xr_array<size_t, 2>, 12> Edges = {{
					{{0, 1}}, {{1, 2}}, {{2, 3}}, {{3, 0}},
					{{4, 5}}, {{5, 6}}, {{6, 7}}, {{7, 4}},
					{{0, 4}}, {{1, 5}}, {{2, 6}}, {{3, 7}}
				}};
				for (const auto& Edge : Edges)
				{
					if (Lines.size() >= MaxLegacyShapeDebugLines)
					{
						break;
					}
					AppendDebugLine(
						Lines,
						Corners[Edge[0]],
						Corners[Edge[1]],
						Spawn->Locked()
							? 0xffff0000u
							: 0xffffffffu
					);
				}
			}
			if (Spawn->m_Type == ptEnvMod &&
				Spawn->m_EM_ShapeType == CShapeData::cfSphere)
			{
				AppendWireSphere(
					Lines,
					Position,
					Spawn->m_EM_Radius,
					0xffffae00u
				);
			}
		}

		Fvector Projected;
		const float W =
			Position.x * EDevice->mFullTransform._14 +
			Position.y * EDevice->mFullTransform._24 +
			Position.z * EDevice->mFullTransform._34 +
			EDevice->mFullTransform._44;
		if (W > 0.0f)
		{
			EDevice->mFullTransform.transform(Projected, Position);
			FEditorOverlayText& Label = OverlayText.emplace_back();
			Label.Position = {Projected.x, Projected.y};
			Label.Color = {1.0f, 1.0f, 1.0f, 1.0f};
			Label.ShadowColor = {0.0f, 0.0f, 0.0f, 1.0f};
			switch (Spawn->m_Type)
			{
				case ptRPoint:
					Label.Text = "RPoint T:" +
						xr_string::ToString(Spawn->m_RP_TeamID);
					break;
				case ptEnvMod:
					Label.Text = "EnvMod";
					break;
				case ptSpawnPoint:
					Label.Text = Spawn->RefName()
						? Spawn->RefName()
						: "Spawn";
					break;
				default:
					Label.Text = "Spawn";
					break;
			}
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(Revision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
			HashBytes(Revision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
		}
	}
	for (size_t Index = FirstTriangle;
		 Index < Triangles.size();
		 ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Triangles[Index].Vertices)
		{
			HashBytes(
				Revision,
				Vertex.Position.data(),
				Vertex.Position.size() * sizeof(float)
			);
			HashBytes(
				Revision,
				Vertex.Color.data(),
				Vertex.Color.size() * sizeof(float)
			);
		}
	}
	for (size_t Index = FirstText; Index < OverlayText.size(); ++Index)
	{
		const FEditorOverlayText& Label = OverlayText[Index];
		HashBytes(Revision, Label.Position.data(), Label.Position.size() * sizeof(float));
		HashBytes(Revision, Label.Text.data(), Label.Text.size());
	}
}

void AppendLegacyAiMapDebugDraw(
	FViewportBridgeState& State,
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles,
	u64& Revision
)
{
	auto* AiMap = smart_cast<ESceneAIMapTool*>(
		Scene->GetTool(OBJCLASS_AIMAP)
	);
	if (!AiMap || AiMap->m_Flags.is(ESceneAIMapTool::flHideNodes))
	{
		return;
	}
	const float HalfSize = AiMap->AIParams().fPatchSize * 0.45f;
	u64 SourceRevision = FnvOffset;
	const u64 RenderDataRevision = AiMap->GetRenderDataRevision();
	HashBytes(
		SourceRevision,
		&RenderDataRevision,
		sizeof(RenderDataRevision)
	);
	HashBytes(
		SourceRevision,
		&EDevice->mFullTransform,
		sizeof(EDevice->mFullTransform)
	);
	HashBytes(
		SourceRevision,
		&AiMap->m_VisRadius,
		sizeof(AiMap->m_VisRadius)
	);
	HashBytes(SourceRevision, &HalfSize, sizeof(HalfSize));
	if (SourceRevision == 0)
	{
		SourceRevision = 1;
	}
	const auto AppendCachedGeometry = [&]()
	{
		HashBytes(
			Revision,
			&SourceRevision,
			sizeof(SourceRevision)
		);
		if (Revision == 0)
		{
			Revision = 1;
		}
		if (State.SubmittedDebugDrawRevision == Revision)
		{
			return;
		}
		const size_t AvailableLineCount =
			Lines.size() < MaxLegacyShapeDebugLines
				? MaxLegacyShapeDebugLines - Lines.size()
				: 0;
		const size_t LineCount = std::min(
			State.AiMapDebugLines.size(),
			AvailableLineCount
		);
		Lines.insert(
			Lines.end(),
			State.AiMapDebugLines.begin(),
			State.AiMapDebugLines.begin() + LineCount
		);
		const size_t AvailableTriangleCount =
			Triangles.size() < MaxLegacyShapeDebugTriangles
				? MaxLegacyShapeDebugTriangles - Triangles.size()
				: 0;
		const size_t TriangleCount = std::min(
			State.AiMapDebugTriangles.size(),
			AvailableTriangleCount
		);
		Triangles.insert(
			Triangles.end(),
			State.AiMapDebugTriangles.begin(),
			State.AiMapDebugTriangles.begin() + TriangleCount
		);
	};
	if (State.AiMapDebugSourceRevision == SourceRevision)
	{
		AppendCachedGeometry();
		return;
	}
	AINodeVec VisibleNodes;
	AiMap->CollectVisibleNodes(
		EDevice->vCameraPosition,
		AiMap->m_VisRadius,
		MaxLegacyAiNodeCount,
		VisibleNodes
	);
	const Fvector Up = {0.0f, 1.0f, 0.0f};
	xr_vector<SAINode*> FrustumNodes;
	FrustumNodes.reserve(VisibleNodes.size());
	for (SAINode* Node : VisibleNodes)
	{
		if (!Node)
		{
			continue;
		}
		const float VisibilityRadius = std::max(
			AiMap->AIParams().fPatchSize,
			HalfSize
		);
		if (!::Render->ViewBase.testSphere_dirty(
				Node->Pos,
				VisibilityRadius
			))
		{
			continue;
		}
		FrustumNodes.push_back(Node);
	}

	if (State.AiMapDebugSourceRevision != SourceRevision)
	{
		State.AiMapDebugLines.clear();
		State.AiMapDebugTriangles.clear();
		State.AiMapDebugLines.reserve(FrustumNodes.size() * 2);
		State.AiMapDebugTriangles.reserve(FrustumNodes.size() * 2);
		for (SAINode* Node : FrustumNodes)
		{
			if (State.AiMapDebugTriangles.size() + 1 >=
				MaxLegacyShapeDebugTriangles)
			{
				break;
			}
			const u32 Color = Node->flags.is(SAINode::flSelected)
				? 0xffffffffu
				: Node->flags.is(SAINode::flHLSelected)
					? 0xff909090u
					: 0xff606060u;
			xr_array<Fvector, 4> Corners;
			const xr_array<Fvector2, 4> Offsets = {{
				{-HalfSize, -HalfSize},
				{HalfSize, -HalfSize},
				{HalfSize, HalfSize},
				{-HalfSize, HalfSize}
			}};
			for (size_t Corner = 0; Corner < Corners.size(); ++Corner)
			{
				Fvector Origin = {
					Node->Pos.x + Offsets[Corner].x,
					Node->Pos.y,
					Node->Pos.z + Offsets[Corner].y
				};
				Node->Plane.intersectRayPoint(
					Origin,
					Up,
					Corners[Corner]
				);
				Corners[Corner].mad(
					Corners[Corner],
					Node->Plane.n,
					0.01f
				);
			}
			FEditorDebugTriangle& First =
				State.AiMapDebugTriangles.emplace_back();
			First.Vertices[0] = MakeDebugVertex(Corners[2], Color);
			First.Vertices[1] = MakeDebugVertex(Corners[1], Color);
			First.Vertices[2] = MakeDebugVertex(Corners[0], Color);
			FEditorDebugTriangle& Second =
				State.AiMapDebugTriangles.emplace_back();
			Second.Vertices[0] = MakeDebugVertex(Corners[0], Color);
			Second.Vertices[1] = MakeDebugVertex(Corners[3], Color);
			Second.Vertices[2] = MakeDebugVertex(Corners[2], Color);
			for (SAINode* Neighbor : Node->n)
			{
				if (!Neighbor || State.AiMapDebugLines.size() >=
					MaxLegacyShapeDebugLines)
				{
					continue;
				}
				const bool Reciprocal = std::ranges::find(
					Neighbor->n,
					Node
				) != Neighbor->n + std::size(Neighbor->n);
				if (Reciprocal &&
					reinterpret_cast<std::uintptr_t>(Neighbor) <=
						reinterpret_cast<std::uintptr_t>(Node))
				{
					continue;
				}
				AppendDebugLine(
					State.AiMapDebugLines,
					Node->Pos,
					Neighbor->Pos,
					0xff40a0ffu
				);
			}
		}
		State.AiMapDebugSourceRevision = SourceRevision;
	}

	AppendCachedGeometry();
}

void AppendLegacyWayDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorOverlayText>& OverlayText,
	u64& Revision
)
{
	const size_t FirstLine = Lines.size();
	const size_t FirstText = OverlayText.size();
	for (CCustomObject* CustomObject : Scene->ListObj(OBJCLASS_WAY))
	{
		auto* Way = smart_cast<CWayObject*>(CustomObject);
		if (!Way || !IsLegacyObjectVisibleForTiramisu(Way))
		{
			continue;
		}
		for (CWayPoint* Point : Way->WayPoints())
		{
			if (!Point || !::Render->ViewBase.testSphere_dirty(
					Point->Position(),
					0.75f
				))
			{
				continue;
			}
			Fvector Center = Point->Position();
			Center.y += 1.275f;
			constexpr float HorizontalRadius = 0.75f;
			constexpr float VerticalRadius = 1.275f;
			AppendDebugLine(
				Lines,
				{Center.x - HorizontalRadius, Center.y, Center.z},
				{Center.x + HorizontalRadius, Center.y, Center.z},
				0xff00ff00u
			);
			AppendDebugLine(
				Lines,
				{Center.x, Center.y - VerticalRadius, Center.z},
				{Center.x, Center.y + VerticalRadius, Center.z},
				0xff00ff00u
			);
			AppendDebugLine(
				Lines,
				{Center.x, Center.y, Center.z - HorizontalRadius},
				{Center.x, Center.y, Center.z + HorizontalRadius},
				0xff00ff00u
			);
			for (SWPLink* Link : Point->Links())
			{
				if (!Link || !Link->way_point ||
					Lines.size() >= MaxLegacyShapeDebugLines)
				{
					continue;
				}
				const CWayPoint* Neighbor = Link->way_point;
				const bool Reciprocal = std::ranges::any_of(
					Neighbor->Links(),
					[Point](const SWPLink* Candidate)
					{
						return Candidate &&
							Candidate->way_point == Point;
					}
				);
				if (Reciprocal &&
					reinterpret_cast<std::uintptr_t>(Neighbor) <=
						reinterpret_cast<std::uintptr_t>(Point))
				{
					continue;
				}
				Fvector LinkStart = Point->Position();
				Fvector LinkEnd = Neighbor->Position();
				LinkStart.y += 1.275f;
				LinkEnd.y += 1.275f;
				AppendDebugLine(
					Lines,
					LinkStart,
					LinkEnd,
					Way->Selected()
						? 0xffffff00u
						: 0xff606000u
				);
			}
			if (Way->Selected())
			{
				Fvector Projected;
				const Fvector Position = Point->Position();
				const float W =
					Position.x * EDevice->mFullTransform._14 +
					Position.y * EDevice->mFullTransform._24 +
					Position.z * EDevice->mFullTransform._34 +
					EDevice->mFullTransform._44;
				if (W > 0.0f)
				{
					EDevice->mFullTransform.transform(
						Projected,
						Position
					);
					FEditorOverlayText& Label = OverlayText.emplace_back();
					Label.Position = {Projected.x, Projected.y};
					Label.Color = Point->IsSelected()
						? xr_array<float, 4>{1.0f, 1.0f, 1.0f, 1.0f}
						: xr_array<float, 4>{0.63f, 0.63f, 0.63f, 1.0f};
					Label.ShadowColor = {0.0f, 0.0f, 0.0f, 1.0f};
					Label.Text = xr_string(" ") + Way->GetName() +
						" [" + Point->Name() + "]";
				}
			}
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(Revision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
			HashBytes(Revision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
		}
	}
	for (size_t Index = FirstText; Index < OverlayText.size(); ++Index)
	{
		const FEditorOverlayText& Label = OverlayText[Index];
		HashBytes(Revision, Label.Position.data(), Label.Position.size() * sizeof(float));
		HashBytes(Revision, Label.Text.data(), Label.Text.size());
	}
}

void AppendLegacyGroupDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	u64& Revision
)
{
	const size_t FirstLine = Lines.size();
	for (CCustomObject* CustomObject : Scene->ListObj(OBJCLASS_GROUP))
	{
		auto* Group = smart_cast<CGroupObject*>(CustomObject);
		if (!Group || !IsLegacyObjectVisibleForTiramisu(Group) ||
			!Group->Selected())
		{
			continue;
		}
		Fbox Box;
		if (Group->GetBox(Box))
		{
			AppendWireBox(
				Lines,
				Box,
				Group->Locked() ? 0xffff0000u : 0xff7070ffu
			);
		}
	}
	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(Revision, Vertex.Position.data(), Vertex.Position.size() * sizeof(float));
			HashBytes(Revision, Vertex.Color.data(), Vertex.Color.size() * sizeof(float));
		}
	}
}

void AppendLegacySectorDebugDraw(
	xr_vector<FEditorDebugLine>& Lines,
	u64& Revision
)
{
	const size_t FirstLine = Lines.size();
	for (CCustomObject* CustomObject : Scene->ListObj(OBJCLASS_SECTOR))
	{
		auto* Sector = smart_cast<CSector*>(CustomObject);
		if (!Sector || !IsLegacyObjectVisibleForTiramisu(Sector) ||
			!Sector->Selected())
		{
			continue;
		}

		Fbox Box;
		if (Sector->GetBox(Box))
		{
			AppendWireBox(
				Lines,
				Box,
				Sector->Locked() ? 0xffff0000u : 0xffffffffu
			);
		}
	}

	for (size_t Index = FirstLine; Index < Lines.size(); ++Index)
	{
		for (const FEditorDebugVertex& Vertex : Lines[Index].Vertices)
		{
			HashBytes(
				Revision,
				Vertex.Position.data(),
				Vertex.Position.size() * sizeof(float)
			);
			HashBytes(
				Revision,
				Vertex.Color.data(),
				Vertex.Color.size() * sizeof(float)
			);
		}
	}
}
} // namespace

bool SubmitEditorSceneToEditorRenderer(const u32 ViewportId)
{
	PROF_EVENT("Tiramisu Editor: Submit Scene");
	const auto SubmissionStart = std::chrono::steady_clock::now();
	if (!Scene || !UI || !EDevice)
	{
		return false;
	}
	IEditorRenderBackend& Renderer = GetEditorRenderBackend();
	if (Renderer.GetKind() != EEditorRenderBackendKind::Tiramisu)
	{
		return true;
	}

	FViewportBridgeState& State = ViewportStates[ViewportId];
	const u64 PreviousDebugDrawRevision =
		State.SubmittedDebugDrawRevision;
	const TiramisuEditorNativeSceneDocument& NativeDocument =
		GetEditorNativeSceneDocument();
	const Tiramisu::Scene::FResolvedRenderScene* NativeScene =
		NativeDocument.GetScene();
	xr_vector<FLegacyMeshInstanceSource> VisibleMeshInstances;
	xr_hash_set<CEditableMesh*> UniqueMeshes;
	xr_hash_set<CEditableMesh*> AllLegacyMeshes;
	size_t LegacyMeshInstanceCount = 0;
	if (!NativeScene)
	{
		for (CCustomObject* CustomObject :
			 Scene->ListObj(OBJCLASS_SCENEOBJECT))
		{
			if (!CustomObject)
			{
				continue;
			}
			auto* SceneObject = static_cast<CSceneObject*>(CustomObject);
			EditMeshVec* Meshes = SceneObject->Meshes();
			if (!Meshes)
			{
				continue;
			}
			for (CEditableMesh* Mesh : *Meshes)
			{
				if (Mesh && Mesh->Visible())
				{
					AllLegacyMeshes.insert(Mesh);
					++LegacyMeshInstanceCount;
				}
			}
			if (!IsLegacyObjectVisibleForTiramisu(CustomObject) ||
				!IsLegacyObjectInsideEditorFrustum(*CustomObject))
			{
				continue;
			}
			// Resolve all visible legacy objects in one migration transaction.
			// The cache makes subsequent viewport submissions read-only.
			SceneObject->ResolveRenderMaterials(true);
			for (CEditableMesh* Mesh : *Meshes)
			{
				if (!Mesh || !Mesh->Visible())
				{
					continue;
				}
				VisibleMeshInstances.push_back(
					{SceneObject, SceneObject, Mesh}
				);
				UniqueMeshes.insert(Mesh);
			}
		}

		for (CCustomObject* CustomObject : Scene->ListObj(OBJCLASS_TERRAIN))
		{
			if (!CustomObject)
			{
				continue;
			}
			auto* Terrain = static_cast<CTerrain*>(CustomObject);
			CEditableObject* Reference = Terrain->GetReference();
			if (!Reference)
			{
				continue;
			}
			for (CEditableMesh* Mesh : Reference->Meshes())
			{
				if (Mesh && Mesh->Visible())
				{
					AllLegacyMeshes.insert(Mesh);
					++LegacyMeshInstanceCount;
				}
			}
			if (!IsLegacyObjectVisibleForTiramisu(CustomObject) ||
				!IsLegacyObjectInsideEditorFrustum(*CustomObject))
			{
				continue;
			}
			for (CEditableMesh* Mesh : Reference->Meshes())
			{
				if (!Mesh || !Mesh->Visible())
				{
					continue;
				}
				VisibleMeshInstances.push_back({Terrain, nullptr, Mesh});
				UniqueMeshes.insert(Mesh);
			}
		}
		if (!CSceneObject::FlushRenderMaterialMigration())
		{
			Msg("! Tiramisu could not publish legacy scene material migration database.");
		}
	}

	xr_hash_map<u64, u64> CurrentMeshRevisions;
	xr_hash_map<u64, FEditorMaterialSlotSource>
		MaterialSlotsById;
	if (!NativeScene)
	{
		const FEditorMaterialSlotSource FallbackMaterial =
			MakeMaterialSlotSource(nullptr);
		MaterialSlotsById.emplace(FallbackMaterial.MaterialSlot.Value, FallbackMaterial);
		for (CEditableMesh* Mesh : AllLegacyMeshes)
		{
			if (UniqueMeshes.contains(Mesh))
			{
				for (const auto& [Surface, Faces] : Mesh->GetSurfFaces())
				{
					(void)Faces;
					const FEditorMaterialSlotSource Material =
						MakeMaterialSlotSource(Surface);
					MaterialSlotsById.insert_or_assign(
						Material.MaterialSlot.Value, Material
					);
				}
			}
		}
	}
	const auto CollectionEnd = std::chrono::steady_clock::now();
	xr_vector<FMeshPayload> ChangedPayloads;
	xr_vector<FEditorStaticMeshInstance> Instances;
	xr_vector<FEditorSceneLight> Lights;
	xr_vector<FEditorDecalInstance> Decals;
	xr_vector<FEditorModelInstance> ModelInstances;
	xr_vector<FEditorParticleInstance> ParticleInstances;
	FEditorParticleLibrarySnapshot ParticleLibrary;
	if (!NativeScene)
	{
		Renderer.CopyParticleLibrary(ParticleLibrary);
	}
	if (NativeScene)
	{
		if (!BuildNativeScenePayload(
				NativeDocument,
				*NativeScene,
				State,
				CurrentMeshRevisions,
				MaterialSlotsById,
				ChangedPayloads,
				Instances,
				Lights,
				Decals
			))
		{
			return false;
		}
	}
	else
	{
		AppendLegacyEditorLights(Lights);
		AppendLegacyEditorDecals(MaterialSlotsById, Decals);
		for (CEditableMesh* Mesh : AllLegacyMeshes)
		{
			const FEditorStaticMeshId MeshId{MakePointerId(Mesh)};
			if (!UniqueMeshes.contains(Mesh))
			{
				const auto Existing = State.MeshRevisions.find(
					MeshId.Value
				);
				if (Existing != State.MeshRevisions.end())
				{
					CurrentMeshRevisions.emplace(
						MeshId.Value,
						Existing->second
					);
				}
				continue;
			}
			u64 Revision = 0;
			if (!ComputeMeshRevision(*Mesh, Revision))
			{
				continue;
			}
			CurrentMeshRevisions.emplace(MeshId.Value, Revision);
			const auto Existing = State.MeshRevisions.find(MeshId.Value);
			if (Existing != State.MeshRevisions.end() &&
				Existing->second == Revision)
			{
				continue;
			}
			FMeshPayload Payload;
			if (BuildMeshPayload(*Mesh, MeshId, Revision, Payload))
			{
				ChangedPayloads.push_back(std::move(Payload));
			}
			else
			{
				CurrentMeshRevisions.erase(MeshId.Value);
			}
		}

		Instances.reserve(VisibleMeshInstances.size());
		for (const FLegacyMeshInstanceSource& Source :
			 VisibleMeshInstances)
		{
			CEditableMesh* Mesh = Source.Mesh;
			const FEditorStaticMeshId MeshId{MakePointerId(Mesh)};
			if (!CurrentMeshRevisions.contains(MeshId.Value))
			{
				continue;
			}
			FEditorStaticMeshInstance Instance;
			Instance.ObjectId = {MakePointerId(Source.Object)};
			Instance.MeshId = MeshId;
			Fmatrix LocalToWorld;
			if (Source.SceneObject)
			{
				Source.SceneObject->GetFullTransformToWorld(LocalToWorld);
			}
			else
			{
				LocalToWorld = Source.Object->_Transform();
			}
			CopyMatrix(LocalToWorld, Instance.LocalToWorld);
			if (Source.Object->Selected())
			{
				Instance.Flags = EEditorSceneInstanceFlags::Selected;
			}
			if (Source.SceneObject)
			{
				AppendLegacyMaterialOverrides(
					*Source.SceneObject,
					*Mesh,
					Instance,
					MaterialSlotsById
				);
			}
			Instances.push_back(Instance);
		}
		const FLegacyDetailBridgeResult DetailResult =
			AppendLegacyDetailObjects(
			State,
			CurrentMeshRevisions,
			MaterialSlotsById,
			ChangedPayloads,
			Instances
			);
		if (State.LegacyDetailModelCount != DetailResult.ModelCount ||
			State.LegacyDetailInstanceCount != DetailResult.InstanceCount)
		{
			Msg(
				"* Tiramisu legacy details: models=%u, instances=%u",
				DetailResult.ModelCount,
				DetailResult.InstanceCount
			);
			State.LegacyDetailModelCount = DetailResult.ModelCount;
			State.LegacyDetailInstanceCount = DetailResult.InstanceCount;
		}
		AppendLegacyDetailBaseTexture(
			State,
			CurrentMeshRevisions,
			MaterialSlotsById,
			ChangedPayloads,
			Instances
		);

		for (CCustomObject* CustomObject : Scene->ListObj(OBJCLASS_PS))
		{
			if (!CustomObject ||
				!IsLegacyObjectVisibleForTiramisu(CustomObject))
			{
				continue;
			}
			auto* ParticleObject =
				static_cast<EParticlesObject*>(CustomObject);
			const char* Reference = ParticleObject->GetReferenceName();
			if (!Reference || !Reference[0])
			{
				continue;
			}
			FEditorParticleInstance Instance;
			Instance.ObjectId = {MakePointerId(ParticleObject)};
			Instance.AssetName = Reference;
			for (const FEditorParticleAssetInfo& Asset :
				 ParticleLibrary.Assets)
			{
				if (Asset.Name == Reference)
				{
					Instance.AssetType = Asset.Type;
					break;
				}
			}
			CopyMatrix(
				ParticleObject->_Transform(), Instance.LocalToWorld
			);
			u32 Flags = ParticleObject->Selected()
				? static_cast<u32>(
					  EEditorParticleInstanceFlags::Selected
				  )
				: 0;
			if (ParticleObject->IsPlaying())
			{
				Flags |= static_cast<u32>(
					EEditorParticleInstanceFlags::Playing
				);
			}
			Instance.Flags =
				static_cast<EEditorParticleInstanceFlags>(Flags);
			ParticleInstances.push_back(Instance);
		}
	}
	const auto ScenePayloadEnd = std::chrono::steady_clock::now();
	xr_vector<FEditorDebugLine> DebugLines;
	xr_vector<FEditorDebugTriangle> DebugTriangles;
	xr_vector<FEditorOverlayLine> OverlayLines;
	xr_vector<FEditorOverlayTriangle> OverlayTriangles;
	xr_vector<FEditorOverlayText> OverlayText;
	xr_vector<FEditorTransientMeshCapture> TransientMeshes;
	u64 DebugDrawRevision =
		BuildDebugDraw(DebugLines, DebugTriangles, OverlayLines, OverlayTriangles, OverlayText, TransientMeshes);
	const auto DebugDrawBaseEnd = std::chrono::steady_clock::now();
	auto SpawnDebugStart = DebugDrawBaseEnd;
	auto SpawnDebugEnd = DebugDrawBaseEnd;
	auto AiMapDebugStart = DebugDrawBaseEnd;
	auto AiMapDebugEnd = DebugDrawBaseEnd;
	if (!NativeScene)
	{
		AppendLegacyDetailSlotDebugDraw(
			DebugLines,
			DebugDrawRevision
		);
		AppendLegacyShapeDebugDraw(
			DebugLines,
			DebugTriangles,
			DebugDrawRevision
		);
		AppendLegacySoundSourceDebugDraw(
			DebugLines,
			DebugDrawRevision
		);
		AppendLegacyPortalDebugDraw(
			DebugLines,
			DebugTriangles,
			DebugDrawRevision
		);
		AppendLegacyGlowPayload(
			State,
			CurrentMeshRevisions,
			MaterialSlotsById,
			ChangedPayloads,
			Instances,
			DebugLines,
			DebugDrawRevision
		);
		SpawnDebugStart = std::chrono::steady_clock::now();
		AppendLegacySpawnDebugDraw(
			ModelInstances,
			ParticleInstances,
			ParticleLibrary,
			DebugLines,
			DebugTriangles,
			OverlayText,
			DebugDrawRevision
		);
		SpawnDebugEnd = std::chrono::steady_clock::now();
		AppendLegacyWayDebugDraw(
			DebugLines,
			OverlayText,
			DebugDrawRevision
		);
		AppendLegacyGroupDebugDraw(
			DebugLines,
			DebugDrawRevision
		);
		AppendLegacySectorDebugDraw(
			DebugLines,
			DebugDrawRevision
		);
	}
	AppendNativeLightDebugDraw(
		Lights, DebugLines, DebugDrawRevision
	);
	for (const FEditorParticleInstance& Particle : ParticleInstances)
	{
		HashBytes(
			DebugDrawRevision,
			Particle.AssetName.data(),
			Particle.AssetName.size()
		);
		HashBytes(
			DebugDrawRevision,
			Particle.LocalToWorld.data(),
			Particle.LocalToWorld.size() * sizeof(float)
		);
		HashBytes(
			DebugDrawRevision, &Particle.Flags, sizeof(Particle.Flags)
		);
	}
	AiMapDebugStart = std::chrono::steady_clock::now();
	if (!NativeScene)
	{
		AppendLegacyAiMapDebugDraw(
			State,
			DebugLines,
			DebugTriangles,
			DebugDrawRevision
		);
	}
	AiMapDebugEnd = std::chrono::steady_clock::now();

	for (const FEditorTransientMeshCapture& Capture : TransientMeshes)
	{
		MaterialSlotsById.insert_or_assign(Capture.MaterialSlot.Value, FEditorMaterialSlotSource{Capture.MaterialSlot, Capture.ShaderName, Capture.TextureName, Capture.SurfaceName, Capture.MaterialFlags});

		const auto [RevisionIt, Inserted] = CurrentMeshRevisions.emplace(
			Capture.MeshId.Value, Capture.Revision
		);
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
				Payload.Sections.push_back({0, static_cast<u32>(Payload.Indices.size()), Capture.MaterialSlot});
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
	std::ranges::sort(MaterialSlots, {}, [](const FEditorMaterialSlotSource& Material)
					  { return Material.MaterialSlot.Value; });

	xr_vector<FEditorStaticMeshUpload> ChangedMeshes;
	ChangedMeshes.reserve(ChangedPayloads.size());
	for (const FMeshPayload& Payload : ChangedPayloads)
	{
		ChangedMeshes.push_back(Payload.MakeUpload());
	}

	xr_vector<FEditorStaticMeshId> RemovedMeshes;
	for (const auto& [MeshId, Revision] : State.MeshRevisions)
	{
		(void)Revision;
		if (!CurrentMeshRevisions.contains(MeshId))
		{
			RemovedMeshes.push_back({MeshId});
		}
	}

	FEditorViewportSceneSnapshot Snapshot;
	Snapshot.Camera = BuildCamera();
	Snapshot.MaterialSlots = MaterialSlots;
	Snapshot.StaticMeshes = ChangedMeshes;
	Snapshot.RemovedStaticMeshes = RemovedMeshes;
	Snapshot.Instances = Instances;
	Snapshot.DecalInstances = Decals;
	Snapshot.ModelInstances = ModelInstances;
	Snapshot.Lights = Lights;
	Snapshot.ParticleInstances = ParticleInstances;
	Snapshot.DebugLines = DebugLines;
	Snapshot.DebugTriangles = DebugTriangles;
	Snapshot.OverlayLines = OverlayLines;
	Snapshot.OverlayTriangles = OverlayTriangles;
	Snapshot.OverlayText = OverlayText;
	Snapshot.DebugDrawRevision = DebugDrawRevision;
	Snapshot.Revision = ++State.SceneRevision;
	const auto AuxiliaryPayloadEnd = std::chrono::steady_clock::now();
	if (!Renderer.SubmitViewportScene(ViewportId, Snapshot))
	{
		return false;
	}
	State.MeshRevisions = std::move(CurrentMeshRevisions);
	State.SubmittedDebugDrawRevision = DebugDrawRevision;
	const auto SubmissionMicroseconds =
		std::chrono::duration_cast<std::chrono::microseconds>(
			std::chrono::steady_clock::now() - SubmissionStart
		).count();
	if (SubmissionMicroseconds >= 10'000 &&
		(State.SlowSubmissionLogCount < 3 ||
		 State.SceneRevision % 120 == 0))
	{
		++State.SlowSubmissionLogCount;
		const auto MillisecondsBetween = [](
			const auto Begin,
			const auto End
		)
		{
			return static_cast<double>(
				std::chrono::duration_cast<std::chrono::microseconds>(
					End - Begin
				).count()
			) / 1000.0;
		};
		Msg(
			"! Tiramisu editor scene submit is slow: %.2f ms, "
			"collect=%.2f, payload=%.2f, auxiliary=%.2f "
			"(base=%.2f, spawn=%.2f, ai=%.2f), mailbox=%.2f, "
			"instances=%zu/%zu, changed-meshes=%zu, materials=%zu, "
			"debug-lines=%zu, debug-triangles=%zu, overlay-text=%zu, "
			"debug-revision=%llu/%llu, ai-revision=%llu",
			static_cast<double>(SubmissionMicroseconds) / 1000.0,
			MillisecondsBetween(SubmissionStart, CollectionEnd),
			MillisecondsBetween(CollectionEnd, ScenePayloadEnd),
			MillisecondsBetween(ScenePayloadEnd, AuxiliaryPayloadEnd),
			MillisecondsBetween(ScenePayloadEnd, DebugDrawBaseEnd),
			MillisecondsBetween(SpawnDebugStart, SpawnDebugEnd),
			MillisecondsBetween(AiMapDebugStart, AiMapDebugEnd),
			MillisecondsBetween(
				AuxiliaryPayloadEnd,
				std::chrono::steady_clock::now()
			),
			Instances.size(),
			LegacyMeshInstanceCount,
			ChangedMeshes.size(),
			MaterialSlots.size(),
			DebugLines.size(),
			DebugTriangles.size(),
			OverlayText.size(),
			static_cast<unsigned long long>(DebugDrawRevision),
			static_cast<unsigned long long>(PreviousDebugDrawRevision),
			static_cast<unsigned long long>(
				State.AiMapDebugSourceRevision
			)
		);
	}
	return true;
}

bool SubmitLegacySceneToEditorRenderer(const u32 ViewportId)
{
	return SubmitEditorSceneToEditorRenderer(ViewportId);
}
