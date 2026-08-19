#pragma once

#include "../../../xrCore/xrCore.h"

#include "../../../Include/xrRender/EditorRenderer.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <mutex>
#include <ranges>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

// Renderer-owned binding skeletal vertex. Он не входит в публичный контракт
// LevelEditor: OGF parser добавляет данные только внутри xrRenderTiramisu.
struct FEditorOwnedSkinBinding
{
	xr_array<u16, 4> BoneIndices = {};
	xr_array<float, 4> Weights = {1.0f, 0.0f, 0.0f, 0.0f};
};

// Владеющий CPU-пакет geometry static mesh для render thread.
struct FEditorOwnedStaticMeshUpload
{
	FEditorStaticMeshId MeshId;
	u64 Revision = 0;
	xr_vector<FEditorStaticMeshVertex> Vertices;
	xr_vector<u32> Indices;
	xr_vector<FEditorStaticMeshSection> Sections;
	xr_vector<FEditorOwnedSkinBinding> SkinBindings;
};

// Исходные данные material slot editor mesh до runtime-разрешения.
struct FEditorOwnedMaterialSlotSource
{
	FEditorMaterialSlotId MaterialSlot;
	xr_string ShaderName;
	xr_string TextureName;
	xr_string SurfaceName;
	EEditorMaterialSlotFlags Flags = EEditorMaterialSlotFlags::None;
	xr_string MaterialAsset;
	// Только renderer-owned OGF path запрашивает skeletal permutation.
	bool SupportsSkeletal = false;
};

struct FEditorOwnedParticleInstance
{
	FEditorSceneObjectId ObjectId;
	xr_string AssetName;
	EEditorParticleAssetType AssetType =
		EEditorParticleAssetType::Effect;
	xr_array<float, 16> LocalToWorld = {};
	EEditorParticleInstanceFlags Flags =
		EEditorParticleInstanceFlags::None;
};

struct FEditorOwnedModelInstance
{
	FEditorSceneObjectId ObjectId;
	xr_string AssetName;
	xr_string AnimationName;
	xr_array<float, 16> LocalToWorld = {};
	EEditorSceneInstanceFlags Flags =
		EEditorSceneInstanceFlags::None;
};

// Полный неизменяемый снимок editor scene для передачи renderer.
struct FEditorOwnedViewportScenePacket
{
	FEditorViewportCamera Camera;
	xr_vector<FEditorOwnedMaterialSlotSource> MaterialSlots;
	xr_vector<FEditorOwnedStaticMeshUpload> StaticMeshUpdates;
	xr_vector<FEditorStaticMeshId> RemovedStaticMeshes;
	xr_vector<FEditorStaticMeshInstance> Instances;
	xr_vector<FEditorDecalInstance> DecalInstances;
	xr_vector<FEditorOwnedModelInstance> ModelInstances;
	u32 SourceInstanceCount = 0;
	u32 PendingModelLoadCount = 0;
	u32 ModelDrawCount = 0;
	xr_vector<FEditorSceneLight> Lights;
	xr_vector<FEditorOwnedParticleInstance> ParticleInstances;
	xr_vector<FEditorDebugLine> DebugLines;
	xr_vector<FEditorDebugTriangle> DebugTriangles;
	xr_vector<FEditorOverlayLine> OverlayLines;
	xr_vector<FEditorOverlayTriangle> OverlayTriangles;
	xr_vector<FEditorOverlayText> OverlayText;
	u64 DebugDrawRevision = 0;
	u64 Revision = 0;
};

// Single-producer/single-consumer mailbox used between the editor/game side
// and the renderer. It owns every submitted byte, coalesces mesh updates by
// stable ID and never exposes editor memory to the render thread.
class TiramisuEditorViewportSceneMailbox
{
public:
	[[nodiscard]] bool Submit(const FEditorViewportSceneSnapshot& Snapshot, xr_string* OutDiagnostic = nullptr)
	{
		if (OutDiagnostic)
		{
			OutDiagnostic->clear();
		}
		xr_vector<FEditorOwnedMaterialSlotSource> MaterialCopies;
		MaterialCopies.reserve(Snapshot.MaterialSlots.size());
		xr_hash_set<u64> SubmittedMaterialIds;
		for (const FEditorMaterialSlotSource& Material : Snapshot.MaterialSlots)
		{
			if (!Material.MaterialSlot.IsValid() ||
				!SubmittedMaterialIds.insert(Material.MaterialSlot.Value).second)
			{
				SetDiagnostic(OutDiagnostic, "Material source has an invalid or duplicate slot ID");
				return false;
			}
			MaterialCopies.push_back({Material.MaterialSlot, xr_string(Material.ShaderName), xr_string(Material.TextureName), xr_string(Material.SurfaceName), Material.Flags, xr_string(Material.MaterialAsset)});
		}

		xr_vector<FEditorOwnedStaticMeshUpload> MeshCopies;
		MeshCopies.reserve(Snapshot.StaticMeshes.size());
		xr_hash_set<u64> SubmittedIds;

		for (const FEditorStaticMeshUpload& Mesh : Snapshot.StaticMeshes)
		{
			if (!ValidateMesh(Mesh, SubmittedIds, SubmittedMaterialIds, OutDiagnostic))
			{
				return false;
			}

			FEditorOwnedStaticMeshUpload Copy;
			Copy.MeshId = Mesh.MeshId;
			Copy.Revision = Mesh.Revision;
			Copy.Vertices.assign(Mesh.Vertices.begin(), Mesh.Vertices.end());
			Copy.Indices.assign(Mesh.Indices.begin(), Mesh.Indices.end());
			Copy.Sections.assign(Mesh.Sections.begin(), Mesh.Sections.end());
			MeshCopies.push_back(std::move(Copy));
		}

		if (!ValidateCamera(Snapshot.Camera, OutDiagnostic))
		{
			return false;
		}
		for (const FEditorStaticMeshId Removed : Snapshot.RemovedStaticMeshes)
		{
			if (!Removed.IsValid())
			{
				SetDiagnostic(OutDiagnostic, "Removed mesh ID is invalid");
				return false;
			}
		}
		for (const FEditorStaticMeshInstance& Instance : Snapshot.Instances)
		{
			if (!Instance.ObjectId.IsValid() || !Instance.MeshId.IsValid() ||
				!IsFinite(Instance.LocalToWorld))
			{
				SetDiagnostic(OutDiagnostic, "Scene instance has an invalid ID or transform");
				return false;
			}
			xr_hash_set<u64> OverriddenBaseSlots;
			for (const FEditorMaterialSlotOverride& Override :
				 Instance.MaterialOverrides)
			{
				if (!Override.BaseMaterialSlot.IsValid() ||
					!Override.MaterialSlot.IsValid() ||
					!OverriddenBaseSlots.insert(
											Override.BaseMaterialSlot.Value
					)
						 .second ||
					!SubmittedMaterialIds.contains(
						Override.MaterialSlot.Value
					))
				{
					SetDiagnostic(OutDiagnostic, "Scene instance has an invalid, duplicate or missing "
												 "material override");
					return false;
				}
			}
		}
		xr_hash_set<u64> SceneObjectIds;
		SceneObjectIds.reserve(
			Snapshot.Instances.size() + Snapshot.ModelInstances.size() +
			Snapshot.DecalInstances.size() + Snapshot.Lights.size() +
			Snapshot.ParticleInstances.size()
		);
		for (const FEditorStaticMeshInstance& Instance : Snapshot.Instances)
		{
			SceneObjectIds.insert(Instance.ObjectId.Value);
		}
		if (Snapshot.DecalInstances.size() >
			EditorViewportMaxDecalCount)
		{
			SetDiagnostic(
				OutDiagnostic,
				"Scene decal count exceeds the viewport limit"
			);
			return false;
		}
		for (const FEditorDecalInstance& Decal :
			 Snapshot.DecalInstances)
		{
			if (!Decal.ObjectId.IsValid() ||
				!SceneObjectIds.insert(Decal.ObjectId.Value).second ||
				!Decal.MaterialSlot.IsValid() ||
				!SubmittedMaterialIds.contains(Decal.MaterialSlot.Value) ||
				!IsFinite(Decal.LocalToWorld))
			{
				SetDiagnostic(
					OutDiagnostic,
					"Scene decal has an invalid ID, material or transform"
				);
				return false;
			}
		}
		xr_vector<FEditorOwnedModelInstance> ModelCopies;
		ModelCopies.reserve(Snapshot.ModelInstances.size());
		for (const FEditorModelInstance& Model : Snapshot.ModelInstances)
		{
			if (!Model.ObjectId.IsValid() ||
				!SceneObjectIds.insert(Model.ObjectId.Value).second ||
				Model.AssetName.empty() || Model.AssetName.size() > 1024 ||
				Model.AnimationName.size() > 1024 ||
				!IsFinite(Model.LocalToWorld))
			{
				SetDiagnostic(
					OutDiagnostic,
					"Model instance has an invalid asset, ID or transform"
				);
				return false;
			}
			ModelCopies.push_back({
				Model.ObjectId,
				xr_string(Model.AssetName),
				xr_string(Model.AnimationName),
				Model.LocalToWorld,
				Model.Flags
			});
		}
		if (Snapshot.Lights.size() > EditorViewportMaxLightCount)
		{
			SetDiagnostic(OutDiagnostic, "Scene light count exceeds the viewport limit");
			return false;
		}
		for (const FEditorSceneLight& Light : Snapshot.Lights)
		{
			const bool ValidType =
				Light.Type == EEditorSceneLightType::Directional ||
				Light.Type == EEditorSceneLightType::Point ||
				Light.Type == EEditorSceneLightType::Spot;
			const bool LocalLight =
				Light.Type == EEditorSceneLightType::Point ||
				Light.Type == EEditorSceneLightType::Spot;
			const bool Spot =
				Light.Type == EEditorSceneLightType::Spot;
			if (!ValidType || !Light.ObjectId.IsValid() ||
				!SceneObjectIds.insert(Light.ObjectId.Value).second ||
				!IsFinite(Light.LocalToWorld) ||
				!IsFinite(Light.Color) ||
				!std::isfinite(Light.Intensity) ||
				Light.Intensity < 0.0f ||
				std::ranges::any_of(Light.Color, [](const float Value)
									{ return Value < 0.0f; }) ||
				(LocalLight && (!std::isfinite(Light.Range) || Light.Range <= 0.0f)) || (Spot && (!std::isfinite(Light.InnerConeAngleDegrees) || !std::isfinite(Light.OuterConeAngleDegrees) || Light.InnerConeAngleDegrees < 0.0f || Light.OuterConeAngleDegrees <= 0.0f || Light.InnerConeAngleDegrees > Light.OuterConeAngleDegrees || Light.OuterConeAngleDegrees >= 90.0f)))
			{
				SetDiagnostic(OutDiagnostic, "Scene light has invalid IDs, transform, radiometry, "
											 "range or cone angles");
				return false;
			}
		}
		xr_vector<FEditorOwnedParticleInstance> ParticleCopies;
		ParticleCopies.reserve(Snapshot.ParticleInstances.size());
		for (const FEditorParticleInstance& Particle :
			 Snapshot.ParticleInstances)
		{
			const bool ValidType =
				Particle.AssetType == EEditorParticleAssetType::Effect ||
				Particle.AssetType == EEditorParticleAssetType::Group;
			if (!ValidType || !Particle.ObjectId.IsValid() ||
				!SceneObjectIds.insert(Particle.ObjectId.Value).second ||
				Particle.AssetName.empty() ||
				Particle.AssetName.size() > 1024 ||
				!IsFinite(Particle.LocalToWorld))
			{
				SetDiagnostic(
					OutDiagnostic,
					"Particle instance has an invalid asset, ID or transform"
				);
				return false;
			}
			ParticleCopies.push_back({
				Particle.ObjectId,
				xr_string(Particle.AssetName),
				Particle.AssetType,
				Particle.LocalToWorld,
				Particle.Flags
			});
		}
		const bool DebugDrawChanged =
			Snapshot.DebugDrawRevision != AcceptedDebugDrawRevision;
		if ((!Snapshot.DebugLines.empty() || !Snapshot.DebugTriangles.empty() ||
			 !Snapshot.OverlayLines.empty() ||
			 !Snapshot.OverlayTriangles.empty() ||
			 !Snapshot.OverlayText.empty()) &&
			Snapshot.DebugDrawRevision == 0)
		{
			SetDiagnostic(OutDiagnostic, "Non-empty editor debug draw has no revision");
			return false;
		}
		if (DebugDrawChanged)
		{
			for (const FEditorDebugLine& Line : Snapshot.DebugLines)
			{
				for (const FEditorDebugVertex& Vertex : Line.Vertices)
				{
					if (!ValidateDebugVertex(Vertex))
					{
						SetDiagnostic(OutDiagnostic, "Editor debug line contains a non-finite vertex");
						return false;
					}
				}
			}
			for (const FEditorDebugTriangle& Triangle : Snapshot.DebugTriangles)
			{
				for (const FEditorDebugVertex& Vertex : Triangle.Vertices)
				{
					if (!ValidateDebugVertex(Vertex))
					{
						SetDiagnostic(OutDiagnostic, "Editor debug triangle contains a non-finite vertex");
						return false;
					}
				}
			}
			for (const FEditorOverlayLine& Line : Snapshot.OverlayLines)
			{
				for (const FEditorOverlayVertex& Vertex : Line.Vertices)
				{
					if (!ValidateOverlayVertex(Vertex))
					{
						SetDiagnostic(OutDiagnostic, "Editor overlay line contains an invalid vertex");
						return false;
					}
				}
			}
			for (const FEditorOverlayTriangle& Triangle : Snapshot.OverlayTriangles)
			{
				for (const FEditorOverlayVertex& Vertex : Triangle.Vertices)
				{
					if (!ValidateOverlayVertex(Vertex))
					{
						SetDiagnostic(OutDiagnostic, "Editor overlay triangle contains an invalid vertex");
						return false;
					}
				}
			}
			for (const FEditorOverlayText& Text : Snapshot.OverlayText)
			{
				if (!ValidateOverlayText(Text))
				{
					SetDiagnostic(OutDiagnostic, "Editor overlay text is empty, oversized or non-finite");
					return false;
				}
			}
		}

		std::scoped_lock Lock(Mutex);
		xr_hash_set<u64> AvailableMeshIds;
		AvailableMeshIds.reserve(AcceptedMeshRevisions.size() + MeshCopies.size());
		for (const auto& [MeshId, Revision] : AcceptedMeshRevisions)
		{
			(void)Revision;
			AvailableMeshIds.insert(MeshId);
		}
		for (const FEditorStaticMeshId Removed : Snapshot.RemovedStaticMeshes)
		{
			AvailableMeshIds.erase(Removed.Value);
		}
		for (const FEditorOwnedStaticMeshUpload& Mesh : MeshCopies)
		{
			AvailableMeshIds.insert(Mesh.MeshId.Value);
		}
		for (const FEditorStaticMeshInstance& Instance : Snapshot.Instances)
		{
			if (!AvailableMeshIds.contains(Instance.MeshId.Value))
			{
				SetDiagnostic(OutDiagnostic, "Scene instance references a mesh that was not submitted");
				return false;
			}

			const FEditorOwnedStaticMeshUpload* SubmittedMesh = nullptr;
			for (const FEditorOwnedStaticMeshUpload& Mesh : MeshCopies)
			{
				if (Mesh.MeshId == Instance.MeshId)
				{
					SubmittedMesh = &Mesh;
					break;
				}
			}
			if (SubmittedMesh)
			{
				xr_hash_set<u64> MeshMaterialSlots;
				for (const FEditorStaticMeshSection& Section : SubmittedMesh->Sections)
				{
					MeshMaterialSlots.insert(Section.MaterialSlot.Value);
					if (!SubmittedMaterialIds.contains(Section.MaterialSlot.Value))
					{
						SetDiagnostic(OutDiagnostic, "Scene instance references a material slot absent from the snapshot");
						return false;
					}
				}
				for (const FEditorMaterialSlotOverride& Override :
					 Instance.MaterialOverrides)
				{
					if (!MeshMaterialSlots.contains(
							Override.BaseMaterialSlot.Value
						))
					{
						SetDiagnostic(OutDiagnostic, "Scene instance overrides a slot absent from its mesh");
						return false;
					}
				}
			}
			else
			{
				const auto CachedSlots = AcceptedMeshMaterialSlots.find(
					Instance.MeshId.Value
				);
				if (CachedSlots == AcceptedMeshMaterialSlots.end())
				{
					SetDiagnostic(OutDiagnostic, "Scene instance references a mesh without material metadata");
					return false;
				}
				for (const u64 Slot : CachedSlots->second)
				{
					if (!SubmittedMaterialIds.contains(Slot))
					{
						SetDiagnostic(OutDiagnostic, "Cached mesh material slot is absent from the snapshot");
						return false;
					}
				}
				for (const FEditorMaterialSlotOverride& Override :
					 Instance.MaterialOverrides)
				{
					if (std::ranges::find(CachedSlots->second, Override.BaseMaterialSlot.Value) ==
						CachedSlots->second.end())
					{
						SetDiagnostic(OutDiagnostic, "Scene instance overrides a slot absent from its "
													 "cached mesh");
						return false;
					}
				}
			}
		}

		// Validation above is transactional: shared state is changed only after
		// the complete snapshot is known to be self-consistent.
		for (const FEditorStaticMeshId Removed : Snapshot.RemovedStaticMeshes)
		{
			AcceptedMeshRevisions.erase(Removed.Value);
			AcceptedMeshMaterialSlots.erase(Removed.Value);
			PendingMeshUpdates.erase(Removed.Value);
			PendingRemovedMeshes.insert(Removed.Value);
		}

		for (FEditorOwnedStaticMeshUpload& Mesh : MeshCopies)
		{
			const auto Existing = AcceptedMeshRevisions.find(Mesh.MeshId.Value);
			if (Existing != AcceptedMeshRevisions.end() &&
				Existing->second == Mesh.Revision)
			{
				continue;
			}
			AcceptedMeshRevisions[Mesh.MeshId.Value] = Mesh.Revision;
			xr_vector<u64>& MaterialSlots =
				AcceptedMeshMaterialSlots[Mesh.MeshId.Value];
			MaterialSlots.clear();
			MaterialSlots.reserve(Mesh.Sections.size());
			for (const FEditorStaticMeshSection& Section : Mesh.Sections)
			{
				MaterialSlots.push_back(Section.MaterialSlot.Value);
			}
			PendingRemovedMeshes.erase(Mesh.MeshId.Value);
			PendingMeshUpdates[Mesh.MeshId.Value] = std::move(Mesh);
		}

		PendingCamera = Snapshot.Camera;
		PendingMaterialSlots = std::move(MaterialCopies);
		PendingInstances.assign(Snapshot.Instances.begin(), Snapshot.Instances.end());
		PendingDecalInstances.assign(
			Snapshot.DecalInstances.begin(),
			Snapshot.DecalInstances.end()
		);
		PendingModelInstances = std::move(ModelCopies);
		PendingLights.assign(Snapshot.Lights.begin(), Snapshot.Lights.end());
		PendingParticleInstances = std::move(ParticleCopies);
		if (DebugDrawChanged)
		{
			PendingDebugLines.assign(
				Snapshot.DebugLines.begin(),
				Snapshot.DebugLines.end()
			);
			PendingDebugTriangles.assign(
				Snapshot.DebugTriangles.begin(),
				Snapshot.DebugTriangles.end()
			);
			PendingOverlayLines.assign(
				Snapshot.OverlayLines.begin(),
				Snapshot.OverlayLines.end()
			);
			PendingOverlayTriangles.assign(
				Snapshot.OverlayTriangles.begin(),
				Snapshot.OverlayTriangles.end()
			);
			PendingOverlayText.assign(
				Snapshot.OverlayText.begin(),
				Snapshot.OverlayText.end()
			);
			PendingDebugDrawChanged = true;
		}
		AcceptedDebugDrawRevision = Snapshot.DebugDrawRevision;
		PendingDebugDrawRevision = Snapshot.DebugDrawRevision;
		PendingRevision = Snapshot.Revision;
		HasPendingScene = true;
		return true;
	}

	[[nodiscard]] bool Consume(FEditorOwnedViewportScenePacket& OutPacket)
	{
		std::scoped_lock Lock(Mutex);
		if (!HasPendingScene)
		{
			return false;
		}

		if (PendingDebugDrawChanged)
		{
			OutPacket = {};
		}
		else
		{
			auto DebugLines = std::move(OutPacket.DebugLines);
			auto DebugTriangles = std::move(OutPacket.DebugTriangles);
			auto OverlayLines = std::move(OutPacket.OverlayLines);
			auto OverlayTriangles =
				std::move(OutPacket.OverlayTriangles);
			auto OverlayText = std::move(OutPacket.OverlayText);
			OutPacket = {};
			OutPacket.DebugLines = std::move(DebugLines);
			OutPacket.DebugTriangles = std::move(DebugTriangles);
			OutPacket.OverlayLines = std::move(OverlayLines);
			OutPacket.OverlayTriangles = std::move(OverlayTriangles);
			OutPacket.OverlayText = std::move(OverlayText);
		}
		OutPacket.Camera = PendingCamera;
		OutPacket.MaterialSlots = std::move(PendingMaterialSlots);
		OutPacket.Revision = PendingRevision;
		OutPacket.Instances = std::move(PendingInstances);
		OutPacket.DecalInstances = std::move(PendingDecalInstances);
		OutPacket.ModelInstances = std::move(PendingModelInstances);
		OutPacket.SourceInstanceCount = static_cast<u32>(
			OutPacket.Instances.size()
		);
		OutPacket.Lights = std::move(PendingLights);
		OutPacket.ParticleInstances =
			std::move(PendingParticleInstances);
		if (PendingDebugDrawChanged)
		{
			OutPacket.DebugLines = std::move(PendingDebugLines);
			OutPacket.DebugTriangles =
				std::move(PendingDebugTriangles);
			OutPacket.OverlayLines = std::move(PendingOverlayLines);
			OutPacket.OverlayTriangles =
				std::move(PendingOverlayTriangles);
			OutPacket.OverlayText = std::move(PendingOverlayText);
		}
		OutPacket.DebugDrawRevision = PendingDebugDrawRevision;
		OutPacket.StaticMeshUpdates.reserve(PendingMeshUpdates.size());
		for (auto& [MeshId, Mesh] : PendingMeshUpdates)
		{
			(void)MeshId;
			OutPacket.StaticMeshUpdates.push_back(std::move(Mesh));
		}
		for (const u64 MeshId : PendingRemovedMeshes)
		{
			OutPacket.RemovedStaticMeshes.push_back({MeshId});
		}

		PendingMeshUpdates.clear();
		PendingRemovedMeshes.clear();
		PendingDebugDrawChanged = false;
		HasPendingScene = false;
		return true;
	}

private:
	static void SetDiagnostic(xr_string* OutDiagnostic, const char* Text)
	{
		if (OutDiagnostic)
		{
			*OutDiagnostic = Text;
		}
	}

	template <size_t Size>
	[[nodiscard]] static bool IsFinite(const xr_array<float, Size>& Values)
	{
		for (const float Value : Values)
		{
			if (!std::isfinite(Value))
			{
				return false;
			}
		}
		return true;
	}

	[[nodiscard]] static bool ValidateCamera(const FEditorViewportCamera& Camera, xr_string* OutDiagnostic)
	{
		if (!IsFinite(Camera.View) || !IsFinite(Camera.Projection) ||
			!IsFinite(Camera.ViewProjection) || !IsFinite(Camera.WorldPosition) ||
			!std::isfinite(Camera.NearPlane) || !std::isfinite(Camera.FarPlane) ||
			Camera.NearPlane <= 0.0f || Camera.FarPlane <= Camera.NearPlane)
		{
			SetDiagnostic(OutDiagnostic, "Viewport camera is invalid");
			return false;
		}
		return true;
	}

	[[nodiscard]] static bool ValidateDebugVertex(
		const FEditorDebugVertex& Vertex
	)
	{
		return IsFinite(Vertex.Position) && IsFinite(Vertex.Color);
	}

	[[nodiscard]] static bool ValidateOverlayVertex(
		const FEditorOverlayVertex& Vertex
	)
	{
		// Off-screen coordinates are legal and are clipped by the rasterizer.
		return IsFinite(Vertex.Position) && IsFinite(Vertex.Color);
	}

	[[nodiscard]] static bool ValidateOverlayText(
		const FEditorOverlayText& Text
	)
	{
		constexpr size_t MaxOverlayTextLength = 4096;
		return !Text.Text.empty() && Text.Text.size() <= MaxOverlayTextLength &&
			   IsFinite(Text.Position) && IsFinite(Text.Color) &&
			   IsFinite(Text.ShadowColor);
	}

	[[nodiscard]] static bool ValidateMesh(const FEditorStaticMeshUpload& Mesh, xr_hash_set<u64>& SubmittedIds, const xr_hash_set<u64>& SubmittedMaterialIds, xr_string* OutDiagnostic)
	{
		if (!Mesh.MeshId.IsValid() || Mesh.Revision == 0 || Mesh.Vertices.empty() ||
			Mesh.Indices.empty() || Mesh.Indices.size() % 3 != 0 ||
			!SubmittedIds.insert(Mesh.MeshId.Value).second)
		{
			SetDiagnostic(OutDiagnostic, "Static mesh has an invalid ID, revision, topology or duplicate ID");
			return false;
		}
		for (const FEditorStaticMeshVertex& Vertex : Mesh.Vertices)
		{
			if (!IsFinite(Vertex.Position) || !IsFinite(Vertex.Normal) ||
				!IsFinite(Vertex.TexCoord))
			{
				SetDiagnostic(OutDiagnostic, "Static mesh contains a non-finite vertex");
				return false;
			}
		}
		for (const u32 Index : Mesh.Indices)
		{
			if (Index >= Mesh.Vertices.size())
			{
				SetDiagnostic(OutDiagnostic, "Static mesh index is out of range");
				return false;
			}
		}
		u64 ExpectedFirstIndex = 0;
		for (const FEditorStaticMeshSection& Section : Mesh.Sections)
		{
			const u64 End = static_cast<u64>(Section.FirstIndex) +
							Section.IndexCount;
			if (!Section.MaterialSlot.IsValid() ||
				!SubmittedMaterialIds.contains(Section.MaterialSlot.Value) ||
				Section.FirstIndex != ExpectedFirstIndex || Section.IndexCount == 0 ||
				Section.IndexCount % 3 != 0 || End > Mesh.Indices.size())
			{
				SetDiagnostic(OutDiagnostic, "Static mesh section has an invalid material or index range");
				return false;
			}
			ExpectedFirstIndex = End;
		}
		if (ExpectedFirstIndex != Mesh.Indices.size())
		{
			SetDiagnostic(OutDiagnostic, "Static mesh sections do not cover the complete index buffer");
			return false;
		}
		return true;
	}

	std::mutex Mutex;
	xr_hash_map<u64, u64> AcceptedMeshRevisions;
	xr_hash_map<u64, xr_vector<u64>>
		AcceptedMeshMaterialSlots;
	xr_hash_map<u64, FEditorOwnedStaticMeshUpload> PendingMeshUpdates;
	xr_hash_set<u64> PendingRemovedMeshes;
	FEditorViewportCamera PendingCamera;
	xr_vector<FEditorOwnedMaterialSlotSource> PendingMaterialSlots;
	xr_vector<FEditorStaticMeshInstance> PendingInstances;
	xr_vector<FEditorDecalInstance> PendingDecalInstances;
	xr_vector<FEditorOwnedModelInstance> PendingModelInstances;
	xr_vector<FEditorSceneLight> PendingLights;
	xr_vector<FEditorOwnedParticleInstance> PendingParticleInstances;
	xr_vector<FEditorDebugLine> PendingDebugLines;
	xr_vector<FEditorDebugTriangle> PendingDebugTriangles;
	xr_vector<FEditorOverlayLine> PendingOverlayLines;
	xr_vector<FEditorOverlayTriangle> PendingOverlayTriangles;
	xr_vector<FEditorOverlayText> PendingOverlayText;
	u64 PendingDebugDrawRevision = 0;
	u64 AcceptedDebugDrawRevision = 0;
	bool PendingDebugDrawChanged = false;
	u64 PendingRevision = 0;
	bool HasPendingScene = false;
};
