#include "TiramisuEditorNativeScene.h"

#include "../../../TiramisuMaterialEditor/MaterialEditorFileIO.h"

#include <MaterialTypes.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <cmath>
#include <limits>
#include <ranges>
#include <utility>

namespace
{
using FSelectionPoint = xr_array<float, 3>;

xr_string FormatDiagnostics(
	const xr_vector<Tiramisu::Scene::FSceneDiagnostic>& Diagnostics
)
{
	xr_string Result;
	for (const Tiramisu::Scene::FSceneDiagnostic& Diagnostic : Diagnostics)
	{
		if (!Result.empty())
		{
			Result += '\n';
		}
		Result += Diagnostic.Code + ": " + Diagnostic.Message;
	}
	return Result;
}

bool IsFiniteTransform(const xr_array<float, 16>& Transform)
{
	return std::ranges::all_of(Transform, [](const float Value)
							   { return std::isfinite(Value); });
}

bool IsValidSelectionFrustum(
	const FEditorNativeSceneSelectionFrustum& Frustum
)
{
	if (Frustum.Planes.empty() || Frustum.Planes.size() > 12)
	{
		return false;
	}
	for (const FEditorNativeSceneSelectionPlane& Plane : Frustum.Planes)
	{
		if (!std::isfinite(Plane.Distance) ||
			!std::ranges::all_of(Plane.Normal, [](const float Value)
								 { return std::isfinite(Value); }))
		{
			return false;
		}
		const float LengthSquared =
			Plane.Normal[0] * Plane.Normal[0] +
			Plane.Normal[1] * Plane.Normal[1] +
			Plane.Normal[2] * Plane.Normal[2];
		if (LengthSquared <= std::numeric_limits<float>::epsilon())
		{
			return false;
		}
	}
	return true;
}

float Classify(const FEditorNativeSceneSelectionPlane& Plane, const FSelectionPoint& Point)
{
	return Plane.Normal[0] * Point[0] +
		   Plane.Normal[1] * Point[1] +
		   Plane.Normal[2] * Point[2] + Plane.Distance;
}

FSelectionPoint TransformPosition(const xr_array<float, 16>& Transform, const FSelectionPoint& Position)
{
	return {
		Position[0] * Transform[0] + Position[1] * Transform[4] +
			Position[2] * Transform[8] + Transform[12],
		Position[0] * Transform[1] + Position[1] * Transform[5] +
			Position[2] * Transform[9] + Transform[13],
		Position[0] * Transform[2] + Position[1] * Transform[6] +
			Position[2] * Transform[10] + Transform[14]
	};
}

struct FMeshBounds
{
	FSelectionPoint Minimum = {};
	FSelectionPoint Maximum = {};
	bool Valid = false;
};

FMeshBounds CalculateBounds(
	const Tiramisu::Scene::FStaticMeshAsset& Mesh
)
{
	FMeshBounds Bounds;
	if (Mesh.Vertices.empty())
	{
		return Bounds;
	}
	Bounds.Minimum = Mesh.Vertices.front().Position;
	Bounds.Maximum = Bounds.Minimum;
	for (const Tiramisu::Scene::FStaticMeshVertex& Vertex : Mesh.Vertices)
	{
		for (size_t Axis = 0; Axis < 3; ++Axis)
		{
			Bounds.Minimum[Axis] =
				std::min(Bounds.Minimum[Axis], Vertex.Position[Axis]);
			Bounds.Maximum[Axis] =
				std::max(Bounds.Maximum[Axis], Vertex.Position[Axis]);
		}
	}
	Bounds.Valid = true;
	return Bounds;
}

enum class EBoundsFrustumOverlap
{
	Outside,
	Partial,
	Inside
};

EBoundsFrustumOverlap TestBounds(
	const FMeshBounds& Bounds,
	const xr_array<float, 16>& Transform,
	const FEditorNativeSceneSelectionFrustum& Frustum
)
{
	if (!Bounds.Valid)
	{
		return EBoundsFrustumOverlap::Outside;
	}
	xr_array<FSelectionPoint, 8> Corners;
	for (size_t Corner = 0; Corner < Corners.size(); ++Corner)
	{
		const FSelectionPoint Local = {
			(Corner & 1) ? Bounds.Maximum[0] : Bounds.Minimum[0],
			(Corner & 2) ? Bounds.Maximum[1] : Bounds.Minimum[1],
			(Corner & 4) ? Bounds.Maximum[2] : Bounds.Minimum[2]
		};
		Corners[Corner] = TransformPosition(Transform, Local);
	}

	bool FullyInside = true;
	for (const FEditorNativeSceneSelectionPlane& Plane : Frustum.Planes)
	{
		size_t OutsideCount = 0;
		for (const FSelectionPoint& Corner : Corners)
		{
			OutsideCount += Classify(Plane, Corner) > 0.0f ? 1 : 0;
		}
		if (OutsideCount == Corners.size())
		{
			return EBoundsFrustumOverlap::Outside;
		}
		FullyInside &= OutsideCount == 0;
	}
	return FullyInside ? EBoundsFrustumOverlap::Inside
					   : EBoundsFrustumOverlap::Partial;
}

bool TriangleIntersectsFrustum(
	const xr_array<FSelectionPoint, 3>& Triangle,
	const FEditorNativeSceneSelectionFrustum& Frustum
)
{
	xr_vector<FSelectionPoint> Input(
		Triangle.begin(), Triangle.end()
	);
	xr_vector<FSelectionPoint> Output;
	Output.reserve(12);
	for (const FEditorNativeSceneSelectionPlane& Plane : Frustum.Planes)
	{
		Output.clear();
		if (Input.empty())
		{
			return false;
		}
		FSelectionPoint Previous = Input.back();
		float PreviousDistance = Classify(Plane, Previous);
		bool PreviousInside = PreviousDistance <= 0.0f;
		for (const FSelectionPoint& Current : Input)
		{
			const float CurrentDistance = Classify(Plane, Current);
			const bool CurrentInside = CurrentDistance <= 0.0f;
			if (CurrentInside != PreviousInside)
			{
				const float Denominator =
					PreviousDistance - CurrentDistance;
				if (std::abs(Denominator) >
					std::numeric_limits<float>::epsilon())
				{
					const float T = PreviousDistance / Denominator;
					FSelectionPoint Intersection;
					for (size_t Axis = 0; Axis < 3; ++Axis)
					{
						Intersection[Axis] = Previous[Axis] +
											 (Current[Axis] - Previous[Axis]) * T;
					}
					Output.push_back(Intersection);
				}
			}
			if (CurrentInside)
			{
				Output.push_back(Current);
			}
			Previous = Current;
			PreviousDistance = CurrentDistance;
			PreviousInside = CurrentInside;
		}
		Input.swap(Output);
	}
	return Input.size() >= 3;
}

bool MeshIntersectsFrustum(
	const Tiramisu::Scene::FStaticMeshAsset& Mesh,
	const FMeshBounds& Bounds,
	const xr_array<float, 16>& Transform,
	const FEditorNativeSceneSelectionFrustum& Frustum
)
{
	const EBoundsFrustumOverlap BoundsOverlap =
		TestBounds(Bounds, Transform, Frustum);
	if (BoundsOverlap == EBoundsFrustumOverlap::Outside)
	{
		return false;
	}
	if (BoundsOverlap == EBoundsFrustumOverlap::Inside)
	{
		return true;
	}

	for (const Tiramisu::Scene::FStaticMeshSection& Section : Mesh.Sections)
	{
		const u64 SectionEnd = std::min<u64>(
			static_cast<u64>(Section.FirstIndex) +
				Section.IndexCount,
			Mesh.Indices.size()
		);
		for (u64 Index = Section.FirstIndex;
			 Index + 2 < SectionEnd;
			 Index += 3)
		{
			const u32 I0 = Mesh.Indices[Index];
			const u32 I1 = Mesh.Indices[Index + 1];
			const u32 I2 = Mesh.Indices[Index + 2];
			if (I0 >= Mesh.Vertices.size() ||
				I1 >= Mesh.Vertices.size() ||
				I2 >= Mesh.Vertices.size())
			{
				continue;
			}
			const xr_array<FSelectionPoint, 3> Triangle = {
				TransformPosition(Transform, Mesh.Vertices[I0].Position),
				TransformPosition(Transform, Mesh.Vertices[I1].Position),
				TransformPosition(Transform, Mesh.Vertices[I2].Position)
			};
			if (TriangleIntersectsFrustum(Triangle, Frustum))
			{
				return true;
			}
		}
	}
	return false;
}
} // namespace

void TiramisuEditorNativeSceneDocument::NewRenderScene(
	const xr_string_view Name
)
{
	static std::atomic_uint64_t Sequence = 0;
	const u64 UniqueValue =
		static_cast<u64>(
			std::chrono::system_clock::now().time_since_epoch().count()
		) ^
		Sequence.fetch_add(1, std::memory_order_relaxed);
	Scene = {};
	Scene.Scene.Id =
		GenerateDeterministicMaterialGuid(
			"editor-native-render-scene",
			xr_string(Name) + "|" + ToXrString(std::to_string(UniqueValue))
		);
	Scene.Scene.Name = Name.empty()
						   ? "Untitled Render Scene"
						   : xr_string(Name);
	SourcePath.clear();
	SelectedComponents.clear();
	UndoStack.clear();
	RedoStack.clear();
	TransactionBaseline.reset();
	SavedSceneJson.clear();
	EditableRenderScene = true;
	TransactionChanged = false;
	Dirty = true;
	++Revision;
	Open = true;
}

bool TiramisuEditorNativeSceneDocument::OpenStaticMesh(
	const std::filesystem::path& Path, xr_string& Diagnostic
)
{
	Diagnostic.clear();
	Tiramisu::Scene::FStaticMeshAssetParseResult Parsed =
		Tiramisu::Scene::LoadStaticMeshAsset(Path);
	if (!Parsed.Succeeded())
	{
		Diagnostic = FormatDiagnostics(Parsed.Diagnostics);
		return false;
	}

	Tiramisu::Scene::FResolvedRenderScene Candidate;
	Candidate.Scene.Id =
		GenerateDeterministicMaterialGuid(
			"editor-static-mesh-preview-scene", Parsed.Value.Id
		);
	Candidate.Scene.Name = Parsed.Value.Name;
	Candidate.Scene.SourcePath = Path.generic_string();
	Tiramisu::Scene::FStaticMeshComponent Component;
	Component.Id = GenerateDeterministicMaterialGuid(
		"editor-static-mesh-preview-component", Parsed.Value.Id
	);
	Component.Name = Parsed.Value.Name;
	Component.StaticMesh = Path.generic_string();
	Candidate.Scene.StaticMeshComponents.push_back(std::move(Component));
	Candidate.StaticMeshes.emplace(Path.generic_string(), std::move(Parsed.Value));

	Scene = std::move(Candidate);
	SourcePath = Path.lexically_normal();
	SelectedComponents.clear();
	UndoStack.clear();
	RedoStack.clear();
	TransactionBaseline.reset();
	SavedSceneJson.clear();
	EditableRenderScene = false;
	TransactionChanged = false;
	Dirty = false;
	++Revision;
	Open = true;
	return true;
}

bool TiramisuEditorNativeSceneDocument::OpenRenderScene(
	const std::filesystem::path& Path, xr_string& Diagnostic
)
{
	Diagnostic.clear();
	Tiramisu::Scene::FResolvedRenderSceneResult Loaded =
		Tiramisu::Scene::LoadRenderSceneAsset(Path);
	if (!Loaded.Succeeded())
	{
		Diagnostic = FormatDiagnostics(Loaded.Diagnostics);
		return false;
	}
	Scene = std::move(Loaded.Value);
	SourcePath = Path.lexically_normal();
	SelectedComponents.clear();
	UndoStack.clear();
	RedoStack.clear();
	TransactionBaseline.reset();
	SavedSceneJson =
		Tiramisu::Scene::SerializeRenderSceneAssetJson(Scene.Scene);
	EditableRenderScene = true;
	TransactionChanged = false;
	Dirty = false;
	++Revision;
	Open = true;
	return true;
}

void TiramisuEditorNativeSceneDocument::Close() noexcept
{
	Scene = {};
	SourcePath.clear();
	SelectedComponents.clear();
	UndoStack.clear();
	RedoStack.clear();
	TransactionBaseline.reset();
	SavedSceneJson.clear();
	++Revision;
	Open = false;
	EditableRenderScene = false;
	TransactionChanged = false;
	Dirty = false;
}

bool TiramisuEditorNativeSceneDocument::IsOpen() const noexcept
{
	return Open;
}

bool TiramisuEditorNativeSceneDocument::IsEditableRenderScene() const noexcept
{
	return Open && EditableRenderScene;
}

bool TiramisuEditorNativeSceneDocument::IsDirty() const noexcept
{
	return Dirty;
}

const Tiramisu::Scene::FResolvedRenderScene*
TiramisuEditorNativeSceneDocument::GetScene() const noexcept
{
	return Open ? &Scene : nullptr;
}

bool TiramisuEditorNativeSceneDocument::IsComponentSelected(
	const xr_string_view ComponentId
) const
{
	return SelectedComponents.contains(xr_string(ComponentId));
}

size_t TiramisuEditorNativeSceneDocument::GetSelectionCount() const noexcept
{
	return SelectedComponents.size();
}

xr_optional<FEditorNativeSceneComponentDetails>
TiramisuEditorNativeSceneDocument::GetSingleSelectedComponentDetails() const
{
	if (!Open || SelectedComponents.size() != 1)
	{
		return std::nullopt;
	}
	const xr_string& SelectedId = *SelectedComponents.begin();
	const auto Component = std::ranges::find(
		Scene.Scene.StaticMeshComponents, SelectedId, &Tiramisu::Scene::FStaticMeshComponent::Id
	);
	if (Component == Scene.Scene.StaticMeshComponents.end())
	{
		return std::nullopt;
	}
	const auto Mesh = Scene.StaticMeshes.find(Component->StaticMesh);
	if (Mesh == Scene.StaticMeshes.end())
	{
		return std::nullopt;
	}

	FEditorNativeSceneComponentDetails Details;
	Details.Id = Component->Id;
	Details.Name = Component->Name;
	Details.StaticMesh = Component->StaticMesh;
	Details.Position = {Component->LocalToWorld[12], Component->LocalToWorld[13], Component->LocalToWorld[14]};
	Details.Visible = Component->Visible;
	Details.MaterialSlots.reserve(Mesh->second.MaterialSlots.size());
	for (size_t Index = 0;
		 Index < Mesh->second.MaterialSlots.size();
		 ++Index)
	{
		const Tiramisu::Scene::FStaticMeshMaterialSlot& Base =
			Mesh->second.MaterialSlots[Index];
		FEditorNativeSceneMaterialSlotDetails Slot;
		Slot.MaterialSlot = static_cast<u32>(Index);
		Slot.Name = Base.Name;
		Slot.BaseMaterial = Base.Material;
		Slot.BaseTwoSided = Base.TwoSided;
		const auto Override = std::ranges::find(
			Component->MaterialOverrides, Slot.MaterialSlot, &Tiramisu::Scene::FStaticMeshMaterialOverride::MaterialSlot
		);
		if (Override != Component->MaterialOverrides.end())
		{
			Slot.HasOverride = true;
			Slot.OverrideMaterial = Override->Material;
			Slot.OverrideTwoSided = Override->TwoSided;
		}
		Details.MaterialSlots.push_back(std::move(Slot));
	}
	return Details;
}

xr_optional<FEditorNativeSceneLightDetails>
TiramisuEditorNativeSceneDocument::GetSingleSelectedLightDetails() const
{
	if (!Open || SelectedComponents.size() != 1)
	{
		return std::nullopt;
	}
	const xr_string& SelectedId = *SelectedComponents.begin();
	const auto Light = std::ranges::find(
		Scene.Scene.LightComponents, SelectedId, &Tiramisu::Scene::FLightComponent::Id
	);
	if (Light == Scene.Scene.LightComponents.end())
	{
		return std::nullopt;
	}

	FEditorNativeSceneLightDetails Details;
	Details.Id = Light->Id;
	Details.Name = Light->Name;
	Details.Type = Light->Type;
	Details.Position = {
		Light->LocalToWorld[12],
		Light->LocalToWorld[13],
		Light->LocalToWorld[14]
	};
	Details.Color = Light->Color;
	Details.Intensity = Light->Intensity;
	Details.Range = Light->Range;
	Details.InnerConeAngleDegrees = Light->InnerConeAngleDegrees;
	Details.OuterConeAngleDegrees = Light->OuterConeAngleDegrees;
	Details.Visible = Light->Visible;
	Details.CastShadows = Light->CastShadows;
	return Details;
}

xr_optional<FEditorNativeSceneBulkMaterialDetails>
TiramisuEditorNativeSceneDocument::GetSelectedComponentsMaterialDetails() const
{
	if (!Open || SelectedComponents.empty())
	{
		return std::nullopt;
	}

	struct FSelectedComponent
	{
		const Tiramisu::Scene::FStaticMeshComponent* Component = nullptr;
		const Tiramisu::Scene::FStaticMeshAsset* Mesh = nullptr;
	};
	xr_vector<FSelectedComponent> Components;
	Components.reserve(SelectedComponents.size());
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!SelectedComponents.contains(Component.Id))
		{
			continue;
		}
		const auto Mesh = Scene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == Scene.StaticMeshes.end())
		{
			return std::nullopt;
		}
		Components.push_back({&Component, &Mesh->second});
	}
	if (Components.size() != SelectedComponents.size())
	{
		return std::nullopt;
	}

	size_t CommonSlotCount =
		std::numeric_limits<size_t>::max();
	for (const FSelectedComponent& Component : Components)
	{
		CommonSlotCount = std::min(CommonSlotCount, Component.Mesh->MaterialSlots.size());
	}

	FEditorNativeSceneBulkMaterialDetails Details;
	Details.ComponentCount = Components.size();
	Details.MaterialSlots.reserve(CommonSlotCount);
	for (size_t Index = 0; Index < CommonSlotCount; ++Index)
	{
		const Tiramisu::Scene::FStaticMeshMaterialSlot& FirstBase =
			Components.front().Mesh->MaterialSlots[Index];
		FEditorNativeSceneBulkMaterialSlotDetails Slot;
		Slot.MaterialSlot = static_cast<u32>(Index);
		Slot.ComponentCount = Components.size();
		Slot.Name = FirstBase.Name;
		Slot.BaseMaterial = FirstBase.Material;
		Slot.BaseTwoSided = FirstBase.TwoSided;

		bool HasFirstOverride = false;
		for (const FSelectedComponent& Selected : Components)
		{
			const Tiramisu::Scene::FStaticMeshMaterialSlot& Base =
				Selected.Mesh->MaterialSlots[Index];
			Slot.NameMixed |= Base.Name != Slot.Name;
			Slot.BaseMaterialMixed |=
				Base.Material != Slot.BaseMaterial;
			Slot.BaseTwoSidedMixed |=
				Base.TwoSided != Slot.BaseTwoSided;

			const auto Override = std::ranges::find(
				Selected.Component->MaterialOverrides,
				Slot.MaterialSlot,
				&Tiramisu::Scene::FStaticMeshMaterialOverride::
					MaterialSlot
			);
			if (Override ==
				Selected.Component->MaterialOverrides.end())
			{
				continue;
			}
			++Slot.OverrideCount;
			if (!HasFirstOverride)
			{
				Slot.OverrideMaterial = Override->Material;
				Slot.OverrideTwoSided = Override->TwoSided;
				HasFirstOverride = true;
			}
			else
			{
				Slot.OverrideMaterialMixed |=
					Override->Material != Slot.OverrideMaterial;
				Slot.OverrideTwoSidedMixed |=
					Override->TwoSided != Slot.OverrideTwoSided;
			}
		}
		if (Slot.OverrideCount != 0 &&
			Slot.OverrideCount != Slot.ComponentCount)
		{
			Slot.OverrideMaterialMixed = true;
			Slot.OverrideTwoSidedMixed = true;
		}
		Details.MaterialSlots.push_back(std::move(Slot));
	}
	return Details;
}

xr_optional<FEditorNativeSceneBounds>
TiramisuEditorNativeSceneDocument::GetWorldBounds(
	const bool SelectedOnly
) const
{
	if (!Open || (SelectedOnly && SelectedComponents.empty()))
	{
		return std::nullopt;
	}

	xr_hash_map<xr_string, FMeshBounds> BoundsByMesh;
	xr_optional<FEditorNativeSceneBounds> Result;
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!Component.Visible ||
			(SelectedOnly &&
			 !SelectedComponents.contains(Component.Id)))
		{
			continue;
		}
		const auto Mesh = Scene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == Scene.StaticMeshes.end())
		{
			return std::nullopt;
		}
		const auto [Bounds, Inserted] =
			BoundsByMesh.try_emplace(Component.StaticMesh);
		if (Inserted)
		{
			Bounds->second = CalculateBounds(Mesh->second);
		}
		if (!Bounds->second.Valid)
		{
			continue;
		}

		for (size_t Corner = 0; Corner < 8; ++Corner)
		{
			const FSelectionPoint Local = {
				(Corner & 1)
					? Bounds->second.Maximum[0]
					: Bounds->second.Minimum[0],
				(Corner & 2)
					? Bounds->second.Maximum[1]
					: Bounds->second.Minimum[1],
				(Corner & 4)
					? Bounds->second.Maximum[2]
					: Bounds->second.Minimum[2]
			};
			const FSelectionPoint World =
				TransformPosition(Component.LocalToWorld, Local);
			if (!std::ranges::all_of(World, [](const float Value)
									 { return std::isfinite(Value); }))
			{
				return std::nullopt;
			}
			if (!Result)
			{
				Result = FEditorNativeSceneBounds{World, World};
				continue;
			}
			for (size_t Axis = 0; Axis < 3; ++Axis)
			{
				Result->Minimum[Axis] =
					std::min(Result->Minimum[Axis], World[Axis]);
				Result->Maximum[Axis] =
					std::max(Result->Maximum[Axis], World[Axis]);
			}
		}
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (!Light.Visible ||
			(SelectedOnly && !SelectedComponents.contains(Light.Id)))
		{
			continue;
		}
		const FSelectionPoint Position = {
			Light.LocalToWorld[12],
			Light.LocalToWorld[13],
			Light.LocalToWorld[14]
		};
		if (!std::ranges::all_of(Position, [](const float Value)
								 { return std::isfinite(Value); }))
		{
			return std::nullopt;
		}
		constexpr float IconExtent = 0.5f;
		for (size_t Corner = 0; Corner < 8; ++Corner)
		{
			FSelectionPoint World = Position;
			for (size_t Axis = 0; Axis < 3; ++Axis)
			{
				World[Axis] += (Corner & (1 << Axis))
								   ? IconExtent
								   : -IconExtent;
			}
			if (!Result)
			{
				Result = FEditorNativeSceneBounds{World, World};
				continue;
			}
			for (size_t Axis = 0; Axis < 3; ++Axis)
			{
				Result->Minimum[Axis] =
					std::min(Result->Minimum[Axis], World[Axis]);
				Result->Maximum[Axis] =
					std::max(Result->Maximum[Axis], World[Axis]);
			}
		}
	}
	return Result;
}

void TiramisuEditorNativeSceneDocument::ClearSelection()
{
	if (SelectedComponents.empty())
	{
		return;
	}
	SelectedComponents.clear();
	++Revision;
}

bool TiramisuEditorNativeSceneDocument::SelectObject(
	const u64 ObjectId,
	const EEditorNativeSceneSelectionMode Mode
)
{
	if (!Open || ObjectId == 0)
	{
		return false;
	}
	const Tiramisu::Scene::FStaticMeshComponent* Match = nullptr;
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (Tiramisu::Scene::StableSceneIdHash(Component.Id) != ObjectId)
		{
			continue;
		}
		// Treat an extremely unlikely stable-hash collision as an invalid
		// selection instead of editing the wrong component.
		if (Match)
		{
			return false;
		}
		Match = &Component;
	}
	xr_string MatchId = Match ? Match->Id : xr_string{};
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (Tiramisu::Scene::StableSceneIdHash(Light.Id) != ObjectId)
		{
			continue;
		}
		if (!MatchId.empty())
		{
			return false;
		}
		MatchId = Light.Id;
	}
	if (MatchId.empty())
	{
		return false;
	}
	const xr_array<xr_string, 1> ComponentIds = {MatchId};
	return SelectComponents(ComponentIds, Mode) == 1;
}

size_t TiramisuEditorNativeSceneDocument::SelectComponents(
	const xr_span<const xr_string> ComponentIds,
	const EEditorNativeSceneSelectionMode Mode
)
{
	if (!Open)
	{
		return 0;
	}
	const xr_hash_set<xr_string> Requested(
		ComponentIds.begin(), ComponentIds.end()
	);
	xr_hash_set<xr_string> Matches;
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (Requested.contains(Component.Id))
		{
			Matches.insert(Component.Id);
		}
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (Requested.contains(Light.Id))
		{
			Matches.insert(Light.Id);
		}
	}

	xr_hash_set<xr_string> Updated = SelectedComponents;
	switch (Mode)
	{
		case EEditorNativeSceneSelectionMode::Replace:
			Updated = Matches;
			break;
		case EEditorNativeSceneSelectionMode::Add:
			Updated.insert(Matches.begin(), Matches.end());
			break;
		case EEditorNativeSceneSelectionMode::Remove:
			for (const xr_string& MatchId : Matches)
			{
				Updated.erase(MatchId);
			}
			break;
		case EEditorNativeSceneSelectionMode::Toggle:
			for (const xr_string& MatchId : Matches)
			{
				if (Updated.erase(MatchId) == 0)
				{
					Updated.insert(MatchId);
				}
			}
			break;
	}
	if (Updated != SelectedComponents)
	{
		SelectedComponents = std::move(Updated);
		++Revision;
	}
	return Matches.size();
}

size_t TiramisuEditorNativeSceneDocument::SelectFrustum(
	const FEditorNativeSceneSelectionFrustum& Frustum,
	const EEditorNativeSceneSelectionMode Mode
)
{
	if (!Open || !IsValidSelectionFrustum(Frustum))
	{
		return 0;
	}

	xr_hash_map<xr_string, FMeshBounds> BoundsByMesh;
	xr_hash_set<xr_string> Matches;
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!Component.Visible)
		{
			continue;
		}
		const auto Mesh = Scene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == Scene.StaticMeshes.end())
		{
			continue;
		}
		const auto [Bounds, Inserted] =
			BoundsByMesh.try_emplace(Component.StaticMesh);
		if (Inserted)
		{
			Bounds->second = CalculateBounds(Mesh->second);
		}
		if (MeshIntersectsFrustum(Mesh->second, Bounds->second, Component.LocalToWorld, Frustum))
		{
			Matches.insert(Component.Id);
		}
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (!Light.Visible)
		{
			continue;
		}
		const FSelectionPoint Position = {
			Light.LocalToWorld[12],
			Light.LocalToWorld[13],
			Light.LocalToWorld[14]
		};
		if (std::ranges::all_of(Frustum.Planes, [&](const FEditorNativeSceneSelectionPlane& Plane)
								{ return Classify(Plane, Position) <= 0.0f; }))
		{
			Matches.insert(Light.Id);
		}
	}

	xr_hash_set<xr_string> Updated = SelectedComponents;
	switch (Mode)
	{
		case EEditorNativeSceneSelectionMode::Replace:
			Updated = Matches;
			break;
		case EEditorNativeSceneSelectionMode::Add:
			Updated.insert(Matches.begin(), Matches.end());
			break;
		case EEditorNativeSceneSelectionMode::Remove:
			for (const xr_string& Match : Matches)
			{
				Updated.erase(Match);
			}
			break;
		case EEditorNativeSceneSelectionMode::Toggle:
			for (const xr_string& Match : Matches)
			{
				if (Updated.erase(Match) == 0)
				{
					Updated.insert(Match);
				}
			}
			break;
	}
	if (Updated != SelectedComponents)
	{
		SelectedComponents = std::move(Updated);
		++Revision;
	}
	return Matches.size();
}

void TiramisuEditorNativeSceneDocument::SelectAll()
{
	if (!Open)
	{
		return;
	}
	xr_hash_set<xr_string> Selection;
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		Selection.insert(Component.Id);
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		Selection.insert(Light.Id);
	}
	if (Selection == SelectedComponents)
	{
		return;
	}
	SelectedComponents = std::move(Selection);
	++Revision;
}

void TiramisuEditorNativeSceneDocument::InvertSelection()
{
	if (!Open)
	{
		return;
	}
	xr_hash_set<xr_string> Selection;
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!SelectedComponents.contains(Component.Id))
		{
			Selection.insert(Component.Id);
		}
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (!SelectedComponents.contains(Light.Id))
		{
			Selection.insert(Light.Id);
		}
	}
	if (Selection == SelectedComponents)
	{
		return;
	}
	SelectedComponents = std::move(Selection);
	++Revision;
}

bool TiramisuEditorNativeSceneDocument::AddStaticMeshComponent(
	const std::filesystem::path& StaticMeshPath,
	const xr_array<float, 16>& LocalToWorld,
	xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene())
	{
		Diagnostic = "A native RenderScene must be open before adding a "
					 "StaticMesh component.";
		return false;
	}
	if (TransactionBaseline)
	{
		Diagnostic = "Cannot add a component during an active transform "
					 "transaction.";
		return false;
	}
	if (!IsFiniteTransform(LocalToWorld))
	{
		Diagnostic = "StaticMesh component transform is not finite.";
		return false;
	}
	const std::filesystem::path MeshPath =
		StaticMeshPath.lexically_normal();
	Tiramisu::Scene::FStaticMeshAssetParseResult Parsed =
		Tiramisu::Scene::LoadStaticMeshAsset(MeshPath);
	if (!Parsed.Succeeded())
	{
		Diagnostic = FormatDiagnostics(Parsed.Diagnostics);
		return false;
	}
	Parsed.Value.SourcePath = ToXrString(MeshPath.generic_string());
	std::error_code RelativeError;
	std::filesystem::path Reference = std::filesystem::relative(
		MeshPath, SourcePath.parent_path(), RelativeError
	);
	if (RelativeError || Reference.empty())
	{
		Reference = MeshPath;
	}
	const xr_string ReferenceText =
		Reference.lexically_normal().generic_string();

	Tiramisu::Scene::FStaticMeshComponent Component;
	Component.Name = Parsed.Value.Name.empty()
						 ? MeshPath.stem().string()
						 : Parsed.Value.Name;
	Component.StaticMesh = ReferenceText;
	Component.LocalToWorld = LocalToWorld;
	for (u32 Suffix = 0;; ++Suffix)
	{
		Component.Id =
			GenerateDeterministicMaterialGuid(
				"native-scene-static-mesh-component",
				Scene.Scene.Id + "|" + ToXrString(MeshPath.generic_string()) + "|" +
					ToXrString(std::to_string(Suffix))
			);
		if (std::ranges::none_of(
				Scene.Scene.StaticMeshComponents,
				[&](const Tiramisu::Scene::FStaticMeshComponent& Existing)
				{
					return Existing.Id == Component.Id;
				}
			) &&
			std::ranges::none_of(
				Scene.Scene.LightComponents,
				[&](const Tiramisu::Scene::FLightComponent& Existing)
				{
					return Existing.Id == Component.Id;
				}
			))
		{
			break;
		}
	}

	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	Scene.StaticMeshes.insert_or_assign(
		ReferenceText, std::move(Parsed.Value)
	);
	Scene.Scene.StaticMeshComponents.push_back(std::move(Component));
	UndoStack.push_back(Before);
	RedoStack.clear();
	SelectedComponents.clear();
	SelectedComponents.insert(
		Scene.Scene.StaticMeshComponents.back().Id
	);
	PublishSceneChange();
	return true;
}

bool TiramisuEditorNativeSceneDocument::AddLightComponent(
	const Tiramisu::Scene::ELightType Type,
	const xr_array<float, 16>& LocalToWorld,
	xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene())
	{
		Diagnostic = "A native RenderScene must be open before adding a "
					 "Light component.";
		return false;
	}
	if (TransactionBaseline)
	{
		Diagnostic = "Cannot add a light during an active transform "
					 "transaction.";
		return false;
	}
	if (!IsFiniteTransform(LocalToWorld))
	{
		Diagnostic = "Light component transform is not finite.";
		return false;
	}

	Tiramisu::Scene::FLightComponent Light;
	Light.Type = Type;
	Light.LocalToWorld = LocalToWorld;
	switch (Type)
	{
		case Tiramisu::Scene::ELightType::Directional:
			Light.Name = "Directional Light";
			break;
		case Tiramisu::Scene::ELightType::Point:
			Light.Name = "Point Light";
			break;
		case Tiramisu::Scene::ELightType::Spot:
			Light.Name = "Spot Light";
			break;
		default:
			Diagnostic = "Unsupported native light type.";
			return false;
	}

	xr_hash_set<xr_string> ExistingNames;
	for (const Tiramisu::Scene::FLightComponent& Existing :
		 Scene.Scene.LightComponents)
	{
		ExistingNames.insert(Existing.Name);
	}
	const xr_string BaseName = Light.Name;
	for (u64 Suffix = 2;
		 !ExistingNames.insert(Light.Name).second;
		 ++Suffix)
	{
		Light.Name = BaseName + " " + ToXrString(std::to_string(Suffix));
	}
	for (u64 Suffix = 0;; ++Suffix)
	{
		Light.Id =
			GenerateDeterministicMaterialGuid(
				"native-scene-light-component",
				Scene.Scene.Id + "|" +
					xr_string(Tiramisu::Scene::ToString(Type)) + "|" +
					ToXrString(std::to_string(Suffix))
			);
		const bool StaticCollision = std::ranges::any_of(
			Scene.Scene.StaticMeshComponents,
			[&](const Tiramisu::Scene::FStaticMeshComponent& Existing)
			{
				return Existing.Id == Light.Id;
			}
		);
		const bool LightCollision = std::ranges::any_of(
			Scene.Scene.LightComponents,
			[&](const Tiramisu::Scene::FLightComponent& Existing)
			{
				return Existing.Id == Light.Id;
			}
		);
		if (!StaticCollision && !LightCollision)
		{
			break;
		}
	}

	Tiramisu::Scene::FRenderSceneAsset Candidate = Scene.Scene;
	Candidate.Version = Tiramisu::Scene::RenderSceneAssetVersion;
	Candidate.LightComponents.push_back(Light);
	const auto Validation =
		Tiramisu::Scene::ParseRenderSceneAssetJson(
			Tiramisu::Scene::SerializeRenderSceneAssetJson(Candidate)
		);
	if (!Validation.Succeeded())
	{
		Diagnostic = FormatDiagnostics(Validation.Diagnostics);
		return false;
	}

	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	Scene.Scene.Version = Tiramisu::Scene::RenderSceneAssetVersion;
	Scene.Scene.LightComponents.push_back(std::move(Light));
	UndoStack.push_back(Before);
	RedoStack.clear();
	SelectedComponents.clear();
	SelectedComponents.insert(Scene.Scene.LightComponents.back().Id);
	PublishSceneChange();
	return true;
}

size_t TiramisuEditorNativeSceneDocument::DuplicateSelected(
	xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene() || TransactionBaseline ||
		SelectedComponents.empty())
	{
		Diagnostic = "At least one native component must be selected.";
		return 0;
	}

	xr_vector<Tiramisu::Scene::FStaticMeshComponent> MeshCopies;
	xr_vector<Tiramisu::Scene::FLightComponent> LightCopies;
	MeshCopies.reserve(SelectedComponents.size());
	LightCopies.reserve(SelectedComponents.size());
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (SelectedComponents.contains(Component.Id))
		{
			MeshCopies.push_back(Component);
		}
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (SelectedComponents.contains(Light.Id))
		{
			LightCopies.push_back(Light);
		}
	}
	const size_t CopyCount =
		MeshCopies.size() + LightCopies.size();
	if (CopyCount != SelectedComponents.size())
	{
		Diagnostic = "A selected native component no longer exists.";
		return 0;
	}

	xr_hash_set<xr_string> ExistingIds;
	xr_hash_set<xr_string> ExistingNames;
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		ExistingIds.insert(Component.Id);
		ExistingNames.insert(Component.Name);
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		ExistingIds.insert(Light.Id);
		ExistingNames.insert(Light.Name);
	}
	for (Tiramisu::Scene::FStaticMeshComponent& Copy : MeshCopies)
	{
		const xr_string SourceId = Copy.Id;
		for (u64 Suffix = 0;; ++Suffix)
		{
			Copy.Id =
				GenerateDeterministicMaterialGuid(
					"native-scene-static-mesh-component-duplicate",
					Scene.Scene.Id + "|" + SourceId + "|" +
						ToXrString(std::to_string(Suffix))
				);
			if (ExistingIds.insert(Copy.Id).second)
			{
				break;
			}
		}

		const xr_string BaseName = Copy.Name.empty()
									   ? "StaticMesh Copy"
									   : Copy.Name + " Copy";
		Copy.Name = BaseName;
		for (u64 Suffix = 2;
			 !ExistingNames.insert(Copy.Name).second;
			 ++Suffix)
		{
			Copy.Name = BaseName + " " + ToXrString(std::to_string(Suffix));
		}
	}
	for (Tiramisu::Scene::FLightComponent& Copy : LightCopies)
	{
		const xr_string SourceId = Copy.Id;
		for (u64 Suffix = 0;; ++Suffix)
		{
			Copy.Id =
				GenerateDeterministicMaterialGuid(
					"native-scene-light-component-duplicate",
					Scene.Scene.Id + "|" + SourceId + "|" +
						ToXrString(std::to_string(Suffix))
				);
			if (ExistingIds.insert(Copy.Id).second)
			{
				break;
			}
		}

		const xr_string BaseName = Copy.Name.empty()
									   ? "Light Copy"
									   : Copy.Name + " Copy";
		Copy.Name = BaseName;
		for (u64 Suffix = 2;
			 !ExistingNames.insert(Copy.Name).second;
			 ++Suffix)
		{
			Copy.Name = BaseName + " " + ToXrString(std::to_string(Suffix));
		}
	}

	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	SelectedComponents.clear();
	for (Tiramisu::Scene::FStaticMeshComponent& Copy : MeshCopies)
	{
		SelectedComponents.insert(Copy.Id);
		Scene.Scene.StaticMeshComponents.push_back(std::move(Copy));
	}
	for (Tiramisu::Scene::FLightComponent& Copy : LightCopies)
	{
		SelectedComponents.insert(Copy.Id);
		Scene.Scene.LightComponents.push_back(std::move(Copy));
	}
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return CopyCount;
}

size_t TiramisuEditorNativeSceneDocument::CopySelectedToClipboard(
	xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!Open || SelectedComponents.empty())
	{
		Diagnostic = "At least one native component must be selected.";
		return 0;
	}

	xr_vector<FClipboardEntry> Candidate;
	xr_vector<Tiramisu::Scene::FLightComponent> LightCandidate;
	Candidate.reserve(SelectedComponents.size());
	LightCandidate.reserve(SelectedComponents.size());
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!SelectedComponents.contains(Component.Id))
		{
			continue;
		}
		const auto Mesh = Scene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == Scene.StaticMeshes.end())
		{
			Diagnostic = "A selected native component references an "
						 "unresolved StaticMesh.";
			return 0;
		}

		std::filesystem::path MeshSource(Mesh->second.SourcePath.c_str());
		if (MeshSource.empty())
		{
			MeshSource =
				SourcePath.parent_path() / Component.StaticMesh.c_str();
		}
		else if (MeshSource.is_relative())
		{
			MeshSource = SourcePath.parent_path() / MeshSource;
		}
		if (MeshSource.is_relative())
		{
			std::error_code AbsoluteError;
			const std::filesystem::path Absolute =
				std::filesystem::absolute(MeshSource, AbsoluteError);
			if (!AbsoluteError)
			{
				MeshSource = Absolute;
			}
		}
		MeshSource = MeshSource.lexically_normal();
		if (MeshSource.empty())
		{
			Diagnostic = "A selected StaticMesh has no source path.";
			return 0;
		}

		FClipboardEntry Entry;
		Entry.Component = Component;
		Entry.StaticMesh = Mesh->second;
		Entry.StaticMesh.SourcePath = MeshSource.generic_string();
		Entry.StaticMeshSourcePath = std::move(MeshSource);
		Candidate.push_back(std::move(Entry));
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (SelectedComponents.contains(Light.Id))
		{
			LightCandidate.push_back(Light);
		}
	}
	if (Candidate.size() + LightCandidate.size() !=
		SelectedComponents.size())
	{
		Diagnostic = "A selected native component no longer exists.";
		return 0;
	}
	Clipboard = std::move(Candidate);
	LightClipboard = std::move(LightCandidate);
	return Clipboard.size() + LightClipboard.size();
}

size_t TiramisuEditorNativeSceneDocument::CutSelectedToClipboard(
	xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene() || TransactionBaseline)
	{
		Diagnostic = "An editable native RenderScene is required for cut.";
		return 0;
	}
	const size_t Copied = CopySelectedToClipboard(Diagnostic);
	if (Copied == 0)
	{
		return 0;
	}
	const size_t Removed = RemoveSelected();
	if (Removed != Copied)
	{
		Diagnostic = "Native cut could not remove every copied component.";
	}
	return Removed;
}

size_t TiramisuEditorNativeSceneDocument::PasteClipboard(
	xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene() || TransactionBaseline)
	{
		Diagnostic = "An editable native RenderScene is required for paste.";
		return 0;
	}
	if (Clipboard.empty() && LightClipboard.empty())
	{
		Diagnostic = "The native scene clipboard is empty.";
		return 0;
	}

	struct FPreparedEntry
	{
		Tiramisu::Scene::FStaticMeshComponent Component;
		Tiramisu::Scene::FStaticMeshAsset StaticMesh;
		xr_string Reference;
	};
	xr_vector<FPreparedEntry> Prepared;
	Prepared.reserve(Clipboard.size());
	xr_vector<Tiramisu::Scene::FLightComponent> PreparedLights =
		LightClipboard;
	xr_hash_map<xr_string, xr_string> MeshIdsByReference;
	for (const auto& [Reference, Mesh] : Scene.StaticMeshes)
	{
		MeshIdsByReference.insert_or_assign(Reference, Mesh.Id);
	}

	std::error_code CurrentPathError;
	const std::filesystem::path DestinationRoot = SourcePath.empty()
													  ? std::filesystem::current_path(CurrentPathError)
													  : SourcePath.parent_path();
	for (const FClipboardEntry& Entry : Clipboard)
	{
		std::error_code RelativeError;
		std::filesystem::path Reference = CurrentPathError
											  ? Entry.StaticMeshSourcePath
											  : std::filesystem::relative(
													Entry.StaticMeshSourcePath, DestinationRoot, RelativeError
												);
		if (RelativeError || Reference.empty())
		{
			Reference = Entry.StaticMeshSourcePath;
		}
		Reference = Reference.lexically_normal();
		if (Reference.empty())
		{
			Diagnostic = "Clipboard StaticMesh reference is empty.";
			return 0;
		}
		const xr_string ReferenceText = Reference.generic_string();
		const auto Existing = MeshIdsByReference.find(ReferenceText);
		if (Existing != MeshIdsByReference.end() &&
			Existing->second != Entry.StaticMesh.Id)
		{
			Diagnostic = "Clipboard StaticMesh path collides with a "
						 "different asset in the target scene.";
			return 0;
		}
		MeshIdsByReference.insert_or_assign(
			ReferenceText, Entry.StaticMesh.Id
		);

		FPreparedEntry Copy;
		Copy.Component = Entry.Component;
		Copy.Component.StaticMesh = ReferenceText;
		Copy.StaticMesh = Entry.StaticMesh;
		Copy.StaticMesh.SourcePath =
			Entry.StaticMeshSourcePath.generic_string();
		Copy.Reference = ReferenceText;
		Prepared.push_back(std::move(Copy));
	}

	xr_hash_set<xr_string> ExistingIds;
	xr_hash_set<xr_string> ExistingNames;
	for (const Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		ExistingIds.insert(Component.Id);
		ExistingNames.insert(Component.Name);
	}
	for (const Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		ExistingIds.insert(Light.Id);
		ExistingNames.insert(Light.Name);
	}
	for (FPreparedEntry& Copy : Prepared)
	{
		const xr_string SourceId = Copy.Component.Id;
		for (u64 Suffix = 0;; ++Suffix)
		{
			Copy.Component.Id =
				GenerateDeterministicMaterialGuid(
					"native-scene-static-mesh-component-paste",
					Scene.Scene.Id + "|" + SourceId + "|" +
						ToXrString(std::to_string(Suffix))
				);
			if (ExistingIds.insert(Copy.Component.Id).second)
			{
				break;
			}
		}

		const xr_string OriginalName = Copy.Component.Name.empty()
										   ? "StaticMesh"
										   : Copy.Component.Name;
		if (!ExistingNames.insert(OriginalName).second)
		{
			const xr_string BaseName = OriginalName + " Copy";
			Copy.Component.Name = BaseName;
			for (u64 Suffix = 2;
				 !ExistingNames.insert(Copy.Component.Name).second;
				 ++Suffix)
			{
				Copy.Component.Name =
					BaseName + " " + ToXrString(std::to_string(Suffix));
			}
		}
		else
		{
			Copy.Component.Name = OriginalName;
		}
	}
	for (Tiramisu::Scene::FLightComponent& Copy : PreparedLights)
	{
		const xr_string SourceId = Copy.Id;
		for (u64 Suffix = 0;; ++Suffix)
		{
			Copy.Id =
				GenerateDeterministicMaterialGuid(
					"native-scene-light-component-paste",
					Scene.Scene.Id + "|" + SourceId + "|" +
						ToXrString(std::to_string(Suffix))
				);
			if (ExistingIds.insert(Copy.Id).second)
			{
				break;
			}
		}

		const xr_string OriginalName = Copy.Name.empty()
										   ? "Light"
										   : Copy.Name;
		if (!ExistingNames.insert(OriginalName).second)
		{
			const xr_string BaseName = OriginalName + " Copy";
			Copy.Name = BaseName;
			for (u64 Suffix = 2;
				 !ExistingNames.insert(Copy.Name).second;
				 ++Suffix)
			{
				Copy.Name =
					BaseName + " " + ToXrString(std::to_string(Suffix));
			}
		}
		else
		{
			Copy.Name = OriginalName;
		}
	}

	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	SelectedComponents.clear();
	for (FPreparedEntry& Copy : Prepared)
	{
		Scene.StaticMeshes.insert_or_assign(
			Copy.Reference, std::move(Copy.StaticMesh)
		);
		SelectedComponents.insert(Copy.Component.Id);
		Scene.Scene.StaticMeshComponents.push_back(
			std::move(Copy.Component)
		);
	}
	if (!PreparedLights.empty())
	{
		Scene.Scene.Version = Tiramisu::Scene::RenderSceneAssetVersion;
	}
	for (Tiramisu::Scene::FLightComponent& Copy : PreparedLights)
	{
		SelectedComponents.insert(Copy.Id);
		Scene.Scene.LightComponents.push_back(std::move(Copy));
	}
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return Prepared.size() + PreparedLights.size();
}

size_t TiramisuEditorNativeSceneDocument::RemoveSelected()
{
	if (!IsEditableRenderScene() || TransactionBaseline ||
		SelectedComponents.empty())
	{
		return 0;
	}
	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	const size_t PreviousMeshCount =
		Scene.Scene.StaticMeshComponents.size();
	const size_t PreviousLightCount =
		Scene.Scene.LightComponents.size();
	std::erase_if(Scene.Scene.StaticMeshComponents, [this](const Tiramisu::Scene::FStaticMeshComponent& Component)
				  { return SelectedComponents.contains(Component.Id); });
	std::erase_if(Scene.Scene.LightComponents, [this](const Tiramisu::Scene::FLightComponent& Light)
				  { return SelectedComponents.contains(Light.Id); });
	const size_t Removed =
		PreviousMeshCount - Scene.Scene.StaticMeshComponents.size() +
		PreviousLightCount - Scene.Scene.LightComponents.size();
	if (Removed == 0)
	{
		return 0;
	}

	std::erase_if(Scene.StaticMeshes, [this](const auto& Mesh)
				  { return std::ranges::none_of(
						Scene.Scene.StaticMeshComponents,
						[&Mesh](
							const Tiramisu::Scene::FStaticMeshComponent& Component
						)
						{
							return Component.StaticMesh == Mesh.first;
						}
					); });
	UndoStack.push_back(Before);
	RedoStack.clear();
	SelectedComponents.clear();
	PublishSceneChange();
	return Removed;
}

bool TiramisuEditorNativeSceneDocument::SetSelectedComponentName(
	const xr_string_view Name, xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene() || TransactionBaseline ||
		SelectedComponents.size() != 1)
	{
		Diagnostic = "Exactly one native component must be selected.";
		return false;
	}
	if (Name.empty())
	{
		Diagnostic = "Native component name cannot be empty.";
		return false;
	}
	auto Component = std::ranges::find(
		Scene.Scene.StaticMeshComponents, *SelectedComponents.begin(), &Tiramisu::Scene::FStaticMeshComponent::Id
	);
	xr_string* SelectedName = Component !=
									  Scene.Scene.StaticMeshComponents.end()
								  ? &Component->Name
								  : nullptr;
	if (!SelectedName)
	{
		auto Light = std::ranges::find(
			Scene.Scene.LightComponents, *SelectedComponents.begin(), &Tiramisu::Scene::FLightComponent::Id
		);
		if (Light != Scene.Scene.LightComponents.end())
		{
			SelectedName = &Light->Name;
		}
	}
	if (!SelectedName)
	{
		Diagnostic = "Selected native component no longer exists.";
		return false;
	}
	if (*SelectedName == Name)
	{
		return true;
	}
	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	*SelectedName = Name;
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return true;
}

bool TiramisuEditorNativeSceneDocument::SetSelectedComponentVisibility(
	const bool Visible
)
{
	if (!IsEditableRenderScene() || TransactionBaseline ||
		SelectedComponents.size() != 1)
	{
		return false;
	}
	auto Component = std::ranges::find(
		Scene.Scene.StaticMeshComponents, *SelectedComponents.begin(), &Tiramisu::Scene::FStaticMeshComponent::Id
	);
	bool* SelectedVisible = Component !=
									Scene.Scene.StaticMeshComponents.end()
								? &Component->Visible
								: nullptr;
	if (!SelectedVisible)
	{
		auto Light = std::ranges::find(
			Scene.Scene.LightComponents, *SelectedComponents.begin(), &Tiramisu::Scene::FLightComponent::Id
		);
		if (Light != Scene.Scene.LightComponents.end())
		{
			SelectedVisible = &Light->Visible;
		}
	}
	if (!SelectedVisible)
	{
		return false;
	}
	if (*SelectedVisible == Visible)
	{
		return true;
	}
	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	*SelectedVisible = Visible;
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return true;
}

size_t TiramisuEditorNativeSceneDocument::SetSelectedComponentsVisibility(
	const bool Visible
)
{
	if (!IsEditableRenderScene() || TransactionBaseline ||
		SelectedComponents.empty())
	{
		return 0;
	}
	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	size_t Changed = 0;
	for (Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (SelectedComponents.contains(Component.Id) &&
			Component.Visible != Visible)
		{
			Component.Visible = Visible;
			++Changed;
		}
	}
	for (Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (SelectedComponents.contains(Light.Id) &&
			Light.Visible != Visible)
		{
			Light.Visible = Visible;
			++Changed;
		}
	}
	if (Changed == 0)
	{
		return 0;
	}
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return Changed;
}

size_t TiramisuEditorNativeSceneDocument::SetUnselectedComponentsVisibility(
	const bool Visible
)
{
	if (!IsEditableRenderScene() || TransactionBaseline)
	{
		return 0;
	}
	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	size_t Changed = 0;
	for (Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!SelectedComponents.contains(Component.Id) &&
			Component.Visible != Visible)
		{
			Component.Visible = Visible;
			++Changed;
		}
	}
	for (Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (!SelectedComponents.contains(Light.Id) &&
			Light.Visible != Visible)
		{
			Light.Visible = Visible;
			++Changed;
		}
	}
	if (Changed == 0)
	{
		return 0;
	}
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return Changed;
}

size_t TiramisuEditorNativeSceneDocument::SetAllComponentsVisibility(
	const bool Visible
)
{
	if (!IsEditableRenderScene() || TransactionBaseline)
	{
		return 0;
	}
	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	size_t Changed = 0;
	for (Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (Component.Visible != Visible)
		{
			Component.Visible = Visible;
			++Changed;
		}
	}
	for (Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (Light.Visible != Visible)
		{
			Light.Visible = Visible;
			++Changed;
		}
	}
	if (Changed == 0)
	{
		return 0;
	}
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return Changed;
}

bool TiramisuEditorNativeSceneDocument::SetSelectedComponentPosition(
	const xr_array<float, 3>& Position
)
{
	if (!std::ranges::all_of(Position, [](const float Value)
							 { return std::isfinite(Value); }))
	{
		return false;
	}
	return TransformSelected(
		[&Position](xr_array<float, 16>& Transform)
		{
			Transform[12] = Position[0];
			Transform[13] = Position[1];
			Transform[14] = Position[2];
		}
	);
}

bool TiramisuEditorNativeSceneDocument::SetSelectedLightDetails(
	const FEditorNativeSceneLightDetails& Details,
	xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene() || SelectedComponents.size() != 1 ||
		Details.Id != *SelectedComponents.begin())
	{
		Diagnostic = "Exactly one matching native light must be selected.";
		return false;
	}
	auto Light = std::ranges::find(
		Scene.Scene.LightComponents, Details.Id, &Tiramisu::Scene::FLightComponent::Id
	);
	if (Light == Scene.Scene.LightComponents.end())
	{
		Diagnostic = "Selected native light no longer exists.";
		return false;
	}

	Tiramisu::Scene::FLightComponent Updated = *Light;
	Updated.Name = Details.Name;
	Updated.Type = Details.Type;
	Updated.LocalToWorld[12] = Details.Position[0];
	Updated.LocalToWorld[13] = Details.Position[1];
	Updated.LocalToWorld[14] = Details.Position[2];
	Updated.Color = Details.Color;
	Updated.Intensity = Details.Intensity;
	Updated.Range = Details.Range;
	Updated.InnerConeAngleDegrees = Details.InnerConeAngleDegrees;
	Updated.OuterConeAngleDegrees = Details.OuterConeAngleDegrees;
	Updated.Visible = Details.Visible;
	Updated.CastShadows = Details.CastShadows;
	if (Updated.Id == Light->Id && Updated.Name == Light->Name &&
		Updated.Type == Light->Type &&
		Updated.LocalToWorld == Light->LocalToWorld &&
		Updated.Color == Light->Color &&
		Updated.Intensity == Light->Intensity &&
		Updated.Range == Light->Range &&
		Updated.InnerConeAngleDegrees ==
			Light->InnerConeAngleDegrees &&
		Updated.OuterConeAngleDegrees ==
			Light->OuterConeAngleDegrees &&
		Updated.Visible == Light->Visible &&
		Updated.CastShadows == Light->CastShadows)
	{
		return true;
	}

	Tiramisu::Scene::FResolvedRenderScene Candidate = Scene;
	auto CandidateLight = std::ranges::find(
		Candidate.Scene.LightComponents, Details.Id, &Tiramisu::Scene::FLightComponent::Id
	);
	*CandidateLight = Updated;
	const auto Validation =
		Tiramisu::Scene::ParseRenderSceneAssetJson(
			Tiramisu::Scene::SerializeRenderSceneAssetJson(
				Candidate.Scene
			)
		);
	if (!Validation.Succeeded())
	{
		Diagnostic = FormatDiagnostics(Validation.Diagnostics);
		return false;
	}

	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	*Light = std::move(Updated);
	if (TransactionBaseline)
	{
		TransactionChanged = true;
	}
	else
	{
		UndoStack.push_back(Before);
		RedoStack.clear();
	}
	PublishSceneChange();
	return true;
}

bool TiramisuEditorNativeSceneDocument::SetSelectedMaterialOverride(
	const u32 MaterialSlot, const xr_string_view Material, const bool TwoSided, xr_string& Diagnostic
)
{
	return SetSelectedComponentsMaterialOverride(
		MaterialSlot, Material, TwoSided, Diagnostic
	);
}

bool TiramisuEditorNativeSceneDocument::SetSelectedComponentsMaterialOverride(
	const u32 MaterialSlot, const xr_string_view Material, const xr_optional<bool> TwoSided, xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene() || TransactionBaseline ||
		SelectedComponents.empty())
	{
		Diagnostic = "At least one native component must be selected.";
		return false;
	}
	if (Material.empty())
	{
		Diagnostic = "Material override reference cannot be empty.";
		return false;
	}

	struct FSelectedComponent
	{
		Tiramisu::Scene::FStaticMeshComponent* Component = nullptr;
		const Tiramisu::Scene::FStaticMeshMaterialSlot* Base = nullptr;
	};
	xr_vector<FSelectedComponent> Components;
	Components.reserve(SelectedComponents.size());
	for (Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!SelectedComponents.contains(Component.Id))
		{
			continue;
		}
		const auto Mesh = Scene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == Scene.StaticMeshes.end() ||
			MaterialSlot >= Mesh->second.MaterialSlots.size())
		{
			Diagnostic = "Material override slot is not available on all "
						 "selected StaticMesh components.";
			return false;
		}
		Components.push_back(
			{&Component, &Mesh->second.MaterialSlots[MaterialSlot]}
		);
	}
	if (Components.size() != SelectedComponents.size())
	{
		Diagnostic = "A selected native component no longer exists.";
		return false;
	}
	const xr_string MaterialReference(Material);
	const bool Changed = std::ranges::any_of(
		Components,
		[&](const FSelectedComponent& Selected)
		{
			const auto Override = std::ranges::find(
				Selected.Component->MaterialOverrides, MaterialSlot, &Tiramisu::Scene::FStaticMeshMaterialOverride::MaterialSlot
			);
			const bool TargetTwoSided = TwoSided.value_or(
				Override == Selected.Component->MaterialOverrides.end()
					? Selected.Base->TwoSided
					: Override->TwoSided
			);
			return Override ==
					   Selected.Component->MaterialOverrides.end() ||
				   Override->Material != MaterialReference ||
				   Override->TwoSided != TargetTwoSided;
		}
	);
	if (!Changed)
	{
		return true;
	}

	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	for (const FSelectedComponent& Selected : Components)
	{
		auto Override = std::ranges::find(
			Selected.Component->MaterialOverrides, MaterialSlot, &Tiramisu::Scene::FStaticMeshMaterialOverride::MaterialSlot
		);
		const bool TargetTwoSided = TwoSided.value_or(
			Override == Selected.Component->MaterialOverrides.end()
				? Selected.Base->TwoSided
				: Override->TwoSided
		);
		if (Override == Selected.Component->MaterialOverrides.end())
		{
			Selected.Component->MaterialOverrides.push_back(
				{MaterialSlot, MaterialReference, TargetTwoSided}
			);
			std::ranges::sort(
				Selected.Component->MaterialOverrides, {}, &Tiramisu::Scene::FStaticMeshMaterialOverride::MaterialSlot
			);
		}
		else
		{
			Override->Material = MaterialReference;
			Override->TwoSided = TargetTwoSided;
		}
	}
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return true;
}

bool TiramisuEditorNativeSceneDocument::ClearSelectedMaterialOverride(
	const u32 MaterialSlot, xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene() || TransactionBaseline ||
		SelectedComponents.empty())
	{
		Diagnostic = "At least one native component must be selected.";
		return false;
	}

	xr_vector<Tiramisu::Scene::FStaticMeshComponent*> Components;
	Components.reserve(SelectedComponents.size());
	for (Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!SelectedComponents.contains(Component.Id))
		{
			continue;
		}
		const auto Mesh = Scene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == Scene.StaticMeshes.end() ||
			MaterialSlot >= Mesh->second.MaterialSlots.size())
		{
			Diagnostic = "Material override slot is not available on all "
						 "selected StaticMesh components.";
			return false;
		}
		Components.push_back(&Component);
	}
	if (Components.size() != SelectedComponents.size())
	{
		Diagnostic = "A selected native component no longer exists.";
		return false;
	}
	const bool Changed = std::ranges::any_of(
		Components,
		[&](const Tiramisu::Scene::FStaticMeshComponent* Component)
		{
			return std::ranges::find(
					   Component->MaterialOverrides, MaterialSlot, &Tiramisu::Scene::FStaticMeshMaterialOverride::MaterialSlot
				   ) != Component->MaterialOverrides.end();
		}
	);
	if (!Changed)
	{
		return true;
	}

	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	for (Tiramisu::Scene::FStaticMeshComponent* Component : Components)
	{
		const auto Override = std::ranges::find(
			Component->MaterialOverrides, MaterialSlot, &Tiramisu::Scene::FStaticMeshMaterialOverride::MaterialSlot
		);
		if (Override != Component->MaterialOverrides.end())
		{
			Component->MaterialOverrides.erase(Override);
		}
	}
	UndoStack.push_back(Before);
	RedoStack.clear();
	PublishSceneChange();
	return true;
}

bool TiramisuEditorNativeSceneDocument::BeginEditTransaction()
{
	if (!IsEditableRenderScene() || TransactionBaseline)
	{
		return false;
	}
	TransactionBaseline = Scene;
	TransactionChanged = false;
	return true;
}

bool TiramisuEditorNativeSceneDocument::TransformSelected(
	const std::function<void(xr_array<float, 16>&)>& Transform
)
{
	if (!IsEditableRenderScene() || SelectedComponents.empty() || !Transform)
	{
		return false;
	}
	const Tiramisu::Scene::FResolvedRenderScene Before = Scene;
	bool Changed = false;
	for (Tiramisu::Scene::FStaticMeshComponent& Component :
		 Scene.Scene.StaticMeshComponents)
	{
		if (!SelectedComponents.contains(Component.Id))
		{
			continue;
		}
		const xr_array<float, 16> Previous = Component.LocalToWorld;
		Transform(Component.LocalToWorld);
		if (!IsFiniteTransform(Component.LocalToWorld))
		{
			Scene = Before;
			return false;
		}
		Changed |= Component.LocalToWorld != Previous;
	}
	for (Tiramisu::Scene::FLightComponent& Light :
		 Scene.Scene.LightComponents)
	{
		if (!SelectedComponents.contains(Light.Id))
		{
			continue;
		}
		const xr_array<float, 16> Previous = Light.LocalToWorld;
		Transform(Light.LocalToWorld);
		if (!IsFiniteTransform(Light.LocalToWorld))
		{
			Scene = Before;
			return false;
		}
		Changed |= Light.LocalToWorld != Previous;
	}
	if (!Changed)
	{
		return false;
	}

	if (TransactionBaseline)
	{
		TransactionChanged = true;
	}
	else
	{
		UndoStack.push_back(Before);
		RedoStack.clear();
	}
	PublishSceneChange();
	return true;
}

bool TiramisuEditorNativeSceneDocument::TranslateSelected(
	const xr_array<float, 3>& Delta
)
{
	if (!std::ranges::all_of(Delta, [](const float Value)
							 { return std::isfinite(Value); }))
	{
		return false;
	}
	return TransformSelected(
		[&Delta](xr_array<float, 16>& Transform)
		{
			Transform[12] += Delta[0];
			Transform[13] += Delta[1];
			Transform[14] += Delta[2];
		}
	);
}

bool TiramisuEditorNativeSceneDocument::EndEditTransaction(const bool Commit)
{
	if (!TransactionBaseline)
	{
		return false;
	}
	if (!Commit)
	{
		if (TransactionChanged)
		{
			Scene = std::move(*TransactionBaseline);
			PublishSceneChange();
		}
		TransactionBaseline.reset();
		TransactionChanged = false;
		return true;
	}
	if (TransactionChanged)
	{
		UndoStack.push_back(std::move(*TransactionBaseline));
		RedoStack.clear();
	}
	TransactionBaseline.reset();
	TransactionChanged = false;
	return true;
}

bool TiramisuEditorNativeSceneDocument::Undo()
{
	if (!IsEditableRenderScene() || TransactionBaseline || UndoStack.empty())
	{
		return false;
	}
	RedoStack.push_back(Scene);
	Scene = std::move(UndoStack.back());
	UndoStack.pop_back();
	PruneSelection();
	PublishSceneChange();
	return true;
}

bool TiramisuEditorNativeSceneDocument::Redo()
{
	if (!IsEditableRenderScene() || TransactionBaseline || RedoStack.empty())
	{
		return false;
	}
	UndoStack.push_back(Scene);
	Scene = std::move(RedoStack.back());
	RedoStack.pop_back();
	PruneSelection();
	PublishSceneChange();
	return true;
}

bool TiramisuEditorNativeSceneDocument::Save(xr_string& Diagnostic)
{
	return SaveAs(SourcePath, Diagnostic);
}

bool TiramisuEditorNativeSceneDocument::SaveAs(
	const std::filesystem::path& Path, xr_string& Diagnostic
)
{
	Diagnostic.clear();
	if (!IsEditableRenderScene())
	{
		Diagnostic = "The open native document is a read-only static-mesh "
					 "preview, not a render scene.";
		return false;
	}
	if (TransactionBaseline)
	{
		Diagnostic = "Cannot save a native render scene during an active edit "
					 "transaction.";
		return false;
	}
	const std::filesystem::path TargetPath = Path.lexically_normal();
	if (TargetPath.empty())
	{
		Diagnostic = "Native render-scene target path is empty.";
		return false;
	}

	Tiramisu::Scene::FRenderSceneAsset Candidate = Scene.Scene;
	xr_hash_map<xr_string, Tiramisu::Scene::FStaticMeshAsset>
		RebasedMeshes;
	for (Tiramisu::Scene::FStaticMeshComponent& Component :
		 Candidate.StaticMeshComponents)
	{
		const auto Mesh = Scene.StaticMeshes.find(Component.StaticMesh);
		if (Mesh == Scene.StaticMeshes.end())
		{
			Diagnostic = "Native component '" + Component.Name +
						 "' references an unresolved static mesh.";
			return false;
		}
		std::filesystem::path MeshPath(Mesh->second.SourcePath.c_str());
		if (MeshPath.empty())
		{
			MeshPath = SourcePath.parent_path() / Component.StaticMesh.c_str();
		}
		if (MeshPath.is_relative())
		{
			MeshPath = SourcePath.parent_path() / MeshPath;
		}
		MeshPath = MeshPath.lexically_normal();
		std::error_code RelativeError;
		std::filesystem::path Reference = std::filesystem::relative(
			MeshPath, TargetPath.parent_path(), RelativeError
		);
		if (RelativeError || Reference.empty())
		{
			Reference = MeshPath;
		}
		Component.StaticMesh = Reference.lexically_normal().generic_string();
		RebasedMeshes.insert_or_assign(
			Component.StaticMesh, Mesh->second
		);
	}
	Candidate.SourcePath = TargetPath.generic_string();
	const xr_string Json =
		Tiramisu::Scene::SerializeRenderSceneAssetJson(Candidate);
	const Tiramisu::Scene::FRenderSceneAssetParseResult Validation =
		Tiramisu::Scene::ParseRenderSceneAssetJson(
			Json, TargetPath.generic_string()
		);
	if (!Validation.Succeeded())
	{
		Diagnostic = FormatDiagnostics(Validation.Diagnostics);
		return false;
	}
	const Tiramisu::Editor::FAtomicTextFileWriteResult Result =
		Tiramisu::Editor::WriteTextFileAtomically(TargetPath, Json);
	if (!Result.Success)
	{
		Diagnostic = Result.Error;
		return false;
	}
	Scene.Scene = std::move(Candidate);
	Scene.StaticMeshes = std::move(RebasedMeshes);
	SourcePath = TargetPath;
	SavedSceneJson = Json;
	Dirty = false;
	++Revision;
	return true;
}

void TiramisuEditorNativeSceneDocument::PublishSceneChange()
{
	++Revision;
	UpdateDirtyState();
}

void TiramisuEditorNativeSceneDocument::UpdateDirtyState()
{
	Dirty = EditableRenderScene &&
			Tiramisu::Scene::SerializeRenderSceneAssetJson(Scene.Scene) !=
				SavedSceneJson;
}

void TiramisuEditorNativeSceneDocument::PruneSelection()
{
	std::erase_if(SelectedComponents, [this](const xr_string& Selected)
				  {
			const bool MissingMesh = std::ranges::none_of(
				Scene.Scene.StaticMeshComponents,
				[&Selected](
					const Tiramisu::Scene::FStaticMeshComponent& Component)
				{
					return Component.Id == Selected;
				});
			const bool MissingLight = std::ranges::none_of(
				Scene.Scene.LightComponents,
				[&Selected](
					const Tiramisu::Scene::FLightComponent& Light)
				{
					return Light.Id == Selected;
				});
			return MissingMesh && MissingLight; });
}

TiramisuEditorNativeSceneDocument& GetEditorNativeSceneDocument() noexcept
{
	static TiramisuEditorNativeSceneDocument Document;
	return Document;
}
