#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportSceneMailbox.h"

#include <array>
#include <cstdint>
#include <iostream>
#include <limits>
#include <string>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}

FEditorViewportSceneSnapshot MakeSnapshot(
	const xr_span<const FEditorStaticMeshUpload> Meshes,
	const xr_span<const FEditorStaticMeshInstance> Instances,
	const xr_span<const FEditorStaticMeshId> Removed = {},
	const xr_span<const FEditorMaterialSlotSource> Materials = {},
	const xr_span<const FEditorDebugLine> DebugLines = {},
	const xr_span<const FEditorDebugTriangle> DebugTriangles = {},
	const u64 DebugDrawRevision = 0,
	const xr_span<const FEditorOverlayLine> OverlayLines = {},
	const xr_span<const FEditorOverlayTriangle> OverlayTriangles = {},
	const xr_span<const FEditorOverlayText> OverlayText = {},
	const xr_span<const FEditorSceneLight> Lights = {})
{
	FEditorViewportSceneSnapshot Snapshot;
	Snapshot.Camera.View[0] = 1.0f;
	Snapshot.Camera.Projection[0] = 1.0f;
	Snapshot.Camera.ViewProjection[0] = 1.0f;
	Snapshot.MaterialSlots = Materials;
	Snapshot.StaticMeshes = Meshes;
	Snapshot.RemovedStaticMeshes = Removed;
	Snapshot.Instances = Instances;
	Snapshot.Lights = Lights;
	Snapshot.DebugLines = DebugLines;
	Snapshot.DebugTriangles = DebugTriangles;
	Snapshot.OverlayLines = OverlayLines;
	Snapshot.OverlayTriangles = OverlayTriangles;
	Snapshot.OverlayText = OverlayText;
	Snapshot.DebugDrawRevision = DebugDrawRevision;
	Snapshot.Revision = 17;
	return Snapshot;
}
} // namespace

int main()
{
	const xr_array Vertices = {
		FEditorStaticMeshVertex{{0.0f, 0.0f, 0.0f}},
		FEditorStaticMeshVertex{{1.0f, 0.0f, 0.0f}},
		FEditorStaticMeshVertex{{0.0f, 1.0f, 0.0f}}};
	const xr_array<u32, 3> Indices = {0, 1, 2};
	const xr_array Sections = {FEditorStaticMeshSection{0, 3, {9}}};
	const FEditorStaticMeshUpload Mesh{{1}, 4, Vertices, Indices, Sections};
	const FEditorStaticMeshInstance Instance{{7}, {1}};
	const xr_array Materials = {FEditorMaterialSlotSource{{9}, "default",
		"textures/test", "test surface", EEditorMaterialSlotFlags::TwoSided}};
	xr_array<FEditorDebugLine, 1> DebugLines;
	DebugLines[0].Vertices[0].Position = {-1.0f, 0.0f, 0.0f};
	DebugLines[0].Vertices[0].Color = {1.0f, 0.0f, 0.0f, 1.0f};
	DebugLines[0].Vertices[1].Position = {1.0f, 0.0f, 0.0f};
	DebugLines[0].Vertices[1].Color = {0.0f, 1.0f, 0.0f, 1.0f};
	xr_array<FEditorDebugTriangle, 1> DebugTriangles;
	DebugTriangles[0].Vertices[0].Position = {0.0f, 0.0f, 0.0f};
	DebugTriangles[0].Vertices[1].Position = {0.0f, 1.0f, 0.0f};
	DebugTriangles[0].Vertices[2].Position = {1.0f, 0.0f, 0.0f};
	xr_array<FEditorOverlayLine, 1> OverlayLines;
	OverlayLines[0].Vertices[0].Position = {-0.75f, 0.75f, 0.0f};
	OverlayLines[0].Vertices[1].Position = {0.75f, 0.75f, 0.0f};
	xr_array<FEditorOverlayTriangle, 1> OverlayTriangles;
	OverlayTriangles[0].Vertices[0].Position = {-0.5f, -0.5f, 0.0f};
	OverlayTriangles[0].Vertices[1].Position = {0.5f, -0.5f, 0.0f};
	OverlayTriangles[0].Vertices[2].Position = {0.0f, 0.5f, 0.0f};
	xr_array<FEditorOverlayText, 1> OverlayText;
	OverlayText[0].Position = {0.25f, -0.5f};
	OverlayText[0].Text = "owned viewport label";
	xr_array<FEditorSceneLight, 1> Lights;
	Lights[0].ObjectId = {11};
	Lights[0].Type = EEditorSceneLightType::Spot;
	Lights[0].LocalToWorld[12] = 2.0f;
	Lights[0].Color = {1.0f, 0.5f, 0.25f};
	Lights[0].Intensity = 8.0f;
	Lights[0].Range = 16.0f;
	Lights[0].InnerConeAngleDegrees = 15.0f;
	Lights[0].OuterConeAngleDegrees = 35.0f;
	Lights[0].Flags = EEditorSceneLightFlags::CastShadows;

	TiramisuEditorViewportSceneMailbox Mailbox;
	xr_string Diagnostic;
	if (!Mailbox.Submit(MakeSnapshot(xr_span(&Mesh, 1),
			xr_span(&Instance, 1), {}, Materials, DebugLines,
			DebugTriangles, 23, OverlayLines, OverlayTriangles, OverlayText,
			Lights),
			&Diagnostic))
		return Fail(Diagnostic.c_str());
	OverlayText[0].Text = "mutated source label";

	FEditorOwnedViewportScenePacket Packet;
	if (!Mailbox.Consume(Packet))
		return Fail("A submitted scene packet was not published");
	if (Packet.Revision != 17 || Packet.StaticMeshUpdates.size() != 1 ||
		Packet.Instances.size() != 1 || Packet.MaterialSlots.size() != 1 ||
		Packet.MaterialSlots[0].TextureName != "textures/test" ||
		Packet.MaterialSlots[0].Flags != EEditorMaterialSlotFlags::TwoSided ||
		Packet.Lights.size() != 1 ||
		Packet.Lights[0].ObjectId.Value != 11 ||
		Packet.Lights[0].Intensity != 8.0f ||
		Packet.StaticMeshUpdates[0].Vertices.size() != 3 ||
		Packet.DebugLines.size() != 1 || Packet.DebugTriangles.size() != 1 ||
		Packet.OverlayLines.size() != 1 ||
		Packet.OverlayTriangles.size() != 1 ||
		Packet.OverlayText.size() != 1 ||
		Packet.OverlayText[0].Text != "owned viewport label" ||
		Packet.DebugDrawRevision != 23 ||
		Packet.DebugLines[0].Vertices[1].Color[1] != 1.0f)
		return Fail("The mailbox did not own and preserve the scene packet");
	if (Mailbox.Consume(Packet))
		return Fail("A scene packet was consumed more than once");

	// An unchanged mesh revision must not be copied/uploaded again, while the
	// latest instance list and camera still cross the mailbox.
	OverlayText[0].Text = "owned viewport label";
	if (!Mailbox.Submit(MakeSnapshot(xr_span(&Mesh, 1),
		xr_span(&Instance, 1), {}, Materials, DebugLines,
		DebugTriangles, 23, OverlayLines, OverlayTriangles, OverlayText,
		Lights)))
		return Fail("The mailbox rejected an unchanged mesh revision");
	if (!Mailbox.Consume(Packet) || !Packet.StaticMeshUpdates.empty() ||
		Packet.Instances.size() != 1 || Packet.MaterialSlots.size() != 1 ||
		Packet.Lights.size() != 1 ||
		Packet.DebugLines.size() != 1 || Packet.DebugTriangles.size() != 1 ||
		Packet.OverlayLines.size() != 1 || Packet.OverlayTriangles.size() != 1 ||
		Packet.OverlayText.size() != 1 ||
		Packet.OverlayText[0].Text != "owned viewport label")
		return Fail("Unchanged mesh data was not coalesced");

	// A single legacy scene object may publish one instance per editable mesh,
	// so repeated ObjectId values are valid for mesh instances.
	const xr_array<FEditorStaticMeshInstance, 2> MultiMeshObject = {
		Instance, Instance};
	if (!Mailbox.Submit(MakeSnapshot({},
			MultiMeshObject, {}, Materials), &Diagnostic) ||
		!Mailbox.Consume(Packet) || Packet.Instances.size() != 2)
	{
		return Fail("A multi-mesh legacy scene object was rejected");
	}

	const xr_array OverrideMaterials = {
		Materials[0],
		FEditorMaterialSlotSource{{10}, {}, {}, "override surface",
			EEditorMaterialSlotFlags::None,
			"74dfac8a-6739-4253-804a-5d3369df759b"}};
	FEditorStaticMeshInstance OverriddenInstance = Instance;
	OverriddenInstance.MaterialOverrides.push_back({{9}, {10}});
	if (!Mailbox.Submit(MakeSnapshot({},
			xr_span(&OverriddenInstance, 1), {}, OverrideMaterials),
			&Diagnostic) ||
		!Mailbox.Consume(Packet) ||
		Packet.Instances[0].MaterialOverrides.size() != 1 ||
		ResolveEditorMaterialSlot(Packet.Instances[0], {9}).Value != 10)
	{
		return Fail("A valid per-instance material override was not preserved");
	}

	if (Mailbox.Submit(MakeSnapshot({},
			xr_span(&OverriddenInstance, 1), {}, Materials), &Diagnostic))
	{
		return Fail("An instance override referencing an undeclared material was accepted");
	}
	FEditorStaticMeshInstance MissingBaseOverride = Instance;
	MissingBaseOverride.MaterialOverrides.push_back({{99}, {10}});
	if (Mailbox.Submit(MakeSnapshot({},
			xr_span(&MissingBaseOverride, 1), {}, OverrideMaterials),
			&Diagnostic))
	{
		return Fail("An instance override for a slot absent from its mesh was accepted");
	}
	FEditorStaticMeshInstance DuplicateOverride = OverriddenInstance;
	DuplicateOverride.MaterialOverrides.push_back({{9}, {10}});
	if (Mailbox.Submit(MakeSnapshot({},
			xr_span(&DuplicateOverride, 1), {}, OverrideMaterials),
			&Diagnostic))
	{
		return Fail("Duplicate per-instance material overrides were accepted");
	}

	if (Mailbox.Submit(MakeSnapshot({}, xr_span(&Instance, 1)), &Diagnostic))
		return Fail("A cached mesh material omitted from the snapshot was accepted");

	const FEditorStaticMeshId Removed{1};
	if (!Mailbox.Submit(MakeSnapshot({}, {}, xr_span(&Removed, 1))))
		return Fail("The mailbox rejected a valid mesh removal");
	if (!Mailbox.Consume(Packet) || Packet.RemovedStaticMeshes.size() != 1 ||
		Packet.RemovedStaticMeshes[0] != Removed)
		return Fail("A mesh removal was not published");

	FEditorStaticMeshUpload InvalidIndexMesh = Mesh;
	const xr_array<u32, 3> InvalidIndices = {0, 1, 3};
	InvalidIndexMesh.Indices = InvalidIndices;
	if (Mailbox.Submit(MakeSnapshot(xr_span(&InvalidIndexMesh, 1), {}, {},
		Materials), &Diagnostic))
		return Fail("An out-of-range mesh index was accepted");

	const xr_array MissingMaterials = {FEditorMaterialSlotSource{{10},
		"default", "textures/other", "other", EEditorMaterialSlotFlags::None}};
	if (Mailbox.Submit(MakeSnapshot(xr_span(&Mesh, 1), {}, {},
		MissingMaterials), &Diagnostic))
		return Fail("A mesh section referencing an undeclared material was accepted");

	FEditorViewportSceneSnapshot InvalidCamera = MakeSnapshot({}, {});
	InvalidCamera.Camera.NearPlane = std::numeric_limits<float>::quiet_NaN();
	if (Mailbox.Submit(InvalidCamera, &Diagnostic))
		return Fail("A non-finite camera was accepted");

	const FEditorStaticMeshInstance MissingMeshInstance{{8}, {99}};
	if (Mailbox.Submit(MakeSnapshot({}, xr_span(&MissingMeshInstance, 1)),
			&Diagnostic))
		return Fail("An instance referencing a missing mesh was accepted");

	FEditorStaticMeshUpload TransactionMesh = Mesh;
	TransactionMesh.MeshId = {2};
	const FEditorStaticMeshInstance InvalidTransactionInstance{{9}, {99}};
	if (Mailbox.Submit(MakeSnapshot(xr_span(&TransactionMesh, 1),
			xr_span(&InvalidTransactionInstance, 1), {}, Materials), &Diagnostic))
		return Fail("An invalid transactional scene update was accepted");
	const FEditorStaticMeshInstance LeakedTransactionInstance{{10}, {2}};
	if (Mailbox.Submit(MakeSnapshot({},
			xr_span(&LeakedTransactionInstance, 1), {}, Materials), &Diagnostic))
		return Fail("A rejected mesh update leaked into the accepted mesh cache");

	FEditorViewportSceneSnapshot MissingDebugRevision =
		MakeSnapshot({}, {}, {}, {}, DebugLines);
	if (Mailbox.Submit(MissingDebugRevision, &Diagnostic))
		return Fail("A non-empty debug draw without a revision was accepted");
	xr_array<FEditorDebugLine, 1> InvalidDebugLines = DebugLines;
	InvalidDebugLines[0].Vertices[0].Position[0] =
		std::numeric_limits<float>::quiet_NaN();
	FEditorViewportSceneSnapshot InvalidDebug =
		MakeSnapshot({}, {}, {}, {}, InvalidDebugLines, {}, 24);
	if (Mailbox.Submit(InvalidDebug, &Diagnostic))
		return Fail("A non-finite editor debug vertex was accepted");

	xr_array<FEditorOverlayLine, 1> InvalidOverlayLines = OverlayLines;
	InvalidOverlayLines[0].Vertices[0].Position[0] =
		std::numeric_limits<float>::quiet_NaN();
	FEditorViewportSceneSnapshot InvalidOverlay = MakeSnapshot({}, {}, {}, {},
		{}, {}, 25, InvalidOverlayLines);
	if (Mailbox.Submit(InvalidOverlay, &Diagnostic))
		return Fail("A non-finite editor overlay vertex was accepted");

	xr_array<FEditorOverlayText, 1> InvalidOverlayText = OverlayText;
	InvalidOverlayText[0].Position[1] =
		std::numeric_limits<float>::quiet_NaN();
	FEditorViewportSceneSnapshot InvalidText = MakeSnapshot({}, {}, {}, {},
		{}, {}, 26, {}, {}, InvalidOverlayText);
	if (Mailbox.Submit(InvalidText, &Diagnostic))
		return Fail("A non-finite editor overlay text position was accepted");
	InvalidOverlayText[0].Position = {};
	InvalidOverlayText[0].Text.clear();
	InvalidText = MakeSnapshot({}, {}, {}, {}, {}, {}, 27, {}, {},
		InvalidOverlayText);
	if (Mailbox.Submit(InvalidText, &Diagnostic))
		return Fail("An empty editor overlay label was accepted");

	xr_array<FEditorSceneLight, 1> InvalidLights = Lights;
	InvalidLights[0].Range = 0.0f;
	if (Mailbox.Submit(MakeSnapshot({}, {}, {}, {}, {}, {}, 0,
			{}, {}, {}, InvalidLights), &Diagnostic))
	{
		return Fail("A local light with zero range was accepted");
	}
	InvalidLights = Lights;
	InvalidLights[0].ObjectId = Instance.ObjectId;
	if (Mailbox.Submit(MakeSnapshot({},
			xr_span(&Instance, 1), {}, Materials, {}, {}, 0,
			{}, {}, {}, InvalidLights), &Diagnostic))
	{
		return Fail("A light duplicated a scene instance object ID");
	}

	return 0;
}
