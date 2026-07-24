#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportScenePicker.h"

#include <array>
#include <cmath>
#include <iostream>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}

FEditorStaticMeshInstance MakeInstance(const u64 ObjectId,
	const float Z)
{
	FEditorStaticMeshInstance Instance;
	Instance.ObjectId = {ObjectId};
	Instance.MeshId = {11};
	Instance.LocalToWorld[14] = Z;
	return Instance;
}

bool Near(const float A, const float B)
{
	return std::abs(A - B) <= 1.0e-4f;
}
} // namespace

int main()
{
	TiramisuEditorViewportScenePicker Picker;
	xr_array<FEditorStaticMeshVertex, 3> Vertices;
	Vertices[0].Position = {-1.0f, -1.0f, 0.0f};
	Vertices[1].Position = {1.0f, -1.0f, 0.0f};
	Vertices[2].Position = {0.0f, 1.0f, 0.0f};
	const xr_array<u32, 3> Indices = {0, 1, 2};
	const xr_array<FEditorStaticMeshSection, 1> Sections = {{{0, 3, {17}}}};
	const xr_array<FEditorStaticMeshUpload, 1> Meshes = {{{
		{11}, 1, Vertices, Indices, Sections}}};
	FEditorStaticMeshInstance NearInstance = MakeInstance(102, 2.0f);
	NearInstance.MaterialOverrides.push_back({{17}, {19}});
	const xr_array<FEditorStaticMeshInstance, 2> Instances = {
		MakeInstance(101, 5.0f), NearInstance};
	FEditorViewportSceneSnapshot Snapshot;
	Snapshot.StaticMeshes = Meshes;
	Snapshot.Instances = Instances;
	Snapshot.Revision = 1;
	Picker.Submit(Snapshot);

	FEditorViewportPickRequest Request;
	Request.RayDirection = {0.0f, 0.0f, 2.0f};
	FEditorViewportPickResult Result = Picker.Pick(Request);
	if (!Result.Hit || Result.ObjectId.Value != 102 ||
		Result.MeshId.Value != 11 || Result.MaterialSlot.Value != 19 ||
		!Near(Result.Distance, 2.0f) || !Near(Result.WorldPosition[2], 2.0f) ||
		!Near(Result.WorldNormal[2], 1.0f) || Result.SceneRevision != 1)
	{
		return Fail("The nearest transformed triangle was not picked correctly");
	}

	Request.MaxDistance = 1.5f;
	if (Picker.Pick(Request).Hit)
		return Fail("MaxDistance did not reject a farther triangle");
	Request.MaxDistance = 10.0f;
	Request.CullBackFaces = true;
	if (Picker.Pick(Request).Hit)
		return Fail("Back-face culling did not reject the back side");
	Request.RayOrigin = {0.0f, 0.0f, 3.0f};
	Request.RayDirection = {0.0f, 0.0f, -1.0f};
	Result = Picker.Pick(Request);
	if (!Result.Hit || Result.ObjectId.Value != 102 || !Near(Result.Distance, 1.0f))
		return Fail("Front-face picking failed");

	const xr_array<FEditorStaticMeshInstance, 1> Moved = {MakeInstance(103, 1.0f)};
	FEditorViewportSceneSnapshot InstanceOnly;
	InstanceOnly.Instances = Moved;
	InstanceOnly.Revision = 2;
	Picker.Submit(InstanceOnly);
	Request.CullBackFaces = false;
	Request.RayOrigin = {};
	Request.RayDirection = {0.0f, 0.0f, 1.0f};
	Result = Picker.Pick(Request);
	if (!Result.Hit || Result.ObjectId.Value != 103 ||
		!Near(Result.Distance, 1.0f) || Result.SceneRevision != 2)
	{
		return Fail("Instance-only snapshot did not retain cached mesh data");
	}

	const xr_array<FEditorStaticMeshId, 1> Removed = {{{11}}};
	FEditorViewportSceneSnapshot Empty;
	Empty.RemovedStaticMeshes = Removed;
	Empty.Instances = Moved;
	Empty.Revision = 3;
	Picker.Submit(Empty);
	Result = Picker.Pick(Request);
	if (Result.Hit || Result.SceneRevision != 3)
		return Fail("Removed mesh remained pickable");

	Request.RayDirection = {};
	if (Picker.Pick(Request).Hit)
		return Fail("Invalid zero-length ray was accepted");
	return 0;
}
