#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportScenePicker.h"
#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorOgfModelLoader.h"

#include <array>
#include <cmath>
#include <fstream>
#include <iostream>
#include <iterator>

namespace
{
using FVector = xr_array<float, 3>;

int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}

FEditorStaticMeshInstance MakeInstance(const u64 ObjectId, const float Z)
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

bool TestRealOgfPicking()
{
	std::ifstream Stream(
		"gamedata/meshes/dynamics/scene_objects/part/part_none.ogf",
		std::ios::binary
	);
	if (!Stream)
	{
		return false;
	}
	const xr_vector<char> FileBytes{
		std::istreambuf_iterator<char>(Stream),
		std::istreambuf_iterator<char>()
	};
	xr_vector<u8> Bytes(FileBytes.begin(), FileBytes.end());
	IReader Reader(Bytes.data(), Bytes.size());
	FTiramisuEditorOgfModelSource Model;
	if (!LoadTiramisuEditorOgfModel(Reader, Model) ||
		!Model.IsValid())
	{
		return false;
	}

	const FTiramisuEditorOgfMeshSource& Source = Model.Meshes.front();
	FVector Centroid = {};
	FVector Normal = {};
	bool TriangleFound = false;
	for (size_t Index = 0; Index + 2 < Source.Indices.size(); Index += 3)
	{
		const FVector& A = Source.Vertices[Source.Indices[Index]].Position;
		const FVector& B = Source.Vertices[Source.Indices[Index + 1]].Position;
		const FVector& C = Source.Vertices[Source.Indices[Index + 2]].Position;
		const FVector Edge1 = {
			B[0] - A[0], B[1] - A[1], B[2] - A[2]
		};
		const FVector Edge2 = {
			C[0] - A[0], C[1] - A[1], C[2] - A[2]
		};
		Normal = {
			Edge1[1] * Edge2[2] - Edge1[2] * Edge2[1],
			Edge1[2] * Edge2[0] - Edge1[0] * Edge2[2],
			Edge1[0] * Edge2[1] - Edge1[1] * Edge2[0]
		};
		const float Length = std::sqrt(
			Normal[0] * Normal[0] + Normal[1] * Normal[1] +
			Normal[2] * Normal[2]
		);
		if (Length <= 1.0e-6f)
		{
			continue;
		}
		for (float& Component : Normal)
		{
			Component /= Length;
		}
		Centroid = {
			(A[0] + B[0] + C[0]) / 3.0f,
			(A[1] + B[1] + C[1]) / 3.0f,
			(A[2] + B[2] + C[2]) / 3.0f
		};
		TriangleFound = true;
		break;
	}
	if (!TriangleFound)
	{
		return false;
	}

	const xr_array<FEditorStaticMeshSection, 1> Sections = {{
		{0, static_cast<u32>(Source.Indices.size()), {71}}
	}};
	const xr_array<FEditorStaticMeshUpload, 1> Meshes = {{
		{{77}, 1, Source.Vertices, Source.Indices, Sections}
	}};
	FEditorStaticMeshInstance Instance;
	Instance.ObjectId = {900};
	Instance.MeshId = {77};
	const xr_array<FEditorStaticMeshInstance, 1> Instances = {Instance};
	FEditorViewportSceneSnapshot Snapshot;
	Snapshot.StaticMeshes = Meshes;
	Snapshot.Instances = Instances;
	Snapshot.Revision = 4;
	TiramisuEditorViewportScenePicker Picker;
	Picker.Submit(Snapshot);

	FEditorViewportPickRequest Request;
	Request.RayOrigin = {
		Centroid[0] + Normal[0] * 2.0f,
		Centroid[1] + Normal[1] * 2.0f,
		Centroid[2] + Normal[2] * 2.0f
	};
	Request.RayDirection = {-Normal[0], -Normal[1], -Normal[2]};
	Request.MaxDistance = 4.0f;
	const FEditorViewportPickResult Result = Picker.Pick(Request);
	return Result.Hit && Result.ObjectId.Value == 900 &&
		Result.MeshId.Value == 77 && Result.MaterialSlot.Value == 71 &&
		Result.Distance > 0.0f && Result.Distance <= 2.001f &&
		Result.SceneRevision == 4;
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
	const xr_array<FEditorStaticMeshUpload, 1> Meshes = {{{{11}, 1, Vertices, Indices, Sections}}};
	FEditorStaticMeshInstance NearInstance = MakeInstance(102, 2.0f);
	NearInstance.MaterialOverrides.push_back({{17}, {19}});
	const xr_array<FEditorStaticMeshInstance, 2> Instances = {
		MakeInstance(101, 5.0f), NearInstance
	};
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
	{
		return Fail("MaxDistance did not reject a farther triangle");
	}
	Request.MaxDistance = 10.0f;
	Request.CullBackFaces = true;
	if (Picker.Pick(Request).Hit)
	{
		return Fail("Back-face culling did not reject the back side");
	}
	Request.RayOrigin = {0.0f, 0.0f, 3.0f};
	Request.RayDirection = {0.0f, 0.0f, -1.0f};
	Result = Picker.Pick(Request);
	if (!Result.Hit || Result.ObjectId.Value != 102 || !Near(Result.Distance, 1.0f))
	{
		return Fail("Front-face picking failed");
	}

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
	{
		return Fail("Removed mesh remained pickable");
	}

	Request.RayDirection = {};
	if (Picker.Pick(Request).Hit)
	{
		return Fail("Invalid zero-length ray was accepted");
	}
	if (!TestRealOgfPicking())
	{
		return Fail("Real skeletal OGF bind pose was not pickable");
	}
	return 0;
}
