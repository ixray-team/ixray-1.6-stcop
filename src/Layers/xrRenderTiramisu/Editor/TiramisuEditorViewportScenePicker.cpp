#include "TiramisuEditorViewportScenePicker.h"

#include <algorithm>
#include <cmath>
#include <limits>

namespace
{
using FVector = xr_array<float, 3>;

[[nodiscard]] FVector Subtract(const FVector& A, const FVector& B)
{
	return {A[0] - B[0], A[1] - B[1], A[2] - B[2]};
}

[[nodiscard]] FVector AddScaled(const FVector& A, const FVector& B,
	const float Scale)
{
	return {A[0] + B[0] * Scale, A[1] + B[1] * Scale,
		A[2] + B[2] * Scale};
}

[[nodiscard]] float Dot(const FVector& A, const FVector& B)
{
	return A[0] * B[0] + A[1] * B[1] + A[2] * B[2];
}

[[nodiscard]] FVector Cross(const FVector& A, const FVector& B)
{
	return {A[1] * B[2] - A[2] * B[1],
		A[2] * B[0] - A[0] * B[2],
		A[0] * B[1] - A[1] * B[0]};
}

[[nodiscard]] bool Normalize(FVector& Value)
{
	const float LengthSquared = Dot(Value, Value);
	if (!std::isfinite(LengthSquared) || LengthSquared <= 1.0e-12f)
		return false;
	const float InverseLength = 1.0f / std::sqrt(LengthSquared);
	for (float& Component : Value)
		Component *= InverseLength;
	return true;
}

[[nodiscard]] FVector TransformPosition(const xr_array<float, 16>& Matrix,
	const FVector& Position)
{
	return {
		Position[0] * Matrix[0] + Position[1] * Matrix[4] +
			Position[2] * Matrix[8] + Matrix[12],
		Position[0] * Matrix[1] + Position[1] * Matrix[5] +
			Position[2] * Matrix[9] + Matrix[13],
		Position[0] * Matrix[2] + Position[1] * Matrix[6] +
			Position[2] * Matrix[10] + Matrix[14]};
}

[[nodiscard]] bool IntersectTriangle(const FVector& Origin,
	const FVector& Direction, const FVector& A, const FVector& B,
	const FVector& C, const bool CullBackFaces, float& OutDistance,
	FVector& OutNormal)
{
	constexpr float Epsilon = 1.0e-7f;
	const FVector Edge1 = Subtract(B, A);
	const FVector Edge2 = Subtract(C, A);
	const FVector P = Cross(Direction, Edge2);
	const float Determinant = Dot(Edge1, P);
	if (CullBackFaces ? Determinant <= Epsilon : std::abs(Determinant) <= Epsilon)
		return false;
	const float InverseDeterminant = 1.0f / Determinant;
	const FVector T = Subtract(Origin, A);
	const float U = Dot(T, P) * InverseDeterminant;
	if (U < 0.0f || U > 1.0f)
		return false;
	const FVector Q = Cross(T, Edge1);
	const float V = Dot(Direction, Q) * InverseDeterminant;
	if (V < 0.0f || U + V > 1.0f)
		return false;
	const float Distance = Dot(Edge2, Q) * InverseDeterminant;
	if (!std::isfinite(Distance) || Distance < 0.0f)
		return false;
	OutNormal = Cross(Edge1, Edge2);
	if (!Normalize(OutNormal))
		return false;
	OutDistance = Distance;
	return true;
}
} // namespace

void TiramisuEditorViewportScenePicker::Submit(
	const FEditorViewportSceneSnapshot& Snapshot)
{
	std::scoped_lock Lock(Mutex);
	for (const FEditorStaticMeshId Removed : Snapshot.RemovedStaticMeshes)
		Meshes.erase(Removed.Value);
	for (const FEditorStaticMeshUpload& Upload : Snapshot.StaticMeshes)
	{
		if (!Upload.MeshId.IsValid())
			continue;
		FMesh& Mesh = Meshes[Upload.MeshId.Value];
		if (Mesh.Revision == Upload.Revision)
			continue;
		Mesh.Vertices.assign(Upload.Vertices.begin(), Upload.Vertices.end());
		Mesh.Indices.assign(Upload.Indices.begin(), Upload.Indices.end());
		Mesh.Sections.assign(Upload.Sections.begin(), Upload.Sections.end());
		Mesh.Revision = Upload.Revision;
	}
	Instances.assign(Snapshot.Instances.begin(), Snapshot.Instances.end());
	SceneRevision = Snapshot.Revision;
}

FEditorViewportPickResult TiramisuEditorViewportScenePicker::Pick(
	const FEditorViewportPickRequest& Request) const
{
	FEditorViewportPickResult Result;
	std::scoped_lock Lock(Mutex);
	Result.SceneRevision = SceneRevision;
	FVector Direction = Request.RayDirection;
	if (!Normalize(Direction) || !std::isfinite(Request.MaxDistance) ||
		Request.MaxDistance < 0.0f)
	{
		return Result;
	}

	float Closest = Request.MaxDistance;
	for (const FEditorStaticMeshInstance& Instance : Instances)
	{
		const auto MeshIt = Meshes.find(Instance.MeshId.Value);
		if (MeshIt == Meshes.end())
			continue;
		const FMesh& Mesh = MeshIt->second;
		for (const FEditorStaticMeshSection& Section : Mesh.Sections)
		{
			const u64 SectionEnd = std::min<u64>(
				static_cast<u64>(Section.FirstIndex) + Section.IndexCount,
				Mesh.Indices.size());
			for (u64 Index = Section.FirstIndex;
				Index + 2 < SectionEnd; Index += 3)
			{
				const u32 I0 = Mesh.Indices[Index];
				const u32 I1 = Mesh.Indices[Index + 1];
				const u32 I2 = Mesh.Indices[Index + 2];
				if (I0 >= Mesh.Vertices.size() || I1 >= Mesh.Vertices.size() ||
					I2 >= Mesh.Vertices.size())
				{
					continue;
				}
				const FVector A = TransformPosition(Instance.LocalToWorld,
					Mesh.Vertices[I0].Position);
				const FVector B = TransformPosition(Instance.LocalToWorld,
					Mesh.Vertices[I1].Position);
				const FVector C = TransformPosition(Instance.LocalToWorld,
					Mesh.Vertices[I2].Position);
				float Distance = 0.0f;
				FVector Normal;
				if (!IntersectTriangle(Request.RayOrigin, Direction, A, B, C,
						Request.CullBackFaces, Distance, Normal) || Distance >= Closest)
				{
					continue;
				}
				Closest = Distance;
				Result.Hit = true;
				Result.ObjectId = Instance.ObjectId;
				Result.MeshId = Instance.MeshId;
				Result.MaterialSlot = ResolveEditorMaterialSlot(
					Instance, Section.MaterialSlot);
				Result.WorldPosition = AddScaled(Request.RayOrigin, Direction, Distance);
				Result.WorldNormal = Normal;
				Result.Distance = Distance;
				Result.TriangleIndex = static_cast<u32>(Index / 3);
			}
		}
	}
	return Result;
}
