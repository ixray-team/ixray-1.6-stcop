#include "LegacyDecalProjection.h"

#include <algorithm>
#include <cmath>

namespace Tiramisu::Scene
{
namespace
{
using FVector3 = xr_array<float, 3>;

[[nodiscard]] bool IsFinite(const FVector3& Value)
{
	return std::ranges::all_of(Value, [](const float Item)
	{
		return std::isfinite(Item);
	});
}

[[nodiscard]] FVector3 Subtract(
	const FVector3& Left,
	const FVector3& Right
)
{
	return {
		Left[0] - Right[0],
		Left[1] - Right[1],
		Left[2] - Right[2]
	};
}

[[nodiscard]] FVector3 Multiply(const FVector3& Value, const float Scale)
{
	return {Value[0] * Scale, Value[1] * Scale, Value[2] * Scale};
}

void AddScaled(FVector3& Target, const FVector3& Value, const float Scale)
{
	for (size_t Axis = 0; Axis < Target.size(); ++Axis)
	{
		Target[Axis] += Value[Axis] * Scale;
	}
}

[[nodiscard]] float LengthSquared(const FVector3& Value)
{
	return Value[0] * Value[0] + Value[1] * Value[1] +
		Value[2] * Value[2];
}

[[nodiscard]] bool Normalize(FVector3& Value)
{
	const float Squared = LengthSquared(Value);
	if (!std::isfinite(Squared) || Squared <= 1.0e-12f)
	{
		return false;
	}
	Value = Multiply(Value, 1.0f / std::sqrt(Squared));
	return true;
}

[[nodiscard]] FVector3 Cross(
	const FVector3& Left,
	const FVector3& Right
)
{
	return {
		Left[1] * Right[2] - Left[2] * Right[1],
		Left[2] * Right[0] - Left[0] * Right[2],
		Left[0] * Right[1] - Left[1] * Right[0]
	};
}

void SetAxis(
	xr_array<float, 16>& Transform,
	const size_t Offset,
	const FVector3& Axis,
	const float Scale
)
{
	Transform[Offset] = Axis[0] * Scale;
	Transform[Offset + 1] = Axis[1] * Scale;
	Transform[Offset + 2] = Axis[2] * Scale;
}
} // namespace

FLegacyDecalProjectionResult BuildLegacyDecalProjection(
	const xr_vector<FLegacyDecalVertex>& Vertices,
	const float Width,
	const float Height
)
{
	FLegacyDecalProjectionResult Result;
	if (Vertices.size() < 3)
	{
		Result.DiagnosticCode = "decal_projection.too_few_vertices";
		return Result;
	}
	if (!std::isfinite(Width) || !std::isfinite(Height))
	{
		Result.DiagnosticCode = "decal_projection.non_finite_size";
		return Result;
	}
	for (const FLegacyDecalVertex& Vertex : Vertices)
	{
		if (!IsFinite(Vertex.Position) ||
			!std::isfinite(Vertex.TexCoord[0]) ||
			!std::isfinite(Vertex.TexCoord[1]))
		{
			Result.DiagnosticCode = "decal_projection.non_finite_vertex";
			return Result;
		}
	}

	FVector3 Tangent = {};
	FVector3 Bitangent = {};
	bool HasBasis = false;
	for (size_t Index = 0; Index + 2 < Vertices.size(); Index += 3)
	{
		const FLegacyDecalVertex& A = Vertices[Index];
		const FLegacyDecalVertex& B = Vertices[Index + 1];
		const FLegacyDecalVertex& C = Vertices[Index + 2];
		const FVector3 Edge1 = Subtract(B.Position, A.Position);
		const FVector3 Edge2 = Subtract(C.Position, A.Position);
		const float Du1 = B.TexCoord[0] - A.TexCoord[0];
		const float Dv1 = B.TexCoord[1] - A.TexCoord[1];
		const float Du2 = C.TexCoord[0] - A.TexCoord[0];
		const float Dv2 = C.TexCoord[1] - A.TexCoord[1];
		const float Determinant = Du1 * Dv2 - Du2 * Dv1;
		if (std::abs(Determinant) <= 1.0e-6f)
		{
			continue;
		}
		const float InverseDeterminant = 1.0f / Determinant;
		Tangent = Multiply(Edge1, Dv2);
		AddScaled(Tangent, Edge2, -Dv1);
		Tangent = Multiply(Tangent, InverseDeterminant);
		Bitangent = Multiply(Edge2, Du1);
		AddScaled(Bitangent, Edge1, -Du2);
		Bitangent = Multiply(Bitangent, InverseDeterminant);
		if (LengthSquared(Tangent) > 1.0e-12f &&
			LengthSquared(Bitangent) > 1.0e-12f)
		{
			HasBasis = true;
			break;
		}
	}
	if (!HasBasis)
	{
		Result.DiagnosticCode = "decal_projection.degenerate_uv";
		return Result;
	}

	FVector3 Center = {};
	for (const FLegacyDecalVertex& Vertex : Vertices)
	{
		FVector3 ProjectorCenter = Vertex.Position;
		AddScaled(ProjectorCenter, Tangent, 0.5f - Vertex.TexCoord[0]);
		AddScaled(ProjectorCenter, Bitangent, 0.5f - Vertex.TexCoord[1]);
		AddScaled(Center, ProjectorCenter, 1.0f);
	}
	Center = Multiply(Center, 1.0f / static_cast<float>(Vertices.size()));
	if (!Normalize(Tangent) || !Normalize(Bitangent))
	{
		Result.DiagnosticCode = "decal_projection.degenerate_basis";
		return Result;
	}
	FVector3 Normal = Cross(Tangent, Bitangent);
	if (!Normalize(Normal))
	{
		Result.DiagnosticCode = "decal_projection.degenerate_basis";
		return Result;
	}
	Bitangent = Cross(Normal, Tangent);
	if (!Normalize(Bitangent))
	{
		Result.DiagnosticCode = "decal_projection.degenerate_basis";
		return Result;
	}

	SetAxis(Result.LocalToWorld, 0, Tangent, std::max(Width, 0.01f));
	SetAxis(Result.LocalToWorld, 4, Bitangent, std::max(Height, 0.01f));
	const float Depth = std::clamp(
		std::max(Width, Height) * 0.1f,
		0.05f,
		0.5f
	);
	SetAxis(Result.LocalToWorld, 8, Normal, Depth);
	Result.LocalToWorld[12] = Center[0];
	Result.LocalToWorld[13] = Center[1];
	Result.LocalToWorld[14] = Center[2];
	return Result;
}
} // namespace Tiramisu::Scene
