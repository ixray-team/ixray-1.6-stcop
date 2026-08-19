#include "TiramisuEditorViewportSceneCulling.h"

#include <algorithm>
#include <cmath>

namespace
{
using FVector4 = xr_array<float, 4>;

[[nodiscard]] FVector4 TransformPoint(
	const xr_array<float, 16>& Matrix,
	const FVector4& Point
) noexcept
{
	FVector4 Result = {};
	for (size_t Row = 0; Row < 4; ++Row)
	{
		for (size_t Column = 0; Column < 4; ++Column)
		{
			Result[Row] += Matrix[Column * 4 + Row] * Point[Column];
		}
	}
	return Result;
}

[[nodiscard]] bool IsFinite(const FVector4& Value) noexcept
{
	return std::ranges::all_of(Value, [](const float Component)
	{
		return std::isfinite(Component);
	});
}
} // namespace

bool IsTiramisuEditorDecalVisible(
	const FEditorDecalInstance& Decal,
	const FEditorViewportCamera& Camera
) noexcept
{
	constexpr xr_array<FVector4, 8> Corners = {{
		{{-0.5f, -0.5f, -0.5f, 1.0f}},
		{{0.5f, -0.5f, -0.5f, 1.0f}},
		{{0.5f, 0.5f, -0.5f, 1.0f}},
		{{-0.5f, 0.5f, -0.5f, 1.0f}},
		{{-0.5f, -0.5f, 0.5f, 1.0f}},
		{{0.5f, -0.5f, 0.5f, 1.0f}},
		{{0.5f, 0.5f, 0.5f, 1.0f}},
		{{-0.5f, 0.5f, 0.5f, 1.0f}}
	}};
	xr_array<FVector4, Corners.size()> ClipCorners = {};
	for (size_t Index = 0; Index < Corners.size(); ++Index)
	{
		const FVector4 World = TransformPoint(
			Decal.LocalToWorld,
			Corners[Index]
		);
		ClipCorners[Index] = TransformPoint(
			Camera.ViewProjection,
			World
		);
		if (!IsFinite(ClipCorners[Index]))
		{
			// Некорректные данные должны быть отклонены mailbox, но culling
			// остаётся консервативным и не скрывает объект молча.
			return true;
		}
	}

	const auto AllOutside = [&ClipCorners](const auto& Predicate)
	{
		return std::ranges::all_of(ClipCorners, Predicate);
	};
	if (AllOutside([](const FVector4& Point)
			{ return Point[0] < -Point[3]; }) ||
		AllOutside([](const FVector4& Point)
			{ return Point[0] > Point[3]; }) ||
		AllOutside([](const FVector4& Point)
			{ return Point[1] < -Point[3]; }) ||
		AllOutside([](const FVector4& Point)
			{ return Point[1] > Point[3]; }) ||
		AllOutside([](const FVector4& Point)
			{ return Point[2] < -Point[3]; }) ||
		AllOutside([](const FVector4& Point)
			{ return Point[2] > Point[3]; }))
	{
		return false;
	}
	return true;
}
