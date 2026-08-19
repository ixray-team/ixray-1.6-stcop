#pragma once

#include "../../../xrCore/xrCore.h"

// Переносимая команда indexed indirect для Vulkan и D3D12 без зависимости
// editor tests от NRI headers.
struct FEditorDrawIndexedIndirectCommand
{
	u32 IndexCount = 0;
	u32 InstanceCount = 0;
	u32 FirstIndex = 0;
	s32 BaseVertex = 0;
	u32 BaseInstance = 0;
};

// D3D12 layout содержит дубли base values для эмуляции draw parameters NRI.
struct FEditorDrawIndexedIndirectEmulatedCommand
{
	s32 ShaderBaseVertex = 0;
	u32 ShaderBaseInstance = 0;
	FEditorDrawIndexedIndirectCommand Draw;
};

static_assert(sizeof(FEditorDrawIndexedIndirectCommand) == 20);
static_assert(sizeof(FEditorDrawIndexedIndirectEmulatedCommand) == 28);

[[nodiscard]] inline u32 GetEditorDrawIndexedIndirectCommandStride(
	const bool EmulateDrawParameters
) noexcept
{
	return EmulateDrawParameters
		? sizeof(FEditorDrawIndexedIndirectEmulatedCommand)
		: sizeof(FEditorDrawIndexedIndirectCommand);
}

inline void AppendEditorDrawIndexedIndirectCommand(
	xr_vector<u8>& Destination,
	const FEditorDrawIndexedIndirectCommand& Draw,
	const bool EmulateDrawParameters
)
{
	if (EmulateDrawParameters)
	{
		const FEditorDrawIndexedIndirectEmulatedCommand Command = {
			Draw.BaseVertex,
			Draw.BaseInstance,
			Draw
		};
		const auto* Bytes = reinterpret_cast<const u8*>(&Command);
		Destination.insert(
			Destination.end(),
			Bytes,
			Bytes + sizeof(Command)
		);
		return;
	}
	const auto* Bytes = reinterpret_cast<const u8*>(&Draw);
	Destination.insert(
		Destination.end(),
		Bytes,
		Bytes + sizeof(Draw)
	);
}
