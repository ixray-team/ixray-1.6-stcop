#pragma once

#include "TiramisuEditorTypes.h"

#include "TiramisuEditorNriTypes.h"
#include "../../../../xrCore/RenderTestPolicy.h"

#include <string_view>

struct FEditorNriStartupConfig
{
	bool Enabled = false;
	ETiramisuEditorGraphicsApi Api = ETiramisuEditorGraphicsApi::Vulkan;
	FRenderDeterministicTestPolicy DeterministicTest;

	[[nodiscard]] constexpr bool IsValid() const noexcept
	{
		return DeterministicTest.IsValid() &&
			   (!DeterministicTest.Enabled || Enabled);
	}
};

[[nodiscard]] inline bool HasEditorCommandLineFlag(
	const xr_string_view CommandLine, const xr_string_view Flag
) noexcept
{
	size_t Position = 0;
	while (Position < CommandLine.size())
	{
		while (Position < CommandLine.size() &&
			   (CommandLine[Position] == ' ' || CommandLine[Position] == '\t'))
		{
			++Position;
		}
		const size_t Begin = Position;
		while (Position < CommandLine.size() &&
			   CommandLine[Position] != ' ' && CommandLine[Position] != '\t')
		{
			++Position;
		}
		if (CommandLine.substr(Begin, Position - Begin) == Flag)
		{
			return true;
		}
	}
	return false;
}

[[nodiscard]] inline FEditorNriStartupConfig ParseEditorNriStartupConfig(
	const xr_string_view CommandLine
) noexcept
{
	FEditorNriStartupConfig Result;
	Result.Enabled = HasEditorCommandLineFlag(CommandLine, "-tiramisu-editor");
	Result.Api = HasEditorCommandLineFlag(CommandLine, "-dx12")
					 ? ETiramisuEditorGraphicsApi::D3D12
					 : ETiramisuEditorGraphicsApi::Vulkan;
	Result.DeterministicTest =
		ResolveRenderDeterministicTestPolicy(CommandLine);
	return Result;
}
