#pragma once

#include "TiramisuEditorTypes.h"

#include "TiramisuEditorNriTypes.h"
#include "../../../../xrCore/RenderTestPolicy.h"

#include <string_view>

struct FEditorNriStartupConfig
{
	// LevelEditor использует xrRenderTiramisu по умолчанию. Поле сохранено в
	// контракте запуска, чтобы остальные editor executables не зависели от
	// renderer-specific типов и существующие launch-конфигурации не ломались.
	bool Enabled = true;
	bool HiddenTestWindow = false;
	ETiramisuEditorGraphicsApi Api = ETiramisuEditorGraphicsApi::Vulkan;
	FRenderDeterministicTestPolicy DeterministicTest;

	[[nodiscard]] constexpr bool IsValid() const noexcept
	{
		return DeterministicTest.IsValid() &&
			   (!DeterministicTest.Enabled || Enabled) &&
			   (!HiddenTestWindow || DeterministicTest.Enabled);
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
	Result.HiddenTestWindow = HasEditorCommandLineFlag(
		CommandLine,
		"-editor-test-hidden"
	);
	Result.Api = HasEditorCommandLineFlag(CommandLine, "-dx12")
					 ? ETiramisuEditorGraphicsApi::D3D12
					 : ETiramisuEditorGraphicsApi::Vulkan;
	Result.DeterministicTest =
		ResolveRenderDeterministicTestPolicy(CommandLine);
	return Result;
}
