#pragma once

#include <string_view>

struct FRenderDebugPolicy
{
	bool ShaderDebugInfo = false;
	bool GraphicsApiValidation = false;
	bool NriValidation = false;
	bool RenderDocActive = false;
	bool ValidationSuppressedByRenderDoc = false;
	bool ForceRenderDocValidation = false;
};

[[nodiscard]] inline bool HasRenderCommandLineFlag(
	const std::string_view CommandLine, const std::string_view Flag
) noexcept
{
	std::size_t Position = 0;
	while (Position < CommandLine.size())
	{
		while (Position < CommandLine.size() &&
			   (CommandLine[Position] == ' ' || CommandLine[Position] == '\t'))
		{
			++Position;
		}
		const std::size_t Begin = Position;
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

// -rdbg keeps shader debug information under RenderDoc, but conflicting
// graphics API and NRI validation layers are suppressed by default.
[[nodiscard]] inline FRenderDebugPolicy ResolveRenderDebugPolicy(
	const std::string_view CommandLine, const bool RenderDocActive
) noexcept
{
	FRenderDebugPolicy Result;
	Result.RenderDocActive = RenderDocActive;
	Result.ShaderDebugInfo =
		HasRenderCommandLineFlag(CommandLine, "-rdebug") ||
		HasRenderCommandLineFlag(CommandLine, "-rdbg");
	const bool ValidationRequested =
		Result.ShaderDebugInfo ||
		HasRenderCommandLineFlag(CommandLine, "-d3ddebug") ||
		HasRenderCommandLineFlag(CommandLine, "-vkdebug");
	Result.ForceRenderDocValidation =
		HasRenderCommandLineFlag(CommandLine, "-renderdoc-validation");
	Result.ValidationSuppressedByRenderDoc =
		ValidationRequested && RenderDocActive &&
		!Result.ForceRenderDocValidation;
	Result.GraphicsApiValidation =
		ValidationRequested && !Result.ValidationSuppressedByRenderDoc;
	Result.NriValidation = Result.GraphicsApiValidation;
	return Result;
}
