#include "../LevelEditor/Renderer/Tiramisu/TiramisuEditorNriStartup.h"
#include "../xrECore/Editor/EditorWindowPlacement.h"
#include "../../xrCore/RenderDebugPolicy.h"

#include <cstdlib>
#include <iostream>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return EXIT_FAILURE;
}
} // namespace

int main()
{
	constexpr FEditorWindowPlacementRect PrimaryDisplay{0, 0, 1920, 1080};
	constexpr FEditorWindowPlacementRect LeftDisplay{-1920, 0, 1920, 1080};
	if (!IsEditorWindowTitleAreaVisible(
			{100, 100, 1280, 720},
			PrimaryDisplay
		) ||
		!IsEditorWindowTitleAreaVisible(
			{-1800, 100, 1280, 720},
			LeftDisplay
		) ||
		IsEditorWindowTitleAreaVisible(
			{-32000, -32000, 1280, 720},
			PrimaryDisplay
		) ||
		IsEditorWindowTitleAreaVisible(
			{1900, 1060, 1280, 720},
			PrimaryDisplay
		))
	{
		return Fail("The editor window placement visibility policy is invalid");
	}

	const FEditorNriStartupConfig Default = ParseEditorNriStartupConfig("");
	if (!Default.Enabled || Default.Api != ETiramisuEditorGraphicsApi::Vulkan ||
		Default.DeterministicTest.Enabled || !Default.IsValid())
	{
		return Fail("LevelEditor must use Tiramisu and Vulkan by default");
	}

	const FEditorNriStartupConfig Vulkan =
		ParseEditorNriStartupConfig("-tiramisu-editor -rdbg");
	if (!Vulkan.Enabled || Vulkan.Api != ETiramisuEditorGraphicsApi::Vulkan)
	{
		return Fail("The Vulkan NRI editor command line was parsed incorrectly");
	}

	const FEditorNriStartupConfig D3D12 =
		ParseEditorNriStartupConfig("-dx12\t-tiramisu-editor");
	if (!D3D12.Enabled || D3D12.Api != ETiramisuEditorGraphicsApi::D3D12)
	{
		return Fail("The D3D12 NRI editor command line was parsed incorrectly");
	}

	if (!ParseEditorNriStartupConfig("-tiramisu-editor-disabled").Enabled)
	{
		return Fail("A partial compatibility token disabled Tiramisu");
	}

	const FEditorNriStartupConfig Deterministic =
		ParseEditorNriStartupConfig(
			"-tiramisu-editor -rdbg -render-deterministic"
		);
	if (!Deterministic.Enabled || !Deterministic.IsValid() ||
		!Deterministic.DeterministicTest.Enabled ||
		!Deterministic.DeterministicTest.RequiredRdbgPresent ||
		Deterministic.DeterministicTest.RandomSeed != 0x13572468u ||
		Deterministic.DeterministicTest.FixedDeltaSeconds != 1.0f / 60.0f ||
		Deterministic.DeterministicTest.FixedShaderTimeSeconds != 123.0f ||
		Deterministic.DeterministicTest.FixedWeatherTimeSeconds != 43200.0f ||
		Deterministic.DeterministicTest.FixedExposure != 1.0f)
	{
		return Fail("The deterministic GPU test policy was parsed incorrectly");
	}

	const FEditorNriStartupConfig HiddenDeterministic =
		ParseEditorNriStartupConfig(
			"-tiramisu-editor -rdbg -render-deterministic "
			"-editor-test-hidden"
		);
	if (!HiddenDeterministic.IsValid() ||
		!HiddenDeterministic.HiddenTestWindow)
	{
		return Fail("The hidden editor smoke mode was parsed incorrectly");
	}
	if (ParseEditorNriStartupConfig(
			"-tiramisu-editor -rdbg -editor-test-hidden"
		).IsValid())
	{
		return Fail("A hidden editor window must require deterministic mode");
	}
	if (ParseEditorNriStartupConfig(
			"-tiramisu-editor -rdbg -render-deterministic "
			"-editor-test-hidden-disabled"
		).HiddenTestWindow)
	{
		return Fail("A partial hidden-window token enabled the test mode");
	}

	if (ParseEditorNriStartupConfig(
			"-tiramisu-editor -render-deterministic"
		)
			.IsValid())
	{
		return Fail("Deterministic GPU tests must require the exact -rdbg flag");
	}

	if (ParseEditorNriStartupConfig(
			"-tiramisu-editor -rdebug -render-deterministic"
		)
			.IsValid())
	{
		return Fail("-rdebug must not satisfy the deterministic -rdbg contract");
	}

	const FEditorNriStartupConfig DefaultDeterministic =
		ParseEditorNriStartupConfig("-rdbg -render-deterministic");
	if (!DefaultDeterministic.Enabled || !DefaultDeterministic.IsValid() ||
		!DefaultDeterministic.DeterministicTest.Enabled)
	{
		return Fail("The default Tiramisu editor rejected deterministic mode");
	}

	if (ParseEditorNriStartupConfig(
			"-tiramisu-editor -rdbg -render-deterministic-disabled"
		)
			.DeterministicTest.Enabled)
	{
		return Fail("A partial deterministic token enabled the test mode");
	}

	const FRenderDebugPolicy DebugWithoutRenderDoc =
		ResolveRenderDebugPolicy("-rdbg", false);
	if (!DebugWithoutRenderDoc.ShaderDebugInfo ||
		!DebugWithoutRenderDoc.GraphicsApiValidation ||
		!DebugWithoutRenderDoc.NriValidation ||
		DebugWithoutRenderDoc.ValidationSuppressedByRenderDoc)
	{
		return Fail("The normal -rdbg validation policy is incorrect");
	}

	const FRenderDebugPolicy DebugWithRenderDoc =
		ResolveRenderDebugPolicy("-rdbg -renderdoc", true);
	if (!DebugWithRenderDoc.ShaderDebugInfo ||
		DebugWithRenderDoc.GraphicsApiValidation ||
		DebugWithRenderDoc.NriValidation ||
		!DebugWithRenderDoc.ValidationSuppressedByRenderDoc)
	{
		return Fail("RenderDoc did not suppress only the conflicting validation layers");
	}

	const FRenderDebugPolicy ForcedValidation = ResolveRenderDebugPolicy(
		"-rdbg -renderdoc -renderdoc-validation", true
	);
	if (!ForcedValidation.ShaderDebugInfo ||
		!ForcedValidation.GraphicsApiValidation ||
		!ForcedValidation.NriValidation ||
		ForcedValidation.ValidationSuppressedByRenderDoc ||
		!ForcedValidation.ForceRenderDocValidation)
	{
		return Fail("The explicit RenderDoc validation override is incorrect");
	}

	const FRenderDebugPolicy PartialFlag =
		ResolveRenderDebugPolicy("-rdbg-disabled", true);
	if (PartialFlag.ShaderDebugInfo || PartialFlag.GraphicsApiValidation ||
		PartialFlag.NriValidation)
	{
		return Fail("A partial debug token enabled renderer validation");
	}

	return EXIT_SUCCESS;
}
