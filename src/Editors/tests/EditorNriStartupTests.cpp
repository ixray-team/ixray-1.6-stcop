#include "../LevelEditor/Renderer/Tiramisu/TiramisuEditorNriStartup.h"
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
	const FEditorNriStartupConfig Default = ParseEditorNriStartupConfig("");
	if (Default.Enabled || Default.Api != ETiramisuEditorGraphicsApi::Vulkan ||
		Default.DeterministicTest.Enabled || !Default.IsValid())
	{
		return Fail("The NRI editor must remain opt-in and default to Vulkan");
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

	if (ParseEditorNriStartupConfig("-tiramisu-editor-disabled").Enabled)
	{
		return Fail("A partial command-line token enabled the NRI editor");
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

	if (ParseEditorNriStartupConfig(
			"-rdbg -render-deterministic"
		)
			.IsValid())
	{
		return Fail("The editor deterministic mode requires -tiramisu-editor");
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
