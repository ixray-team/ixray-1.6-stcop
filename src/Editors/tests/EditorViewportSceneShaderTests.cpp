#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorSceneIndirect.h"
#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportSceneShader.h"
#include "../../xrTiramisuMaterialCore/TiramisuMaterialShaderCompiler.h"

#include <array>
#include <cstddef>
#include <cstring>
#include <iostream>

namespace
{
int Fail(const char* Message, const xr_vector<FMaterialDiagnostic>& Diagnostics = {})
{
	std::cerr << Message << '\n';
	for (const auto& Diagnostic : Diagnostics)
	{
		std::cerr << Diagnostic.Code << ": " << Diagnostic.Message << '\n';
	}
	return 1;
}
} // namespace

int main()
{
	static_assert(
		offsetof(
			FEditorDrawIndexedIndirectEmulatedCommand,
			ShaderBaseVertex
		) == 0
	);
	static_assert(
		offsetof(
			FEditorDrawIndexedIndirectEmulatedCommand,
			ShaderBaseInstance
		) == sizeof(s32)
	);
	static_assert(
		offsetof(FEditorDrawIndexedIndirectEmulatedCommand, Draw) ==
		2 * sizeof(u32)
	);

	const xr_array<FEditorDrawIndexedIndirectCommand, 2> SourceCommands = {{
		{123, 7, 41, -9, 2048},
		{93228, 1, 17907, 820, 8258}
	}};
	const auto CommandsEqual = [](
		const FEditorDrawIndexedIndirectCommand& Left,
		const FEditorDrawIndexedIndirectCommand& Right
	)
	{
		return Left.IndexCount == Right.IndexCount &&
			Left.InstanceCount == Right.InstanceCount &&
			Left.FirstIndex == Right.FirstIndex &&
			Left.BaseVertex == Right.BaseVertex &&
			Left.BaseInstance == Right.BaseInstance;
	};

	xr_vector<u8> VulkanCommands;
	for (const auto& SourceCommand : SourceCommands)
	{
		AppendEditorDrawIndexedIndirectCommand(
			VulkanCommands,
			SourceCommand,
			false
		);
	}
	if (GetEditorDrawIndexedIndirectCommandStride(false) != 20 ||
		VulkanCommands.size() !=
			SourceCommands.size() *
			GetEditorDrawIndexedIndirectCommandStride(false))
	{
		return Fail("Vulkan indirect command ABI is invalid");
	}
	for (size_t Index = 0; Index < SourceCommands.size(); ++Index)
	{
		FEditorDrawIndexedIndirectCommand VulkanCommand;
		std::memcpy(
			&VulkanCommand,
			VulkanCommands.data() + Index * sizeof(VulkanCommand),
			sizeof(VulkanCommand)
		);
		if (!CommandsEqual(VulkanCommand, SourceCommands[Index]))
		{
			return Fail("Vulkan indirect command sequence is invalid");
		}
	}

	xr_vector<u8> D3D12Commands;
	for (const auto& SourceCommand : SourceCommands)
	{
		AppendEditorDrawIndexedIndirectCommand(
			D3D12Commands,
			SourceCommand,
			true
		);
	}
	if (GetEditorDrawIndexedIndirectCommandStride(true) != 28 ||
		D3D12Commands.size() !=
			SourceCommands.size() *
			GetEditorDrawIndexedIndirectCommandStride(true))
	{
		return Fail("D3D12 emulated indirect command ABI is invalid");
	}
	for (size_t Index = 0; Index < SourceCommands.size(); ++Index)
	{
		FEditorDrawIndexedIndirectEmulatedCommand D3D12Command;
		std::memcpy(
			&D3D12Command,
			D3D12Commands.data() + Index * sizeof(D3D12Command),
			sizeof(D3D12Command)
		);
		const auto& SourceCommand = SourceCommands[Index];
		if (D3D12Command.ShaderBaseVertex !=
				SourceCommand.BaseVertex ||
			D3D12Command.ShaderBaseInstance !=
				SourceCommand.BaseInstance ||
			!CommandsEqual(D3D12Command.Draw, SourceCommand))
		{
			return Fail(
				"D3D12 emulated indirect command sequence is invalid"
			);
		}
	}

	TiramisuMaterialShaderCompiler Compiler;
	if (!Compiler.IsAvailable())
	{
		return Fail("DXC is unavailable for the editor viewport shader test");
	}

	constexpr xr_array Backends = {
		EMaterialShaderBackend::D3D12,
		EMaterialShaderBackend::Vulkan
	};
	for (const auto Backend : Backends)
	{
		for (const auto [EntryPoint, Profile] : {
				 xr_pair{"VSMain", "vs_6_6"}, xr_pair{"PSMain", "ps_6_6"}, xr_pair{"VSDebug", "vs_6_6"}, xr_pair{"PSDebug", "ps_6_6"}, xr_pair{"VSOverlay", "vs_6_6"}
			 })
		{
			FMaterialShaderCompileRequest Request;
			Request.Backend = Backend;
			Request.Source.assign(EditorViewportSceneShaderSource);
			Request.SourceName = "editor-viewport-scene.hlsl";
			Request.EntryPoint = EntryPoint;
			Request.TargetProfile = Profile;
			Request.IncludeDirectories = {"gamedata/shaders/r5/common"};
			const auto Result = Compiler.Compile(Request);
			if (!Result.Succeeded())
			{
				return Fail("The editor viewport shader did not compile for both APIs", Result.Diagnostics);
			}
		}
	}
	return 0;
}
