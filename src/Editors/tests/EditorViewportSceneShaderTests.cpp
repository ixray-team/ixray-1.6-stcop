#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorSceneIndirect.h"
#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportSceneShader.h"
#include "../../xrTiramisuMaterialCore/TiramisuMaterialShaderCompiler.h"

#include <array>
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
	const FEditorDrawIndexedIndirectCommand SourceCommand = {
		123,
		7,
		41,
		-9,
		2048
	};
	xr_vector<u8> VulkanCommands;
	AppendEditorDrawIndexedIndirectCommand(
		VulkanCommands,
		SourceCommand,
		false
	);
	FEditorDrawIndexedIndirectCommand VulkanCommand;
	std::memcpy(
		&VulkanCommand,
		VulkanCommands.data(),
		sizeof(VulkanCommand)
	);
	if (VulkanCommands.size() != 20 ||
		VulkanCommand.IndexCount != SourceCommand.IndexCount ||
		VulkanCommand.InstanceCount != SourceCommand.InstanceCount ||
		VulkanCommand.FirstIndex != SourceCommand.FirstIndex ||
		VulkanCommand.BaseVertex != SourceCommand.BaseVertex ||
		VulkanCommand.BaseInstance != SourceCommand.BaseInstance)
	{
		return Fail("Vulkan indirect command ABI is invalid");
	}

	xr_vector<u8> D3D12Commands;
	AppendEditorDrawIndexedIndirectCommand(
		D3D12Commands,
		SourceCommand,
		true
	);
	FEditorDrawIndexedIndirectEmulatedCommand D3D12Command;
	std::memcpy(
		&D3D12Command,
		D3D12Commands.data(),
		sizeof(D3D12Command)
	);
	if (D3D12Commands.size() != 28 ||
		D3D12Command.ShaderBaseVertex != SourceCommand.BaseVertex ||
		D3D12Command.ShaderBaseInstance != SourceCommand.BaseInstance ||
		D3D12Command.Draw.IndexCount != SourceCommand.IndexCount ||
		D3D12Command.Draw.BaseInstance != SourceCommand.BaseInstance)
	{
		return Fail("D3D12 emulated indirect command ABI is invalid");
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
