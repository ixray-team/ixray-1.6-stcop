#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportSceneShader.h"
#include "../../xrTiramisuMaterialCore/TiramisuMaterialShaderCompiler.h"

#include <array>
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
