#pragma once

#include "MaterialAsset.h"

#include <filesystem>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

enum class EMaterialShaderBackend : u8
{
	D3D12,
	Vulkan
};

// Результат сборки engine template и material implementation в единый HLSL source.
struct FMaterialSourceAssemblyResult
{
	xr_string Source;
	xr_string ParameterDeclarations;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Разрешает два generated include из MaterialTemplate.hlsl и добавляет
// engine-owned validation entry point. File IO остаётся за границей xrTiramisuMaterialCore.
// Собирает engine-owned template и material implementation в единый source.
[[nodiscard]] FMaterialSourceAssemblyResult AssembleMaterialShaderSource(
	const FMaterialAsset& Asset,
	xr_string_view TemplateSource,
	xr_string_view ImplementationSource,
	const FMaterialStaticParameterSet& StaticParameters,
	bool AppendValidationEntryPoint = true
);

// Собирает engine-owned template и material implementation в единый source.
[[nodiscard]] FMaterialSourceAssemblyResult AssembleMaterialShaderSourceForPass(
	const FMaterialAsset& Asset,
	xr_string_view TemplateSource,
	xr_string_view ImplementationSource,
	const FMaterialStaticParameterSet& StaticParameters,
	xr_string_view PassSource,
	xr_string_view PassSourceName
);

// Полностью детерминированный запрос DXC для одной material stage.
struct FMaterialShaderCompileRequest
{
	EMaterialShaderBackend Backend = EMaterialShaderBackend::D3D12;
	xr_string Source;
	xr_string SourceName = "material-generated.hlsl";
	xr_string EntryPoint = "Main";
	xr_string TargetProfile = "ps_6_6";
	xr_vector<std::filesystem::path> IncludeDirectories;
	xr_vector<xr_string> Defines;
	bool Debug = false;
	bool WarningsAsErrors = true;
};

// Shader bytecode, reflection metadata и diagnostics одного DXC запуска.
struct FMaterialShaderCompileResult
{
	xr_vector<u8> Bytecode;
	xr_vector<FMaterialDiagnostic> Diagnostics;

	[[nodiscard]] bool Succeeded() const noexcept;
};

// Backend-neutral, NRI-independent DXC boundary shared by renderer tools,
// cooker, editor and tests. Pipeline creation deliberately remains renderer-owned.
class TiramisuMaterialShaderCompiler
{
public:
	TiramisuMaterialShaderCompiler();
	~TiramisuMaterialShaderCompiler();

	TiramisuMaterialShaderCompiler(TiramisuMaterialShaderCompiler&&) noexcept;
	TiramisuMaterialShaderCompiler& operator=(TiramisuMaterialShaderCompiler&&) noexcept;
	TiramisuMaterialShaderCompiler(const TiramisuMaterialShaderCompiler&) = delete;
	TiramisuMaterialShaderCompiler& operator=(const TiramisuMaterialShaderCompiler&) = delete;

	// Выполняет одинаковую DXC-компиляцию для editor, cooker и development runtime.
	[[nodiscard]] bool IsAvailable() const noexcept;
	[[nodiscard]] FMaterialShaderCompileResult Compile(const FMaterialShaderCompileRequest& Request) const;

private:
	struct FImpl;
	std::unique_ptr<FImpl> Impl;
};
