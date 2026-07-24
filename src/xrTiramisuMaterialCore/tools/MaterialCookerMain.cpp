#include "LegacyMaterialResolver.h"
#include "MaterialAsset.h"
#include "MaterialBundle.h"
#include "MaterialGraph.h"
#include "TiramisuMaterialShaderCompiler.h"

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <vector>


namespace
{
struct FArguments
{
    std::filesystem::path Input;
    std::filesystem::path ShaderRoot;
    std::filesystem::path Output;
    bool ValidateOnly = false;
    bool DebugRuntime = false;
};

xr_optional<FArguments> ParseArguments(const int Count, char** Values)
{
    FArguments Result;
    for (int Index = 1; Index < Count; ++Index)
    {
        const xr_string Argument = Values[Index];
        if (Argument == "--validate-only")
            Result.ValidateOnly = true;
        else if (Argument == "-rdbg")
            Result.DebugRuntime = true;
        else if ((Argument == "--input" || Argument == "--shader-root" || Argument == "--output") && Index + 1 < Count)
        {
            const std::filesystem::path Value = Values[++Index];
            if (Argument == "--input") Result.Input = Value;
            else if (Argument == "--shader-root") Result.ShaderRoot = Value;
            else Result.Output = Value;
        }
        else
            return std::nullopt;
    }
    if (Result.Input.empty() || (!Result.ValidateOnly && Result.Output.empty()))
        return std::nullopt;
    if (Result.ShaderRoot.empty())
        Result.ShaderRoot = Result.Input.parent_path() / "shaders" / "r5";
    return Result;
}

xr_string ReadText(const std::filesystem::path& Path)
{
    std::ifstream Stream(Path, std::ios::binary);
    if (!Stream)
        throw std::runtime_error("Cannot open " + Path.generic_string());
    std::ostringstream Text;
    Text << Stream.rdbuf();
    return Text.str();
}

bool EndsWith(const xr_string& Value, const xr_string_view Suffix)
{
    return Value.size() >= Suffix.size() && Value.compare(Value.size() - Suffix.size(), Suffix.size(), Suffix) == 0;
}

void PrintDiagnostics(const xr_vector<FMaterialDiagnostic>& Diagnostics, const xr_string_view Asset)
{
    for (const FMaterialDiagnostic& Diagnostic : Diagnostics)
    {
        const char* Severity = Diagnostic.Severity == EMaterialDiagnosticSeverity::Error ? "error" :
            Diagnostic.Severity == EMaterialDiagnosticSeverity::Warning ? "warning" : "info";
        std::cerr << Severity << " [" << Diagnostic.Code << "] " << Asset << ": " << Diagnostic.Message << '\n';
    }
}

FMaterialStaticParameterSet DefaultStaticParameters(const FMaterialAsset& Asset)
{
    FMaterialStaticParameterSet Result;
    for (const FMaterialParameterDefinition& Parameter : Asset.Parameters)
        if (Parameter.IsStatic()) Result.emplace(Parameter.Id, Parameter.DefaultValue);
    return Result;
}

void AddPermutation(xr_vector<FMaterialStaticParameterSet>& Permutations, FMaterialStaticParameterSet Parameters)
{
    const auto Existing = std::ranges::find(Permutations, Parameters);
    if (Existing == Permutations.end())
        Permutations.push_back(std::move(Parameters));
}

xr_string BackendName(const EMaterialShaderBackend Backend)
{
    return Backend == EMaterialShaderBackend::D3D12 ? "d3d12" : "vulkan";
}
} // namespace

int main(const int ArgumentCount, char** ArgumentValues)
{
    const xr_optional<FArguments> Arguments = ParseArguments(ArgumentCount, ArgumentValues);
    if (!Arguments)
    {
        std::cerr << "Usage: xrMaterialCooker --input <render_materials> [--shader-root <shaders/r5>] "
                     "[--output <bundle>] [--validate-only] [-rdbg]\n";
        return 2;
    }

    try
    {
        xr_vector<std::filesystem::path> MasterPaths;
        xr_vector<std::filesystem::path> InstancePaths;
        xr_optional<std::filesystem::path> LegacyMapPath;
        for (const std::filesystem::directory_entry& Entry : std::filesystem::recursive_directory_iterator(Arguments->Input))
        {
            if (!Entry.is_regular_file()) continue;
            const xr_string Name = Entry.path().filename().string();
            if (EndsWith(Name, ".material-instance.json")) InstancePaths.push_back(Entry.path());
            else if (EndsWith(Name, ".material.json")) MasterPaths.push_back(Entry.path());
            else if (Name == "legacy-map.json") LegacyMapPath = Entry.path();
        }
        std::ranges::sort(MasterPaths);
        std::ranges::sort(InstancePaths);

        TiramisuMaterialLibrary Library;
        xr_vector<FMaterialAsset> Masters;
        xr_vector<FMaterialInstanceAsset> Instances;
        bool Failed = false;
        for (const std::filesystem::path& Path : MasterPaths)
        {
            const xr_string SourceReference =
                std::filesystem::relative(Path, Arguments->Input).generic_string();
            FMaterialAssetParseResult Parsed = ParseMaterialAssetJson(
                ReadText(Path), SourceReference);
            PrintDiagnostics(Parsed.Diagnostics, Path.generic_string());
            if (!Parsed.Succeeded()) { Failed = true; continue; }
            FMaterialRegistrationResult Registered = Library.RegisterMaster(Parsed.Value);
            PrintDiagnostics(Registered.Diagnostics, Path.generic_string());
            if (!Registered.Succeeded()) { Failed = true; continue; }
            Masters.push_back(std::move(Parsed.Value));
        }
        for (const std::filesystem::path& Path : InstancePaths)
        {
            const xr_string SourceReference =
                std::filesystem::relative(Path, Arguments->Input).generic_string();
            FMaterialInstanceParseResult Parsed = ParseMaterialInstanceJson(
                ReadText(Path), SourceReference);
            PrintDiagnostics(Parsed.Diagnostics, Path.generic_string());
            if (!Parsed.Succeeded()) { Failed = true; continue; }
            FMaterialRegistrationResult Registered = Library.RegisterInstance(Parsed.Value);
            PrintDiagnostics(Registered.Diagnostics, Path.generic_string());
            if (!Registered.Succeeded()) { Failed = true; continue; }
            Instances.push_back(std::move(Parsed.Value));
        }
        if (LegacyMapPath)
        {
            const FLegacyMaterialMapParseResult Parsed = ParseLegacyMaterialMapJson(ReadText(*LegacyMapPath));
            PrintDiagnostics(Parsed.Diagnostics, LegacyMapPath->generic_string());
            Failed |= !Parsed.Succeeded();
        }
        else
        {
            std::cerr << "error [cooker.missing_legacy_map] legacy-map.json was not found.\n";
            Failed = true;
        }
        if (Failed)
            return 1;

        FMaterialBundle Bundle;
        xr_map<xr_string, xr_vector<FMaterialStaticParameterSet>> Permutations;
        for (const FMaterialAsset& Master : Masters)
            AddPermutation(Permutations[Master.Id.Value], DefaultStaticParameters(Master));

        for (const FMaterialAsset& Master : Masters)
        {
            FMaterialBundleRecord Record;
            Record.Type = EMaterialBundleRecordType::Master;
            Record.Id = Master.Id;
            Record.MasterId = Master.Id;
            Record.SourcePath = Master.SourcePath;
            Record.AssetPayload = SerializeMaterialAssetJson(Master);
            Record.Dependencies = Master.Dependencies;
            if (Master.Implementation.Type == EMaterialImplementationType::Graph)
            {
                FMaterialGraphCompileOptions Options;
                Options.Parameters = Master.Parameters;
                for (const FMaterialParameterDefinition& Parameter : Master.Parameters)
                    if (Parameter.IsStatic()) Options.StaticParameters.emplace(Parameter.Id, Parameter.DefaultValue);
                const FMaterialGraphCompileResult Compiled = CompileMaterialGraph(Master.Implementation.Graph, Options);
                PrintDiagnostics(Compiled.Diagnostics, Master.SourcePath);
                if (!Compiled.Succeeded()) { Failed = true; continue; }
                Record.GeneratedHlsl = Compiled.GeneratedHlsl;
            }
            else
                Record.GeneratedHlsl = ReadText(Arguments->ShaderRoot / Master.Implementation.Source.c_str());
            Bundle.Records.push_back(std::move(Record));
        }

        for (const FMaterialInstanceAsset& Instance : Instances)
        {
            const FMaterialResolveResult Resolved = Library.Resolve(Instance.Id.Value);
            PrintDiagnostics(Resolved.Diagnostics, Instance.SourcePath);
            if (!Resolved.Succeeded()) { Failed = true; continue; }
            AddPermutation(Permutations[Resolved.Value.MasterId.Value], Resolved.Value.StaticParameters);

            FMaterialInstanceAsset Flattened = Instance;
            Flattened.Parent = Resolved.Value.MasterId.Value;
            Flattened.Overrides = Resolved.Value.Parameters;
            Flattened.StaticOverrides = Resolved.Value.StaticParameters;
            FMaterialBundleRecord Record;
            Record.Type = EMaterialBundleRecordType::FlattenedInstance;
            Record.Id = Instance.Id;
            Record.MasterId = Resolved.Value.MasterId;
            Record.SourcePath = Instance.SourcePath;
            Record.AssetPayload = SerializeMaterialInstanceJson(Flattened);
            Bundle.Records.push_back(std::move(Record));
        }
        if (Failed)
            return 1;

        std::cout << "Validated " << Masters.size() << " master material(s) and " << Instances.size() << " instance(s).\n";
        if (Arguments->ValidateOnly)
            return 0;

        TiramisuMaterialShaderCompiler Compiler;
        if (!Compiler.IsAvailable())
        {
            const FMaterialShaderCompileResult Unavailable = Compiler.Compile({});
            PrintDiagnostics(Unavailable.Diagnostics, "DXC");
            return 1;
        }

        for (const FMaterialAsset& Master : Masters)
        {
            const xr_string TemplateSource = ReadText(Arguments->ShaderRoot / Master.HlslTemplate.c_str());
            const xr_string HandWrittenImplementation = Master.Implementation.Type == EMaterialImplementationType::Hlsl ?
                ReadText(Arguments->ShaderRoot / Master.Implementation.Source.c_str()) : xr_string{};
            for (const FMaterialStaticParameterSet& StaticParameters : Permutations[Master.Id.Value])
            {
                xr_string Implementation = HandWrittenImplementation;
                if (Master.Implementation.Type == EMaterialImplementationType::Graph)
                {
                    FMaterialGraphCompileOptions Options;
                    Options.Parameters = Master.Parameters;
                    Options.StaticParameters = StaticParameters;
                    const FMaterialGraphCompileResult Generated = CompileMaterialGraph(Master.Implementation.Graph, Options);
                    PrintDiagnostics(Generated.Diagnostics, Master.SourcePath);
                    if (!Generated.Succeeded()) { Failed = true; continue; }
                    Implementation = Generated.GeneratedHlsl;
                }

                xr_vector<EMaterialPass> Passes = GetRequiredMaterialPasses(Master);
                Passes.push_back(EMaterialPass::Validation);
                for (const EMaterialPass Pass : Passes)
                {
                    const FMaterialPassDefinition* PassDefinition = FindMaterialPassDefinition(Pass);
                    if (!PassDefinition) { Failed = true; continue; }
                    const FMaterialVertexFactoryDefinition* VertexFactoryDefinition =
                        FindMaterialVertexFactoryDefinition(PassDefinition->VertexFactory);
                    if (!VertexFactoryDefinition)
                    {
                        std::cerr << "error [cooker.missing_vertex_factory] " << Master.SourcePath
                                  << ": vertex factory '" << PassDefinition->VertexFactory
                                  << "' is not registered.\n";
                        Failed = true;
                        continue;
                    }
                    const xr_string PassSource = ReadText(Arguments->ShaderRoot / PassDefinition->ShaderSource);
                    const FMaterialSourceAssemblyResult PixelSource = AssembleMaterialShaderSourceForPass(
                        Master, TemplateSource, Implementation, StaticParameters, PassSource, PassDefinition->ShaderSource);
                    PrintDiagnostics(PixelSource.Diagnostics, Master.SourcePath + " [" + xr_string(PassDefinition->Name) + "]");
                    if (!PixelSource.Succeeded()) { Failed = true; continue; }

                    const xr_string VertexFactorySource =
                        ReadText(Arguments->ShaderRoot / VertexFactoryDefinition->ShaderSource);
                    const FMaterialSourceAssemblyResult VertexSource = AssembleMaterialShaderSourceForPass(
                        Master, TemplateSource, Implementation, StaticParameters, VertexFactorySource,
                        VertexFactoryDefinition->ShaderSource);
                    PrintDiagnostics(VertexSource.Diagnostics, Master.SourcePath + " [" +
                        xr_string(VertexFactoryDefinition->Name) + "/vertex]");
                    if (!VertexSource.Succeeded()) { Failed = true; continue; }

                    for (const EMaterialShaderBackend Backend :
                        {EMaterialShaderBackend::D3D12, EMaterialShaderBackend::Vulkan})
                    {
                        FResolvedMaterialInstance PipelineMaterial;
                        PipelineMaterial.MasterId = Master.Id;
                        PipelineMaterial.StaticParameters = StaticParameters;
                        const FMaterialPipelineKey Key = MakeCookedMaterialPipelineKey(
                            PipelineMaterial, *PassDefinition, BackendName(Backend));
                        const EMaterialShaderBlobFormat Format =
                            Backend == EMaterialShaderBackend::D3D12 ?
                            EMaterialShaderBlobFormat::Dxil : EMaterialShaderBlobFormat::SpirV;

                        const auto CompileStage = [&](const FMaterialSourceAssemblyResult& Source,
                            const xr_string_view EntryPoint, const xr_string_view TargetProfile,
                            const EMaterialShaderStage Stage, const xr_string_view StageName)
                        {
                            FMaterialShaderCompileRequest Request;
                            Request.Backend = Backend;
                            Request.Source = Source.Source;
                            Request.SourceName = Master.SourcePath + ":" +
                                xr_string(PassDefinition->Name) + ":" + xr_string(StageName);
                            Request.EntryPoint = EntryPoint;
                            Request.TargetProfile = TargetProfile;
                            if (Stage == EMaterialShaderStage::Vertex)
                            {
                                Request.Defines.emplace_back("MATERIAL_VERTEX_SHADER=1");
                                if (Backend == EMaterialShaderBackend::D3D12)
                                    Request.Defines.emplace_back("NRI_ENABLE_DRAW_PARAMETERS_EMULATION=1");
                            }
                            Request.IncludeDirectories = {Arguments->ShaderRoot,
                                Arguments->ShaderRoot / "common",
                                Arguments->ShaderRoot / "materials",
                                Arguments->ShaderRoot / "materials" / "passes",
                                Arguments->ShaderRoot / "materials" / "vertex"};
                            const FMaterialShaderCompileResult Compiled = Compiler.Compile(Request);
                            const xr_string DiagnosticName = Master.SourcePath + " [" +
                                xr_string(PassDefinition->Name) + "/" + xr_string(StageName) +
                                "/" + BackendName(Backend) + "]";
                            PrintDiagnostics(Compiled.Diagnostics, DiagnosticName);
                            if (!Compiled.Succeeded())
                            {
                                Failed = true;
                                return;
                            }

                            FMaterialShaderBlob Blob;
                            Blob.MaterialId = Master.Id;
                            Blob.PipelineKey = Key.StableHash();
                            Blob.Format = Format;
                            Blob.EntryPoint = Request.EntryPoint;
                            Blob.Bytecode = Compiled.Bytecode;
                            Blob.Pass = Pass;
                            Blob.Stage = Stage;
                            Blob.VertexFactory = PassDefinition->VertexFactory;
                            Blob.RenderPassSignature = PassDefinition->RenderPassSignature;
                            Bundle.ShaderBlobs.push_back(std::move(Blob));
                        };

                        CompileStage(VertexSource, VertexFactoryDefinition->EntryPoint,
                            VertexFactoryDefinition->TargetProfile, EMaterialShaderStage::Vertex, "vertex");
                        CompileStage(PixelSource, PassDefinition->EntryPoint,
                            PassDefinition->TargetProfile, EMaterialShaderStage::Pixel, "pixel");
                    }
                }
            }
        }
        if (Failed)
            return 1;

        // Both shader stages are present. Keep the cooked-runtime gate closed until the renderer
        // consumes the bundle and flattened records no longer require JSON parsing at runtime.
        Bundle.CompleteShaderSet = false;
        const FMaterialBundleWriteResult Serialized = SerializeMaterialBundle(Bundle);
        PrintDiagnostics(Serialized.Diagnostics, Arguments->Output.generic_string());
        if (!Serialized.Succeeded())
            return 1;
        std::ofstream Output(Arguments->Output, std::ios::binary | std::ios::trunc);
        Output.write(reinterpret_cast<const char*>(Serialized.Data.data()), static_cast<std::streamsize>(Serialized.Data.size()));
        if (!Output)
            throw std::runtime_error("Failed to write " + Arguments->Output.generic_string());
        std::cout << "Wrote development bundle " << Arguments->Output.generic_string() << " (" << Serialized.Data.size()
                  << " bytes) with " << Bundle.ShaderBlobs.size() << " DXIL/SPIR-V production/validation blob(s). "
                  << "Production shader set is incomplete; cooked runtime must not consume it.\n";
        return 0;
    }
    catch (const std::exception& Error)
    {
        std::cerr << "error [cooker.exception] " << Error.what() << '\n';
        return 1;
    }
}
