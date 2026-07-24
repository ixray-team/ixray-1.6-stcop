#include "TiramisuMaterialShaderCompiler.h"
#include "MaterialParameterLayout.h"

#include <algorithm>
#include <cctype>
#include <cstring>
#include <ranges>
#include <sstream>
#include <utility>

#if defined(_WIN32)
#include <Windows.h>
#include <dxcapi.h>
#include <wrl/client.h>
#endif

namespace
{
bool HasErrors(const xr_vector<FMaterialDiagnostic>& Diagnostics)
{
    return std::ranges::any_of(Diagnostics,
        [](const FMaterialDiagnostic& Diagnostic) { return Diagnostic.Severity == EMaterialDiagnosticSeverity::Error; });
}

void AddDiagnostic(xr_vector<FMaterialDiagnostic>& Diagnostics, const EMaterialDiagnosticSeverity Severity,
    xr_string Code, xr_string Message)
{
    Diagnostics.push_back({Severity, std::move(Code), std::move(Message), {}, {}});
}

xr_string StaticLiteral(const FMaterialValue& Value)
{
    if (const bool* Boolean = std::get_if<bool>(&Value))
        return *Boolean ? "1" : "0";
    if (const s32* Integer = std::get_if<s32>(&Value))
        return std::to_string(*Integer);
    return {};
}

size_t CountOccurrences(const xr_string_view Text, const xr_string_view Token)
{
    size_t Count = 0;
    size_t Position = 0;
    while ((Position = Text.find(Token, Position)) != xr_string_view::npos)
    {
        ++Count;
        Position += Token.size();
    }
    return Count;
}

void ReplaceOnce(xr_string& Text, const xr_string_view Token, const xr_string_view Replacement)
{
    const size_t Position = Text.find(Token);
    if (Position != xr_string::npos)
        Text.replace(Position, Token.size(), Replacement);
}

xr_string EscapeLineFile(xr_string Value)
{
    std::ranges::replace(Value, '\\', '/');
    xr_string Result;
    Result.reserve(Value.size());
    for (const char Character : Value)
    {
        if (Character == '"') Result += '\'';
        else Result += Character;
    }
    return Result;
}

xr_string SanitizeDxcSourceName(xr_string Value)
{
    if (Value.empty())
        return "material-generated.hlsl";
    std::ranges::replace(Value, '\\', '/');
    // IDxcCompiler3 interprets its final positional argument as the in-memory
    // source name. Colons outside a Windows drive prefix are parsed as an
    // invalid file argument by some DXC builds, so cooker diagnostic labels
    // such as "asset:pass:stage" must not be forwarded verbatim.
    for (size_t Index = 0; Index < Value.size(); ++Index)
    {
        if (Value[Index] == ':' && !(Index == 1 &&
            std::isalpha(static_cast<unsigned char>(Value[0]))))
        {
            Value[Index] = '_';
        }
    }
    const size_t LastSlash = Value.find_last_of('/');
    const xr_string_view FileName = LastSlash == xr_string::npos
        ? xr_string_view(Value) : xr_string_view(Value).substr(LastSlash + 1);
    if (!FileName.ends_with(".hlsl"))
        Value += ".hlsl";
    return Value;
}

constexpr xr_string_view ValidationEntryPoint = R"(

// Engine-owned validation pass. Production passes provide their own entry points
// while consuming the exact same EvaluateMaterial contract.
struct MaterialValidationPixelInput
{
    float4 Position : SV_Position;
    float2 TexCoord0 : TEXCOORD0;
    float4 VertexColor : COLOR0;
    float3 WorldNormal : NORMAL0;
    float3 WorldPosition : TEXCOORD1;
    nointerpolation uint MaterialInstanceIndex : TEXCOORD4;
    float2 TexCoord1 : TEXCOORD5;
};

float4 Main(MaterialValidationPixelInput Input) : SV_Target0
{
    MaterialContext Context;
    Context.TexCoord0 = Input.TexCoord0;
    Context.TexCoord1 = Input.TexCoord1;
    Context.VertexColor = Input.VertexColor;
    Context.WorldNormal = normalize(Input.WorldNormal);
    Context.WorldPosition = Input.WorldPosition;
    Context.CameraPosition = float3(0.0f, 0.0f, 0.0f);
    Context.CameraVector = normalize(Context.CameraPosition - Context.WorldPosition);
    Context.Time = 0.0f;

    MaterialInputs Result;
    const MaterialParameters Parameters = LoadMaterialParametersForInstance(Input.MaterialInstanceIndex);
    EvaluateMaterial(Context, Parameters, Result);
    return float4(Result.BaseColor + Result.Emissive, Result.Opacity);
}
)";

#if defined(_WIN32)
std::wstring Utf8ToWide(const xr_string_view Text)
{
    if (Text.empty()) return {};
    const int Length = MultiByteToWideChar(CP_UTF8, 0, Text.data(), static_cast<int>(Text.size()), nullptr, 0);
    if (Length <= 0) return {};
    std::wstring Result(static_cast<size_t>(Length), L'\0');
    MultiByteToWideChar(CP_UTF8, 0, Text.data(), static_cast<int>(Text.size()), Result.data(), Length);
    return Result;
}

xr_string DxcMessage(IDxcResult& Result)
{
    Microsoft::WRL::ComPtr<IDxcBlobUtf8> Messages;
    if (FAILED(Result.GetOutput(DXC_OUT_ERRORS, IID_PPV_ARGS(Messages.ReleaseAndGetAddressOf()), nullptr)) || !Messages)
        return {};
    return xr_string(Messages->GetStringPointer(), Messages->GetStringLength());
}
#endif
} // namespace

bool FMaterialSourceAssemblyResult::Succeeded() const noexcept
{
    return !HasErrors(Diagnostics);
}

FMaterialSourceAssemblyResult AssembleMaterialShaderSource(const FMaterialAsset& Asset,
    const xr_string_view TemplateSource, const xr_string_view ImplementationSource,
    const FMaterialStaticParameterSet& StaticParameters, const bool AppendValidationEntryPoint)
{
    FMaterialSourceAssemblyResult Result;
    constexpr xr_string_view ParametersInclude = "#include \"MaterialParameters.generated.hlsl\"";
    constexpr xr_string_view ImplementationInclude = "#include \"MaterialImplementation.generated.hlsl\"";

    if (CountOccurrences(TemplateSource, ParametersInclude) != 1)
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.template_parameters_include",
            "Material template must contain exactly one MaterialParameters.generated.hlsl include.");
    if (CountOccurrences(TemplateSource, ImplementationInclude) != 1)
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.template_implementation_include",
            "Material template must contain exactly one MaterialImplementation.generated.hlsl include.");
    if (ImplementationSource.empty())
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.empty_implementation",
            "Material implementation source is empty.");

    xr_vector<const FMaterialParameterDefinition*> SortedParameters;
    SortedParameters.reserve(Asset.Parameters.size());
    for (const FMaterialParameterDefinition& Parameter : Asset.Parameters)
        SortedParameters.push_back(&Parameter);
    std::ranges::sort(SortedParameters, {}, [](const FMaterialParameterDefinition* Parameter) { return Parameter->Id.Value; });

    std::ostringstream Declarations;
    Declarations << "// Generated by xrTiramisuMaterialCore. Do not edit.\n";
    Declarations << "#define MATERIAL_DOMAIN_SURFACE " << (Asset.Domain == EMaterialDomain::Surface ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_DOMAIN_DECAL " << (Asset.Domain == EMaterialDomain::Decal ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_DOMAIN_UI " << (Asset.Domain == EMaterialDomain::UI ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_DOMAIN_POST_PROCESS " << (Asset.Domain == EMaterialDomain::PostProcess ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_BLEND_MASKED " << (Asset.BlendMode == EMaterialBlendMode::Masked ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_TWO_SIDED " << (Asset.TwoSided ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_SHADING_DEFAULT_LIT " << (Asset.ShadingModel == EMaterialShadingModel::DefaultLit ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_SHADING_UNLIT " << (Asset.ShadingModel == EMaterialShadingModel::Unlit ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_SHADING_FOLIAGE " << (Asset.ShadingModel == EMaterialShadingModel::Foliage ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_SHADING_HAIR " << (Asset.ShadingModel == EMaterialShadingModel::Hair ? 1 : 0) << "\n";
    Declarations << "#define MATERIAL_OPACITY_MASK_CLIP_VALUE 0.5f\n";
    for (const FMaterialParameterDefinition* Parameter : SortedParameters)
    {
        if (!Parameter->IsStatic()) continue;
        const auto Override = StaticParameters.find(Parameter->Id);
        const FMaterialValue& Value = Override == StaticParameters.end() ? Parameter->DefaultValue : Override->second;
        if (!ValueMatchesParameterType(Value, Parameter->Type))
        {
            AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.static_parameter_type",
                "Static parameter '" + Parameter->Id.Value + "' has an invalid permutation value.");
            continue;
        }
        Declarations << "#define MATERIAL_STATIC_" << MaterialParameterHlslFieldName(Parameter->Id) << ' ' << StaticLiteral(Value) << "\n";
    }
    for (const auto& [Id, Value] : StaticParameters)
    {
        const auto Existing = std::ranges::find_if(Asset.Parameters,
            [&Id](const FMaterialParameterDefinition& Definition) { return Definition.Id == Id && Definition.IsStatic(); });
        if (Existing == Asset.Parameters.end())
            AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.unknown_static_parameter",
                "Permutation contains unknown static parameter '" + Id.Value + "'.");
    }

    const FMaterialParameterLayoutResult ParameterLayout = BuildMaterialParameterLayout(Asset.Parameters);
    Result.Diagnostics.insert(Result.Diagnostics.end(), ParameterLayout.Diagnostics.begin(), ParameterLayout.Diagnostics.end());
    if (ParameterLayout.Succeeded())
        Declarations << GenerateMaterialParameterHlsl(ParameterLayout.Value);
    Result.ParameterDeclarations = Declarations.str();

    if (HasErrors(Result.Diagnostics))
        return Result;

    Result.Source.assign(TemplateSource);
    ReplaceOnce(Result.Source, ParametersInclude,
        "#line 1 \"material-parameters/generated\"\n" + Result.ParameterDeclarations);
    const xr_string SourceName = Asset.SourcePath.empty() ? "material-implementation" : EscapeLineFile(Asset.SourcePath);
    ReplaceOnce(Result.Source, ImplementationInclude,
        "#line 1 \"" + SourceName + "\"\n" + xr_string(ImplementationSource));
    if (AppendValidationEntryPoint)
        Result.Source += xr_string(ValidationEntryPoint);
    return Result;
}

FMaterialSourceAssemblyResult AssembleMaterialShaderSourceForPass(const FMaterialAsset& Asset,
    const xr_string_view TemplateSource, const xr_string_view ImplementationSource,
    const FMaterialStaticParameterSet& StaticParameters, const xr_string_view PassSource,
    const xr_string_view PassSourceName)
{
    FMaterialSourceAssemblyResult Result = AssembleMaterialShaderSource(
        Asset, TemplateSource, ImplementationSource, StaticParameters, false);
    if (PassSource.empty())
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.empty_pass",
            "Material pass source is empty.");
    if (HasErrors(Result.Diagnostics))
        return Result;

    const xr_string Name = PassSourceName.empty() ? "material-pass" : EscapeLineFile(xr_string(PassSourceName));
    Result.Source += "\n#line 1 \"" + Name + "\"\n" + xr_string(PassSource);
    return Result;
}

bool FMaterialShaderCompileResult::Succeeded() const noexcept
{
    return !Bytecode.empty() && !HasErrors(Diagnostics);
}

struct TiramisuMaterialShaderCompiler::FImpl
{
#if defined(_WIN32)
    Microsoft::WRL::ComPtr<IDxcUtils> Utils;
    Microsoft::WRL::ComPtr<IDxcCompiler3> Compiler;
    Microsoft::WRL::ComPtr<IDxcIncludeHandler> IncludeHandler;
#endif
    xr_string InitializationError;
};

TiramisuMaterialShaderCompiler::TiramisuMaterialShaderCompiler() : Impl(std::make_unique<FImpl>())
{
#if defined(_WIN32)
    HRESULT Status = DxcCreateInstance(CLSID_DxcUtils, IID_PPV_ARGS(Impl->Utils.ReleaseAndGetAddressOf()));
    if (SUCCEEDED(Status))
        Status = DxcCreateInstance(CLSID_DxcCompiler, IID_PPV_ARGS(Impl->Compiler.ReleaseAndGetAddressOf()));
    if (SUCCEEDED(Status))
        Status = Impl->Utils->CreateDefaultIncludeHandler(Impl->IncludeHandler.ReleaseAndGetAddressOf());
    if (FAILED(Status))
        Impl->InitializationError = "DXC initialization failed with HRESULT " + std::to_string(static_cast<unsigned long>(Status)) + ".";
#else
    Impl->InitializationError = "DXC material compilation is currently supported only on Windows.";
#endif
}

TiramisuMaterialShaderCompiler::~TiramisuMaterialShaderCompiler() = default;
TiramisuMaterialShaderCompiler::TiramisuMaterialShaderCompiler(TiramisuMaterialShaderCompiler&&) noexcept = default;
TiramisuMaterialShaderCompiler& TiramisuMaterialShaderCompiler::operator=(TiramisuMaterialShaderCompiler&&) noexcept = default;

bool TiramisuMaterialShaderCompiler::IsAvailable() const noexcept
{
    return Impl && Impl->InitializationError.empty();
}

FMaterialShaderCompileResult TiramisuMaterialShaderCompiler::Compile(const FMaterialShaderCompileRequest& Request) const
{
    FMaterialShaderCompileResult Result;
    if (!IsAvailable())
    {
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.dxc_unavailable",
            Impl ? Impl->InitializationError : "DXC compiler instance is not initialized.");
        return Result;
    }
    if (Request.Source.empty() || Request.EntryPoint.empty() || Request.TargetProfile.empty())
    {
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.invalid_request",
            "Shader source, entry point and target profile must be provided.");
        return Result;
    }

#if defined(_WIN32)
    xr_vector<std::wstring> Arguments;
    Arguments.reserve(32 + (Request.IncludeDirectories.size() + Request.Defines.size()) * 2);
    if (Request.Backend == EMaterialShaderBackend::Vulkan)
    {
        Arguments.emplace_back(L"-spirv");
        Arguments.emplace_back(L"-fspv-target-env=vulkan1.3");
        Arguments.emplace_back(L"-fvk-s-shift"); Arguments.emplace_back(L"0"); Arguments.emplace_back(L"0");
        Arguments.emplace_back(L"-fvk-t-shift"); Arguments.emplace_back(L"128"); Arguments.emplace_back(L"0");
        Arguments.emplace_back(L"-fvk-b-shift"); Arguments.emplace_back(L"32"); Arguments.emplace_back(L"2");
        Arguments.emplace_back(L"-fvk-u-shift"); Arguments.emplace_back(L"64"); Arguments.emplace_back(L"0");
        Arguments.emplace_back(L"-fvk-bind-resource-heap"); Arguments.emplace_back(L"0"); Arguments.emplace_back(L"0");
        Arguments.emplace_back(L"-fvk-bind-sampler-heap"); Arguments.emplace_back(L"1"); Arguments.emplace_back(L"1");
    }
    Arguments.emplace_back(L"-T"); Arguments.push_back(Utf8ToWide(Request.TargetProfile));
    Arguments.emplace_back(L"-E"); Arguments.push_back(Utf8ToWide(Request.EntryPoint));
    Arguments.emplace_back(L"-HV"); Arguments.emplace_back(L"2021");
    Arguments.emplace_back(L"-Ges");
    Arguments.emplace_back(Request.Debug ? L"-Od" : L"-O3");
    if (Request.Debug)
    {
        Arguments.emplace_back(L"-Zi");
        Arguments.emplace_back(L"-Qembed_debug");
    }
    if (Request.WarningsAsErrors)
        Arguments.emplace_back(L"-WX");
    for (const xr_string& Define : Request.Defines)
    {
        Arguments.emplace_back(L"-D");
        Arguments.push_back(Utf8ToWide(Define));
    }
    for (const std::filesystem::path& Directory : Request.IncludeDirectories)
    {
        Arguments.emplace_back(L"-I");
        Arguments.push_back(Directory.wstring());
    }
    Arguments.push_back(Utf8ToWide(SanitizeDxcSourceName(Request.SourceName)));

    xr_vector<LPCWSTR> ArgumentPointers;
    ArgumentPointers.reserve(Arguments.size());
    for (const std::wstring& Argument : Arguments)
        ArgumentPointers.push_back(Argument.c_str());

    DxcBuffer Buffer{};
    Buffer.Ptr = Request.Source.data();
    Buffer.Size = Request.Source.size();
    Buffer.Encoding = DXC_CP_UTF8;
    Microsoft::WRL::ComPtr<IDxcResult> Compilation;
    const HRESULT CompileStatus = Impl->Compiler->Compile(&Buffer, ArgumentPointers.data(),
        static_cast<u32>(ArgumentPointers.size()), Impl->IncludeHandler.Get(),
        IID_PPV_ARGS(Compilation.ReleaseAndGetAddressOf()));
    if (FAILED(CompileStatus) || !Compilation)
    {
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.dxc_call_failed",
            "DXC invocation failed with HRESULT " + std::to_string(static_cast<unsigned long>(CompileStatus)) + ".");
        return Result;
    }

    HRESULT ShaderStatus = E_FAIL;
    Compilation->GetStatus(&ShaderStatus);
    const xr_string Messages = DxcMessage(*Compilation.Get());
    if (FAILED(ShaderStatus))
    {
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.dxc_compile_failed",
            Messages.empty() ? "DXC rejected the material shader." : Messages);
        return Result;
    }
    if (!Messages.empty())
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Warning, "shader.dxc_message", Messages);

    Microsoft::WRL::ComPtr<IDxcBlob> Object;
    if (FAILED(Compilation->GetOutput(DXC_OUT_OBJECT, IID_PPV_ARGS(Object.ReleaseAndGetAddressOf()), nullptr)) || !Object)
    {
        AddDiagnostic(Result.Diagnostics, EMaterialDiagnosticSeverity::Error, "shader.dxc_missing_object",
            "DXC succeeded but returned no shader object.");
        return Result;
    }
    const auto* Bytes = static_cast<const u8*>(Object->GetBufferPointer());
    Result.Bytecode.assign(Bytes, Bytes + Object->GetBufferSize());
#endif
    return Result;
}
