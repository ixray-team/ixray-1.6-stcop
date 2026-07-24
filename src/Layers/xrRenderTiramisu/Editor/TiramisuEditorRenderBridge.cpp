#include "stdafx.h"
#include "TiramisuEditorRenderBridge.h"
#include "../Core/TiramisuRenderDevice.h"
#include "../../../xrCore/RenderDebugPolicy.h"
#include "../../../xrCore/RenderDocIntegration.h"
#include "TEditorBoundedAsyncQueue.h"
#include "TiramisuEditorNriFrameScheduler.h"
#include "TiramisuEditorNriTextureRegistry.h"
#include "TiramisuEditorTextureMailbox.h"
#include "TiramisuEditorViewportSceneMailbox.h"
#include "TiramisuEditorViewportScenePicker.h"
#include "TiramisuEditorViewportSceneShader.h"

#include <MaterialPreviewCompiler.h>
#include <MaterialPreviewAssets.h>
#include <EditorViewportMaterialResolver.h>
#include <MaterialDependencyWatcher.h>
#include "TiramisuMaterialShaderCompiler.h"
#include "MaterialParameterLayout.h"
#include "MaterialRuntime.h"

#include <RedImage/RedImage.hpp>

// xrEngine exposes its legacy global render device through a Device macro.
// NRI has a type with the same name, and this translation unit never needs
// the legacy shorthand.
#undef Device

#include <NRI.h>
#include <Extensions/NRIDeviceCreation.h>
#include <Extensions/NRIHelper.h>
#include <Extensions/NRIImgui.h>
#include <Extensions/NRIStreamer.h>
#include <Extensions/NRISwapChain.h>

#include <SDL3/SDL.h>
#include <imgui.h>

#include <array>
#include <bit>
#include <cctype>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <future>
#include <limits>
#include <mutex>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace
{
constexpr u32 QueuedFrameCount = 3;
constexpr nri::VKBindingOffsets VkBindingOffsets = {0, 128, 32, 64};

struct FEditorSceneDrawConstants
{
	xr_array<float, 16> LocalToWorld;
	xr_array<float, 16> ViewProjection;
	u32 MaterialSlotLow = 0;
	u32 MaterialSlotHigh = 0;
	u32 InstanceFlags = 0;
	u32 Padding = 0;
};

static_assert(sizeof(FEditorSceneDrawConstants) == 144);
static_assert(sizeof(FEditorStaticMeshVertex) == 60);
static_assert(sizeof(FEditorDebugVertex) == 28);
static_assert(sizeof(FEditorOverlayVertex) == sizeof(FEditorDebugVertex));

[[nodiscard]] xr_array<float, 16> MakeConstantBufferMatrix(
	const Fmatrix& XRayMatrix)
{
	xr_array<float, 16> Result = {};
	std::copy_n(XRayMatrix.mm, Result.size(), Result.begin());
	return Result;
}

struct alignas(16) FEditorMaterialGlobalConstants
{
	xr_array<float, 4> SceneView = {};
	xr_array<float, 16> ViewProjectionWorldMatrix = {};
	xr_array<float, 4> CameraPositionAndTime = {};
	u32 DrawDataBufferIndex = 0;
	u32 MaterialInstanceBufferIndex = 0;
	u32 MaterialParameterBufferIndex = 0;
	u32 DefaultMaterialSamplerIndex = 0;
	u32 LightDataBufferIndex = 0;
	u32 LightDataOffset = 0;
	u32 LightCount = 0;
	u32 LightingFlags = 0;
};

static_assert(sizeof(FEditorMaterialGlobalConstants) == 128);

constexpr u32 MaxMaterialPreviews = 64;
constexpr u32 PreviewParameterStride = 4096;
constexpr u32 MaxEditorMaterialInstances = 2048;
constexpr u32 MaxSceneViewports = 8;
constexpr u32 MaxSceneDrawsPerViewport = 8192;
constexpr u32 MaxSceneLightsPerViewport =
	static_cast<u32>(EditorViewportMaxLightCount);
constexpr u32 MaxEditorDrawRecords = MaxMaterialPreviews +
	MaxSceneViewports * MaxSceneDrawsPerViewport;
constexpr u32 PreviewDrawDataDescriptorIndex = 0;
constexpr u32 PreviewMaterialInstanceDescriptorIndex = 1;
constexpr u32 PreviewMaterialParameterDescriptorIndex = 2;
constexpr u32 PreviewWhiteTextureDescriptorIndex = 3;
constexpr u32 PreviewWhiteCubeDescriptorIndex = 4;
constexpr u32 PreviewLightDataDescriptorIndex = 5;
constexpr u32 PreviewFirstAssetTextureDescriptorIndex = 6;
constexpr u32 PreviewResourceDescriptorCount = 512;
constexpr u32 PreviewDefaultSamplerIndex = 0;
constexpr size_t MaxConcurrentSceneMaterialCompiles = 2;

nri::Format MaterialPreviewTextureFormat(
	const RedImageTool::RedTexturePixelFormat Format, const bool Srgb) noexcept
{
	using RedImageTool::RedTexturePixelFormat;
	switch (Format)
	{
	case RedTexturePixelFormat::R8:
		return nri::Format::R8_UNORM;
	case RedTexturePixelFormat::R8G8:
		return nri::Format::RG8_UNORM;
	case RedTexturePixelFormat::R8G8B8A8:
		return Srgb ? nri::Format::RGBA8_SRGB : nri::Format::RGBA8_UNORM;
	case RedTexturePixelFormat::R32F:
		return nri::Format::R32_SFLOAT;
	case RedTexturePixelFormat::R32G32F:
		return nri::Format::RG32_SFLOAT;
	case RedTexturePixelFormat::R32G32B32F:
		return nri::Format::RGB32_SFLOAT;
	case RedTexturePixelFormat::R32G32B32A32F:
		return nri::Format::RGBA32_SFLOAT;
	case RedTexturePixelFormat::BC1:
		return Srgb ? nri::Format::BC1_RGBA_SRGB : nri::Format::BC1_RGBA_UNORM;
	case RedTexturePixelFormat::BC2:
		return Srgb ? nri::Format::BC2_RGBA_SRGB : nri::Format::BC2_RGBA_UNORM;
	case RedTexturePixelFormat::BC3:
		return Srgb ? nri::Format::BC3_RGBA_SRGB : nri::Format::BC3_RGBA_UNORM;
	case RedTexturePixelFormat::BC4:
		return nri::Format::BC4_R_UNORM;
	case RedTexturePixelFormat::BC5:
		return nri::Format::BC5_RG_UNORM;
	case RedTexturePixelFormat::BC6:
		return nri::Format::BC6H_RGB_UFLOAT;
	case RedTexturePixelFormat::BC7:
		return Srgb ? nri::Format::BC7_RGBA_SRGB : nri::Format::BC7_RGBA_UNORM;
	default:
		return nri::Format::UNKNOWN;
	}
}

void NRI_CALL EditorNriMessageCallback(const nri::Message MessageType,
	const char* File, const u32 Line, const char* Message, void*)
{
	const char* Severity = MessageType == nri::Message::ERROR ? "error" :
		MessageType == nri::Message::WARNING ? "warning" : "info";
	Msg("%s NRI[%s] %s:%u: %s",
		MessageType == nri::Message::ERROR ? "!" : "*", Severity,
		File ? File : "<unknown>", Line, Message ? Message : "<no message>");
}

bool IsSrgb(const nri::Format Format) noexcept
{
	return Format == nri::Format::RGBA8_SRGB ||
		Format == nri::Format::BGRA8_SRGB;
}

u64 TextureHandleKey(const FEditorTextureHandle Handle) noexcept
{
	return (static_cast<u64>(Handle.Generation) << 32) | Handle.Index;
}
} // namespace

struct TiramisuEditorRenderBridge::FImpl
{
	struct FFrame
	{
		nri::CommandAllocator* Allocator = nullptr;
		nri::CommandBuffer* CommandBuffer = nullptr;
	};

	struct FSwapTexture
	{
		nri::Texture* Texture = nullptr;
		nri::Descriptor* ColorAttachment = nullptr;
		nri::Fence* AcquireSemaphore = nullptr;
		nri::Fence* ReleaseSemaphore = nullptr;
		bool HasPresentState = false;
	};

	struct FViewport
	{
		u32 DesiredWidth = 0;
		u32 DesiredHeight = 0;
		u32 Width = 0;
		u32 Height = 0;
		bool CaptureRequested = false;
		bool HasShaderResourceState = false;
		nri::Texture* Texture = nullptr;
		xr_vector<nri::Memory*> Memory;
		nri::Descriptor* ShaderResource = nullptr;
		nri::Descriptor* ColorAttachment = nullptr;
		nri::Texture* DepthTexture = nullptr;
		nri::Descriptor* DepthAttachment = nullptr;
		bool HasDepthAttachmentState = false;
		std::unique_ptr<TiramisuEditorViewportSceneMailbox> SceneMailbox =
			std::make_unique<TiramisuEditorViewportSceneMailbox>();
		std::unique_ptr<TiramisuEditorViewportScenePicker> ScenePicker =
			std::make_unique<TiramisuEditorViewportScenePicker>();
		FEditorOwnedViewportScenePacket ScenePacket;
		u32 SceneDrawBase = UINT32_MAX;
		u32 SceneLightBase = UINT32_MAX;
		u32 SceneDrawCount = 0;
		u32 SceneSelectionDrawCount = 0;
		u64 DebugDrawRevision = 0;
		nri::Buffer* DebugDrawBuffer = nullptr;
		u64 DebugDrawBufferBytes = 0;
		u64 DebugTriangleOffset = 0;
		u64 OverlayLineOffset = 0;
		u64 OverlayTriangleOffset = 0;
		u32 DebugLineVertexCount = 0;
		u32 DebugTriangleVertexCount = 0;
		u32 OverlayLineVertexCount = 0;
		u32 OverlayTriangleVertexCount = 0;
		bool DebugDrawNeedsBarrier = false;
		nri::Buffer* SceneConstantsBuffer = nullptr;
		xr_array<nri::Descriptor*, QueuedFrameCount> SceneConstantsDescriptors = {};
		xr_array<nri::DescriptorSet*, QueuedFrameCount> SceneConstantsSets = {};
	};

	struct FGpuMesh
	{
		nri::Buffer* Buffer = nullptr;
		u64 VertexOffset = 0;
		u32 IndexCount = 0;
		u64 ByteSize = 0;
		xr_vector<FEditorStaticMeshSection> Sections;
		bool NeedsBarrier = true;
	};

	struct FDeferredBuffer
	{
		nri::Buffer* Buffer = nullptr;
		u64 RetireFence = 0;
	};

	struct FUiTexture
	{
		nri::Texture* Texture = nullptr;
		nri::Descriptor* ShaderResource = nullptr;
		u32 Width = 0;
		u32 Height = 0;
		u64 Revision = 0;
	};

	struct FDeferredUiTexture
	{
		nri::Texture* Texture = nullptr;
		nri::Descriptor* ShaderResource = nullptr;
		u64 RetireFence = 0;
	};

	struct FDeferredPipeline
	{
		nri::Pipeline* Pipeline = nullptr;
		u64 RetireFence = 0;
	};

	struct FMaterialPreviewTexture
	{
		nri::Texture* Texture = nullptr;
		nri::Descriptor* Descriptor = nullptr;
		u32 DescriptorIndex = PreviewWhiteTextureDescriptorIndex;
		bool Cube = false;
		u64 ByteSize = 0;
	};

	struct FMaterialPreview
	{
		u32 Generation = 1;
		bool Alive = false;
		u32 ViewportId = 0;
		FEditorStaticMeshId MeshId;
		EMaterialPreviewPrimitive Primitive = EMaterialPreviewPrimitive::Sphere;
		xr_string Environment = "Studio";
		EMaterialPreviewState State = EMaterialPreviewState::Unavailable;
		u64 RequestedRevision = 0;
		u64 AcceptedRevision = 0;
		xr_string Diagnostic;
		nri::Pipeline* Pipeline = nullptr;
		u64 PipelineKey = 0;
		xr_vector<u8> ParameterData;
		u64 ParameterLayoutHash = 0;
		u32 DrawFlags = PreviewWhiteCubeDescriptorIndex;
	};

	struct FMaterialPreviewCompileJob
	{
		FMaterialPreviewHandle Handle;
		u64 Revision = 0;
		std::future<Tiramisu::Editor::FMaterialPreviewCompileResult> Future;
	};

	struct FSceneMaterial
	{
		u32 InstanceIndex = UINT32_MAX;
		u64 RequestedRevision = 0;
		u64 AcceptedRevision = 0;
		xr_string SourceKey;
		xr_string Diagnostic;
		bool TwoSided = false;
		EMaterialBlendMode BlendMode =
			EMaterialBlendMode::Opaque;
		nri::Pipeline* Pipeline = nullptr;
		u64 PipelineKey = 0;
		xr_vector<u8> ParameterData;
		u64 ParameterLayoutHash = 0;
		FEditorOwnedMaterialSlotSource Source;
		u32 ReloadCount = 0;
		bool PipelineTwoSided = false;
	};

	struct FSceneMaterialCompileRequest
	{
		u64 MaterialSlot = 0;
		u64 Revision = 0;
		bool TwoSided = false;
		bool Reload = false;
		Tiramisu::Editor::FMaterialPreviewCompileRequest Request;
	};

	struct FSceneMaterialCompileResult
	{
		u64 MaterialSlot = 0;
		u64 Revision = 0;
		bool TwoSided = false;
		bool Reload = false;
		Tiramisu::Editor::FMaterialPreviewCompileResult Compiled;
	};

	struct FScenePipelineCacheKey
	{
		u64 PipelineKey = 0;
		bool TwoSided = false;

		bool operator==(const FScenePipelineCacheKey&) const = default;
	};

	struct FScenePipelineCacheKeyHash
	{
		size_t operator()(
			const FScenePipelineCacheKey& Key) const noexcept
		{
			const u64 Sidedness =
				Key.TwoSided ? 0x9e3779b97f4a7c15ull : 0;
			return static_cast<size_t>(
				Key.PipelineKey ^ (Sidedness + (Key.PipelineKey << 6u) +
					(Key.PipelineKey >> 2u)));
		}
	};

	struct FScenePipelineCacheEntry
	{
		nri::Pipeline* Pipeline = nullptr;
		u32 ReferenceCount = 0;
	};

	struct FSceneMaterialResolverReloadResult
	{
		std::unique_ptr<Tiramisu::Editor::TiramisuEditorViewportMaterialResolver> Resolver;
		xr_vector<FMaterialDiagnostic> Diagnostics;
		bool Loaded = false;
	};

	explicit FImpl(SDL_Window* InWindow, const ETiramisuEditorGraphicsApi InApi,
		const FRenderDeterministicTestPolicy& InDeterministicTest)
		: Window(InWindow), Api(InApi),
		  DeterministicTest(InDeterministicTest),
		  SceneMaterialCompileQueue(MaxConcurrentSceneMaterialCompiles,
			  [](FSceneMaterialCompileRequest Request)
			  {
				  FSceneMaterialCompileResult Result;
				  Result.MaterialSlot = Request.MaterialSlot;
				  Result.Revision = Request.Revision;
				  Result.TwoSided = Request.TwoSided;
				  Result.Reload = Request.Reload;
				  Result.Compiled =
					  Tiramisu::Editor::CompileMaterialPreview(Request.Request);
				  return Result;
			  })
	{}

	bool Check(const nri::Result Result, const char* Message)
	{
		if (Result == nri::Result::SUCCESS)
			return true;
		Diagnostic = Message;
		return false;
	}

	[[nodiscard]] FMaterialPreview* FindPreview(
		const FMaterialPreviewHandle Handle) noexcept
	{
		if (Handle.Index >= MaterialPreviews.size())
			return nullptr;
		FMaterialPreview& Preview = MaterialPreviews[Handle.Index];
		return Preview.Alive && Preview.Generation == Handle.Generation
			? &Preview : nullptr;
	}

	[[nodiscard]] const FMaterialPreview* FindPreview(
		const FMaterialPreviewHandle Handle) const noexcept
	{
		if (Handle.Index >= MaterialPreviews.size())
			return nullptr;
		const FMaterialPreview& Preview = MaterialPreviews[Handle.Index];
		return Preview.Alive && Preview.Generation == Handle.Generation
			? &Preview : nullptr;
	}

	[[nodiscard]] xr_string ReadGameShader(const char* RelativePath)
	{
		IReader* Reader = FS.r_open("$game_shaders$", RelativePath);
		if (!Reader)
			return {};
		xr_string Source(static_cast<const char*>(Reader->pointer()),
			static_cast<size_t>(Reader->length()));
		FS.r_close(Reader);
		return Source;
	}

	[[nodiscard]] xr_string ReadR5Shader(const xr_string_view RelativePath)
	{
		xr_string Path(RelativePath);
		std::ranges::replace(Path, '/', '\\');
		if (!Path.starts_with("r5\\"))
			Path.insert(0, "r5\\");
		return ReadGameShader(Path.c_str());
	}

	[[nodiscard]] std::filesystem::path ResolveR5ShaderPath(
		const xr_string_view RelativePath) const
	{
		string_path ShaderRoot = {};
		FS.update_path(ShaderRoot, "$game_shaders$", "r5\\");
		xr_string Path(RelativePath);
		std::ranges::replace(Path, '/', '\\');
		if (Path.starts_with("r5\\"))
			Path.erase(0, 3);
		return (std::filesystem::path(ShaderRoot) / Path.c_str()).lexically_normal();
	}

	[[nodiscard]] Tiramisu::Editor::FMaterialPreviewCompileRequest
		MakePreviewCompileRequest(const FMaterialPreviewSource& Source)
	{
		Tiramisu::Editor::FMaterialPreviewCompileRequest Request;
		Request.Backend = Api == ETiramisuEditorGraphicsApi::D3D12
			? EMaterialShaderBackend::D3D12
			: EMaterialShaderBackend::Vulkan;
		Request.MaterialJson.assign(Source.MaterialJson);
		Request.MaterialInstanceJson.assign(Source.MaterialInstanceJson);
		Request.GeneratedHlsl.assign(Source.GeneratedHlsl);
		Request.TemplateSource = ReadGameShader(
			"r5\\materials\\MaterialTemplate.hlsl");
		Request.VertexFactorySource = ReadGameShader(
			"r5\\materials\\vertex\\MaterialLevelStaticVertexFactory.hlsl");
		Request.Pass = EMaterialPass::Validation;
		Request.PassSource = ReadGameShader(
			"r5\\materials\\passes\\MaterialPreviewPass.hlsl");
		Request.DependencySources.push_back(ReadGameShader(
			"r5\\materials\\passes\\MaterialLightingCommon.hlsl"));

		string_path ShaderRoot = {};
		FS.update_path(ShaderRoot, "$game_shaders$", "r5\\");
		const std::filesystem::path Root = ShaderRoot;
		Request.IncludeDirectories = {Root, Root / "common", Root / "materials",
			Root / "materials/passes", Root / "materials/vertex"};
		Request.Debug = strstr(Core.Params, "-rdebug") != nullptr ||
			strstr(Core.Params, "-rdbg") != nullptr;
		return Request;
	}

	[[nodiscard]] Tiramisu::Editor::FMaterialPreviewCompileRequest
		MakeSceneMaterialCompileRequest(
			const Tiramisu::Editor::FEditorViewportMaterialResolution& Resolution)
	{
		Tiramisu::Editor::FMaterialPreviewCompileRequest Request;
		Request.Backend = Api == ETiramisuEditorGraphicsApi::D3D12
			? EMaterialShaderBackend::D3D12
			: EMaterialShaderBackend::Vulkan;
		Request.MaterialJson = SerializeMaterialAssetJson(Resolution.Master);
		Request.MaterialInstanceJson = SerializeMaterialInstanceJson(
			Resolution.FlattenedInstance);
		if (Resolution.Master.Implementation.Type ==
			EMaterialImplementationType::Hlsl)
		{
			Request.GeneratedHlsl = ReadR5Shader(
				Resolution.Master.Implementation.Source);
		}
		Request.TemplateSource = ReadR5Shader(
			Resolution.Master.HlslTemplate);
		const FMaterialVertexFactoryDefinition* VertexFactory =
			FindMaterialVertexFactoryDefinition("level_static");
		const FMaterialPassDefinition* Forward =
			FindMaterialPassDefinition(EMaterialPass::Forward);
		if (VertexFactory)
			Request.VertexFactorySource = ReadR5Shader(
				VertexFactory->ShaderSource);
		if (Forward)
			Request.PassSource = ReadR5Shader(Forward->ShaderSource);
		Request.Pass = EMaterialPass::Forward;
		Request.RenderPassSignature = "editor_forward:rgba8:d32";
		Request.CompilerOptions = "editor_viewport_scene_v1";
		for (const xr_string& Dependency : Resolution.Master.Dependencies)
			Request.DependencySources.push_back(ReadR5Shader(Dependency));
		Request.DependencySources.push_back(ReadR5Shader(
			"materials/passes/MaterialLightingCommon.hlsl"));

		string_path ShaderRoot = {};
		FS.update_path(ShaderRoot, "$game_shaders$", "r5\\");
		const std::filesystem::path Root = ShaderRoot;
		Request.IncludeDirectories = {Root, Root / "common", Root / "materials",
			Root / "materials/passes", Root / "materials/vertex"};
		Request.Debug = strstr(Core.Params, "-rdebug") != nullptr ||
			strstr(Core.Params, "-rdbg") != nullptr;
		return Request;
	}

	[[nodiscard]] bool LoadSceneMaterialResolver()
	{
		string_path MaterialRoot = {};
		FS.update_path(MaterialRoot, "$game_data$", "render_materials\\");
		SceneMaterialRoot = std::filesystem::path(MaterialRoot).lexically_normal();
		xr_vector<FMaterialDiagnostic> Diagnostics;
		auto Candidate =
			std::make_unique<Tiramisu::Editor::TiramisuEditorViewportMaterialResolver>();
		const bool Loaded = Candidate->Load(SceneMaterialRoot, &Diagnostics);
		for (const auto& Item : Diagnostics)
		{
			if (Item.Severity != EMaterialDiagnosticSeverity::Info)
				Msg("%s Editor material resolver [%s]: %s",
					Item.Severity == EMaterialDiagnosticSeverity::Error
						? "!" : "*", Item.Code.c_str(), Item.Message.c_str());
		}
		if (!Loaded)
			Diagnostic = "Failed to load editor viewport material assets";
		else
			SceneMaterialResolver = std::move(Candidate);
		return Loaded;
	}

	void RegisterSceneMaterialDependencies(
		const Tiramisu::Editor::FEditorViewportMaterialResolution& Resolution)
	{
		xr_vector<std::filesystem::path> Dependencies =
			SceneMaterialDependencies;
		Dependencies.insert(Dependencies.end(),
			Resolution.AssetDependencies.begin(), Resolution.AssetDependencies.end());
		Dependencies.push_back(ResolveR5ShaderPath(
			Resolution.Master.HlslTemplate));
		if (Resolution.Master.Implementation.Type == EMaterialImplementationType::Hlsl)
		{
			Dependencies.push_back(ResolveR5ShaderPath(
				Resolution.Master.Implementation.Source));
		}
		for (const xr_string& Dependency : Resolution.Master.Dependencies)
			Dependencies.push_back(ResolveR5ShaderPath(Dependency));
		if (const FMaterialVertexFactoryDefinition* VertexFactory =
			FindMaterialVertexFactoryDefinition("level_static"))
		{
			Dependencies.push_back(ResolveR5ShaderPath(
				VertexFactory->ShaderSource));
		}
		if (const FMaterialPassDefinition* Forward =
			FindMaterialPassDefinition(EMaterialPass::Forward))
		{
			Dependencies.push_back(ResolveR5ShaderPath(
				Forward->ShaderSource));
		}
		Dependencies.push_back(ResolveR5ShaderPath(
			"materials/passes/MaterialLightingCommon.hlsl"));
		std::ranges::sort(Dependencies);
		const auto Unique = std::ranges::unique(Dependencies);
		Dependencies.erase(Unique.begin(), Unique.end());
		if (Dependencies == SceneMaterialDependencies)
			return;
		SceneMaterialDependencies = std::move(Dependencies);
		SceneMaterialDependencyWatcher.Reset(SceneMaterialDependencies);
	}

	void QueueSceneMaterialCompile(const FEditorOwnedMaterialSlotSource& Source,
		const bool Force = false)
	{
		if (!SceneMaterialResolver || !SceneMaterialResolver->IsLoaded())
			return;
		Tiramisu::Editor::FEditorViewportLegacyMaterialSource Legacy;
		Legacy.MaterialSlot = Source.MaterialSlot.Value;
		Legacy.MaterialAsset = Source.MaterialAsset;
		Legacy.ShaderName = Source.ShaderName;
		if (!Source.TextureName.empty())
			Legacy.Textures.push_back(Source.TextureName);
		Legacy.SurfaceName = Source.SurfaceName;
		Legacy.TwoSided = (static_cast<u32>(Source.Flags) &
			static_cast<u32>(EEditorMaterialSlotFlags::TwoSided)) != 0;
		Tiramisu::Editor::FEditorViewportMaterialResolution Resolution =
			SceneMaterialResolver->Resolve(Legacy);
		FSceneMaterial& Material = SceneMaterials[Source.MaterialSlot.Value];
		Material.Source = Source;
		if (!Resolution.Succeeded())
		{
			Material.Diagnostic = FormatMaterialDiagnostics(Resolution.Diagnostics);
			Msg("! Editor material slot %llu could not be resolved",
				static_cast<unsigned long long>(Source.MaterialSlot.Value));
			return;
		}
		RegisterSceneMaterialDependencies(Resolution);

		if (Material.InstanceIndex == UINT32_MAX)
		{
			if (NextSceneMaterialInstance >= MaxEditorMaterialInstances)
			{
				Material.Diagnostic = "Editor material instance buffer is full";
				return;
			}
			Material.InstanceIndex = NextSceneMaterialInstance++;
		}
		if (!Force && Material.SourceKey == Resolution.CacheKey &&
			(Material.RequestedRevision != 0 || Material.AcceptedRevision != 0))
		{
			return;
		}
		Material.SourceKey = Resolution.CacheKey;
		Material.TwoSided = Resolution.TwoSided;
		Material.BlendMode = Resolution.Resolved.BlendMode;
		Material.Diagnostic.clear();
		Material.RequestedRevision = ++SceneMaterialRevision;
		SceneMaterialCompileQueue.ErasePendingIf(
			[Slot = Source.MaterialSlot.Value](
				const FSceneMaterialCompileRequest& Request)
			{
				return Request.MaterialSlot == Slot;
			});
		FSceneMaterialCompileRequest Request;
		Request.MaterialSlot = Source.MaterialSlot.Value;
		Request.Revision = Material.RequestedRevision;
		Request.TwoSided = Resolution.TwoSided;
		Request.Reload = Force;
		Request.Request = MakeSceneMaterialCompileRequest(Resolution);
		SceneMaterialCompileQueue.Enqueue(std::move(Request));
	}

	void StartSceneMaterialResolverReload()
	{
		if (SceneMaterialResolverReload.valid() || SceneMaterialRoot.empty())
			return;
		const std::filesystem::path Root = SceneMaterialRoot;
		SceneMaterialResolverReload = std::async(std::launch::async,
			[Root]() mutable
			{
				FSceneMaterialResolverReloadResult Result;
				Result.Resolver = std::make_unique<
					Tiramisu::Editor::TiramisuEditorViewportMaterialResolver>();
				Result.Loaded = Result.Resolver->Load(Root, &Result.Diagnostics);
				return Result;
			});
	}

	void PollSceneMaterialDependencies()
	{
		using namespace std::chrono_literals;
		if (!SceneMaterialReloadSmokeTriggered &&
			strstr(Core.Params, "-viewport-material-reload-smoke") != nullptr &&
			std::ranges::any_of(SceneMaterials,
				[](const auto& Entry)
				{
					return Entry.second.AcceptedRevision != 0;
				}))
		{
			SceneMaterialReloadSmokeTriggered = true;
			StartSceneMaterialResolverReload();
			Msg("* Editor scene material reload smoke: resolver reload requested");
		}
		if (SceneMaterialResolverReload.valid())
		{
			if (SceneMaterialResolverReload.wait_for(0s) !=
				std::future_status::ready)
			{
				return;
			}
			FSceneMaterialResolverReloadResult Result =
				SceneMaterialResolverReload.get();
			if (!Result.Loaded)
			{
				const xr_string ReloadDiagnostic =
					FormatMaterialDiagnostics(Result.Diagnostics);
				for (auto& [Slot, Material] : SceneMaterials)
				{
					(void)Slot;
					Material.Diagnostic = ReloadDiagnostic;
				}
				Msg("! Editor scene material reload failed; last-good pipelines remain active");
				return;
			}

			SceneMaterialResolver = std::move(Result.Resolver);
			xr_vector<FEditorOwnedMaterialSlotSource> Sources;
			Sources.reserve(SceneMaterials.size());
			for (const auto& [Slot, Material] : SceneMaterials)
			{
				(void)Slot;
				Sources.push_back(Material.Source);
			}
			SceneMaterialDependencies.clear();
			for (const FEditorOwnedMaterialSlotSource& Source : Sources)
				QueueSceneMaterialCompile(Source, true);
			Msg("* Editor scene materials reloaded: %u slots queued",
				static_cast<unsigned>(Sources.size()));
			return;
		}

		// A background filesystem poll can make the accepted pipeline revision
		// depend on host timing. Deterministic tests still permit the explicit
		// reload-smoke transaction above, but ignore ambient file changes.
		if (DeterministicTest.Enabled)
			return;

		const auto Now = std::chrono::steady_clock::now();
		if (Now < NextSceneMaterialDependencyPoll)
			return;
		NextSceneMaterialDependencyPoll = Now + 250ms;
		const xr_vector<Tiramisu::Editor::FMaterialDependencyChange> Changes =
			SceneMaterialDependencyWatcher.Poll();
		if (Changes.empty())
			return;
		for (const auto& Change : Changes)
			Msg("* Editor scene material dependency changed: %s",
				Change.Path.string().c_str());
		StartSceneMaterialResolverReload();
	}

	[[nodiscard]] FMaterialPreviewHandle CreateMaterialPreview()
	{
		u32 Index = 0;
		if (!FreeMaterialPreviewSlots.empty())
		{
			Index = FreeMaterialPreviewSlots.back();
			FreeMaterialPreviewSlots.pop_back();
		}
		else
		{
			if (MaterialPreviews.size() >= MaxMaterialPreviews)
				return {};
			Index = static_cast<u32>(MaterialPreviews.size());
			MaterialPreviews.emplace_back();
		}

		FMaterialPreview& Preview = MaterialPreviews[Index];
		VERIFY(!Preview.Alive);
		Preview.Alive = true;
		Preview.ViewportId = 0x80000000u | Index;
		Preview.MeshId = {0xf000000000000000ull |
			(static_cast<u64>(Preview.Generation) << 32u) | Index};
		Preview.Primitive = EMaterialPreviewPrimitive::Sphere;
		Preview.Environment = "Studio";
		Preview.State = EMaterialPreviewState::Unavailable;
		Preview.RequestedRevision = 0;
		Preview.AcceptedRevision = 0;
		Preview.Diagnostic.clear();
		Preview.Pipeline = nullptr;
		Preview.PipelineKey = 0;
		Preview.ParameterData.clear();
		Preview.ParameterLayoutHash = 0;
		Preview.DrawFlags = PreviewWhiteCubeDescriptorIndex;
		(void)Viewports[Preview.ViewportId];
		return {Index, Preview.Generation};
	}

	void QueueMaterialPreviewCompile(const FMaterialPreviewHandle Handle,
		const FMaterialPreviewSource& Source)
	{
		FMaterialPreview* Preview = FindPreview(Handle);
		if (!Preview || Source.Revision <= Preview->RequestedRevision)
			return;
		const bool PrimitiveChanged = Preview->Primitive != Source.Primitive;
		Preview->RequestedRevision = Source.Revision;
		Preview->Primitive = Source.Primitive;
		Preview->Environment.assign(Source.Environment);
		if (PrimitiveChanged && GpuMeshes.contains(Preview->MeshId.Value))
			(void)RebuildPreviewMesh(*Preview);
		Preview->State = EMaterialPreviewState::Compiling;
		Preview->Diagnostic.clear();
		Tiramisu::Editor::FMaterialPreviewCompileRequest Request =
			MakePreviewCompileRequest(Source);
		FMaterialPreviewCompileJob Job;
		Job.Handle = Handle;
		Job.Revision = Source.Revision;
		Job.Future = std::async(std::launch::async,
			[Request = std::move(Request)]() mutable
			{
				return Tiramisu::Editor::CompileMaterialPreview(Request);
			});
		MaterialPreviewCompileJobs.push_back(std::move(Job));
	}

	bool CreateSwapchain()
	{
		if (!Window || !NriDevice || !GraphicsQueue)
		{
			Diagnostic = "NRI editor swapchain prerequisites are missing";
			return false;
		}

		int PixelWidth = 0;
		int PixelHeight = 0;
		SDL_GetWindowSizeInPixels(Window, &PixelWidth, &PixelHeight);
		if (PixelWidth <= 0 || PixelHeight <= 0)
		{
			Diagnostic = "NRI editor window has no drawable area";
			return false;
		}

		nri::SwapChainDesc Desc = {};
		Desc.window.windows.hwnd = SDL_GetPointerProperty(
			SDL_GetWindowProperties(Window),
			SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
		Desc.queue = GraphicsQueue;
		Desc.width = static_cast<nri::Dim_t>(PixelWidth);
		Desc.height = static_cast<nri::Dim_t>(PixelHeight);
		Desc.textureNum = static_cast<u8>(QueuedFrameCount + 1);
		Desc.format = nri::SwapChainFormat::BT709_G22_8BIT;
		Desc.flags = nri::SwapChainBits::VSYNC;
		Desc.queuedFrameNum = static_cast<u8>(QueuedFrameCount);
		if (!Desc.window.windows.hwnd ||
			!Check(SwapChainInterface.CreateSwapChain(
				*NriDevice, Desc, SwapChain),
				"Failed to create the NRI editor swapchain"))
		{
			return false;
		}

		u32 TextureCount = 0;
		nri::Texture* const* Textures =
			SwapChainInterface.GetSwapChainTextures(*SwapChain, TextureCount);
		if (!Textures || TextureCount == 0)
		{
			Diagnostic = "NRI editor swapchain returned no textures";
			DestroySwapchain();
			return false;
		}

		SwapTextures.resize(TextureCount);
		SwapFormat = CoreInterface.GetTextureDesc(*Textures[0]).format;
		for (u32 Index = 0; Index < TextureCount; ++Index)
		{
			FSwapTexture& Item = SwapTextures[Index];
			Item.Texture = Textures[Index];

			nri::TextureViewDesc View = {};
			View.texture = Item.Texture;
			View.type = nri::TextureView::COLOR_ATTACHMENT;
			View.format = SwapFormat;
			if (!Check(CoreInterface.CreateTextureView(View, Item.ColorAttachment),
					"Failed to create an NRI editor swapchain attachment") ||
				!Check(CoreInterface.CreateFence(*NriDevice,
					nri::SWAPCHAIN_SEMAPHORE, Item.AcquireSemaphore),
					"Failed to create an NRI editor acquire semaphore") ||
				!Check(CoreInterface.CreateFence(*NriDevice,
					nri::SWAPCHAIN_SEMAPHORE, Item.ReleaseSemaphore),
					"Failed to create an NRI editor release semaphore"))
			{
				DestroySwapchain();
				return false;
			}
		}

		Width = static_cast<u32>(PixelWidth);
		Height = static_cast<u32>(PixelHeight);
		return true;
	}

	void DestroySwapchain()
	{
		for (FSwapTexture& Item : SwapTextures)
		{
			if (Item.AcquireSemaphore)
				CoreInterface.DestroyFence(Item.AcquireSemaphore);
			if (Item.ReleaseSemaphore)
				CoreInterface.DestroyFence(Item.ReleaseSemaphore);
			if (Item.ColorAttachment)
				CoreInterface.DestroyDescriptor(Item.ColorAttachment);
		}
		SwapTextures.clear();
		if (SwapChain)
			SwapChainInterface.DestroySwapChain(SwapChain);
		SwapChain = nullptr;
		SwapFormat = nri::Format::UNKNOWN;
		Width = 0;
		Height = 0;
	}

	bool EnsureSwapchainSize()
	{
		int PixelWidth = 0;
		int PixelHeight = 0;
		SDL_GetWindowSizeInPixels(Window, &PixelWidth, &PixelHeight);
		if (PixelWidth <= 0 || PixelHeight <= 0)
			return false;
		if (SwapChain && Width == static_cast<u32>(PixelWidth) &&
			Height == static_cast<u32>(PixelHeight))
		{
			return true;
		}
		CoreInterface.QueueWaitIdle(GraphicsQueue);
		DestroySwapchain();
		return CreateSwapchain();
	}

	void DestroyViewport(FViewport& Viewport)
	{
		RegisteredUserTextures.Unregister(Viewport.ShaderResource);
		if (Viewport.ShaderResource)
			CoreInterface.DestroyDescriptor(Viewport.ShaderResource);
		if (Viewport.ColorAttachment)
			CoreInterface.DestroyDescriptor(Viewport.ColorAttachment);
		if (Viewport.DepthAttachment)
			CoreInterface.DestroyDescriptor(Viewport.DepthAttachment);
		if (Viewport.Texture)
			CoreInterface.DestroyTexture(Viewport.Texture);
		if (Viewport.DepthTexture)
			CoreInterface.DestroyTexture(Viewport.DepthTexture);
		for (nri::Memory* MemoryAllocation : Viewport.Memory)
		{
			if (MemoryAllocation)
				CoreInterface.FreeMemory(MemoryAllocation);
		}
		Viewport.Texture = nullptr;
		Viewport.Memory.clear();
		Viewport.ShaderResource = nullptr;
		Viewport.ColorAttachment = nullptr;
		Viewport.DepthTexture = nullptr;
		Viewport.DepthAttachment = nullptr;
		Viewport.Width = 0;
		Viewport.Height = 0;
		Viewport.HasShaderResourceState = false;
		Viewport.HasDepthAttachmentState = false;
	}

	void DestroyViewports()
	{
		for (auto& [ViewportId, Viewport] : Viewports)
		{
			(void)ViewportId;
			if (Viewport.DebugDrawBuffer)
				CoreInterface.DestroyBuffer(Viewport.DebugDrawBuffer);
			Viewport.DebugDrawBuffer = nullptr;
			DestroyViewport(Viewport);
			DestroySceneViewportMaterialContext(Viewport);
		}
		Viewports.clear();
	}

	void DeferUiTexture(FUiTexture& Texture)
	{
		RegisteredUserTextures.Unregister(Texture.ShaderResource);
		if (Texture.Texture || Texture.ShaderResource)
		{
			DeferredUiTextures.push_back({Texture.Texture,
				Texture.ShaderResource, FrameIndex});
		}
		Texture = {};
	}

	void DestroyUiTexture(FDeferredUiTexture& Texture)
	{
		if (Texture.ShaderResource)
			CoreInterface.DestroyDescriptor(Texture.ShaderResource);
		if (Texture.Texture)
			CoreInterface.DestroyTexture(Texture.Texture);
		Texture = {};
	}

	void CollectDeferredUiTextures(const u64 CompletedFence)
	{
		for (auto It = DeferredUiTextures.begin(); It != DeferredUiTextures.end();)
		{
			if (It->RetireFence > CompletedFence)
			{
				++It;
				continue;
			}
			DestroyUiTexture(*It);
			It = DeferredUiTextures.erase(It);
		}
	}

	bool CreateUiTexture(const FEditorOwnedTextureUpload& Upload,
		FUiTexture& OutTexture)
	{
		nri::TextureDesc Desc = {};
		Desc.type = nri::TextureType::TEXTURE_2D;
		Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE;
		switch (Upload.Format)
		{
		case EEditorTextureFormat::Rgba8Srgb:
			Desc.format = nri::Format::RGBA8_SRGB;
			break;
		case EEditorTextureFormat::Bgra8Unorm:
			Desc.format = nri::Format::BGRA8_UNORM;
			break;
		case EEditorTextureFormat::Bgra8Srgb:
			Desc.format = nri::Format::BGRA8_SRGB;
			break;
		default:
			Desc.format = nri::Format::RGBA8_UNORM;
			break;
		}
		Desc.width = static_cast<nri::Dim_t>(Upload.Width);
		Desc.height = static_cast<nri::Dim_t>(Upload.Height);
		if (!Check(CoreInterface.CreateCommittedTexture(*NriDevice,
				nri::MemoryLocation::DEVICE, 1.0f, Desc, OutTexture.Texture),
				"Failed to create an NRI editor UI texture"))
		{
			return false;
		}

		nri::TextureViewDesc View = {};
		View.texture = OutTexture.Texture;
		View.type = nri::TextureView::TEXTURE;
		View.format = Desc.format;
		if (!Check(CoreInterface.CreateTextureView(View, OutTexture.ShaderResource),
				"Failed to create an NRI editor UI texture view"))
		{
			if (OutTexture.Texture)
				CoreInterface.DestroyTexture(OutTexture.Texture);
			OutTexture = {};
			return false;
		}
		if (!Upload.DebugName.empty())
			CoreInterface.SetDebugName(OutTexture.ShaderResource, Upload.DebugName.c_str());

		nri::TextureSubresourceUploadDesc Subresource = {};
		Subresource.slices = Upload.Pixels.data();
		Subresource.rowPitch = Upload.RowPitch;
		Subresource.slicePitch =
			static_cast<u64>(Upload.RowPitch) * Upload.Height;
		Subresource.sliceNum = 1;
		nri::TextureUploadDesc TextureUpload = {};
		TextureUpload.subresources = &Subresource;
		TextureUpload.texture = OutTexture.Texture;
		TextureUpload.after = {nri::AccessBits::SHADER_RESOURCE,
			nri::Layout::SHADER_RESOURCE};
		if (!Check(HelperInterface.UploadData(*GraphicsQueue,
				&TextureUpload, 1, nullptr, 0),
				"Failed to upload an NRI editor UI texture"))
		{
			CoreInterface.DestroyDescriptor(OutTexture.ShaderResource);
			CoreInterface.DestroyTexture(OutTexture.Texture);
			OutTexture = {};
			return false;
		}

		OutTexture.Width = Upload.Width;
		OutTexture.Height = Upload.Height;
		OutTexture.Revision = Upload.Revision;
		return true;
	}

	void ProcessUiTextureMailbox()
	{
		TiramisuEditorTextureMailboxPacket Packet;
		if (!UiTextureMailbox.Consume(Packet))
			return;

		std::scoped_lock Lock(UiTexturesMutex);
		for (const FEditorTextureHandle Handle : Packet.Releases)
		{
			const auto It = UiTextures.find(TextureHandleKey(Handle));
			if (It == UiTextures.end())
				continue;
			DeferUiTexture(It->second);
			UiTextures.erase(It);
		}
		for (const FEditorOwnedTextureUpload& Upload : Packet.Updates)
		{
			FUiTexture NewTexture;
			if (!CreateUiTexture(Upload, NewTexture))
				continue;
			const u64 Key = TextureHandleKey(Upload.Handle);
			const auto Existing = UiTextures.find(Key);
			if (Existing != UiTextures.end())
			{
				DeferUiTexture(Existing->second);
				Existing->second = NewTexture;
			}
			else
			{
				UiTextures.emplace(Key, NewTexture);
			}
			RegisteredUserTextures.Register(NewTexture.ShaderResource);
		}
	}

	void DestroyUiTextures()
	{
		std::scoped_lock Lock(UiTexturesMutex);
		for (auto& [Key, Texture] : UiTextures)
		{
			(void)Key;
			RegisteredUserTextures.Unregister(Texture.ShaderResource);
			FDeferredUiTexture Immediate{Texture.Texture,
				Texture.ShaderResource, 0};
			DestroyUiTexture(Immediate);
		}
		UiTextures.clear();
		for (FDeferredUiTexture& Texture : DeferredUiTextures)
			DestroyUiTexture(Texture);
		DeferredUiTextures.clear();
	}

	[[nodiscard]] bool CreatePreviewBuffer(const u64 Size,
		const nri::BufferUsageBits Usage, const nri::BufferView ViewType,
		nri::Buffer*& Buffer, nri::Descriptor*& Descriptor)
	{
		nri::BufferDesc Desc = {};
		Desc.size = Size;
		Desc.structureStride = ViewType == nri::BufferView::CONSTANT_BUFFER ? 0 : 4;
		Desc.usage = Usage;
		if (!Check(CoreInterface.CreateCommittedBuffer(*NriDevice,
				nri::MemoryLocation::DEVICE_UPLOAD, 0.5f, Desc, Buffer),
				"Failed to create a material preview buffer"))
		{
			return false;
		}
		nri::BufferViewDesc View = {};
		View.buffer = Buffer;
		View.type = ViewType;
		View.offset = 0;
		View.size = Size;
		if (!Check(CoreInterface.CreateBufferView(View, Descriptor),
				"Failed to create a material preview buffer view"))
		{
			CoreInterface.DestroyBuffer(Buffer);
			Buffer = nullptr;
			return false;
		}
		return true;
	}

	[[nodiscard]] bool WritePreviewBuffer(nri::Buffer* Buffer,
		const u64 Offset, const void* Data, const u64 Size)
	{
		if (!Buffer || !Data || Size == 0)
			return false;
		void* Destination = CoreInterface.MapBuffer(*Buffer, Offset, Size);
		if (!Destination)
			return false;
		std::memcpy(Destination, Data, static_cast<size_t>(Size));
		CoreInterface.UnmapBuffer(*Buffer);
		Statistics.RecordUpload(Size);
		return true;
	}

	[[nodiscard]] bool CreateMaterialPreviewFallbackCube()
	{
		nri::TextureDesc Desc = {};
		Desc.type = nri::TextureType::TEXTURE_2D;
		Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE;
		Desc.format = nri::Format::RGBA8_UNORM;
		Desc.width = 1;
		Desc.height = 1;
		Desc.layerNum = 6;
		if (!Check(CoreInterface.CreateCommittedTexture(*NriDevice,
				nri::MemoryLocation::DEVICE, 1.0f, Desc,
				PreviewWhiteCubeTexture),
				"Failed to create the material preview fallback cube"))
		{
			return false;
		}

		nri::TextureViewDesc View = {};
		View.texture = PreviewWhiteCubeTexture;
		View.type = nri::TextureView::TEXTURE_CUBE;
		View.format = Desc.format;
		View.mipNum = 1;
		View.layerNum = 6;
		if (!Check(CoreInterface.CreateTextureView(View,
				PreviewWhiteCubeDescriptor),
				"Failed to create the material preview fallback cube view"))
		{
			return false;
		}

		const u32 WhitePixel = 0xffffffffu;
		xr_array<nri::TextureSubresourceUploadDesc, 6> Subresources = {};
		for (nri::TextureSubresourceUploadDesc& Subresource : Subresources)
		{
			Subresource.slices = &WhitePixel;
			Subresource.rowPitch = sizeof(WhitePixel);
			Subresource.slicePitch = sizeof(WhitePixel);
			Subresource.sliceNum = 1;
		}
		nri::TextureUploadDesc Upload = {};
		Upload.subresources = Subresources.data();
		Upload.texture = PreviewWhiteCubeTexture;
		Upload.after = {nri::AccessBits::SHADER_RESOURCE,
			nri::Layout::SHADER_RESOURCE};
		return Check(HelperInterface.UploadData(*GraphicsQueue,
			&Upload, 1, nullptr, 0),
			"Failed to upload the material preview fallback cube");
	}

	[[nodiscard]] xr_optional<xr_string> ResolveMaterialPreviewTextureFile(
		const xr_string_view AssetPath) const
	{
		const xr_string Normalized =
			Tiramisu::Editor::NormalizeMaterialPreviewTexturePath(AssetPath);
		if (Normalized.empty())
			return std::nullopt;
		xr_string VirtualName = Normalized;
		std::ranges::replace(VirtualName, '/', '\\');
		string_path FileName = {};
		if (!FS.exist(FileName, _game_textures_, VirtualName.c_str(), ".dds"))
			return std::nullopt;
		VirtualName += ".dds";
		return VirtualName;
	}

	[[nodiscard]] u32 ResolveMaterialPreviewTexture(
		const xr_string_view AssetPath, const bool ExpectCube,
		xr_string& Warning)
	{
		using namespace Tiramisu::Editor;
		const xr_string Normalized =
			NormalizeMaterialPreviewTexturePath(AssetPath);
		const xr_string CacheKey = xr_string(ExpectCube ? "cube:" : "2d:") +
			Normalized;
		if (const auto It = MaterialPreviewTextures.find(CacheKey);
			It != MaterialPreviewTextures.end())
		{
			return It->second.DescriptorIndex;
		}

		const u32 Fallback = ExpectCube
			? PreviewWhiteCubeDescriptorIndex
			: PreviewWhiteTextureDescriptorIndex;
		const xr_optional<xr_string> FileName =
			ResolveMaterialPreviewTextureFile(AssetPath);
		if (!FileName)
		{
			// The built-in white reference intentionally has no disk asset.
			if (Normalized != "default/default_white")
				Warning = "Material preview texture was not found: " +
					xr_string(AssetPath);
			return Fallback;
		}

		IReader* Reader = FS.r_open(_game_textures_, FileName->c_str());
		if (!Reader)
		{
			Warning = "Material preview texture could not be opened: " + *FileName;
			return Fallback;
		}
		RedImageTool::RedImage Image;
		const bool Loaded = Image.LoadFromMemory(Reader->pointer(), Reader->length());
		FS.r_close(Reader);
		if (!Loaded || Image.Empty())
		{
			Warning = "Material preview texture could not be decoded: " + *FileName;
			return Fallback;
		}
		if (Image.GetFormat() == RedImageTool::RedTexturePixelFormat::R8G8B8)
			Image.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);
		if (ExpectCube != Image.IsCubeMap() ||
			(!ExpectCube && Image.GetDepth() != 1) ||
			(ExpectCube && Image.GetDepth() != 6))
		{
			Warning = "Material preview texture has the wrong dimension: " +
				xr_string(AssetPath);
			return Fallback;
		}
		if (NextMaterialPreviewTextureDescriptor >= PreviewResourceDescriptorCount)
		{
			Warning = "Material preview texture descriptor heap is full";
			return Fallback;
		}

		const nri::Format Format = MaterialPreviewTextureFormat(
			Image.GetFormat(), true);
		if (Format == nri::Format::UNKNOWN || Image.GetWidth() == 0 ||
			Image.GetHeight() == 0 || Image.GetMips() == 0)
		{
			Warning = "Material preview texture format is unsupported: " +
				xr_string(AssetPath);
			return Fallback;
		}

		FMaterialPreviewTexture Resource;
		Resource.Cube = ExpectCube;
		Resource.DescriptorIndex = NextMaterialPreviewTextureDescriptor;
		nri::TextureDesc Desc = {};
		Desc.type = nri::TextureType::TEXTURE_2D;
		Desc.usage = nri::TextureUsageBits::SHADER_RESOURCE;
		Desc.format = Format;
		Desc.width = static_cast<nri::Dim_t>(Image.GetWidth());
		Desc.height = static_cast<nri::Dim_t>(Image.GetHeight());
		Desc.mipNum = static_cast<nri::Dim_t>(Image.GetMips());
		Desc.layerNum = static_cast<nri::Dim_t>(Image.GetDepth());
		if (CoreInterface.CreateCommittedTexture(*NriDevice,
				nri::MemoryLocation::DEVICE, 1.0f, Desc, Resource.Texture) !=
			nri::Result::SUCCESS)
		{
			Warning = "NRI could not create material preview texture: " +
				xr_string(AssetPath);
			return Fallback;
		}

		nri::TextureViewDesc View = {};
		View.texture = Resource.Texture;
		View.type = ExpectCube ? nri::TextureView::TEXTURE_CUBE :
			nri::TextureView::TEXTURE;
		View.format = Format;
		View.mipNum = Desc.mipNum;
		View.layerNum = Desc.layerNum;
		if (CoreInterface.CreateTextureView(View, Resource.Descriptor) !=
			nri::Result::SUCCESS)
		{
			CoreInterface.DestroyTexture(Resource.Texture);
			Warning = "NRI could not create material preview texture view: " +
				xr_string(AssetPath);
			return Fallback;
		}

		xr_vector<nri::TextureSubresourceUploadDesc> Subresources;
		Subresources.reserve(Image.GetDepth() * Image.GetMips());
		auto* Pixels = static_cast<RedImageTool::u8*>(*Image);
		for (size_t Layer = 0; Layer < Image.GetDepth(); ++Layer)
		{
			for (size_t Mip = 0; Mip < Image.GetMips(); ++Mip)
			{
				const size_t Width =
					RedImageTool::RedTextureUtils::GetMip(Image.GetWidth(), Mip);
				const size_t Height =
					RedImageTool::RedTextureUtils::GetMip(Image.GetHeight(), Mip);
				nri::TextureSubresourceUploadDesc& Subresource =
					Subresources.emplace_back();
				Subresource.slices = RedImageTool::RedTextureUtils::GetImage(
					Pixels, Image.GetWidth(), Image.GetHeight(), Image.GetMips(),
					Layer, Mip, Image.GetFormat());
				Subresource.rowPitch = static_cast<u32>(
					RedImageTool::RedTextureUtils::GetSizeWidth(
						Width, Image.GetFormat()));
				Subresource.slicePitch = static_cast<u32>(
					RedImageTool::RedTextureUtils::GetSizeDepth(
						Width, Height, Image.GetFormat()));
				Subresource.sliceNum = 1;
				Resource.ByteSize += Subresource.slicePitch;
			}
		}
		nri::TextureUploadDesc Upload = {};
		Upload.subresources = Subresources.data();
		Upload.texture = Resource.Texture;
		Upload.after = {nri::AccessBits::SHADER_RESOURCE,
			nri::Layout::SHADER_RESOURCE};
		if (HelperInterface.UploadData(*GraphicsQueue, &Upload, 1, nullptr, 0) !=
			nri::Result::SUCCESS)
		{
			CoreInterface.DestroyDescriptor(Resource.Descriptor);
			CoreInterface.DestroyTexture(Resource.Texture);
			Warning = "NRI could not upload material preview texture: " +
				xr_string(AssetPath);
			return Fallback;
		}

		const nri::Descriptor* Descriptors[] = {Resource.Descriptor};
		const nri::UpdateDescriptorRangeDesc Update = {PreviewResourcesSet, 0,
			Resource.DescriptorIndex, Descriptors, 1};
		CoreInterface.UpdateDescriptorRanges(&Update, 1);
		++NextMaterialPreviewTextureDescriptor;
		const u32 Result = Resource.DescriptorIndex;
		Msg("* Material preview texture loaded: %s (%s, %zux%zu, mips=%zu, descriptor=%u)",
			Normalized.c_str(), ExpectCube ? "TextureCube" : "Texture2D",
			Image.GetWidth(), Image.GetHeight(), Image.GetMips(), Result);
		MaterialPreviewTextures.emplace(CacheKey, std::move(Resource));
		return Result;
	}

	[[nodiscard]] bool CreateMaterialPreviewContext()
	{
		nri::DescriptorPoolDesc Pool = {};
		Pool.descriptorSetMaxNum = 3 + MaxSceneViewports * QueuedFrameCount;
		Pool.mutableMaxNum = PreviewResourceDescriptorCount;
		Pool.samplerMaxNum = 4;
		Pool.constantBufferMaxNum = 1 + MaxSceneViewports * QueuedFrameCount;
		if (!Check(CoreInterface.CreateDescriptorPool(
				*NriDevice, Pool, PreviewDescriptorPool),
				"Failed to create the material preview descriptor pool"))
		{
			return false;
		}

		const nri::DescriptorRangeDesc Ranges[] = {
			{0, PreviewResourceDescriptorCount, nri::DescriptorType::MUTABLE,
				nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER,
				nri::DescriptorRangeBits::ARRAY |
					nri::DescriptorRangeBits::PARTIALLY_BOUND},
			{1, 4, nri::DescriptorType::SAMPLER,
				nri::StageBits::VERTEX_SHADER | nri::StageBits::FRAGMENT_SHADER,
				nri::DescriptorRangeBits::ARRAY |
					nri::DescriptorRangeBits::PARTIALLY_BOUND}};
		const nri::DescriptorRangeDesc Constants = {0, 1,
			nri::DescriptorType::CONSTANT_BUFFER, nri::StageBits::ALL};
		const nri::DescriptorSetDesc Sets[] = {
			{0, &Ranges[0], 1}, {1, &Ranges[1], 1}, {2, &Constants, 1}};
		nri::PipelineLayoutDesc Layout = {};
		Layout.descriptorSets = Sets;
		Layout.descriptorSetNum = static_cast<u32>(std::size(Sets));
		Layout.shaderStages = nri::StageBits::VERTEX_SHADER |
			nri::StageBits::FRAGMENT_SHADER;
		Layout.flags = nri::PipelineLayoutBits::RESOURCE_HEAP_DIRECTLY_INDEXED |
			nri::PipelineLayoutBits::SAMPLER_HEAP_DIRECTLY_INDEXED;
		if (Api == ETiramisuEditorGraphicsApi::D3D12)
			Layout.flags |= nri::PipelineLayoutBits::ENABLE_DRAW_PARAMETERS_EMULATION;
		if (!Check(CoreInterface.CreatePipelineLayout(
				*NriDevice, Layout, PreviewPipelineLayout),
				"Failed to create the material preview pipeline layout"))
		{
			return false;
		}
		if (!Check(CoreInterface.AllocateDescriptorSets(*PreviewDescriptorPool,
				*PreviewPipelineLayout, 0, &PreviewResourcesSet, 1, 0),
				"Failed to allocate the material preview resource heap") ||
			!Check(CoreInterface.AllocateDescriptorSets(*PreviewDescriptorPool,
				*PreviewPipelineLayout, 1, &PreviewSamplersSet, 1, 0),
				"Failed to allocate the material preview sampler heap") ||
			!Check(CoreInterface.AllocateDescriptorSets(*PreviewDescriptorPool,
				*PreviewPipelineLayout, 2, &PreviewConstantsSet, 1, 0),
				"Failed to allocate the material preview constant set"))
		{
			return false;
		}

		if (!CreatePreviewBuffer(
				u64(MaxEditorDrawRecords) * QueuedFrameCount *
					MaterialDrawGpuDataSize,
				nri::BufferUsageBits::SHADER_RESOURCE,
				nri::BufferView::BYTE_ADDRESS_BUFFER,
				PreviewDrawDataBuffer, PreviewDrawDataDescriptor) ||
			!CreatePreviewBuffer(
				u64(MaxEditorMaterialInstances) * QueuedFrameCount *
					MaterialInstanceGpuDataSize,
				nri::BufferUsageBits::SHADER_RESOURCE,
				nri::BufferView::BYTE_ADDRESS_BUFFER,
				PreviewMaterialInstanceBuffer,
				PreviewMaterialInstanceDescriptor) ||
			!CreatePreviewBuffer(
				u64(MaxEditorMaterialInstances) * QueuedFrameCount *
					PreviewParameterStride,
				nri::BufferUsageBits::SHADER_RESOURCE,
				nri::BufferView::BYTE_ADDRESS_BUFFER,
				PreviewMaterialParameterBuffer,
				PreviewMaterialParameterDescriptor) ||
			!CreatePreviewBuffer(
				u64(MaxSceneViewports) * QueuedFrameCount *
					MaxSceneLightsPerViewport *
					MaterialLightGpuDataSize,
				nri::BufferUsageBits::SHADER_RESOURCE,
				nri::BufferView::BYTE_ADDRESS_BUFFER,
				PreviewLightDataBuffer,
				PreviewLightDataDescriptor) ||
			!CreatePreviewBuffer(256, nri::BufferUsageBits::CONSTANT_BUFFER,
				nri::BufferView::CONSTANT_BUFFER,
				PreviewGlobalConstantsBuffer,
				PreviewGlobalConstantsDescriptor))
		{
			return false;
		}

		nri::SamplerDesc Sampler = {};
		Sampler.filters = {nri::Filter::LINEAR, nri::Filter::LINEAR,
			nri::Filter::LINEAR};
		Sampler.mipMax = 16.0f;
		if (!Check(CoreInterface.CreateSampler(*NriDevice, Sampler,
				PreviewDefaultSampler),
				"Failed to create the material preview sampler"))
		{
			return false;
		}

		nri::TextureDesc White = {};
		White.type = nri::TextureType::TEXTURE_2D;
		White.usage = nri::TextureUsageBits::SHADER_RESOURCE;
		White.format = nri::Format::RGBA8_UNORM;
		White.width = 1;
		White.height = 1;
		if (!Check(CoreInterface.CreateCommittedTexture(*NriDevice,
				nri::MemoryLocation::DEVICE, 1.0f, White,
				PreviewWhiteTexture),
				"Failed to create the material preview fallback texture"))
		{
			return false;
		}
		nri::TextureViewDesc WhiteView = {};
		WhiteView.texture = PreviewWhiteTexture;
		WhiteView.type = nri::TextureView::TEXTURE;
		WhiteView.format = White.format;
		if (!Check(CoreInterface.CreateTextureView(
				WhiteView, PreviewWhiteTextureDescriptor),
				"Failed to create the material preview fallback texture view"))
		{
			return false;
		}
		const u32 WhitePixel = 0xffffffffu;
		nri::TextureSubresourceUploadDesc WhiteSubresource = {};
		WhiteSubresource.slices = &WhitePixel;
		WhiteSubresource.rowPitch = sizeof(WhitePixel);
		WhiteSubresource.slicePitch = sizeof(WhitePixel);
		WhiteSubresource.sliceNum = 1;
		nri::TextureUploadDesc WhiteUpload = {};
		WhiteUpload.subresources = &WhiteSubresource;
		WhiteUpload.texture = PreviewWhiteTexture;
		WhiteUpload.after = {nri::AccessBits::SHADER_RESOURCE,
			nri::Layout::SHADER_RESOURCE};
		if (!Check(HelperInterface.UploadData(*GraphicsQueue,
				&WhiteUpload, 1, nullptr, 0),
				"Failed to upload the material preview fallback texture"))
		{
			return false;
		}
		if (!CreateMaterialPreviewFallbackCube())
			return false;

		const nri::Descriptor* BufferDescriptors[] = {
			PreviewDrawDataDescriptor, PreviewMaterialInstanceDescriptor,
			PreviewMaterialParameterDescriptor};
		const nri::Descriptor* LightBufferDescriptors[] = {
			PreviewLightDataDescriptor};
		const nri::Descriptor* TextureDescriptors[] = {
			PreviewWhiteTextureDescriptor, PreviewWhiteCubeDescriptor};
		const nri::Descriptor* Samplers[] = {PreviewDefaultSampler};
		const nri::Descriptor* ConstantsDescriptors[] = {
			PreviewGlobalConstantsDescriptor};
		const nri::UpdateDescriptorRangeDesc Updates[] = {
			// NRI mutable ranges require one descriptor type per update call.
			{PreviewResourcesSet, 0, 0, BufferDescriptors,
				static_cast<u32>(std::size(BufferDescriptors))},
			{PreviewResourcesSet, 0, PreviewWhiteTextureDescriptorIndex,
				TextureDescriptors,
				static_cast<u32>(std::size(TextureDescriptors))},
			{PreviewResourcesSet, 0, PreviewLightDataDescriptorIndex,
				LightBufferDescriptors, 1},
			{PreviewSamplersSet, 0, 0, Samplers, 1},
			{PreviewConstantsSet, 0, 0, ConstantsDescriptors, 1}};
		CoreInterface.UpdateDescriptorRanges(Updates,
			static_cast<u32>(std::size(Updates)));

		Fmatrix View;
		Fmatrix Projection;
		Fmatrix ViewProjection;
		Fvector Position = {0.0f, 0.0f, -3.2f};
		Fvector Direction = {0.0f, 0.0f, 1.0f};
		Fvector Up = {0.0f, 1.0f, 0.0f};
		View.build_camera_dir(Position, Direction, Up);
		Projection.build_projection(deg2rad(45.0f), 1.0f, 0.05f, 100.0f);
		ViewProjection.mul(Projection, View);
		FEditorMaterialGlobalConstants Global;
		Global.ViewProjectionWorldMatrix =
			MakeConstantBufferMatrix(ViewProjection);
		Global.CameraPositionAndTime = {Position.x, Position.y, Position.z,
			DeterministicTest.Enabled
				? DeterministicTest.FixedShaderTimeSeconds : 0.0f};
		Global.DrawDataBufferIndex = PreviewDrawDataDescriptorIndex;
		Global.MaterialInstanceBufferIndex =
			PreviewMaterialInstanceDescriptorIndex;
		Global.MaterialParameterBufferIndex =
			PreviewMaterialParameterDescriptorIndex;
		Global.DefaultMaterialSamplerIndex = PreviewDefaultSamplerIndex;
		Global.LightDataBufferIndex = PreviewLightDataDescriptorIndex;
		NextMaterialPreviewTextureDescriptor =
			PreviewFirstAssetTextureDescriptorIndex;
		return WritePreviewBuffer(PreviewGlobalConstantsBuffer, 0,
			&Global, sizeof(Global));
	}

	[[nodiscard]] bool EnsureSceneViewportMaterialContext(FViewport& Viewport)
	{
		if (Viewport.SceneDrawBase == UINT32_MAX)
		{
			if (SceneViewportCount >= MaxSceneViewports)
			{
				Diagnostic = "Editor material draw ranges are exhausted";
				return false;
			}
			const u32 ViewportSlot = SceneViewportCount++;
			Viewport.SceneDrawBase = MaxMaterialPreviews +
				ViewportSlot * MaxSceneDrawsPerViewport;
			Viewport.SceneLightBase =
				ViewportSlot * MaxSceneLightsPerViewport;
		}
		if (Viewport.SceneConstantsBuffer)
			return true;

		nri::BufferDesc Buffer = {};
		Buffer.size = 256ull * QueuedFrameCount;
		Buffer.usage = nri::BufferUsageBits::CONSTANT_BUFFER;
		if (!Check(CoreInterface.CreateCommittedBuffer(*NriDevice,
				nri::MemoryLocation::DEVICE_UPLOAD, 0.5f, Buffer,
				Viewport.SceneConstantsBuffer),
				"Failed to create editor scene material constants"))
		{
			return false;
		}
		for (u32 Frame = 0; Frame < QueuedFrameCount; ++Frame)
		{
			nri::BufferViewDesc View = {};
			View.buffer = Viewport.SceneConstantsBuffer;
			View.type = nri::BufferView::CONSTANT_BUFFER;
			View.offset = 256ull * Frame;
			View.size = 256;
			if (!Check(CoreInterface.CreateBufferView(View,
					Viewport.SceneConstantsDescriptors[Frame]),
					"Failed to create editor scene material constants view") ||
				!Check(CoreInterface.AllocateDescriptorSets(*PreviewDescriptorPool,
					*PreviewPipelineLayout, 2,
					&Viewport.SceneConstantsSets[Frame], 1, 0),
					"Failed to allocate editor scene material constants set"))
			{
				return false;
			}
			const nri::Descriptor* Constants[] = {
				Viewport.SceneConstantsDescriptors[Frame]};
			const nri::UpdateDescriptorRangeDesc Update = {
				Viewport.SceneConstantsSets[Frame], 0, 0, Constants, 1};
			CoreInterface.UpdateDescriptorRanges(&Update, 1);
		}
		return true;
	}

	void DestroySceneViewportMaterialContext(FViewport& Viewport)
	{
		for (nri::Descriptor*& Descriptor : Viewport.SceneConstantsDescriptors)
		{
			if (Descriptor)
				CoreInterface.DestroyDescriptor(Descriptor);
			Descriptor = nullptr;
		}
		if (Viewport.SceneConstantsBuffer)
			CoreInterface.DestroyBuffer(Viewport.SceneConstantsBuffer);
		Viewport.SceneConstantsBuffer = nullptr;
		Viewport.SceneConstantsSets = {};
	}

	void DestroyMaterialPreviewContext()
	{
		for (FMaterialPreviewCompileJob& Job : MaterialPreviewCompileJobs)
			if (Job.Future.valid()) Job.Future.wait();
		MaterialPreviewCompileJobs.clear();
		SceneMaterialCompileQueue.CancelPendingAndWait();
		if (SceneMaterialResolverReload.valid())
			SceneMaterialResolverReload.wait();
		for (FMaterialPreview& Preview : MaterialPreviews)
		{
			if (Preview.Pipeline)
				CoreInterface.DestroyPipeline(Preview.Pipeline);
			Preview.Pipeline = nullptr;
		}
		for (auto& [Slot, Material] : SceneMaterials)
		{
			(void)Slot;
			Material.Pipeline = nullptr;
			Material.PipelineKey = 0;
			Material.PipelineTwoSided = false;
		}
		SceneMaterials.clear();
		for (auto& [Key, Entry] : ScenePipelineCache)
		{
			(void)Key;
			if (Entry.Pipeline)
				CoreInterface.DestroyPipeline(Entry.Pipeline);
		}
		ScenePipelineCache.clear();
		SceneMaterialResolver.reset();
		SceneMaterialDependencies.clear();
		SceneMaterialDependencyWatcher.Reset({});
		for (FDeferredPipeline& Deferred : DeferredPipelines)
			if (Deferred.Pipeline) CoreInterface.DestroyPipeline(Deferred.Pipeline);
		DeferredPipelines.clear();
		for (auto& [Key, Resource] : MaterialPreviewTextures)
		{
			(void)Key;
			if (Resource.Descriptor)
				CoreInterface.DestroyDescriptor(Resource.Descriptor);
			if (Resource.Texture)
				CoreInterface.DestroyTexture(Resource.Texture);
		}
		MaterialPreviewTextures.clear();

		if (PreviewDefaultSampler) CoreInterface.DestroyDescriptor(PreviewDefaultSampler);
		if (PreviewWhiteCubeDescriptor) CoreInterface.DestroyDescriptor(PreviewWhiteCubeDescriptor);
		if (PreviewWhiteTextureDescriptor) CoreInterface.DestroyDescriptor(PreviewWhiteTextureDescriptor);
		if (PreviewGlobalConstantsDescriptor) CoreInterface.DestroyDescriptor(PreviewGlobalConstantsDescriptor);
		if (PreviewLightDataDescriptor) CoreInterface.DestroyDescriptor(PreviewLightDataDescriptor);
		if (PreviewMaterialParameterDescriptor) CoreInterface.DestroyDescriptor(PreviewMaterialParameterDescriptor);
		if (PreviewMaterialInstanceDescriptor) CoreInterface.DestroyDescriptor(PreviewMaterialInstanceDescriptor);
		if (PreviewDrawDataDescriptor) CoreInterface.DestroyDescriptor(PreviewDrawDataDescriptor);
		if (PreviewWhiteCubeTexture) CoreInterface.DestroyTexture(PreviewWhiteCubeTexture);
		if (PreviewWhiteTexture) CoreInterface.DestroyTexture(PreviewWhiteTexture);
		if (PreviewGlobalConstantsBuffer) CoreInterface.DestroyBuffer(PreviewGlobalConstantsBuffer);
		if (PreviewLightDataBuffer) CoreInterface.DestroyBuffer(PreviewLightDataBuffer);
		if (PreviewMaterialParameterBuffer) CoreInterface.DestroyBuffer(PreviewMaterialParameterBuffer);
		if (PreviewMaterialInstanceBuffer) CoreInterface.DestroyBuffer(PreviewMaterialInstanceBuffer);
		if (PreviewDrawDataBuffer) CoreInterface.DestroyBuffer(PreviewDrawDataBuffer);
		if (PreviewDescriptorPool) CoreInterface.DestroyDescriptorPool(PreviewDescriptorPool);
		if (PreviewPipelineLayout) CoreInterface.DestroyPipelineLayout(PreviewPipelineLayout);

		PreviewDefaultSampler = nullptr;
		PreviewWhiteCubeDescriptor = nullptr;
		PreviewWhiteTextureDescriptor = nullptr;
		PreviewGlobalConstantsDescriptor = nullptr;
		PreviewLightDataDescriptor = nullptr;
		PreviewMaterialParameterDescriptor = nullptr;
		PreviewMaterialInstanceDescriptor = nullptr;
		PreviewDrawDataDescriptor = nullptr;
		PreviewWhiteCubeTexture = nullptr;
		PreviewWhiteTexture = nullptr;
		PreviewGlobalConstantsBuffer = nullptr;
		PreviewLightDataBuffer = nullptr;
		PreviewMaterialParameterBuffer = nullptr;
		PreviewMaterialInstanceBuffer = nullptr;
		PreviewDrawDataBuffer = nullptr;
		PreviewResourcesSet = nullptr;
		PreviewSamplersSet = nullptr;
		PreviewConstantsSet = nullptr;
		PreviewDescriptorPool = nullptr;
		PreviewPipelineLayout = nullptr;
		NextMaterialPreviewTextureDescriptor =
			PreviewFirstAssetTextureDescriptorIndex;
		MaterialPreviews.clear();
		FreeMaterialPreviewSlots.clear();
		NextSceneMaterialInstance = MaxMaterialPreviews;
		SceneViewportCount = 0;
		SceneMaterialRevision = 0;
		SceneMaterialRoot.clear();
		NextSceneMaterialDependencyPoll = {};
		SceneMaterialReloadSmokeTriggered = false;
	}

	bool CreateViewport(FViewport& Viewport)
	{
		nri::TextureDesc TextureDesc = {};
		TextureDesc.type = nri::TextureType::TEXTURE_2D;
		TextureDesc.usage = nri::TextureUsageBits::SHADER_RESOURCE |
			nri::TextureUsageBits::COLOR_ATTACHMENT;
		TextureDesc.format = nri::Format::RGBA8_UNORM;
		TextureDesc.width = static_cast<nri::Dim_t>(Viewport.DesiredWidth);
		TextureDesc.height = static_cast<nri::Dim_t>(Viewport.DesiredHeight);
		if (!Check(CoreInterface.CreateTexture(*NriDevice,
				TextureDesc, Viewport.Texture),
				"Failed to create an NRI editor viewport texture"))
		{
			return false;
		}

		nri::ResourceGroupDesc ResourceGroup = {};
		ResourceGroup.memoryLocation = nri::MemoryLocation::DEVICE;
		ResourceGroup.textures = &Viewport.Texture;
		ResourceGroup.textureNum = 1;
		const u32 AllocationCount =
			HelperInterface.CalculateAllocationNumber(*NriDevice, ResourceGroup);
		if (AllocationCount == 0)
		{
			Diagnostic = "NRI editor viewport has no compatible memory allocation";
			DestroyViewport(Viewport);
			return false;
		}
		Viewport.Memory.resize(AllocationCount);
		if (!Check(HelperInterface.AllocateAndBindMemory(*NriDevice,
				ResourceGroup, Viewport.Memory.data()),
				"Failed to allocate an NRI editor viewport texture"))
		{
			DestroyViewport(Viewport);
			return false;
		}

		nri::TextureViewDesc ShaderView = {};
		ShaderView.texture = Viewport.Texture;
		ShaderView.type = nri::TextureView::TEXTURE;
		ShaderView.format = TextureDesc.format;
		nri::TextureViewDesc AttachmentView = ShaderView;
		AttachmentView.type = nri::TextureView::COLOR_ATTACHMENT;
		if (!Check(CoreInterface.CreateTextureView(
				ShaderView, Viewport.ShaderResource),
				"Failed to create an NRI editor viewport shader resource") ||
			!Check(CoreInterface.CreateTextureView(
				AttachmentView, Viewport.ColorAttachment),
				"Failed to create an NRI editor viewport attachment"))
		{
			DestroyViewport(Viewport);
			return false;
		}

		nri::TextureDesc DepthDesc = {};
		DepthDesc.type = nri::TextureType::TEXTURE_2D;
		DepthDesc.usage = nri::TextureUsageBits::DEPTH_STENCIL_ATTACHMENT;
		DepthDesc.format = nri::Format::D32_SFLOAT;
		DepthDesc.width = static_cast<nri::Dim_t>(Viewport.DesiredWidth);
		DepthDesc.height = static_cast<nri::Dim_t>(Viewport.DesiredHeight);
		DepthDesc.optimizedClearValue.depthStencil = {1.0f, 0};
		if (!Check(CoreInterface.CreateCommittedTexture(*NriDevice,
				nri::MemoryLocation::DEVICE, 1.0f, DepthDesc,
				Viewport.DepthTexture),
				"Failed to create an NRI editor viewport depth texture"))
		{
			DestroyViewport(Viewport);
			return false;
		}
		nri::TextureViewDesc DepthView = {};
		DepthView.texture = Viewport.DepthTexture;
		DepthView.type = nri::TextureView::DEPTH_STENCIL_ATTACHMENT;
		DepthView.format = DepthDesc.format;
		// ALL means every plane is writable. Supplying only DEPTH makes NRI
		// request a read-only stencil plane, which is invalid for D32_SFLOAT.
		DepthView.planes = nri::PlaneBits::ALL;
		if (!Check(CoreInterface.CreateTextureView(
				DepthView, Viewport.DepthAttachment),
				"Failed to create an NRI editor viewport depth attachment"))
		{
			DestroyViewport(Viewport);
			return false;
		}

		Viewport.Width = Viewport.DesiredWidth;
		Viewport.Height = Viewport.DesiredHeight;
		RegisteredUserTextures.Register(Viewport.ShaderResource);
		return true;
	}

	bool CreateScenePipeline()
	{
		TiramisuMaterialShaderCompiler Compiler;
		if (!Compiler.IsAvailable())
		{
			Diagnostic = "DXC is unavailable for the NRI editor scene pipeline";
			return false;
		}

		auto CompileStage = [&](const char* EntryPoint, const char* Profile)
		{
			FMaterialShaderCompileRequest Request;
			Request.Backend = Api == ETiramisuEditorGraphicsApi::D3D12
				? EMaterialShaderBackend::D3D12
				: EMaterialShaderBackend::Vulkan;
			Request.Source.assign(EditorViewportSceneShaderSource);
			Request.SourceName = "editor-viewport-scene.hlsl";
			Request.EntryPoint = EntryPoint;
			Request.TargetProfile = Profile;
			string_path ShaderRoot = {};
			FS.update_path(ShaderRoot, "$game_shaders$", "r5\\");
			Request.IncludeDirectories = {
				std::filesystem::path(ShaderRoot) / "common"};
			return Compiler.Compile(Request);
		};

		FMaterialShaderCompileResult Vertex = CompileStage("VSMain", "vs_6_6");
		FMaterialShaderCompileResult Pixel = CompileStage("PSMain", "ps_6_6");
		FMaterialShaderCompileResult DebugVertex =
			CompileStage("VSDebug", "vs_6_6");
		FMaterialShaderCompileResult DebugPixel =
			CompileStage("PSDebug", "ps_6_6");
		FMaterialShaderCompileResult OverlayVertex =
			CompileStage("VSOverlay", "vs_6_6");
		if (!Vertex.Succeeded() || !Pixel.Succeeded() ||
			!DebugVertex.Succeeded() || !DebugPixel.Succeeded() ||
			!OverlayVertex.Succeeded())
		{
			Diagnostic = "Failed to compile the NRI editor scene shader";
			return false;
		}

		nri::RootConstantDesc RootConstants = {};
		RootConstants.registerIndex = 0;
		RootConstants.size = sizeof(FEditorSceneDrawConstants);
		RootConstants.shaderStages = nri::StageBits::VERTEX_SHADER;
		nri::PipelineLayoutDesc Layout = {};
		Layout.rootRegisterSpace = 0;
		Layout.rootConstants = &RootConstants;
		Layout.rootConstantNum = 1;
		Layout.shaderStages = nri::StageBits::VERTEX_SHADER |
			nri::StageBits::FRAGMENT_SHADER;
		if (!Check(CoreInterface.CreatePipelineLayout(
				*NriDevice, Layout, ScenePipelineLayout),
				"Failed to create the NRI editor scene pipeline layout"))
		{
			return false;
		}

		const nri::VertexAttributeDesc Attributes[] = {
			{{"POSITION", 0}, {0}, offsetof(FEditorStaticMeshVertex, Position),
				nri::Format::RGB32_SFLOAT, 0},
			{{"NORMAL", 0}, {1}, offsetof(FEditorStaticMeshVertex, Normal),
				nri::Format::RGB32_SFLOAT, 0}};
		nri::VertexStreamDesc Stream = {};
		Stream.bindingSlot = 0;
		Stream.stride = sizeof(FEditorStaticMeshVertex);
		nri::VertexInputDesc VertexInput = {};
		VertexInput.attributes = Attributes;
		VertexInput.attributeNum = static_cast<u8>(std::size(Attributes));
		VertexInput.streams = &Stream;
		VertexInput.streamNum = 1;

		nri::InputAssemblyDesc InputAssembly = {};
		InputAssembly.topology = nri::Topology::TRIANGLE_LIST;
		nri::RasterizationDesc Rasterization = {};
		Rasterization.fillMode = nri::FillMode::SOLID;
		Rasterization.cullMode = nri::CullMode::NONE;
		nri::ColorAttachmentDesc Color = {};
		Color.format = nri::Format::RGBA8_UNORM;
		Color.colorWriteMask = nri::ColorWriteBits::RGBA;
		nri::OutputMergerDesc OutputMerger = {};
		OutputMerger.colors = &Color;
		OutputMerger.colorNum = 1;
		OutputMerger.depthStencilFormat = nri::Format::D32_SFLOAT;
		OutputMerger.depth = {nri::CompareOp::LESS, true, false};

		const nri::ShaderDesc Shaders[] = {
			{nri::StageBits::VERTEX_SHADER, Vertex.Bytecode.data(),
				Vertex.Bytecode.size(), "VSMain"},
			{nri::StageBits::FRAGMENT_SHADER, Pixel.Bytecode.data(),
				Pixel.Bytecode.size(), "PSMain"}};
		nri::GraphicsPipelineDesc Pipeline = {};
		Pipeline.pipelineLayout = ScenePipelineLayout;
		Pipeline.vertexInput = &VertexInput;
		Pipeline.inputAssembly = InputAssembly;
		Pipeline.rasterization = Rasterization;
		Pipeline.outputMerger = OutputMerger;
		Pipeline.shaders = Shaders;
		Pipeline.shaderNum = static_cast<u32>(std::size(Shaders));
		if (!Check(CoreInterface.CreateGraphicsPipeline(
				*NriDevice, Pipeline, ScenePipeline),
				"Failed to create the NRI editor scene pipeline"))
		{
			CoreInterface.DestroyPipelineLayout(ScenePipelineLayout);
			ScenePipelineLayout = nullptr;
			return false;
		}

		Rasterization.fillMode = nri::FillMode::WIREFRAME;
		OutputMerger.depth = {nri::CompareOp::LESS_EQUAL, false, false};
		Pipeline.rasterization = Rasterization;
		Pipeline.outputMerger = OutputMerger;
		if (!Check(CoreInterface.CreateGraphicsPipeline(
				*NriDevice, Pipeline, SceneSelectionPipeline),
				"Failed to create the NRI editor selection pipeline"))
		{
			CoreInterface.DestroyPipeline(ScenePipeline);
			CoreInterface.DestroyPipelineLayout(ScenePipelineLayout);
			ScenePipeline = nullptr;
			ScenePipelineLayout = nullptr;
			return false;
		}

		const nri::VertexAttributeDesc DebugAttributes[] = {
			{{"POSITION", 0}, {0}, offsetof(FEditorDebugVertex, Position),
				nri::Format::RGB32_SFLOAT, 0},
			{{"COLOR", 0}, {1}, offsetof(FEditorDebugVertex, Color),
				nri::Format::RGBA32_SFLOAT, 0}};
		nri::VertexStreamDesc DebugStream = {};
		DebugStream.bindingSlot = 0;
		DebugStream.stride = sizeof(FEditorDebugVertex);
		nri::VertexInputDesc DebugVertexInput = {};
		DebugVertexInput.attributes = DebugAttributes;
		DebugVertexInput.attributeNum =
			static_cast<u8>(std::size(DebugAttributes));
		DebugVertexInput.streams = &DebugStream;
		DebugVertexInput.streamNum = 1;
		const nri::ShaderDesc DebugShaders[] = {
			{nri::StageBits::VERTEX_SHADER, DebugVertex.Bytecode.data(),
				DebugVertex.Bytecode.size(), "VSDebug"},
			{nri::StageBits::FRAGMENT_SHADER, DebugPixel.Bytecode.data(),
				DebugPixel.Bytecode.size(), "PSDebug"}};
		Color.blendEnabled = true;
		Color.colorBlend = {nri::BlendFactor::SRC_ALPHA,
			nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendOp::ADD};
		Color.alphaBlend = {nri::BlendFactor::ONE,
			nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendOp::ADD};
		OutputMerger.depth = {nri::CompareOp::LESS_EQUAL, false, false};
		Rasterization.fillMode = nri::FillMode::SOLID;
		Pipeline.vertexInput = &DebugVertexInput;
		Pipeline.rasterization = Rasterization;
		Pipeline.outputMerger = OutputMerger;
		Pipeline.shaders = DebugShaders;
		Pipeline.shaderNum = static_cast<u32>(std::size(DebugShaders));
		InputAssembly.topology = nri::Topology::LINE_LIST;
		Pipeline.inputAssembly = InputAssembly;
		if (!Check(CoreInterface.CreateGraphicsPipeline(
				*NriDevice, Pipeline, SceneDebugLinePipeline),
				"Failed to create the NRI editor debug-line pipeline"))
		{
			CoreInterface.DestroyPipeline(SceneSelectionPipeline);
			CoreInterface.DestroyPipeline(ScenePipeline);
			CoreInterface.DestroyPipelineLayout(ScenePipelineLayout);
			SceneSelectionPipeline = nullptr;
			ScenePipeline = nullptr;
			ScenePipelineLayout = nullptr;
			return false;
		}
		InputAssembly.topology = nri::Topology::TRIANGLE_LIST;
		Pipeline.inputAssembly = InputAssembly;
		if (!Check(CoreInterface.CreateGraphicsPipeline(
				*NriDevice, Pipeline, SceneDebugTrianglePipeline),
				"Failed to create the NRI editor debug-triangle pipeline"))
		{
			CoreInterface.DestroyPipeline(SceneDebugLinePipeline);
			CoreInterface.DestroyPipeline(SceneSelectionPipeline);
			CoreInterface.DestroyPipeline(ScenePipeline);
			CoreInterface.DestroyPipelineLayout(ScenePipelineLayout);
			SceneDebugLinePipeline = nullptr;
			SceneSelectionPipeline = nullptr;
			ScenePipeline = nullptr;
			ScenePipelineLayout = nullptr;
			return false;
		}

		const nri::ShaderDesc OverlayShaders[] = {
			{nri::StageBits::VERTEX_SHADER, OverlayVertex.Bytecode.data(),
				OverlayVertex.Bytecode.size(), "VSOverlay"},
			{nri::StageBits::FRAGMENT_SHADER, DebugPixel.Bytecode.data(),
				DebugPixel.Bytecode.size(), "PSDebug"}};
		OutputMerger.depth = {nri::CompareOp::ALWAYS, false, false};
		Pipeline.outputMerger = OutputMerger;
		Pipeline.shaders = OverlayShaders;
		InputAssembly.topology = nri::Topology::LINE_LIST;
		Pipeline.inputAssembly = InputAssembly;
		if (!Check(CoreInterface.CreateGraphicsPipeline(
				*NriDevice, Pipeline, SceneOverlayLinePipeline),
				"Failed to create the NRI editor overlay-line pipeline"))
		{
			CoreInterface.DestroyPipeline(SceneDebugTrianglePipeline);
			CoreInterface.DestroyPipeline(SceneDebugLinePipeline);
			CoreInterface.DestroyPipeline(SceneSelectionPipeline);
			CoreInterface.DestroyPipeline(ScenePipeline);
			CoreInterface.DestroyPipelineLayout(ScenePipelineLayout);
			SceneDebugTrianglePipeline = nullptr;
			SceneDebugLinePipeline = nullptr;
			SceneSelectionPipeline = nullptr;
			ScenePipeline = nullptr;
			ScenePipelineLayout = nullptr;
			return false;
		}
		InputAssembly.topology = nri::Topology::TRIANGLE_LIST;
		Pipeline.inputAssembly = InputAssembly;
		if (!Check(CoreInterface.CreateGraphicsPipeline(
				*NriDevice, Pipeline, SceneOverlayTrianglePipeline),
				"Failed to create the NRI editor overlay-triangle pipeline"))
		{
			CoreInterface.DestroyPipeline(SceneOverlayLinePipeline);
			CoreInterface.DestroyPipeline(SceneDebugTrianglePipeline);
			CoreInterface.DestroyPipeline(SceneDebugLinePipeline);
			CoreInterface.DestroyPipeline(SceneSelectionPipeline);
			CoreInterface.DestroyPipeline(ScenePipeline);
			CoreInterface.DestroyPipelineLayout(ScenePipelineLayout);
			SceneOverlayLinePipeline = nullptr;
			SceneDebugTrianglePipeline = nullptr;
			SceneDebugLinePipeline = nullptr;
			SceneSelectionPipeline = nullptr;
			ScenePipeline = nullptr;
			ScenePipelineLayout = nullptr;
			return false;
		}
		return true;
	}

	[[nodiscard]] FEditorOwnedStaticMeshUpload BuildPreviewMesh(
		const FMaterialPreview& Preview) const
	{
		FEditorOwnedStaticMeshUpload Mesh;
		Mesh.MeshId = Preview.MeshId;
		Mesh.Revision = static_cast<u64>(Preview.Primitive) + 1;
		auto AddVertex = [&](const xr_array<float, 3>& Position, const xr_array<float, 3>& Normal, const xr_array<float, 2>& TexCoord)
		{
			FEditorStaticMeshVertex Vertex;
			Vertex.Position = Position;
			Vertex.Normal = Normal;
			Vertex.TexCoord = TexCoord;
			Mesh.Vertices.push_back(Vertex);
		};

		if (Preview.Primitive == EMaterialPreviewPrimitive::Plane)
		{
			AddVertex({-1.0f, -1.0f, 0.0f}, {0.0f, 0.0f, -1.0f}, {0.0f, 1.0f});
			AddVertex({-1.0f, 1.0f, 0.0f}, {0.0f, 0.0f, -1.0f}, {0.0f, 0.0f});
			AddVertex({1.0f, 1.0f, 0.0f}, {0.0f, 0.0f, -1.0f}, {1.0f, 0.0f});
			AddVertex({1.0f, -1.0f, 0.0f}, {0.0f, 0.0f, -1.0f}, {1.0f, 1.0f});
			Mesh.Indices = {0, 1, 2, 0, 2, 3};
		}
		else if (Preview.Primitive == EMaterialPreviewPrimitive::Cube)
		{
			const xr_array<xr_array<float, 3>, 6> Normals = {{
				{0.0f, 0.0f, -1.0f}, {0.0f, 0.0f, 1.0f},
				{-1.0f, 0.0f, 0.0f}, {1.0f, 0.0f, 0.0f},
				{0.0f, -1.0f, 0.0f}, {0.0f, 1.0f, 0.0f}}};
			const xr_array<xr_array<xr_array<float, 3>, 4>, 6> Faces = {{
				{{{-0.8f, -0.8f, -0.8f}, {-0.8f, 0.8f, -0.8f}, {0.8f, 0.8f, -0.8f}, {0.8f, -0.8f, -0.8f}}},
				{{{0.8f, -0.8f, 0.8f}, {0.8f, 0.8f, 0.8f}, {-0.8f, 0.8f, 0.8f}, {-0.8f, -0.8f, 0.8f}}},
				{{{-0.8f, -0.8f, 0.8f}, {-0.8f, 0.8f, 0.8f}, {-0.8f, 0.8f, -0.8f}, {-0.8f, -0.8f, -0.8f}}},
				{{{0.8f, -0.8f, -0.8f}, {0.8f, 0.8f, -0.8f}, {0.8f, 0.8f, 0.8f}, {0.8f, -0.8f, 0.8f}}},
				{{{-0.8f, -0.8f, 0.8f}, {-0.8f, -0.8f, -0.8f}, {0.8f, -0.8f, -0.8f}, {0.8f, -0.8f, 0.8f}}},
				{{{-0.8f, 0.8f, -0.8f}, {-0.8f, 0.8f, 0.8f}, {0.8f, 0.8f, 0.8f}, {0.8f, 0.8f, -0.8f}}}}};
			const xr_array<xr_array<float, 2>, 4> Uvs = {{
				{0.0f, 1.0f}, {0.0f, 0.0f}, {1.0f, 0.0f}, {1.0f, 1.0f}}};
			for (u32 Face = 0; Face < Faces.size(); ++Face)
			{
				const u32 Base = static_cast<u32>(Mesh.Vertices.size());
				for (u32 Corner = 0; Corner < 4; ++Corner)
					AddVertex(Faces[Face][Corner], Normals[Face], Uvs[Corner]);
				Mesh.Indices.insert(Mesh.Indices.end(),
					{Base, Base + 1, Base + 2, Base, Base + 2, Base + 3});
			}
		}
		else
		{
			constexpr u32 Segments = 32;
			constexpr u32 Rings = 16;
			for (u32 Ring = 0; Ring <= Rings; ++Ring)
			{
				const float V = static_cast<float>(Ring) / Rings;
				const float Theta = V * PI;
				const float Y = std::cos(Theta);
				const float Radius = std::sin(Theta);
				for (u32 Segment = 0; Segment <= Segments; ++Segment)
				{
					const float U = static_cast<float>(Segment) / Segments;
					const float Phi = U * PI_MUL_2;
					const xr_array<float, 3> Normal = {
						Radius * std::cos(Phi), Y, Radius * std::sin(Phi)};
					AddVertex(Normal, Normal, {U, V});
				}
			}
			for (u32 Ring = 0; Ring < Rings; ++Ring)
			{
				for (u32 Segment = 0; Segment < Segments; ++Segment)
				{
					const u32 A = Ring * (Segments + 1) + Segment;
					const u32 B = A + Segments + 1;
					Mesh.Indices.insert(Mesh.Indices.end(),
						{A, B, A + 1, A + 1, B, B + 1});
				}
			}
		}
		Mesh.Sections.push_back({0,
			static_cast<u32>(Mesh.Indices.size()), {1}});
		return Mesh;
	}

	[[nodiscard]] bool RebuildPreviewMesh(FMaterialPreview& Preview)
	{
		FEditorOwnedStaticMeshUpload Upload = BuildPreviewMesh(Preview);
		FGpuMesh Replacement;
		if (!CreateGpuMesh(Upload, Replacement))
			return false;
		const auto Existing = GpuMeshes.find(Preview.MeshId.Value);
		if (Existing != GpuMeshes.end())
		{
			QueueBufferDeletion(Existing->second.Buffer);
			Existing->second = Replacement;
		}
		else
		{
			GpuMeshes.emplace(Preview.MeshId.Value, Replacement);
		}
		return true;
	}

	[[nodiscard]] nri::Pipeline* CreateEditorMaterialPipeline(
		const Tiramisu::Editor::FMaterialPreviewCompileResult& Compiled,
		const bool TwoSided)
	{
		const nri::VertexAttributeDesc Attributes[] = {
			{{"POSITION", 0}, {0}, offsetof(FEditorStaticMeshVertex, Position),
				nri::Format::RGB32_SFLOAT, 0},
			{{"NORMAL", 0}, {1}, offsetof(FEditorStaticMeshVertex, Normal),
				nri::Format::RGB32_SFLOAT, 0},
			{{"TANGENT", 0}, {2}, offsetof(FEditorStaticMeshVertex, Tangent),
				nri::Format::RGBA32_SFLOAT, 0},
			{{"TEXCOORD", 0}, {3}, offsetof(FEditorStaticMeshVertex, TexCoord),
				nri::Format::RG32_SFLOAT, 0},
			{{"TEXCOORD", 1}, {4}, offsetof(FEditorStaticMeshVertex, TexCoord1),
				nri::Format::RG32_SFLOAT, 0},
			{{"COLOR", 0}, {5}, offsetof(FEditorStaticMeshVertex, Color),
				nri::Format::RGBA8_UNORM, 0}};
		const nri::VertexAttributeDesc VulkanAttributes[] = {
			Attributes[0], Attributes[1], Attributes[3], Attributes[4], Attributes[5]};
		nri::VertexStreamDesc Stream = {};
		Stream.bindingSlot = 0;
		Stream.stride = sizeof(FEditorStaticMeshVertex);
		nri::VertexInputDesc VertexInput = {};
		VertexInput.attributes = Api == ETiramisuEditorGraphicsApi::Vulkan
			? VulkanAttributes : Attributes;
		VertexInput.attributeNum = static_cast<u8>(
			Api == ETiramisuEditorGraphicsApi::Vulkan
				? std::size(VulkanAttributes) : std::size(Attributes));
		VertexInput.streams = &Stream;
		VertexInput.streamNum = 1;
		nri::InputAssemblyDesc InputAssembly = {};
		InputAssembly.topology = nri::Topology::TRIANGLE_LIST;
		nri::RasterizationDesc Rasterization = {};
		Rasterization.fillMode = nri::FillMode::SOLID;
		Rasterization.cullMode = TwoSided
			? nri::CullMode::NONE : nri::CullMode::BACK;
		nri::ColorAttachmentDesc Color = {};
		Color.format = nri::Format::RGBA8_UNORM;
		Color.colorWriteMask = nri::ColorWriteBits::RGBA;
		if (Compiled.ResolvedMaterial.BlendMode ==
			EMaterialBlendMode::Translucent)
		{
			Color.blendEnabled = true;
			Color.colorBlend = {nri::BlendFactor::SRC_ALPHA,
				nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendOp::ADD};
			Color.alphaBlend = {nri::BlendFactor::ONE,
				nri::BlendFactor::ONE_MINUS_SRC_ALPHA, nri::BlendOp::ADD};
		}
		else if (Compiled.ResolvedMaterial.BlendMode ==
			EMaterialBlendMode::Additive)
		{
			Color.blendEnabled = true;
			Color.colorBlend = {nri::BlendFactor::SRC_ALPHA,
				nri::BlendFactor::ONE, nri::BlendOp::ADD};
			Color.alphaBlend = {nri::BlendFactor::ONE,
				nri::BlendFactor::ONE, nri::BlendOp::ADD};
		}
		else if (Compiled.ResolvedMaterial.BlendMode ==
			EMaterialBlendMode::Modulate)
		{
			Color.blendEnabled = true;
			Color.colorBlend = {nri::BlendFactor::DST_COLOR,
				nri::BlendFactor::ZERO, nri::BlendOp::ADD};
			Color.alphaBlend = {nri::BlendFactor::ONE,
				nri::BlendFactor::ZERO, nri::BlendOp::ADD};
		}
		nri::OutputMergerDesc Output = {};
		Output.colors = &Color;
		Output.colorNum = 1;
		Output.depthStencilFormat = nri::Format::D32_SFLOAT;
		const bool WritesDepth = Compiled.ResolvedMaterial.BlendMode ==
			EMaterialBlendMode::Opaque ||
			Compiled.ResolvedMaterial.BlendMode == EMaterialBlendMode::Masked;
		Output.depth = {nri::CompareOp::LESS, WritesDepth, false};
		const nri::ShaderDesc Shaders[] = {
			{nri::StageBits::VERTEX_SHADER, Compiled.VertexBytecode.data(),
				Compiled.VertexBytecode.size(), "Main"},
			{nri::StageBits::FRAGMENT_SHADER, Compiled.PixelBytecode.data(),
				Compiled.PixelBytecode.size(), "Main"}};
		nri::GraphicsPipelineDesc Pipeline = {};
		Pipeline.pipelineLayout = PreviewPipelineLayout;
		Pipeline.vertexInput = &VertexInput;
		Pipeline.inputAssembly = InputAssembly;
		Pipeline.rasterization = Rasterization;
		Pipeline.outputMerger = Output;
		Pipeline.shaders = Shaders;
		Pipeline.shaderNum = static_cast<u32>(std::size(Shaders));
		nri::Pipeline* Result = nullptr;
		if (!Check(CoreInterface.CreateGraphicsPipeline(
				*NriDevice, Pipeline, Result),
				"Failed to create the NRI editor material pipeline"))
		{
			return nullptr;
		}
		return Result;
	}

	[[nodiscard]] xr_string FormatMaterialDiagnostics(
		const xr_vector<FMaterialDiagnostic>& Diagnostics) const
	{
		xr_string Text;
		for (const auto& Item : Diagnostics)
		{
			if (!Text.empty()) Text += '\n';
			Text += '[' + Item.Code + "] " + Item.Message;
		}
		return Text;
	}

	void InstallMaterialPreviewResult(FMaterialPreview& Preview,
		const u32 PreviewIndex,
		Tiramisu::Editor::FMaterialPreviewCompileResult Compiled,
		const u64 Revision)
	{
		if (!Compiled.Succeeded())
		{
			Preview.State = EMaterialPreviewState::Error;
			Preview.Diagnostic = FormatMaterialDiagnostics(Compiled.Diagnostics);
			return;
		}
		if (Compiled.ParameterBlock.Data.size() > PreviewParameterStride)
		{
			Preview.State = EMaterialPreviewState::Error;
			Preview.Diagnostic = "Material preview parameter block exceeds the per-preview capacity";
			return;
		}

		xr_vector<xr_string> ResourceWarnings;
		FMaterialParameterPackResult Patched = PatchMaterialParameterResources(
			Compiled.ParameterBlock,
			[this, &ResourceWarnings](
				const FMaterialParameterResourceReference& Reference)
				-> xr_optional<FDescriptorHeapIndex>
			{
				if (Reference.Type == EMaterialParameterType::SamplerPreset)
					return FDescriptorHeapIndex{PreviewDefaultSamplerIndex};
				if (Reference.Type == EMaterialParameterType::Texture2D ||
					Reference.Type == EMaterialParameterType::TextureCube)
				{
					xr_string Warning;
					const u32 Index = ResolveMaterialPreviewTexture(
						Reference.AssetPath,
						Reference.Type == EMaterialParameterType::TextureCube,
						Warning);
					if (!Warning.empty())
						ResourceWarnings.push_back(std::move(Warning));
					return FDescriptorHeapIndex{Index};
				}
				return std::nullopt;
			});
		if (!Patched.Succeeded())
		{
			Preview.State = EMaterialPreviewState::Error;
			Preview.Diagnostic = FormatMaterialDiagnostics(Patched.Diagnostics);
			return;
		}
		xr_string EnvironmentWarning;
		const u32 EnvironmentDescriptor = ResolveMaterialPreviewTexture(
			Tiramisu::Editor::MaterialPreviewEnvironmentAsset(Preview.Environment),
			true, EnvironmentWarning);
		if (!EnvironmentWarning.empty())
			ResourceWarnings.push_back(std::move(EnvironmentWarning));

		nri::Pipeline* NewPipeline = CreateEditorMaterialPipeline(Compiled,
			Compiled.ResolvedMaterial.TwoSided);
		if (!NewPipeline)
		{
			Preview.State = EMaterialPreviewState::Error;
			Preview.Diagnostic = Diagnostic;
			return;
		}
		if (!GpuMeshes.contains(Preview.MeshId.Value) && !RebuildPreviewMesh(Preview))
		{
			CoreInterface.DestroyPipeline(NewPipeline);
			Preview.State = EMaterialPreviewState::Error;
			Preview.Diagnostic = "Failed to create the material preview primitive";
			return;
		}

		if (Preview.Pipeline)
			DeferredPipelines.push_back({Preview.Pipeline, FrameIndex});
		Preview.Pipeline = NewPipeline;
		Preview.PipelineKey = Compiled.PipelineKey;
		Preview.ParameterData = std::move(Patched.Value.Data);
		Preview.ParameterLayoutHash = Patched.Value.LayoutHash;
		Preview.DrawFlags = EnvironmentDescriptor;
		Preview.AcceptedRevision = Revision;
		Preview.State = EMaterialPreviewState::Ready;
		Preview.Diagnostic.clear();
		for (const xr_string& Warning : ResourceWarnings)
		{
			if (!Preview.Diagnostic.empty())
				Preview.Diagnostic += '\n';
			Preview.Diagnostic += "[preview.resource_warning] " + Warning;
			Msg("! Material preview resource warning: %s", Warning.c_str());
		}
	}

	void PollMaterialPreviewCompiles()
	{
		for (auto It = MaterialPreviewCompileJobs.begin();
			It != MaterialPreviewCompileJobs.end();)
		{
			if (It->Future.wait_for(std::chrono::seconds(0)) !=
				std::future_status::ready)
			{
				++It;
				continue;
			}
			Tiramisu::Editor::FMaterialPreviewCompileResult Result =
				It->Future.get();
			FMaterialPreview* Preview = FindPreview(It->Handle);
			if (Preview && It->Revision == Preview->RequestedRevision)
				InstallMaterialPreviewResult(*Preview, It->Handle.Index,
					std::move(Result), It->Revision);
			It = MaterialPreviewCompileJobs.erase(It);
		}
	}

	[[nodiscard]] nri::Pipeline* AcquireSceneMaterialPipeline(
		const Tiramisu::Editor::FMaterialPreviewCompileResult& Compiled,
		const bool TwoSided)
	{
		const FScenePipelineCacheKey Key = {
			Compiled.PipelineKey, TwoSided};
		if (const auto Existing = ScenePipelineCache.find(Key);
			Existing != ScenePipelineCache.end())
		{
			++Existing->second.ReferenceCount;
			return Existing->second.Pipeline;
		}

		nri::Pipeline* Pipeline =
			CreateEditorMaterialPipeline(Compiled, TwoSided);
		if (!Pipeline)
			return nullptr;
		ScenePipelineCache.emplace(Key,
			FScenePipelineCacheEntry{Pipeline, 1});
		return Pipeline;
	}

	void ReleaseSceneMaterialPipeline(FSceneMaterial& Material)
	{
		if (!Material.Pipeline || Material.PipelineKey == 0)
			return;
		const FScenePipelineCacheKey Key = {
			Material.PipelineKey, Material.PipelineTwoSided};
		const auto Existing = ScenePipelineCache.find(Key);
		if (Existing == ScenePipelineCache.end())
		{
			Material.Pipeline = nullptr;
			Material.PipelineKey = 0;
			Material.PipelineTwoSided = false;
			return;
		}
		if (Existing->second.ReferenceCount > 1)
			--Existing->second.ReferenceCount;
		else
		{
			DeferredPipelines.push_back(
				{Existing->second.Pipeline, FrameIndex});
			ScenePipelineCache.erase(Existing);
		}
		Material.Pipeline = nullptr;
		Material.PipelineKey = 0;
		Material.PipelineTwoSided = false;
	}

	void InstallSceneMaterialResult(const u64 MaterialSlot,
		Tiramisu::Editor::FMaterialPreviewCompileResult Compiled,
		const u64 Revision, const bool TwoSided, const bool Reload)
	{
		const auto Found = SceneMaterials.find(MaterialSlot);
		if (Found == SceneMaterials.end() ||
			Found->second.RequestedRevision != Revision)
		{
			return;
		}
		FSceneMaterial& Material = Found->second;
		if (!Compiled.Succeeded())
		{
			// Safe last-good behavior: diagnostics change, the active pipeline and
			// parameter data remain untouched.
			Material.Diagnostic = FormatMaterialDiagnostics(Compiled.Diagnostics);
			return;
		}
		if (Compiled.ParameterBlock.Data.size() > PreviewParameterStride)
		{
			Material.Diagnostic =
				"Editor material parameter block exceeds its fixed stride";
			return;
		}
		xr_vector<xr_string> ResourceWarnings;
		FMaterialParameterPackResult Patched = PatchMaterialParameterResources(
			Compiled.ParameterBlock,
			[this, &ResourceWarnings](
				const FMaterialParameterResourceReference& Reference)
				-> xr_optional<FDescriptorHeapIndex>
			{
				if (Reference.Type == EMaterialParameterType::SamplerPreset)
					return FDescriptorHeapIndex{PreviewDefaultSamplerIndex};
				if (Reference.Type == EMaterialParameterType::Texture2D ||
					Reference.Type == EMaterialParameterType::TextureCube)
				{
					xr_string Warning;
					const u32 Index = ResolveMaterialPreviewTexture(
						Reference.AssetPath,
						Reference.Type == EMaterialParameterType::TextureCube,
						Warning);
					if (!Warning.empty())
						ResourceWarnings.push_back(std::move(Warning));
					return FDescriptorHeapIndex{Index};
				}
				return std::nullopt;
			});
		if (!Patched.Succeeded())
		{
			Material.Diagnostic = FormatMaterialDiagnostics(Patched.Diagnostics);
			return;
		}

		const bool PipelineTwoSided =
			TwoSided || Compiled.ResolvedMaterial.TwoSided;
		const bool ReusesActivePipeline = Material.Pipeline &&
			Material.PipelineKey == Compiled.PipelineKey &&
			Material.PipelineTwoSided == PipelineTwoSided;
		nri::Pipeline* NewPipeline = Material.Pipeline;
		if (!ReusesActivePipeline)
		{
			NewPipeline = AcquireSceneMaterialPipeline(
				Compiled, PipelineTwoSided);
			if (!NewPipeline)
			{
				Material.Diagnostic = Diagnostic;
				return;
			}
			ReleaseSceneMaterialPipeline(Material);
			Material.Pipeline = NewPipeline;
			Material.PipelineKey = Compiled.PipelineKey;
			Material.PipelineTwoSided = PipelineTwoSided;
		}
		Material.ParameterData = std::move(Patched.Value.Data);
		Material.ParameterLayoutHash = Patched.Value.LayoutHash;
		Material.AcceptedRevision = Revision;
		if (Reload)
			++Material.ReloadCount;
		Material.Diagnostic.clear();
		for (const xr_string& Warning : ResourceWarnings)
		{
			if (!Material.Diagnostic.empty())
				Material.Diagnostic += '\n';
			Material.Diagnostic += "[scene.resource_warning] " + Warning;
			Msg("! Editor scene material resource warning: %s", Warning.c_str());
		}
	}

	void PollSceneMaterialCompiles()
	{
		for (FSceneMaterialCompileResult& Result :
			SceneMaterialCompileQueue.PollReady())
		{
			InstallSceneMaterialResult(Result.MaterialSlot,
				std::move(Result.Compiled), Result.Revision,
				Result.TwoSided, Result.Reload);
		}
	}

	[[nodiscard]] bool UploadMaterialInstanceForFrame(
		const u32 FrameContext, const u32 LocalInstanceIndex,
		const xr_vector<u8>& Parameters,
		const u64 LayoutHash)
	{
		if (FrameContext >= QueuedFrameCount ||
			LocalInstanceIndex >= MaxEditorMaterialInstances ||
			Parameters.empty() || Parameters.size() > PreviewParameterStride)
		{
			return false;
		}
		const u32 AbsoluteInstanceIndex =
			FrameContext * MaxEditorMaterialInstances + LocalInstanceIndex;
		const u64 ParameterOffset =
			(u64(FrameContext) * MaxEditorMaterialInstances +
				LocalInstanceIndex) * PreviewParameterStride;
		if (!WritePreviewBuffer(PreviewMaterialParameterBuffer, ParameterOffset,
				Parameters.data(), Parameters.size()))
		{
			return false;
		}
		FMaterialInstanceGpuData Instance;
		Instance.ParameterDataOffset = static_cast<u32>(ParameterOffset);
		Instance.ParameterDataSize = static_cast<u32>(Parameters.size());
		Instance.LayoutHashLow = static_cast<u32>(LayoutHash);
		Instance.LayoutHashHigh = static_cast<u32>(LayoutHash >> 32u);
		return WritePreviewBuffer(PreviewMaterialInstanceBuffer,
			u64(AbsoluteInstanceIndex) * sizeof(Instance),
			&Instance, sizeof(Instance));
	}

	void UploadMaterialFrameData(const u32 FrameContext)
	{
		for (u32 Index = 0; Index < MaterialPreviews.size(); ++Index)
		{
			const FMaterialPreview& Preview = MaterialPreviews[Index];
			if (!Preview.Alive || !Preview.Pipeline || Preview.ParameterData.empty())
				continue;
			if (!UploadMaterialInstanceForFrame(FrameContext, Index,
					Preview.ParameterData, Preview.ParameterLayoutHash))
			{
				continue;
			}
			FMaterialDrawGpuData Draw;
			Draw.LocalToWorld = {1.0f, 0.0f, 0.0f, 0.0f,
				0.0f, 1.0f, 0.0f, 0.0f,
				0.0f, 0.0f, 1.0f, 0.0f,
				0.0f, 0.0f, 0.0f, 1.0f};
			Draw.PreviousLocalToWorld = Draw.LocalToWorld;
			Draw.MaterialInstanceIndex =
				FrameContext * MaxEditorMaterialInstances + Index;
			Draw.ObjectId = Index + 1;
			Draw.Flags = Preview.DrawFlags;
			const u32 AbsoluteDrawIndex =
				FrameContext * MaxEditorDrawRecords + Index;
			(void)WritePreviewBuffer(PreviewDrawDataBuffer,
				u64(AbsoluteDrawIndex) * sizeof(Draw),
				&Draw, sizeof(Draw));
		}
		for (const auto& [Slot, Material] : SceneMaterials)
		{
			(void)Slot;
			if (!Material.Pipeline || Material.ParameterData.empty())
				continue;
			(void)UploadMaterialInstanceForFrame(FrameContext,
				Material.InstanceIndex, Material.ParameterData,
				Material.ParameterLayoutHash);
		}
	}

	void QueueBufferDeletion(nri::Buffer* Buffer)
	{
		if (Buffer)
			DeferredBuffers.push_back({Buffer, FrameIndex + 1});
	}

	void CollectDeferredBuffers(const u64 CompletedFence)
	{
		std::erase_if(DeferredBuffers, [&](const FDeferredBuffer& Item)
		{
			if (Item.RetireFence > CompletedFence)
				return false;
			CoreInterface.DestroyBuffer(Item.Buffer);
			return true;
		});
	}

	void CollectDeferredPipelines(const u64 CompletedFence)
	{
		std::erase_if(DeferredPipelines, [&](const FDeferredPipeline& Item)
		{
			if (Item.RetireFence > CompletedFence)
				return false;
			CoreInterface.DestroyPipeline(Item.Pipeline);
			return true;
		});
	}

	[[nodiscard]] bool CreateGpuMesh(
		const FEditorOwnedStaticMeshUpload& Upload, FGpuMesh& OutMesh)
	{
		constexpr u64 Alignment = 16;
		const u64 IndexBytes = Upload.Indices.size() * sizeof(u32);
		const u64 VertexBytes = Upload.Vertices.size() *
			sizeof(FEditorStaticMeshVertex);
		const u64 VertexOffset =
			(IndexBytes + Alignment - 1) & ~(Alignment - 1);
		nri::BufferDesc Desc = {};
		Desc.size = VertexOffset + VertexBytes;
		Desc.usage = nri::BufferUsageBits::VERTEX_BUFFER |
			nri::BufferUsageBits::INDEX_BUFFER;
		if (!Check(CoreInterface.CreateCommittedBuffer(*NriDevice,
				nri::MemoryLocation::HOST_UPLOAD, 0.5f, Desc, OutMesh.Buffer),
				"Failed to create an NRI editor static-mesh buffer"))
		{
			return false;
		}
		void* Destination = CoreInterface.MapBuffer(*OutMesh.Buffer, 0, Desc.size);
		if (!Destination)
		{
			Diagnostic = "Failed to map an NRI editor static-mesh buffer";
			CoreInterface.DestroyBuffer(OutMesh.Buffer);
			OutMesh.Buffer = nullptr;
			return false;
		}
		std::memcpy(Destination, Upload.Indices.data(), IndexBytes);
		std::memcpy(static_cast<u8*>(Destination) + VertexOffset,
			Upload.Vertices.data(), VertexBytes);
		CoreInterface.UnmapBuffer(*OutMesh.Buffer);
		OutMesh.VertexOffset = VertexOffset;
		OutMesh.IndexCount = static_cast<u32>(Upload.Indices.size());
		OutMesh.ByteSize = Desc.size;
		OutMesh.Sections = Upload.Sections;
		OutMesh.NeedsBarrier = true;
		return true;
	}

	[[nodiscard]] bool UpdateViewportDebugDraw(FViewport& Viewport,
		const FEditorOwnedViewportScenePacket& Packet)
	{
		if (Viewport.DebugDrawRevision == Packet.DebugDrawRevision)
			return true;
		const u32 LineVertexCount = static_cast<u32>(
			Packet.DebugLines.size() * 2);
		const u32 TriangleVertexCount = static_cast<u32>(
			Packet.DebugTriangles.size() * 3);
		const u32 OverlayLineVertexCount =
			static_cast<u32>(Packet.OverlayLines.size() * 2);
		const u32 OverlayTriangleVertexCount =
			static_cast<u32>(Packet.OverlayTriangles.size() * 3);
		if (LineVertexCount == 0 && TriangleVertexCount == 0 &&
			OverlayLineVertexCount == 0 && OverlayTriangleVertexCount == 0)
		{
			QueueBufferDeletion(Viewport.DebugDrawBuffer);
			Viewport.DebugDrawBuffer = nullptr;
			Viewport.DebugDrawBufferBytes = 0;
			Viewport.DebugDrawRevision = Packet.DebugDrawRevision;
			Viewport.DebugTriangleOffset = 0;
			Viewport.OverlayLineOffset = 0;
			Viewport.OverlayTriangleOffset = 0;
			Viewport.DebugLineVertexCount = 0;
			Viewport.DebugTriangleVertexCount = 0;
			Viewport.OverlayLineVertexCount = 0;
			Viewport.OverlayTriangleVertexCount = 0;
			Viewport.DebugDrawNeedsBarrier = false;
			return true;
		}

		xr_vector<FEditorDebugVertex> Vertices;
		Vertices.reserve(static_cast<size_t>(LineVertexCount) +
			TriangleVertexCount + OverlayLineVertexCount +
			OverlayTriangleVertexCount);
		for (const FEditorDebugLine& Line : Packet.DebugLines)
			Vertices.insert(Vertices.end(), Line.Vertices.begin(), Line.Vertices.end());
		const u64 TriangleOffset =
			Vertices.size() * sizeof(FEditorDebugVertex);
		for (const FEditorDebugTriangle& Triangle : Packet.DebugTriangles)
			Vertices.insert(Vertices.end(), Triangle.Vertices.begin(),
				Triangle.Vertices.end());
		const u64 OverlayLineOffset =
			Vertices.size() * sizeof(FEditorDebugVertex);
		auto AppendOverlayVertex = [&](const FEditorOverlayVertex& Source)
		{
			FEditorDebugVertex Destination;
			Destination.Position = Source.Position;
			Destination.Color = Source.Color;
			Vertices.push_back(Destination);
		};
		for (const FEditorOverlayLine& Line : Packet.OverlayLines)
			for (const FEditorOverlayVertex& Vertex : Line.Vertices)
				AppendOverlayVertex(Vertex);
		const u64 OverlayTriangleOffset =
			Vertices.size() * sizeof(FEditorDebugVertex);
		for (const FEditorOverlayTriangle& Triangle : Packet.OverlayTriangles)
			for (const FEditorOverlayVertex& Vertex : Triangle.Vertices)
				AppendOverlayVertex(Vertex);

		nri::BufferDesc Desc = {};
		Desc.size = Vertices.size() * sizeof(FEditorDebugVertex);
		Desc.usage = nri::BufferUsageBits::VERTEX_BUFFER;
		nri::Buffer* Replacement = nullptr;
		if (!Check(CoreInterface.CreateCommittedBuffer(*NriDevice,
				nri::MemoryLocation::HOST_UPLOAD, 0.5f, Desc, Replacement),
				"Failed to create the NRI editor debug-draw buffer"))
		{
			return false;
		}
		void* Destination = CoreInterface.MapBuffer(*Replacement, 0, Desc.size);
		if (!Destination)
		{
			Diagnostic = "Failed to map the NRI editor debug-draw buffer";
			CoreInterface.DestroyBuffer(Replacement);
			return false;
		}
		std::memcpy(Destination, Vertices.data(), Desc.size);
		CoreInterface.UnmapBuffer(*Replacement);

		QueueBufferDeletion(Viewport.DebugDrawBuffer);
		Viewport.DebugDrawBuffer = Replacement;
		Viewport.DebugDrawBufferBytes = Desc.size;
		Viewport.DebugDrawRevision = Packet.DebugDrawRevision;
		Viewport.DebugTriangleOffset = TriangleOffset;
		Viewport.OverlayLineOffset = OverlayLineOffset;
		Viewport.OverlayTriangleOffset = OverlayTriangleOffset;
		Viewport.DebugLineVertexCount = LineVertexCount;
		Viewport.DebugTriangleVertexCount = TriangleVertexCount;
		Viewport.OverlayLineVertexCount = OverlayLineVertexCount;
		Viewport.OverlayTriangleVertexCount = OverlayTriangleVertexCount;
		Viewport.DebugDrawNeedsBarrier = true;
		return true;
	}

	void ApplyScenePacket(FViewport& Viewport,
		const FEditorOwnedViewportScenePacket& Packet)
	{
		for (const FEditorOwnedMaterialSlotSource& Material : Packet.MaterialSlots)
			QueueSceneMaterialCompile(Material);
		for (const FEditorStaticMeshId Removed : Packet.RemovedStaticMeshes)
		{
			const auto Existing = GpuMeshes.find(Removed.Value);
			if (Existing == GpuMeshes.end())
				continue;
			QueueBufferDeletion(Existing->second.Buffer);
			GpuMeshes.erase(Existing);
		}
		for (const FEditorOwnedStaticMeshUpload& Upload : Packet.StaticMeshUpdates)
		{
			FGpuMesh Replacement;
			if (!CreateGpuMesh(Upload, Replacement))
				continue;
			const auto Existing = GpuMeshes.find(Upload.MeshId.Value);
			if (Existing != GpuMeshes.end())
			{
				QueueBufferDeletion(Existing->second.Buffer);
				Existing->second = Replacement;
			}
			else
			{
				GpuMeshes.emplace(Upload.MeshId.Value, Replacement);
			}
		}
		(void)UpdateViewportDebugDraw(Viewport, Packet);
		if (!Packet.Instances.empty())
			(void)EnsureSceneViewportMaterialContext(Viewport);
	}

	[[nodiscard]] bool UploadSceneViewportFrameData(FViewport& Viewport,
		const u32 FrameContext)
	{
		if (Viewport.ScenePacket.Instances.empty())
		{
			Viewport.SceneDrawCount = 0;
			Viewport.SceneSelectionDrawCount = 0;
			return true;
		}
		if (!EnsureSceneViewportMaterialContext(Viewport) ||
			FrameContext >= QueuedFrameCount)
		{
			return false;
		}

		FEditorMaterialGlobalConstants Global;
		Global.SceneView = {Viewport.ScenePacket.Camera.NearPlane,
			Viewport.ScenePacket.Camera.FarPlane,
			static_cast<float>(Viewport.Width),
			static_cast<float>(Viewport.Height)};
		Global.ViewProjectionWorldMatrix =
			Viewport.ScenePacket.Camera.ViewProjection;
		Global.CameraPositionAndTime = {
			Viewport.ScenePacket.Camera.WorldPosition[0],
			Viewport.ScenePacket.Camera.WorldPosition[1],
			Viewport.ScenePacket.Camera.WorldPosition[2],
			DeterministicTest.Enabled
				? DeterministicTest.FixedShaderTimeSeconds : 0.0f};
		Global.DrawDataBufferIndex = PreviewDrawDataDescriptorIndex;
		Global.MaterialInstanceBufferIndex =
			PreviewMaterialInstanceDescriptorIndex;
		Global.MaterialParameterBufferIndex =
			PreviewMaterialParameterDescriptorIndex;
		Global.DefaultMaterialSamplerIndex = PreviewDefaultSamplerIndex;
		Global.LightDataBufferIndex =
			PreviewLightDataDescriptorIndex;
		const u32 LightCount =
			std::min<u32>(
				static_cast<u32>(
					Viewport.ScenePacket.Lights.size()),
				MaxSceneLightsPerViewport);
		const u32 AbsoluteLightIndex =
			FrameContext * MaxSceneViewports *
				MaxSceneLightsPerViewport +
			Viewport.SceneLightBase;
		Global.LightDataOffset = AbsoluteLightIndex;
		Global.LightCount = LightCount;

		xr_vector<FMaterialLightGpuData> GpuLights;
		GpuLights.reserve(LightCount);
		for (u32 Index = 0; Index < LightCount; ++Index)
		{
			const FEditorSceneLight& Source =
				Viewport.ScenePacket.Lights[Index];
			FMaterialLightGpuData Light;
			Light.Position = {
				Source.LocalToWorld[12],
				Source.LocalToWorld[13],
				Source.LocalToWorld[14]};
			Light.Range = Source.Range;
			Light.Direction = {
				Source.LocalToWorld[8],
				Source.LocalToWorld[9],
				Source.LocalToWorld[10]};
			const float DirectionLength = std::sqrt(
				Light.Direction[0] * Light.Direction[0] +
				Light.Direction[1] * Light.Direction[1] +
				Light.Direction[2] * Light.Direction[2]);
			if (DirectionLength >
				std::numeric_limits<float>::epsilon())
			{
				for (float& Component : Light.Direction)
					Component /= DirectionLength;
			}
			else
			{
				Light.Direction = {0.0f, 0.0f, 1.0f};
			}
			switch (Source.Type)
			{
			case EEditorSceneLightType::Directional:
				Light.Type = static_cast<u32>(
					EMaterialLightType::Directional);
				break;
			case EEditorSceneLightType::Point:
				Light.Type = static_cast<u32>(
					EMaterialLightType::Point);
				break;
			case EEditorSceneLightType::Spot:
				Light.Type = static_cast<u32>(
					EMaterialLightType::Spot);
				break;
			}
			Light.Color = Source.Color;
			Light.Intensity = Source.Intensity;
			constexpr float DegreesToRadians =
				3.14159265358979323846f / 180.0f;
			Light.CosInnerCone = std::cos(
				Source.InnerConeAngleDegrees * DegreesToRadians);
			Light.CosOuterCone = std::cos(
				Source.OuterConeAngleDegrees * DegreesToRadians);
			const u32 SourceFlags =
				static_cast<u32>(Source.Flags);
			if ((SourceFlags & static_cast<u32>(
					EEditorSceneLightFlags::CastShadows)) != 0)
			{
				Light.Flags |= static_cast<u32>(
					EMaterialLightFlags::CastShadows);
			}
			if ((SourceFlags & static_cast<u32>(
					EEditorSceneLightFlags::Selected)) != 0)
			{
				Light.Flags |= static_cast<u32>(
					EMaterialLightFlags::Selected);
			}
			GpuLights.push_back(Light);
		}
		if (!GpuLights.empty() &&
			!WritePreviewBuffer(PreviewLightDataBuffer,
				u64(AbsoluteLightIndex) *
					MaterialLightGpuDataSize,
				GpuLights.data(),
				GpuLights.size() * MaterialLightGpuDataSize))
		{
			return false;
		}
		if (!WritePreviewBuffer(Viewport.SceneConstantsBuffer,
				u64(FrameContext) * 256, &Global, sizeof(Global)))
		{
			return false;
		}

		xr_vector<FMaterialDrawGpuData> Draws;
		Viewport.SceneSelectionDrawCount = 0;
		Draws.reserve(Viewport.ScenePacket.Instances.size());
		for (const FEditorStaticMeshInstance& Instance :
			Viewport.ScenePacket.Instances)
		{
			const auto MeshIt = GpuMeshes.find(Instance.MeshId.Value);
			if (MeshIt == GpuMeshes.end())
				continue;
			for (const FEditorStaticMeshSection& Section : MeshIt->second.Sections)
			{
				if (Draws.size() >= MaxSceneDrawsPerViewport)
				{
					Diagnostic = "Editor viewport exceeded its material draw capacity";
					break;
				}
				FMaterialDrawGpuData Draw;
				Draw.LocalToWorld = MakeMaterialDrawBufferMatrix(
					Instance.LocalToWorld);
				Draw.PreviousLocalToWorld = Draw.LocalToWorld;
				const FEditorMaterialSlotId MaterialSlot =
					ResolveEditorMaterialSlot(
						Instance, Section.MaterialSlot);
				const auto Material = SceneMaterials.find(MaterialSlot.Value);
				const u32 LocalMaterialIndex =
					Material == SceneMaterials.end() ? 0u :
					Material->second.InstanceIndex;
				Draw.MaterialInstanceIndex =
					FrameContext * MaxEditorMaterialInstances +
					LocalMaterialIndex;
				Draw.ObjectId = static_cast<u32>(Instance.ObjectId.Value) ^
					static_cast<u32>(Instance.ObjectId.Value >> 32u);
				Draw.Flags = static_cast<u32>(Instance.Flags);
				Draws.push_back(Draw);
				if ((static_cast<u32>(Instance.Flags) &
					static_cast<u32>(EEditorSceneInstanceFlags::Selected)) != 0)
				{
					++Viewport.SceneSelectionDrawCount;
				}
			}
			if (Draws.size() >= MaxSceneDrawsPerViewport)
				break;
		}
		Viewport.SceneDrawCount = static_cast<u32>(Draws.size());
		if (Draws.empty())
			return true;
		const u32 AbsoluteDrawIndex =
			FrameContext * MaxEditorDrawRecords + Viewport.SceneDrawBase;
		return WritePreviewBuffer(PreviewDrawDataBuffer,
			u64(AbsoluteDrawIndex) * sizeof(FMaterialDrawGpuData),
			Draws.data(), Draws.size() * sizeof(FMaterialDrawGpuData));
	}

	void RecordPendingMeshBarriers(nri::CommandBuffer& CommandBuffer)
	{
		xr_vector<nri::BufferBarrierDesc> Barriers;
		for (auto& [MeshId, Mesh] : GpuMeshes)
		{
			(void)MeshId;
			if (!Mesh.NeedsBarrier)
				continue;
			nri::BufferBarrierDesc Barrier = {};
			Barrier.buffer = Mesh.Buffer;
			Barrier.before = {nri::AccessBits::NONE, nri::StageBits::NONE};
			Barrier.after = {nri::AccessBits::VERTEX_BUFFER |
				nri::AccessBits::INDEX_BUFFER,
				nri::StageBits::VERTEX_SHADER | nri::StageBits::INDEX_INPUT};
			Barriers.push_back(Barrier);
			Mesh.NeedsBarrier = false;
		}
		for (auto& [ViewportId, Viewport] : Viewports)
		{
			(void)ViewportId;
			if (!Viewport.DebugDrawBuffer || !Viewport.DebugDrawNeedsBarrier)
				continue;
			nri::BufferBarrierDesc Barrier = {};
			Barrier.buffer = Viewport.DebugDrawBuffer;
			Barrier.before = {nri::AccessBits::NONE, nri::StageBits::NONE};
			Barrier.after = {nri::AccessBits::VERTEX_BUFFER,
				nri::StageBits::VERTEX_SHADER};
			Barriers.push_back(Barrier);
			Viewport.DebugDrawNeedsBarrier = false;
		}
		if (Barriers.empty())
			return;
		nri::BarrierDesc Desc = {};
		Desc.buffers = Barriers.data();
		Desc.bufferNum = static_cast<u32>(Barriers.size());
		CoreInterface.CmdBarrier(CommandBuffer, Desc);
	}

	void RecordSceneGeometry(nri::CommandBuffer& CommandBuffer,
		const FViewport& Viewport, const u32 FrameContext)
	{
		if (!ScenePipeline || !ScenePipelineLayout ||
			FrameContext >= QueuedFrameCount)
			return;
		const nri::Viewport NriViewport = {0.0f, 0.0f,
			static_cast<float>(Viewport.Width), static_cast<float>(Viewport.Height),
			0.0f, 1.0f};
		CoreInterface.CmdSetViewports(CommandBuffer, &NriViewport, 1);
		const nri::Rect Scissor = {0, 0,
			static_cast<nri::Dim_t>(Viewport.Width),
			static_cast<nri::Dim_t>(Viewport.Height)};
		CoreInterface.CmdSetScissors(CommandBuffer, &Scissor, 1);

		// The first frame normally uses the diagnostic pipeline while the real
		// material permutation is compiling in the background.
		CoreInterface.CmdSetPipelineLayout(CommandBuffer,
			nri::BindPoint::GRAPHICS, *ScenePipelineLayout);
		bool MaterialLayoutBound = false;
		nri::Pipeline* BoundPipeline = nullptr;
		u32 DrawOffset = 0;
		for (const FEditorStaticMeshInstance& Instance : Viewport.ScenePacket.Instances)
		{
			const auto MeshIt = GpuMeshes.find(Instance.MeshId.Value);
			if (MeshIt == GpuMeshes.end())
				continue;
			const FGpuMesh& Mesh = MeshIt->second;
			CoreInterface.CmdSetIndexBuffer(CommandBuffer, *Mesh.Buffer,
				0, nri::IndexType::UINT32);
			const nri::VertexBufferDesc VertexBuffer = {
				Mesh.Buffer, Mesh.VertexOffset, sizeof(FEditorStaticMeshVertex)};
			CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &VertexBuffer, 1);
			for (const FEditorStaticMeshSection& Section : Mesh.Sections)
			{
				if (DrawOffset >= Viewport.SceneDrawCount)
					return;
				const FEditorMaterialSlotId MaterialSlot =
					ResolveEditorMaterialSlot(
						Instance, Section.MaterialSlot);
				const auto MaterialIt = SceneMaterials.find(MaterialSlot.Value);
				const FSceneMaterial* Material = MaterialIt == SceneMaterials.end()
					? nullptr : &MaterialIt->second;
				if (Material && Material->Pipeline &&
					Viewport.SceneConstantsSets[FrameContext])
				{
					if (!MaterialLayoutBound)
					{
						CoreInterface.CmdSetDescriptorPool(CommandBuffer,
							*PreviewDescriptorPool);
						CoreInterface.CmdSetPipelineLayout(CommandBuffer,
							nri::BindPoint::GRAPHICS, *PreviewPipelineLayout);
						const nri::SetDescriptorSetDesc Sets[] = {
							{0, PreviewResourcesSet}, {1, PreviewSamplersSet},
							{2, Viewport.SceneConstantsSets[FrameContext]}};
						for (const nri::SetDescriptorSetDesc& Set : Sets)
							CoreInterface.CmdSetDescriptorSet(CommandBuffer, Set);
						MaterialLayoutBound = true;
						BoundPipeline = nullptr;
					}
					if (BoundPipeline != Material->Pipeline)
					{
						CoreInterface.CmdSetPipeline(CommandBuffer,
							*Material->Pipeline);
						BoundPipeline = Material->Pipeline;
					}
					const u32 AbsoluteDrawIndex =
						FrameContext * MaxEditorDrawRecords +
						Viewport.SceneDrawBase + DrawOffset;
					CoreInterface.CmdDrawIndexed(CommandBuffer,
						{Section.IndexCount, 1, Section.FirstIndex, 0,
							AbsoluteDrawIndex});
					Statistics.RecordDraw(Section.IndexCount / 3);
					++DrawOffset;
					continue;
				}

				if (MaterialLayoutBound)
				{
					CoreInterface.CmdSetPipelineLayout(CommandBuffer,
						nri::BindPoint::GRAPHICS, *ScenePipelineLayout);
					MaterialLayoutBound = false;
					BoundPipeline = nullptr;
				}
				CoreInterface.CmdSetPipeline(CommandBuffer, *ScenePipeline);
				FEditorSceneDrawConstants Constants = {
					Instance.LocalToWorld,
					Viewport.ScenePacket.Camera.ViewProjection,
					static_cast<u32>(MaterialSlot.Value),
					static_cast<u32>(MaterialSlot.Value >> 32u),
					static_cast<u32>(Instance.Flags)};
				const nri::SetRootConstantsDesc RootConstants = {
					0, &Constants, sizeof(Constants), 0,
					nri::BindPoint::GRAPHICS};
				CoreInterface.CmdSetRootConstants(CommandBuffer, RootConstants);
				CoreInterface.CmdDrawIndexed(CommandBuffer,
					{Section.IndexCount, 1, Section.FirstIndex, 0, 0});
				Statistics.RecordDraw(Section.IndexCount / 3);
				++DrawOffset;
			}
		}
	}

	void RecordSceneSelectionOverlay(nri::CommandBuffer& CommandBuffer,
		const FViewport& Viewport)
	{
		if (!SceneSelectionPipeline || !ScenePipelineLayout ||
			Viewport.SceneSelectionDrawCount == 0)
		{
			return;
		}
		CoreInterface.CmdSetPipelineLayout(CommandBuffer,
			nri::BindPoint::GRAPHICS, *ScenePipelineLayout);
		CoreInterface.CmdSetPipeline(CommandBuffer, *SceneSelectionPipeline);
		for (const FEditorStaticMeshInstance& Instance : Viewport.ScenePacket.Instances)
		{
			if ((static_cast<u32>(Instance.Flags) &
				static_cast<u32>(EEditorSceneInstanceFlags::Selected)) == 0)
			{
				continue;
			}
			const auto MeshIt = GpuMeshes.find(Instance.MeshId.Value);
			if (MeshIt == GpuMeshes.end())
				continue;
			const FGpuMesh& Mesh = MeshIt->second;
			CoreInterface.CmdSetIndexBuffer(CommandBuffer, *Mesh.Buffer,
				0, nri::IndexType::UINT32);
			const nri::VertexBufferDesc VertexBuffer = {
				Mesh.Buffer, Mesh.VertexOffset, sizeof(FEditorStaticMeshVertex)};
			CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &VertexBuffer, 1);
			for (const FEditorStaticMeshSection& Section : Mesh.Sections)
			{
				const FEditorMaterialSlotId MaterialSlot =
					ResolveEditorMaterialSlot(
						Instance, Section.MaterialSlot);
				FEditorSceneDrawConstants Constants = {
					Instance.LocalToWorld,
					Viewport.ScenePacket.Camera.ViewProjection,
					static_cast<u32>(MaterialSlot.Value),
					static_cast<u32>(MaterialSlot.Value >> 32u),
					static_cast<u32>(Instance.Flags)};
				const nri::SetRootConstantsDesc RootConstants = {
					0, &Constants, sizeof(Constants), 0,
					nri::BindPoint::GRAPHICS};
				CoreInterface.CmdSetRootConstants(CommandBuffer, RootConstants);
				CoreInterface.CmdDrawIndexed(CommandBuffer,
					{Section.IndexCount, 1, Section.FirstIndex, 0, 0});
				Statistics.RecordDraw(Section.IndexCount / 3);
			}
		}
	}

	void RecordViewportDebugDraw(nri::CommandBuffer& CommandBuffer,
		const FViewport& Viewport)
	{
		if (!ScenePipelineLayout || !Viewport.DebugDrawBuffer ||
			(Viewport.DebugLineVertexCount == 0 &&
				Viewport.DebugTriangleVertexCount == 0 &&
				Viewport.OverlayLineVertexCount == 0 &&
				Viewport.OverlayTriangleVertexCount == 0))
		{
			return;
		}
		const nri::Viewport NriViewport = {0.0f, 0.0f,
			static_cast<float>(Viewport.Width), static_cast<float>(Viewport.Height),
			0.0f, 1.0f};
		CoreInterface.CmdSetViewports(CommandBuffer, &NriViewport, 1);
		const nri::Rect Scissor = {0, 0,
			static_cast<nri::Dim_t>(Viewport.Width),
			static_cast<nri::Dim_t>(Viewport.Height)};
		CoreInterface.CmdSetScissors(CommandBuffer, &Scissor, 1);
		CoreInterface.CmdSetPipelineLayout(CommandBuffer,
			nri::BindPoint::GRAPHICS, *ScenePipelineLayout);
		FEditorSceneDrawConstants Constants = {};
		Constants.LocalToWorld = {
			1.0f, 0.0f, 0.0f, 0.0f,
			0.0f, 1.0f, 0.0f, 0.0f,
			0.0f, 0.0f, 1.0f, 0.0f,
			0.0f, 0.0f, 0.0f, 1.0f};
		Constants.ViewProjection =
			Viewport.ScenePacket.Camera.ViewProjection;
		const nri::SetRootConstantsDesc RootConstants = {
			0, &Constants, sizeof(Constants), 0, nri::BindPoint::GRAPHICS};
		CoreInterface.CmdSetRootConstants(CommandBuffer, RootConstants);

		if (Viewport.DebugLineVertexCount != 0 && SceneDebugLinePipeline)
		{
			CoreInterface.CmdSetPipeline(CommandBuffer, *SceneDebugLinePipeline);
			const nri::VertexBufferDesc VertexBuffer = {
				Viewport.DebugDrawBuffer, 0, sizeof(FEditorDebugVertex)};
			CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &VertexBuffer, 1);
			CoreInterface.CmdDraw(CommandBuffer,
				{Viewport.DebugLineVertexCount, 1, 0, 0});
			Statistics.RecordDraw(0, Viewport.DebugLineVertexCount / 2);
		}
		if (Viewport.DebugTriangleVertexCount != 0 &&
			SceneDebugTrianglePipeline)
		{
			CoreInterface.CmdSetPipeline(CommandBuffer,
				*SceneDebugTrianglePipeline);
			const nri::VertexBufferDesc VertexBuffer = {
				Viewport.DebugDrawBuffer, Viewport.DebugTriangleOffset,
				sizeof(FEditorDebugVertex)};
			CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &VertexBuffer, 1);
			CoreInterface.CmdDraw(CommandBuffer,
				{Viewport.DebugTriangleVertexCount, 1, 0, 0});
			Statistics.RecordDraw(Viewport.DebugTriangleVertexCount / 3);
		}
		if (Viewport.OverlayLineVertexCount != 0 && SceneOverlayLinePipeline)
		{
			CoreInterface.CmdSetPipeline(CommandBuffer, *SceneOverlayLinePipeline);
			const nri::VertexBufferDesc VertexBuffer = {
				Viewport.DebugDrawBuffer, Viewport.OverlayLineOffset,
				sizeof(FEditorDebugVertex)};
			CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &VertexBuffer, 1);
			CoreInterface.CmdDraw(CommandBuffer,
				{Viewport.OverlayLineVertexCount, 1, 0, 0});
			Statistics.RecordDraw(0, Viewport.OverlayLineVertexCount / 2);
		}
		if (Viewport.OverlayTriangleVertexCount != 0 &&
			SceneOverlayTrianglePipeline)
		{
			CoreInterface.CmdSetPipeline(CommandBuffer,
				*SceneOverlayTrianglePipeline);
			const nri::VertexBufferDesc VertexBuffer = {
				Viewport.DebugDrawBuffer, Viewport.OverlayTriangleOffset,
				sizeof(FEditorDebugVertex)};
			CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &VertexBuffer, 1);
			CoreInterface.CmdDraw(CommandBuffer,
				{Viewport.OverlayTriangleVertexCount, 1, 0, 0});
			Statistics.RecordDraw(Viewport.OverlayTriangleVertexCount / 3);
		}
	}

	[[nodiscard]] FMaterialPreview* FindPreviewByViewport(
		const u32 ViewportId) noexcept
	{
		if ((ViewportId & 0x80000000u) == 0)
			return nullptr;
		const u32 Index = ViewportId & 0x7fffffffu;
		if (Index >= MaterialPreviews.size())
			return nullptr;
		FMaterialPreview& Preview = MaterialPreviews[Index];
		return Preview.Alive && Preview.ViewportId == ViewportId ? &Preview : nullptr;
	}

	void RecordMaterialPreviewGeometry(nri::CommandBuffer& CommandBuffer,
		const FViewport& Viewport, const FMaterialPreview& Preview,
		const u32 FrameContext)
	{
		if (!Preview.Pipeline || !PreviewPipelineLayout)
			return;
		const auto MeshIt = GpuMeshes.find(Preview.MeshId.Value);
		if (MeshIt == GpuMeshes.end())
			return;
		// Direct descriptor-heap indexing is part of the material shader
		// contract, so the owning heap must be bound before its root signature.
		CoreInterface.CmdSetDescriptorPool(CommandBuffer, *PreviewDescriptorPool);
		CoreInterface.CmdSetPipelineLayout(CommandBuffer,
			nri::BindPoint::GRAPHICS, *PreviewPipelineLayout);
		const nri::SetDescriptorSetDesc Sets[] = {
			{0, PreviewResourcesSet}, {1, PreviewSamplersSet},
			{2, PreviewConstantsSet}};
		for (const nri::SetDescriptorSetDesc& Set : Sets)
			CoreInterface.CmdSetDescriptorSet(CommandBuffer, Set);
		CoreInterface.CmdSetPipeline(CommandBuffer, *Preview.Pipeline);
		const nri::Viewport NriViewport = {0.0f, 0.0f,
			static_cast<float>(Viewport.Width), static_cast<float>(Viewport.Height),
			0.0f, 1.0f};
		CoreInterface.CmdSetViewports(CommandBuffer, &NriViewport, 1);
		const nri::Rect Scissor = {0, 0,
			static_cast<nri::Dim_t>(Viewport.Width),
			static_cast<nri::Dim_t>(Viewport.Height)};
		CoreInterface.CmdSetScissors(CommandBuffer, &Scissor, 1);

		const FGpuMesh& Mesh = MeshIt->second;
		CoreInterface.CmdSetIndexBuffer(CommandBuffer, *Mesh.Buffer,
			0, nri::IndexType::UINT32);
		const nri::VertexBufferDesc VertexBuffer = {
			Mesh.Buffer, Mesh.VertexOffset, sizeof(FEditorStaticMeshVertex)};
		CoreInterface.CmdSetVertexBuffers(CommandBuffer, 0, &VertexBuffer, 1);
		const u32 PreviewIndex =
			static_cast<u32>(&Preview - MaterialPreviews.data());
		const u32 AbsoluteDrawIndex =
			FrameContext * MaxEditorDrawRecords + PreviewIndex;
		CoreInterface.CmdDrawIndexed(CommandBuffer,
			{Mesh.IndexCount, 1, 0, 0, AbsoluteDrawIndex});
		Statistics.RecordDraw(Mesh.IndexCount / 3);
	}

	bool EnsureViewportResources()
	{
		bool MustWaitForResize = false;
		for (const auto& [ViewportId, Viewport] : Viewports)
		{
			(void)ViewportId;
			MustWaitForResize |= Viewport.Texture &&
				(Viewport.Width != Viewport.DesiredWidth ||
					Viewport.Height != Viewport.DesiredHeight);
		}
		if (MustWaitForResize)
			CoreInterface.QueueWaitIdle(GraphicsQueue);

		for (auto& [ViewportId, Viewport] : Viewports)
		{
			(void)ViewportId;
			if (Viewport.DesiredWidth == 0 || Viewport.DesiredHeight == 0)
				continue;
			if (Viewport.Texture && Viewport.Width == Viewport.DesiredWidth &&
				Viewport.Height == Viewport.DesiredHeight)
			{
				continue;
			}
			DestroyViewport(Viewport);
			if (!CreateViewport(Viewport))
				return false;
		}
		return true;
	}

	void RecordViewportPasses(nri::CommandBuffer& CommandBuffer,
		const u32 FrameContext)
	{
		UploadMaterialFrameData(FrameContext);
		for (auto& [ViewportId, Viewport] : Viewports)
		{
			if (!Viewport.CaptureRequested || !Viewport.Texture)
				continue;
			FMaterialPreview* MaterialPreview = FindPreviewByViewport(ViewportId);
			CoreInterface.CmdBeginAnnotation(CommandBuffer,
				MaterialPreview ? "Editor.MaterialPreview" :
					"Editor.SceneViewport",
				nri::BGRA_UNUSED);
			Statistics.RecordPass();

			// Consume only while recording the renderer frame. The mailbox owns
			// editor-side arrays, so a dedicated render thread can replace this
			// call site without changing the scene submission contract.
			if (Viewport.SceneMailbox->Consume(Viewport.ScenePacket))
				ApplyScenePacket(Viewport, Viewport.ScenePacket);
			if (!MaterialPreview &&
				!UploadSceneViewportFrameData(Viewport, FrameContext))
			{
				Viewport.SceneDrawCount = 0;
			}
			RecordPendingMeshBarriers(CommandBuffer);

			nri::TextureBarrierDesc ToAttachment = {};
			ToAttachment.texture = Viewport.Texture;
			if (Viewport.HasShaderResourceState)
			{
				ToAttachment.before = {nri::AccessBits::SHADER_RESOURCE,
					nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER};
			}
			ToAttachment.after = {nri::AccessBits::COLOR_ATTACHMENT_WRITE,
				nri::Layout::COLOR_ATTACHMENT, nri::StageBits::COLOR_ATTACHMENT};
			ToAttachment.mipNum = 1;
			ToAttachment.layerNum = 1;
			xr_array<nri::TextureBarrierDesc, 2> AttachmentBarriers = {};
			AttachmentBarriers[0] = ToAttachment;
			u32 AttachmentBarrierCount = 1;
			if (!Viewport.HasDepthAttachmentState)
			{
				nri::TextureBarrierDesc& ToDepth =
					AttachmentBarriers[AttachmentBarrierCount++];
				ToDepth.texture = Viewport.DepthTexture;
				ToDepth.after = {nri::AccessBits::DEPTH_STENCIL_ATTACHMENT_WRITE,
					nri::Layout::DEPTH_STENCIL_ATTACHMENT,
					nri::StageBits::DEPTH_STENCIL_ATTACHMENT};
				ToDepth.mipNum = 1;
				ToDepth.layerNum = 1;
				ToDepth.planes = nri::PlaneBits::DEPTH;
			}
			nri::BarrierDesc Barrier = {};
			Barrier.textures = AttachmentBarriers.data();
			Barrier.textureNum = AttachmentBarrierCount;
			CoreInterface.CmdBarrier(CommandBuffer, Barrier);

			const float Accent = static_cast<float>((ViewportId % 5u) + 1u) * 0.018f;
			nri::AttachmentDesc Color = {};
			Color.descriptor = Viewport.ColorAttachment;
			Color.loadOp = nri::LoadOp::CLEAR;
			if (MaterialPreview && MaterialPreview->Environment == "Neutral")
				Color.clearValue.color.f = {0.12f, 0.12f, 0.12f, 1.0f};
			else if (MaterialPreview && MaterialPreview->Environment == "Outdoor")
				Color.clearValue.color.f = {0.16f, 0.24f, 0.34f, 1.0f};
			else
				Color.clearValue.color.f = MaterialPreview
					? nri::Color32f{0.035f, 0.04f, 0.055f, 1.0f}
					: nri::Color32f{0.025f + Accent, 0.035f, 0.055f, 1.0f};
			nri::AttachmentDesc Depth = {};
			Depth.descriptor = Viewport.DepthAttachment;
			Depth.loadOp = nri::LoadOp::CLEAR;
			Depth.storeOp = nri::StoreOp::STORE;
			Depth.clearValue.depthStencil = {1.0f, 0};
			nri::RenderingDesc Rendering = {};
			Rendering.colors = &Color;
			Rendering.colorNum = 1;
			Rendering.depth = Depth;
			CoreInterface.CmdBeginRendering(CommandBuffer, Rendering);
			if (MaterialPreview)
				RecordMaterialPreviewGeometry(CommandBuffer, Viewport,
					*MaterialPreview, FrameContext);
			else
			{
				RecordSceneGeometry(CommandBuffer, Viewport, FrameContext);
				RecordSceneSelectionOverlay(CommandBuffer, Viewport);
				RecordViewportDebugDraw(CommandBuffer, Viewport);
			}
			CoreInterface.CmdEndRendering(CommandBuffer);

			nri::TextureBarrierDesc ToShaderResource = ToAttachment;
			ToShaderResource.before = ToAttachment.after;
			ToShaderResource.after = {nri::AccessBits::SHADER_RESOURCE,
				nri::Layout::SHADER_RESOURCE, nri::StageBits::FRAGMENT_SHADER};
			Barrier.textures = &ToShaderResource;
			Barrier.textureNum = 1;
			CoreInterface.CmdBarrier(CommandBuffer, Barrier);
			Viewport.HasShaderResourceState = true;
			Viewport.HasDepthAttachmentState = true;
			Viewport.CaptureRequested = false;
			CoreInterface.CmdEndAnnotation(CommandBuffer);
		}
	}

	[[nodiscard]] FRenderResourceStatistics CollectResourceStatistics()
	{
		FRenderResourceStatistics Result;
		const auto AddBuffer = [&](const nri::Buffer* Buffer,
			const u64 Bytes = 0)
		{
			if (!Buffer)
				return;
			++Result.TrackedBufferCount;
			Result.TrackedBufferBytes += Bytes;
		};
		const auto AddTexture = [&](const nri::Texture* Texture,
			const u64 Bytes = 0)
		{
			if (!Texture)
				return;
			++Result.TrackedTextureCount;
			Result.TrackedTextureBytes += Bytes;
		};
		const auto AddPipeline = [&](const nri::Pipeline* Pipeline)
		{
			if (Pipeline)
				++Result.TrackedPipelineCount;
		};
		const auto AddDescriptor = [&](const void* Descriptor)
		{
			if (Descriptor)
				++Result.TrackedDescriptorCount;
		};

		const u64 SwapTextureBytes =
			static_cast<u64>(Width) * Height * 4;
		for (const FSwapTexture& Texture : SwapTextures)
		{
			AddTexture(Texture.Texture, SwapTextureBytes);
			AddDescriptor(Texture.ColorAttachment);
		}
		for (const auto& [ViewportId, Viewport] : Viewports)
		{
			(void)ViewportId;
			const u64 AttachmentBytes =
				static_cast<u64>(Viewport.Width) *
				Viewport.Height * 4;
			AddTexture(Viewport.Texture, AttachmentBytes);
			AddTexture(Viewport.DepthTexture, AttachmentBytes);
			AddDescriptor(Viewport.ShaderResource);
			AddDescriptor(Viewport.ColorAttachment);
			AddDescriptor(Viewport.DepthAttachment);
			AddBuffer(Viewport.DebugDrawBuffer,
				Viewport.DebugDrawBufferBytes);
			AddBuffer(Viewport.SceneConstantsBuffer,
				256ull * QueuedFrameCount);
			for (const nri::Descriptor* Descriptor :
				Viewport.SceneConstantsDescriptors)
			{
				AddDescriptor(Descriptor);
			}
		}
		for (const auto& [MeshId, Mesh] : GpuMeshes)
		{
			(void)MeshId;
			AddBuffer(Mesh.Buffer, Mesh.ByteSize);
		}
		for (const FDeferredBuffer& Buffer : DeferredBuffers)
			AddBuffer(Buffer.Buffer);

		{
			std::scoped_lock Lock(UiTexturesMutex);
			for (const auto& [TextureId, Texture] : UiTextures)
			{
				(void)TextureId;
				AddTexture(Texture.Texture,
					static_cast<u64>(Texture.Width) *
						Texture.Height * 4);
				AddDescriptor(Texture.ShaderResource);
			}
		}
		for (const FDeferredUiTexture& Texture : DeferredUiTextures)
		{
			AddTexture(Texture.Texture);
			AddDescriptor(Texture.ShaderResource);
		}

		const u64 PreviewDrawDataBytes =
			u64(MaxEditorDrawRecords) * QueuedFrameCount *
			MaterialDrawGpuDataSize;
		const u64 PreviewInstanceBytes =
			u64(MaxEditorMaterialInstances) * QueuedFrameCount *
			MaterialInstanceGpuDataSize;
		const u64 PreviewParameterBytes =
			u64(MaxEditorMaterialInstances) * QueuedFrameCount *
			PreviewParameterStride;
		const u64 PreviewLightBytes =
			u64(MaxSceneViewports) * QueuedFrameCount *
			MaxSceneLightsPerViewport *
			MaterialLightGpuDataSize;
		AddBuffer(PreviewDrawDataBuffer, PreviewDrawDataBytes);
		AddBuffer(PreviewMaterialInstanceBuffer, PreviewInstanceBytes);
		AddBuffer(PreviewMaterialParameterBuffer, PreviewParameterBytes);
		AddBuffer(PreviewLightDataBuffer, PreviewLightBytes);
		AddBuffer(PreviewGlobalConstantsBuffer, 256);
		AddDescriptor(PreviewDrawDataDescriptor);
		AddDescriptor(PreviewMaterialInstanceDescriptor);
		AddDescriptor(PreviewMaterialParameterDescriptor);
		AddDescriptor(PreviewLightDataDescriptor);
		AddDescriptor(PreviewGlobalConstantsDescriptor);
		AddDescriptor(PreviewDefaultSampler);
		AddTexture(PreviewWhiteTexture, 4);
		AddDescriptor(PreviewWhiteTextureDescriptor);
		AddTexture(PreviewWhiteCubeTexture, 6 * 4);
		AddDescriptor(PreviewWhiteCubeDescriptor);
		for (const auto& [Path, Texture] : MaterialPreviewTextures)
		{
			(void)Path;
			AddTexture(Texture.Texture, Texture.ByteSize);
			AddDescriptor(Texture.Descriptor);
		}

		AddPipeline(ScenePipeline);
		AddPipeline(SceneSelectionPipeline);
		AddPipeline(SceneDebugLinePipeline);
		AddPipeline(SceneDebugTrianglePipeline);
		AddPipeline(SceneOverlayLinePipeline);
		AddPipeline(SceneOverlayTrianglePipeline);
		for (const FMaterialPreview& Preview : MaterialPreviews)
			if (Preview.Alive)
				AddPipeline(Preview.Pipeline);
		for (const auto& [PipelineKey, Entry] : ScenePipelineCache)
		{
			(void)PipelineKey;
			AddPipeline(Entry.Pipeline);
		}
		for (const FDeferredPipeline& Pipeline : DeferredPipelines)
			AddPipeline(Pipeline.Pipeline);

		Result.DeferredResourceCount =
			static_cast<u32>(DeferredBuffers.size() +
				DeferredUiTextures.size() + DeferredPipelines.size());
		return Result;
	}

	void DestroySceneResources()
	{
		for (auto& [MeshId, Mesh] : GpuMeshes)
		{
			(void)MeshId;
			if (Mesh.Buffer)
				CoreInterface.DestroyBuffer(Mesh.Buffer);
		}
		GpuMeshes.clear();
		for (const FDeferredBuffer& Item : DeferredBuffers)
		{
			if (Item.Buffer)
				CoreInterface.DestroyBuffer(Item.Buffer);
		}
		DeferredBuffers.clear();
		if (ScenePipeline)
			CoreInterface.DestroyPipeline(ScenePipeline);
		if (SceneSelectionPipeline)
			CoreInterface.DestroyPipeline(SceneSelectionPipeline);
		if (SceneDebugLinePipeline)
			CoreInterface.DestroyPipeline(SceneDebugLinePipeline);
		if (SceneDebugTrianglePipeline)
			CoreInterface.DestroyPipeline(SceneDebugTrianglePipeline);
		if (SceneOverlayLinePipeline)
			CoreInterface.DestroyPipeline(SceneOverlayLinePipeline);
		if (SceneOverlayTrianglePipeline)
			CoreInterface.DestroyPipeline(SceneOverlayTrianglePipeline);
		if (ScenePipelineLayout)
			CoreInterface.DestroyPipelineLayout(ScenePipelineLayout);
		ScenePipeline = nullptr;
		SceneSelectionPipeline = nullptr;
		SceneDebugLinePipeline = nullptr;
		SceneDebugTrianglePipeline = nullptr;
		SceneOverlayLinePipeline = nullptr;
		SceneOverlayTrianglePipeline = nullptr;
		ScenePipelineLayout = nullptr;
	}

	void Destroy()
	{
		if (GraphicsQueue && CoreInterface.QueueWaitIdle)
			CoreInterface.QueueWaitIdle(GraphicsQueue);
		DestroyViewports();
		DestroyUiTextures();
		DestroyMaterialPreviewContext();
		DestroySceneResources();
		DestroySwapchain();
		for (FFrame& Frame : Frames)
		{
			if (Frame.CommandBuffer)
				CoreInterface.DestroyCommandBuffer(Frame.CommandBuffer);
			if (Frame.Allocator)
				CoreInterface.DestroyCommandAllocator(Frame.Allocator);
		}
		Frames = {};
		if (FrameFence)
			CoreInterface.DestroyFence(FrameFence);
		FrameFence = nullptr;
		if (Imgui)
			ImguiInterface.DestroyImgui(Imgui);
		Imgui = nullptr;
		Streamer = nullptr;
		GraphicsQueue = nullptr;
		NriDevice = nullptr;
		if (OwnsRenderDevice)
			GRenderDevice.Destroy();
		OwnsRenderDevice = false;
		CoreInterface = {};
		HelperInterface = {};
		SwapChainInterface = {};
		StreamerInterface = {};
		ImguiInterface = {};
		FrameIndex = 0;
		Statistics.Reset();
		RegisteredUserTextures.Clear();
		SkippedIncompatibleTextureCount = 0;
		Initialized = false;
	}

	SDL_Window* Window = nullptr;
	ETiramisuEditorGraphicsApi Api = ETiramisuEditorGraphicsApi::Vulkan;
	FRenderDeterministicTestPolicy DeterministicTest;
	xr_string Diagnostic;
	bool Initialized = false;
	bool OwnsRenderDevice = false;

	nri::Device* NriDevice = nullptr;
	nri::Queue* GraphicsQueue = nullptr;
	nri::CoreInterface CoreInterface = {};
	nri::HelperInterface HelperInterface = {};
	nri::SwapChainInterface SwapChainInterface = {};
	nri::StreamerInterface StreamerInterface = {};
	nri::ImguiInterface ImguiInterface = {};
	nri::Streamer* Streamer = nullptr;
	nri::Imgui* Imgui = nullptr;
	nri::Fence* FrameFence = nullptr;
	xr_array<FFrame, QueuedFrameCount> Frames = {};

	nri::SwapChain* SwapChain = nullptr;
	nri::Format SwapFormat = nri::Format::UNKNOWN;
	xr_vector<FSwapTexture> SwapTextures;
	u32 Width = 0;
	u32 Height = 0;
	u64 FrameIndex = 0;
	FRenderStatisticsTracker Statistics;
	xr_hash_map<u32, FViewport> Viewports;
	xr_hash_map<u64, FGpuMesh> GpuMeshes;
	xr_vector<FDeferredBuffer> DeferredBuffers;
	TiramisuEditorTextureMailbox UiTextureMailbox;
	mutable std::mutex UiTexturesMutex;
	xr_hash_map<u64, FUiTexture> UiTextures;
	xr_vector<FDeferredUiTexture> DeferredUiTextures;
	xr_vector<FMaterialPreview> MaterialPreviews;
	xr_vector<u32> FreeMaterialPreviewSlots;
	xr_vector<FMaterialPreviewCompileJob> MaterialPreviewCompileJobs;
	std::unique_ptr<Tiramisu::Editor::TiramisuEditorViewportMaterialResolver>
		SceneMaterialResolver;
	std::filesystem::path SceneMaterialRoot;
	Tiramisu::Editor::TiramisuMaterialDependencyWatcher SceneMaterialDependencyWatcher;
	xr_vector<std::filesystem::path> SceneMaterialDependencies;
	std::future<FSceneMaterialResolverReloadResult> SceneMaterialResolverReload;
	std::chrono::steady_clock::time_point NextSceneMaterialDependencyPoll{};
	bool SceneMaterialReloadSmokeTriggered = false;
	xr_hash_map<u64, FSceneMaterial> SceneMaterials;
	TEditorBoundedAsyncQueue<FSceneMaterialCompileRequest,
		FSceneMaterialCompileResult> SceneMaterialCompileQueue;
	xr_hash_map<FScenePipelineCacheKey, FScenePipelineCacheEntry,
		FScenePipelineCacheKeyHash> ScenePipelineCache;
	u32 NextSceneMaterialInstance = MaxMaterialPreviews;
	u32 SceneViewportCount = 0;
	u64 SceneMaterialRevision = 0;
	xr_vector<u32> PendingMaterialPreviewDestroy;
	xr_vector<FDeferredPipeline> DeferredPipelines;
	nri::DescriptorPool* PreviewDescriptorPool = nullptr;
	nri::PipelineLayout* PreviewPipelineLayout = nullptr;
	nri::DescriptorSet* PreviewResourcesSet = nullptr;
	nri::DescriptorSet* PreviewSamplersSet = nullptr;
	nri::DescriptorSet* PreviewConstantsSet = nullptr;
	nri::Buffer* PreviewDrawDataBuffer = nullptr;
	nri::Buffer* PreviewMaterialInstanceBuffer = nullptr;
	nri::Buffer* PreviewMaterialParameterBuffer = nullptr;
	nri::Buffer* PreviewLightDataBuffer = nullptr;
	nri::Buffer* PreviewGlobalConstantsBuffer = nullptr;
	nri::Descriptor* PreviewDrawDataDescriptor = nullptr;
	nri::Descriptor* PreviewMaterialInstanceDescriptor = nullptr;
	nri::Descriptor* PreviewMaterialParameterDescriptor = nullptr;
	nri::Descriptor* PreviewLightDataDescriptor = nullptr;
	nri::Descriptor* PreviewGlobalConstantsDescriptor = nullptr;
	nri::Descriptor* PreviewDefaultSampler = nullptr;
	nri::Texture* PreviewWhiteTexture = nullptr;
	nri::Descriptor* PreviewWhiteTextureDescriptor = nullptr;
	nri::Texture* PreviewWhiteCubeTexture = nullptr;
	nri::Descriptor* PreviewWhiteCubeDescriptor = nullptr;
	xr_hash_map<xr_string, FMaterialPreviewTexture>
		MaterialPreviewTextures;
	u32 NextMaterialPreviewTextureDescriptor =
		PreviewFirstAssetTextureDescriptorIndex;
	nri::PipelineLayout* ScenePipelineLayout = nullptr;
	nri::Pipeline* ScenePipeline = nullptr;
	nri::Pipeline* SceneSelectionPipeline = nullptr;
	nri::Pipeline* SceneDebugLinePipeline = nullptr;
	nri::Pipeline* SceneDebugTrianglePipeline = nullptr;
	nri::Pipeline* SceneOverlayLinePipeline = nullptr;
	nri::Pipeline* SceneOverlayTrianglePipeline = nullptr;
	TiramisuEditorNriTextureRegistry RegisteredUserTextures;
	u32 SkippedIncompatibleTextureCount = 0;
};

TiramisuEditorRenderBridge::TiramisuEditorRenderBridge(
	SDL_Window* Window, const ETiramisuEditorGraphicsApi Api,
	const FRenderDeterministicTestPolicy& DeterministicTest)
	: Impl(std::make_unique<FImpl>(Window, Api, DeterministicTest))
{}

TiramisuEditorRenderBridge::~TiramisuEditorRenderBridge()
{
	Shutdown();
}

EXrUIRendererPlatform TiramisuEditorRenderBridge::GetPlatform() const noexcept
{
	return Impl->Api == ETiramisuEditorGraphicsApi::Vulkan
		? EXrUIRendererPlatform::Vulkan
		: EXrUIRendererPlatform::D3D;
}

bool TiramisuEditorRenderBridge::SupportsPlatformViewports() const noexcept
{
	return false;
}

bool TiramisuEditorRenderBridge::OwnsMainPresentation() const noexcept
{
	return true;
}

bool TiramisuEditorRenderBridge::Initialize()
{
	if (Impl->Initialized)
		return true;
	Impl->Diagnostic.clear();
	if (!Impl->Window)
		Impl->Window = g_AppInfo.Window;

	const nri::GraphicsAPI RequestedApi =
		Impl->Api == ETiramisuEditorGraphicsApi::D3D12
		? nri::GraphicsAPI::D3D12
		: nri::GraphicsAPI::VK;
	if (GRenderDevice.IsInitialized() &&
		GRenderDevice.GraphicsApi != RequestedApi)
	{
		Impl->Diagnostic =
			"Tiramisu render device uses a different graphics API";
		return false;
	}

	Impl->OwnsRenderDevice = !GRenderDevice.IsInitialized();
	if (Impl->OwnsRenderDevice)
	{
		nri::CallbackInterface CallbackInterface = {};
		CallbackInterface.MessageCallback = EditorNriMessageCallback;
		GRenderDevice.Initialize(RequestedApi, CallbackInterface);
	}

	// Редактор использует устройство, очереди и streamer основного рендера.
	// NRI-объекты не создаются и не принадлежат модулю LevelEditor.
	Impl->NriDevice = GRenderDevice.Device;
	Impl->GraphicsQueue = GRenderDevice.GraphicsQueue;
	Impl->CoreInterface = GRenderDevice.CoreInterface;
	Impl->HelperInterface = GRenderDevice.HelperInterface;
	Impl->SwapChainInterface = GRenderDevice.SwapChainInterface;
	Impl->StreamerInterface = GRenderDevice.StreamerInterface;
	Impl->ImguiInterface = GRenderDevice.ImGuiInterface;
	Impl->Streamer = GRenderDevice.Streamer;

	// Scene rendering is allowed to degrade to the diagnostic clear viewport;
	// ImGui and editor controls remain usable if DXC or pipeline creation fails.
	(void)Impl->CreateScenePipeline();
	if (!Impl->CreateMaterialPreviewContext())
	{
		Impl->Destroy();
		return false;
	}
	// A missing/invalid material library keeps the diagnostic scene pipeline
	// available, but prevents creation of real material pass proxies.
	(void)Impl->LoadSceneMaterialResolver();

	nri::ImguiDesc ImguiDesc = {};
	ImguiDesc.descriptorPoolSize = 16384;
	if (!Impl->Check(Impl->ImguiInterface.CreateImgui(
			*Impl->NriDevice, ImguiDesc, Impl->Imgui),
			"Failed to create the NRI editor ImGui renderer") ||
		!Impl->Check(Impl->CoreInterface.CreateFence(
			*Impl->NriDevice, 0, Impl->FrameFence),
			"Failed to create the NRI editor frame fence"))
	{
		Impl->Destroy();
		return false;
	}

	for (FImpl::FFrame& Frame : Impl->Frames)
	{
		if (!Impl->Check(Impl->CoreInterface.CreateCommandAllocator(
				*Impl->GraphicsQueue, Frame.Allocator),
				"Failed to create an NRI editor command allocator") ||
			!Impl->Check(Impl->CoreInterface.CreateCommandBuffer(
				*Frame.Allocator, Frame.CommandBuffer),
				"Failed to create an NRI editor command buffer"))
		{
			Impl->Destroy();
			return false;
		}
	}
	if (!Impl->CreateSwapchain())
	{
		Impl->Destroy();
		return false;
	}

	ImGuiIO& Io = ImGui::GetIO();
	Io.BackendRendererName = "Tiramisu_NRIImgui";
	Io.BackendRendererUserData = this;
	Io.BackendFlags |= ImGuiBackendFlags_RendererHasTextures;
	Impl->Initialized = true;
	return true;
}

void TiramisuEditorRenderBridge::Shutdown()
{
	if (ImGui::GetCurrentContext())
	{
		ImGuiIO& Io = ImGui::GetIO();
		if (Io.BackendRendererUserData == this)
		{
			Io.BackendRendererName = nullptr;
			Io.BackendRendererUserData = nullptr;
			Io.BackendFlags &= ~ImGuiBackendFlags_RendererHasTextures;
		}
	}
	Impl->Destroy();
}

void TiramisuEditorRenderBridge::BeginFrame()
{
	// Consume copied producer data before UI construction so GetTextureSurface
	// returns the descriptor that this frame's ImGui draw lists will reference.
	if (Impl->Initialized)
	{
		Impl->ProcessUiTextureMailbox();
	}
}

void TiramisuEditorRenderBridge::RenderDrawData(ImDrawData& DrawData)
{
	Impl->SkippedIncompatibleTextureCount = 0;
	if (!Impl->Initialized || DrawData.CmdListsCount == 0 ||
		!Impl->EnsureSwapchainSize() || !Impl->EnsureViewportResources())
	{
		return;
	}

	const xr_optional<FEditorNriFramePlan> Plan = MakeEditorNriFramePlan(
		Impl->FrameIndex, QueuedFrameCount,
		static_cast<u32>(Impl->SwapTextures.size()));
	if (!Plan)
		return;
	FImpl::FFrame& Frame = Impl->Frames[Plan->FrameContextIndex];
	Impl->CoreInterface.Wait(*Impl->FrameFence, Plan->ReuseFenceValue);
	Impl->CollectDeferredBuffers(
		Impl->CoreInterface.GetFenceValue(*Impl->FrameFence));
	Impl->CollectDeferredUiTextures(
		Impl->CoreInterface.GetFenceValue(*Impl->FrameFence));
	Impl->CollectDeferredPipelines(
		Impl->CoreInterface.GetFenceValue(*Impl->FrameFence));
	// Install completed CPU/DXC jobs only after this frame context is no
	// longer in flight. Per-frame material records prevent writes from racing
	// the other two queued contexts.
	Impl->PollSceneMaterialDependencies();
	Impl->PollMaterialPreviewCompiles();
	Impl->PollSceneMaterialCompiles();
	Impl->CoreInterface.ResetCommandAllocator(*Frame.Allocator);

	const u32 RecycledSemaphore = Plan->RecycledSemaphoreIndex;
	u32 TextureIndex = 0;
	const nri::Result AcquireResult = Impl->SwapChainInterface.AcquireNextTexture(
		*Impl->SwapChain,
		*Impl->SwapTextures[RecycledSemaphore].AcquireSemaphore,
		TextureIndex);
	if (AcquireResult == nri::Result::OUT_OF_DATE)
	{
		Impl->CoreInterface.QueueWaitIdle(Impl->GraphicsQueue);
		Impl->DestroySwapchain();
		Impl->CreateSwapchain();
		return;
	}
	if (!Impl->Check(AcquireResult,
		"Failed to acquire the NRI editor swapchain texture"))
	{
		return;
	}

	FImpl::FSwapTexture& Target = Impl->SwapTextures[TextureIndex];
	if (!Impl->Check(Impl->CoreInterface.BeginCommandBuffer(
			*Frame.CommandBuffer, nullptr),
			"Failed to begin the NRI editor command buffer"))
	{
		return;
	}
	const auto CpuFrameStart = std::chrono::steady_clock::now();
	Impl->Statistics.BeginFrame(Impl->FrameIndex);
	Impl->RecordViewportPasses(*Frame.CommandBuffer,
		Plan->FrameContextIndex);

	// During migration many editor panels still submit raw DX9 pointers as
	// user textures. Passing one to NRI as a Descriptor would be an invalid
	// GPU access. Keep text/font rendering alive and skip only unregistered
	// user-image draw commands until their owning panel moves to Tiramisu.
	for (int ListIndex = 0; ListIndex < DrawData.CmdListsCount; ++ListIndex)
	{
		ImDrawList* DrawList = DrawData.CmdLists[ListIndex];
		for (ImDrawCmd& Command : DrawList->CmdBuffer)
		{
			if (!Command.UserCallback && !Command.TexRef._TexData)
			{
				const ImTextureID TextureId = Command.TexRef._TexID;
				static_assert(sizeof(ImTextureID) == sizeof(std::uintptr_t));
				const std::uintptr_t DescriptorId =
					std::bit_cast<std::uintptr_t>(TextureId);
				if (TextureId == ImTextureID_Invalid ||
					!Impl->RegisteredUserTextures.Contains(DescriptorId))
				{
					Command.ElemCount = 0;
					// NRI resolves/binds the texture before it observes ElemCount.
					// Keep skipped legacy/null commands pointed at a valid
					// renderer-owned descriptor so validation never dereferences a
					// DX9 or null handle.
					Command.TexRef = ImTextureRef(static_cast<ImTextureID>(
						Impl->PreviewWhiteTextureDescriptor));
					++Impl->SkippedIncompatibleTextureCount;
				}
			}
			if (!Command.UserCallback && Command.ElemCount != 0)
				Impl->Statistics.RecordDraw(Command.ElemCount / 3);
		}
	}
	if (strstr(Core.Params, "-rdbg"))
	{
		static bool LoggedImguiTextures = false;
		if (!LoggedImguiTextures)
		{
			LoggedImguiTextures = true;
			Msg("* NRI ImGui textures: updates=%d, draw-lists=%d",
				DrawData.Textures ? DrawData.Textures->Size : 0,
				DrawData.CmdListsCount);
			if (DrawData.Textures)
			{
				for (ImTextureData* Texture : *DrawData.Textures)
					Msg("* NRI ImGui texture update: data=%p id=%llu status=%d size=%dx%d",
						Texture, static_cast<unsigned long long>(
							std::bit_cast<std::uintptr_t>(Texture->TexID)),
						static_cast<int>(Texture->Status), Texture->Width, Texture->Height);
			}
		}
	}

	nri::CopyImguiDataDesc CopyDesc = {};
	CopyDesc.drawLists = DrawData.CmdLists.Data;
	CopyDesc.drawListNum = static_cast<u32>(DrawData.CmdLists.Size);
	if (DrawData.Textures)
	{
		CopyDesc.textures = DrawData.Textures->Data;
		CopyDesc.textureNum = static_cast<u32>(DrawData.Textures->Size);
	}
	Impl->ImguiInterface.CmdCopyImguiData(
		*Frame.CommandBuffer, *Impl->Streamer, *Impl->Imgui, CopyDesc);

	nri::TextureBarrierDesc ToAttachment = {};
	ToAttachment.texture = Target.Texture;
	ToAttachment.before = {nri::AccessBits::NONE,
		Target.HasPresentState ? nri::Layout::PRESENT : nri::Layout::UNDEFINED,
		nri::StageBits::NONE};
	ToAttachment.after = {nri::AccessBits::COLOR_ATTACHMENT_WRITE,
		nri::Layout::COLOR_ATTACHMENT, nri::StageBits::COLOR_ATTACHMENT};
	ToAttachment.mipNum = 1;
	ToAttachment.layerNum = 1;
	nri::BarrierDesc Barrier = {};
	Barrier.textures = &ToAttachment;
	Barrier.textureNum = 1;
	Impl->CoreInterface.CmdBarrier(*Frame.CommandBuffer, Barrier);

	nri::AttachmentDesc Color = {};
	Color.descriptor = Target.ColorAttachment;
	Color.loadOp = nri::LoadOp::CLEAR;
	Color.clearValue.color.f = {0.025f, 0.025f, 0.035f, 1.0f};
	nri::RenderingDesc Rendering = {};
	Rendering.colors = &Color;
	Rendering.colorNum = 1;
	Impl->CoreInterface.CmdBeginRendering(*Frame.CommandBuffer, Rendering);
	Impl->CoreInterface.CmdBeginAnnotation(*Frame.CommandBuffer,
		"Editor.ImGui", nri::BGRA_UNUSED);
	Impl->Statistics.RecordPass();

	nri::DrawImguiDesc DrawDesc = {};
	DrawDesc.drawLists = DrawData.CmdLists.Data;
	DrawDesc.drawListNum = static_cast<u32>(DrawData.CmdLists.Size);
	DrawDesc.displaySize = {
		static_cast<nri::Dim_t>(DrawData.DisplaySize.x),
		static_cast<nri::Dim_t>(DrawData.DisplaySize.y)};
	DrawDesc.hdrScale = 1.0f;
	DrawDesc.attachmentFormat = Impl->SwapFormat;
	DrawDesc.linearColor = IsSrgb(Impl->SwapFormat);
	Impl->ImguiInterface.CmdDrawImgui(
		*Frame.CommandBuffer, *Impl->Imgui, DrawDesc);
	Impl->CoreInterface.CmdEndAnnotation(*Frame.CommandBuffer);
	Impl->CoreInterface.CmdEndRendering(*Frame.CommandBuffer);

	nri::TextureBarrierDesc ToPresent = {};
	ToPresent.texture = Target.Texture;
	ToPresent.before = ToAttachment.after;
	ToPresent.after = {nri::AccessBits::NONE, nri::Layout::PRESENT,
		nri::StageBits::NONE};
	ToPresent.mipNum = 1;
	ToPresent.layerNum = 1;
	Barrier.textures = &ToPresent;
	Impl->CoreInterface.CmdBarrier(*Frame.CommandBuffer, Barrier);
	if (!Impl->Check(Impl->CoreInterface.EndCommandBuffer(*Frame.CommandBuffer),
			"Failed to end the NRI editor command buffer"))
	{
		return;
	}

	nri::FenceSubmitDesc Wait = {
		Impl->SwapTextures[RecycledSemaphore].AcquireSemaphore,
		0, nri::StageBits::COLOR_ATTACHMENT};
	const nri::FenceSubmitDesc Signals[] = {
		{Target.ReleaseSemaphore, 0, nri::StageBits::ALL},
		{Impl->FrameFence, Plan->SignalFenceValue, nri::StageBits::ALL},
	};
	nri::CommandBuffer* CommandBuffer = Frame.CommandBuffer;
	nri::QueueSubmitDesc Submit = {};
	Submit.waitFences = &Wait;
	Submit.waitFenceNum = 1;
	Submit.commandBuffers = &CommandBuffer;
	Submit.commandBufferNum = 1;
	Submit.signalFences = Signals;
	Submit.signalFenceNum = static_cast<u32>(std::size(Signals));
	if (!Impl->Check(Impl->CoreInterface.QueueSubmit(
			*Impl->GraphicsQueue, Submit),
			"Failed to submit the NRI editor ImGui frame"))
	{
		return;
	}
	Target.HasPresentState = true;
	Impl->StreamerInterface.EndStreamerFrame(*Impl->Streamer);
	const nri::Result PresentResult = Impl->SwapChainInterface.QueuePresent(
		*Impl->SwapChain, *Target.ReleaseSemaphore);
	if (PresentResult == nri::Result::OUT_OF_DATE)
	{
		Impl->CoreInterface.QueueWaitIdle(Impl->GraphicsQueue);
		Impl->DestroySwapchain();
		Impl->CreateSwapchain();
	}
	else
	{
		Impl->Check(PresentResult, "Failed to present the NRI editor frame");
	}
	Impl->Statistics.SetResources(Impl->CollectResourceStatistics());
	const auto CpuFrameEnd = std::chrono::steady_clock::now();
	Impl->Statistics.EndFrame(static_cast<u64>(
		std::chrono::duration_cast<std::chrono::nanoseconds>(
			CpuFrameEnd - CpuFrameStart).count()));
	++Impl->FrameIndex;
}

void TiramisuEditorRenderBridge::InvalidateDeviceObjects()
{
	// Swapchain lifetime is managed from RenderDrawData/Shutdown.
}

void TiramisuEditorRenderBridge::CreateDeviceObjects()
{
	// Swapchain recreation is lazy and uses the current SDL pixel size.
}

EEditorRenderBackendKind TiramisuEditorRenderBridge::GetKind() const noexcept
{
	return EEditorRenderBackendKind::Tiramisu;
}

void TiramisuEditorRenderBridge::CaptureViewport(const u32 ViewportId)
{
	Impl->Viewports[ViewportId].CaptureRequested = true;
}

void TiramisuEditorRenderBridge::ResizeViewport(const u32 ViewportId,
	const u32 Width, const u32 Height)
{
	FImpl::FViewport& Viewport = Impl->Viewports[ViewportId];
	Viewport.DesiredWidth = Width;
	Viewport.DesiredHeight = Height;
}

bool TiramisuEditorRenderBridge::SubmitViewportScene(const u32 ViewportId,
	const FEditorViewportSceneSnapshot& Snapshot)
{
	FImpl::FViewport& Viewport = Impl->Viewports[ViewportId];
	xr_string SubmitDiagnostic;
	if (!Viewport.SceneMailbox->Submit(Snapshot, &SubmitDiagnostic))
	{
		Impl->Diagnostic = std::move(SubmitDiagnostic);
		return false;
	}
	Viewport.ScenePicker->Submit(Snapshot);
	return true;
}

FEditorViewportPickResult TiramisuEditorRenderBridge::PickViewport(
	const u32 ViewportId,
	const FEditorViewportPickRequest& Request) const
{
	const auto Found = Impl->Viewports.find(ViewportId);
	return Found == Impl->Viewports.end()
		? FEditorViewportPickResult{} : Found->second.ScenePicker->Pick(Request);
}

FEditorViewportSurface TiramisuEditorRenderBridge::GetViewportSurface(
	const u32 ViewportId) const
{
	FEditorViewportSurface Surface;
	const auto It = Impl->Viewports.find(ViewportId);
	if (It == Impl->Viewports.end())
		return Surface;
	const FImpl::FViewport& Viewport = It->second;
	Surface.ImGuiTextureId = Viewport.ShaderResource;
	Surface.Width = Viewport.Width;
	Surface.Height = Viewport.Height;
	return Surface;
}

void TiramisuEditorRenderBridge::CopyViewportOverlayText(
	const u32 ViewportId,
	xr_vector<FEditorOverlayText>& OutText) const
{
	OutText.clear();
	const auto Viewport = Impl->Viewports.find(ViewportId);
	if (Viewport == Impl->Viewports.end())
		return;
	OutText = Viewport->second.ScenePacket.OverlayText;
}

FEditorTextureHandle TiramisuEditorRenderBridge::CreateTexture(
	const FEditorTextureUpload& Upload)
{
	xr_string SubmitDiagnostic;
	const FEditorTextureHandle Handle =
		Impl->UiTextureMailbox.Create(Upload, &SubmitDiagnostic);
	if (!Handle.IsValid())
		Impl->Diagnostic = std::move(SubmitDiagnostic);
	return Handle;
}

bool TiramisuEditorRenderBridge::UpdateTexture(const FEditorTextureHandle Handle,
	const FEditorTextureUpload& Upload)
{
	xr_string SubmitDiagnostic;
	if (!Impl->UiTextureMailbox.Update(Handle, Upload, &SubmitDiagnostic))
	{
		Impl->Diagnostic = std::move(SubmitDiagnostic);
		return false;
	}
	return true;
}

void TiramisuEditorRenderBridge::DestroyTexture(const FEditorTextureHandle Handle)
{
	(void)Impl->UiTextureMailbox.Destroy(Handle);
}

FEditorViewportSurface TiramisuEditorRenderBridge::GetTextureSurface(
	const FEditorTextureHandle Handle) const
{
	FEditorViewportSurface Surface;
	if (!Impl->UiTextureMailbox.IsAlive(Handle))
		return Surface;
	std::scoped_lock Lock(Impl->UiTexturesMutex);
	const auto It = Impl->UiTextures.find(TextureHandleKey(Handle));
	if (It == Impl->UiTextures.end())
		return Surface;
	Surface.ImGuiTextureId = It->second.ShaderResource;
	Surface.Width = It->second.Width;
	Surface.Height = It->second.Height;
	return Surface;
}

FRenderStatisticsSnapshot TiramisuEditorRenderBridge::GetRenderStatistics()
	const noexcept
{
	return Impl->Statistics.GetSnapshot();
}

bool TiramisuEditorRenderBridge::IsAvailable() const noexcept
{
	return Impl->Initialized && Impl->PreviewPipelineLayout != nullptr;
}

FMaterialPreviewHandle TiramisuEditorRenderBridge::CreatePreview()
{
	if (!IsAvailable())
		return {};
	return Impl->CreateMaterialPreview();
}

void TiramisuEditorRenderBridge::DestroyPreview(
	const FMaterialPreviewHandle Handle)
{
	FImpl::FMaterialPreview* Preview = Impl->FindPreview(Handle);
	if (!Preview)
		return;
	if (Impl->GraphicsQueue)
		Impl->CoreInterface.QueueWaitIdle(Impl->GraphicsQueue);
	const auto Viewport = Impl->Viewports.find(Preview->ViewportId);
	if (Viewport != Impl->Viewports.end())
	{
		Impl->DestroyViewport(Viewport->second);
		Impl->Viewports.erase(Viewport);
	}
	const auto Mesh = Impl->GpuMeshes.find(Preview->MeshId.Value);
	if (Mesh != Impl->GpuMeshes.end())
	{
		if (Mesh->second.Buffer)
			Impl->CoreInterface.DestroyBuffer(Mesh->second.Buffer);
		Impl->GpuMeshes.erase(Mesh);
	}
	if (Preview->Pipeline)
		Impl->CoreInterface.DestroyPipeline(Preview->Pipeline);
	Preview->Pipeline = nullptr;
	Preview->Alive = false;
	Preview->State = EMaterialPreviewState::Unavailable;
	Preview->Diagnostic.clear();
	Preview->ParameterData.clear();
	Preview->ParameterLayoutHash = 0;
	Preview->DrawFlags = PreviewWhiteCubeDescriptorIndex;
	Preview->Generation = Preview->Generation ==
		std::numeric_limits<u32>::max()
		? 1 : Preview->Generation + 1;
	Impl->FreeMaterialPreviewSlots.push_back(Handle.Index);
}

void TiramisuEditorRenderBridge::UpdatePreview(
	const FMaterialPreviewHandle Handle, const FMaterialPreviewSource& Source)
{
	if (IsAvailable())
		Impl->QueueMaterialPreviewCompile(Handle, Source);
}

void TiramisuEditorRenderBridge::ResizePreview(
	const FMaterialPreviewHandle Handle,
	const u32 Width, const u32 Height)
{
	FImpl::FMaterialPreview* Preview = Impl->FindPreview(Handle);
	if (!Preview)
		return;
	FImpl::FViewport& Viewport = Impl->Viewports[Preview->ViewportId];
	Viewport.DesiredWidth = std::max(1u, Width);
	Viewport.DesiredHeight = std::max(1u, Height);
}

void TiramisuEditorRenderBridge::RenderPreview(
	const FMaterialPreviewHandle Handle, const float DeltaSeconds)
{
	(void)DeltaSeconds;
	FImpl::FMaterialPreview* Preview = Impl->FindPreview(Handle);
	if (!Preview)
		return;
	Impl->Viewports[Preview->ViewportId].CaptureRequested = true;
}

FMaterialPreviewFrame TiramisuEditorRenderBridge::GetPreviewFrame(
	const FMaterialPreviewHandle Handle) const
{
	FMaterialPreviewFrame Frame;
	const FImpl::FMaterialPreview* Preview = Impl->FindPreview(Handle);
	if (!Preview)
	{
		Frame.Diagnostic = "Material preview handle is stale or unavailable";
		return Frame;
	}
	Frame.State = Preview->State;
	Frame.RequestedRevision = Preview->RequestedRevision;
	Frame.AcceptedRevision = Preview->AcceptedRevision;
	Frame.PipelineKey = Preview->PipelineKey;
	Frame.UsingLastGoodPipeline = Preview->Pipeline != nullptr &&
		Preview->AcceptedRevision != 0 &&
		Preview->AcceptedRevision < Preview->RequestedRevision;
	Frame.Backend = Impl->Api == ETiramisuEditorGraphicsApi::D3D12
		? "D3D12/DXIL" : "Vulkan/SPIR-V";
	Frame.RenderPass = "MaterialPreview";
	Frame.VertexFactory = "MaterialLevelStatic";
	Frame.Diagnostic = Preview->Diagnostic;
	const auto Viewport = Impl->Viewports.find(Preview->ViewportId);
	if (Viewport != Impl->Viewports.end())
	{
		Frame.Surface.ImGuiTextureId = Viewport->second.ShaderResource;
		Frame.Surface.Width = Viewport->second.Width;
		Frame.Surface.Height = Viewport->second.Height;
	}
	return Frame;
}

void TiramisuEditorRenderBridge::RegisterImguiTexture(void* ShaderResourceDescriptor)
{
	if (ShaderResourceDescriptor)
		Impl->RegisteredUserTextures.Register(ShaderResourceDescriptor);
}

void TiramisuEditorRenderBridge::UnregisterImguiTexture(void* ShaderResourceDescriptor)
{
	if (ShaderResourceDescriptor)
		Impl->RegisteredUserTextures.Unregister(ShaderResourceDescriptor);
}

u32 TiramisuEditorRenderBridge::GetSkippedIncompatibleTextureCount() const noexcept
{
	return Impl->SkippedIncompatibleTextureCount;
}

FEditorViewportMaterialStatus TiramisuEditorRenderBridge::GetViewportMaterialStatus(
	const u32 ViewportId,
	const FEditorMaterialSlotId MaterialSlot) const
{
	FEditorViewportMaterialStatus Status;
	const auto Viewport = Impl->Viewports.find(ViewportId);
	const auto Material = Impl->SceneMaterials.find(MaterialSlot.Value);
	if (Viewport == Impl->Viewports.end() || Material == Impl->SceneMaterials.end())
		return Status;
	Status.RequestedRevision = Material->second.RequestedRevision;
	Status.AcceptedRevision = Material->second.AcceptedRevision;
	Status.PipelineKey = Material->second.PipelineKey;
	if (Material->second.Pipeline && Material->second.PipelineKey != 0)
	{
		const FImpl::FScenePipelineCacheKey CacheKey = {
			Material->second.PipelineKey,
			Material->second.PipelineTwoSided};
		if (const auto Cached = Impl->ScenePipelineCache.find(CacheKey);
			Cached != Impl->ScenePipelineCache.end())
		{
			Status.SharedPipelineReferenceCount =
				Cached->second.ReferenceCount;
		}
	}
	Status.DrawCount = Viewport->second.SceneDrawCount;
	Status.SelectionDrawCount = Viewport->second.SceneSelectionDrawCount;
	Status.SelectionOverlayReady = Impl->SceneSelectionPipeline != nullptr &&
		Status.SelectionDrawCount != 0;
	Status.DebugLineCount = Viewport->second.DebugLineVertexCount / 2;
	Status.DebugTriangleCount = Viewport->second.DebugTriangleVertexCount / 3;
	Status.OverlayLineCount = Viewport->second.OverlayLineVertexCount / 2;
	Status.OverlayTriangleCount =
		Viewport->second.OverlayTriangleVertexCount / 3;
	Status.OverlayTextCount = static_cast<u32>(
		Viewport->second.ScenePacket.OverlayText.size());
	Status.LightCount = std::min<u32>(
		static_cast<u32>(
			Viewport->second.ScenePacket.Lights.size()),
		MaxSceneLightsPerViewport);
	Status.DebugOverlayReady = Viewport->second.DebugDrawBuffer != nullptr &&
		(Status.DebugLineCount == 0 || Impl->SceneDebugLinePipeline != nullptr) &&
		(Status.DebugTriangleCount == 0 ||
			Impl->SceneDebugTrianglePipeline != nullptr) &&
		(Status.DebugLineCount != 0 || Status.DebugTriangleCount != 0);
	Status.ScreenOverlayReady = Viewport->second.DebugDrawBuffer != nullptr &&
		(Status.OverlayLineCount == 0 ||
			Impl->SceneOverlayLinePipeline != nullptr) &&
		(Status.OverlayTriangleCount == 0 ||
			Impl->SceneOverlayTrianglePipeline != nullptr) &&
		(Status.OverlayLineCount != 0 || Status.OverlayTriangleCount != 0);
	Status.ReloadCount = Material->second.ReloadCount;
	Status.Diagnostic = Material->second.Diagnostic;
	Status.Ready = Material->second.Pipeline != nullptr &&
		Status.AcceptedRevision != 0 && Status.PipelineKey != 0 &&
		Status.DrawCount != 0;
	return Status;
}

xr_string_view TiramisuEditorRenderBridge::GetLastDiagnostic() const noexcept
{
	return Impl->Diagnostic;
}
