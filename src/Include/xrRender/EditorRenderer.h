#pragma once

#include "../../xrCore/xrCore.h"
#include "../../xrCore/RenderStatistics.h"

#include <array>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <span>
#include <string>
#include <string_view>
#include <vector>

enum class EEditorRenderBackendKind : u8
{
	Legacy,
	Tiramisu
};

// Opaque presentation data consumed by ImGui. Renderer API objects remain
// owned by the selected editor backend and never cross this boundary.
struct FEditorViewportSurface
{
	void* ImGuiTextureId = nullptr;
	u32 Width = 0;
	u32 Height = 0;
	u64 Revision = 0;

	[[nodiscard]] bool IsValid() const noexcept
	{
		return ImGuiTextureId != nullptr;
	}
};

enum class EEditorTextureFormat : u8
{
	Rgba8Unorm,
	Rgba8Srgb,
	Bgra8Unorm,
	Bgra8Srgb
};

struct FEditorTextureHandle
{
	static constexpr u32 InvalidIndex =
		std::numeric_limits<u32>::max();

	u32 Index = InvalidIndex;
	u32 Generation = 0;

	[[nodiscard]] bool IsValid() const noexcept
	{
		return Index != InvalidIndex && Generation != 0;
	}

	friend bool operator==(const FEditorTextureHandle&, const FEditorTextureHandle&) = default;
};

// Pixels are valid only for the duration of CreateTexture/UpdateTexture. The
// backend owns a copy before returning, while API resources and ImGui
// descriptors stay private to the backend. Revision is monotonically
// increasing for one handle; equal revisions are coalesced.
struct FEditorTextureUpload
{
	u32 Width = 0;
	u32 Height = 0;
	u32 RowPitch = 0;
	EEditorTextureFormat Format = EEditorTextureFormat::Rgba8Unorm;
	xr_span<const std::byte> Pixels;
	u64 Revision = 0;
	xr_string_view DebugName;
};

struct FEditorStaticMeshId
{
	u64 Value = 0;

	[[nodiscard]] bool IsValid() const noexcept { return Value != 0; }
	friend bool operator==(const FEditorStaticMeshId&, const FEditorStaticMeshId&) = default;
};

struct FEditorSceneObjectId
{
	u64 Value = 0;

	[[nodiscard]] bool IsValid() const noexcept { return Value != 0; }
	friend bool operator==(const FEditorSceneObjectId&, const FEditorSceneObjectId&) = default;
};

struct FEditorMaterialSlotId
{
	u64 Value = 0;

	[[nodiscard]] bool IsValid() const noexcept { return Value != 0; }
	friend bool operator==(const FEditorMaterialSlotId&, const FEditorMaterialSlotId&) = default;
};

enum class EEditorMaterialSlotFlags : u32
{
	None = 0,
	TwoSided = 1u << 0
};

// Complete material-source description for one visible scene snapshot. The
// strings are valid only for SubmitViewportScene; a renderer must copy them
// before returning. MaterialSlot is stable for identical legacy source data
// and is later replaced by a static-mesh material asset reference in the new
// scene format.
struct FEditorMaterialSlotSource
{
	FEditorMaterialSlotId MaterialSlot;
	xr_string_view ShaderName;
	xr_string_view TextureName;
	xr_string_view SurfaceName;
	EEditorMaterialSlotFlags Flags = EEditorMaterialSlotFlags::None;
	// New static-mesh assets provide a master/instance GUID or path directly.
	// Empty keeps the legacy shaders.xr -> legacy-map.json migration route.
	xr_string_view MaterialAsset;
};

struct FEditorStaticMeshVertex
{
	xr_array<float, 3> Position = {};
	xr_array<float, 3> Normal = {0.0f, 1.0f, 0.0f};
	xr_array<float, 4> Tangent = {1.0f, 0.0f, 0.0f, 1.0f};
	xr_array<float, 2> TexCoord = {};
	xr_array<float, 2> TexCoord1 = {};
	u32 Color = 0xffffffffu;
};

struct FEditorStaticMeshSection
{
	u32 FirstIndex = 0;
	u32 IndexCount = 0;
	FEditorMaterialSlotId MaterialSlot;
};

// Source arrays are valid only for the duration of SubmitViewportScene. The
// backend must copy changed data before returning and may upload it later on
// its render thread. Revision is scoped to MeshId and must change whenever any
// vertex, index or section data changes.
struct FEditorStaticMeshUpload
{
	FEditorStaticMeshId MeshId;
	u64 Revision = 0;
	xr_span<const FEditorStaticMeshVertex> Vertices;
	xr_span<const u32> Indices;
	xr_span<const FEditorStaticMeshSection> Sections;
};

enum class EEditorSceneInstanceFlags : u32
{
	None = 0,
	Selected = 1u << 0,
	TwoSided = 1u << 1,
	DepthBias = 1u << 2
};

// Renderer-neutral состояние presentation и публикации editor surfaces.
// Счётчики позволяют smoke-тесту доказать пересоздание ресурсов без выпуска
// NRI/D3D/Vulkan объектов за границу xrRenderTiramisu.
struct FEditorRenderLifecycleStatus
{
	bool PresentationReady = false;
	bool DedicatedRenderThreadActive = false;
	u32 PresentationWidth = 0;
	u32 PresentationHeight = 0;
	u64 RenderExecutionThreadId = 0;
	u64 SwapchainRevision = 0;
	u64 PresentedFrameCount = 0;
	u64 ViewportResourceRevision = 0;
	u64 ImGuiTextureRedirectCount = 0;
};

// Per-component replacement of a static-mesh material slot. Both IDs must be
// present in the snapshot material table. This mirrors a UE static-mesh
// component override without duplicating the geometry asset.
struct FEditorMaterialSlotOverride
{
	FEditorMaterialSlotId BaseMaterialSlot;
	FEditorMaterialSlotId MaterialSlot;
};

struct FEditorStaticMeshInstance
{
	FEditorSceneObjectId ObjectId;
	FEditorStaticMeshId MeshId;
	xr_array<float, 16> LocalToWorld = {
		1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f
	};
	EEditorSceneInstanceFlags Flags = EEditorSceneInstanceFlags::None;
	xr_vector<FEditorMaterialSlotOverride> MaterialOverrides;
};

enum class EEditorDecalInstanceFlags : u32
{
	None = 0,
	Selected = 1u << 0
};

inline constexpr size_t EditorViewportMaxDecalCount = 4096;

// Проекционная декаль в renderer-neutral scene packet. LocalToWorld переводит
// канонический объём [-0.5, 0.5] в мир; материал обязан иметь domain Decal.
// Геометрия старого Wallmark через этот контракт не передаётся.
struct FEditorDecalInstance
{
	FEditorSceneObjectId ObjectId;
	FEditorMaterialSlotId MaterialSlot;
	xr_array<float, 16> LocalToWorld = {
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
	};
	s32 SortOrder = 0;
	EEditorDecalInstanceFlags Flags = EEditorDecalInstanceFlags::None;
};

// Renderer-neutral ссылка на standalone OGF. LevelEditor не создаёт
// IRenderVisual и не разбирает геометрию: xrRenderTiramisu загружает ассет,
// кэширует его draw-parts и создаёт GPU-ресурсы на render thread.
struct FEditorModelInstance
{
	FEditorSceneObjectId ObjectId;
	xr_string_view AssetName;
	// Необязательный motion из legacy Spawn. Renderer самостоятельно
	// разрешает его после загрузки skeleton/motions OGF.
	xr_string_view AnimationName;
	xr_array<float, 16> LocalToWorld = {
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
	};
	EEditorSceneInstanceFlags Flags =
		EEditorSceneInstanceFlags::None;
};

enum class EEditorSceneLightType : u8
{
	Directional,
	Point,
	Spot
};

enum class EEditorSceneLightFlags : u32
{
	None = 0,
	Selected = 1u << 0,
	CastShadows = 1u << 1
};

inline constexpr size_t EditorViewportMaxLightCount = 64;

// Renderer-neutral light data. Local +Z is the emitted-light direction for
// directional and spot lights. The source array is valid only during
// SubmitViewportScene and is copied before the producer returns.
struct FEditorSceneLight
{
	FEditorSceneObjectId ObjectId;
	EEditorSceneLightType Type = EEditorSceneLightType::Point;
	xr_array<float, 16> LocalToWorld = {
		1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f
	};
	xr_array<float, 3> Color = {1.0f, 1.0f, 1.0f};
	float Intensity = 1.0f;
	float Range = 10.0f;
	float InnerConeAngleDegrees = 20.0f;
	float OuterConeAngleDegrees = 45.0f;
	EEditorSceneLightFlags Flags = EEditorSceneLightFlags::None;
};

enum class EEditorParticleAssetType : u8
{
	Effect,
	Group,
	AnimationCurve
};

enum class EEditorParticleInstanceFlags : u32
{
	None = 0,
	Selected = 1u << 0,
	Playing = 1u << 1
};

// Renderer-neutral instance legacy particle asset. Симуляция, текстуры и
// GPU-ресурсы принадлежат xrRenderTiramisu; LevelEditor передаёт только
// стабильную ссылку на каталог, transform и состояние authoring controls.
struct FEditorParticleInstance
{
	FEditorSceneObjectId ObjectId;
	xr_string_view AssetName;
	EEditorParticleAssetType AssetType =
		EEditorParticleAssetType::Effect;
	xr_array<float, 16> LocalToWorld = {
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
	};
	EEditorParticleInstanceFlags Flags =
		EEditorParticleInstanceFlags::None;
};

[[nodiscard]] inline FEditorMaterialSlotId ResolveEditorMaterialSlot(
	const FEditorStaticMeshInstance& Instance,
	const FEditorMaterialSlotId BaseMaterialSlot
) noexcept
{
	for (const FEditorMaterialSlotOverride& Override :
		 Instance.MaterialOverrides)
	{
		if (Override.BaseMaterialSlot == BaseMaterialSlot)
		{
			return Override.MaterialSlot;
		}
	}
	return BaseMaterialSlot;
}

struct FEditorViewportCamera
{
	xr_array<float, 16> View = {};
	xr_array<float, 16> Projection = {};
	xr_array<float, 16> ViewProjection = {};
	xr_array<float, 3> WorldPosition = {};
	float NearPlane = 0.1f;
	float FarPlane = 1000.0f;
};

// Depth-tested world-space primitives used by editor diagnostics and gizmos.
// Colors are linear RGBA values. The source arrays are valid only for the
// duration of SubmitViewportScene and must be copied by the backend.
struct FEditorDebugVertex
{
	xr_array<float, 3> Position = {};
	xr_array<float, 4> Color = {1.0f, 1.0f, 1.0f, 1.0f};
};

struct FEditorDebugLine
{
	xr_array<FEditorDebugVertex, 2> Vertices;
};

struct FEditorDebugTriangle
{
	xr_array<FEditorDebugVertex, 3> Vertices;
};

// Screen-space primitives use normalized device coordinates: X/Y are in
// [-1, 1], the origin is the viewport centre and positive Y points up. They
// are rendered after world-space debug draw without depth testing.
struct FEditorOverlayVertex
{
	xr_array<float, 3> Position = {};
	xr_array<float, 4> Color = {1.0f, 1.0f, 1.0f, 1.0f};
};

struct FEditorOverlayLine
{
	xr_array<FEditorOverlayVertex, 2> Vertices;
};

struct FEditorOverlayTriangle
{
	xr_array<FEditorOverlayVertex, 3> Vertices;
};

// Text is composited by the editor UI over the renderer-owned viewport image.
// Position is the shadow baseline in NDC; the main glyphs use the legacy
// one-pixel upper-left offset. Owning the string makes capture safe for labels
// produced from temporary buffers during scene traversal.
struct FEditorOverlayText
{
	xr_array<float, 2> Position = {};
	xr_array<float, 4> Color = {1.0f, 1.0f, 1.0f, 1.0f};
	xr_array<float, 4> ShadowColor = {0.0f, 0.0f, 0.0f, 1.0f};
	xr_string Text;
};

// StaticMeshes contains the complete set of changed mesh assets for this
// snapshot. Instances is the complete visible instance list. RemovedStaticMeshes
// allows the backend to release cached GPU assets without renderer-specific
// handles leaking into LevelEditor. Debug and overlay spans are complete lists
// and DebugDrawRevision changes whenever any of these lists changes.
struct FEditorViewportSceneSnapshot
{
	FEditorViewportCamera Camera;
	xr_span<const FEditorMaterialSlotSource> MaterialSlots;
	xr_span<const FEditorStaticMeshUpload> StaticMeshes;
	xr_span<const FEditorStaticMeshId> RemovedStaticMeshes;
	xr_span<const FEditorStaticMeshInstance> Instances;
	xr_span<const FEditorDecalInstance> DecalInstances;
	xr_span<const FEditorModelInstance> ModelInstances;
	xr_span<const FEditorSceneLight> Lights;
	xr_span<const FEditorParticleInstance> ParticleInstances;
	xr_span<const FEditorDebugLine> DebugLines;
	xr_span<const FEditorDebugTriangle> DebugTriangles;
	xr_span<const FEditorOverlayLine> OverlayLines;
	xr_span<const FEditorOverlayTriangle> OverlayTriangles;
	xr_span<const FEditorOverlayText> OverlayText;
	u64 DebugDrawRevision = 0;
	u64 Revision = 0;
};

struct FEditorViewportPickRequest
{
	xr_array<float, 3> RayOrigin = {};
	xr_array<float, 3> RayDirection = {0.0f, 0.0f, 1.0f};
	float MaxDistance = std::numeric_limits<float>::max();
	bool CullBackFaces = false;
};

struct FEditorViewportPickResult
{
	bool Hit = false;
	FEditorSceneObjectId ObjectId;
	FEditorStaticMeshId MeshId;
	FEditorMaterialSlotId MaterialSlot;
	xr_array<float, 3> WorldPosition = {};
	xr_array<float, 3> WorldNormal = {};
	float Distance = std::numeric_limits<float>::max();
	u32 TriangleIndex = 0;
	u64 SceneRevision = 0;
};

// Диагностический снимок material path одного viewport. Структура не содержит
// NRI-объектов и используется редактором и deterministic smoke-тестами.
struct FEditorViewportMaterialStatus
{
	bool Ready = false;
	bool SelectionOverlayReady = false;
	bool DebugOverlayReady = false;
	bool ScreenOverlayReady = false;
	bool ParticleBillboardReady = false;
	bool DecalReady = false;
	bool ModelPickingReady = false;
	bool ModelAnimationReady = false;
	bool ModelSkinningReady = false;
	bool ModelPaletteChanged = false;
	u64 RequestedRevision = 0;
	u64 AcceptedRevision = 0;
	u64 PipelineKey = 0;
	u32 SharedPipelineReferenceCount = 0;
	u32 DrawCount = 0;
	u32 DecalInstanceCount = 0;
	u32 DecalDrawCount = 0;
	u32 DecalCulledCount = 0;
	u32 SelectionDrawCount = 0;
	u32 DebugLineCount = 0;
	u32 DebugTriangleCount = 0;
	u32 OverlayLineCount = 0;
	u32 OverlayTriangleCount = 0;
	u32 OverlayTextCount = 0;
	u32 LightCount = 0;
	u32 ModelInstanceCount = 0;
	u32 ModelDrawCount = 0;
	u32 PendingModelLoadCount = 0;
	u32 AnimatedModelCount = 0;
	u32 SkinnedModelCount = 0;
	u32 GpuSkinnedModelCount = 0;
	u32 ModelPaletteMatrixCount = 0;
	u32 UploadedSkinningMatrixCount = 0;
	u32 ParticleInstanceCount = 0;
	u32 ParticleGroupInstanceCount = 0;
	u32 ParticleChildInstanceCount = 0;
	u32 SimulatedParticleCount = 0;
	u32 ParticleBillboardCount = 0;
	u32 ParticleBillboardDrawCount = 0;
	u32 ReloadCount = 0;
	xr_string Diagnostic;
};

// Копируемая запись renderer-owned библиотеки частиц. Имена зависимостей
// содержат эффекты, используемые группой, либо animation curves эффекта.
// Legacy PS-типы и shader handles через эту границу не передаются.
struct FEditorParticleAssetInfo
{
	xr_string Name;
	EEditorParticleAssetType Type = EEditorParticleAssetType::Effect;
	xr_string ShaderName;
	xr_string TextureName;
	xr_vector<xr_string> Dependencies;
	u32 MaxParticles = 0;
	u32 GroupEntryCount = 0;
	u32 EnabledGroupEntryCount = 0;
	u32 GroupChildCallbackCount = 0;
	bool HasCompiledActions = false;
};

// Цельный снимок каталога. Revision детерминированно вычисляется из данных,
// поэтому UI может не перестраивать списки при неизменившейся библиотеке.
struct FEditorParticleLibrarySnapshot
{
	xr_vector<FEditorParticleAssetInfo> Assets;
	u64 Revision = 0;
	xr_string Diagnostic;

	[[nodiscard]] bool IsReady() const noexcept
	{
		return Revision != 0;
	}
};

// Контракт передачи editor scene в выбранный renderer без NRI/D3D/Vulkan типов.
// Все входные span/string_view копируются реализацией до возврата из вызова.
class IEditorRenderBackend
{
public:
	virtual ~IEditorRenderBackend() = default;

	[[nodiscard]] virtual EEditorRenderBackendKind GetKind() const noexcept = 0;
	virtual void CaptureViewport(u32 ViewportId) = 0;
	virtual void ResizeViewport(u32 ViewportId, u32 Width, u32 Height) = 0;
	virtual bool SubmitViewportScene(u32 ViewportId, const FEditorViewportSceneSnapshot& Snapshot) = 0;
	[[nodiscard]] virtual FEditorViewportPickResult PickViewport(
		u32 ViewportId,
		const FEditorViewportPickRequest& Request
	) const = 0;
	[[nodiscard]] virtual FEditorViewportSurface GetViewportSurface(
		u32 ViewportId
	) const = 0;
	virtual void CopyViewportOverlayText(u32 ViewportId, xr_vector<FEditorOverlayText>& OutText) const = 0;

	// These calls are producer-thread safe. GPU creation, upload and deferred
	// deletion are performed by the backend consumer/render thread.
	[[nodiscard]] virtual FEditorTextureHandle CreateTexture(
		const FEditorTextureUpload& Upload
	) = 0;
	virtual bool UpdateTexture(FEditorTextureHandle Handle, const FEditorTextureUpload& Upload) = 0;
	virtual void DestroyTexture(FEditorTextureHandle Handle) = 0;
	[[nodiscard]] virtual FEditorViewportSurface GetTextureSurface(
		FEditorTextureHandle Handle
	) const = 0;
	[[nodiscard]] virtual FRenderStatisticsSnapshot GetRenderStatistics()
		const noexcept
	{
		return {};
	}
	[[nodiscard]] virtual FEditorRenderLifecycleStatus
	GetRenderLifecycleStatus() const noexcept
	{
		return {};
	}
	// Вызывается после остановки общего renderer. Нужен backend, который
	// создал shared device до появления render thread и должен удалить его
	// только после освобождения всех renderer-owned ресурсов.
	virtual void FinalizeRendererShutdown()
	{
	}
	// После bootstrap shared device общий renderer уже может выполнять команды
	// в выделенном потоке. Editor backend создаёт свои GPU-ресурсы здесь.
	[[nodiscard]] virtual bool InitializeRendererResources()
	{
		return true;
	}
	[[nodiscard]] virtual bool IsAvailable() const noexcept
	{
		return true;
	}
	[[nodiscard]] virtual FEditorViewportMaterialStatus
	GetViewportMaterialStatus(u32 ViewportId, FEditorMaterialSlotId MaterialSlot) const
	{
		return {};
	}
	[[nodiscard]] virtual xr_string_view GetLastDiagnostic() const noexcept
	{
		return {};
	}

	// Перезагрузка и хранение библиотеки выполняются renderer module. Вызов
	// копирования не выпускает наружу ссылки на внутренние asset records.
	[[nodiscard]] virtual bool ReloadParticleLibrary()
	{
		return false;
	}
	virtual void CopyParticleLibrary(
		FEditorParticleLibrarySnapshot& OutSnapshot
	) const
	{
		OutSnapshot = {};
	}
};

enum class EMaterialPreviewPrimitive : u8
{
	Sphere,
	Cube,
	Plane
};

enum class EMaterialPreviewState : u8
{
	Unavailable,
	Compiling,
	Ready,
	Error
};

struct FMaterialPreviewHandle
{
	static constexpr u32 InvalidIndex =
		std::numeric_limits<u32>::max();

	u32 Index = InvalidIndex;
	u32 Generation = 0;

	[[nodiscard]] bool IsValid() const noexcept
	{
		return Index != InvalidIndex && Generation != 0;
	}

	friend bool operator==(const FMaterialPreviewHandle&, const FMaterialPreviewHandle&) = default;
};

// All string views are valid only for the duration of UpdatePreview. A backend
// must copy any source data that it needs on its compiler or render threads.
struct FMaterialPreviewSource
{
	xr_string_view MaterialAssetId;
	xr_string_view MaterialJson;
	xr_string_view MaterialInstanceJson;
	xr_string_view GeneratedHlsl;
	xr_string_view Environment;
	EMaterialPreviewPrimitive Primitive = EMaterialPreviewPrimitive::Sphere;
	u64 Revision = 0;
};

// Diagnostic storage remains owned by the backend and is valid until its next
// preview operation. Renderer resources never cross this interface.
struct FMaterialPreviewFrame
{
	FEditorViewportSurface Surface;
	EMaterialPreviewState State = EMaterialPreviewState::Unavailable;
	u64 RequestedRevision = 0;
	u64 AcceptedRevision = 0;
	u64 PipelineKey = 0;
	bool UsingLastGoodPipeline = false;
	xr_string_view Backend;
	xr_string_view RenderPass;
	xr_string_view VertexFactory;
	xr_string_view Diagnostic;
};

// Узкий интерфейс material preview; GPU-типы и lifetime ресурсов остаются
// внутри renderer DLL.
class IMaterialPreviewRenderer
{
public:
	virtual ~IMaterialPreviewRenderer() = default;

	[[nodiscard]] virtual bool IsAvailable() const noexcept = 0;
	[[nodiscard]] virtual FMaterialPreviewHandle CreatePreview() = 0;
	virtual void DestroyPreview(FMaterialPreviewHandle Handle) = 0;
	virtual void UpdatePreview(FMaterialPreviewHandle Handle, const FMaterialPreviewSource& Source) = 0;
	virtual void ResizePreview(FMaterialPreviewHandle Handle, u32 Width, u32 Height) = 0;
	virtual void RenderPreview(FMaterialPreviewHandle Handle, float DeltaSeconds) = 0;
	[[nodiscard]] virtual FMaterialPreviewFrame GetPreviewFrame(
		FMaterialPreviewHandle Handle
	) const = 0;
};
