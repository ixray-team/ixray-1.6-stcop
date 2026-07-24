#pragma once

#include "../../../xrCore/xrCore.h"

#include "../../../Include/xrRender/TiramisuEditorRendererFactory.h"

#include <memory>
#include <string>
#include <string_view>

struct SDL_Window;

// Связывает LevelEditor с editor-проходами основного xrRenderTiramisu.
// Владеет только ресурсами конкретных editor surfaces; NRI device, очереди
// и streamer получает у общего TiramisuRenderDevice.
class TiramisuEditorRenderBridge final : public IXrUIRendererBackend, public IEditorRenderBackend, public IMaterialPreviewRenderer
{
public:
	// Создаёт bridge для окна редактора и выбранного renderer API.
	TiramisuEditorRenderBridge(SDL_Window* Window, ETiramisuEditorGraphicsApi Api, const FRenderDeterministicTestPolicy& DeterministicTest = {});
	~TiramisuEditorRenderBridge() override;

	TiramisuEditorRenderBridge(const TiramisuEditorRenderBridge&) = delete;
	TiramisuEditorRenderBridge& operator=(const TiramisuEditorRenderBridge&) = delete;

	// Реализует renderer-neutral ImGui presentation contract.
	[[nodiscard]] EXrUIRendererPlatform GetPlatform() const noexcept override;
	[[nodiscard]] bool SupportsPlatformViewports() const noexcept override;
	[[nodiscard]] bool OwnsMainPresentation() const noexcept override;
	[[nodiscard]] bool Initialize() override;
	void Shutdown() override;
	void BeginFrame() override;
	void RenderDrawData(ImDrawData& DrawData) override;
	void InvalidateDeviceObjects() override;
	void CreateDeviceObjects() override;

	// Принимает immutable scene snapshots и публикует viewport surfaces.
	[[nodiscard]] EEditorRenderBackendKind GetKind() const noexcept override;
	void CaptureViewport(u32 ViewportId) override;
	void ResizeViewport(u32 ViewportId, u32 Width, u32 Height) override;
	bool SubmitViewportScene(u32 ViewportId, const FEditorViewportSceneSnapshot& Snapshot) override;
	[[nodiscard]] FEditorViewportPickResult PickViewport(
		u32 ViewportId,
		const FEditorViewportPickRequest& Request
	) const override;
	[[nodiscard]] FEditorViewportSurface GetViewportSurface(
		u32 ViewportId
	) const override;
	void CopyViewportOverlayText(u32 ViewportId, xr_vector<FEditorOverlayText>& OutText) const override;
	[[nodiscard]] FEditorTextureHandle CreateTexture(
		const FEditorTextureUpload& Upload
	) override;
	bool UpdateTexture(FEditorTextureHandle Handle, const FEditorTextureUpload& Upload) override;
	void DestroyTexture(FEditorTextureHandle Handle) override;
	[[nodiscard]] FEditorViewportSurface GetTextureSurface(
		FEditorTextureHandle Handle
	) const override;
	[[nodiscard]] FRenderStatisticsSnapshot GetRenderStatistics()
		const noexcept override;

	// Управляет безопасным material preview через тот же device и material ABI.
	[[nodiscard]] bool IsAvailable() const noexcept override;
	[[nodiscard]] FMaterialPreviewHandle CreatePreview() override;
	void DestroyPreview(FMaterialPreviewHandle Handle) override;
	void UpdatePreview(FMaterialPreviewHandle Handle, const FMaterialPreviewSource& Source) override;
	void ResizePreview(FMaterialPreviewHandle Handle, u32 Width, u32 Height) override;
	void RenderPreview(FMaterialPreviewHandle Handle, float DeltaSeconds) override;
	[[nodiscard]] FMaterialPreviewFrame GetPreviewFrame(
		FMaterialPreviewHandle Handle
	) const override;

	// Расширение NRI ImGui принимает ImTextureID только для SHADER_RESOURCE descriptor.
	// Renderer surfaces регистрируют descriptor строго на время жизни GPU-ресурса.
	void RegisterImguiTexture(void* ShaderResourceDescriptor);
	void UnregisterImguiTexture(void* ShaderResourceDescriptor);
	[[nodiscard]] u32 GetSkippedIncompatibleTextureCount() const noexcept;
	// Renderer-neutral статус для детерминированного GPU smoke test.
	// Снимок не выпускает NRI-объекты за границу renderer.
	[[nodiscard]] FEditorViewportMaterialStatus GetViewportMaterialStatus(
		u32 ViewportId, FEditorMaterialSlotId MaterialSlot
	) const override;
	[[nodiscard]] xr_string_view GetLastDiagnostic() const noexcept override;

private:
	struct FImpl;
	std::unique_ptr<FImpl> Impl;
};
