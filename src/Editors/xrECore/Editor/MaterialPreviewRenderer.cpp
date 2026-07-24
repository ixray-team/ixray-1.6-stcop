#include "stdafx.h"
#include "MaterialPreviewRenderer.h"

namespace
{
class FUnavailableMaterialPreviewRenderer final : public IMaterialPreviewRenderer
{
public:
	[[nodiscard]] bool IsAvailable() const noexcept override
	{
		return false;
	}

	[[nodiscard]] FMaterialPreviewHandle CreatePreview() override
	{
		return {};
	}

	void DestroyPreview(FMaterialPreviewHandle) override {}
	void UpdatePreview(FMaterialPreviewHandle, const FMaterialPreviewSource&) override {}
	void ResizePreview(FMaterialPreviewHandle, u32, u32) override {}
	void RenderPreview(FMaterialPreviewHandle, float) override {}

	[[nodiscard]] FMaterialPreviewFrame GetPreviewFrame(
		FMaterialPreviewHandle
	) const override
	{
		FMaterialPreviewFrame Frame;
		Frame.Diagnostic = "Tiramisu material preview backend is not installed";
		return Frame;
	}
};

FUnavailableMaterialPreviewRenderer UnavailableMaterialPreviewRenderer;
IMaterialPreviewRenderer* InstalledMaterialPreviewRenderer = nullptr;
} // namespace

IMaterialPreviewRenderer& GetMaterialPreviewRenderer() noexcept
{
	return InstalledMaterialPreviewRenderer
			   ? *InstalledMaterialPreviewRenderer
			   : UnavailableMaterialPreviewRenderer;
}

IMaterialPreviewRenderer* InstallMaterialPreviewRenderer(
	IMaterialPreviewRenderer* Renderer
) noexcept
{
	IMaterialPreviewRenderer* Previous = InstalledMaterialPreviewRenderer;
	InstalledMaterialPreviewRenderer = Renderer;
	return Previous;
}
