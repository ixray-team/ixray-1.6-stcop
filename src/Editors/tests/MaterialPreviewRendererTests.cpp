#include "../xrECore/Editor/MaterialPreviewRenderer.h"

#include <cmath>
#include <cstdint>
#include <iostream>
#include <string>

namespace
{
class FTestMaterialPreviewRenderer final : public IMaterialPreviewRenderer
{
public:
	[[nodiscard]] bool IsAvailable() const noexcept override
	{
		return true;
	}

	[[nodiscard]] FMaterialPreviewHandle CreatePreview() override
	{
		++CreateCount;
		return Handle;
	}

	void DestroyPreview(const FMaterialPreviewHandle InHandle) override
	{
		DestroyedHandle = InHandle;
		++DestroyCount;
	}

	void UpdatePreview(const FMaterialPreviewHandle InHandle, const FMaterialPreviewSource& Source) override
	{
		UpdatedHandle = InHandle;
		MaterialAssetId = Source.MaterialAssetId;
		MaterialJson = Source.MaterialJson;
		MaterialInstanceJson = Source.MaterialInstanceJson;
		GeneratedHlsl = Source.GeneratedHlsl;
		Environment = Source.Environment;
		Primitive = Source.Primitive;
		Revision = Source.Revision;
		++UpdateCount;
	}

	void ResizePreview(const FMaterialPreviewHandle InHandle, const u32 InWidth, const u32 InHeight) override
	{
		ResizedHandle = InHandle;
		Width = InWidth;
		Height = InHeight;
	}

	void RenderPreview(const FMaterialPreviewHandle InHandle, const float DeltaSeconds) override
	{
		RenderedHandle = InHandle;
		LastDeltaSeconds = DeltaSeconds;
		++RenderCount;
	}

	[[nodiscard]] FMaterialPreviewFrame GetPreviewFrame(
		const FMaterialPreviewHandle InHandle
	) const override
	{
		RequestedHandle = InHandle;
		FMaterialPreviewFrame Frame;
		Frame.Surface.ImGuiTextureId = &TextureToken;
		Frame.Surface.Width = Width;
		Frame.Surface.Height = Height;
		Frame.State = EMaterialPreviewState::Ready;
		Frame.RequestedRevision = Revision;
		Frame.AcceptedRevision = Revision;
		Frame.PipelineKey = 0x123456789abcdef0ull;
		Frame.Backend = "Vulkan/SPIR-V";
		Frame.RenderPass = "MaterialPreview";
		Frame.VertexFactory = "MaterialLevelStatic";
		Frame.Diagnostic = "ready";
		return Frame;
	}

	FMaterialPreviewHandle Handle{11, 4};
	FMaterialPreviewHandle UpdatedHandle;
	FMaterialPreviewHandle ResizedHandle;
	FMaterialPreviewHandle RenderedHandle;
	FMaterialPreviewHandle DestroyedHandle;
	mutable FMaterialPreviewHandle RequestedHandle;
	xr_string MaterialAssetId;
	xr_string MaterialJson;
	xr_string MaterialInstanceJson;
	xr_string GeneratedHlsl;
	xr_string Environment;
	EMaterialPreviewPrimitive Primitive = EMaterialPreviewPrimitive::Sphere;
	u64 Revision = 0;
	u32 Width = 0;
	u32 Height = 0;
	float LastDeltaSeconds = 0.0f;
	int CreateCount = 0;
	int UpdateCount = 0;
	int RenderCount = 0;
	int DestroyCount = 0;
	mutable int TextureToken = 0;
};

struct FResetMaterialPreviewRenderer
{
	~FResetMaterialPreviewRenderer()
	{
		(void)InstallMaterialPreviewRenderer(nullptr);
	}
};

int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main()
{
	IMaterialPreviewRenderer& Unavailable = GetMaterialPreviewRenderer();
	if (Unavailable.IsAvailable() || Unavailable.CreatePreview().IsValid())
	{
		return Fail("The default preview renderer must be safely unavailable");
	}
	const FMaterialPreviewFrame UnavailableFrame = Unavailable.GetPreviewFrame({});
	if (UnavailableFrame.State != EMaterialPreviewState::Unavailable ||
		UnavailableFrame.Diagnostic.empty())
	{
		return Fail("The unavailable preview renderer did not return a diagnostic");
	}

	FTestMaterialPreviewRenderer Renderer;
	if (InstallMaterialPreviewRenderer(&Renderer) != nullptr)
	{
		return Fail("The first preview renderer unexpectedly replaced a custom renderer");
	}
	FResetMaterialPreviewRenderer ResetOnExit;
	if (&GetMaterialPreviewRenderer() != &Renderer)
	{
		return Fail("The installed material preview renderer was not published");
	}

	IMaterialPreviewRenderer& Active = GetMaterialPreviewRenderer();
	const FMaterialPreviewHandle Handle = Active.CreatePreview();
	if (!Handle.IsValid() || Handle != Renderer.Handle || Renderer.CreateCount != 1)
	{
		return Fail("CreatePreview did not return the backend generation-counted handle");
	}

	const xr_string MaterialJson = R"({"asset_version":1})";
	const xr_string InstanceJson = R"({"parent":"master"})";
	FMaterialPreviewSource Source;
	Source.MaterialAssetId = "material-guid";
	Source.MaterialJson = MaterialJson;
	Source.MaterialInstanceJson = InstanceJson;
	Source.GeneratedHlsl = "void EvaluateMaterial() {}";
	Source.Environment = "studio";
	Source.Primitive = EMaterialPreviewPrimitive::Cube;
	Source.Revision = 77;
	Active.UpdatePreview(Handle, Source);
	Active.ResizePreview(Handle, 512, 288);
	Active.RenderPreview(Handle, 0.016f);

	if (Renderer.UpdateCount != 1 || Renderer.UpdatedHandle != Handle ||
		Renderer.MaterialAssetId != "material-guid" ||
		Renderer.MaterialJson != MaterialJson ||
		Renderer.MaterialInstanceJson != InstanceJson ||
		Renderer.GeneratedHlsl != Source.GeneratedHlsl ||
		Renderer.Environment != "studio" ||
		Renderer.Primitive != EMaterialPreviewPrimitive::Cube || Renderer.Revision != 77)
	{
		return Fail("UpdatePreview did not forward and copy the renderer-neutral source");
	}
	if (Renderer.ResizedHandle != Handle || Renderer.Width != 512 || Renderer.Height != 288)
	{
		return Fail("ResizePreview did not reach the installed backend");
	}
	if (Renderer.RenderedHandle != Handle || Renderer.RenderCount != 1 ||
		std::abs(Renderer.LastDeltaSeconds - 0.016f) > 0.0001f)
	{
		return Fail("RenderPreview did not reach the installed backend");
	}

	const FMaterialPreviewFrame Frame = Active.GetPreviewFrame(Handle);
	if (Renderer.RequestedHandle != Handle || Frame.State != EMaterialPreviewState::Ready ||
		Frame.RequestedRevision != 77 || Frame.AcceptedRevision != 77 ||
		Frame.PipelineKey != 0x123456789abcdef0ull ||
		Frame.Backend != "Vulkan/SPIR-V" || Frame.RenderPass != "MaterialPreview" ||
		Frame.VertexFactory != "MaterialLevelStatic" || Frame.UsingLastGoodPipeline ||
		!Frame.Surface.IsValid() ||
		Frame.Surface.Width != 512 || Frame.Surface.Height != 288 ||
		Frame.Diagnostic != "ready")
	{
		return Fail("GetPreviewFrame did not expose the backend result");
	}

	Active.DestroyPreview(Handle);
	if (Renderer.DestroyCount != 1 || Renderer.DestroyedHandle != Handle)
	{
		return Fail("DestroyPreview did not release the backend handle");
	}

	if (InstallMaterialPreviewRenderer(nullptr) != &Renderer)
	{
		return Fail("Resetting the preview renderer did not return the installed backend");
	}
	if (GetMaterialPreviewRenderer().IsAvailable())
	{
		return Fail("Resetting the preview renderer did not restore the unavailable adapter");
	}

	return 0;
}
