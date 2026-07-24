#include "stdafx.h"
#include "EditorRenderBackend.h"
#include "ui_main.h"

#include <mutex>
#include <vector>

namespace
{
class FLegacyEditorRenderBackend final : public IEditorRenderBackend
{
public:
	[[nodiscard]] EEditorRenderBackendKind GetKind() const noexcept override
	{
		return EEditorRenderBackendKind::Legacy;
	}

	void CaptureViewport(const u32 ViewportId) override
	{
		if (!UI || !GRHI || !UI->RT)
			return;

		const auto It = UI->Views.find(static_cast<int>(ViewportId));
		if (It == UI->Views.end() || !It->second.RTFreez)
			return;

		const ref_rt& Target = It->second.RTFreez;
		if (!Target->pRT || !UI->RT->pRT)
			return;

		GRHI->CopySurface(Target->pRT, UI->RT->pRT);
	}

	void ResizeViewport(const u32 ViewportId, const u32 Width,
		const u32 Height) override
	{
		if (!UI)
			return;

		const auto It = UI->Views.find(static_cast<int>(ViewportId));
		if (It == UI->Views.end())
			return;

		It->second.RTSize.set(static_cast<int>(Width), static_cast<int>(Height));
	}

	bool SubmitViewportScene(const u32 ViewportId,
		const FEditorViewportSceneSnapshot& Snapshot) override
	{
		(void)ViewportId;
		(void)Snapshot;
		// The legacy renderer already consumes EScene directly. This no-op keeps
		// scene packet production independent from the selected editor backend.
		return true;
	}

	[[nodiscard]] FEditorViewportPickResult PickViewport(
		const u32 ViewportId,
		const FEditorViewportPickRequest& Request) const override
	{
		(void)ViewportId;
		(void)Request;
		// Legacy editor tools already perform exact CPU object/mesh picking.
		return {};
	}

	[[nodiscard]] FEditorViewportSurface GetViewportSurface(const u32 ViewportId) const override
	{
		FEditorViewportSurface Surface;
		if (!UI)
			return Surface;

		const auto It = UI->Views.find(static_cast<int>(ViewportId));
		if (It == UI->Views.end() || !It->second.RTFreez)
			return Surface;

		const TUI::Viewport& Viewport = It->second;
		if (!Viewport.RTFreez->pSurface)
			return Surface;

		Surface.ImGuiTextureId = Viewport.RTFreez->pSurface->GetRawTexture();
		Surface.Width = Viewport.RTSize.x > 0 ? static_cast<u32>(Viewport.RTSize.x) : 0;
		Surface.Height = Viewport.RTSize.y > 0 ? static_cast<u32>(Viewport.RTSize.y) : 0;
		return Surface;
	}

	void CopyViewportOverlayText(const u32 ViewportId,
		xr_vector<FEditorOverlayText>& OutText) const override
	{
		(void)ViewportId;
		OutText.clear();
		// Legacy text is already part of the captured D3D9 viewport image.
	}

	[[nodiscard]] FEditorTextureHandle CreateTexture(
		const FEditorTextureUpload& Upload) override
	{
		if (!ValidateUpload(Upload))
			return {};
		std::scoped_lock Lock(TextureMutex);
		u32 Index = FEditorTextureHandle::InvalidIndex;
		if (!FreeTextureSlots.empty())
		{
			Index = FreeTextureSlots.back();
			FreeTextureSlots.pop_back();
		}
		else
		{
			Index = static_cast<u32>(TextureSlots.size());
			TextureSlots.emplace_back();
		}

		FTextureSlot& Slot = TextureSlots[Index];
		Slot.Generation = NextGeneration(Slot.Generation);
		Slot.Alive = true;
		if (!UploadTexture(Slot, Upload))
		{
			Slot.Alive = false;
			FreeTextureSlots.push_back(Index);
			return {};
		}
		return {Index, Slot.Generation};
	}

	bool UpdateTexture(const FEditorTextureHandle Handle,
		const FEditorTextureUpload& Upload) override
	{
		if (!ValidateUpload(Upload))
			return false;
		std::scoped_lock Lock(TextureMutex);
		FTextureSlot* Slot = FindTexture(Handle);
		if (!Slot || Upload.Revision < Slot->Revision)
			return false;
		if (Upload.Revision == Slot->Revision)
			return true;
		return UploadTexture(*Slot, Upload);
	}

	void DestroyTexture(const FEditorTextureHandle Handle) override
	{
		std::scoped_lock Lock(TextureMutex);
		FTextureSlot* Slot = FindTexture(Handle);
		if (!Slot)
			return;
		if (Slot->Surface)
			Slot->Surface->Release();
		Slot->Surface = nullptr;
		Slot->Alive = false;
		Slot->Revision = 0;
		Slot->Width = 0;
		Slot->Height = 0;
		FreeTextureSlots.push_back(Handle.Index);
	}

	[[nodiscard]] FEditorViewportSurface GetTextureSurface(
		const FEditorTextureHandle Handle) const override
	{
		FEditorViewportSurface Result;
		std::scoped_lock Lock(TextureMutex);
		const FTextureSlot* Slot = FindTexture(Handle);
		if (!Slot || !Slot->Surface)
			return Result;
		Result.ImGuiTextureId = Slot->Surface->GetRawTexture();
		Result.Width = Slot->Width;
		Result.Height = Slot->Height;
		return Result;
	}

private:
	struct FTextureSlot
	{
		IRHISurface* Surface = nullptr;
		u32 Generation = 0;
		u32 Width = 0;
		u32 Height = 0;
		u64 Revision = 0;
		bool Alive = false;
	};

	[[nodiscard]] static u32 NextGeneration(const u32 Current)
	{
		const u32 Next = Current + 1;
		return Next == 0 ? 1 : Next;
	}

	[[nodiscard]] static bool ValidateUpload(const FEditorTextureUpload& Upload)
	{
		if (!GRHI || Upload.Width == 0 || Upload.Height == 0 ||
			Upload.Revision == 0 || Upload.RowPitch < Upload.Width * 4ull)
		{
			return false;
		}
		const u64 Required =
			static_cast<u64>(Upload.RowPitch) * Upload.Height;
		return Required <= Upload.Pixels.size();
	}

	bool UploadTexture(FTextureSlot& Slot, const FEditorTextureUpload& Upload)
	{
		ERHI_FORMAT Format = ERHI_FORMAT::R8G8B8A8_UNORM;
		switch (Upload.Format)
		{
		case EEditorTextureFormat::Rgba8Srgb:
			Format = ERHI_FORMAT::R8G8B8A8_UNORM_SRGB;
			break;
		case EEditorTextureFormat::Bgra8Unorm:
			Format = ERHI_FORMAT::B8G8R8A8_UNORM;
			break;
		case EEditorTextureFormat::Bgra8Srgb:
			Format = ERHI_FORMAT::B8G8R8A8_UNORM_SRGB;
			break;
		default:
			break;
		}
		RHITextureDesc Desc;
		Desc.Width = Upload.Width;
		Desc.Height = Upload.Height;
		Desc.Format = Format;
		Desc.MipLevels = 1;
		Desc.ArraySize = 1;
		Desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
		Desc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;

		RHISubResource Subresource = {};
		Subresource.Width = Upload.Width;
		Subresource.Height = Upload.Height;
		Subresource.TextureFormat = Format;
		Subresource.RowPitch = Upload.RowPitch;
		Subresource.Data = const_cast<std::byte*>(Upload.Pixels.data());

		if (!Slot.Surface || Slot.Width != Upload.Width ||
			Slot.Height != Upload.Height || Slot.Surface->GetFormat() != Format)
		{
			if (Slot.Surface)
				Slot.Surface->Release();
			Slot.Surface = GRHI->CreateTexture2D(Desc, Subresource);
		}
		else
		{
			RHIBox Box;
			Box.left = 0;
			Box.top = 0;
			Box.front = 0;
			Box.right = Upload.Width;
			Box.bottom = Upload.Height;
			Box.back = 1;
			Slot.Surface->UpdateData(0, 0, &Subresource, Box);
		}
		if (!Slot.Surface)
			return false;
		Slot.Width = Upload.Width;
		Slot.Height = Upload.Height;
		Slot.Revision = Upload.Revision;
		return true;
	}

	[[nodiscard]] FTextureSlot* FindTexture(const FEditorTextureHandle Handle)
	{
		if (!Handle.IsValid() || Handle.Index >= TextureSlots.size())
			return nullptr;
		FTextureSlot& Slot = TextureSlots[Handle.Index];
		return Slot.Alive && Slot.Generation == Handle.Generation ? &Slot : nullptr;
	}

	[[nodiscard]] const FTextureSlot* FindTexture(
		const FEditorTextureHandle Handle) const
	{
		if (!Handle.IsValid() || Handle.Index >= TextureSlots.size())
			return nullptr;
		const FTextureSlot& Slot = TextureSlots[Handle.Index];
		return Slot.Alive && Slot.Generation == Handle.Generation ? &Slot : nullptr;
	}

	mutable std::mutex TextureMutex;
	xr_vector<FTextureSlot> TextureSlots;
	xr_vector<u32> FreeTextureSlots;
};

FLegacyEditorRenderBackend LegacyEditorRenderBackend;
IEditorRenderBackend* InstalledEditorRenderBackend = nullptr;
constexpr size_t MaxCapturedDebugPrimitives = 1u << 20u;
bool EditorDebugDrawCaptureActive = false;
const void* EditorTransientObjectIdentity = nullptr;
xr_vector<FEditorDebugLine> CapturedEditorDebugLines;
xr_vector<FEditorDebugTriangle> CapturedEditorDebugTriangles;
xr_vector<FEditorOverlayLine> CapturedEditorOverlayLines;
xr_vector<FEditorOverlayTriangle> CapturedEditorOverlayTriangles;
xr_vector<FEditorOverlayText> CapturedEditorOverlayText;
xr_vector<FEditorTransientMeshCapture> CapturedEditorTransientMeshes;
} // namespace

IEditorRenderBackend& GetEditorRenderBackend() noexcept
{
	return InstalledEditorRenderBackend ? *InstalledEditorRenderBackend : LegacyEditorRenderBackend;
}

IEditorRenderBackend* InstallEditorRenderBackend(IEditorRenderBackend* Backend) noexcept
{
	IEditorRenderBackend* Previous = InstalledEditorRenderBackend;
	InstalledEditorRenderBackend = Backend;
	return Previous;
}

void BeginEditorDebugDrawCapture()
{
	CapturedEditorDebugLines.clear();
	CapturedEditorDebugTriangles.clear();
	CapturedEditorOverlayLines.clear();
	CapturedEditorOverlayTriangles.clear();
	CapturedEditorOverlayText.clear();
	CapturedEditorTransientMeshes.clear();
	EditorTransientObjectIdentity = nullptr;
	EditorDebugDrawCaptureActive = true;
}

bool IsEditorDebugDrawCaptureActive() noexcept
{
	return EditorDebugDrawCaptureActive;
}

const void* SetEditorTransientObjectIdentity(const void* Identity) noexcept
{
	const void* Previous = EditorTransientObjectIdentity;
	EditorTransientObjectIdentity = Identity;
	return Previous;
}

const void* GetEditorTransientObjectIdentity() noexcept
{
	return EditorTransientObjectIdentity;
}

void CaptureEditorTransientMesh(FEditorTransientMeshCapture Mesh)
{
	if (!EditorDebugDrawCaptureActive || !Mesh.MeshId.IsValid() ||
		!Mesh.ObjectId.IsValid() || !Mesh.MaterialSlot.IsValid() ||
		Mesh.Revision == 0 || Mesh.Vertices.empty() || Mesh.Indices.empty() ||
		CapturedEditorTransientMeshes.size() >= MaxCapturedDebugPrimitives)
	{
		return;
	}
	CapturedEditorTransientMeshes.push_back(std::move(Mesh));
}

void CaptureEditorDebugLine(const FEditorDebugLine& Line)
{
	if (!EditorDebugDrawCaptureActive ||
		CapturedEditorDebugLines.size() >= MaxCapturedDebugPrimitives)
	{
		return;
	}
	CapturedEditorDebugLines.push_back(Line);
}

void CaptureEditorDebugTriangle(const FEditorDebugTriangle& Triangle)
{
	if (!EditorDebugDrawCaptureActive ||
		CapturedEditorDebugTriangles.size() >= MaxCapturedDebugPrimitives)
	{
		return;
	}
	CapturedEditorDebugTriangles.push_back(Triangle);
}

void CaptureEditorOverlayLine(const FEditorOverlayLine& Line)
{
	if (!EditorDebugDrawCaptureActive ||
		CapturedEditorOverlayLines.size() >= MaxCapturedDebugPrimitives)
	{
		return;
	}
	CapturedEditorOverlayLines.push_back(Line);
}

void CaptureEditorOverlayTriangle(const FEditorOverlayTriangle& Triangle)
{
	if (!EditorDebugDrawCaptureActive ||
		CapturedEditorOverlayTriangles.size() >= MaxCapturedDebugPrimitives)
	{
		return;
	}
	CapturedEditorOverlayTriangles.push_back(Triangle);
}

void CaptureEditorOverlayText(const FEditorOverlayText& Text)
{
	if (!EditorDebugDrawCaptureActive || Text.Text.empty() ||
		CapturedEditorOverlayText.size() >= MaxCapturedDebugPrimitives)
	{
		return;
	}
	CapturedEditorOverlayText.push_back(Text);
}

void EndEditorDebugDrawCapture(xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles,
	xr_vector<FEditorOverlayLine>& OverlayLines,
	xr_vector<FEditorOverlayTriangle>& OverlayTriangles,
	xr_vector<FEditorOverlayText>& OverlayText,
	xr_vector<FEditorTransientMeshCapture>& TransientMeshes) noexcept
{
	EditorDebugDrawCaptureActive = false;
	Lines.swap(CapturedEditorDebugLines);
	Triangles.swap(CapturedEditorDebugTriangles);
	OverlayLines.swap(CapturedEditorOverlayLines);
	OverlayTriangles.swap(CapturedEditorOverlayTriangles);
	OverlayText.swap(CapturedEditorOverlayText);
	TransientMeshes.swap(CapturedEditorTransientMeshes);
	CapturedEditorDebugLines.clear();
	CapturedEditorDebugTriangles.clear();
	CapturedEditorOverlayLines.clear();
	CapturedEditorOverlayTriangles.clear();
	CapturedEditorOverlayText.clear();
	CapturedEditorTransientMeshes.clear();
	EditorTransientObjectIdentity = nullptr;
}
