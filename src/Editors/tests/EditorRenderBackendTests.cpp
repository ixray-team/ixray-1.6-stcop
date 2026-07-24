#include "../xrECore/Editor/EditorRenderBackend.h"

#include <cstdint>
#include <iostream>

namespace
{
class FTestEditorRenderBackend final : public IEditorRenderBackend
{
public:
	explicit FTestEditorRenderBackend(const EEditorRenderBackendKind InKind) : Kind(InKind) {}

	[[nodiscard]] EEditorRenderBackendKind GetKind() const noexcept override
	{
		return Kind;
	}

	void CaptureViewport(const u32 ViewportId) override
	{
		CapturedViewport = ViewportId;
		++CaptureCount;
	}

	void ResizeViewport(const u32 ViewportId, const u32 Width,
		const u32 Height) override
	{
		ResizedViewport = ViewportId;
		Surface.Width = Width;
		Surface.Height = Height;
	}

	bool SubmitViewportScene(const u32 ViewportId,
		const FEditorViewportSceneSnapshot& Snapshot) override
	{
		SubmittedViewport = ViewportId;
		SubmittedRevision = Snapshot.Revision;
		SubmittedInstanceCount = static_cast<u32>(Snapshot.Instances.size());
		SubmittedDebugLineCount = static_cast<u32>(Snapshot.DebugLines.size());
		SubmittedDebugTriangleCount =
			static_cast<u32>(Snapshot.DebugTriangles.size());
		SubmittedDebugDrawRevision = Snapshot.DebugDrawRevision;
		SubmittedOverlayLineCount =
			static_cast<u32>(Snapshot.OverlayLines.size());
		SubmittedOverlayTriangleCount =
			static_cast<u32>(Snapshot.OverlayTriangles.size());
		SubmittedOverlayText.assign(Snapshot.OverlayText.begin(),
			Snapshot.OverlayText.end());
		return AcceptScene;
	}

	[[nodiscard]] FEditorViewportPickResult PickViewport(
		const u32 ViewportId,
		const FEditorViewportPickRequest& Request) const override
	{
		PickedViewport = ViewportId;
		PickOrigin = Request.RayOrigin;
		return PickResult;
	}

	[[nodiscard]] FEditorViewportSurface GetViewportSurface(const u32 ViewportId) const override
	{
		RequestedViewport = ViewportId;
		FEditorViewportSurface Result = Surface;
		Result.ImGuiTextureId = &TextureToken;
		return Result;
	}

	void CopyViewportOverlayText(const u32 ViewportId,
		xr_vector<FEditorOverlayText>& OutText) const override
	{
		RequestedOverlayViewport = ViewportId;
		OutText = SubmittedOverlayText;
	}

	[[nodiscard]] FEditorTextureHandle CreateTexture(
		const FEditorTextureUpload& Upload) override
	{
		TextureRevision = Upload.Revision;
		return TextureHandle;
	}

	bool UpdateTexture(const FEditorTextureHandle Handle,
		const FEditorTextureUpload& Upload) override
	{
		if (Handle != TextureHandle)
			return false;
		TextureRevision = Upload.Revision;
		return true;
	}

	void DestroyTexture(const FEditorTextureHandle Handle) override
	{
		if (Handle == TextureHandle)
			TextureDestroyed = true;
	}

	[[nodiscard]] FEditorViewportSurface GetTextureSurface(
		const FEditorTextureHandle Handle) const override
	{
		FEditorViewportSurface Result;
		if (Handle == TextureHandle && !TextureDestroyed)
		{
			Result.ImGuiTextureId = &TextureToken;
			Result.Width = 2;
			Result.Height = 2;
		}
		return Result;
	}

	EEditorRenderBackendKind Kind;
	u32 CapturedViewport = 0;
	u32 ResizedViewport = 0;
	mutable u32 RequestedViewport = 0;
	mutable u32 PickedViewport = 0;
	mutable u32 RequestedOverlayViewport = 0;
	mutable xr_array<float, 3> PickOrigin = {};
	u32 CaptureCount = 0;
	u32 SubmittedViewport = 0;
	u32 SubmittedInstanceCount = 0;
	u32 SubmittedDebugLineCount = 0;
	u32 SubmittedDebugTriangleCount = 0;
	u32 SubmittedOverlayLineCount = 0;
	u32 SubmittedOverlayTriangleCount = 0;
	xr_vector<FEditorOverlayText> SubmittedOverlayText;
	u64 SubmittedRevision = 0;
	u64 SubmittedDebugDrawRevision = 0;
	bool AcceptScene = true;
	FEditorViewportSurface Surface;
	mutable int TextureToken = 0;
	FEditorTextureHandle TextureHandle{3, 7};
	u64 TextureRevision = 0;
	bool TextureDestroyed = false;
	FEditorViewportPickResult PickResult{true, {91}, {92}, {93},
		{1.0f, 2.0f, 3.0f}, {0.0f, 1.0f, 0.0f}, 4.0f, 5, 42};
};

struct FResetEditorRenderBackend
{
	~FResetEditorRenderBackend()
	{
		(void)InstallEditorRenderBackend(nullptr);
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
	if (GetEditorRenderBackend().GetKind() != EEditorRenderBackendKind::Legacy)
		return Fail("The built-in editor renderer must default to the legacy adapter");
	FEditorDebugLine CapturedLine;
	CapturedLine.Vertices[0].Position = {1.0f, 2.0f, 3.0f};
	FEditorDebugTriangle CapturedTriangle;
	CapturedTriangle.Vertices[2].Color = {0.25f, 0.5f, 0.75f, 1.0f};
	xr_vector<FEditorDebugLine> CapturedLines;
	xr_vector<FEditorDebugTriangle> CapturedTriangles;
	xr_vector<FEditorOverlayLine> CapturedOverlayLines;
	xr_vector<FEditorOverlayTriangle> CapturedOverlayTriangles;
	xr_vector<FEditorOverlayText> CapturedOverlayText;
	xr_vector<FEditorTransientMeshCapture> CapturedTransientMeshes;
	CaptureEditorDebugLine(CapturedLine);
	CaptureEditorOverlayLine({});
	EndEditorDebugDrawCapture(CapturedLines, CapturedTriangles,
		CapturedOverlayLines, CapturedOverlayTriangles, CapturedOverlayText,
		CapturedTransientMeshes);
	if (!CapturedLines.empty() || !CapturedTriangles.empty() ||
		!CapturedOverlayLines.empty() || !CapturedOverlayTriangles.empty() ||
		!CapturedOverlayText.empty() || !CapturedTransientMeshes.empty())
		return Fail("Inactive editor debug capture accepted primitives");
	BeginEditorDebugDrawCapture();
	if (!IsEditorDebugDrawCaptureActive())
		return Fail("Editor debug capture did not become active");
	const int ObjectIdentity = 0;
	if (GetEditorTransientObjectIdentity() != nullptr)
		return Fail("Editor transient object identity was not reset at capture start");
	{
		TiramisuEditorTransientObjectCaptureScope IdentityScope(&ObjectIdentity);
		if (GetEditorTransientObjectIdentity() != &ObjectIdentity)
			return Fail("Editor transient object identity scope was not published");
	}
	if (GetEditorTransientObjectIdentity() != nullptr)
		return Fail("Editor transient object identity scope was not restored");
	CaptureEditorDebugLine(CapturedLine);
	CaptureEditorDebugTriangle(CapturedTriangle);
	FEditorOverlayLine CapturedOverlayLine;
	CapturedOverlayLine.Vertices[1].Position = {0.5f, -0.5f, 0.0f};
	FEditorOverlayTriangle CapturedOverlayTriangle;
	CapturedOverlayTriangle.Vertices[2].Color = {1.0f, 0.5f, 0.0f, 0.25f};
	FEditorOverlayText CapturedText;
	CapturedText.Position = {0.25f, -0.5f};
	CapturedText.Text = "temporary label ownership";
	FEditorTransientMeshCapture CapturedMesh;
	CapturedMesh.MeshId = {101};
	CapturedMesh.ObjectId = {102};
	CapturedMesh.MaterialSlot = {103};
	CapturedMesh.Revision = 7;
	CapturedMesh.ShaderName = "editor\\spawn_icon";
	CapturedMesh.TextureName = "ui\\spawn_test";
	CapturedMesh.SurfaceName = "captured entity";
	CapturedMesh.Vertices.resize(3);
	CapturedMesh.Indices = {0, 1, 2};
	CaptureEditorOverlayLine(CapturedOverlayLine);
	CaptureEditorOverlayTriangle(CapturedOverlayTriangle);
	CaptureEditorOverlayText(CapturedText);
	CaptureEditorTransientMesh(CapturedMesh);
	CapturedMesh.TextureName = "mutated after capture";
	EndEditorDebugDrawCapture(CapturedLines, CapturedTriangles,
		CapturedOverlayLines, CapturedOverlayTriangles, CapturedOverlayText,
		CapturedTransientMeshes);
	if (IsEditorDebugDrawCaptureActive() || CapturedLines.size() != 1 ||
		CapturedTriangles.size() != 1 ||
		CapturedOverlayLines.size() != 1 ||
		CapturedOverlayTriangles.size() != 1 ||
		CapturedOverlayText.size() != 1 ||
		CapturedTransientMeshes.size() != 1 ||
		CapturedLines[0].Vertices[0].Position !=
			CapturedLine.Vertices[0].Position ||
		CapturedTriangles[0].Vertices[2].Color !=
			CapturedTriangle.Vertices[2].Color ||
		CapturedOverlayLines[0].Vertices[1].Position !=
			CapturedOverlayLine.Vertices[1].Position ||
		CapturedOverlayTriangles[0].Vertices[2].Color !=
			CapturedOverlayTriangle.Vertices[2].Color ||
		CapturedOverlayText[0].Position != CapturedText.Position ||
		CapturedOverlayText[0].Text != CapturedText.Text ||
		CapturedTransientMeshes[0].MeshId.Value != 101 ||
		CapturedTransientMeshes[0].TextureName != "ui\\spawn_test" ||
		CapturedTransientMeshes[0].Vertices.size() != 3 ||
		CapturedTransientMeshes[0].Indices.size() != 3)
	{
		return Fail("Editor debug capture did not preserve one redraw packet");
	}

	FTestEditorRenderBackend First(EEditorRenderBackendKind::Tiramisu);
	if (InstallEditorRenderBackend(&First) != nullptr)
		return Fail("The first installed backend unexpectedly replaced a custom backend");
	FResetEditorRenderBackend ResetOnExit;

	if (&GetEditorRenderBackend() != &First)
		return Fail("GetEditorRenderBackend did not publish the installed backend");

	IEditorRenderBackend& Active = GetEditorRenderBackend();
	Active.CaptureViewport(7);
	Active.ResizeViewport(7, 640, 360);
	const FEditorStaticMeshInstance Instance = {};
	const FEditorDebugLine DebugLine = {};
	const FEditorDebugTriangle DebugTriangle = {};
	const FEditorOverlayLine OverlayLine = {};
	const FEditorOverlayTriangle OverlayTriangle = {};
	xr_array<FEditorOverlayText, 1> OverlayText;
	OverlayText[0].Position = {0.25f, -0.5f};
	OverlayText[0].Text = "viewport label";
	FEditorViewportSceneSnapshot Snapshot;
	Snapshot.Instances = xr_span(&Instance, 1);
	Snapshot.DebugLines = xr_span(&DebugLine, 1);
	Snapshot.DebugTriangles = xr_span(&DebugTriangle, 1);
	Snapshot.OverlayLines = xr_span(&OverlayLine, 1);
	Snapshot.OverlayTriangles = xr_span(&OverlayTriangle, 1);
	Snapshot.OverlayText = OverlayText;
	Snapshot.DebugDrawRevision = 24;
	Snapshot.Revision = 42;
	if (!Active.SubmitViewportScene(7, Snapshot))
		return Fail("The installed backend rejected a valid scene snapshot");
	FEditorViewportPickRequest PickRequest;
	PickRequest.RayOrigin = {4.0f, 5.0f, 6.0f};
	const FEditorViewportPickResult PickResult =
		Active.PickViewport(7, PickRequest);
	const FEditorViewportSurface Surface = Active.GetViewportSurface(7);
	xr_vector<FEditorOverlayText> CopiedOverlayText;
	Active.CopyViewportOverlayText(7, CopiedOverlayText);
	if (First.CaptureCount != 1 || First.CapturedViewport != 7 || First.ResizedViewport != 7 ||
		First.RequestedViewport != 7)
		return Fail("Viewport operations were not forwarded to the installed backend");
	if (First.SubmittedViewport != 7 || First.SubmittedRevision != 42 ||
		First.SubmittedInstanceCount != 1 || First.SubmittedDebugLineCount != 1 ||
		First.SubmittedDebugTriangleCount != 1 ||
		First.SubmittedOverlayLineCount != 1 ||
		First.SubmittedOverlayTriangleCount != 1 ||
		First.RequestedOverlayViewport != 7 || CopiedOverlayText.size() != 1 ||
		CopiedOverlayText[0].Text != "viewport label" ||
		First.SubmittedDebugDrawRevision != 24)
		return Fail("Scene snapshots were not forwarded to the installed backend");
	if (!PickResult.Hit || PickResult.ObjectId.Value != 91 ||
		First.PickedViewport != 7 || First.PickOrigin != PickRequest.RayOrigin)
		return Fail("Viewport picking was not forwarded to the installed backend");
	if (!Surface.IsValid() || Surface.Width != 640 || Surface.Height != 360)
		return Fail("The installed backend did not return its opaque viewport surface");

	FTestEditorRenderBackend Second(EEditorRenderBackendKind::Tiramisu);
	if (InstallEditorRenderBackend(&Second) != &First)
		return Fail("Installing a nested backend did not return the previous backend");
	if (&GetEditorRenderBackend() != &Second)
		return Fail("The nested backend was not published");
	if (InstallEditorRenderBackend(&First) != &Second)
		return Fail("Restoring the previous backend did not return the replaced backend");
	if (&GetEditorRenderBackend() != &First)
		return Fail("The previous backend was not restored");

	if (InstallEditorRenderBackend(nullptr) != &First)
		return Fail("Resetting the backend did not return the installed backend");
	if (GetEditorRenderBackend().GetKind() != EEditorRenderBackendKind::Legacy)
		return Fail("Resetting the backend did not restore the legacy adapter");

	return 0;
}
