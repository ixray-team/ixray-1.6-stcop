#pragma once

#include "../../../Include/xrRender/EditorRenderer.h"

#include <vector>

#ifndef ECORE_API
#ifdef XRECORE_EXPORTS
#define ECORE_API __declspec(dllexport)
#else
#define ECORE_API __declspec(dllimport)
#endif
#endif

// Returns the installed backend or the built-in legacy adapter when none was installed.
[[nodiscard]] ECORE_API IEditorRenderBackend& GetEditorRenderBackend() noexcept;

// The caller owns Backend and must restore the previous backend before destroying it.
// Passing nullptr restores the built-in legacy adapter. Editor UI calls this on its UI thread.
[[nodiscard]] ECORE_API IEditorRenderBackend* InstallEditorRenderBackend(IEditorRenderBackend* Backend) noexcept;

// One-frame legacy mesh converted to the renderer-neutral static-mesh
// contract. Strings and arrays are owned because capture may outlive temporary
// shader/vertex data until LevelEditor drains the redraw packet.
struct FEditorTransientMeshCapture
{
	FEditorStaticMeshId MeshId;
	FEditorSceneObjectId ObjectId;
	FEditorMaterialSlotId MaterialSlot;
	u64 Revision = 0;
	xr_string ShaderName;
	xr_string TextureName;
	xr_string SurfaceName;
	EEditorMaterialSlotFlags MaterialFlags = EEditorMaterialSlotFlags::None;
	xr_vector<FEditorStaticMeshVertex> Vertices;
	xr_vector<u32> Indices;
	xr_array<float, 16> LocalToWorld = {
		1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f
	};
	EEditorSceneInstanceFlags InstanceFlags = EEditorSceneInstanceFlags::None;
};

// Captures legacy CDrawUtilities world-space primitives and special transient
// meshes during one editor redraw. LevelEditor drains the lists into its
// renderer-neutral scene packet; the legacy backend continues rendering the
// same primitives normally.
ECORE_API void BeginEditorDebugDrawCapture();
[[nodiscard]] ECORE_API bool IsEditorDebugDrawCaptureActive() noexcept;
// Legacy objects publish their address only as a process-lifetime object ID;
// the pointer is never dereferenced by the renderer or serialized.
[[nodiscard]] ECORE_API const void* SetEditorTransientObjectIdentity(
	const void* Identity
) noexcept;
[[nodiscard]] ECORE_API const void* GetEditorTransientObjectIdentity() noexcept;

class TiramisuEditorTransientObjectCaptureScope
{
public:
	explicit TiramisuEditorTransientObjectCaptureScope(const void* Identity) noexcept
		: Previous(SetEditorTransientObjectIdentity(Identity))
	{
	}
	~TiramisuEditorTransientObjectCaptureScope()
	{
		(void)SetEditorTransientObjectIdentity(Previous);
	}
	TiramisuEditorTransientObjectCaptureScope(
		const TiramisuEditorTransientObjectCaptureScope&
	) = delete;
	TiramisuEditorTransientObjectCaptureScope& operator=(
		const TiramisuEditorTransientObjectCaptureScope&
	) = delete;

private:
	const void* Previous = nullptr;
};

ECORE_API void CaptureEditorTransientMesh(FEditorTransientMeshCapture Mesh);
ECORE_API void CaptureEditorDebugLine(const FEditorDebugLine& Line);
ECORE_API void CaptureEditorDebugTriangle(const FEditorDebugTriangle& Triangle);
ECORE_API void CaptureEditorOverlayLine(const FEditorOverlayLine& Line);
ECORE_API void CaptureEditorOverlayTriangle(
	const FEditorOverlayTriangle& Triangle
);
ECORE_API void CaptureEditorOverlayText(const FEditorOverlayText& Text);
ECORE_API void EndEditorDebugDrawCapture(
	xr_vector<FEditorDebugLine>& Lines,
	xr_vector<FEditorDebugTriangle>& Triangles,
	xr_vector<FEditorOverlayLine>& OverlayLines,
	xr_vector<FEditorOverlayTriangle>& OverlayTriangles,
	xr_vector<FEditorOverlayText>& OverlayText,
	xr_vector<FEditorTransientMeshCapture>& TransientMeshes
) noexcept;
