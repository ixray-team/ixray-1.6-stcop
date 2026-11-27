#pragma once

// Per-session viewport target and stock-R4 scene-output resolve helpers.
// No terrain or gameplay simulation is implemented here. Draws stay on the primary thread.

#include "../../../xrRHI/RHI.h"

namespace ImUtils::Viewer
{
	class CViewerRenderer
	{
	public:
		// Creates the color target and scene-resolve resources.
		// False (logged) on any failure.
		bool Init(u32 width, u32 height);
		void Shutdown();
		bool IsReady() const { return ColorSRV != nullptr; }

		void* ViewSRV() const; // ImGui texture id (native D3D11 SRV)
		u32   ViewportWidth() const { return Width; }
		u32   ViewportHeight() const { return Height; }

		void ClearScene();
		// The viewer's color render-target view (kept for reference/diagnostics).
		IRHIRenderTargetView* SceneTargetView() const { return ColorRTV; }

		// Resolves the scene without its HUD. Only the selected scene is read;
		// the primary renderer's targets are restored before returning.
		void ResolveFromFinalScene();
		// Resolves the selected scene texture, including stock HUD/dialog pixels
		// when present. The panel-sized target preserves the scene aspect ratio.
		void ResolveFromSceneTexture(IRHIShaderResourceView* sceneSRV);

		// Verification: reads the render target back, counts non-black scene
		// pixels, reports lower-frame coverage, and writes a BMP for inspection.
		// Positive coverage is returned only for a fully opaque scene; negative on failure or non-opaque output.
		float VerifyReadback(const char* bmpPath);
		// Dumps any render target view to a BMP (verification aid for the swapchain).
		static bool DumpRTVToBmp(IRHIRenderTargetView* rtv, const char* bmpPath);
		bool DumpSurfaceToBmp(IRHISurface* surface, const char* bmpPath);

	private:
		u32 Width = 0;
		u32 Height = 0;
		u32 LoggedSourceWidth = 0;
		u32 LoggedSourceHeight = 0;
		u32 LoggedViewportWidth = 0;
		u32 LoggedViewportHeight = 0;

		IRHISurface*           ColorRT = nullptr;
		IRHIRenderTargetView*  ColorRTV = nullptr;
		IRHIShaderResourceView* ColorSRV = nullptr;
		void* ResolveVS = nullptr;  // fullscreen-triangle scene resolve
		void* ResolvePS = nullptr;
		void* ResolveSampler = nullptr;
	};

}
