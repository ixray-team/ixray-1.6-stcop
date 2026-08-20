#pragma once

struct RENDERDOC_API_1_6_0;

namespace xrRenderDoc
{
// Loads RenderDoc before a graphics API device is created. The function is
// idempotent because xrCore can be initialized more than once by editor tools.
XRCORE_API bool Initialize();

// True when RenderDoc hooks are present, including external qrenderdoc
// injection even if the in-application API could not be queried.
XRCORE_API bool IsLoaded();
XRCORE_API bool IsAvailable();
XRCORE_API RENDERDOC_API_1_6_0* GetApi();
XRCORE_API const char* GetCapturePathTemplate();

// Schedules a capture of the next frame. Interactive captures use F12.
XRCORE_API bool TriggerCapture();

// Явная граница capture нужна автоматическим smoke-тестам: она исключает
// неоднозначный выбор следующего Present при наличии нескольких API devices.
// D3D12 требует native device для надёжного сопоставления с game swapchain.
XRCORE_API bool BeginCapture(
	void* WindowHandle,
	void* DeviceHandle = nullptr
);
XRCORE_API bool EndCapture(
	void* WindowHandle,
	void* DeviceHandle = nullptr
);
} // namespace xrRenderDoc
