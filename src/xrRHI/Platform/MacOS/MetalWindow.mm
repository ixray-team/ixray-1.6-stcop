#import <Cocoa/Cocoa.h>
#import <QuartzCore/CAMetalLayer.h>
#import <Metal/Metal.h>
#include <SDL3/SDL.h>

namespace RHIPlatform
{
// Layout required by DXMT's native view-hook ABI.
struct NativeWindowData
{
	void* Handle;
	void* CocoaWindow;
	void* CocoaView;
	void* ClientView;
};

class MetalView final
{
public:
	MetalView(NSView* Parent, id<MTLDevice> Device)
	{
		View = [[NSView alloc] initWithFrame:Parent.bounds];
		View.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
		View.wantsLayer = YES;
		CAMetalLayer* Layer = [CAMetalLayer layer];
		Layer.device = Device;
		Layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
		Layer.contentsScale = Parent.window.backingScaleFactor;
		View.layer = Layer;
		[Parent addSubview:View];
	}

	~MetalView()
	{
		if ([NSThread isMainThread])
		{
			ReleaseView();
		}
		else
		{
			dispatch_sync(dispatch_get_main_queue(), ^{
			  ReleaseView();
			});
		}
	}

	MetalView(const MetalView&) = delete;
	MetalView& operator=(const MetalView&) = delete;

	CAMetalLayer* GetLayer() const
	{
		return (CAMetalLayer*)View.layer;
	}

private:
	void ReleaseView()
	{
		[View removeFromSuperview];
		[View release];
	}

	NSView* View = nil;
};
} // namespace RHIPlatform

// DXMT resolves these entry points dynamically; no Wine runtime is involved.
extern "C" __attribute__((visibility("default"))) RHIPlatform::NativeWindowData* get_win_data(void* Handle)
{
	if (!SDL_IsMainThread() || Handle == nullptr)
	{
		return nullptr;
	}
	auto* Window = static_cast<SDL_Window*>(Handle);
	NSWindow* CocoaWindow = (NSWindow*)SDL_GetPointerProperty(SDL_GetWindowProperties(Window), SDL_PROP_WINDOW_COCOA_WINDOW_POINTER, nullptr);
	if (CocoaWindow == nil)
	{
		return nullptr;
	}
	return new RHIPlatform::NativeWindowData{Handle, CocoaWindow, CocoaWindow.contentView, CocoaWindow.contentView};
}

extern "C" __attribute__((visibility("default"))) void release_win_data(RHIPlatform::NativeWindowData* Data)
{
	delete Data;
}

extern "C" __attribute__((visibility("default"))) void* macdrv_view_create_metal_view(void* Parent, void* Device)
{
	if (!SDL_IsMainThread() || Parent == nullptr || Device == nullptr)
	{
		return nullptr;
	}
	return new RHIPlatform::MetalView((NSView*)Parent, (id<MTLDevice>)Device);
}

extern "C" __attribute__((visibility("default"))) void* macdrv_view_get_metal_layer(void* View)
{
	return static_cast<RHIPlatform::MetalView*>(View)->GetLayer();
}

extern "C" __attribute__((visibility("default"))) void macdrv_view_release_metal_view(void* View)
{
	delete static_cast<RHIPlatform::MetalView*>(View);
}

