// DXMT native host bridge (macOS).
//
// DXMT's swapchain asks the Wine window driver for a Metal view:
//   dlsym(RTLD_DEFAULT, "macdrv_functions")->get_win_data(hwnd)->client_cocoa_view
//   -> macdrv_view_create_metal_view(view, device) -> macdrv_view_get_metal_layer()
// In a plain macOS process none of that exists, so DXMT ends up with no
// CAMetalLayer and presents nothing (black window). This file provides the
// minimal macdrv surface DXMT needs, backed by the app's own NSView.
//
// The "HWND" our engine passes here is an NSView* (see Device.cpp).

#include <stdint.h>
#include <stdbool.h>

#ifdef __APPLE__

#import <Cocoa/Cocoa.h>
#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>

#include <SDL3/SDL.h>

// Layout must match DXMT's struct macdrv_win_data (src/winemetal/unix/winemetal_unix.c).
struct dxmt_macdrv_win_data
{
	void* hwnd;
	void* cocoa_window;
	void* cocoa_view;
	void* client_cocoa_view;
};

static struct dxmt_macdrv_win_data g_win_data = {};
static CAMetalLayer* g_metal_layer = nil; // owned by this bridge
static NSView* g_metal_view = nil;        // weak: owned by the window

// HWND in DXMT-native terms is the NSView* of our SDL window.
static void* dxmt_bridge_get_win_data(void* hwnd)
{
	g_win_data.hwnd = hwnd;
	g_win_data.cocoa_window = nullptr;
	g_win_data.cocoa_view = hwnd;
	g_win_data.client_cocoa_view = hwnd;
	return &g_win_data;
}

static void dxmt_bridge_release_win_data(void* /*data*/)
{
}

static void* dxmt_bridge_get_cocoa_window(void* hwnd, bool /*require_on_screen*/)
{
	NSView* view = (__bridge NSView*)hwnd;
	return view ? (__bridge void*)view.window : nullptr;
}

static void* dxmt_bridge_create_metal_device(void)
{
	id<MTLDevice> device = MTLCreateSystemDefaultDevice();
	return (__bridge_retained void*)device;
}

static void dxmt_bridge_release_metal_device(void* device)
{
	if (device != nullptr)
	{
		id<MTLDevice> obj = (__bridge_transfer id<MTLDevice>)device;
		obj = nil;
	}
}

static void* dxmt_bridge_create_metal_view(void* view, void* device)
{
	NSView* nsview = (__bridge NSView*)view;
	id<MTLDevice> dev = (__bridge id<MTLDevice>)device;

	if (nsview == nil || dev == nil)
		return nullptr;

	CAMetalLayer* layer = [CAMetalLayer layer];
	layer.device = dev;
	layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
	layer.framebufferOnly = YES;
	layer.opaque = YES;
	layer.contentsScale = nsview.window ? nsview.window.backingScaleFactor : 1.0;

	// AppKit wants layer changes on the main thread; dispatch_sync there would
	// deadlock when the swapchain is already created on it.
	if ([NSThread isMainThread])
	{
		nsview.wantsLayer = YES;
		nsview.layer = layer;
	}
	else
	{
		dispatch_sync(dispatch_get_main_queue(), ^{
			nsview.wantsLayer = YES;
			nsview.layer = layer;
		});
	}

	g_metal_layer = layer;
	g_metal_view = nsview;
	return (__bridge void*)layer;
}

static void* dxmt_bridge_get_metal_layer(void* metal_view)
{
	return metal_view;
}

static void dxmt_bridge_release_metal_view(void* /*metal_view*/)
{
	g_metal_layer = nil;
	g_metal_view = nil;
}

// DXMT looks this struct up with dlsym(RTLD_DEFAULT, "macdrv_functions").
struct dxmt_macdrv_functions
{
	void (*macdrv_init_display_devices)(int);
	void* (*get_win_data)(void*);
	void (*release_win_data)(void*);
	void* (*macdrv_get_cocoa_window)(void*, bool);
	void* (*macdrv_create_metal_device)(void);
	void (*macdrv_release_metal_device)(void*);
	void* (*macdrv_view_create_metal_view)(void*, void*);
	void* (*macdrv_view_get_metal_layer)(void*);
	void (*macdrv_view_release_metal_view)(void*);
	void (*on_main_thread)(void (^)(void));
};

__attribute__((visibility("default"))) struct dxmt_macdrv_functions macdrv_functions = {
	nullptr,
	dxmt_bridge_get_win_data,
	dxmt_bridge_release_win_data,
	dxmt_bridge_get_cocoa_window,
	dxmt_bridge_create_metal_device,
	dxmt_bridge_release_metal_device,
	dxmt_bridge_create_metal_view,
	dxmt_bridge_get_metal_layer,
	dxmt_bridge_release_metal_view,
	nullptr,
};

// C entry point used by Device.cpp: SDL window -> NSView* to pass as HWND.
extern "C" void* IxrDxmtGetNSView(void* sdl_window)
{
	SDL_Window* window = (SDL_Window*)sdl_window;
	if (window == nullptr)
		return nullptr;

	SDL_PropertiesID props = SDL_GetWindowProperties(window);
	if (props == 0)
		return nullptr;

	NSWindow* ns_window = (__bridge NSWindow*)SDL_GetPointerProperty(
		props, SDL_PROP_WINDOW_COCOA_WINDOW_POINTER, nullptr);

	if (ns_window == nil)
		return nullptr;

	NSView* view = [ns_window contentView];
	return (__bridge void*)view;
}

#endif // __APPLE__
