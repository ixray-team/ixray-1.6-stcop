#include "stdafx.h"
#include "ScreenshotManager.h"

extern int SM_FOR_SEND_WIDTH;
extern int SM_FOR_SEND_HEIGHT;

bool ScreenshotManager::SaveScreenshot(IRender_interface::ScreenshotMode mode, const char* name, CMemoryWriter* memory_writer)
{
	if (!GRHI || !GRHI->DevicePtr) {
		return false;
	}
	IRHIRenderTargetView* target = GRHI->GetRenderTargetView(0);
	if (!target) {
		target = RTarget;
	}
	if (!target) {
		return false;
	}
	u32 width = 0;
	u32 height = 0;
	u32 format = 3;
	bool linear = false;
	bool srgb = false;
	bool screenshots_path = false;
	bool flush = false;
	string_path filename = {};
	switch (mode) {
	case IRender_interface::SM_FOR_GAMESAVE:
		width = EngineExternal().gamesaveSize.x;
		height = EngineExternal().gamesaveSize.y;
		flush = true;
		break;
	case IRender_interface::SM_FOR_MPSENDING:
		width = SM_FOR_SEND_WIDTH;
		height = SM_FOR_SEND_HEIGHT;
		flush = true;
		break;
	case IRender_interface::SM_NORMAL: {
		string64 stamp = {};
		xr_string level = g_pGameLevel ? g_pStringTable->translate(g_pGameLevel->name().c_str()).c_str() : "mainmenu";
		format = ps_screenshot_format == 0 ? 0 : ps_screenshot_format == 1 ? 1 : 2;
		const char* extensions[] = { "jpg", "tga", "png" };
		xr_sprintf(filename, "ss_%s_%s_(%s).%s", Core.UserName, timestamp(stamp), level.c_str(), extensions[format]);
		name = filename;
		screenshots_path = true;
		srgb = true;
		break;
	}
	case IRender_interface::SM_FOR_LEVELMAP:
	case IRender_interface::SM_FOR_CUBEMAP:
		width = height = Device.TargetHeight;
		format = 1;
		linear = true;
		VERIFY(name);
		xr_strconcat(filename, name, ".tga");
		name = filename;
		screenshots_path = true;
		break;
	default:
		return true;
	}
	xr_vector<u8> data;
	if (FAILED(GRHI->EncodeRenderTarget(target, width, height, format, linear, srgb, data)) || data.size() > UINT32_MAX) {
		return false;
	}
	if (mode == IRender_interface::SM_FOR_MPSENDING && memory_writer) {
		memory_writer->w(data.data(), static_cast<u32>(data.size()));
		return true;
	}
	IWriter* writer = screenshots_path ? FS.w_open("$screenshots$", name) : FS.w_open(name);
	if (!writer) {
		return false;
	}
	writer->w(data.data(), static_cast<u32>(data.size()));
	FS.w_close(writer, flush);
	return true;
}