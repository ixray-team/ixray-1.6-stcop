#pragma once

#include "../../xrEngine/Render.h"

class AsyncScreenshotService
{
public:
	AsyncScreenshotService();
	~AsyncScreenshotService();
	AsyncScreenshotService(const AsyncScreenshotService&) = delete;
	AsyncScreenshotService& operator=(const AsyncScreenshotService&) = delete;

	bool Queue(IRender_interface::ScreenshotMode Mode, const char* Name);
	void Process(bool Wait = false);

private:
	class State;
	xr_shared_ptr<State> Data;
};
