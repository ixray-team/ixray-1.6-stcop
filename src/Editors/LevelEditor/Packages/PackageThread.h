#pragma once

#include "../../../xrCore/xrCore.h"
#include <SDL3/SDL_thread.h>
#include <functional>
#include <stop_token>

namespace SdkPackages
{
class PackageThread final
{
public:
	// Jobs return failures through their result; callbacks must not throw.
	using Callback = std::function<void(std::stop_token)>;
	using Launcher = std::function<SDL_Thread*(SDL_ThreadFunction, const char*, void*)>;
	PackageThread() = default;
	~PackageThread();
	PackageThread(const PackageThread&) = delete;
	PackageThread& operator=(const PackageThread&) = delete;
	bool Start(const Callback& Function, const char* Name, xr_string& Error, const Launcher& Launch = {});
	void Cancel();
	void Join();

private:
	static int SDLCALL Run(void* Data);
	SDL_Thread* Handle = nullptr;
	std::stop_source Stop;
	Callback Function;
};
} // namespace SdkPackages
