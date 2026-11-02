#include "stdafx.h"
#include "PackageThread.h"

namespace SdkPackages
{
PackageThread::~PackageThread()
{
	Cancel();
	Join();
}
bool PackageThread::Start(const Callback& Job, const char* Name, xr_string& Error, const Launcher& Launch)
{
	Error.clear();
	if (Handle || !Job)
	{
		Error = "Package worker is already running or has no work.";
		return false;
	}
	Stop = std::stop_source{};
	Function = Job;
	Handle = Launch ? Launch(Run, Name, this) : SDL_CreateThread(Run, Name, this);
	if (!Handle)
	{
		Function = {};
		Error = xr_string("Cannot start package worker: ") + SDL_GetError();
		return false;
	}
	return true;
}
void PackageThread::Cancel()
{
	Stop.request_stop();
}
void PackageThread::Join()
{
	if (Handle)
	{
		SDL_WaitThread(Handle, nullptr);
		Handle = nullptr;
		Function = {};
	}
}
int SDLCALL PackageThread::Run(void* Data)
{
	auto* Self = static_cast<PackageThread*>(Data);
	Self->Function(Self->Stop.get_token());
	return 0;
}
} // namespace SdkPackages
