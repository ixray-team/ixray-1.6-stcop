#include "stdafx.h"
#include "appinfo.h"

XRCORE_API CAppInfo g_AppInfo;

void CAppInfo::SetMainThread()
{
	MainThread = Platform::GetCurrentThread();
	MainThreadId = Platform::GetThreadId(MainThread);
}

void CAppInfo::SetAsyncLoadThread(bool IsActive)
{
	AsyncLoadThreadId.store(IsActive ? Platform::GetThreadId(Platform::GetCurrentThread()) : 0, std::memory_order_release);
}

bool CAppInfo::IsAsyncLoadThread() const noexcept
{
	const size_t Id = AsyncLoadThreadId.load(std::memory_order_acquire);
	return Id != 0 && Id == Platform::GetThreadId(Platform::GetCurrentThread());
}

void CAppInfo::SetRenderThread(bool IsActive)
{
	RenderThreadId.store(IsActive ? Platform::GetThreadId(Platform::GetCurrentThread()) : 0, std::memory_order_release);
}

bool CAppInfo::IsRenderThread() const noexcept
{
	const size_t Id = RenderThreadId.load(std::memory_order_acquire);
	return Id != 0 ? Id == Platform::GetThreadId(Platform::GetCurrentThread()) : IsPrimaryThread();
}

void CAppInfo::SetPrimaryDelegate(bool IsActive)
{
	PrimaryDelegateId.store(IsActive ? Platform::GetThreadId(Platform::GetCurrentThread()) : 0, std::memory_order_release);
}

bool CAppInfo::IsPrimaryThread() const noexcept
{
	const size_t Id = Platform::GetThreadId(Platform::GetCurrentThread());
	return MainThreadId != 0 && (MainThreadId == Id || PrimaryDelegateId.load(std::memory_order_acquire) == Id);
}
