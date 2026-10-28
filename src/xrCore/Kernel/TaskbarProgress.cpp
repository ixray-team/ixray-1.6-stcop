#include "stdafx.h"
#include "TaskbarProgress.h"

CTaskbarProgress& CTaskbarProgress::Instance()
{
	static CTaskbarProgress Taskbar;
	return Taskbar;
}

#ifndef IXR_WINDOWS

bool CTaskbarProgress::Initialize(SDL_Window*)
{
	return false;
}

void CTaskbarProgress::Release()
{
}

bool CTaskbarProgress::IsInitialized() const
{
	return false;
}

void CTaskbarProgress::SetState(ETaskbarProgressState)
{
}

void CTaskbarProgress::SetProgress(u64, u64)
{
}

void CTaskbarProgress::SetTooltip(const char*)
{
}

void CTaskbarProgress::SetOverlayIcon(int, const char*)
{
}

void CTaskbarProgress::ClearOverlayIcon()
{
}

void CTaskbarProgress::Reset()
{
}

void CTaskbarProgress::RequestAttention()
{
}

void CTaskbarProgress::Update()
{
}

#endif
