#include "stdafx.h"
#include "../../Kernel/TaskbarProgress.h"

#include <ShObjIdl.h>

namespace
{
constexpr int OverlayIconSize = 16;
constexpr size_t MaxTextLength = 1024;

struct STaskbarState
{
	ITaskbarList3* Taskbar = nullptr;
	HWND Hwnd = nullptr;
	bool NeedCoUninit = false;
	ThreadID OwnerThread = nullptr;

	// Written from any thread, applied by Update().
	xr_atomic_bool Initialized = false;
	xr_atomic_bool PendingAttention = false;
	xr_atomic_s32 PendingState = (s32)ETaskbarProgressState::NoProgress;
	xr_atomic_u64 PendingCompleted = 0;
	xr_atomic_u64 PendingTotal = 0;
	xr_atomic_s32 PendingOverlayId = 0;
	xrCriticalSection TextLock;
	xr_string PendingTooltip;
	xr_string PendingOverlayDescription;

	// Written on the owner thread only.
	ETaskbarProgressState AppliedState = ETaskbarProgressState::NoProgress;
	u64 AppliedCompleted = 0;
	u64 AppliedTotal = 0;
	int AppliedOverlayId = 0;
	xr_string AppliedTooltip;
	xr_string AppliedOverlayDescription;
	bool AppliedTooltipValid = false;
	bool AppliedOverlayValid = false;

	xr_hash_map<int, HICON> Icons;
};

STaskbarState State;

TBPFLAG ToTbpflag(ETaskbarProgressState Value)
{
	switch (Value)
	{
		case ETaskbarProgressState::Indeterminate:
			return TBPF_INDETERMINATE;
		case ETaskbarProgressState::Normal:
			return TBPF_NORMAL;
		case ETaskbarProgressState::Paused:
			return TBPF_PAUSED;
		case ETaskbarProgressState::Error:
			return TBPF_ERROR;
		case ETaskbarProgressState::NoProgress:
		default:
			return TBPF_NOPROGRESS;
	}
}

xr_string GetPendingText(const xr_string& Text)
{
	xr_string Result = Text;
	if (Result.size() > MaxTextLength)
	{
		Result.resize(MaxTextLength);
	}

	return Result;
}

HICON GetOverlayIcon(int ResourceId)
{
	auto It = State.Icons.find(ResourceId);
	if (It != State.Icons.end())
	{
		return It->second;
	}

	HICON Icon = static_cast<HICON>(LoadImageW(
		GetModuleHandleW(nullptr),
		MAKEINTRESOURCEW(ResourceId),
		IMAGE_ICON,
		OverlayIconSize,
		OverlayIconSize,
		LR_DEFAULTCOLOR
	));

	State.Icons.emplace(ResourceId, Icon);
	return Icon;
}
} // namespace

bool CTaskbarProgress::Initialize(SDL_Window* Window)
{
	Release();

	if (Window == nullptr)
	{
		Msg("! TaskbarProgress: window is null");
		return false;
	}

	const SDL_PropertiesID Properties = SDL_GetWindowProperties(Window);
	void* NativeHandle = nullptr;
	if (Properties != 0)
	{
		NativeHandle = SDL_GetPointerProperty(Properties, SDL_PROP_WINDOW_WIN32_HWND_POINTER, nullptr);
	}

	if (NativeHandle == nullptr)
	{
		Msg("! TaskbarProgress: native window handle not found");
		return false;
	}

	const HRESULT CoResult = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE);
	if (FAILED(CoResult) && CoResult != RPC_E_CHANGED_MODE)
	{
		Msg("! TaskbarProgress: CoInitializeEx failed (0x%08X)", static_cast<unsigned>(CoResult));
		return false;
	}

	State.NeedCoUninit = SUCCEEDED(CoResult);

	ITaskbarList3* Taskbar = nullptr;
	HRESULT Result = CoCreateInstance(CLSID_TaskbarList, nullptr, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&Taskbar));
	if (FAILED(Result) || Taskbar == nullptr)
	{
		Msg("! TaskbarProgress: CoCreateInstance failed (0x%08X)", static_cast<unsigned>(Result));
		if (State.NeedCoUninit)
		{
			CoUninitialize();
			State.NeedCoUninit = false;
		}

		return false;
	}

	Result = Taskbar->HrInit();
	if (FAILED(Result))
	{
		Msg("! TaskbarProgress: HrInit failed (0x%08X)", static_cast<unsigned>(Result));
		Taskbar->Release();
		if (State.NeedCoUninit)
		{
			CoUninitialize();
			State.NeedCoUninit = false;
		}

		return false;
	}

	State.Taskbar = Taskbar;
	State.Hwnd = static_cast<HWND>(NativeHandle);
	State.OwnerThread = Platform::GetCurrentThread();

	State.PendingState.store((s32)ETaskbarProgressState::NoProgress);
	State.PendingAttention.store(false);
	State.PendingCompleted.store(0);
	State.PendingTotal.store(0);
	State.PendingOverlayId.store(0);
	{
		xrCriticalSectionGuard Guard(State.TextLock);
		State.PendingTooltip.clear();
		State.PendingOverlayDescription.clear();
	}

	State.AppliedState = ETaskbarProgressState::NoProgress;
	State.AppliedCompleted = 0;
	State.AppliedTotal = 0;
	State.AppliedOverlayId = 0;
	State.AppliedTooltip.clear();
	State.AppliedOverlayDescription.clear();
	State.AppliedTooltipValid = false;
	State.AppliedOverlayValid = false;

	State.Initialized.store(true);
	return true;
}

void CTaskbarProgress::Release()
{
	if (!State.Initialized.load())
	{
		return;
	}

	if (State.OwnerThread != Platform::GetCurrentThread())
	{
		Msg("! TaskbarProgress: Release called from a non-owner thread");
		return;
	}

	State.Initialized.store(false);
	State.PendingAttention.store(false);

	if (State.Taskbar != nullptr)
	{
		if (State.Hwnd != nullptr)
		{
			State.Taskbar->SetProgressState(State.Hwnd, TBPF_NOPROGRESS);
			State.Taskbar->SetOverlayIcon(State.Hwnd, nullptr, L"");
		}

		State.Taskbar->Release();
		State.Taskbar = nullptr;
	}

	for (const auto& [Id, Icon] : State.Icons)
	{
		if (Icon != nullptr)
		{
			DestroyIcon(Icon);
		}
	}

	State.Icons.clear();
	State.Hwnd = nullptr;
	State.OwnerThread = nullptr;

	if (State.NeedCoUninit)
	{
		CoUninitialize();
		State.NeedCoUninit = false;
	}
}

bool CTaskbarProgress::IsInitialized() const
{
	return State.Initialized.load();
}

void CTaskbarProgress::SetState(ETaskbarProgressState Value)
{
	State.PendingState.store((s32)Value);
}

void CTaskbarProgress::SetProgress(u64 Completed, u64 Total)
{
	State.PendingCompleted.store(Completed);
	State.PendingTotal.store(Total);
}

void CTaskbarProgress::SetTooltip(const char* Text)
{
	xrCriticalSectionGuard Guard(State.TextLock);
	State.PendingTooltip = Text != nullptr ? Text : "";
}

void CTaskbarProgress::SetOverlayIcon(int ResourceId, const char* Description)
{
	{
		xrCriticalSectionGuard Guard(State.TextLock);
		State.PendingOverlayDescription = Description != nullptr ? Description : "";
	}

	State.PendingOverlayId.store(ResourceId);
}

void CTaskbarProgress::ClearOverlayIcon()
{
	{
		xrCriticalSectionGuard Guard(State.TextLock);
		State.PendingOverlayDescription.clear();
	}

	State.PendingOverlayId.store(0);
}

void CTaskbarProgress::Reset()
{
	SetState(ETaskbarProgressState::NoProgress);
}

void CTaskbarProgress::RequestAttention()
{
	State.PendingAttention.store(true);
}

void CTaskbarProgress::Update()
{
	if (!State.Initialized.load() || State.Taskbar == nullptr || State.Hwnd == nullptr)
	{
		return;
	}

	if (State.OwnerThread != Platform::GetCurrentThread())
	{
		return;
	}

	const ETaskbarProgressState PendingState = (ETaskbarProgressState)State.PendingState.load();
	if (PendingState != State.AppliedState)
	{
		State.AppliedState = PendingState;
		State.Taskbar->SetProgressState(State.Hwnd, ToTbpflag(PendingState));
	}

	const u64 Completed = State.PendingCompleted.load();
	const u64 Total = State.PendingTotal.load();
	if (Total != 0 && (Completed != State.AppliedCompleted || Total != State.AppliedTotal))
	{
		State.AppliedCompleted = Completed;
		State.AppliedTotal = Total;
		State.Taskbar->SetProgressValue(State.Hwnd, Completed, Total);
	}

	xr_string Tooltip;
	{
		xrCriticalSectionGuard Guard(State.TextLock);
		Tooltip = GetPendingText(State.PendingTooltip);
	}

	if (!State.AppliedTooltipValid || Tooltip != State.AppliedTooltip)
	{
		State.AppliedTooltip = Tooltip;
		State.AppliedTooltipValid = true;
		State.Taskbar->SetThumbnailTooltip(State.Hwnd, Platform::ANSI_TO_TCHAR_U8(Tooltip.c_str()));
	}

	int OverlayId = 0;
	xr_string OverlayDescription;
	{
		xrCriticalSectionGuard Guard(State.TextLock);
		OverlayId = State.PendingOverlayId.load();
		OverlayDescription = GetPendingText(State.PendingOverlayDescription);
	}

	if (!State.AppliedOverlayValid || OverlayId != State.AppliedOverlayId || OverlayDescription != State.AppliedOverlayDescription)
	{
		State.AppliedOverlayId = OverlayId;
		State.AppliedOverlayDescription = OverlayDescription;
		State.AppliedOverlayValid = true;

		HICON Icon = OverlayId != 0 ? GetOverlayIcon(OverlayId) : nullptr;
		State.Taskbar->SetOverlayIcon(State.Hwnd, Icon, Platform::ANSI_TO_TCHAR_U8(OverlayDescription.c_str()));
	}

	if (State.PendingAttention.load())
	{
		State.PendingAttention.store(false);

		FLASHWINFO Info = {};
		Info.cbSize = sizeof(Info);
		Info.hwnd = State.Hwnd;
		Info.dwFlags = FLASHW_TRAY;
		Info.uCount = 3;
		FlashWindowEx(&Info);
	}
}
