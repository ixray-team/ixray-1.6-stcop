#include "stdafx.h"
#include "../../Kernel/TaskbarProgress.h"

#include <ShObjIdl.h>

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

static STaskbarState g_TaskbarState;

static TBPFLAG ToTbpflag(ETaskbarProgressState Value)
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

static xr_string GetPendingText(const xr_string& Text)
{
	xr_string Result = Text;
	if (Result.size() > MaxTextLength)
	{
		Result.resize(MaxTextLength);
	}

	return Result;
}

static HICON GetOverlayIcon(int ResourceId)
{
	auto It = g_TaskbarState.Icons.find(ResourceId);
	if (It != g_TaskbarState.Icons.end())
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

	g_TaskbarState.Icons.emplace(ResourceId, Icon);
	return Icon;
}

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

	g_TaskbarState.NeedCoUninit = SUCCEEDED(CoResult);

	ITaskbarList3* Taskbar = nullptr;
	HRESULT Result = CoCreateInstance(CLSID_TaskbarList, nullptr, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&Taskbar));
	if (FAILED(Result) || Taskbar == nullptr)
	{
		Msg("! TaskbarProgress: CoCreateInstance failed (0x%08X)", static_cast<unsigned>(Result));
		if (g_TaskbarState.NeedCoUninit)
		{
			CoUninitialize();
			g_TaskbarState.NeedCoUninit = false;
		}

		return false;
	}

	Result = Taskbar->HrInit();
	if (FAILED(Result))
	{
		Msg("! TaskbarProgress: HrInit failed (0x%08X)", static_cast<unsigned>(Result));
		Taskbar->Release();
		if (g_TaskbarState.NeedCoUninit)
		{
			CoUninitialize();
			g_TaskbarState.NeedCoUninit = false;
		}

		return false;
	}

	g_TaskbarState.Taskbar = Taskbar;
	g_TaskbarState.Hwnd = static_cast<HWND>(NativeHandle);
	g_TaskbarState.OwnerThread = Platform::GetCurrentThread();

	g_TaskbarState.PendingState.store((s32)ETaskbarProgressState::NoProgress);
	g_TaskbarState.PendingAttention.store(false);
	g_TaskbarState.PendingCompleted.store(0);
	g_TaskbarState.PendingTotal.store(0);
	g_TaskbarState.PendingOverlayId.store(0);
	{
		xrCriticalSectionGuard Guard(g_TaskbarState.TextLock);
		g_TaskbarState.PendingTooltip.clear();
		g_TaskbarState.PendingOverlayDescription.clear();
	}

	g_TaskbarState.AppliedState = ETaskbarProgressState::NoProgress;
	g_TaskbarState.AppliedCompleted = 0;
	g_TaskbarState.AppliedTotal = 0;
	g_TaskbarState.AppliedOverlayId = 0;
	g_TaskbarState.AppliedTooltip.clear();
	g_TaskbarState.AppliedOverlayDescription.clear();
	g_TaskbarState.AppliedTooltipValid = false;
	g_TaskbarState.AppliedOverlayValid = false;

	g_TaskbarState.Initialized.store(true);
	return true;
}

void CTaskbarProgress::Release()
{
	if (!g_TaskbarState.Initialized.load())
	{
		return;
	}

	if (g_TaskbarState.OwnerThread != Platform::GetCurrentThread())
	{
		Msg("! TaskbarProgress: Release called from a non-owner thread");
		return;
	}

	g_TaskbarState.Initialized.store(false);
	g_TaskbarState.PendingAttention.store(false);

	if (g_TaskbarState.Taskbar != nullptr)
	{
		if (g_TaskbarState.Hwnd != nullptr)
		{
			g_TaskbarState.Taskbar->SetProgressState(g_TaskbarState.Hwnd, TBPF_NOPROGRESS);
			g_TaskbarState.Taskbar->SetOverlayIcon(g_TaskbarState.Hwnd, nullptr, L"");
		}

		g_TaskbarState.Taskbar->Release();
		g_TaskbarState.Taskbar = nullptr;
	}

	for (const auto& [Id, Icon] : g_TaskbarState.Icons)
	{
		if (Icon != nullptr)
		{
			DestroyIcon(Icon);
		}
	}

	g_TaskbarState.Icons.clear();
	g_TaskbarState.Hwnd = nullptr;
	g_TaskbarState.OwnerThread = nullptr;

	if (g_TaskbarState.NeedCoUninit)
	{
		CoUninitialize();
		g_TaskbarState.NeedCoUninit = false;
	}
}

bool CTaskbarProgress::IsInitialized() const
{
	return g_TaskbarState.Initialized.load();
}

void CTaskbarProgress::SetState(ETaskbarProgressState Value)
{
	g_TaskbarState.PendingState.store((s32)Value);
}

void CTaskbarProgress::SetProgress(u64 Completed, u64 Total)
{
	g_TaskbarState.PendingCompleted.store(Completed);
	g_TaskbarState.PendingTotal.store(Total);
}

void CTaskbarProgress::SetTooltip(const char* Text)
{
	xrCriticalSectionGuard Guard(g_TaskbarState.TextLock);
	g_TaskbarState.PendingTooltip = Text != nullptr ? Text : "";
}

void CTaskbarProgress::SetOverlayIcon(int ResourceId, const char* Description)
{
	{
		xrCriticalSectionGuard Guard(g_TaskbarState.TextLock);
		g_TaskbarState.PendingOverlayDescription = Description != nullptr ? Description : "";
	}

	g_TaskbarState.PendingOverlayId.store(ResourceId);
}

void CTaskbarProgress::ClearOverlayIcon()
{
	{
		xrCriticalSectionGuard Guard(g_TaskbarState.TextLock);
		g_TaskbarState.PendingOverlayDescription.clear();
	}

	g_TaskbarState.PendingOverlayId.store(0);
}

void CTaskbarProgress::Reset()
{
	SetState(ETaskbarProgressState::NoProgress);
}

void CTaskbarProgress::RequestAttention()
{
	g_TaskbarState.PendingAttention.store(true);
}

void CTaskbarProgress::Update()
{
	if (!g_TaskbarState.Initialized.load() || g_TaskbarState.Taskbar == nullptr || g_TaskbarState.Hwnd == nullptr)
	{
		return;
	}

	if (g_TaskbarState.OwnerThread != Platform::GetCurrentThread())
	{
		return;
	}

	const ETaskbarProgressState PendingState = (ETaskbarProgressState)g_TaskbarState.PendingState.load();
	if (PendingState != g_TaskbarState.AppliedState)
	{
		g_TaskbarState.AppliedState = PendingState;
		g_TaskbarState.Taskbar->SetProgressState(g_TaskbarState.Hwnd, ToTbpflag(PendingState));
	}

	const u64 Completed = g_TaskbarState.PendingCompleted.load();
	const u64 Total = g_TaskbarState.PendingTotal.load();
	if (Total != 0 && (Completed != g_TaskbarState.AppliedCompleted || Total != g_TaskbarState.AppliedTotal))
	{
		g_TaskbarState.AppliedCompleted = Completed;
		g_TaskbarState.AppliedTotal = Total;
		g_TaskbarState.Taskbar->SetProgressValue(g_TaskbarState.Hwnd, Completed, Total);
	}

	xr_string Tooltip;
	{
		xrCriticalSectionGuard Guard(g_TaskbarState.TextLock);
		Tooltip = GetPendingText(g_TaskbarState.PendingTooltip);
	}

	if (!g_TaskbarState.AppliedTooltipValid || Tooltip != g_TaskbarState.AppliedTooltip)
	{
		g_TaskbarState.AppliedTooltip = Tooltip;
		g_TaskbarState.AppliedTooltipValid = true;
		g_TaskbarState.Taskbar->SetThumbnailTooltip(g_TaskbarState.Hwnd, Platform::ANSI_TO_TCHAR_U8(Tooltip.c_str()));
	}

	int OverlayId = 0;
	xr_string OverlayDescription;
	{
		xrCriticalSectionGuard Guard(g_TaskbarState.TextLock);
		OverlayId = g_TaskbarState.PendingOverlayId.load();
		OverlayDescription = GetPendingText(g_TaskbarState.PendingOverlayDescription);
	}

	if (!g_TaskbarState.AppliedOverlayValid || OverlayId != g_TaskbarState.AppliedOverlayId || OverlayDescription != g_TaskbarState.AppliedOverlayDescription)
	{
		g_TaskbarState.AppliedOverlayId = OverlayId;
		g_TaskbarState.AppliedOverlayDescription = OverlayDescription;
		g_TaskbarState.AppliedOverlayValid = true;

		HICON Icon = OverlayId != 0 ? GetOverlayIcon(OverlayId) : nullptr;
		g_TaskbarState.Taskbar->SetOverlayIcon(g_TaskbarState.Hwnd, Icon, Platform::ANSI_TO_TCHAR_U8(OverlayDescription.c_str()));
	}

	if (g_TaskbarState.PendingAttention.load())
	{
		g_TaskbarState.PendingAttention.store(false);

		FLASHWINFO Info = {};
		Info.cbSize = sizeof(Info);
		Info.hwnd = g_TaskbarState.Hwnd;
		Info.dwFlags = FLASHW_TRAY;
		Info.uCount = 3;
		FlashWindowEx(&Info);
	}
}
