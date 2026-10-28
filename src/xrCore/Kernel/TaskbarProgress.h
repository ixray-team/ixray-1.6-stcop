#pragma once

#include "../xrCore.h"

struct SDL_Window;

// Taskbar progress for long-running work.
// The setters only record the desired state and may be called from any thread.
// Update() applies the recorded state on the thread that called Initialize().
enum class ETaskbarProgressState : int
{
	NoProgress = 0,
	Indeterminate,
	Normal,
	Paused,
	Error,
};

class XRCORE_API CTaskbarProgress
{
public:
	static CTaskbarProgress& Instance();

	CTaskbarProgress(const CTaskbarProgress&) = delete;
	CTaskbarProgress& operator=(const CTaskbarProgress&) = delete;

	// Must be called on the UI thread. Returns false when the platform has no taskbar support.
	bool Initialize(SDL_Window* Window);
	void Release();
	bool IsInitialized() const;

	// Thread-safe: the pending state is applied by Update() on the UI thread.
	void SetState(ETaskbarProgressState State);
	void SetProgress(u64 Completed, u64 Total);
	void SetTooltip(const char* Text);
	// ResourceId is an icon resource of the executable; 0 clears the overlay.
	void SetOverlayIcon(int ResourceId, const char* Description);
	void ClearOverlayIcon();
	void Reset();

	// Flashes the taskbar button once, until the window gets focus.
	void RequestAttention();

	// Must be called on the UI thread, once per frame.
	void Update();

private:
	CTaskbarProgress() = default;
};
