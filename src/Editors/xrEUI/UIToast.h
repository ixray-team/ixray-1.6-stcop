#pragma once

enum class EToastType : u8
{
	Info,
	Success,
	Warning,
	Error,
};

// Short notifications stacked in the bottom right corner of the main window. Timed by SDL ticks and
// drawn as small ImGui windows kept above every other window, so a click on a toast does not reach
// the widgets under it. The close button dismisses a toast; hovering it stops the timer.
class XREUI_API UIToastQueue
{
public:
	// Any thread. Title and Text are UTF-8; the timer starts when the toast is first drawn.
	void Push(EToastType Type, const char* Title, const char* Text, u32 DurationMs = 4000);
	// Main thread, inside the ImGui frame
	void Draw();

private:
	struct Toast
	{
		EToastType Type = EToastType::Info;
		xr_string Title;
		xr_string Text;
		u64 StartTicks = 0;
		u32 DurationMs = 0;
		bool Closing = false;
	};

	xrCriticalSection Lock;
	xr_vector<Toast> Toasts;
	u64 LastTicks = 0;
};
