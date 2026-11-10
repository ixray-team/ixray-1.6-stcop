#include "stdafx.h"
#include "../../Kernel/AccessibilityShortcutGuard.h"

namespace
{
	template <typename T>
	bool GetParam(u32 Action, T& Value)
	{
		Value = {};
		Value.cbSize = sizeof(T);
		return SystemParametersInfo(Action, sizeof(T), &Value, 0) != 0;
	}

	// Rewrites only dwFlags, keeping the other fields at their current values.
	template <typename T>
	void SetFlags(u32 GetAction, u32 SetAction, u32 Flags)
	{
		T Value;
		if (GetParam(GetAction, Value))
		{
			Value.dwFlags = Flags;
			SystemParametersInfo(SetAction, sizeof(T), &Value, 0);
		}
	}

	// A feature the user has turned off can only be triggered by accident during
	// gameplay, so its hotkey is dropped. A feature that is on is left as is.
	u32 SuppressedFlags(u32 Flags, u32 OnFlag, u32 HotkeyFlags)
	{
		return (Flags & OnFlag) != 0 ? Flags : (Flags & ~HotkeyFlags);
	}
}

void AccessibilityShortcutGuard::Suppress()
{
	if (IsSuppressed)
	{
		return;
	}

	// Re-read every time: the user may have changed the settings while the game
	// was in the background.
	STICKYKEYS StickyKeys;
	TOGGLEKEYS ToggleKeys;
	FILTERKEYS FilterKeys;
	if (!GetParam(SPI_GETSTICKYKEYS, StickyKeys) || !GetParam(SPI_GETTOGGLEKEYS, ToggleKeys) || !GetParam(SPI_GETFILTERKEYS, FilterKeys))
	{
		return;
	}

	StickyKeysFlags = StickyKeys.dwFlags;
	ToggleKeysFlags = ToggleKeys.dwFlags;
	FilterKeysFlags = FilterKeys.dwFlags;
	IsSuppressed = true;

	SetFlags<STICKYKEYS>(SPI_GETSTICKYKEYS, SPI_SETSTICKYKEYS, SuppressedFlags(StickyKeysFlags, SKF_STICKYKEYSON, SKF_HOTKEYACTIVE | SKF_CONFIRMHOTKEY));
	SetFlags<TOGGLEKEYS>(SPI_GETTOGGLEKEYS, SPI_SETTOGGLEKEYS, SuppressedFlags(ToggleKeysFlags, TKF_TOGGLEKEYSON, TKF_HOTKEYACTIVE | TKF_CONFIRMHOTKEY));
	SetFlags<FILTERKEYS>(SPI_GETFILTERKEYS, SPI_SETFILTERKEYS, SuppressedFlags(FilterKeysFlags, FKF_FILTERKEYSON, FKF_HOTKEYACTIVE | FKF_CONFIRMHOTKEY));
}

void AccessibilityShortcutGuard::Restore()
{
	if (!IsSuppressed)
	{
		return;
	}

	IsSuppressed = false;

	SetFlags<STICKYKEYS>(SPI_GETSTICKYKEYS, SPI_SETSTICKYKEYS, StickyKeysFlags);
	SetFlags<TOGGLEKEYS>(SPI_GETTOGGLEKEYS, SPI_SETTOGGLEKEYS, ToggleKeysFlags);
	SetFlags<FILTERKEYS>(SPI_GETFILTERKEYS, SPI_SETFILTERKEYS, FilterKeysFlags);
}
