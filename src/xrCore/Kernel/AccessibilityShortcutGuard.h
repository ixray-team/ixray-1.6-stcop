#pragma once

class XRCORE_API AccessibilityShortcutGuard
{
public:
	static AccessibilityShortcutGuard& Instance();

	AccessibilityShortcutGuard();
	~AccessibilityShortcutGuard();

	AccessibilityShortcutGuard(const AccessibilityShortcutGuard&) = delete;
	AccessibilityShortcutGuard& operator=(const AccessibilityShortcutGuard&) = delete;

	void Suppress();
	void Restore();

private:
	// Saved dwFlags of STICKYKEYS, TOGGLEKEYS and FILTERKEYS.
	u32 StickyKeysFlags = 0;
	u32 ToggleKeysFlags = 0;
	u32 FilterKeysFlags = 0;
	bool IsSuppressed = false;
};
