#include "stdafx.h"
#include "AccessibilityShortcutGuard.h"

AccessibilityShortcutGuard& AccessibilityShortcutGuard::Instance()
{
	static AccessibilityShortcutGuard Guard;
	return Guard;
}

AccessibilityShortcutGuard::AccessibilityShortcutGuard() = default;

AccessibilityShortcutGuard::~AccessibilityShortcutGuard()
{
	Restore();
}
