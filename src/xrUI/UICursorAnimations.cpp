#include "stdafx.h"
#include "UICursor.h"
#include "../xrEngine/SequenceSectionRegistry.h"

namespace CursorAnimations
{
	static constexpr const char* Keys[] = {"select", "clickable", "drag", "scroll_drag", "hover_slot_drop", "click_lmb", "click_rmb", "scroll_up", "scroll_down", "drop", "scroll_drop", "idle", "context"};
	static_assert(std::size(Keys) == static_cast<u32>(UICursorAction::Count));
	static_assert(std::size(Keys) <= 32);
	static constexpr u32 Bit(UICursorAction Action) { return 1u << static_cast<u32>(Action); }
}

void CUICursor::ConfigureAnimations()
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	AreAnimationsEnabled = pSettings->section_exist("ui_cursor") && pSettings->read_if_exists<bool>("ui_cursor", "enabled", false);
	for (u32 Index = 0; Index < static_cast<u32>(UICursorAction::Count); ++Index)
	{
		const xr_string Prefix = CursorAnimations::Keys[Index];
		Bindings[Index].IsEnabled = AreAnimationsEnabled && pSettings->read_if_exists<bool>("ui_cursor", (Prefix + "_enabled").c_str(), false);
		Bindings[Index].Section = Bindings[Index].IsEnabled ? pSettings->read_if_exists<const char*>("ui_cursor", (Prefix + "_section").c_str(), "") : "";
	}
	if (AreAnimationsEnabled)
	{
		HoldGrabDelay = std::min(pSettings->read_if_exists<u32>("ui_cursor", "drag_hold_delay", 1000), 1000000000u);
		IdleInterval = pSettings->read_if_exists<Ivector2>("ui_cursor", "idle_interval", Ivector2().set(15000, 35000));
		IdleInterval.x = std::clamp(IdleInterval.x, 1, 1000000000);
		IdleInterval.y = std::clamp(IdleInterval.y, IdleInterval.x, 1000000000);
	}
	IdleRandomState = Device.dwTimeContinual | 1u;
	ResetIdleTimer();
	RefreshAnimation();
}

void CUICursor::ResetIdleTimer()
{
	IdleRandomState ^= IdleRandomState << 13;
	IdleRandomState ^= IdleRandomState >> 17;
	IdleRandomState ^= IdleRandomState << 5;
	const u32 Delay = static_cast<u32>(IdleInterval.x) + IdleRandomState % static_cast<u32>(IdleInterval.y - IdleInterval.x + 1);
	NextIdleTime = Device.dwTimeContinual + Delay;
	if (Pulse == UICursorAction::Idle || IsPulseLooping)
	{
		Pulse = UICursorAction::Count;
		IsPulseLooping = false;
	}
}

void CUICursor::RefreshAnimation()
{
	shared_str Next = "";
	if (AreAnimationsEnabled && bVisible && !AreAutomaticSectionsPaused)
	{
		for (s32 Index = static_cast<s32>(UICursorAction::Count) - 1; Index >= 0; --Index)
		{
			if (((ActionMask & (1u << Index)) || Pulse == static_cast<UICursorAction>(Index)) && Bindings[Index].IsEnabled && Bindings[Index].Section.size())
			{
				Next = Bindings[Index].Section;
				break;
			}
		}
	}
	// default is a real event mapping: it suppresses lower-priority actions without registering a name.
	if (Next == "default")
	{
		Next = "";
	}
	if (Next == SelectedSection)
	{
		return;
	}
	const shared_str Previous = SelectedSection;
	const bool WaitForGrip = Next.size() && Next == Bindings[static_cast<u32>(UICursorAction::HoverSlotDrop)].Section && Previous == Bindings[static_cast<u32>(UICursorAction::Drag)].Section;
	SelectedSection = Next;
	if (SelectedSection.size())
	{
		ActivateSequenceSection(*SelectedSection, WaitForGrip, true);
	}
	if (Previous.size())
	{
		DeactivateSequenceSection(*Previous, WaitForGrip);
	}
}

void CUICursor::SetAutomaticSectionsPaused(bool IsPaused)
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	AreAutomaticSectionsPaused = IsPaused;
	Pulse = UICursorAction::Count;
	ResetIdleTimer();
	RefreshAnimation();
}

void CUICursor::PulseAnimation(UICursorAction Action)
{
	// A menu supersedes release; closing it must resolve the current hover, not resume an old pulse.
	if (AreAutomaticSectionsPaused || ((Action == UICursorAction::Drop || Action == UICursorAction::ScrollDrop) && !ContextOwners.empty()))
	{
		return;
	}
	const auto& Binding = Bindings[static_cast<u32>(Action)];
	if (!Binding.IsEnabled || !Binding.Section.size())
	{
		return;
	}
	if (Pulse != UICursorAction::Count && Bindings[static_cast<u32>(Pulse)].Section == Binding.Section)
	{
		// Repeated events or aliases keep the existing playback and its original deadline.
		Pulse = Action;
		RefreshAnimation();
		return;
	}
	Pulse = Action;
	const u64 Duration = Engine.SequenceSections.GetDuration(*Binding.Section);
	IsPulseLooping = Duration == std::numeric_limits<u64>::max();
	PulseEndTime = Device.dwTimeContinual + static_cast<u32>(Duration ? std::min<u64>(Duration, 1000000000) : 1000);
	RefreshAnimation();
}

void CUICursor::UpdateAnimation()
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	if (!AreAnimationsEnabled)
	{
		return;
	}
	const bool WidgetDragging = WidgetDragFrame == Device.dwFrame;
	if (WidgetDragging != IsWidgetDragging)
	{
		IsWidgetDragging = WidgetDragging;
		const u32 Mask = CursorAnimations::Bit(UICursorAction::ScrollDrag);
		ActionMask = IsWidgetDragging ? ActionMask | Mask : ActionMask & ~Mask;
		if (IsWidgetDragging)
		{
			Pulse = UICursorAction::Count;
			ActionMask &= ~(CursorAnimations::Bit(UICursorAction::ClickLmb) | CursorAnimations::Bit(UICursorAction::ClickRmb));
		}
		else
		{
			PulseAnimation(UICursorAction::ScrollDrop);
		}
		ResetIdleTimer();
		RefreshAnimation();
	}
	const InventoryHover Hover = InventoryHoverFrame == Device.dwFrame ? PendingInventoryHover : InventoryHover{};
	ApplyInventoryHover(Hover.Owner ? Hover.Owner : HoverOwner, Hover.Owner != nullptr, Hover.HasItem, Hover.IsSlot, Hover.CanDrop, Hover.Item, Hover.IsButton);
	const auto& DragBinding = Bindings[static_cast<u32>(UICursorAction::Drag)];
	if (IsLeftPressed && !IsNativeDragging && !IsHoldGrab && HoldGrabDelay && PressedItemKey && PressedItemKey == HoveredItemKey && ContextOwners.empty() && DragBinding.IsEnabled && DragBinding.Section.size() && Device.dwTimeContinual - LeftPressTime >= HoldGrabDelay)
	{
		// Visual grip only. Actual drag creation remains owned by the inventory input handler.
		IsHoldGrab = true;
		ActionMask |= CursorAnimations::Bit(UICursorAction::Drag);
		ActionMask &= ~CursorAnimations::Bit(UICursorAction::ClickLmb);
		ResetIdleTimer();
		RefreshAnimation();
	}
	if (Pulse != UICursorAction::Count && !IsPulseLooping && static_cast<s32>(Device.dwTimeContinual - PulseEndTime) >= 0)
	{
		const bool WasIdle = Pulse == UICursorAction::Idle;
		Pulse = UICursorAction::Count;
		if (WasIdle)
		{
			ResetIdleTimer();
		}
		RefreshAnimation();
	}
	const u32 IdleBlockers = CursorAnimations::Bit(UICursorAction::Drag) | CursorAnimations::Bit(UICursorAction::ScrollDrag) | CursorAnimations::Bit(UICursorAction::Context);
	if (!(ActionMask & IdleBlockers) && Pulse == UICursorAction::Count && static_cast<s32>(Device.dwTimeContinual - NextIdleTime) >= 0)
	{
		ResetIdleTimer();
		PulseAnimation(UICursorAction::Idle);
	}
}

void CUICursor::Hide()
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	bVisible = false;
	ActionMask = 0;
	Pulse = UICursorAction::Count;
	ContextOwners.clear();
	HoverOwner = nullptr;
	HasHoveredItem = false;
	IsHoveredButton = false;
	IsHoveredSlot = false;
	CanDropOnHover = false;
	HoverWasDragging = false;
	HoveredItemKey = nullptr;
	PressedItemKey = nullptr;
	IsLeftPressed = false;
	IsNativeDragging = false;
	IsWidgetDragging = false;
	WidgetDragFrame = u32(-1);
	IsHoldGrab = false;
	PendingInventoryHover = {};
	InventoryHoverFrame = u32(-1);
	RefreshAnimation();
}

void CUICursor::OnInputActivity()
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	if (!AreAnimationsEnabled)
	{
		return;
	}
	ResetIdleTimer();
	RefreshAnimation();
}

void CUICursor::OnMouseButton(int Button, bool IsPressed)
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	if (!AreAnimationsEnabled)
	{
		return;
	}
	ResetIdleTimer();
	if (IsPressed && (Pulse == UICursorAction::Drop || Pulse == UICursorAction::ScrollDrop))
	{
		Pulse = UICursorAction::Count;
	}
	if (Button == 0)
	{
		IsLeftPressed = IsPressed;
		if (IsPressed)
		{
			LeftPressTime = Device.dwTimeContinual;
			PressedItemKey = HoveredItemKey;
		}
		else
		{
			PressedItemKey = nullptr;
			if (IsHoldGrab && !IsNativeDragging)
			{
				IsHoldGrab = false;
				ActionMask &= ~(CursorAnimations::Bit(UICursorAction::Drag) | CursorAnimations::Bit(UICursorAction::HoverSlotDrop));
				if (HoverOwner && HasHoveredItem)
				{
					ActionMask |= CursorAnimations::Bit(IsHoveredButton ? UICursorAction::Clickable : UICursorAction::Select);
				}
				PulseAnimation(UICursorAction::Drop);
			}
		}
	}
	if (Button == 0 || Button == 1)
	{
		const u32 Mask = CursorAnimations::Bit(Button == 0 ? UICursorAction::ClickLmb : UICursorAction::ClickRmb);
		ActionMask = IsPressed ? ActionMask | Mask : ActionMask & ~Mask;
	}
	RefreshAnimation();
}

void CUICursor::OnMouseWheel(int Direction)
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	if (!AreAnimationsEnabled)
	{
		return;
	}
	ResetIdleTimer();
	PulseAnimation(Direction > 0 ? UICursorAction::ScrollUp : UICursorAction::ScrollDown);
}

void CUICursor::SetDragging(bool IsDragging)
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	const bool WasDragging = IsNativeDragging;
	if (!AreAnimationsEnabled || WasDragging == IsDragging)
	{
		return;
	}
	ResetIdleTimer();
	IsNativeDragging = IsDragging;
	IsHoldGrab = false;
	if (IsDragging)
	{
		Pulse = UICursorAction::Count;
	}
	else
	{
		IsLeftPressed = false;
		PressedItemKey = nullptr;
	}
	const u32 Mask = CursorAnimations::Bit(UICursorAction::Drag);
	ActionMask = IsDragging ? ActionMask | Mask : ActionMask & ~Mask;
	ActionMask &= ~(CursorAnimations::Bit(UICursorAction::ClickLmb) | CursorAnimations::Bit(UICursorAction::ClickRmb) | CursorAnimations::Bit(UICursorAction::HoverSlotDrop));
	if (!IsDragging)
	{
		if (HoverOwner && HasHoveredItem)
		{
			ActionMask |= CursorAnimations::Bit(IsHoveredButton ? UICursorAction::Clickable : UICursorAction::Select);
		}
		PulseAnimation(UICursorAction::Drop);
	}
	RefreshAnimation();
}

void CUICursor::ReportWidgetDrag()
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	WidgetDragFrame = Device.dwFrame;
}

void CUICursor::SetInventoryHover(const void* Owner, bool IsOver, bool HasItem, bool IsSlot, bool CanDrop, const void* Item, bool IsButton)
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	if (!AreAnimationsEnabled)
	{
		return;
	}
	if (InventoryHoverFrame != Device.dwFrame)
	{
		InventoryHoverFrame = Device.dwFrame;
		PendingInventoryHover = {};
	}
	if (IsOver)
	{
		PendingInventoryHover = {Owner, HasItem, IsButton, IsSlot, CanDrop, Item};
	}
	else if (PendingInventoryHover.Owner == Owner)
	{
		PendingInventoryHover = {};
	}
}

void CUICursor::ApplyInventoryHover(const void* Owner, bool IsOver, bool HasItem, bool IsSlot, bool CanDrop, const void* Item, bool IsButton)
{
	// Resolve all list candidates once before drawing; intermediate focus-loss events never switch SEQ.
	if (!IsOver && !HoverOwner)
	{
		return;
	}
	if (!AreAnimationsEnabled || (!IsOver && HoverOwner != Owner))
	{
		return;
	}
	const bool IsDragging = (ActionMask & (CursorAnimations::Bit(UICursorAction::Drag) | CursorAnimations::Bit(UICursorAction::ScrollDrag))) != 0;
	if (IsOver && HoverOwner == Owner && HasHoveredItem == HasItem && IsHoveredButton == IsButton && IsHoveredSlot == IsSlot && CanDropOnHover == CanDrop && HoverWasDragging == IsDragging && HoveredItemKey == Item)
	{
		return;
	}
	HoverOwner = IsOver ? Owner : nullptr;
	HoveredItemKey = IsOver ? Item : nullptr;
	HasHoveredItem = IsOver && HasItem;
	IsHoveredButton = IsOver && IsButton;
	IsHoveredSlot = IsOver && IsSlot;
	CanDropOnHover = IsOver && CanDrop;
	HoverWasDragging = IsDragging;
	const u32 Mask = CursorAnimations::Bit(UICursorAction::Select) | CursorAnimations::Bit(UICursorAction::Clickable) | CursorAnimations::Bit(UICursorAction::HoverSlotDrop);
	ActionMask &= ~Mask;
	if (IsOver)
	{
		if (IsDragging)
		{
			if (CanDrop && IsNativeDragging)
			{
				ActionMask |= CursorAnimations::Bit(UICursorAction::HoverSlotDrop);
			}
		}
		else if (HasItem)
		{
			ActionMask |= CursorAnimations::Bit(IsHoveredButton ? UICursorAction::Clickable : UICursorAction::Select);
		}
	}
	ResetIdleTimer();
	RefreshAnimation();
}

void CUICursor::OnInventoryItemPressed(const void* Item)
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	if (!AreAnimationsEnabled || !Item)
	{
		return;
	}
	if (!IsLeftPressed)
	{
		IsLeftPressed = true;
		LeftPressTime = Device.dwTimeContinual;
	}
	PressedItemKey = Item;
}

void CUICursor::SetContextMenu(const void* Owner, bool IsOpen)
{
	xrCriticalSectionGuard Lock(AnimationGuard);
	if (!AreAnimationsEnabled)
	{
		return;
	}
	const auto It = std::find(ContextOwners.begin(), ContextOwners.end(), Owner);
	if (IsOpen && It == ContextOwners.end())
	{
		ContextOwners.push_back(Owner);
		Pulse = UICursorAction::Count;
	}
	else if (!IsOpen && It != ContextOwners.end())
	{
		ContextOwners.erase(It);
	}
	const u32 Mask = CursorAnimations::Bit(UICursorAction::Context);
	ActionMask = ContextOwners.empty() ? ActionMask & ~Mask : ActionMask | Mask;
	ResetIdleTimer();
	RefreshAnimation();
}
