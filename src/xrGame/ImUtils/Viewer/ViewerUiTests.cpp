#include "StdAfx.h"
#include "ViewerSession.h"
#include "../../../xrEngine/ImGuiManager.h"
#include <imgui_internal.h>
#include "../../../xrCore/Kernel/appinfo.h"

namespace ImUtils::Viewer
{
	namespace ViewerUiTestsDetail
	{
		void QueuePointer(float x, float y, bool pressed)
		{
			SDL_Event event = {};
			event.type = SDL_EVENT_MOUSE_MOTION;
			event.motion.windowID = SDL_GetWindowID(g_AppInfo.Window);
			event.motion.x = x;
			event.motion.y = y;
			event.motion.state = pressed ? SDL_BUTTON_LMASK : 0;
			SDL_PushEvent(&event);
		}

		void QueueButton(float x, float y, bool pressed)
		{
			SDL_Event event = {};
			event.type = pressed ? SDL_EVENT_MOUSE_BUTTON_DOWN : SDL_EVENT_MOUSE_BUTTON_UP;
			event.button.windowID = SDL_GetWindowID(g_AppInfo.Window);
			event.button.button = SDL_BUTTON_LEFT;
			event.button.down = pressed;
			event.button.clicks = 1;
			event.button.x = x;
			event.button.y = y;
			SDL_PushEvent(&event);
		}
	}

	void CViewerSession::UiProbeStart(const char* control, const char* windowName)
	{
		char previousWindow[64];
		xr_strcpy(previousWindow, Lifecycle.Ui.Window);
		if (windowName == nullptr && ImGui::GetCurrentContext() != nullptr && ImGui::GetCurrentContext()->NavWindow != nullptr)
			windowName = ImGui::GetCurrentContext()->NavWindow->Name;
		Lifecycle.Ui = {};
		Lifecycle.Ui.Active = true;
		Lifecycle.Ui.Editor = Lifecycle.ExpectedEditor;
		Lifecycle.RestoreCapture |= !GImGUIManager->IsCapturingInputs();
		xr_strcpy(Lifecycle.Ui.Control, control);
		xr_strcpy(Lifecycle.Ui.Window, windowName != nullptr ? windowName : previousWindow);
	}

	void CViewerSession::UiProbeObserve(const char* control, bool activated)
	{
		SViewerUiProbeState& probe = Lifecycle.Ui;
		if (!Lifecycle.Active || !probe.Active || probe.Activated || probe.Editor != ActivePreset().EditorKind || xr_strcmp(probe.Control, control) != 0)
			return;
		if (activated)
		{
			probe.Activated = true;
			return;
		}
		if (!ImGui::IsItemVisible())
		{
			ImGui::SetScrollHereY(0.5f);
			return;
		}
		const ImVec2 minimum = ImGui::GetItemRectMin();
		const ImVec2 maximum = ImGui::GetItemRectMax();
		probe.Center = { (minimum.x + maximum.x) * 0.5f, (minimum.y + maximum.y) * 0.5f };
		probe.RectangleSeen = true;
		probe.WindowSeen = ImGui::GetCurrentContext()->CurrentWindow != nullptr;
		ImGui::SetWindowFocus();
		ImGui::BringWindowToDisplayFront(ImGui::GetCurrentWindow());
		if (probe.Pressed && !ImGui::IsItemHovered())
		{
			const ImGuiViewport* viewport = ImGui::GetMainViewport();
			const float width = std::max(260.f, viewport->Size.x * 0.25f - 60.f);
			ImGui::SetWindowPos(ImVec2(viewport->Size.x - width - 20.f, 40.f));
			ImGui::SetWindowSize(ImVec2(width, viewport->Size.y - 80.f));
			if (Lifecycle.CheckWait % 60 == 0)
			{
				const ImGuiIO& input = ImGui::GetIO();
				const ImGuiContext& context = *ImGui::GetCurrentContext();
				Msg("! [editor-checks] '%s' not hovered: mouse=[%.1f %.1f] down=%d rect=[%.1f %.1f %.1f %.1f] item=%d current=%s hovered=%s flags=0x%x pos=[%.1f %.1f] dock=%d", probe.Control,
					input.MousePos.x, input.MousePos.y, input.MouseDown[0], minimum.x, minimum.y, maximum.x, maximum.y, ImGui::IsItemHovered(),
					context.CurrentWindow != nullptr ? context.CurrentWindow->Name : "<none>", context.HoveredWindow != nullptr ? context.HoveredWindow->Name : "<none>",
					context.CurrentWindow != nullptr ? context.CurrentWindow->Flags : 0, context.CurrentWindow != nullptr ? context.CurrentWindow->Pos.x : 0.f,
					context.CurrentWindow != nullptr ? context.CurrentWindow->Pos.y : 0.f, context.CurrentWindow != nullptr && context.CurrentWindow->DockNode != nullptr);
				Msg("! [editor-checks] item-id=%u hovered-id=%u active-id=%u item-flags=0x%x status=0x%x nav=%d/%d relaxed=%d content=%d popup=%d", ImGui::GetItemID(), ImGui::GetHoveredID(), context.ActiveId,
					context.LastItemData.ItemFlags, context.LastItemData.StatusFlags, context.NavCursorVisible, context.NavHighlightItemUnderNav,
					ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled | ImGuiHoveredFlags_AllowWhenBlockedByActiveItem | ImGuiHoveredFlags_AllowWhenBlockedByPopup),
					ImGui::IsWindowContentHoverable(ImGui::GetCurrentWindow(), ImGuiHoveredFlags_None), context.OpenPopupStack.Size);
				Msg("! [editor-checks] mouse-owned=%d config=0x%x moving=%s hidden=%d/%d skip=%d capture=%d hovered-viewport=%u", input.MouseDownOwned[0], input.ConfigFlags,
					context.MovingWindow != nullptr ? context.MovingWindow->Name : "<none>", context.CurrentWindow->Hidden, context.CurrentWindow->Active, context.CurrentWindow->SkipItems,
					input.WantCaptureMouse, input.MouseHoveredViewport);
			}
			probe.Pressed = false;
		}
	}

	void CViewerSession::UiProbeTick()
	{
		SViewerUiProbeState& probe = Lifecycle.Ui;
		if (probe.Active)
		{
			GImGUIManager->SetCaptureInputs(true);
		}
		else if (Lifecycle.RestoreCapture)
		{
			GImGUIManager->SetCaptureInputs(false);
			Lifecycle.RestoreCapture = false;
		}
		if (!probe.Active || probe.Activated || ImGui::GetCurrentContext() == nullptr)
			return;
		if (probe.Window[0] != 0)
			ImGui::SetWindowFocus(probe.Window);
		if (!probe.RectangleSeen)
		{
			if (probe.Window[0] == 0)
				ImGui::SetNextWindowFocus();
			if (Lifecycle.CheckWait == 10 || Lifecycle.CheckWait == 120)
				Msg("! [editor-checks] control '%s' has no rectangle: editor=%u active-editor=%u capture=%d gui=%d windows=%d", probe.Control,
					u32(probe.Editor), u32(ActivePreset().EditorKind), GImGUIManager->IsCapturingInputs(), GImGUIManager->IsGUIRendering(), ImGui::GetCurrentContext()->Windows.Size);
			return;
		}
		ViewerUiTestsDetail::QueuePointer(probe.Center.x, probe.Center.y, probe.Pressed);
		if (!probe.Pressed && ImGui::GetActiveID() != 0)
		{
			ViewerUiTestsDetail::QueueButton(probe.Center.x, probe.Center.y, false);
			return;
		}
		if (!probe.Pressed)
		{
			Msg("* [editor-checks] clicking '%s' at %.1f, %.1f", probe.Control, probe.Center.x, probe.Center.y);
			ViewerUiTestsDetail::QueueButton(probe.Center.x, probe.Center.y, true);
			probe.Pressed = true;
		}
		else
		{
			ViewerUiTestsDetail::QueueButton(probe.Center.x, probe.Center.y, false);
		}
	}
}
