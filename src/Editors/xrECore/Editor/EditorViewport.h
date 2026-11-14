#pragma once
#include "UI_Camera.h"

class UIRenderForm;
struct SDL_Window;

// One 3D view of the editor: its camera, the image it shows and the window it lives in.
// Only the active viewport is rendered; the others keep showing their last frame.
class ECORE_API EditorViewport
{
public:
	static constexpr int MainId = 0;
	static constexpr int NoTab = -1;

	explicit EditorViewport(int InId = MainId) : Id(InId) {}

	int Id = MainId;
	UIRenderForm* Form = nullptr;
	CUI_Camera Camera;

	// Final image of the view, sized as the view (Size). Shown by the form, written by TUI::Redraw
	ref_rt Target;
	// Size of the view window in pixels, set by the form every frame it is drawn.
	// Not empty from the start: the forms are drawn by the redraw, which needs a size to run
	Ivector2 Size = {128, 128};
	SDL_Window* Window = nullptr;

	// Camera of the previous rendered frame: the velocity pass reprojects the depth with it
	Fmatrix PrevView;
	Fmatrix PrevProject;
	bool HasPrevTransform = false;

	// Editor tab that shows this view: the view becomes active with the tab. NoTab: activated by focus only
	int TabIndex = NoTab;

	// Called once when the view becomes active / stops being active
	xr_delegate<void()> OnActivate;
	xr_delegate<void()> OnDeactivate;

	bool HasSize() const { return Size.x > 0 && Size.y > 0; }
	bool IsTargetSized() const;
	// Recreates the target at the view size; returns true when it was recreated
	bool UpdateTarget();
	void DestroyTarget() { Target.destroy(); }
};
