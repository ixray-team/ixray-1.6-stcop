#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/ui_defs.h"
#include "../../xrScripts/script_export_space.h"
#include "../../xrEngine/AI/alife_space.h"

class IRenderVisual;
class CInventoryItem;

class CUIWeaponViewport final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	CUIWeaponViewport();
	~CUIWeaponViewport() override;

	void SetItem(ALife::_OBJECT_ID Id);
	ALife::_OBJECT_ID GetItem() const { return ItemId; }

	void Rotate(float DeltaYaw, float DeltaPitch);
	void SetAngles(float NewYaw, float NewPitch);
	float GetYaw() const { return TargetYaw; }
	float GetPitch() const { return TargetPitch; }
	void Zoom(float Steps);
	void SetZoom(float Value);
	float GetZoom() const { return TargetZoom; }
	void ResetView();
	void FocusSlot(ALife::_OBJECT_ID Owner, const char* Bone);
	void ClearFocus();
	void SetAutoRotate(bool Value) { AutoRotate = Value; }
	void SetFov(float Degrees);
	void SetZoomLimits(float Min, float Max);

	u32 GetSlotCount() const { return u32(Slots.size()); }
	ALife::_OBJECT_ID GetSlotOwner(u32 Index) const;
	const char* GetSlotBone(u32 Index) const;
	Fvector2 GetSlotPoint(ALife::_OBJECT_ID Owner, const char* Bone) const;
	bool HasSlotPoint(ALife::_OBJECT_ID Owner, const char* Bone) const;
	int GetHoverSlot() const { return HoverSlot; }
	int PopClickedSlot();
	bool IsDragging() const { return Drag != EDrag::None; }

	void SetCallout(ALife::_OBJECT_ID Owner, const char* Bone, float X, float Y);
	void ClearCallouts() { Callouts.clear(); }
	void SetSelected(ALife::_OBJECT_ID Owner, const char* Bone);
	void SetLineColors(u32 Normal, u32 Selected, u32 Empty);

	void SetPreview(ALife::_OBJECT_ID Owner, const char* Bone, const char* Section);
	void ClearPreview();
	bool HasPreview() const { return Preview.Visual != nullptr; }

	void Draw() override;
	void Update() override;
	bool OnMouseAction(float X, float Y, EUIMessages Action) override;

	static void script_register(lua_State* L);

private:
	enum class EDrag : u8
	{
		None,
		Rotate,
		Pan,
	};

	struct SDrawItem
	{
		IRenderVisual* Visual = nullptr;
		Fmatrix Local = Fidentity;
		Fmatrix World = Fidentity;
		bool IsPreview = false;
	};

	struct SSlot
	{
		ALife::_OBJECT_ID Owner = ALife::INVALID_OBJECT_ID;
		shared_str Bone;
		bool Occupied = false;
		Fvector Local = {};
		Fvector2 Point = {};
		bool Valid = false;
	};

	struct SCallout
	{
		ALife::_OBJECT_ID Owner = ALife::INVALID_OBJECT_ID;
		shared_str Bone;
		Fvector2 Point = {};
	};

	struct SPreview
	{
		ALife::_OBJECT_ID Owner = ALife::INVALID_OBJECT_ID;
		shared_str Bone;
		shared_str Section;
		IRenderVisual* Visual = nullptr;
		Fmatrix Offset = Fidentity;
	};

	CInventoryItem* ResolveItem() const;
	void Collect(CInventoryItem* Item, const Fmatrix& Xform);
	void UpdateBounds();
	void BuildTransforms();
	bool ProjectPoint(const Fvector& World, Fvector2& Result);
	const SSlot* FindSlot(ALife::_OBJECT_ID Owner, const shared_str& Bone) const;
	void DrawCallouts();
	void ReleaseCapture();
	int SlotAt(const Fvector2& Point, float Radius) const;

	ALife::_OBJECT_ID ItemId = ALife::INVALID_OBJECT_ID;
	u32 TreeSignature = 0;

	xr_vector<SDrawItem> Draws;
	xr_vector<SSlot> Slots;
	xr_vector<SCallout> Callouts;
	SPreview Preview;

	Fmatrix ViewMatrix = Fidentity;
	Fmatrix ProjectMatrix = Fidentity;
	Fmatrix ModelXform = Fidentity;
	Fvector BoundsCenter = {};
	float BoundsRadius = 1.0f;
	bool BoundsValid = false;

	float Fov = 20.0f;
	float Yaw = PI * 0.5f;
	float Pitch = 0.0f;
	float TargetYaw = PI * 0.5f;
	float TargetPitch = 0.0f;
	float ZoomValue = 1.0f;
	float TargetZoom = 1.0f;
	float MinZoom = 0.6f;
	float MaxZoom = 4.0f;
	Fvector2 Pan = {};
	Fvector2 TargetPan = {};
	Fvector Focus = {};
	Fvector TargetFocus = {};
	bool HasFocusSlot = false;
	ALife::_OBJECT_ID FocusOwner = ALife::INVALID_OBJECT_ID;
	shared_str FocusBone;
	bool AutoRotate = false;
	u32 IdleTime = 0;
	u32 LastUpdateTime = 0;

	EDrag Drag = EDrag::None;
	Fvector2 DragLast = {};
	float DragDistance = 0.0f;
	EUIMessages DragButtonUp = WINDOW_LBUTTON_UP;

	int HoverSlot = -1;
	int ClickedSlot = -1;
	ALife::_OBJECT_ID SelectedOwner = ALife::INVALID_OBJECT_ID;
	shared_str SelectedBone;

	u32 LineColor = color_rgba(170, 170, 170, 140);
	u32 SelectedColor = color_rgba(215, 205, 90, 255);
	u32 EmptyColor = color_rgba(120, 120, 120, 110);
	ui_shader LineShader;
};
