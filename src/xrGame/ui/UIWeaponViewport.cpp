#include "StdAfx.h"
#include "pch_script.h"
#include "UIWeaponViewport.h"

#include "../Level.h"
#include "../inventory_item.h"
#include "../../xrUI/UICursor.h"
#include "../../xrUI/ui_base.h"
#include "../../xrEngine/Render.h"
#include "../../xrEngine/vis_common.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../Include/xrRender/UIRender.h"

using namespace luabind;

namespace WeaponViewport
{
constexpr float ViewDistance = 2.0f;
constexpr float RotateSpeed = 0.012f;
constexpr float PitchLimit = PI_DIV_2 * 0.95f;
constexpr float ZoomStep = 1.15f;
constexpr float ClickTolerance = 4.0f;
constexpr float HoverRadius = 18.0f;
constexpr float ClickRadius = 24.0f;
constexpr u32 AutoRotateDelay = 4000;
}

CUIWeaponViewport::CUIWeaponViewport()
{
	LineShader->create("hud\\crosshair");
}

CUIWeaponViewport::~CUIWeaponViewport()
{
	ClearPreview();
}

CInventoryItem* CUIWeaponViewport::ResolveItem() const
{
	if (!g_pGameLevel || ItemId == ALife::INVALID_OBJECT_ID)
	{
		return nullptr;
	}
	CObject* Object = Level().Objects.net_Find(ItemId);
	return Object ? Object->cast_inventory_item() : nullptr;
}

void CUIWeaponViewport::SetItem(ALife::_OBJECT_ID Id)
{
	if (ItemId == Id)
	{
		return;
	}
	ItemId = Id;
	BoundsValid = false;
	TreeSignature = 0;
	ClearPreview();
	ResetView();
}

void CUIWeaponViewport::Rotate(float DeltaYaw, float DeltaPitch)
{
	TargetYaw += DeltaYaw;
	TargetPitch = std::clamp(TargetPitch + DeltaPitch, -WeaponViewport::PitchLimit, WeaponViewport::PitchLimit);
	IdleTime = 0;
}

void CUIWeaponViewport::SetAngles(float NewYaw, float NewPitch)
{
	TargetYaw = NewYaw;
	TargetPitch = std::clamp(NewPitch, -WeaponViewport::PitchLimit, WeaponViewport::PitchLimit);
	IdleTime = 0;
}

void CUIWeaponViewport::Zoom(float Steps)
{
	SetZoom(TargetZoom * powf(WeaponViewport::ZoomStep, Steps));
}

void CUIWeaponViewport::SetZoom(float Value)
{
	TargetZoom = std::clamp(Value, MinZoom, MaxZoom);
	IdleTime = 0;
}

void CUIWeaponViewport::SetZoomLimits(float Min, float Max)
{
	MinZoom = std::max(0.05f, std::min(Min, Max));
	MaxZoom = std::max(MinZoom, std::max(Min, Max));
	SetZoom(TargetZoom);
}

void CUIWeaponViewport::SetFov(float Degrees)
{
	Fov = std::clamp(Degrees, 5.0f, 90.0f);
}

void CUIWeaponViewport::ResetView()
{
	TargetYaw = PI * 0.5f;
	TargetPitch = 0.0f;
	TargetZoom = 1.0f;
	TargetPan.set(0.0f, 0.0f);
	ClearFocus();
	IdleTime = 0;
}

void CUIWeaponViewport::FocusSlot(ALife::_OBJECT_ID Owner, const char* Bone)
{
	HasFocusSlot = Bone && Bone[0];
	FocusOwner = Owner;
	FocusBone = Bone;
	TargetPan.set(0.0f, 0.0f);
	IdleTime = 0;
}

void CUIWeaponViewport::ClearFocus()
{
	HasFocusSlot = false;
	FocusOwner = ALife::INVALID_OBJECT_ID;
	FocusBone = nullptr;
}

ALife::_OBJECT_ID CUIWeaponViewport::GetSlotOwner(u32 Index) const
{
	return Index < Slots.size() ? Slots[Index].Owner : ALife::INVALID_OBJECT_ID;
}

const char* CUIWeaponViewport::GetSlotBone(u32 Index) const
{
	return Index < Slots.size() ? Slots[Index].Bone.c_str() : "";
}

const CUIWeaponViewport::SSlot* CUIWeaponViewport::FindSlot(ALife::_OBJECT_ID Owner, const shared_str& Bone) const
{
	for (const SSlot& Slot : Slots)
	{
		if (Slot.Owner == Owner && Slot.Bone == Bone)
		{
			return &Slot;
		}
	}
	return nullptr;
}

Fvector2 CUIWeaponViewport::GetSlotPoint(ALife::_OBJECT_ID Owner, const char* Bone) const
{
	const SSlot* Slot = FindSlot(Owner, shared_str(Bone));
	return Slot && Slot->Valid ? Slot->Point : Fvector2().set(-10000.0f, -10000.0f);
}

bool CUIWeaponViewport::HasSlotPoint(ALife::_OBJECT_ID Owner, const char* Bone) const
{
	const SSlot* Slot = FindSlot(Owner, shared_str(Bone));
	return Slot && Slot->Valid;
}

int CUIWeaponViewport::PopClickedSlot()
{
	const int Result = ClickedSlot;
	ClickedSlot = -1;
	return Result;
}

void CUIWeaponViewport::SetCallout(ALife::_OBJECT_ID Owner, const char* Bone, float X, float Y)
{
	const shared_str Name = Bone;
	for (SCallout& Callout : Callouts)
	{
		if (Callout.Owner == Owner && Callout.Bone == Name)
		{
			Callout.Point.set(X, Y);
			return;
		}
	}
	Callouts.push_back({ Owner, Name, Fvector2().set(X, Y) });
}

void CUIWeaponViewport::SetSelected(ALife::_OBJECT_ID Owner, const char* Bone)
{
	SelectedOwner = Owner;
	SelectedBone = Bone;
}

void CUIWeaponViewport::SetLineColors(u32 Normal, u32 Selected, u32 Empty)
{
	LineColor = Normal;
	SelectedColor = Selected;
	EmptyColor = Empty;
}

void CUIWeaponViewport::SetPreview(ALife::_OBJECT_ID Owner, const char* Bone, const char* Section)
{
	if (Preview.Visual && Preview.Owner == Owner && Preview.Bone == Bone && Preview.Section == Section)
	{
		return;
	}

	ClearPreview();
	if (!Section || !Section[0] || !pSettings->line_exist(Section, "visual"))
	{
		return;
	}

	IRenderVisual* Visual = ::Render->model_Create(pSettings->r_string(Section, "visual"));
	if (!Visual)
	{
		return;
	}

	if (!PKinematics(Visual))
	{
		::Render->model_Delete(Visual);
		return;
	}

	PKinematics(Visual)->CalculateBones_Invalidate();
	PKinematics(Visual)->CalculateBones(true);

	Preview.Owner = Owner;
	Preview.Bone = Bone;
	Preview.Section = Section;
	Preview.Visual = Visual;
}

void CUIWeaponViewport::ClearPreview()
{
	if (Preview.Visual)
	{
		::Render->model_Delete(Preview.Visual);
	}
	Preview = SPreview();
}

void CUIWeaponViewport::Collect(CInventoryItem* Item, const Fmatrix& Xform)
{
	IKinematics* Model = PKinematics(Item->object().Visual());
	if (!Model)
	{
		return;
	}

	Model->CalculateBones_Invalidate();
	Model->CalculateBones(true);
	Draws.push_back({ Item->object().Visual(), Xform, Fidentity, false });

	for (const auto& [Bone, Sections] : Item->ItemDesc().m_customize_bones)
	{
		const u16 BoneId = Model->LL_BoneID(Bone);
		if (BoneId == BI_NONE)
		{
			continue;
		}

		Fmatrix BoneXform;
		BoneXform.mul_43(Xform, Model->LL_GetTransform(BoneId));

		CInventoryItem* Attached = nullptr;
		for (const auto& [Id, Child] : Item->m_children)
		{
			if (Child->m_attach_bone == Bone)
			{
				Attached = Child;
				break;
			}
		}

		SSlot Slot;
		Slot.Owner = Item->object_id();
		Slot.Bone = Bone;
		Slot.Occupied = Attached != nullptr;
		Slot.Local = BoneXform.c;
		Slots.push_back(Slot);

		if (Preview.Visual && Preview.Owner == Item->object_id() && Preview.Bone == Bone)
		{
			const auto Placement = Item->ItemDesc().m_attach_placements.find(Preview.Section);
			Fmatrix PreviewXform;
			PreviewXform.mul_43(BoneXform, Placement != Item->ItemDesc().m_attach_placements.end() ? Placement->second.m_world : Fidentity);
			PKinematics(Preview.Visual)->CalculateBones(true);
			Draws.push_back({ Preview.Visual, PreviewXform, Fidentity, true });
			continue;
		}

		if (Attached)
		{
			Fmatrix ChildXform;
			ChildXform.mul_43(BoneXform, Attached->m_attach_offset);
			Collect(Attached, ChildXform);
		}
	}
}

void CUIWeaponViewport::UpdateBounds()
{
	Fbox Total;
	Total.invalidate();
	for (const SDrawItem& Item : Draws)
	{
		if (Item.IsPreview)
		{
			continue;
		}
		Fbox Box = Item.Visual->getVisData().box;
		Box.xform(Item.Local);
		Total.merge(Box);
	}

	if (!Total.is_valid())
	{
		BoundsCenter.set(0.0f, 0.0f, 0.0f);
		BoundsRadius = 1.0f;
		BoundsValid = false;
		return;
	}

	Total.getcenter(BoundsCenter);
	BoundsRadius = std::max(Total.getradius(), 0.01f);
	if (!BoundsValid && !HasFocusSlot)
	{
		Focus = BoundsCenter;
		TargetFocus = BoundsCenter;
	}
	BoundsValid = true;
}

void CUIWeaponViewport::BuildTransforms()
{
	const float ScreenWidth = float(Device.TargetWidth);
	const float ScreenHeight = float(Device.TargetHeight);
	const float ScaleX = UI().ClientToScreenScaledX(1.0f);
	const float ScaleY = UI().ClientToScreenScaledY(1.0f);
	if (ScreenWidth < 1.0f || ScreenHeight < 1.0f || ScaleX <= 0.0f || ScaleY <= 0.0f)
	{
		return;
	}

	Frect Rect;
	GetAbsoluteRect(Rect);

	const Fvector2 PixelSize = { Rect.width() * ScaleX, Rect.height() * ScaleY };
	const Fvector2 PixelCenter = { (Rect.left + Rect.width() * 0.5f + Pan.x) * ScaleX, (Rect.top + Rect.height() * 0.5f + Pan.y) * ScaleY };

	ViewMatrix.build_camera_dir(Fidentity.c, Fidentity.k, Fidentity.j);
	ProjectMatrix.build_projection(deg2rad(Fov), ScreenHeight / ScreenWidth, 0.1f, 50.0f);

	Fvector Unit;
	ProjectMatrix.transform(Unit, Fvector().set(1.0f, 1.0f, WeaponViewport::ViewDistance));

	const Fvector2 NdcCenter = { PixelCenter.x / ScreenWidth * 2.0f - 1.0f, 1.0f - PixelCenter.y / ScreenHeight * 2.0f };
	const Fvector2 HalfWorld = { PixelSize.x / ScreenWidth / Unit.x, PixelSize.y / ScreenHeight / Unit.y };
	const float Scale = 0.9f * std::min(HalfWorld.x, HalfWorld.y) / BoundsRadius * ZoomValue;

	Fmatrix Step;
	ModelXform.translate(-Focus.x, -Focus.y, -Focus.z);
	Step.scale(Scale, Scale, Scale);
	ModelXform.mulA_43(Step);
	Step.setHPB(Yaw, Pitch, 0.0f);
	ModelXform.mulA_43(Step);
	Step.translate(NdcCenter.x / Unit.x, NdcCenter.y / Unit.y, WeaponViewport::ViewDistance);
	ModelXform.mulA_43(Step);

	for (SDrawItem& Item : Draws)
	{
		Item.World.mul_43(ModelXform, Item.Local);
	}

	for (SSlot& Slot : Slots)
	{
		Fvector World;
		ModelXform.transform_tiny(World, Slot.Local);
		Slot.Valid = ProjectPoint(World, Slot.Point);
	}
}

bool CUIWeaponViewport::ProjectPoint(const Fvector& World, Fvector2& Result)
{
	if (World.z <= 0.1f)
	{
		return false;
	}

	Fvector Ndc;
	ProjectMatrix.transform(Ndc, World);

	Frect Rect;
	GetAbsoluteRect(Rect);

	const float PixelX = (Ndc.x + 1.0f) * 0.5f * float(Device.TargetWidth);
	const float PixelY = (1.0f - Ndc.y) * 0.5f * float(Device.TargetHeight);
	Result.set(PixelX / UI().ClientToScreenScaledX(1.0f) - Rect.left, PixelY / UI().ClientToScreenScaledY(1.0f) - Rect.top);
	return true;
}

int CUIWeaponViewport::SlotAt(const Fvector2& Point, float Radius) const
{
	int Best = -1;
	float BestDistance = Radius * Radius;
	for (u32 Index = 0; Index < Slots.size(); ++Index)
	{
		const SSlot& Slot = Slots[Index];
		if (!Slot.Valid)
		{
			continue;
		}
		const float DX = Slot.Point.x - Point.x;
		const float DY = Slot.Point.y - Point.y;
		const float Distance = DX * DX + DY * DY;
		if (Distance < BestDistance)
		{
			BestDistance = Distance;
			Best = int(Index);
		}
	}
	return Best;
}

void CUIWeaponViewport::Update()
{
	inherited::Update();

	const u32 Now = Device.dwTimeContinual;
	const u32 Elapsed = LastUpdateTime ? Now - LastUpdateTime : 0;
	LastUpdateTime = Now;
	const float Dt = std::clamp(float(Elapsed) * 0.001f, 0.0f, 0.1f);

	if (Drag == EDrag::None)
	{
		IdleTime += Elapsed;
	}
	if (AutoRotate && IdleTime > WeaponViewport::AutoRotateDelay)
	{
		TargetYaw += Dt * 0.35f;
	}

	if (HasFocusSlot)
	{
		if (const SSlot* Slot = FindSlot(FocusOwner, FocusBone))
		{
			TargetFocus = Slot->Local;
		}
	}
	else if (BoundsValid)
	{
		TargetFocus = BoundsCenter;
	}

	const float Blend = std::clamp(Dt * 10.0f, 0.0f, 1.0f);
	Yaw += (TargetYaw - Yaw) * Blend;
	Pitch += (TargetPitch - Pitch) * Blend;
	ZoomValue += (TargetZoom - ZoomValue) * Blend;
	Pan.x += (TargetPan.x - Pan.x) * Blend;
	Pan.y += (TargetPan.y - Pan.y) * Blend;
	Focus.lerp(Focus, TargetFocus, Blend);

	HoverSlot = -1;
	if (Drag == EDrag::None && IsShown())
	{
		Frect Rect;
		GetAbsoluteRect(Rect);
		const Fvector2 Cursor = GetUICursor().GetCursorPosition();
		if (Rect.in(Cursor.x, Cursor.y))
		{
			HoverSlot = SlotAt(Fvector2().set(Cursor.x - Rect.left, Cursor.y - Rect.top), WeaponViewport::HoverRadius);
		}
	}
}

void CUIWeaponViewport::ReleaseCapture()
{
	if (Drag != EDrag::None && GetParent())
	{
		GetParent()->SetCapture(this, false);
	}
	Drag = EDrag::None;
}

bool CUIWeaponViewport::OnMouseAction(float X, float Y, EUIMessages Action)
{
	const Fvector2 Cursor = { X, Y };
	if (Drag != EDrag::None)
	{
		if (Action == WINDOW_MOUSE_MOVE)
		{
			const Fvector2 Delta = { Cursor.x - DragLast.x, Cursor.y - DragLast.y };
			DragLast = Cursor;
			DragDistance += std::abs(Delta.x) + std::abs(Delta.y);
			if (Drag == EDrag::Rotate)
			{
				Rotate(Delta.x * WeaponViewport::RotateSpeed, Delta.y * WeaponViewport::RotateSpeed);
			}
			else
			{
				TargetPan.add(Delta);
				Pan.add(Delta);
				IdleTime = 0;
			}
		}
		else if (Action == DragButtonUp)
		{
			if (Drag == EDrag::Rotate && DragDistance < WeaponViewport::ClickTolerance)
			{
				ClickedSlot = SlotAt(Cursor, WeaponViewport::ClickRadius);
			}
			ReleaseCapture();
		}
		return true;
	}

	if (inherited::OnMouseAction(X, Y, Action))
	{
		return true;
	}

	switch (Action)
	{
	case WINDOW_LBUTTON_DOWN:
	case WINDOW_RBUTTON_DOWN:
	case WINDOW_CBUTTON_DOWN:
	{
		Drag = Action == WINDOW_LBUTTON_DOWN ? EDrag::Rotate : EDrag::Pan;
		DragButtonUp = Action == WINDOW_LBUTTON_DOWN ? WINDOW_LBUTTON_UP : Action == WINDOW_RBUTTON_DOWN ? WINDOW_RBUTTON_UP : WINDOW_CBUTTON_UP;
		DragLast = Cursor;
		DragDistance = 0.0f;
		IdleTime = 0;
		if (GetParent())
		{
			GetParent()->SetCapture(this, true);
		}
		return true;
	}
	case WINDOW_MOUSE_WHEEL_UP:
		Zoom(1.0f);
		return true;
	case WINDOW_MOUSE_WHEEL_DOWN:
		Zoom(-1.0f);
		return true;
	default:
		break;
	}
	return false;
}

void CUIWeaponViewport::DrawCallouts()
{
	if (Callouts.empty())
	{
		return;
	}

	Frect Rect;
	GetAbsoluteRect(Rect);

	u32 Count = 0;
	for (const SCallout& Callout : Callouts)
	{
		const SSlot* Slot = FindSlot(Callout.Owner, Callout.Bone);
		Count += Slot && Slot->Valid ? 1 : 0;
	}
	if (!Count)
	{
		return;
	}

	UIRender->SetShader(*LineShader);
	UIRender->StartPrimitive(Count * 2, IUIRender::ptLineList, UI().m_currentPointType);
	for (const SCallout& Callout : Callouts)
	{
		const SSlot* Slot = FindSlot(Callout.Owner, Callout.Bone);
		if (!Slot || !Slot->Valid)
		{
			continue;
		}

		const bool Selected = Slot->Owner == SelectedOwner && Slot->Bone == SelectedBone;
		const u32 Color = Selected ? SelectedColor : Slot->Occupied ? LineColor : EmptyColor;

		Fvector2 From;
		Fvector2 To;
		UI().ClientToScreenScaled(From, Rect.left + Slot->Point.x, Rect.top + Slot->Point.y);
		UI().ClientToScreenScaled(To, Rect.left + Callout.Point.x, Rect.top + Callout.Point.y);
		UIRender->PushPoint(From.x, From.y, 0.0f, Color, 0.0f, 0.0f);
		UIRender->PushPoint(To.x, To.y, 0.0f, Color, 0.0f, 0.0f);
	}
	UIRender->FlushPrimitive();
}

void CUIWeaponViewport::Draw()
{
	Draws.clear();
	Slots.clear();

	CInventoryItem* Item = ResolveItem();
	if (!Item)
	{
		inherited::Draw();
		return;
	}

	Collect(Item, Fidentity);

	u32 Signature = u32(Slots.size()) * 31u;
	for (const SDrawItem& Entry : Draws)
	{
		Signature = Signature * 16777619u + (Entry.IsPreview ? 0u : 1u);
	}
	for (const SSlot& Slot : Slots)
	{
		Signature = Signature * 16777619u + (Slot.Occupied ? 7u : 3u);
	}
	if (!BoundsValid || Signature != TreeSignature)
	{
		TreeSignature = Signature;
		UpdateBounds();
	}

	BuildTransforms();

	Frect Rect;
	GetAbsoluteRect(Rect);
	UI().PushScissor(Rect);

	Device.vCameraTop.set(Fidentity.j);
	Device.vCameraRight.set(Fidentity.i);
	Device.vCameraDirection.set(Fidentity.k);
	Device.vCameraPosition.set(Fidentity.c);
	Device.mView = ViewMatrix;
	Device.mProject = ProjectMatrix;
	Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);

	IDSGraphManager* Graph = ::Render->GetUIGraph();
	Graph->set_UI(true);
	for (SDrawItem& Entry : Draws)
	{
		Graph->add_Dynamic(Entry.Visual, &Entry.World, true);
	}
	Graph->set_UI(false);

	Fcolor Color;
	Color.set(1.0f, 1.0f, 1.0f, 1.0f);
	::Render->RenderUI(&Color);

	UI().PopScissor();

	Device.vCameraPosition.set(Device.vCameraPosition_saved);
	Device.vCameraDirection.set(Device.vCameraDirection_saved);
	Device.vCameraRight.set(Device.vCameraRight_saved);
	Device.vCameraTop.set(Device.vCameraTop_saved);
	Device.mView = Device.mView_saved;
	Device.mProject = Device.mProject_saved;
	Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);

	DrawCallouts();
	inherited::Draw();
}

void CUIWeaponViewport::script_register(lua_State* L)
{
	module(L)
	[
		class_<CUIWeaponViewport, CUIWindow>("CUIWeaponViewport")
			.def(constructor<>())
			.def("SetItem", &CUIWeaponViewport::SetItem)
			.def("GetItem", &CUIWeaponViewport::GetItem)
			.def("Rotate", &CUIWeaponViewport::Rotate)
			.def("SetAngles", &CUIWeaponViewport::SetAngles)
			.def("GetYaw", &CUIWeaponViewport::GetYaw)
			.def("GetPitch", &CUIWeaponViewport::GetPitch)
			.def("Zoom", &CUIWeaponViewport::Zoom)
			.def("SetZoom", &CUIWeaponViewport::SetZoom)
			.def("GetZoom", &CUIWeaponViewport::GetZoom)
			.def("SetZoomLimits", &CUIWeaponViewport::SetZoomLimits)
			.def("SetFov", &CUIWeaponViewport::SetFov)
			.def("ResetView", &CUIWeaponViewport::ResetView)
			.def("FocusSlot", &CUIWeaponViewport::FocusSlot)
			.def("ClearFocus", &CUIWeaponViewport::ClearFocus)
			.def("SetAutoRotate", &CUIWeaponViewport::SetAutoRotate)
			.def("GetSlotCount", &CUIWeaponViewport::GetSlotCount)
			.def("GetSlotOwner", &CUIWeaponViewport::GetSlotOwner)
			.def("GetSlotBone", &CUIWeaponViewport::GetSlotBone)
			.def("GetSlotPoint", &CUIWeaponViewport::GetSlotPoint)
			.def("HasSlotPoint", &CUIWeaponViewport::HasSlotPoint)
			.def("GetHoverSlot", &CUIWeaponViewport::GetHoverSlot)
			.def("PopClickedSlot", &CUIWeaponViewport::PopClickedSlot)
			.def("IsDragging", &CUIWeaponViewport::IsDragging)
			.def("SetCallout", &CUIWeaponViewport::SetCallout)
			.def("ClearCallouts", &CUIWeaponViewport::ClearCallouts)
			.def("SetSelected", &CUIWeaponViewport::SetSelected)
			.def("SetLineColors", &CUIWeaponViewport::SetLineColors)
			.def("SetPreview", &CUIWeaponViewport::SetPreview)
			.def("ClearPreview", &CUIWeaponViewport::ClearPreview)
			.def("HasPreview", &CUIWeaponViewport::HasPreview)
	];
}
