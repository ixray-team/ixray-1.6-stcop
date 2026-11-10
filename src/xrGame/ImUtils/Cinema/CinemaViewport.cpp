#include "StdAfx.h"
#include "CinemaEditor.h"
#include "../../../xrEngine/ImGuiManager.h"

namespace Cinema::ViewportUi
{
constexpr float LookRate = 0.003f, PanRate = 0.0025f, DollyRate = 0.25f, MaxPitch = 1.55f, KeyRadius = 0.12f, ArrowLength = 0.6f;
constexpr float RollRate = 0.7853982f, FovRate = 30.0f, MinFov = 5.0f, MaxFov = 150.0f;
constexpr float PickRange = 1000.0f, PickPixels = 32.0f, CubeSize = 128.0f, CubeLength = 8.0f;
constexpr float MarkerRadius = 0.3f, GrabPixels = 8.0f, HandlePixels = 5.0f;
constexpr u32 CircleSegments = 24, FaintAlpha = 90;
constexpr u32 ActorBit = 1u << static_cast<u32>(ObjectType::Actor), SpotBit = 1u << static_cast<u32>(ObjectType::Light), ParticleBit = 1u << static_cast<u32>(ObjectType::Particle), PointBit = 1u << 4;
constexpr const char* GizmoLabels[3] = { "Move", "Rotate", "Scale" };
constexpr ImGuizmo::OPERATION Operations[3] = { ImGuizmo::TRANSLATE, ImGuizmo::ROTATE, ImGuizmo::SCALE };
constexpr Param ScaleParams[4] = { Param::Scale, Param::Scale, Param::Range, Param::Size };

float Project(const Fvector& Point, ImVec2& Screen)
{
	Fvector4 Clip;
	Device.mFullTransform.transform(Clip, Point);
	if (!(Clip.w > EPS_S))
	{
		return 0.0f;
	}
	const ImGuiViewport* Main = ImGui::GetMainViewport();
	Screen = ImVec2(Main->Pos.x + (Clip.x + 1.0f) * 0.5f * Main->Size.x, Main->Pos.y + (1.0f - Clip.y) * 0.5f * Main->Size.y);
	return 0.5f * Main->Size.y * Device.mProject._22 / Clip.w;
}

void Line3D(ImDrawList* List, Fvector From, Fvector To, u32 Color)
{
	const Fmatrix& Full = Device.mFullTransform;
	const auto Depth = [&Full](const Fvector& Point) { return Point.x * Full._14 + Point.y * Full._24 + Point.z * Full._34 + Full._44; };
	const float Near = Device.fViewportNear, A = Depth(From), B = Depth(To);
	if (A < Near && B < Near)
	{
		return;
	}
	if (A < Near)
	{
		From.lerp(From, To, (Near - A) / (B - A));
	}
	if (B < Near)
	{
		To.lerp(To, From, (Near - B) / (A - B));
	}
	ImVec2 Start, End;
	if (Project(From, Start) > 0.0f && Project(To, End) > 0.0f)
	{
		List->AddLine(Start, End, Color, 1.5f);
	}
}

bool MouseRay(Fvector& Origin, Fvector& Direction)
{
	const ImGuiViewport* Main = ImGui::GetMainViewport();
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	if (!(Main->Size.x > 0.0f && Main->Size.y > 0.0f))
	{
		return false;
	}
	const float X = 2.0f * (Mouse.x - Main->Pos.x) / Main->Size.x - 1.0f, Y = 1.0f - 2.0f * (Mouse.y - Main->Pos.y) / Main->Size.y;
	Fvector Near, Far;
	Device.mInvFullTransform.transform(Near, Fvector().set(X, Y, 0.0f));
	Device.mInvFullTransform.transform(Far, Fvector().set(X, Y, 1.0f));
	Direction.sub(Far, Near);
	if (!(Direction.square_magnitude() > EPS))
	{
		return false;
	}
	Origin = Near;
	Direction.normalize();
	return true;
}

Fmatrix Compose(const Fvector (&Values)[3])
{
	Fmatrix Result;
	Result.setHPB(deg2rad(Values[1].x), deg2rad(Values[1].y), deg2rad(Values[1].z));
	Result.i.mul(Values[2].x);
	Result.j.mul(Values[2].y);
	Result.k.mul(Values[2].z);
	Result.c = Values[0];
	return Result;
}

void Decompose(Fmatrix Source, Fvector (&Values)[3])
{
	Values[0] = Source.c;
	Values[2].set(Source.i.magnitude(), Source.j.magnitude(), Source.k.magnitude());
	Source.i.div(Values[2].x);
	Source.j.div(Values[2].y);
	Source.k.div(Values[2].z);
	Source.getHPB(Values[1]);
	Values[1].mul(rad2deg(1.0f));
}

void Mirror(Fmatrix& Value)
{
	for (u32 Index : { 0u, 1u, 3u })
	{
		Value.m[Index][2] = -Value.m[Index][2];
		Value.m[2][Index] = -Value.m[2][Index];
	}
}

void Place(const LinkFrames& Frames, Fvector& Position, Fmatrix& Turn)
{
	Turn.c = Position;
	Spaces::Compose(Frames, Turn);
	Position = Turn.c;
	Turn.c.set(0.0f, 0.0f, 0.0f);
}

void LinkLines(ImDrawList* List, const LinkFrames& Frames, const Fvector& From, u32 Color)
{
	if (Frames.HasParent)
	{
		Line3D(List, From, Frames.Parent.c, Color);
	}
	if (Frames.HasLook)
	{
		Line3D(List, From, Frames.Look, Color);
	}
}

void Circle3D(ImDrawList* List, const Fvector& Centre, const Fvector& AxisA, const Fvector& AxisB, float Radius, u32 Color)
{
	Fvector From = Fvector().mad(Centre, AxisA, Radius);
	for (u32 Part = 1; Part <= CircleSegments; ++Part)
	{
		const float Angle = PI_MUL_2 * static_cast<float>(Part) / static_cast<float>(CircleSegments);
		const Fvector To = Fvector().mad(Centre, AxisA, Radius * std::cos(Angle)).mad(AxisB, Radius * std::sin(Angle));
		Line3D(List, From, To, Color);
		From = To;
	}
}

bool AxisPoint(const Fvector& Centre, const Fvector& Axis, float& Along)
{
	Fvector Origin, Direction;
	if (!MouseRay(Origin, Direction))
	{
		return false;
	}
	const Fvector Gap = Fvector().sub(Centre, Origin);
	const float Cos = Axis.dotproduct(Direction), Spread = 1.0f - Cos * Cos;
	if (!(Spread > 1e-4f))
	{
		return false;
	}
	Along = (Cos * Direction.dotproduct(Gap) - Axis.dotproduct(Gap)) / Spread;
	return true;
}

const ObjectState* HelperState(const Player& Core, const Object& Item)
{
	const ObjectState* State = Item.Type == ObjectType::Light || Item.Type == ObjectType::Particle ? Core.FindState(Item.Id) : nullptr;
	return State && State->IsReady ? State : nullptr;
}

bool IsSkipped(const Object& Item, GizmoMode Mode)
{
	return (Item.Type == ObjectType::Light && (Mode == GizmoMode::Scale || (Mode == GizmoMode::Rotate && !IsParamShown(Item, Param::Rotation)))) || (Item.Type == ObjectType::Camera && Mode == GizmoMode::Scale);
}

void DrawEffectHelper(ImDrawList* List, const Object& Item, const Fmatrix& World, float Time, u32 Tint)
{
	const float Tau = Time - Item.Span.Start, Local = std::clamp(Tau, 0.0f, Item.Span.Length);
	const u32 Color = Item.IsMuted || Tau < 0.0f || Tau >= Item.Span.Length ? (Tint & 0x00FFFFFFu) | (FaintAlpha << 24) : Tint;
	const float Range = Curves::LaneScalar(Item.Lanes, Param::Range, 0, Local, Item.Light.Range);
	ImVec2 Centre;
	const float Scale = Project(World.c, Centre), Radius = std::max(3.0f, MarkerRadius * Scale);
	if (Item.Type == ObjectType::Light && Item.Light.Kind == LightKind::Spot)
	{
		const Fvector Base = Fvector().mad(World.c, World.k, Range);
		const float Width = Range * std::tan(0.5f * deg2rad(Curves::LaneScalar(Item.Lanes, Param::Cone, 0, Local, Item.Light.Cone)));
		Circle3D(List, Base, World.i, World.j, Width, Color);
		for (u32 Side = 0; Side < 4; ++Side)
		{
			Line3D(List, World.c, Fvector().mad(Base, Side < 2 ? World.i : World.j, Side & 1 ? -Width : Width), Color);
		}
		return;
	}
	if (Item.Type == ObjectType::Light)
	{
		Circle3D(List, World.c, Device.vCameraRight, Device.vCameraTop, Range, Color);
	}
	if (!(Scale > 0.0f))
	{
		return;
	}
	List->AddCircle(Centre, Radius, Color, 0, 1.5f);
	if (Item.Type == ObjectType::Particle)
	{
		const float Arm = 0.7071f * Radius;
		List->AddLine(ImVec2(Centre.x - Arm, Centre.y - Arm), ImVec2(Centre.x + Arm, Centre.y + Arm), Color, 1.5f);
		List->AddLine(ImVec2(Centre.x - Arm, Centre.y + Arm), ImVec2(Centre.x + Arm, Centre.y - Arm), Color, 1.5f);
		List->AddText(ImVec2(Centre.x + Radius + 2.0f, Centre.y + 2.0f), Color, Item.Name.c_str());
	}
}

void DrawFocus(ImDrawList* List, const CameraPose& Lens, const char* Target, u32 Color)
{
	const float Half = std::tan(deg2rad(Lens.Fov > 0.0f ? Lens.Fov : GameFov()) * 0.5f);
	const float Depths[3] = { Lens.Dof.x, Lens.Dof.y, std::min(Lens.Dof.z, PickRange) };
	for (u32 Index = 0; Index < 3; ++Index)
	{
		const float Depth = Depths[Index];
		const Fvector Centre = Fvector().mad(Lens.Position, Lens.Orientation.k, Depth);
		const Fvector Up = Fvector().mul(Lens.Orientation.j, Depth * Half), Side = Fvector().mul(Lens.Orientation.i, Depth * Half / Device.fASPECT);
		const Fvector Corners[4] = { Fvector().add(Centre, Up).add(Side), Fvector().add(Centre, Up).sub(Side), Fvector().sub(Centre, Up).sub(Side), Fvector().sub(Centre, Up).add(Side) };
		for (u32 Edge = 0; Depth > 0.05f && Edge < 4; ++Edge)
		{
			Line3D(List, Corners[Edge], Corners[(Edge + 1) % 4], Index == 1 ? Color : (Color & 0x00FFFFFFu) | (FaintAlpha << 24));
		}
	}
	ImVec2 Spot;
	if (Project(Lens.Position, Spot) > 0.0f)
	{
		string256 Text;
		xr_sprintf(Text, "Focus %s%.1f m", Target, Lens.Dof.y);
		List->AddText(ImVec2(Spot.x + 8.0f, Spot.y + 8.0f), Color, Text);
	}
}
}

namespace Cinema
{
void Editor::UseEditorCamera(Player& Core)
{
	if (!IsEyeValid || Core.View() == ViewSource::Global)
	{
		const bool IsShot = Core.View() == ViewSource::Global;
		Fmatrix Turn = Fidentity;
		Turn.i = Device.vCameraRight;
		Turn.j = Device.vCameraTop;
		Turn.k = Device.vCameraDirection;
		float Heading = 0.0f, Tilt = 0.0f, Bank = 0.0f;
		Turn.getHPB(Heading, Tilt, Bank);
		Roll = IsShot ? Bank : 0.0f;

		Eye = Device.vCameraPosition;
		Device.vCameraDirection.getHP(Yaw, Pitch);
		EyeFov = IsShot ? std::clamp(Device.fFOV, ViewportUi::MinFov, ViewportUi::MaxFov) : 0.0f;
		IsEyeValid = true;
	}
	IsEditorCamera = true;
	Core.IsLookingThrough = false;
}

bool Editor::IsViewportFree() const
{
	return CImGuiManager::Instance().IsCapturingInputs() && !ImGui::IsWindowHovered(ImGuiHoveredFlags_AnyWindow) && !ImGui::IsAnyItemActive() && !ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId | ImGuiPopupFlags_AnyPopupLevel) && !IsGizmoActive && !IsGizmoCancelled && !IsTangentActive && !IsTangentCancelled && !IsPickingColor && !IsPickingFocus && !ImGuizmo::IsUsingViewManipulate();
}

void Editor::UpdateFly(Player& Core)
{
	const ImGuiIO& Input = ImGui::GetIO();
	const bool IsCapturing = CImGuiManager::Instance().IsCapturingInputs();
	const bool IsFree = IsViewportFree();
	const bool WasFlying = IsFlying, WasArmed = IsFlyArmed;
	IsFlyArmed = IsCapturing && (IsFlyArmed ? ImGui::IsMouseDown(ImGuiMouseButton_Right) : IsFree && ImGui::IsMouseClicked(ImGuiMouseButton_Right));
	IsPanning = IsCapturing && (IsPanning ? ImGui::IsMouseDown(ImGuiMouseButton_Middle) : IsFree && ImGui::IsMouseClicked(ImGuiMouseButton_Middle));
	IsFlying = IsFlyArmed && (IsFlying || ImGui::IsMouseDragging(ImGuiMouseButton_Right, 4.0f));
	ShouldOpenPopup = WasArmed && !IsFlyArmed && !WasFlying && ImGui::IsMouseReleased(ImGuiMouseButton_Right);
	const Object* Rig = FindCamera(Core.Data);
	if (WasFlying && !IsFlying && IsAutoKey && Rig && IsSelected(Rig->Id))
	{
		KeyFromView(Core, Core.Time, true);
	}

	const float Wheel = IsFlyArmed || IsFree ? Input.MouseWheel : 0.0f;
	if ((IsFlying && !WasFlying) || (IsPanning && ImGui::IsMouseClicked(ImGuiMouseButton_Middle)) || (Wheel != 0.0f && !IsFlyArmed))
	{
		UseEditorCamera(Core);
	}
	FlyFov = IsFlying && !WasFlying ? EyeFov : FlyFov;

	if (IsFlying != WasFlying)
	{
		CImGuiManager::Instance().SetMouseLook(IsFlying);
		ImGui::TeleportMousePos(Input.MouseClickedPos[ImGuiMouseButton_Right]);
	}
	const auto Axis = [&Input](ImGuiKey Plus, ImGuiKey Minus) { return Input.WantTextInput ? 0.0f : static_cast<float>(ImGui::IsKeyDown(Plus)) - static_cast<float>(ImGui::IsKeyDown(Minus)); };
	const float Boost = IsFlying ? (Input.KeyShift ? 4.0f : 1.0f) * Input.DeltaTime : 0.0f;
	if (IsFlying)
	{
		const ImVec2 Look = CImGuiManager::Instance().TakeMouseLook();
		Yaw -= Look.x * ViewportUi::LookRate;
		Pitch = std::clamp(Pitch - Look.y * ViewportUi::LookRate, -ViewportUi::MaxPitch, ViewportUi::MaxPitch);
		Roll = std::remainder(Roll + Axis(ImGuiKey_E, ImGuiKey_Q) * ViewportUi::RollRate * Boost, PI_MUL_2);
		const float Zoom = Axis(ImGuiKey_T, ImGuiKey_R);
		EyeFov = Zoom != 0.0f ? std::clamp((EyeFov > 0.0f ? EyeFov : GameFov()) + Zoom * ViewportUi::FovRate * Boost, ViewportUi::MinFov, ViewportUi::MaxFov) : EyeFov;
		ImGui::SetMouseCursor(ImGuiMouseCursor_None);
	}
	Fmatrix View, Level;
	View.setHPB(Yaw, Pitch, Roll);
	Level.setHPB(Yaw, Pitch, 0.0f);
	const float Step = Settings.FlySpeed * Boost;
	const float Pan = IsPanning ? Settings.FlySpeed * ViewportUi::PanRate : 0.0f;
	Eye.mad(Level.k, Axis(ImGuiKey_W, ImGuiKey_S) * Step).mad(Level.i, Axis(ImGuiKey_D, ImGuiKey_A) * Step);
	Eye.y += Axis(ImGuiKey_Space, ImGuiKey_C) * Step;
	Eye.mad(View.i, -Input.MouseDelta.x * Pan).mad(View.j, Input.MouseDelta.y * Pan);
	if (IsFlyArmed)
	{
		Settings.FlySpeed = std::clamp(Settings.FlySpeed * std::pow(1.25f, Wheel), 0.25f, 64.0f);
	}
	else
	{
		Eye.mad(View.k, Wheel * Settings.FlySpeed * ViewportUi::DollyRate);
	}
	Core.SetEditorView(true, IsEditorCamera, Eye, View, EyeFov);
}

void Editor::CollectGizmo(const Player& Core, bool HasCamera, GizmoMode Mode)
{
	GizmoItems.clear();
	Fvector Sum = Fvector().set(0.0f, 0.0f, 0.0f);
	for (u32 Id : Selected)
	{
		const Object* Target = Find(Core.Data, Id);
		GizmoItem Item;
		Item.Id = Id;
		Item.Values[2].set(1.0f, 1.0f, 1.0f);
		const float Local = Target ? Core.Time - Target->Span.Start : 0.0f;
		const Attachment* Links = Target ? &Target->Attach : nullptr;
		const bool IsLight = Target && Target->Type == ObjectType::Light;
		if (Target && ViewportUi::IsSkipped(*Target, Mode))
		{
			continue;
		}
		if (Target && Target->Type != ObjectType::Camera)
		{
			const ObjectState* State = Core.FindState(Id);
			const float Size = Curves::LaneScalar(Target->Lanes, IsLight ? Param::Range : Param::Size, 0, Local, IsLight ? Target->Light.Range : Target->Particle.Size);
			Item.Values[0] = Curves::LaneVector(Target->Lanes, Param::Position, Local, *PlacementOf(*Target, 0));
			Item.Values[1] = IsParamShown(*Target, Param::Rotation) ? Curves::LaneVector(Target->Lanes, Param::Rotation, Local, *PlacementOf(*Target, 1)) : Fvector().set(0.0f, 0.0f, 0.0f);
			Item.Values[2] = Target->Type == ObjectType::Actor ? Curves::LaneVector(Target->Lanes, Param::Scale, Local, Target->Actor.Scale) : Target->Type == ObjectType::Particle || Mode == GizmoMode::Range ? Fvector().set(Size, Size, Size) : Item.Values[2];
			Item.Frames = State ? State->Frames : LinkFrames {};
		}
		else
		{
			Item.Block = static_cast<s16>(Target && HasCamera ? Cameras::BlockAt(Target->Camera, Local) : -1);
			PoseKey Pose;
			u32 At = u32(-1);
			const BlockCache* Cache = Item.Block < 0 || Target->Camera.Blocks[Item.Block].Keys.empty() ? nullptr : PoseAt(Core, Target->Camera.Blocks[Item.Block], Item.Block, Core.Time - TargetStart(*Target, Item.Block), Pose, At);
			if (!Cache)
			{
				continue;
			}
			Item.Values[0] = Pose.Position;
			Item.Values[1] = Pose.Rotation;
			Item.Frames = Cache->Frames;
			Links = &Target->Camera.Blocks[Item.Block].Attach;
		}
		const bool IsLed = Mode != GizmoMode::Range && HasSelectedParent(Core.Data, Links->Parent.ObjectId, Mode);
		if (IsLed && Mode != GizmoMode::Universal)
		{
			continue;
		}
		Item.Leader = IsLed ? Links->Parent.ObjectId : 0;
		Item.Start = ViewportUi::Compose(Item.Values);
		Spaces::Compose(Item.Frames, Item.Start);
		Sum.add(Item.Start.c);
		GizmoItems.push_back(Item);
	}
	if (GizmoItems.size() > 1)
	{
		GizmoPivot = IsGizmoLocalMulti ? GizmoItems[0].Start : Fidentity;
		Spaces::Orthonormalize(GizmoPivot);
		GizmoPivot.c.div(Sum, static_cast<float>(GizmoItems.size()));
	}
	else if (!GizmoItems.empty())
	{
		GizmoPivot = GizmoItems[0].Start;
	}
}

void Editor::PutTransform(Player& Core, GizmoItem& Item, u32 Kind, const Fvector& NewValue)
{
	Object* Target = Find(Core.Data, Item.Id);
	const Param Id = Target && Kind == 2 ? ViewportUi::ScaleParams[static_cast<u32>(Target->Type)] : static_cast<Param>(Kind);
	const u8 Count = Info(Id).Components;
	const Fvector Moved = Fvector().sub(NewValue, Item.Values[Kind]);
	const u32 Most = std::fabs(Moved.x) >= std::max(std::fabs(Moved.y), std::fabs(Moved.z)) ? 0 : std::fabs(Moved.y) >= std::fabs(Moved.z) ? 1 : 2;
	for (u32 Component = 0; Target && Component < Count; ++Component)
	{
		const float Shift = Moved[Count == 3 ? Component : Most];
		const float Delta = Kind == 1 ? std::remainder(Shift, 360.0f) : Shift;
		const u16 Bit = static_cast<u16>(1u << (Kind * 3 + Component));
		if (std::fabs(Delta) > 1e-4f || (Item.Written & Bit))
		{
			Item.Written |= Bit;
			WriteParam(Core, *Target, Item.Block, Id, 0, static_cast<u8>(Component), Delta, true);
		}
	}
}

void Editor::PutWorld(Player& Core, GizmoItem& Item, Fmatrix World, u32 Kinds)
{
	const Object* Target = Find(Core.Data, Item.Id);
	Kinds &= Target && !IsParamShown(*Target, Param::Rotation) ? 1u : 3u;
	Fvector Values[3];
	Spaces::Decompose(Item.Frames, World);
	ViewportUi::Decompose(World, Values);
	for (u32 Kind = 0; Kind < 2; ++Kind)
	{
		if ((Kinds >> Kind) & 1u)
		{
			PutTransform(Core, Item, Kind, Values[Kind]);
		}
	}
}

bool Editor::HasSelectedParent(const Scene& Data, u32 Parent, GizmoMode Mode) const
{
	for (u32 Depth = 0; Parent && Depth <= MaxAttachDepth; ++Depth)
	{
		const Object* Item = Find(Data, Parent);
		if (IsSelected(Parent) && !(Item && ViewportUi::IsSkipped(*Item, Mode)))
		{
			return true;
		}
		Parent = Item ? Item->Attach.Parent.ObjectId : 0;
	}
	return false;
}

void Editor::DrawGizmo(Player& Core)
{
	const ImGuiIO& Input = ImGui::GetIO();
	const ImGuiViewport* Main = ImGui::GetMainViewport();
	ImGuizmo::BeginFrame();
	ImGui::SetNextWindowPos(Main->Pos);
	ImGui::SetNextWindowSize(Main->Size);
	ImGui::SetNextWindowBgAlpha(0.0f);
	ImGui::Begin("##CinemaOverlay", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoFocusOnAppearing | ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoNav);
	Overlay = ImGui::GetWindowDrawList();
	ImGui::End();
	ImGuizmo::SetDrawlist(Overlay);
	ImGuizmo::SetRect(Main->Pos.x, Main->Pos.y, Main->Size.x, Main->Size.y);
	ImGuizmo::SetOrthographic(false);
	IsGizmoHovered = false;
	IsGizmoCancelled = IsGizmoCancelled && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	if (IsGizmoActive && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		Undo.Cancel(Core.Data);
		IsGizmoCancelled = true;
		IsGizmoActive = false;
	}
	ImGuizmo::Enable(!IsGizmoCancelled && !IsTangentActive && !IsPickingColor && !IsPickingFocus && CImGuiManager::Instance().IsCapturingInputs() && !ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId | ImGuiPopupFlags_AnyPopupLevel));
	u32 Kinds = 0;
	for (u32 Id : Selected)
	{
		const Object* Item = Find(Core.Data, Id);
		Kinds |= !Item ? 0u : IsParamShown(*Item, Param::Rotation) ? 1u << static_cast<u32>(Item->Type) : ViewportUi::PointBit;
	}
	const bool IsScaleless = Gizmo == GizmoMode::Scale && !(Kinds & (ViewportUi::ActorBit | ViewportUi::ParticleBit));
	const bool IsRange = IsScaleless && (Kinds & (ViewportUi::SpotBit | ViewportUi::PointBit));
	const u32 Mode = static_cast<u32>(IsRange ? GizmoMode::Range : IsScaleless || (Gizmo == GizmoMode::Rotate && !(Kinds & ~ViewportUi::PointBit)) ? GizmoMode::Translate : Gizmo);
	const bool IsUniversal = Mode == static_cast<u32>(GizmoMode::Universal), IsPose = Mode < 2 || IsUniversal;
	GizmoShown = Kinds && !IsUniversal ? static_cast<GizmoMode>(std::min(Mode, 2u)) : Gizmo;
	if (!IsGizmoActive)
	{
		CollectGizmo(Core, IsPose && Core.View() != ViewSource::Global, static_cast<GizmoMode>(Mode));
		IsGizmoRelative = GizmoItems.size() > 1 || (GizmoItems.size() == 1 && IsGizmoLocalSingle && IsPose && GizmoItems[0].Frames.HasParent);
		if (GizmoItems.size() == 1 && IsGizmoRelative)
		{
			GizmoPivot = GizmoItems[0].Frames.Parent;
			GizmoPivot.c = GizmoItems[0].Start.c;
		}
	}
	if (GizmoItems.empty())
	{
		IsGizmoActive = false;
		return;
	}
	if (IsRange)
	{
		DrawRangeHandle(Core);
		return;
	}
	const auto TakePart = [this, Mode, IsUniversal]
	{
		const ImGuizmo::OPERATION Using = ImGuizmo::GetUsingOperation();
		GizmoPart = !IsUniversal ? Mode : !Using ? GizmoPart : Using == ImGuizmo::ROTATE ? 1u : Using == ImGuizmo::SCALE ? 2u : 0u;
	};
	TakePart();
	const float Step = GizmoPart == 0 ? Settings.MoveStep : GizmoPart == 1 ? Settings.RotateStep : Settings.ScaleStep;
	const float Snap[3] = { Step, Step, Step };
	const bool IsLocal = GizmoItems.size() > 1 ? IsGizmoLocalMulti : IsGizmoLocalSingle;
	const Fmatrix Before = GizmoPivot;
	const u32 Scale = (Kinds & ViewportUi::ActorBit) && (IsLocal || !IsUniversal) ? ImGuizmo::SCALE : ImGuizmo::SCALEU;
	const u32 Operation = IsUniversal ? ImGuizmo::TRANSLATE | (Kinds & ~ViewportUi::PointBit ? ImGuizmo::ROTATE : 0u) | (Kinds & (ViewportUi::ActorBit | ViewportUi::ParticleBit) ? Scale : 0u) : Mode == 2 ? Scale : ViewportUi::Operations[Mode];
	ImGuizmo::Manipulate(reinterpret_cast<const float*>(&Device.mView), reinterpret_cast<const float*>(&Device.mProject), static_cast<ImGuizmo::OPERATION>(Operation), IsLocal ? ImGuizmo::LOCAL : ImGuizmo::WORLD, reinterpret_cast<float*>(&GizmoPivot), nullptr, Input.KeyCtrl ? nullptr : Snap);
	IsGizmoHovered = ImGuizmo::IsOver();
	const bool IsUsing = ImGuizmo::IsUsing();
	TakePart();
	if (!IsUsing && !IsGizmoActive)
	{
		return;
	}
	if (!IsGizmoActive)
	{
		GizmoInverse.invert(Before);
		Undo.Begin(ViewportUi::GizmoLabels[GizmoPart]);
		for (const GizmoItem& Item : GizmoItems)
		{
			Undo.Touch(Core.Data, Item.Id);
		}
	}
	IsGizmoActive = IsUsing;
	for (GizmoItem& Item : GizmoItems)
	{
		Fmatrix Relative, Moved = GizmoPivot, Started = Item.Start;
		if (IsGizmoRelative)
		{
			Relative.mul(GizmoInverse, Item.Start);
			Moved.mul(GizmoPivot, Relative);
		}
		Spaces::Decompose(Item.Frames, Moved);
		Spaces::Decompose(Item.Frames, Started);
		Fvector Now[3], Was[3];
		ViewportUi::Decompose(Moved, Now);
		ViewportUi::Decompose(Started, Was);
		const Object* Target = Find(Core.Data, Item.Id);
		if (!(Target && ViewportUi::IsSkipped(*Target, static_cast<GizmoMode>(GizmoPart))) && !(Item.Leader && HasSelectedParent(Core.Data, Item.Leader, static_cast<GizmoMode>(GizmoPart))))
		{
			PutTransform(Core, Item, GizmoPart, Fvector().sub(Now[GizmoPart], Was[GizmoPart]).add(Item.Values[GizmoPart]));
		}
		if (GizmoPart && GizmoItems.size() > 1 && !Item.Leader)
		{
			PutTransform(Core, Item, 0, Now[0]);
		}
	}
}

void Editor::DrawRangeHandle(Player& Core)
{
	ImDrawList* List = Overlay;
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	float Nearest = ViewportUi::GrabPixels * ViewportUi::GrabPixels;
	const GizmoItem* Grab = nullptr;
	Fvector GrabAxis = RangeAxis;
	for (const GizmoItem& Item : GizmoItems)
	{
		const Object* Target = Find(Core.Data, Item.Id);
		if (!Target)
		{
			continue;
		}
		const Fvector Axis = Target->Light.Kind == LightKind::Spot ? Fvector().normalize_safe(Item.Start.k) : Device.vCameraRight;
		const float Range = Curves::LaneScalar(Target->Lanes, Param::Range, 0, Core.Time - Target->Span.Start, Target->Light.Range);
		ImVec2 Screen;
		if (!(ViewportUi::Project(Fvector().mad(Item.Start.c, Axis, Range), Screen) > 0.0f))
		{
			continue;
		}
		List->AddCircleFilled(Screen, ViewportUi::HandlePixels, Lighten(Target->Color));
		const float Gap = (Screen.x - Mouse.x) * (Screen.x - Mouse.x) + (Screen.y - Mouse.y) * (Screen.y - Mouse.y);
		if (Gap < Nearest)
		{
			Nearest = Gap;
			Grab = &Item;
			GrabAxis = Axis;
		}
	}
	IsGizmoHovered = Grab != nullptr;
	if (!IsGizmoActive && Grab && ImGui::IsMouseClicked(ImGuiMouseButton_Left) && IsViewportFree() && ViewportUi::AxisPoint(Grab->Start.c, GrabAxis, RangeStart))
	{
		RangeCentre = Grab->Start.c;
		RangeAxis = GrabAxis;
		Undo.Begin("Range");
		for (const GizmoItem& Item : GizmoItems)
		{
			Undo.Touch(Core.Data, Item.Id);
		}
		IsGizmoActive = true;
	}
	float Along = 0.0f;
	if (IsGizmoActive && ViewportUi::AxisPoint(RangeCentre, RangeAxis, Along))
	{
		const float Shift = Along - RangeStart;
		const float Delta = ImGui::GetIO().KeyCtrl ? Shift : std::round(Shift / Settings.RangeStep) * Settings.RangeStep;
		for (GizmoItem& Item : GizmoItems)
		{
			PutTransform(Core, Item, 2, Fvector().set(Delta, Delta, Delta).add(Item.Values[2]));
		}
	}
	IsGizmoActive = IsGizmoActive && ImGui::IsMouseDown(ImGuiMouseButton_Left);
}

void Editor::DrawTangents(Player& Core)
{
	const ImGuiIO& Input = ImGui::GetIO();
	const Object* Rig = FindCamera(Core.Data);
	const s32 Block = Rig && Settings.ShowHelpers && Core.View() != ViewSource::Global ? HelperBlock(Core, *Rig) : -1;
	const BlockCache* Cache = Block >= 0 && Block == TangentBlock ? Core.FindCameraBlock(static_cast<u32>(Block)) : nullptr;
	const xr_vector<PoseKey>* Keys = Cache ? &Rig->Camera.Blocks[Block].Keys : nullptr;
	const s32 Count = Keys ? static_cast<s32>(Keys->size()) : 0;
	IsTangentCancelled = IsTangentCancelled && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	if (IsTangentActive && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		Undo.Cancel(Core.Data);
		IsTangentActive = false;
		IsTangentCancelled = true;
	}
	if (TangentKey < 0 || TangentKey >= Count)
	{
		IsTangentActive = false;
		return;
	}
	const bool IsFree = !IsGizmoHovered && IsViewportFree();
	const u32 Color = Lighten(Rig->Color);
	const s32 Owners[4] = { TangentKey, TangentKey, TangentKey - 1, TangentKey + 1 };
	const u8 Sides[4] = { 0, 1, 1, 0 };
	float Best = ViewportUi::GrabPixels * ViewportUi::GrabPixels;
	s32 Grab = -1;
	Fvector GrabTip = Fvector().set(0.0f, 0.0f, 0.0f);
	for (u32 Index = 0; Index < 4; ++Index)
	{
		const s32 At = Owners[Index], Segment = Sides[Index] ? At : At - 1;
		Fvector In, Out;
		if (At < 0 || At >= Count || Segment < 0 || Segment + 1 >= Count || (*Keys)[Segment].Mode != Interp::Bezier || !Cache->Path.Handles(static_cast<u32>(At), In, Out))
		{
			continue;
		}
		Fvector Base = (*Keys)[At].Position, Tip = Fvector().add(Base, Sides[Index] ? Out : In);
		Fmatrix Turn = Fidentity;
		ViewportUi::Place(Cache->Frames, Base, Turn);
		ViewportUi::Place(Cache->Frames, Tip, Turn);
		ViewportUi::Line3D(Overlay, Base, Tip, Color);
		ImVec2 Screen;
		if (!(ViewportUi::Project(Tip, Screen) > 0.0f))
		{
			continue;
		}
		Overlay->AddCircleFilled(Screen, ViewportUi::HandlePixels, Color);
		const float Gap = (Screen.x - Input.MousePos.x) * (Screen.x - Input.MousePos.x) + (Screen.y - Input.MousePos.y) * (Screen.y - Input.MousePos.y);
		if (Gap < Best)
		{
			Best = Gap;
			Grab = static_cast<s32>(Index);
			GrabTip = Tip;
		}
	}
	if (!IsTangentActive && !IsTangentCancelled && Grab >= 0 && IsFree && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		DragTangentKey = Owners[Grab];
		DragTangentSide = Sides[Grab];
		TangentBase = (*Keys)[DragTangentKey];
		Cache->Path.Handles(static_cast<u32>(DragTangentKey), TangentIn, TangentOut);
		TangentStart = DragTangentSide ? TangentOut : TangentIn;
		TangentPlane = GrabTip;
		IsTangentActive = true;
	}
	IsGizmoHovered = IsGizmoHovered || (Grab >= 0 && IsFree) || IsTangentActive;
	IsTangentActive = IsTangentActive && ImGui::IsMouseDown(ImGuiMouseButton_Left) && DragTangentKey < Count;
	Fvector Origin, Direction;
	const Fvector Normal = Device.vCameraDirection;
	if (!IsTangentActive || !ImGui::IsMouseDragPastThreshold(ImGuiMouseButton_Left, 3.0f) || !ViewportUi::MouseRay(Origin, Direction) || !(std::fabs(Direction.dotproduct(Normal)) > EPS_S))
	{
		return;
	}
	Fmatrix Point = Fidentity;
	Point.c.mad(Origin, Direction, Fvector().sub(TangentPlane, Origin).dotproduct(Normal) / Direction.dotproduct(Normal));
	Spaces::Decompose(Cache->Frames, Point);
	Fvector Offset = Fvector().sub(Point.c, TangentBase.Position);
	Offset = Input.KeyShift ? Fvector().lerp(TangentStart, Offset, 0.1f) : Offset;
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		Offset[Axis] = std::clamp(Offset[Axis], -MaxHandle, MaxHandle);
	}
	PoseKey& Key = Edit(Core, Rig->Id, "Tangent")->Camera.Blocks[Block].Keys[DragTangentKey];
	Key = TangentBase;
	if (Key.Tangent == TangentMode::Auto)
	{
		Key.Tangent = Key.Continuity != 0.0f || Key.Bias != 0.0f ? TangentMode::Broken : TangentMode::Aligned;
		Key.InHandle = TangentIn;
		Key.OutHandle = TangentOut;
	}
	Key.Tangent = Input.KeyCtrl ? TangentMode::Broken : Key.Tangent;
	Fvector& Moved = DragTangentSide ? Key.OutHandle : Key.InHandle;
	Fvector& Other = DragTangentSide ? Key.InHandle : Key.OutHandle;
	Moved = Offset;
	if (Key.Tangent == TangentMode::Aligned && Moved.magnitude() > EPS_S)
	{
		Other = Fvector().normalize(Moved).mul(-Other.magnitude());
	}
}

void Editor::DrawViewCube(Player& Core)
{
	const ImGuiViewport* Main = ImGui::GetMainViewport();
	const float Right = std::max(std::min(ViewRight, Main->Pos.x + Main->Size.x), Main->Pos.x + ViewportUi::CubeSize);
	const bool IsShown = Core.View() == ViewSource::Editor && Settings.ShowHelpers && !IsGizmoActive && Drag == DragKind::None;
	CubeRect = IsShown ? ImRect(Right - ViewportUi::CubeSize, Main->WorkPos.y, Right, Main->WorkPos.y + ViewportUi::CubeSize) : ImRect();
	if (!IsShown)
	{
		return;
	}
	Fmatrix World, View, Identity = Fidentity;
	World.setHPB(Yaw, Pitch, 0.0f);
	World.c = Eye;
	ViewportUi::Mirror(World);
	View.invert(World);
	const Fmatrix Before = View;
	ImGuizmo::ViewManipulate(reinterpret_cast<float*>(&View), reinterpret_cast<const float*>(&Device.mProject), ImGuizmo::TRANSLATE, ImGuizmo::LOCAL, reinterpret_cast<float*>(&Identity), ViewportUi::CubeLength, CubeRect.Min, CubeRect.GetSize(), 0x10101010);
	if (std::memcmp(&Before, &View, sizeof(View)) == 0)
	{
		return;
	}
	World.invert(View);
	ViewportUi::Mirror(World);
	const Fvector Target = Fvector().mad(Eye, Fvector().setHP(Yaw, Pitch), ViewportUi::CubeLength);
	float NewYaw = Yaw, NewPitch = Pitch;
	World.k.getHP(NewYaw, NewPitch);
	Yaw = World.k.x * World.k.x + World.k.z * World.k.z > 1e-6f ? NewYaw : Yaw;
	Pitch = std::clamp(NewPitch, -ViewportUi::MaxPitch, ViewportUi::MaxPitch);
	Eye.mad(Target, Fvector().setHP(Yaw, Pitch), -ViewportUi::CubeLength);
	World.setHPB(Yaw, Pitch, Roll);
	Core.SetEditorView(true, true, Eye, World, EyeFov);

}

void Editor::Pick(Player& Core)
{
	const ImGuiIO& Input = ImGui::GetIO();
	if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		IsPickArmed = IsViewportFree() && !IsGizmoHovered && !CubeRect.Contains(Input.MousePos);
	}
	if (!IsPickArmed || !ImGui::IsMouseReleased(ImGuiMouseButton_Left))
	{
		return;
	}
	IsPickArmed = false;
	Fvector Origin, Direction;
	if (ImGui::IsMouseDragPastThreshold(ImGuiMouseButton_Left, 4.0f) || !ViewportUi::MouseRay(Origin, Direction))
	{
		return;
	}
	float Range = ViewportUi::PickRange, Pixels = ViewportUi::PickPixels * ViewportUi::PickPixels, HitTime = 0.0f;
	u32 Hit = 0, Nearest = 0;
	s32 HitBlock = -1, HitKey = -1;
	const bool HasHelpers = Settings.ShowHelpers && Core.View() != ViewSource::Global;
	for (const Object& Item : Core.Data.Objects)
	{
		const ActorRuntime* Runtime = Item.Type == ObjectType::Actor ? Core.FindActor(Item.Id) : nullptr;
		const ObjectState* Mark = HasHelpers ? ViewportUi::HelperState(Core, Item) : nullptr;
		Fsphere Sphere;
		ImVec2 Centre;
		if (Mark)
		{
			Sphere.set(Mark->World.c, ViewportUi::MarkerRadius);
		}
		else if (!Runtime || !Runtime->PickSphere(Sphere))
		{
			continue;
		}
		Hit = Sphere.intersect(Origin, Direction, Range) != Fsphere::rpNone ? Item.Id : Hit;
		const float Gap = ViewportUi::Project(Sphere.P, Centre) > 0.0f ? (Centre.x - Input.MousePos.x) * (Centre.x - Input.MousePos.x) + (Centre.y - Input.MousePos.y) * (Centre.y - Input.MousePos.y) : FLT_MAX;
		Nearest = Gap < Pixels ? Item.Id : Nearest;
		Pixels = std::min(Gap, Pixels);
	}
	const Object* Rig = FindCamera(Core.Data);
	const s32 KeyBlock = Rig && HasHelpers ? HelperBlock(Core, *Rig) : -1;
	const BlockCache* Cache = KeyBlock >= 0 ? Core.FindCameraBlock(static_cast<u32>(KeyBlock)) : nullptr;
	for (u32 At = 0; Cache && At < Cache->Keys.size(); ++At)
	{
		Fsphere Sphere;
		Fmatrix Turn = Fidentity;
		Fvector Point = Cache->Keys[At].Position;
		ViewportUi::Place(Cache->Frames, Point, Turn);
		Sphere.set(Point, ViewportUi::KeyRadius);
		if (Sphere.intersect(Origin, Direction, Range) != Fsphere::rpNone)
		{
			Hit = Rig->Id;
			HitBlock = KeyBlock;
			HitKey = static_cast<s32>(At);
			HitTime = TargetStart(*Rig, KeyBlock) + Cameras::LocalFromPath(Rig->Camera.Blocks[KeyBlock], *Cache, Cache->Keys[At].Time);
		}
	}
	Hit = Hit ? Hit : Nearest;
	if (!Hit && Input.KeyCtrl)
	{
		return;
	}
	Focus = HitBlock < 0 ? FocusKind::Objects : FocusKind::Block;
	if (!Hit)
	{
		Selected.clear();
		SelectedFolder = 0;
		return;
	}
	Select(Hit, Input.KeyCtrl, false);
	if (HitBlock >= 0)
	{
		FocusObject = Hit;
		SetTangentKey(Core, HitBlock, HitKey);
		FocusBlock = HitBlock;
		Core.IsLookingThrough = false;
		Core.Seek(HitTime);
	}
}

Fvector Editor::FacingRotation(const Fvector& Position, const Fvector* Normal)
{
	Fmatrix Turn = Fidentity;
	Turn.j = Normal ? *Normal : Fvector().set(0.0f, 1.0f, 0.0f);
	Turn.k.sub(Device.vCameraPosition, Position);
	Turn.k.mad(Turn.j, -Turn.k.dotproduct(Turn.j));
	if (!(Turn.k.square_magnitude() > EPS))
	{
		Turn.k.crossproduct(Device.vCameraRight, Turn.j);
	}
	Turn.k.normalize_safe();
	Turn.i.crossproduct(Turn.j, Turn.k);
	Fvector Result;
	Turn.getHPB(Result);
	return Result.mul(rad2deg(1.0f));
}

void Editor::PickFocus(Player& Core)
{
	if (!IsPickingFocus)
	{
		return;
	}
	ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	IsPickingFocus = false;
	const bool IsSampling = ImGui::IsMouseClicked(ImGuiMouseButton_Left) && IsViewportFree();
	IsPickingFocus = !IsSampling && !ImGui::IsKeyPressed(ImGuiKey_Escape, false) && !ImGui::IsMouseClicked(ImGuiMouseButton_Right);
	Object* Rig = FindCamera(Core.Data);
	if (!IsSampling || !Rig || PickFocusBlock < 0 || PickFocusBlock >= ShakeBase || !HasOwner(*Rig, PickFocusBlock))
	{
		return;
	}
	Fvector Origin = Device.vCameraPosition, Direction = Device.vCameraDirection, Point, Normal;
	ViewportUi::MouseRay(Origin, Direction);
	if (!StaticRay(Origin, Direction, ViewportUi::PickRange, Point, Normal))
	{
		SetStatus(true, "There is no surface under the cursor to focus on.");
		return;
	}
	WriteParam(Core, *Edit(Core, Rig->Id, "Pick focus"), PickFocusBlock, Param::DofFocus, 0, 0, Fvector().sub(Point, Device.vCameraPosition).dotproduct(Device.vCameraDirection));
}

void Editor::DrawViewportPopup(Player& Core)
{
	if (ShouldOpenPopup)
	{
		Fvector Origin = Device.vCameraPosition, Direction = Device.vCameraDirection;
		ViewportUi::MouseRay(Origin, Direction);
		HasPopupHit = StaticRay(Origin, Direction, ViewportUi::PickRange, PopupPoint, PopupNormal);
		if (!HasPopupHit)
		{
			PopupPoint.mad(Origin, Direction, 3.0f);
			PopupNormal.set(0.0f, 1.0f, 0.0f);
		}
		IsPopupAligned = ImGui::GetIO().KeyCtrl;
		IsPopupChoosing = false;
		ImGui::OpenPopup("##Viewport");
	}
	if (!ImGui::BeginPopup("##Viewport"))
	{
		return;
	}
	const bool IsAligned = IsPopupAligned || ImGui::GetIO().KeyCtrl;
	const Fvector Facing = FacingRotation(PopupPoint, IsAligned ? &PopupNormal : nullptr);
	shared_str Source, Name;
	LoadPresets();
	ChooserOwners.clear();
	if (IsPopupChoosing && ChooserList(Core, PopupKind, ChooserOwners, Source, Name))
	{
		PopupKind == ChooserKind::Preset ? AddActorFromPreset(Core, Source.c_str(), Core.Time, PopupPoint, Facing) : (void)AddObject(Core, PopupKind == ChooserKind::Effect ? ObjectType::Particle : ObjectType::Actor, Source, Core.Time, PopupPoint, Facing);
		ImGui::CloseCurrentPopup();
	}
	if (IsPopupChoosing)
	{
		ImGui::EndPopup();
		return;
	}
	ImGui::SeparatorText("Add here");
	for (u32 Index = 0; !IsPopupChoosing && Index < 3; ++Index)
	{
		if (ImGui::Selectable(AddChoices[Index], false, ImGuiSelectableFlags_NoAutoClosePopups | (Index == 1 && Presets.empty() ? ImGuiSelectableFlags_Disabled : 0)))
		{
			IsPopupChoosing = IsChooserFresh = true;
			PopupKind = AddKinds[Index];
		}
	}
	if (ImGui::MenuItem("Light"))
	{
		AddLight(Core, Core.Time, Fvector().mad(PopupPoint, PopupNormal, HasPopupHit ? 0.5f : 0.0f));
	}
	if (ImGui::MenuItem("Camera move"))
	{
		PoseKey Pose;
		Pose.Position = PopupPoint;
		Pose.Rotation = Facing;
		AddCameraMove(Core, Core.Time, &Pose);
	}
	ImGui::Separator();
	CollectGizmo(Core, true);
	const bool HasTargets = !GizmoItems.empty();
	const auto Apply = [this, &Core](const char* Label, const auto& Visit)
	{
		Undo.Begin(Label);
		for (GizmoItem& Item : GizmoItems)
		{
			Undo.Touch(Core.Data, Item.Id);
			Visit(Item);
		}
	};
	if (ImGui::MenuItem("Move to cursor", nullptr, false, HasTargets))
	{
		const Fvector Shift = Fvector().sub(PopupPoint, GizmoPivot.c);
		Apply("Move to cursor", [&](GizmoItem& Item)
		{
			const Fvector Position = Fvector().add(Item.Start.c, Shift);
			const Fvector Pose[3] = { Position, FacingRotation(Position, &PopupNormal), Item.Values[2] };
			Fmatrix World = IsAligned ? ViewportUi::Compose(Pose) : Item.Start;
			World.c = Position;
			PutWorld(Core, Item, World, IsAligned ? 3u : 1u);
		});
	}
	if (ImGui::MenuItem("Face camera", nullptr, false, HasTargets))
	{
		Apply("Face camera", [&](GizmoItem& Item)
		{
			const Fvector Pose[3] = { Item.Start.c, FacingRotation(Item.Start.c, nullptr), Item.Values[2] };
			PutWorld(Core, Item, ViewportUi::Compose(Pose), 2u);
		});
	}
	if (ImGui::MenuItem("Snap to ground", nullptr, false, HasTargets))
	{
		Apply("Snap to ground", [&](GizmoItem& Item)
		{
			Fvector Ground, Normal;
			Fmatrix World = Item.Start;
			if (Item.Block < 0 && StaticRay(Fvector().set(World.c.x, World.c.y + 1.5f, World.c.z), Fvector().set(0.0f, -1.0f, 0.0f), 100.0f, Ground, Normal))
			{
				World.c = Ground;
				PutWorld(Core, Item, World, 1u);
			}
		});
	}
	if (ImGui::MenuItem("Delete", "Delete", false, !Selected.empty()))
	{
		Focus = FocusKind::Objects;
		HoverPose = -1;
		DeleteSelection(Core);
	}
	if (ImGui::MenuItem("Frame on timeline", nullptr, false, !Selected.empty()))
	{
		float First = MaxTime, Last = 0.0f;
		for (u32 Id : Selected)
		{
			const Object* Item = Find(Core.Data, Id);
			First = Item ? std::min(First, Item->Span.Start) : First;
			Last = Item ? std::max(Last, ClipEnd(*Item)) : Last;
		}
		Zoom = Last > First ? std::clamp(TrackWidth / (1.1f * (Last - First)), 4.0f, 2000.0f) : Zoom;
		ScrollTime = Last > First ? std::max(First - 0.05f * (Last - First), 0.0f) : ScrollTime;
	}
	ImGui::EndPopup();
}

s32 Editor::HelperBlock(const Player& Core, const Object& Rig) const
{
	const s32 Block = !IsSelected(Rig.Id) ? -1 : FocusObject == Rig.Id && Focus == FocusKind::Block && FocusBlock < ShakeBase ? FocusBlock : Cameras::BlockAt(Rig.Camera, Core.Time - Rig.Span.Start);
	return Block < static_cast<s32>(Rig.Camera.Blocks.size()) ? Block : -1;
}

void Editor::DrawHelpers(const Player& Core) const
{
	if (!Settings.ShowHelpers || Core.View() == ViewSource::Global)
	{
		return;
	}
	ImDrawList* List = Overlay;
	for (const Object& Item : Core.Data.Objects)
	{
		const ActorRuntime* Runtime = Item.Type == ObjectType::Actor ? Core.FindActor(Item.Id) : nullptr;
		const ObjectState* Mark = ViewportUi::HelperState(Core, Item);
		const u32 Tint = IsSelected(Item.Id) ? Lighten(Item.Color) : Item.Color;
		ImVec2 Centre;
		if (Mark)
		{
			ViewportUi::DrawEffectHelper(List, Item, Mark->World, Core.Time, Tint);
		}
		if (Runtime && Runtime->Body.renderable.visual && ViewportUi::Project(Runtime->Body.renderable.xform.c, Centre) > 0.0f)
		{
			List->AddLine(ImVec2(Centre.x - 5.0f, Centre.y), ImVec2(Centre.x + 5.0f, Centre.y), Tint, 1.5f);
			List->AddLine(ImVec2(Centre.x, Centre.y - 5.0f), ImVec2(Centre.x, Centre.y + 5.0f), Tint, 1.5f);
			List->AddText(ImVec2(Centre.x + 6.0f, Centre.y + 2.0f), Tint, Item.Name.c_str());
		}
		const ObjectState* State = IsSelected(Item.Id) ? Core.FindState(Item.Id) : nullptr;
		if (State && State->IsReady)
		{
			ViewportUi::LinkLines(List, State->Frames, State->World.c, Lighten(Item.Color));
		}
	}
	const ActorRuntime* Lit = HighlightBone.size() ? Core.FindActor(HighlightObject) : nullptr;
	Fmatrix Bone;
	ImVec2 Spot;
	if (Lit && Lit->BoneFrame(HighlightBone, Bone) && ViewportUi::Project(Bone.c, Spot) > 0.0f)
	{
		List->AddCircle(Spot, 6.0f, IM_COL32(255, 255, 0, 255), 0, 2.0f);
	}
	const Object* Rig = FindCamera(Core.Data);
	if (!Rig)
	{
		return;
	}
	const u32 Color = IsSelected(Rig->Id) ? Lighten(Rig->Color) : Rig->Color;
	const s32 KeyBlock = HelperBlock(Core, *Rig);
	for (u32 Index = 0; Index < Rig->Camera.Blocks.size(); ++Index)
	{
		const BlockCache* Cache = Core.FindCameraBlock(Index);
		const u32 Count = Cache ? static_cast<u32>(Cache->Keys.size()) : 0;
		for (u32 At = 0; At < Count; ++At)
		{
			const PoseKey& Item = Cache->Keys[At];
			const u32 Segments = At + 1 == Count ? 0 : Item.Mode == Interp::Bezier ? 16 : 1;
			Fvector Point = Item.Position, From, To;
			Fmatrix Turn, Step;
			Turn.setHPB(deg2rad(Item.Rotation.x), deg2rad(Item.Rotation.y), deg2rad(Item.Rotation.z));
			ViewportUi::Place(Cache->Frames, Point, Turn);
			From = Point;
			for (u32 Part = 1; Part <= Segments; ++Part)
			{
				Cache->Path.Evaluate(_lerp(Item.Time, Cache->Keys[At + 1].Time, static_cast<float>(Part) / Segments), To, Step);
				ViewportUi::Place(Cache->Frames, To, Step);
				ViewportUi::Line3D(List, From, To, Color);
				From = To;
			}
			if (static_cast<s32>(Index) != KeyBlock)
			{
				continue;
			}
			ViewportUi::Line3D(List, Point, Fvector().mad(Point, Turn.k, ViewportUi::ArrowLength), Color);
			ImVec2 Centre;
			const float Scale = ViewportUi::Project(Point, Centre);
			if (Scale > 0.0f)
			{
				List->AddCircle(Centre, std::max(3.0f, ViewportUi::KeyRadius * Scale), Color, 0, 1.5f);
			}
		}
	}
	const BlockCache* Cache = KeyBlock >= 0 ? Core.FindCameraBlock(static_cast<u32>(KeyBlock)) : nullptr;
	PoseKey Pose;
	u32 At = u32(-1);
	if (Cache && PoseAt(Core, Rig->Camera.Blocks[KeyBlock], KeyBlock, Core.Time - TargetStart(*Rig, KeyBlock), Pose, At))
	{
		Fmatrix Turn;
		Turn.setHPB(deg2rad(Pose.Rotation.x), deg2rad(Pose.Rotation.y), deg2rad(Pose.Rotation.z));
		ViewportUi::Place(Cache->Frames, Pose.Position, Turn);
		ViewportUi::LinkLines(List, Cache->Frames, Pose.Position, Color);
	}
	const CameraBlock* Shot = Cache ? &Rig->Camera.Blocks[KeyBlock] : nullptr;
	if (Shot && Shot->Dof != DofMode::Off && !Shot->Keys.empty())
	{
		CameraPose Lens;
		Cameras::EvaluateBlock(*Shot, *Cache, std::clamp(Core.Time - TargetStart(*Rig, KeyBlock), 0.0f, Shot->Length), Lens);
		const AttachLink& Link = Shot->FocusTarget;
		string128 Label = {};
		if (Shot->Dof == DofMode::Object && Link.ObjectId)
		{
			xr_sprintf(Label, "-> %s%s%s ", DisplayName(Find(Core.Data, Link.ObjectId)), Link.Bone.size() ? "." : "", Link.Bone.size() ? Link.Bone.c_str() : "");
		}
		ViewportUi::DrawFocus(List, Lens, Label, Color);
	}
}
}
