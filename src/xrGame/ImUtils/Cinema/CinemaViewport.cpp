#include "StdAfx.h"
#include "CinemaEditor.h"
#include "../../../xrEngine/ImGuiManager.h"
#include "../../../xrEngine/IInputReceiver.h"

namespace Cinema::ViewportUi
{
Curve* PathCurve(CameraBlock& Block, u32 Axis)
{
	const auto Found = std::find_if(Block.Lanes.begin(), Block.Lanes.end(), [](const Lane& Item) { return Item.Id == Param::Position && Item.Layer == 0; });
	return Found == Block.Lanes.end() ? nullptr : &Found->Curves[Axis];
}

bool TangentAt(const CameraBlock& Block, float Time, Fvector& In, Fvector& Out)
{
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		const Curve* Part = PathCurve(const_cast<CameraBlock&>(Block), Axis);
		const u32 At = Part ? Curves::KeyAt(*Part, Time, 1e-4f) : u32(-1);
		if (At == u32(-1))
		{
			return false;
		}
		const Key& Item = Part->Keys[At];
		const float Before = At ? (Item.Time - Part->Keys[At - 1].Time) / 3.0f : 0.0f, After = At + 1 < Part->Keys.size() ? (Part->Keys[At + 1].Time - Item.Time) / 3.0f : 0.0f;
		In[Axis] = At && Part->Keys[At - 1].Mode == Interp::Bezier ? Item.InValue : Curves::Value(*Part, Item.Time - Before, Item.Value) - Item.Value;
		Out[Axis] = Item.Mode == Interp::Bezier ? Item.OutValue : Curves::Value(*Part, Item.Time + After, Item.Value) - Item.Value;
	}
	return true;
}

constexpr float LookRate = 0.003f, PanRate = 0.0025f, DollyRate = 0.25f, MaxPitch = 1.55f, KeyRadius = 0.12f, ArrowLength = 0.6f;
constexpr float RollRate = 0.7853982f, FovRate = 30.0f, MinFov = 5.0f, MaxFov = 150.0f;
constexpr float PickRange = 1000.0f, PickPixels = 32.0f, CubeSize = 128.0f, CubeLength = 8.0f;
constexpr float MarkerRadius = 0.3f, GrabPixels = 8.0f, HandlePixels = 5.0f, TangentPixels = 12.0f;
constexpr u32 CircleSegments = 24, FaintAlpha = 90;
constexpr u32 ActorBit = 1u << static_cast<u32>(ObjectType::Actor), SpotBit = 1u << static_cast<u32>(ObjectType::Light), ParticleBit = 1u << static_cast<u32>(ObjectType::Particle), PointBit = 1u << 8;
constexpr const char* GizmoLabels[3] = { "Move", "Rotate", "Scale" };
constexpr const char* ToolLabels[4] = { "TRS", "T", "R", "S" };
constexpr const char* ToolNames[5] = { "Move, rotate and scale (Q)", "Move (W)", "Rotate (E)", "Scale (R)", "Range (R)" };
constexpr const char* StepNames[4] = { "Move step", "Rotate step", "Scale step", "Range step" };
constexpr const char* StepFormats[4] = { "%.2f m", "%.1f deg", "%.2f", "%.2f m" };
constexpr float StepLo[4] = { 0.01f, 0.5f, 0.01f, 0.01f }, StepHi[4] = { 10.0f, 180.0f, 1.0f, 10.0f };
constexpr const char* FlyHint = "W A S D: move | Space, C: up, down | Q, E: roll | R, T: field of view | Shift: fast | Wheel: speed";
constexpr const char* GizmoHint = "Ctrl: flip snap | Shift: snap to the surface | Esc: cancel";
constexpr const char* ViewHint = "Right-drag: fly | Middle-drag: pan | Wheel: dolly | Right-click: menu | Q W E R: gizmo | X: local, world | V: shot, free camera";
constexpr const char* PathHint = "Drag: move the key | Shift: snap to the surface | Ctrl + click the path: add a key";
constexpr const char* InsertHint = "Click: add a key here";
constexpr const char* IkHint = "Drag: move | Click: gizmo for turning and axis moves | Esc: cancel";
constexpr const char* TangentHint = "Drag: bend the path | Ctrl: break the handles | Shift: fine";
constexpr ImGuizmo::OPERATION Operations[3] = { ImGuizmo::TRANSLATE, ImGuizmo::ROTATE, ImGuizmo::SCALE };
constexpr Param ScaleParams[8] = { Param::Scale, Param::Scale, Param::Range, Param::Size, Param::DecalWidth, Param::Scale, Param::Scale, Param::Scale };

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

float RigReach(u32 Control)
{
	constexpr float Reach[14] = { 0.25f, 0.22f, 0.14f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.4f, 0.2f, 0.2f, 0.1f };
	return Control < 14 ? Reach[Control] : 0.0f;
}

Fvector RigRing(const Fvector& Centre, float Radius, u32 Step)
{
	const float Angle = static_cast<float>(Step) * PI_MUL_2 / 24.0f;
	return Fvector().set(Centre.x + cosf(Angle) * Radius, Centre.y, Centre.z + sinf(Angle) * Radius);
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

struct PathSpot
{
	u32 Id = 0;
	s32 Block = -1;
	s32 Key = -1;
	float Time = 0.0f;
	float Gap = 0.0f;
	ImVec2 At;
	Fvector Point;
};

void Offer(PathSpot& Spot, u32 Id, s32 Block, s32 Key, float Time, const Fvector& Point)
{
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	ImVec2 Screen;
	if (!(Project(Point, Screen) > 0.0f))
	{
		return;
	}
	const float Gap = (Screen.x - Mouse.x) * (Screen.x - Mouse.x) + (Screen.y - Mouse.y) * (Screen.y - Mouse.y);
	if (Gap < Spot.Gap)
	{
		Spot = { Id, Block, Key, Time, Gap, Screen, Point };
	}
}

void FitHandles(Lane& Track, const Lane& Base, float Time, float HalfFrame)
{
	u32 Now[3], Was[3];
	float Edge[2] = { -FLT_MAX, FLT_MAX };
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		const xr_vector<Key>& Keys = Base.Curves[Axis].Keys;
		Now[Axis] = Curves::KeyAt(Track.Curves[Axis], Time, HalfFrame);
		Was[Axis] = Curves::KeyAt(Base.Curves[Axis], Time, HalfFrame);
		Edge[0] = Was[Axis] != u32(-1) && Was[Axis] > 0 ? std::max(Edge[0], Keys[Was[Axis] - 1].Time) : Edge[0];
		Edge[1] = Was[Axis] != u32(-1) && Was[Axis] + 1 < Keys.size() ? std::min(Edge[1], Keys[Was[Axis] + 1].Time) : Edge[1];
	}
	const auto At = [](const Lane& Source, float Moment) { return Fvector().set(Curves::Value(Source.Curves[0], Moment, 0.0f), Curves::Value(Source.Curves[1], Moment, 0.0f), Curves::Value(Source.Curves[2], Moment, 0.0f)); };
	const Fvector Old = At(Base, Time), New = At(Track, Time);
	for (u32 Side = 0; Side < 2; ++Side)
	{
		if (!(std::fabs(Edge[Side]) < FLT_MAX))
		{
			continue;
		}
		const Fvector Far = At(Base, Edge[Side]);
		const float Chord = Old.distance_to(Far), Ratio = Chord > EPS_L ? std::clamp(New.distance_to(Far) / Chord, 0.05f, 20.0f) : 1.0f;
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			xr_vector<Key>& Keys = Track.Curves[Axis].Keys;
			const xr_vector<Key>& Before = Base.Curves[Axis].Keys;
			const u32 Mine = Now[Axis], Theirs = Was[Axis];
			if (Mine == u32(-1) || Theirs == u32(-1) || (Side ? Mine + 1 >= Keys.size() || Theirs + 1 >= Before.size() : !Mine || !Theirs))
			{
				continue;
			}
			(Side ? Keys[Mine].OutValue : Keys[Mine].InValue) = (Side ? Before[Theirs].OutValue : Before[Theirs].InValue) * Ratio;
			(Side ? Keys[Mine + 1].InValue : Keys[Mine - 1].OutValue) = (Side ? Before[Theirs + 1].InValue : Before[Theirs - 1].OutValue) * Ratio;
		}
	}
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
		Line3D(List, From, Frames.Parent.c, Frames.Weight < 1.0f ? (Color & 0x00FFFFFFu) | (FaintAlpha << 24) : Color);
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
	const ObjectState* State = Item.Type == ObjectType::Light || Item.Type == ObjectType::Particle || Item.Type == ObjectType::Decal || (Item.Type == ObjectType::Sound && Item.Sound.IsWorld) ? Core.FindState(Item.Id) : nullptr;
	return State && State->IsReady ? State : nullptr;
}

bool IsSkipped(const Object& Item, GizmoMode Mode)
{
	return Item.Type >= ObjectType::Weather || (Item.Type == ObjectType::Sound && (!Item.Sound.IsWorld || Mode == GizmoMode::Rotate || Mode == GizmoMode::Scale)) || (Item.Type == ObjectType::Light && (Mode == GizmoMode::Scale || (Mode == GizmoMode::Rotate && !IsParamShown(Item, Param::Rotation)))) || (Item.Type == ObjectType::Camera && Mode == GizmoMode::Scale);
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
	if (Item.Type == ObjectType::Decal)
	{
		const float Width = 0.5f * Curves::LaneScalar(Item.Lanes, Param::DecalWidth, 0, Local, Item.Decal.Width), Height = 0.5f * Curves::LaneScalar(Item.Lanes, Param::DecalHeight, 0, Local, Item.Decal.Height);
		Fvector Side = World.i, Front = World.k, Up = World.j;
		Side.normalize_safe().mul(Width);
		Front.normalize_safe().mul(Height);
		Up.normalize_safe().mul(-0.5f);
		const Fvector Corners[4] = { Fvector().add(World.c, Side).add(Front), Fvector().sub(World.c, Side).add(Front), Fvector().sub(World.c, Side).sub(Front), Fvector().add(World.c, Side).sub(Front) };
		for (u32 Edge = 0; Edge < 4; ++Edge)
		{
			Line3D(List, Corners[Edge], Corners[(Edge + 1) % 4], Color);
		}
		Line3D(List, World.c, Fvector().add(World.c, Up), Color);
		ImVec2 Spot;
		if (Project(World.c, Spot) > 0.0f)
		{
			List->AddText(ImVec2(Spot.x + 6.0f, Spot.y + 2.0f), Color, Item.Name.c_str());
		}
		return;
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
		ShownEye = Eye;
		ShownYaw = Yaw;
		ShownPitch = Pitch;
		ShownRoll = Roll;
		ShownFov = EyeFov;
		IsGliding = false;
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
	IsFlyArmed = IsCapturing && (IsFlyArmed ? CImGuiManager::Instance().IsButtonHeld(false) : IsFree && ImGui::IsMouseClicked(ImGuiMouseButton_Right));
	IsPanning = IsCapturing && (IsPanning ? CImGuiManager::Instance().IsButtonHeld(true) : IsFree && ImGui::IsMouseClicked(ImGuiMouseButton_Middle));
	IsFlying = IsFlyArmed && (IsFlying || ImGui::IsMouseDragging(ImGuiMouseButton_Right, 4.0f));
	ShouldOpenPopup = WasArmed && !IsFlyArmed && !WasFlying;
	Hint = IsFlying ? ViewportUi::FlyHint : IsGizmoActive || IsGizmoHovered ? (Hint ? Hint : ViewportUi::GizmoHint) : IsFree ? ViewportUi::ViewHint : Hint;
	const Object* Rig = FindCamera(Core.Data);
	const bool IsRecording = IsFlying && Core.IsPlaying && IsAutoKey && Rig && IsSelected(Rig->Id);
	if (IsRecording && ImGui::IsKeyPressed(ImGuiKey_Escape, false))
	{
		Recorded.clear();
		IsRecordCancelled = true;
	}
	if (WasFlying && !IsFlying && IsAutoKey && Rig && IsSelected(Rig->Id) && !HasRecorded)
	{
		KeyFromView(Core, Core.Time, true);
	}
	if (!Recorded.empty() && !IsRecording)
	{
		CommitRecording(Core);
	}
	HasRecorded = HasRecorded && IsFlying;
	IsRecordCancelled = IsRecordCancelled && IsFlying;

	const float Wheel = IsFlyArmed || IsFree ? Input.MouseWheel : 0.0f;
	if ((IsFlying && !WasFlying) || (IsPanning && ImGui::IsMouseClicked(ImGuiMouseButton_Middle)) || (Wheel != 0.0f && !IsFlyArmed))
	{
		UseEditorCamera(Core);
	}
	FlyFov = IsFlying && !WasFlying ? EyeFov : FlyFov;

	if (IsFlying != WasFlying)
	{
		CImGuiManager::Instance().SetMouseLook(IsFlying);
	}
	const auto Axis = [&Input](ImGuiKey Plus, ImGuiKey Minus) { return Input.WantTextInput ? 0.0f : static_cast<float>(ImGui::IsKeyDown(Plus)) - static_cast<float>(ImGui::IsKeyDown(Minus)); };
	const float Boost = IsFlying ? (Input.KeyShift ? 4.0f : 1.0f) * Input.DeltaTime : 0.0f;
	if (IsFlying)
	{
		const ImVec2 Look = CImGuiManager::Instance().TakeMouseLook();
		const float Rate = ViewportUi::LookRate * (EyeFov > 0.0f ? EyeFov : GameFov()) / 67.5f * Settings.LookSensitivity;
		Yaw -= Look.x * Rate;
		Pitch = std::clamp(Pitch - Look.y * Rate * (psMouseInvert ? -1.0f : 1.0f), -ViewportUi::MaxPitch, ViewportUi::MaxPitch);
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
	if (IsRecording && !IsRecordCancelled)
	{
		constexpr u32 MaxSamples = 60 * 600;
		Recorded.reserve(MaxSamples);
		const RecordSample* Last = Recorded.empty() ? nullptr : &Recorded.back();
		const Fvector Turn = Fvector().set(rad2deg(Yaw), rad2deg(Pitch), rad2deg(Roll));
		RecordSample Sample { Core.Time, Eye, Turn, EyeFov };
		Sample.Rotation.x = Last ? Last->Rotation.x + std::remainder(Turn.x - Last->Rotation.x, 360.0f) : Turn.x;
		Sample.Rotation.z = Last ? Last->Rotation.z + std::remainder(Turn.z - Last->Rotation.z, 360.0f) : Turn.z;
		if (Recorded.size() < MaxSamples && (!Last || Core.Time > Last->Time))
		{
			Recorded.push_back(Sample);
		}
		HasRecorded = true;
	}
	const bool IsInput = IsFlying || IsPanning || Wheel != 0.0f;
	const float Keep = Settings.FlySmoothing > 0.0f && (IsInput || IsGliding) ? std::exp(-Input.DeltaTime / Settings.FlySmoothing) : 0.0f;
	const float TurnYaw = std::remainder(ShownYaw - Yaw, PI_MUL_2) * Keep, TurnRoll = std::remainder(ShownRoll - Roll, PI_MUL_2) * Keep;
	ShownEye.lerp(Eye, ShownEye, Keep);
	ShownYaw = Yaw + TurnYaw;
	ShownPitch = Pitch + (ShownPitch - Pitch) * Keep;
	ShownRoll = Roll + TurnRoll;
	ShownFov = EyeFov > 0.0f && ShownFov > 0.0f ? EyeFov + (ShownFov - EyeFov) * Keep : EyeFov;
	IsGliding = Keep > 0.0f && (ShownEye.distance_to_sqr(Eye) > 1e-6f || std::fabs(TurnYaw) + std::fabs(ShownPitch - Pitch) + std::fabs(TurnRoll) > 1e-4f);
	Fmatrix Shown;
	Shown.setHPB(ShownYaw, ShownPitch, ShownRoll);
	Core.SetEditorView(true, IsEditorCamera, ShownEye, Shown, ShownFov);
}

void Editor::CommitRecording(Player& Core)
{
	const u32 Count = static_cast<u32>(Recorded.size());
	const float Sigma = Settings.RecordSmoothing, Frame = 1.0f / static_cast<float>(Core.Data.Header.FrameRate);
	Smoothed = Recorded;
	for (u32 Index = 0; Sigma > 0.0f && Index < Count; ++Index)
	{
		RecordSample Sum { Recorded[Index].Time, Fvector().set(0.0f, 0.0f, 0.0f), Fvector().set(0.0f, 0.0f, 0.0f), 0.0f };
		float Total = 0.0f;
		for (u32 Other = Index; Other-- > 0 && Recorded[Index].Time - Recorded[Other].Time <= 3.0f * Sigma;)
		{
			const float Gap = (Recorded[Index].Time - Recorded[Other].Time) / Sigma, Weight = std::exp(-0.5f * Gap * Gap);
			Sum.Position.mad(Recorded[Other].Position, Weight);
			Sum.Rotation.mad(Recorded[Other].Rotation, Weight);
			Sum.Fov += Recorded[Other].Fov * Weight;
			Total += Weight;
		}
		for (u32 Other = Index; Other < Count && Recorded[Other].Time - Recorded[Index].Time <= 3.0f * Sigma; ++Other)
		{
			const float Gap = (Recorded[Other].Time - Recorded[Index].Time) / Sigma, Weight = std::exp(-0.5f * Gap * Gap);
			Sum.Position.mad(Recorded[Other].Position, Weight);
			Sum.Rotation.mad(Recorded[Other].Rotation, Weight);
			Sum.Fov += Recorded[Other].Fov * Weight;
			Total += Weight;
		}
		Smoothed[Index] = { Sum.Time, Sum.Position.div(Total), Sum.Rotation.div(Total), Sum.Fov / Total };
	}
	Kept.assign(Count, 0);
	Kept.front() = Kept.back() = 1;
	RdpSpans.assign(1, { 0u, Count - 1 });
	while (!RdpSpans.empty())
	{
		const auto [From, To] = RdpSpans.back();
		RdpSpans.pop_back();
		float Worst = 1.0f;
		u32 Split = 0;
		for (u32 Index = From + 1; Index < To; ++Index)
		{
			const float Part = (Smoothed[Index].Time - Smoothed[From].Time) / std::max(Smoothed[To].Time - Smoothed[From].Time, 1e-6f);
			const Fvector Line = Fvector().lerp(Smoothed[From].Position, Smoothed[To].Position, Part), Angle = Fvector().lerp(Smoothed[From].Rotation, Smoothed[To].Rotation, Part);
			const float Error = std::max({ Line.distance_to(Smoothed[Index].Position) / 0.01f, std::fabs(Angle.x - Smoothed[Index].Rotation.x) / 0.25f, std::fabs(Angle.y - Smoothed[Index].Rotation.y) / 0.25f, std::fabs(Angle.z - Smoothed[Index].Rotation.z) / 0.25f });
			Split = Error > Worst ? Index : Split;
			Worst = std::max(Worst, Error);
		}
		if (Split)
		{
			Kept[Split] = 1;
			RdpSpans.push_back({ From, Split });
			RdpSpans.push_back({ Split, To });
		}
	}
	float FovLo = FLT_MAX, FovHi = -FLT_MAX;
	for (const RecordSample& Item : Recorded)
	{
		FovLo = std::min(FovLo, Item.Fov);
		FovHi = std::max(FovHi, Item.Fov);
	}
	const bool HasFov = FovHi - FovLo > 0.01f && FovLo > 0.0f;
	Recorded.clear();
	Undo.Begin("Record camera");
	const auto OwnerOf = [&](u32 Index)
	{
		const Object* Rig = FindCamera(Core.Data);
		return Rig ? Cameras::BlockAt(Rig->Camera, Smoothed[Index].Time - Rig->Span.Start) : -2;
	};
	u32 Written = 0;
	for (s32 Last = static_cast<s32>(Count) - 1; Last >= 0;)
	{
		const s32 Owner = OwnerOf(static_cast<u32>(Last));
		s32 First = Last;
		for (s32 Index = Last - 1; Index >= 0 && (!Kept[Index] || OwnerOf(static_cast<u32>(Index)) == Owner); --Index)
		{
			First = Kept[Index] ? Index : First;
		}
		const Object* Found = FindCamera(Core.Data);
		const BlockCache* Cache = Owner >= 0 ? Core.FindCameraBlock(static_cast<u32>(Owner)) : nullptr;
		const float Begin = Smoothed[First].Time, End = Smoothed[Last].Time;
		if (Found && Owner == -1)
		{
			xr_vector<CameraBlock> Shots(1);
			CameraBlock& Shot = Shots[0];
			Shot.Length = End - Begin + Frame;
			Shot.Fov = Smoothed[First].Fov;
			for (s32 Index = First; Index <= Last && Shot.Keys.size() < MaxPoseKeys; ++Index)
			{
				if (Kept[Index])
				{
					const RecordSample& Item = Smoothed[Index];
					Cameras::SetPoseAt(Shot, Item.Time - Begin, Item.Position, Item.Rotation, Interp::Bezier, 0.5f * Frame);
					HasFov ? Curves::SetKey(Curves::EnsureLane(Shot.Lanes, Param::Fov, 0).Curves[0], Item.Time - Begin, Item.Fov, 0.5f * Frame) : void();
					++Written;
				}
			}
			InsertShots(Core, Begin, Shots, true);
		}
		else if (Found && Cache)
		{
			Object& Rig = *Edit(Core, Found->Id, "Record camera");
			CameraBlock& Shot = Rig.Camera.Blocks[Owner];
			const float Base = Rig.Span.Start + Shot.Start;
			const float Lo = Cameras::PathTime(Shot, *Cache, Begin - Base - 0.5f * Frame), Hi = Cameras::PathTime(Shot, *Cache, End - Base + 0.5f * Frame);
			for (Lane& Track : Shot.Lanes)
			{
				for (u32 Axis = 0; Track.Id <= Param::Rotation && Axis < 3; ++Axis)
				{
					std::erase_if(Track.Curves[Axis].Keys, [Lo, Hi](const Key& Item) { return Item.Time >= Lo && Item.Time <= Hi; });
				}
			}
			Cameras::Rebuild(Shot);
			if (HasFov)
			{
				UseGameFov(Rig, Owner, Param::Fov);
			}
			for (s32 Index = First; Index <= Last; ++Index)
			{
				if (Kept[Index])
				{
					const RecordSample& Item = Smoothed[Index];
					PoseKey Pose { 0.0f, Item.Position, Item.Rotation, Interp::Bezier };
					ToSpace(Cache->Frames, Pose);
					PutPose(Shot, *Cache, Item.Time - Base, 0.5f * Frame, Pose);
					HasFov ? Curves::SetKey(Curves::EnsureLane(Shot.Lanes, Param::Fov, 0).Curves[0], Item.Time - Base, Item.Fov, 0.5f * Frame) : void();
					++Written;
				}
			}
			for (u32 Index = 0; Index < Shot.Keys.size(); ++Index)
			{
				const float Moment = Shot.Keys[Index].Time;
				if (Moment >= Lo && Moment <= Hi)
				{
					Cameras::ModeAt(Shot, Moment, Interp::Bezier, 1e-4f);
				}
			}
		}
		Last = First - 1;
		while (Last >= 0 && !Kept[Last])
		{
			--Last;
		}
	}
	SetStatus(false, "Recorded %u camera keys.", Written);
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
	ImGuizmo::Enable(!IsGizmoCancelled && !IsIkHover && !IsIkDirect && !IsTangentActive && !IsPickingColor && !IsPickingFocus && CImGuiManager::Instance().IsCapturingInputs() && !ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId | ImGuiPopupFlags_AnyPopupLevel));
	const Object* Posed = IsPosing && SelectedBones.size() == 1 && !IsGizmoActive ? Find(Core.Data, PoseObject) : nullptr;
	const ActorRuntime* Body = Posed ? Core.FindActor(PoseObject) : nullptr;
	for (u32 Joint = 0; Body && !SelectedBones.empty() && Joint < 3 * MaxIkLimbs; ++Joint)
	{
		const IkLimb& Setup = IkOf(*Posed, IkBlock(*Posed))[Joint / 3];
		if (Setup.IsOn && Body->FindBone(Setup.Bones[Joint % 3]) == SelectedBones[0])
		{
			IkHandle = static_cast<s32>(2 * (Joint / 3) + (Joint % 3 == 2 ? 0 : 1));
			SelectedBones.clear();
			SetStatus(false, Joint % 3 == 2 ? "This limb follows its IK target; the target is selected." : "This limb follows its IK target; its pole is selected.");
		}
	}
	if (IsPosing && !SelectedBones.empty())
	{
		IkHandle = -1;
		DrawBoneGizmo(Core);
		return;
	}
	if (IsPosing && IkHandle >= 0)
	{
		IsGizmoActive = false;
		return;
	}
	if (IsHandlePicked)
	{
		IsGizmoActive = false;
		return;
	}
	u32 Kinds = 0;
	for (u32 Id : Selected)
	{
		const Object* Item = Find(Core.Data, Id);
		Kinds |= !Item || Item->Type >= ObjectType::Weather ? 0u : IsParamShown(*Item, Param::Rotation) ? 1u << static_cast<u32>(Item->Type) : ViewportUi::PointBit;
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
	ImGuizmo::Manipulate(reinterpret_cast<const float*>(&Device.mView), reinterpret_cast<const float*>(&Device.mProject), static_cast<ImGuizmo::OPERATION>(Operation), IsLocal ? ImGuizmo::LOCAL : ImGuizmo::WORLD, reinterpret_cast<float*>(&GizmoPivot), nullptr, Input.KeyCtrl != Settings.IsGizmoSnapping ? Snap : nullptr);
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
	Fvector Origin, Direction, Hit, Normal;
	if (IsUsing && GizmoPart == 0 && Input.KeyShift && ViewportUi::MouseRay(Origin, Direction) && StaticRay(Origin, Direction, ViewportUi::PickRange, Hit, Normal))
	{
		Fmatrix Start;
		Start.invert(GizmoInverse);
		for (GizmoItem& Item : GizmoItems)
		{
			const Object* Target = Find(Core.Data, Item.Id);
			if (!Target || Item.Block >= 0)
			{
				continue;
			}
			const bool IsLight = Target->Type == ObjectType::Light, IsAligned = Input.KeyCtrl && !IsLight;
			const Fvector Position = Fvector().sub(Item.Start.c, Start.c).add(Hit).mad(Normal, IsLight ? 0.5f : 0.0f);
			const Fvector Pose[3] = { Position, FacingRotation(Position, &Normal), Item.Values[2] };
			Fmatrix World = IsAligned ? ViewportUi::Compose(Pose) : Item.Start;
			World.c = Position;
			PutWorld(Core, Item, World, IsAligned ? 3u : 1u);
		}
		GizmoPivot.c = Hit;
		return;
	}
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
	const BlockCache* Cache = Block >= 0 && (Block == TangentBlock || IsSelected(Rig->Id)) ? Core.FindCameraBlock(static_cast<u32>(Block)) : nullptr;
	const CameraBlock* Shot = Cache ? &Rig->Camera.Blocks[Block] : nullptr;
	const s32 Count = Shot ? static_cast<s32>(Shot->Keys.size()) : 0;
	IsTangentCancelled = IsTangentCancelled && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	if (IsTangentActive && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		Undo.Cancel(Core.Data);
		IsTangentActive = false;
		IsTangentCancelled = true;
	}
	IsHandlePicked = IsHandlePicked && Shot && IsSelected(Rig->Id) && Block == TangentBlock && TangentKey >= 0 && TangentKey < Count;
	if (!Shot)
	{
		IsTangentActive = IsHandleOver = false;
		return;
	}
	const bool IsFree = !IsGizmoHovered && IsViewportFree();
	const float Tension = Shot->Tension;
	const u32 Color = Lighten(Rig->Color), Picked = IM_COL32(255, 255, 255, 255);
	float Best = ViewportUi::TangentPixels * ViewportUi::TangentPixels;
	s32 HoverKey = -1;
	u8 HoverSide = 0;
	ImVec2 HoverAt;
	for (s32 At = 0; At < Count; ++At)
	{
		Fvector In, Out;
		for (u8 Side = 0; Side < 2 && ViewportUi::TangentAt(*Shot, Shot->Keys[At].Time, In, Out); ++Side)
		{
			if (Side ? At + 1 >= Count : At == 0)
			{
				continue;
			}
			Fvector Base = Shot->Keys[At].Position, Tip = Fvector().mad(Base, Side ? Out : In, Tension);
			Fmatrix Turn = Fidentity, Spin = Fidentity;
			ViewportUi::Place(Cache->Frames, Base, Turn);
			ViewportUi::Place(Cache->Frames, Tip, Spin);
			const bool IsPicked = IsHandlePicked && At == TangentKey && Side == DragTangentSide;
			ViewportUi::Line3D(Overlay, Base, Tip, IsPicked ? Picked : Color);
			ImVec2 Screen;
			if (!(ViewportUi::Project(Tip, Screen) > 0.0f))
			{
				continue;
			}
			Overlay->AddCircleFilled(Screen, IsPicked ? 7.0f : 6.0f, IsPicked ? Picked : Color);
			Overlay->AddCircle(Screen, IsPicked ? 7.0f : 6.0f, IM_COL32(0, 0, 0, 200));
			const float Gap = (Screen.x - Input.MousePos.x) * (Screen.x - Input.MousePos.x) + (Screen.y - Input.MousePos.y) * (Screen.y - Input.MousePos.y);
			if (Gap < Best)
			{
				Best = Gap;
				HoverKey = At;
				HoverSide = Side;
				HoverAt = Screen;
			}
		}
	}
	if (HoverKey >= 0 && IsFree && !IsTangentActive)
	{
		Overlay->AddCircle(HoverAt, 10.0f, IM_COL32(255, 220, 60, 255), 0, 2.0f);
	}
	if (!IsTangentActive && !IsTangentCancelled && IsFree && HoverKey >= 0 && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		SetTangentKey(Core, Block, HoverKey);
		DragTangentSide = HoverSide;
		TangentBase = Shot->Keys[HoverKey];
		if (ViewportUi::TangentAt(*Shot, TangentBase.Time, TangentIn, TangentOut))
		{
			Fmatrix Turn = Fidentity;
			IsTangentActive = true;
			IsBreakPress = Input.KeyCtrl;
			TangentStart = DragTangentSide ? TangentOut : TangentIn;
			TangentGrab.mad(TangentBase.Position, TangentStart, Tension);
			ViewportUi::Place(Cache->Frames, TangentGrab, Turn);
		}
	}
	IsTangentActive = IsTangentActive && ImGui::IsMouseDown(ImGuiMouseButton_Left) && TangentKey >= 0 && TangentKey < Count;
	IsHandlePicked = IsTangentActive;
	IsHandleOver = false;
	IsGizmoHovered = IsGizmoHovered || (HoverKey >= 0 && IsFree) || IsTangentActive;
	Hint = (HoverKey >= 0 && IsFree) || IsTangentActive ? ViewportUi::TangentHint : Hint;
	Fvector Origin, Direction;
	if (!IsTangentActive || !ImGui::IsMouseDragPastThreshold(ImGuiMouseButton_Left, 2.0f) || !ViewportUi::MouseRay(Origin, Direction) || std::fabs(Direction.dotproduct(Device.vCameraDirection)) < EPS)
	{
		return;
	}
	Fmatrix Point = Fidentity;
	Point.c.mad(Origin, Direction, Fvector().sub(TangentGrab, Origin).dotproduct(Device.vCameraDirection) / Direction.dotproduct(Device.vCameraDirection));
	Spaces::Decompose(Cache->Frames, Point);
	Fvector Offset = Fvector().sub(Point.c, TangentBase.Position).div(Tension);
	Offset = Input.KeyShift ? Fvector().lerp(TangentStart, Offset, 0.1f) : Offset;
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		Offset[Axis] = std::clamp(Offset[Axis], -MaxHandle, MaxHandle);
	}
	CameraBlock& Edited = Edit(Core, Rig->Id, "Tangent")->Camera.Blocks[Block];
	const Fvector Other = DragTangentSide ? TangentIn : TangentOut;
	const Fvector Mirror = IsBreakPress || !(Offset.magnitude() > EPS_S) ? Other : Fvector().normalize(Offset).mul(-Other.magnitude());
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		Curve* Part = ViewportUi::PathCurve(Edited, Axis);
		const u32 At = Part ? Curves::KeyAt(*Part, TangentBase.Time, 1e-4f) : u32(-1);
		if (At == u32(-1))
		{
			continue;
		}
		Key& Item = Part->Keys[At];
		Item.IsBroken = Item.IsBroken || IsBreakPress;
		Item.InTime = At ? -(Item.Time - Part->Keys[At - 1].Time) / 3.0f : Item.InTime;
		Item.OutTime = At + 1 < Part->Keys.size() ? (Part->Keys[At + 1].Time - Item.Time) / 3.0f : Item.OutTime;
		if (At && (!DragTangentSide || !Item.IsBroken))
		{
			Part->Keys[At - 1].Mode = Interp::Bezier;
		}
		if (At + 1 < Part->Keys.size() && (DragTangentSide || !Item.IsBroken))
		{
			Item.Mode = Interp::Bezier;
		}
		(DragTangentSide ? Item.OutValue : Item.InValue) = Offset[Axis];
		(DragTangentSide ? Item.InValue : Item.OutValue) = Item.IsBroken ? (DragTangentSide ? TangentIn : TangentOut)[Axis] : Mirror[Axis];
	}
	Cameras::Rebuild(Edited);
}

void Editor::DrawToolbar(Player& Core)
{
	const ImGuiViewport* Main = ImGui::GetMainViewport();
	const ImGuiStyle& Style = ImGui::GetStyle();
	ImGui::SetNextWindowPos(ImVec2(Main->WorkPos.x + 8.0f, Main->WorkPos.y + 8.0f), ImGuiCond_FirstUseEver);
	if (!ImGui::Begin("##CinemaTools", nullptr, ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoNavInputs))
	{
		ImGui::End();
		return;
	}
	IsOwnWindowFocused |= ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	u32 Kinds = 0;
	for (u32 Id : Selected)
	{
		const Object* Item = Find(Core.Data, Id);
		Kinds |= !Item || Item->Type >= ObjectType::Weather ? 0u : IsParamShown(*Item, Param::Rotation) ? 1u << static_cast<u32>(Item->Type) : ViewportUi::PointBit;
	}
	const bool IsLit = !(Kinds & (ViewportUi::ActorBit | ViewportUi::ParticleBit)) && (Kinds & (ViewportUi::SpotBit | ViewportUi::PointBit));
	const float Spacing = Style.ItemInnerSpacing.x;
	const auto Toggle = [&Style, Spacing](const char* Label, bool IsOn, const char* Tip)
	{
		ImGui::PushStyleColor(ImGuiCol_Button, Style.Colors[IsOn ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg]);
		const bool IsPressed = ImGui::Button(Label);
		ImGui::PopStyleColor();
		ImGui::SetItemTooltip("%s", Tip);
		ImGui::SameLine(0.0f, Spacing);
		return IsPressed;
	};
	for (u32 Index = 0; Index < 4; ++Index)
	{
		const GizmoMode Mode = Index ? static_cast<GizmoMode>(Index - 1) : GizmoMode::Universal;
		ImGui::BeginDisabled(Mode == GizmoMode::Rotate && Kinds && !(Kinds & ~ViewportUi::PointBit));
		if (Toggle(ViewportUi::ToolLabels[Index], GizmoShown == Mode, ViewportUi::ToolNames[Index == 3 && IsLit ? 4u : Index]))
		{
			Gizmo = Mode;
		}
		ImGui::EndDisabled();
	}
	bool& IsLocal = GizmoItems.size() > 1 ? IsGizmoLocalMulti : IsGizmoLocalSingle;
	ImGui::SameLine(0.0f, Style.ItemSpacing.x);
	if (Toggle(IsLocal ? "Local" : "World", false, "Gizmo axes (X)"))
	{
		IsLocal = !IsLocal;
	}
	if (Toggle("Snap", Settings.IsGizmoSnapping, "Snap the gizmo to the steps; Ctrl while dragging does the opposite. Right-click: steps"))
	{
		Settings.IsGizmoSnapping = !Settings.IsGizmoSnapping;
	}
	if (ImGui::BeginPopupContextItem("##Steps"))
	{
		float* const Steps[4] = { &Settings.MoveStep, &Settings.RotateStep, &Settings.ScaleStep, &Settings.RangeStep };
		for (u32 Slot = 0; Slot < 4; ++Slot)
		{
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 6.0f);
			ImGui::DragFloat(ViewportUi::StepNames[Slot], Steps[Slot], ViewportUi::StepLo[Slot], ViewportUi::StepLo[Slot], ViewportUi::StepHi[Slot], ViewportUi::StepFormats[Slot], ImGuiSliderFlags_AlwaysClamp);
		}
		ImGui::EndPopup();
	}
	ImGui::SameLine(0.0f, Style.ItemSpacing.x);
	if (Toggle("Key", IsAutoKey, "Auto-key (K)"))
	{
		IsAutoKey = !IsAutoKey;
	}
	if (Toggle("Bones", IsPosing, "Bone posing (B)"))
	{
		IsPosing = !IsPosing;
	}
	if (Toggle("Helpers", Settings.ShowHelpers, "Helpers in the game view"))
	{
		Settings.ShowHelpers = !Settings.ShowHelpers;
	}
	if (Toggle("Free cam", IsEditorCamera, "Editor camera instead of the shot or the game view"))
	{
		if (IsEditorCamera)
		{
			IsEditorCamera = false;
		}
		else
		{
			UseEditorCamera(Core);
		}
	}
	ImGui::NewLine();
	ImGui::End();
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
	if (ImGui::IsMouseDragPastThreshold(ImGuiMouseButton_Left, 4.0f) || !ViewportUi::MouseRay(Origin, Direction) || PickBone(Core, Input.KeyCtrl) || ShowAnimator)
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
		PopupKind == ChooserKind::Preset ? AddActorFromPreset(Core, Source.c_str(), Core.Time, PopupPoint, Facing) : (void)AddObject(Core, AddedType(PopupKind), Source, Core.Time, PopupPoint, Facing);
		ImGui::CloseCurrentPopup();
	}
	if (IsPopupChoosing)
	{
		ImGui::EndPopup();
		return;
	}
	ImGui::SeparatorText("Add here");
	for (u32 Index = 0; !IsPopupChoosing && Index < std::size(AddChoices); ++Index)
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

float Editor::FrameAspect() const
{
	const float Screen = static_cast<float>(Device.TargetWidth) / static_cast<float>(std::max(Device.TargetHeight, 1u));
	const u32 Ratio = std::min(static_cast<u32>(Settings.FrameRatio), 8u);
	return Ratio ? std::min(FrameAspects[Ratio], Screen) : Screen;
}

void Editor::DrawFrameLines(const Player& Core)
{
	const u32 Ratio = std::min(static_cast<u32>(Settings.FrameRatio), 8u);
	if (!Overlay || (Core.View() != ViewSource::Global && Core.View() != ViewSource::Editor) || (!Ratio && !Settings.ShowSafe && !Settings.ShowThirds))
	{
		return;
	}
	const ImGuiViewport* Main = ImGui::GetMainViewport();
	const float Screen = Main->Size.x / std::max(Main->Size.y, 1.0f), Wanted = Ratio ? FrameAspects[Ratio] : Screen;
	const ImVec2 Size = Wanted > Screen ? ImVec2(Main->Size.x, Main->Size.x / Wanted) : ImVec2(Main->Size.y * Wanted, Main->Size.y);
	const ImVec2 Min(Main->Pos.x + 0.5f * (Main->Size.x - Size.x), Main->Pos.y + 0.5f * (Main->Size.y - Size.y)), Max(Min.x + Size.x, Min.y + Size.y);
	const ImVec2 End(Main->Pos.x + Main->Size.x, Main->Pos.y + Main->Size.y);
	const u32 Mask = IM_COL32(0, 0, 0, static_cast<u32>(Settings.FrameMask * 255.0f)), Line = IM_COL32(255, 255, 255, 160), Faint = IM_COL32(255, 255, 255, 90);
	Overlay->AddRectFilled(Main->Pos, ImVec2(End.x, Min.y), Mask);
	Overlay->AddRectFilled(ImVec2(Main->Pos.x, Max.y), End, Mask);
	Overlay->AddRectFilled(ImVec2(Main->Pos.x, Min.y), ImVec2(Min.x, Max.y), Mask);
	Overlay->AddRectFilled(ImVec2(Max.x, Min.y), ImVec2(End.x, Max.y), Mask);
	if (Ratio)
	{
		Overlay->AddRect(Min, Max, Line);
	}
	for (u32 Inset = 1; Settings.ShowSafe && Inset <= 2; ++Inset)
	{
		const ImVec2 Cut(Size.x * 0.05f * static_cast<float>(Inset), Size.y * 0.05f * static_cast<float>(Inset));
		Overlay->AddRect(ImVec2(Min.x + Cut.x, Min.y + Cut.y), ImVec2(Max.x - Cut.x, Max.y - Cut.y), Faint);
	}
	const ImVec2 Centre(0.5f * (Min.x + Max.x), 0.5f * (Min.y + Max.y));
	if (Settings.ShowSafe)
	{
		Overlay->AddLine(ImVec2(Centre.x - 10.0f, Centre.y), ImVec2(Centre.x + 10.0f, Centre.y), Line);
		Overlay->AddLine(ImVec2(Centre.x, Centre.y - 10.0f), ImVec2(Centre.x, Centre.y + 10.0f), Line);
	}
	for (u32 Part = 1; Settings.ShowThirds && Part <= 2; ++Part)
	{
		const float Third = static_cast<float>(Part) / 3.0f;
		Overlay->AddLine(ImVec2(Min.x + Size.x * Third, Min.y), ImVec2(Min.x + Size.x * Third, Max.y), Faint);
		Overlay->AddLine(ImVec2(Min.x, Min.y + Size.y * Third), ImVec2(Max.x, Min.y + Size.y * Third), Faint);
	}
}

void Editor::RigPath(string_path& Path) const
{
	string_path Name;
	xr_sprintf(Name, "cinema\\rigs\\%s.ltx", RigVisual.c_str());
	for (char* At = Name + xr_strlen("cinema\\rigs\\"); *At; ++At)
	{
		*At = *At == '\\' || *At == '/' ? '_' : *At;
	}
	FS.update_path(Path, "$game_config$", Name);
}

void Editor::BuildRig(const ActorRuntime& Runtime, const shared_str& Visual)
{
	Rig = ActorRig {};
	RigVisual = Visual;
	const u16 Count = Runtime.BoneCount();
	const auto Bone = [&](const char* Name) { const u16 Id = Runtime.FindBone(shared_str(Name)); return Id < Count ? Id : BI_NONE; };
	constexpr const char* Auto[RigControlCount][3] = { { "bip01_pelvis" }, { "bip01_spine2", "bip01_spine1", "bip01_spine" }, { "bip01_head" }, { "bip01_l_hand" }, { "bip01_r_hand" }, { "bip01_l_foot" }, { "bip01_r_foot" }, { "eye_left" }, { "eye_right" }, { "jaw_1", "bip01_jaw" },
		{ "bip01" }, { "bip01_spine" }, { "bip01_spine1" }, { "bip01_neck" }, { "bip01_l_clavicle" }, { "bip01_r_clavicle" }, { "bip01_l_toe0" }, { "bip01_r_toe0" },
		{ "bip01_l_finger0" }, { "bip01_l_finger1" }, { "bip01_l_finger2" }, { "bip01_l_finger3" }, { "bip01_l_finger4" }, { "bip01_r_finger0" }, { "bip01_r_finger1" }, { "bip01_r_finger2" }, { "bip01_r_finger3" }, { "bip01_r_finger4" } };
	constexpr const char* Chains[RigChainCount][3] = { { "bip01_l_upperarm", "bip01_l_forearm", "bip01_l_hand" }, { "bip01_r_upperarm", "bip01_r_forearm", "bip01_r_hand" }, { "bip01_l_thigh", "bip01_l_calf", "bip01_l_foot" }, { "bip01_r_thigh", "bip01_r_calf", "bip01_r_foot" } };
	for (u32 Control = 0; Control < RigControlCount; ++Control)
	{
		for (u32 Try = 0; Try < 3 && Auto[Control][Try] && Rig.Controls[Control] == BI_NONE; ++Try)
		{
			Rig.Controls[Control] = Bone(Auto[Control][Try]);
		}
		Rig.Controls[Control] = std::find(Rig.Controls, Rig.Controls + Control, Rig.Controls[Control]) == Rig.Controls + Control ? Rig.Controls[Control] : BI_NONE;
	}
	for (u32 Chain = 0; Chain < RigChainCount; ++Chain)
	{
		for (u32 Link = 0; Link < 3; ++Link)
		{
			Rig.Chains[Chain][Link] = Bone(Chains[Chain][Link]);
		}
	}
	constexpr const char* HandChains[2] = { "left_hand_ik_chain", "right_hand_ik_chain" };
	const char* Hands = pSettings->read_if_exists<str_c>("actor", "player_hud_default", "actor_hud");
	for (u32 Side = 0; Side < 2; ++Side)
	{
		const char* Common = pSettings->section_exist("camera_rig") ? pSettings->read_if_exists<str_c>("camera_rig", HandChains[Side], nullptr) : nullptr;
		const char* Names = pSettings->section_exist(Hands) ? pSettings->read_if_exists<str_c>(Hands, HandChains[Side], Common) : Common;
		u16* Links = Rig.Chains[Side];
		if (!Names || _GetItemCount(Names) != 3 || std::find(Links, Links + 3, BI_NONE) == Links + 3)
		{
			continue;
		}
		string128 Part;
		u16 Found[3];
		for (int Link = 0; Link < 3; ++Link)
		{
			Found[Link] = Bone(_GetItem(Names, Link, Part));
		}
		if (std::find(Found, Found + 3, BI_NONE) == Found + 3)
		{
			std::copy(Found, Found + 3, Links);
			Rig.Controls[3 + Side] = Rig.Controls[3 + Side] == BI_NONE ? Found[2] : Rig.Controls[3 + Side];
		}
	}
	Rig.Mirror.assign(Count, BI_NONE);
	constexpr const char* Sides[4][2] = { { "_l_", "_r_" }, { "_r_", "_l_" }, { "left", "right" }, { "right", "left" } };
	for (u16 Id = 0; Id < Count; ++Id)
	{
		const xr_string Name = Runtime.BoneName(Id) ? Runtime.BoneName(Id) : "";
		for (const auto& Side : Sides)
		{
			const size_t At = Name.find(Side[0]);
			Rig.Mirror[Id] = At != xr_string::npos && Rig.Mirror[Id] == BI_NONE ? Bone((Name.substr(0, At) + Side[1] + Name.substr(At + xr_strlen(Side[0]))).c_str()) : Rig.Mirror[Id];
		}
	}
	string_path Path;
	RigPath(Path);
	const CReaderGuarded File(FS.exist(Path) ? FS.r_open(Path) : nullptr);
	xr_string Error = "the file is larger than 64 KiB";
	if (!File)
	{
		return;
	}
	if (static_cast<size_t>(File->length()) > 65536 || !Io::PreScan(*File, Error))
	{
		Msg("! Cinema editor: %s is ignored: %s", Path, Error.c_str());
		return;
	}
	File->seek(0);
	const CInifile Ini(&*File, nullptr);
	Rig.HasOverride = true;
	const auto Named = [&](const char* Section, const char* Key, u16& Out)
	{
		const char* Value = Ini.section_exist(Section) && Ini.line_exist(Section, Key) ? Ini.r_string(Section, Key) : nullptr;
		const u16 Id = Value ? Bone(Value) : BI_NONE;
		Out = Id != BI_NONE ? Id : Out;
		if (Value && Id == BI_NONE)
		{
			Msg("! Cinema editor: %s: [%s] %s names the missing bone %s", Path, Section, Key, Value);
		}
	};
	for (u32 Control = 0; Control < RigControlCount; ++Control)
	{
		Named("controls", RigControlKeys[Control], Rig.Controls[Control]);
	}
	for (u32 Chain = 0; Chain < RigChainCount; ++Chain)
	{
		const char* Value = Ini.section_exist("chains") && Ini.line_exist("chains", RigChainKeys[Chain]) ? Ini.r_string("chains", RigChainKeys[Chain]) : nullptr;
		string256 Part;
		u16 Links[3] = { BI_NONE, BI_NONE, BI_NONE };
		for (u32 Link = 0; Value && Link < 3 && Link < static_cast<u32>(_GetItemCount(Value)); ++Link)
		{
			Links[Link] = Bone(_GetItem(Value, static_cast<int>(Link), Part));
		}
		const bool IsValid = Value && Links[0] != BI_NONE && Links[1] != BI_NONE && Links[2] != BI_NONE;
		std::copy(std::begin(Links), std::end(Links), IsValid ? Rig.Chains[Chain] : Links);
		if (Value && !IsValid)
		{
			Msg("! Cinema editor: %s: [chains] %s needs three existing bones", Path, RigChainKeys[Chain]);
		}
	}
	for (u32 Line = 0; Ini.section_exist("mirror") && Line < Ini.line_count("mirror"); ++Line)
	{
		const char *Key = nullptr, *Value = nullptr;
		Ini.r_line("mirror", static_cast<int>(Line), &Key, &Value);
		const u16 Left = Key ? Bone(Key) : BI_NONE, Right = Value ? Bone(Value) : BI_NONE;
		if (Left != BI_NONE && Right != BI_NONE)
		{
			Rig.Mirror[Left] = Right;
			Rig.Mirror[Right] = Left;
		}
	}
}

void Editor::SaveRig()
{
	const Player* Core = FindPlayer();
	const ActorRuntime* Runtime = Core ? Core->FindActor(RigObject) : nullptr;
	if (!Runtime)
	{
		return;
	}
	CInifile Ini(nullptr, false, false, false);
	string_path Path;
	RigPath(Path);
	Ini.w_u32("rig", "version", 1);
	for (u32 Control = 0; Control < RigControlCount; ++Control)
	{
		if (Rig.Controls[Control] != BI_NONE)
		{
			Ini.w_string("controls", RigControlKeys[Control], Runtime->BoneName(Rig.Controls[Control]));
		}
	}
	for (u32 Chain = 0; Chain < RigChainCount; ++Chain)
	{
		const u16* Links = Rig.Chains[Chain];
		if (Links[0] != BI_NONE && Links[1] != BI_NONE && Links[2] != BI_NONE)
		{
			Ini.w_string("chains", RigChainKeys[Chain], (xr_string(Runtime->BoneName(Links[0])) + ", " + Runtime->BoneName(Links[1]) + ", " + Runtime->BoneName(Links[2])).c_str());
		}
	}
	for (u16 Id = 0; Id < Rig.Mirror.size(); ++Id)
	{
		if (Rig.Mirror[Id] != BI_NONE && Rig.Mirror[Id] > Id)
		{
			Ini.w_string("mirror", Runtime->BoneName(Id), Runtime->BoneName(Rig.Mirror[Id]));
		}
	}
	xr_string Error;
	if (!Io::SaveIni(Ini, Path, Error))
	{
		SetStatus(true, "%s", Error.c_str());
		return;
	}
	Rig.HasOverride = true;
	SetStatus(false, "Rig saved to %s", Path);
}

namespace
{
bool IkGoalWorld(const Object& Actor, s32 Block, const Fmatrix& World, u32 Limb, bool IsPole, float Local, Fmatrix& Out)
{
	const IkLimb& Setup = IkOf(Actor, Block)[Limb];
	const xr_vector<Lane>& Tracks = LaneList(Actor, Block);
	if (!Setup.IsOn)
	{
		return false;
	}
	const Fvector Turn = Curves::LaneVector(Tracks, Param::IkTurn, Local, Setup.Turn, static_cast<u8>(Limb));
	IsPole ? Out.identity() : Out.setHPB(deg2rad(Turn.x), deg2rad(Turn.y), deg2rad(Turn.z));
	Out.c = Curves::LaneVector(Tracks, IsPole ? Param::IkPole : Param::IkTarget, Local, IsPole ? Setup.Pole : Setup.Target, static_cast<u8>(Limb));
	if (Setup.Space == IkSpace::Actor)
	{
		Out.mulA_43(World);
		Out.i.normalize_safe();
		Out.j.normalize_safe();
		Out.k.normalize_safe();
	}
	return true;
}
}

s32 Editor::BoneUnderMouse(const Player& Core) const
{
	const ActorRuntime* Runtime = Core.FindActor(PoseObject);
	const ObjectState* State = Runtime ? Core.FindState(PoseObject) : nullptr;
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	float Best = FLT_MAX;
	s32 Found = -1;
	for (u32 Control = 0; State && RigObject == PoseObject && Control < RigControlCount; ++Control)
	{
		Fmatrix Joint;
		ImVec2 At;
		const u16 Bone = Rig.Controls[Control];
		const float Gap = Bone != BI_NONE && !IsBoneShared(Bone) && Runtime->BoneWorld(Bone, State->World, Joint) && ViewportUi::Project(Joint.c, At) > 0.0f ? std::hypot(At.x - Mouse.x, At.y - Mouse.y) : FLT_MAX;
		float Ring = FLT_MAX;
		for (u32 Step = 0; Gap != FLT_MAX && ViewportUi::RigReach(Control) > 0.0f && Step < 24; ++Step)
		{
			ImVec2 Spot;
			Ring = ViewportUi::Project(ViewportUi::RigRing(Joint.c, ViewportUi::RigReach(Control), Step), Spot) > 0.0f ? std::min(Ring, std::hypot(Spot.x - Mouse.x, Spot.y - Mouse.y)) : Ring;
		}
		const bool IsHit = Gap <= RigControlSizes[Control] + 2.0f || Ring <= 6.0f;
		const float Score = std::min(Gap, Ring);
		Found = IsHit && Score < Best ? static_cast<s32>(Bone) : Found;
		Best = IsHit ? std::min(Score, Best) : Best;
	}
	if (Found >= 0)
	{
		return Found;
	}
	Best = 8.0f;
	for (u16 Bone = 0; State && Bone < Runtime->BoneCount(); ++Bone)
	{
		Fmatrix Self, Parent;
		ImVec2 A, B;
		if (IsBoneShared(Bone) || (!ShowBoneLines && HasRigControls()) || !Runtime->BoneWorld(Bone, State->World, Self) || !(ViewportUi::Project(Self.c, A) > 0.0f))
		{
			continue;
		}
		B = Runtime->BoneWorld(Runtime->BoneParent(Bone), State->World, Parent) && ViewportUi::Project(Parent.c, B) > 0.0f ? B : A;
		const ImVec2 Line(B.x - A.x, B.y - A.y);
		const float Along = std::clamp(((Mouse.x - A.x) * Line.x + (Mouse.y - A.y) * Line.y) / std::max(Line.x * Line.x + Line.y * Line.y, 1.0f), 0.0f, 1.0f);
		const float Gap = std::hypot(A.x + Line.x * Along - Mouse.x, A.y + Line.y * Along - Mouse.y) + 2.0f * Along;
		Found = Gap < Best ? static_cast<s32>(Bone) : Found;
		Best = std::min(Gap, Best);
	}
	return Found;
}

bool Editor::PickBone(Player& Core, bool IsToggle)
{
	if (!IsPosing || !PoseObject || !Core.FindActor(PoseObject))
	{
		return false;
	}
	const s32 Bone = BoneUnderMouse(Core);
	const bool IsAdding = ImGui::GetIO().KeyShift;
	if (Bone < 0)
	{
		const bool HadBones = !SelectedBones.empty() && !IsAdding;
		SelectedBones.resize(IsAdding ? SelectedBones.size() : 0);
		return HadBones;
	}
	const auto Found = std::find(SelectedBones.begin(), SelectedBones.end(), static_cast<u16>(Bone));
	if (!IsToggle && !IsAdding)
	{
		SelectedBones.assign(1, static_cast<u16>(Bone));
	}
	else if (Found == SelectedBones.end())
	{
		SelectedBones.push_back(static_cast<u16>(Bone));
	}
	else if (IsToggle)
	{
		SelectedBones.erase(Found);
	}
	return true;
}

void Editor::DrawBones(Player& Core)
{
	const Object* Actor = nullptr;
	for (u32 Id : Selected)
	{
		const Object* Item = Find(Core.Data, Id);
		Actor = !Actor && Item && Item->Type == ObjectType::Actor ? Item : Actor;
	}
	if (!Actor || Actor->Id != PoseObject)
	{
		SelectedBones.clear();
		PoseObject = Actor ? Actor->Id : 0;
	}
	const ActorRuntime* Runtime = Actor ? Core.FindActor(Actor->Id) : nullptr;
	const ObjectState* State = Runtime ? Core.FindState(Actor->Id) : nullptr;
	if (Runtime && Runtime->BoneCount() && (RigObject != Actor->Id || RigVisual != Actor->Actor.Visual || Rig.Mirror.size() != Runtime->BoneCount()))
	{
		RigObject = Actor->Id;
		BuildRig(*Runtime, Actor->Actor.Visual);
	}
	const ActorRuntime* Hands = Runtime && Actor->Actor.Hud.size() && Actor->Attach.Parent.ObjectId ? Core.FindActor(Actor->Attach.Parent.ObjectId) : nullptr;
	if (SharedOwner != PoseObject || SharedHands != Hands || SharedBones.size() != (Hands ? Runtime->BoneCount() : 0))
	{
		SharedOwner = PoseObject;
		SharedHands = Hands;
		SharedBones.assign(Hands ? Runtime->BoneCount() : 0, 0);
		for (u16 Bone = 0; Bone < SharedBones.size(); ++Bone)
		{
			SharedBones[Bone] = Hands->FindBone(shared_str(Runtime->BoneName(Bone))) < Hands->BoneCount();
		}
		std::erase_if(SelectedBones, [this](u16 Bone) { return IsBoneShared(Bone); });
	}
	if (!IsPosing || !Overlay || !State)
	{
		return;
	}
	const s32 Hover = IsViewportFree() && !IsGizmoHovered && !IsGizmoActive ? BoneUnderMouse(Core) : -1;
	const ImGuiIO& Input = ImGui::GetIO();
	if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		IsBoneBox = IsViewportFree() && !IsGizmoHovered && !IsGizmoActive;
		BoneBoxStart = Input.MousePos;
	}
	IsBoneBox = IsBoneBox && !ImGui::IsMouseClicked(ImGuiMouseButton_Right) && !ImGui::IsKeyPressed(ImGuiKey_Escape, false);
	const ImRect Box(ImMin(BoneBoxStart, Input.MousePos), ImMax(BoneBoxStart, Input.MousePos));
	const bool IsBoxing = IsBoneBox && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Left] >= 16.0f;
	const bool IsClosing = IsBoxing && ImGui::IsMouseReleased(ImGuiMouseButton_Left);
	if (IsClosing && !Input.KeyCtrl && !Input.KeyShift)
	{
		SelectedBones.clear();
	}
	if (IsBoxing)
	{
		Overlay->AddRectFilled(Box.Min, Box.Max, IM_COL32(255, 255, 255, 30));
		Overlay->AddRect(Box.Min, Box.Max, IM_COL32(255, 255, 255, 160));
	}
	for (u16 Bone = 0; Bone < Runtime->BoneCount(); ++Bone)
	{
		Fmatrix Self, Parent;
		if (IsBoneShared(Bone) || !Runtime->BoneWorld(Bone, State->World, Self))
		{
			continue;
		}
		const bool IsPicked = std::find(SelectedBones.begin(), SelectedBones.end(), Bone) != SelectedBones.end();
		if (!ShowBoneLines && !IsPicked && HasRigControls())
		{
			continue;
		}
		const bool IsEdited = std::any_of(Actor->Actor.Poses.begin(), Actor->Actor.Poses.end(), [&](const PoseRow& Row) { return std::any_of(Row.Bones.begin(), Row.Bones.end(), [&](const PoseBone& Item) { return !xr_strcmp(Item.Name, Runtime->BoneName(Bone)); }); });
		const u32 Color = IsPicked ? IM_COL32(255, 255, 255, 255) : static_cast<s32>(Bone) == Hover ? IM_COL32(255, 220, 60, 255) : IsEdited ? IM_COL32(255, 150, 40, 220) : IM_COL32(160, 180, 200, 170);
		if (Runtime->BoneWorld(Runtime->BoneParent(Bone), State->World, Parent))
		{
			ViewportUi::Line3D(Overlay, Parent.c, Self.c, Color);
		}
		ImVec2 At;
		if (ViewportUi::Project(Self.c, At) > 0.0f)
		{
			Overlay->AddCircleFilled(At, IsPicked ? 4.0f : 2.5f, Color);
			const auto Found = std::find(SelectedBones.begin(), SelectedBones.end(), Bone);
			if (IsClosing && Box.Contains(At))
			{
				Found == SelectedBones.end() ? SelectedBones.push_back(Bone) : Input.KeyCtrl ? (void)SelectedBones.erase(Found) : (void)0;
			}
		}
	}
	IsBoneBox = IsBoneBox && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	for (u32 Control = 0; Control < RigControlCount; ++Control)
	{
		Fmatrix Joint;
		ImVec2 At;
		const u16 Bone = Rig.Controls[Control];
		if (Bone == BI_NONE || IsBoneShared(Bone) || !Runtime->BoneWorld(Bone, State->World, Joint) || !(ViewportUi::Project(Joint.c, At) > 0.0f))
		{
			continue;
		}
		constexpr u32 SideColors[3] = { IM_COL32(255, 210, 60, 230), IM_COL32(90, 150, 255, 230), IM_COL32(255, 90, 90, 230) };
		const bool IsPicked = std::find(SelectedBones.begin(), SelectedBones.end(), Bone) != SelectedBones.end();
		const u32 Color = IsPicked ? IM_COL32(255, 255, 255, 255) : static_cast<s32>(Bone) == Hover ? IM_COL32(255, 240, 120, 255) : SideColors[RigControlSides[Control]];
		const float Size = RigControlSizes[Control], Reach = ViewportUi::RigReach(Control);
		for (u32 Step = 0; Reach > 0.0f && Step < 24; ++Step)
		{
			ViewportUi::Line3D(Overlay, ViewportUi::RigRing(Joint.c, Reach, Step), ViewportUi::RigRing(Joint.c, Reach, Step + 1), Color);
		}
		for (u32 Corner = 0; Control >= 3 && Control <= 6 && Corner < 8; ++Corner)
		{
			const Fvector* const Axes[3] = { &Joint.i, &Joint.j, &Joint.k };
			const auto At3 = [&](u32 Code)
			{
				Fvector Point = Joint.c;
				for (u32 Axis = 0; Axis < 3; ++Axis)
				{
					Point.mad(*Axes[Axis], (Code & (1u << Axis) ? 0.05f : -0.05f) / std::max(Axes[Axis]->magnitude(), EPS_S));
				}
				return Point;
			};
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				if (!(Corner & (1u << Axis)))
				{
					ViewportUi::Line3D(Overlay, At3(Corner), At3(Corner | (1u << Axis)), Color);
				}
			}
		}
		if (IsClosing && Box.Contains(At) && !IsPicked)
		{
			SelectedBones.push_back(Bone);
		}
		if (!RigControlShapes[Control])
		{
			Overlay->AddRect(ImVec2(At.x - Size, At.y - Size), ImVec2(At.x + Size, At.y + Size), Color, 0.0f, 0, 2.0f);
		}
		else
		{
			Overlay->AddNgon(At, Size, Color, RigControlShapes[Control], 2.0f);
		}
	}
	if (Hover >= 0)
	{
		ImGui::SetTooltip("%s", Runtime->BoneName(static_cast<u16>(Hover)));
	}
}

void Editor::DrawPathKeys(Player& Core)
{
	const ImGuiIO& Input = ImGui::GetIO();
	const bool HasHelpers = Settings.ShowHelpers && Core.View() != ViewSource::Global;
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	IsPathKeyCancelled = IsPathKeyCancelled && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	if (IsPathKeyActive && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		Undo.Cancel(Core.Data);
		IsPathKeyActive = false;
		IsPathKeyCancelled = true;
	}
	const bool IsSeeking = HasHelpers && !IsGizmoHovered && IsViewportFree() && !IsPathKeyActive;
	const Object* Rig = FindCamera(Core.Data);
	const s32 KeyBlock = Rig && HasHelpers ? HelperBlock(Core, *Rig) : -1;
	const BlockCache* Cache = KeyBlock >= 0 ? Core.FindCameraBlock(static_cast<u32>(KeyBlock)) : nullptr;
	const CameraBlock* Shot = Cache ? &Rig->Camera.Blocks[KeyBlock] : nullptr;
	ViewportUi::PathSpot Hover, Insert;
	Hover.Gap = ViewportUi::TangentPixels * ViewportUi::TangentPixels;
	Insert.Gap = ViewportUi::GrabPixels * ViewportUi::GrabPixels;
	Fvector Point;
	Fmatrix Turn;
	for (u32 At = 0; IsSeeking && At < Selected.size(); ++At)
	{
		const Object* Item = Find(Core.Data, Selected[At]);
		const Lane* Track = Item && Item->Type != ObjectType::Camera ? Curves::FindLane(Item->Lanes, Param::Position, 0) : nullptr;
		for (u32 Axis = 0; Track && Axis < 3; ++Axis)
		{
			for (const Key& Entry : Track->Curves[Axis].Keys)
			{
				const bool IsRepeat = (Axis > 0 && Curves::KeyAt(Track->Curves[0], Entry.Time, HalfFrame) != u32(-1)) || (Axis > 1 && Curves::KeyAt(Track->Curves[1], Entry.Time, HalfFrame) != u32(-1));
				if (!IsRepeat && Core.PathPoint(Item->Id, Item->Span.Start + Entry.Time, Point))
				{
					ViewportUi::Offer(Hover, Item->Id, -1, -1, Entry.Time, Point);
				}
			}
		}
	}
	for (u32 At = 0; IsSeeking && Shot && At < Shot->Keys.size(); ++At)
	{
		Point = Shot->Keys[At].Position;
		Turn = Fidentity;
		ViewportUi::Place(Cache->Frames, Point, Turn);
		ViewportUi::Offer(Hover, Rig->Id, KeyBlock, static_cast<s32>(At), Shot->Keys[At].Time, Point);
	}
	for (u32 At = 0; IsSeeking && !Hover.Id && Input.KeyCtrl && At < Selected.size(); ++At)
	{
		const Object* Item = Find(Core.Data, Selected[At]);
		const bool HasPath = Item && Item->Type != ObjectType::Camera && Curves::FindLane(Item->Lanes, Param::Position, 0);
		const float Length = HasPath ? Item->Span.Length : 0.0f, Step = std::max(2.0f * HalfFrame, Length / 600.0f);
		for (float Local = 0.0f; HasPath && Local <= Length; Local += Step)
		{
			if (Core.PathPoint(Item->Id, Item->Span.Start + Local, Point))
			{
				ViewportUi::Offer(Insert, Item->Id, -1, -1, Local, Point);
			}
		}
	}
	for (u32 At = 0; IsSeeking && !Hover.Id && Input.KeyCtrl && Shot && At + 1 < Shot->Keys.size(); ++At)
	{
		for (u32 Part = 1; Part < 16; ++Part)
		{
			const float Moment = _lerp(Shot->Keys[At].Time, Shot->Keys[At + 1].Time, static_cast<float>(Part) / 16.0f);
			Cameras::PathPose(*Shot, Moment, Point, Turn);
			ViewportUi::Place(Cache->Frames, Point, Turn);
			ViewportUi::Offer(Insert, Rig->Id, KeyBlock, -1, Moment, Point);
		}
	}
	const bool IsPressed = !IsPathKeyCancelled && ImGui::IsMouseClicked(ImGuiMouseButton_Left);
	if (Hover.Id || Insert.Id)
	{
		Overlay->AddCircle(Hover.Id ? Hover.At : Insert.At, Hover.Id ? 10.0f : 6.0f, IM_COL32(255, 220, 60, 255), 0, 2.0f);
		Hint = Hover.Id ? ViewportUi::PathHint : ViewportUi::InsertHint;
	}
	if (Insert.Id && IsPressed)
	{
		Object& Edited = *Edit(Core, Insert.Id, "Insert path key");
		if (Insert.Block >= 0)
		{
			CameraBlock& Clip = Edited.Camera.Blocks[Insert.Block];
			const auto Next = std::upper_bound(Clip.Keys.begin(), Clip.Keys.end(), Insert.Time, [](float Moment, const PoseKey& Item) { return Moment < Item.Time; });
			const Interp Mode = Next == Clip.Keys.begin() ? Interp::EaseInOut : (Next - 1)->Mode;
			Cameras::PathPose(Clip, Insert.Time, Point, Turn);
			Cameras::SetPoseAt(Clip, Insert.Time, Point, Curves::LaneVector(Clip.Lanes, Param::Rotation, Insert.Time, Clip.Rotation), Mode, 1e-4f);
			Core.IsLookingThrough = false;
			Core.Seek(TargetStart(Edited, Insert.Block) + Cameras::LocalFromPath(Clip, Insert.Time));
		}
		else
		{
			Lane& Track = Curves::EnsureLane(Edited.Lanes, Param::Position, 0);
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				Curves::SetKey(Track.Curves[Axis], Insert.Time, Curves::Value(Track.Curves[Axis], Insert.Time, (*PlacementOf(Edited, 0))[Axis]), HalfFrame);
			}
			Core.Seek(Edited.Span.Start + Insert.Time);
		}
		Core.Pause();
	}
	if (Hover.Id && IsPressed)
	{
		const Object* Owner = Find(Core.Data, Hover.Id);
		const Lane* Track = Curves::FindLane(Hover.Block >= 0 ? Shot->Lanes : Owner->Lanes, Param::Position, 0);
		IsPathKeyActive = true;
		PathKeyObject = Hover.Id;
		PathKeyBlock = Hover.Block;
		PathKeyTime = Hover.Time;
		PathKeyStart = Hover.Point;
		PathKeyBase = Track ? *Track : Lane();
		Core.Pause();
		if (Hover.Block >= 0)
		{
			Focus = FocusKind::Block;
			FocusObject = Hover.Id;
			FocusBlock = Hover.Block;
			SetTangentKey(Core, Hover.Block, Hover.Key);
			Core.IsLookingThrough = false;
		}
		Core.Seek(Hover.Block >= 0 ? TargetStart(*Owner, Hover.Block) + Cameras::LocalFromPath(*Shot, *Cache, Hover.Time) : Owner->Span.Start + Hover.Time);
	}
	IsGizmoHovered = IsGizmoHovered || Hover.Id || Insert.Id || IsPathKeyActive;
	const Object* Item = IsPathKeyActive ? Find(Core.Data, PathKeyObject) : nullptr;
	const ObjectState* State = Item && PathKeyBlock < 0 ? Core.FindState(PathKeyObject) : nullptr;
	const BlockCache* Held = Item && PathKeyBlock >= 0 && PathKeyBlock < static_cast<s32>(Item->Camera.Blocks.size()) ? Core.FindCameraBlock(static_cast<u32>(PathKeyBlock)) : nullptr;
	Fvector Origin, Direction, Normal;
	if ((!State && !Held) || !ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		IsPathKeyActive = false;
		return;
	}
	Hint = ViewportUi::PathHint;
	if (!ViewportUi::MouseRay(Origin, Direction) || !ImGui::IsMouseDragPastThreshold(ImGuiMouseButton_Left, 2.0f) || std::fabs(Direction.dotproduct(Device.vCameraDirection)) < EPS)
	{
		return;
	}
	Fmatrix Pose = Fidentity;
	if (!Input.KeyShift || !StaticRay(Origin, Direction, ViewportUi::PickRange, Pose.c, Normal))
	{
		Pose.c.mad(Origin, Direction, Fvector().sub(PathKeyStart, Origin).dotproduct(Device.vCameraDirection) / Direction.dotproduct(Device.vCameraDirection));
	}
	Spaces::Decompose(Held ? Held->Frames : State->Frames, Pose);
	Object& Edited = *Edit(Core, PathKeyObject, "Move path key");
	Lane& Track = Curves::EnsureLane(Held ? Edited.Camera.Blocks[PathKeyBlock].Lanes : Edited.Lanes, Param::Position, 0);
	for (u8 Axis = 0; Axis < 3; ++Axis)
	{
		if (Held)
		{
			Curves::SetKey(Track.Curves[Axis], PathKeyTime, Pose.c[Axis], 1e-4f);
		}
		else
		{
			WriteParam(Core, Edited, -1, Param::Position, 0, Axis, Pose.c[Axis]);
		}
	}
	Lane& Moved = Curves::EnsureLane(Held ? Edited.Camera.Blocks[PathKeyBlock].Lanes : Edited.Lanes, Param::Position, 0);
	ViewportUi::FitHandles(Moved, PathKeyBase, PathKeyTime, Held ? 1e-4f : HalfFrame);
	if (Held)
	{
		Cameras::Rebuild(Edited.Camera.Blocks[PathKeyBlock]);
	}
}

void Editor::EvenSpeed(Player& Core, s32 Block)
{
	const Object* Rig = FindCamera(Core.Data);
	if (!Rig || Block < 0 || Block >= static_cast<s32>(Rig->Camera.Blocks.size()) || Rig->Camera.Blocks[Block].Keys.size() < 3)
	{
		return;
	}
	const CameraBlock& Shot = Rig->Camera.Blocks[Block];
	const u32 Count = static_cast<u32>(Shot.Keys.size());
	const float Frame = 1.0f / static_cast<float>(Core.Data.Header.FrameRate);
	DragTimes.clear();
	BlockStarts.assign(1, 0.0f);
	Fvector From, To;
	Fmatrix Turn;
	for (u32 At = 0; At < Count; ++At)
	{
		float Reach = 0.0f;
		DragTimes.push_back(Shot.Keys[At].Time);
		Cameras::PathPose(Shot, Shot.Keys[At].Time, From, Turn);
		for (u32 Part = 1; At + 1 < Count && Part <= 16; ++Part)
		{
			Cameras::PathPose(Shot, _lerp(Shot.Keys[At].Time, Shot.Keys[At + 1].Time, static_cast<float>(Part) / 16.0f), To, Turn);
			Reach += From.distance_to(To);
			From = To;
		}
		if (At + 1 < Count)
		{
			BlockStarts.push_back(BlockStarts.back() + Reach);
		}
	}
	const float Total = BlockStarts.back(), First = DragTimes.front(), Span = DragTimes.back() - First;
	if (!(Total > EPS_L))
	{
		SetStatus(true, "The camera does not move in this clip.");
		return;
	}
	for (u32 At = 0; At < Count; ++At)
	{
		BlockStarts[At] = std::max(First + Span * BlockStarts[At] / Total, At ? BlockStarts[At - 1] + Frame : First);
	}
	BlockStarts[Count - 1] = DragTimes.back();
	for (u32 At = Count - 1; At-- > 0;)
	{
		BlockStarts[At] = std::min(BlockStarts[At], BlockStarts[At + 1] - Frame);
	}
	if (BlockStarts[0] < First - 1e-4f)
	{
		SetStatus(true, "The clip has too many keys for its length to space them evenly.");
		return;
	}
	CameraBlock& Edited = Edit(Core, Rig->Id, "Even speed")->Camera.Blocks[Block];
	for (Lane& Track : Edited.Lanes)
	{
		for (u32 Axis = 0; (Track.Id == Param::Position || Track.Id == Param::Rotation) && Track.Layer == 0 && Axis < 3; ++Axis)
		{
			xr_vector<Key>& Keys = Track.Curves[Axis].Keys;
			for (Key& Item : Keys)
			{
				const auto Near = std::lower_bound(DragTimes.begin(), DragTimes.end(), Item.Time - 1e-4f);
				Item.Time = Near == DragTimes.end() ? Item.Time : BlockStarts[Near - DragTimes.begin()];
			}
			for (u32 At = 0; At < Keys.size(); ++At)
			{
				Keys[At].InTime = At ? -(Keys[At].Time - Keys[At - 1].Time) / 3.0f : Keys[At].InTime;
				Keys[At].OutTime = At + 1 < Keys.size() ? (Keys[At + 1].Time - Keys[At].Time) / 3.0f : Keys[At].OutTime;
			}
		}
	}
	Cameras::Rebuild(Edited);
	SelectedKeys.clear();
}

void Editor::DrawIkHandles(Player& Core)
{
	const ImGuiIO& Input = ImGui::GetIO();
	const Object* Actor = IsPosing ? Find(Core.Data, PoseObject) : nullptr;
	const ActorRuntime* Runtime = Actor ? Core.FindActor(PoseObject) : nullptr;
	const ObjectState* State = Runtime ? Core.FindState(PoseObject) : nullptr;
	IsIkCancelled = IsIkCancelled && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	if (IsIkActive && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		Undo.Cancel(Core.Data);
		IsIkActive = false;
		IsIkCancelled = true;
	}
	if (!State)
	{
		IkHandle = -1;
		IsIkActive = IsIkOver = IsIkHover = IsIkDirect = false;
		return;
	}
	const float Local = PoseLocal(Core, *Actor);
	const bool IsFree = IsViewportFree();
	float Best = ViewportUi::TangentPixels * ViewportUi::TangentPixels;
	s32 Hover = -1;
	ImVec2 HoverAt;
	for (u32 Handle = 0; Handle < 2 * MaxIkLimbs; ++Handle)
	{
		Fmatrix Goal, Anchor;
		ImVec2 Screen;
		const u32 Limb = Handle / 2;
		const bool IsPole = Handle & 1, IsPicked = static_cast<s32>(Handle) == IkHandle;
		if (!IkGoalWorld(*Actor, IkBlock(*Actor), State->World, Limb, IsPole, Local, Goal))
		{
			continue;
		}
		const u32 Color = IsPicked ? IM_COL32(255, 255, 255, 255) : Limb & 1 ? IM_COL32(255, 90, 90, 230) : IM_COL32(90, 150, 255, 230);
		const u16 From = Runtime->FindBone(IkOf(*Actor, IkBlock(*Actor))[Limb].Bones[IsPole ? 1 : 2]);
		if (From != BI_NONE && Runtime->BoneWorld(From, State->World, Anchor))
		{
			ViewportUi::Line3D(Overlay, Anchor.c, Goal.c, Color);
		}
		if (!(ViewportUi::Project(Goal.c, Screen) > 0.0f))
		{
			continue;
		}
		const float Radius = (IsPole ? 5.0f : 6.0f) + (IsPicked ? 1.0f : 0.0f);
		Overlay->AddNgonFilled(Screen, Radius, Color, IsPole ? 4 : 16);
		Overlay->AddNgon(Screen, Radius, IM_COL32(0, 0, 0, 200), IsPole ? 4 : 16);
		const float Gap = (Screen.x - Input.MousePos.x) * (Screen.x - Input.MousePos.x) + (Screen.y - Input.MousePos.y) * (Screen.y - Input.MousePos.y);
		Hover = Gap < Best ? static_cast<s32>(Handle) : Hover;
		HoverAt = Gap < Best ? Screen : HoverAt;
		Best = std::min(Gap, Best);
	}
	IsIkHover = Hover >= 0 && IsFree && !IsIkActive;
	if (IsIkHover)
	{
		Overlay->AddCircle(HoverAt, 10.0f, IM_COL32(255, 220, 60, 255), 0, 2.0f);
		ImGui::SetTooltip("%s %s", RigChainKeys[Hover / 2], Hover & 1 ? "pole" : "target");
		Hint = ViewportUi::IkHint;
	}
	if (!IsIkActive && !IsIkCancelled && IsFree && (Hover >= 0 || !IsIkOver) && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		IkHandle = Hover;
		IsIkDirect = Hover >= 0;
		Hover >= 0 ? SelectedBones.clear() : (void)0;
	}
	IsIkDirect = IsIkDirect && !IsIkCancelled && ImGui::IsMouseDown(ImGuiMouseButton_Left);
	IsGizmoHovered = IsGizmoHovered || IsIkHover || IsIkOver || IsIkActive || IsIkDirect;
	const u32 Limb = static_cast<u32>(IkHandle) / 2;
	const bool IsPole = IkHandle & 1;
	Fmatrix Goal;
	if (IkHandle < 0 || !IkGoalWorld(*Actor, IkBlock(*Actor), State->World, Limb, IsPole, Local, Goal))
	{
		IkHandle = -1;
		IsIkActive = IsIkOver = false;
		return;
	}
	if (!IsIkActive)
	{
		IkMatrix = IsPole || !IsGizmoLocalSingle ? Fidentity : Goal;
		IkMatrix.c = Goal.c;
		IkGrab = IkMatrix;
		IkGoal = Goal;
		BoneTurn = Curves::LaneVector(LaneList(*Actor, IkBlock(*Actor)), Param::IkTurn, Local, IkOf(*Actor, IkBlock(*Actor))[Limb].Turn, static_cast<u8>(Limb));
	}
	Fvector Origin, Direction;
	const bool IsDragged = IsIkDirect && ImGui::IsMouseDragPastThreshold(ImGuiMouseButton_Left, 2.0f) && ViewportUi::MouseRay(Origin, Direction) && std::fabs(Direction.dotproduct(Device.vCameraDirection)) > EPS;
	if (IsDragged)
	{
		IkMatrix = IkGrab;
		IkMatrix.c.mad(Origin, Direction, Fvector().sub(IkGoal.c, Origin).dotproduct(Device.vCameraDirection) / Direction.dotproduct(Device.vCameraDirection));
	}
	ImGuizmo::PushID("CinemaIk");
	const float Step = ImGuizmo::GetUsingOperation() == ImGuizmo::ROTATE ? Settings.RotateStep : Settings.MoveStep, Snap[3] = { Step, Step, Step };
	ImGuizmo::Enable(!IsIkDirect && !IsIkCancelled && CImGuiManager::Instance().IsCapturingInputs() && !ImGui::IsPopupOpen(nullptr, ImGuiPopupFlags_AnyPopupId | ImGuiPopupFlags_AnyPopupLevel));
	ImGuizmo::Manipulate(reinterpret_cast<const float*>(&Device.mView), reinterpret_cast<const float*>(&Device.mProject), IsPole ? ImGuizmo::TRANSLATE : static_cast<ImGuizmo::OPERATION>(ImGuizmo::TRANSLATE | ImGuizmo::ROTATE), IsGizmoLocalSingle ? ImGuizmo::LOCAL : ImGuizmo::WORLD, reinterpret_cast<float*>(&IkMatrix), nullptr, Input.KeyCtrl != Settings.IsGizmoSnapping ? Snap : nullptr);
	const bool IsUsing = IsDragged || ImGuizmo::IsUsing();
	IsIkOver = !IsIkDirect && ImGuizmo::IsOver();
	const ImGuizmo::OPERATION Using = ImGuizmo::GetUsingOperation();
	ImGuizmo::PopID();
	IsGizmoHovered = IsGizmoHovered || IsIkOver;
	if (!IsUsing || IsIkCancelled)
	{
		IsIkActive = false;
		return;
	}
	IsIkActive = true;
	Object& Item = *Edit(Core, PoseObject, IsPole ? "Move IK pole" : "Move IK target");
	Fmatrix Value, Inverse;
	Inverse.invert(IkGrab);
	Value.mul_43(IkMatrix, Inverse);
	Value.mulB_43(IkGoal);
	if (IkOf(Item, IkBlock(Item))[Limb].Space == IkSpace::Actor)
	{
		Inverse.invert(State->World);
		Value.mulA_43(Inverse);
		Value.i.normalize_safe();
		Value.j.normalize_safe();
		Value.k.normalize_safe();
	}
	Fvector Angles;
	Value.getHPB(Angles);
	for (u8 Axis = 0; (IsDragged || Using) && Axis < 3; ++Axis)
	{
		const float Degrees = rad2deg(Angles[Axis]);
		!IsDragged && Using == ImGuizmo::ROTATE ? WriteParam(Core, Item, IkBlock(Item), Param::IkTurn, static_cast<u8>(Limb), Axis, Degrees + 360.0f * std::round((BoneTurn[Axis] - Degrees) / 360.0f)) : WriteParam(Core, Item, IkBlock(Item), IsPole ? Param::IkPole : Param::IkTarget, static_cast<u8>(Limb), Axis, Value.c[Axis]);
	}
}

s32 Editor::EnsurePoseBone(Object& Item, const shared_str& Bone)
{
	if (const s32 Block = PoseBlock(Item); Block >= 0)
	{
		xr_vector<PoseBone>& Bones = TrackOf(Item, Block).Bones;
		auto Found = std::find_if(Bones.begin(), Bones.end(), [&Bone](const PoseBone& Entry) { return Entry.Name == Bone; });
		if (Found == Bones.end() && Bones.size() < MaxClipBones && Bone.size())
		{
			Bones.push_back({ Bone });
			Found = Bones.end() - 1;
		}
		return Found == Bones.end() ? -1 : static_cast<s32>(Found - Bones.begin());
	}
	xr_vector<PoseRow>& Rows = Item.Actor.Poses;
	if (Rows.empty())
	{
		Rows.emplace_back();
	}
	ActivePose = std::min<u32>(ActivePose, static_cast<u32>(Rows.size() - 1));
	xr_vector<PoseBone>& Bones = Rows[ActivePose].Bones;
	auto Found = std::find_if(Bones.begin(), Bones.end(), [&Bone](const PoseBone& Entry) { return Entry.Name == Bone; });
	if (Found == Bones.end() && Bones.size() < MaxPoseBones && Bone.size())
	{
		Bones.push_back({ Bone });
		Found = Bones.end() - 1;
	}
	return Found == Bones.end() ? -1 : PoseLayer(ActivePose, static_cast<u32>(Found - Bones.begin()));
}

float Editor::PoseLocal(const Player& Core, const Object& Item) const
{
	const s32 Block = PoseBlock(Item);
	return Block < 0 ? std::clamp(Core.Time - Item.Span.Start, 0.0f, Item.Span.Length) : std::clamp(Core.Time - ClipOrigin, 0.0f, Item.Actor.Clips[ClipIndex(Block)].Length);
}

void Editor::DrawBoneGizmo(Player& Core)
{
	const Object* Actor = Find(Core.Data, PoseObject);
	const ActorRuntime* Runtime = Actor ? Core.FindActor(PoseObject) : nullptr;
	const ObjectState* State = Runtime ? Core.FindState(PoseObject) : nullptr;
	const u16 Bone = SelectedBones[0];
	Fmatrix World;
	if (!State || !Runtime->BoneWorld(Bone, State->World, World))
	{
		IsGizmoActive = false;
		return;
	}
	const shared_str Name = Runtime->BoneName(Bone);
	if (!IsGizmoActive)
	{
		BonePivot = World;
	}
	const ImGuizmo::OPERATION Using = ImGuizmo::GetUsingOperation();
	const float Step = Using == ImGuizmo::SCALE ? Settings.ScaleStep : Using == ImGuizmo::TRANSLATE ? Settings.MoveStep : Settings.RotateStep, Snap[3] = { Step, Step, Step };
	const u32 Operation = Gizmo == GizmoMode::Scale ? ImGuizmo::SCALE : ImGuizmo::UNIVERSAL;
	ImGuizmo::Manipulate(reinterpret_cast<const float*>(&Device.mView), reinterpret_cast<const float*>(&Device.mProject), static_cast<ImGuizmo::OPERATION>(Operation), ImGuizmo::LOCAL, reinterpret_cast<float*>(&BonePivot), nullptr, ImGui::GetIO().KeyCtrl != Settings.IsGizmoSnapping ? Snap : nullptr);
	IsGizmoHovered = ImGuizmo::IsOver();
	const bool IsUsing = ImGuizmo::IsUsing(), IsScaling = ImGuizmo::GetUsingOperation() == ImGuizmo::SCALE;
	if (!IsUsing && !IsGizmoActive)
	{
		return;
	}
	Object& Item = *Edit(Core, PoseObject, IsScaling ? "Scale bone" : "Pose bone");
	const float Local = PoseLocal(Core, Item);
	const s32 Block = PoseBlock(Item);
	const xr_vector<Lane>& Tracks = LaneList(Item, Block);
	if (!IsGizmoActive)
	{
		BoneLayer = EnsurePoseBone(Item, Name);
		const PoseBone* Entry = BoneLayer < 0 ? nullptr : BoneEntry(Item, Block, static_cast<u8>(BoneLayer));
		BoneStart = World;
		BoneTurn = Entry ? Curves::LaneVector(Tracks, Param::BoneRotation, Local, Entry->Rotation, static_cast<u8>(BoneLayer)) : Fvector().set(0.0f, 0.0f, 0.0f);
		BoneShift = Entry ? Curves::LaneVector(Tracks, Param::BonePosition, Local, Entry->Offset, static_cast<u8>(BoneLayer)) : Fvector().set(0.0f, 0.0f, 0.0f);
		BoneSize = Entry ? Curves::LaneVector(Tracks, Param::BoneScale, Local, Entry->Scale, static_cast<u8>(BoneLayer)) : Fvector().set(1.0f, 1.0f, 1.0f);
		BoneParentModel = Fidentity;
		Runtime->BoneWorld(Runtime->BoneParent(Bone), Fidentity, BoneParentModel);
		const bool IsCurl = RigObject == PoseObject && std::find(Rig.Controls + RigFirstFinger, Rig.Controls + RigControlCount, Bone) != Rig.Controls + RigControlCount;
		CurlLayers.clear();
		for (u16 Joint = IsCurl ? Bone : BI_NONE, Depth = 0; Joint != BI_NONE && Depth < 2; ++Depth)
		{
			u16 Child = BI_NONE;
			for (u16 Other = 0; Other < Runtime->BoneCount() && Child == BI_NONE; ++Other)
			{
				Child = Runtime->BoneParent(Other) == Joint && Other != Joint ? Other : BI_NONE;
			}
			Joint = Child;
			const s32 Layer = Joint != BI_NONE ? EnsurePoseBone(Item, Runtime->BoneName(Joint)) : -1;
			Layer >= 0 ? CurlLayers.push_back(Layer) : (void)0;
		}
	}
	IsGizmoActive = IsUsing;
	if (BoneLayer < 0)
	{
		return;
	}
	if (IsScaling)
	{
		const Fvector* const Axes[2][3] = { { &BoneStart.i, &BoneStart.j, &BoneStart.k }, { &BonePivot.i, &BonePivot.j, &BonePivot.k } };
		for (u8 Axis = 0; ImGuizmo::GetUsingOperation() && Axis < 3; ++Axis)
		{
			const float Ratio = Axes[1][Axis]->magnitude() / std::max(Axes[0][Axis]->magnitude(), EPS_S);
			WriteParam(Core, Item, Block, Param::BoneScale, static_cast<u8>(BoneLayer), Axis, std::clamp(BoneSize[Axis] * Ratio, 0.01f, 100.0f));
		}
		return;
	}
	Fmatrix Inverse, Before, After, Delta, Turned;
	Inverse.invert(State->World);
	Before.mul_43(Inverse, BoneStart);
	After.mul_43(Inverse, BonePivot);
	for (Fmatrix* Frame : { &Before, &After })
	{
		Frame->i.normalize_safe();
		Frame->j.normalize_safe();
		Frame->k.normalize_safe();
	}
	if (BoneMode(Item, Block, static_cast<u8>(BoneLayer)) == RowMode::Add)
	{
		Fmatrix Start, Back, Relative;
		Start.setHPB(deg2rad(BoneTurn.x), deg2rad(BoneTurn.y), deg2rad(BoneTurn.z));
		Start.c = BoneShift;
		Back.invert(Before);
		Relative.mul_43(Back, After);
		Delta.mul_43(Start, Relative);
	}
	else
	{
		Fmatrix Rest = Fidentity, Back, Local43;
		Runtime->RestLocal(Bone, Rest);
		Back.invert(BoneParentModel);
		Local43.mul_43(Back, After);
		Local43.i.normalize_safe();
		Local43.j.normalize_safe();
		Local43.k.normalize_safe();
		Turned.invert(Rest);
		Delta.mul_43(Turned, Local43);
	}
	Fvector Angles;
	Delta.getHPB(Angles);
	for (u8 Axis = 0; ImGuizmo::GetUsingOperation() && Axis < 3; ++Axis)
	{
		const float Degrees = rad2deg(Angles[Axis]);
		if (ImGuizmo::GetUsingOperation() == ImGuizmo::TRANSLATE)
		{
			WriteParam(Core, Item, Block, Param::BonePosition, static_cast<u8>(BoneLayer), Axis, Delta.c[Axis]);
			continue;
		}
		const float Turn = Degrees + 360.0f * std::round((BoneTurn[Axis] - Degrees) / 360.0f);
		WriteParam(Core, Item, Block, Param::BoneRotation, static_cast<u8>(BoneLayer), Axis, Turn);
		for (s32 Layer : CurlLayers)
		{
			WriteParam(Core, Item, Block, Param::BoneRotation, static_cast<u8>(Layer), Axis, Turn);
		}
	}
}

void Editor::DrawGhosts(Player& Core)
{
	ImDrawList* List = Overlay;
	const float Frame = 1.0f / static_cast<float>(Core.Data.Header.FrameRate);
	const s32 Count = static_cast<s32>(Settings.OnionCount), Step = static_cast<s32>(Settings.OnionStep);
	const bool IsEditing = Drag != DragKind::None || IsGizmoActive || Undo.IsOpen();
	if (Settings.ShowOnion && (IsEditing || GhostTime != Core.Time || GhostSerial != Undo.Serial() || GhostSpans.size() != 2u * Count * std::min<size_t>(Selected.size(), 4u)))
	{
		GhostLines.clear();
		GhostSpans.clear();
		GhostTime = Core.Time;
		GhostSerial = Undo.Serial();
		for (u32 At = 0; At < std::min<size_t>(Selected.size(), 4u); ++At)
		{
			for (s32 Index = -Count; Index <= Count; ++Index)
			{
				if (Index)
				{
					Core.GhostBones(Selected[At], Core.Time + static_cast<float>(Index * Step) * Frame, GhostLines);
					GhostSpans.push_back(static_cast<u32>(GhostLines.size()) | (Index < 0 ? 0x80000000u : 0u) | (static_cast<u32>(std::abs(Index)) << 24));
				}
			}
		}
	}
	for (u32 Span = 0, From = 0; Settings.ShowOnion && Span < GhostSpans.size(); From = GhostSpans[Span++] & 0xFFFFFFu)
	{
		const u32 Code = GhostSpans[Span], To = Code & 0xFFFFFFu, Far = (Code >> 24) & 0x7Fu;
		const u32 Alpha = static_cast<u32>(220 - 150 * (Far - 1) / std::max(Count, 1));
		const u32 Color = (Code & 0x80000000u) ? IM_COL32(90, 140, 230, Alpha) : IM_COL32(235, 150, 60, Alpha);
		for (u32 Index = From; Index + 1 < To; Index += 2)
		{
			ViewportUi::Line3D(List, GhostLines[Index], GhostLines[Index + 1], Color);
		}
	}
	const float Lo = ScrollTime, Hi = ScrollTime + TrackWidth / Zoom, Pace = std::max(Frame, (Hi - Lo) / 600.0f);
	for (u32 At = 0; Settings.ShowPaths && At < Selected.size(); ++At)
	{
		const Object* Item = Find(Core.Data, Selected[At]);
		PathPoints.clear();
		for (float Moment = Lo; Item && Moment <= Hi; Moment += Pace)
		{
			Fvector Point;
			if (Core.PathPoint(Selected[At], Moment, Point))
			{
				PathPoints.push_back(Point);
			}
		}
		for (u32 Index = 0; Index < PathPoints.size(); ++Index)
		{
			ImVec2 Screen;
			if (Index)
			{
				ViewportUi::Line3D(List, PathPoints[Index - 1], PathPoints[Index], Item->Color);
			}
			if (ViewportUi::Project(PathPoints[Index], Screen) > 0.0f)
			{
				List->AddCircleFilled(Screen, 1.5f, Lighten(Item->Color));
			}
		}
	}
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
		const Lane* Track = State && Item.Type != ObjectType::Camera ? Curves::FindLane(Item.Lanes, Param::Position, 0) : nullptr;
		const float Start = Item.Span.Start, Length = Item.Span.Length, HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
		Fvector Last, Point;
		bool HasLast = false;
		for (float Local = 0.0f; Track && Local < Length + 2.0f * HalfFrame; Local += std::max(2.0f * HalfFrame, Length / 600.0f))
		{
			if (Core.PathPoint(Item.Id, Start + std::min(Local, Length), Point))
			{
				HasLast ? ViewportUi::Line3D(List, Last, Point, Tint) : (void)0;
				Last = Point;
				HasLast = true;
			}
		}
		for (u32 Axis = 0; Track && Axis < 3; ++Axis)
		{
			for (const Key& Entry : Track->Curves[Axis].Keys)
			{
				const bool IsRepeat = (Axis > 0 && Curves::KeyAt(Track->Curves[0], Entry.Time, HalfFrame) != u32(-1)) || (Axis > 1 && Curves::KeyAt(Track->Curves[1], Entry.Time, HalfFrame) != u32(-1));
				ImVec2 Centre;
				const float Scale = !IsRepeat && Core.PathPoint(Item.Id, Start + Entry.Time, Point) ? ViewportUi::Project(Point, Centre) : 0.0f;
				if (!(Scale > 0.0f))
				{
					continue;
				}
				const float Radius = std::max(3.0f, ViewportUi::KeyRadius * Scale);
				std::fabs(Start + Entry.Time - Core.Time) < HalfFrame ? List->AddCircleFilled(Centre, Radius, Tint) : List->AddCircle(Centre, Radius, Tint, 0, 1.5f);
				const Fvector Turn = IsParamShown(Item, Param::Rotation) && !Item.Attach.Parent.ObjectId ? Curves::LaneVector(Item.Lanes, Param::Rotation, Entry.Time, *PlacementOf(Item, 1)) : Fvector().set(0.0f, 0.0f, 0.0f);
				Fmatrix Facing;
				Facing.setHPB(deg2rad(Turn.x), deg2rad(Turn.y), deg2rad(Turn.z));
				IsParamShown(Item, Param::Rotation) && !Item.Attach.Parent.ObjectId ? ViewportUi::Line3D(List, Point, Fvector().mad(Point, Facing.k, ViewportUi::ArrowLength), Tint) : (void)0;
			}
		}
		const u32 Frames = Track ? static_cast<u32>(Length / (2.0f * HalfFrame)) : 0, Stride = Frames / 400 + 1;
		for (u32 Step = 0; Track && Step <= Frames; Step += Stride)
		{
			if (Core.PathPoint(Item.Id, Start + static_cast<float>(Step) * 2.0f * HalfFrame, Point) && ViewportUi::Project(Point, Centre) > 0.0f)
			{
				List->AddCircleFilled(Centre, 1.5f, Tint);
			}
		}
		if (Track && Core.Time >= Start && Core.Time <= Start + Length && Core.PathPoint(Item.Id, Core.Time, Point) && ViewportUi::Project(Point, Centre) > 0.0f)
		{
			List->AddCircleFilled(Centre, 4.0f, IM_COL32(255, 255, 255, 255));
			List->AddCircle(Centre, 4.0f, IM_COL32(0, 0, 0, 200));
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
			const u32 Segments = At + 1 == Count ? 0 : 16;
			Fvector Point = Item.Position, From, To;
			Fmatrix Turn, Step;
			Turn.setHPB(deg2rad(Item.Rotation.x), deg2rad(Item.Rotation.y), deg2rad(Item.Rotation.z));
			ViewportUi::Place(Cache->Frames, Point, Turn);
			From = Point;
			for (u32 Part = 1; Part <= Segments; ++Part)
			{
				Cameras::PathPose(Rig->Camera.Blocks[Index], _lerp(Item.Time, Cache->Keys[At + 1].Time, static_cast<float>(Part) / Segments), To, Step);
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
		if (ViewportUi::Project(Pose.Position, Spot) > 0.0f)
		{
			List->AddCircleFilled(Spot, 4.0f, IM_COL32(255, 255, 255, 255));
			List->AddCircle(Spot, 4.0f, IM_COL32(0, 0, 0, 200));
		}
	}
	const CameraBlock* Shot = Cache ? &Rig->Camera.Blocks[KeyBlock] : nullptr;
	const float Frame = 1.0f / static_cast<float>(Core.Data.Header.FrameRate);
	const u32 Frames = Shot && !Shot->Keys.empty() ? static_cast<u32>(Shot->Length / Frame) : 0, Stride = Frames / 400 + 1;
	for (u32 Step = 0; Shot && !Shot->Keys.empty() && Step <= Frames; Step += Stride)
	{
		Fvector Point;
		Fmatrix Turn;
		Cameras::PathPose(*Shot, Cameras::PathTime(*Shot, *Cache, static_cast<float>(Step) * Frame), Point, Turn);
		ViewportUi::Place(Cache->Frames, Point, Turn);
		if (ViewportUi::Project(Point, Spot) > 0.0f)
		{
			List->AddCircleFilled(Spot, 1.5f, Color);
		}
	}
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
