#include "StdAfx.h"
#include "CinemaEditor.h"
#include "../../Cinema/CinemaShake.h"

namespace Cinema::TimelineUi
{
struct PresetKey
{
	u8 Preset;
	float Time;
	float Share;
	Interp Mode;
};

constexpr const char* PresetNames[5] = { "Ease in-out", "Overshoot", "Bounce", "Step", "Wiggle" };
constexpr PresetKey PresetKeys[] = {
	{ 0, 0.0f, 0.0f, Interp::EaseInOut }, { 0, 1.0f, 1.0f, Interp::EaseInOut },
	{ 1, 0.0f, 0.0f, Interp::EaseOut }, { 1, 0.6f, 1.2f, Interp::EaseInOut }, { 1, 1.0f, 1.0f, Interp::EaseInOut },
	{ 2, 0.0f, 1.0f, Interp::EaseIn }, { 2, 0.4f, 0.0f, Interp::EaseOut }, { 2, 0.6f, 0.45f, Interp::EaseIn }, { 2, 0.8f, 0.0f, Interp::EaseOut }, { 2, 0.9f, 0.15f, Interp::EaseIn }, { 2, 1.0f, 0.0f, Interp::Linear },
	{ 3, 0.0f, 0.0f, Interp::Hold }, { 3, 1.0f / 3.0f, 1.0f / 3.0f, Interp::Hold }, { 3, 2.0f / 3.0f, 2.0f / 3.0f, Interp::Hold }, { 3, 1.0f, 1.0f, Interp::Hold }
};
constexpr float ScaleFactors[4] = { 0.5f, 0.75f, 1.5f, 2.0f };
constexpr const char* ScaleLabels[4] = { "x0.5", "x0.75", "x1.5", "x2" };

void ReverseCurve(Curve& Part)
{
	const size_t Count = Part.Keys.size();
	if (Count < 2)
	{
		return;
	}
	constexpr Interp Mirror[6] = { Interp::Linear, Interp::EaseOut, Interp::EaseIn, Interp::EaseInOut, Interp::Bezier, Interp::Hold };
	const float Sum = Part.Keys.front().Time + Part.Keys.back().Time;
	const Interp Last = Part.Keys.front().Mode;
	std::reverse(Part.Keys.begin(), Part.Keys.end());
	for (size_t Index = 0; Index < Count; ++Index)
	{
		Key& Item = Part.Keys[Index];
		Item.Time = Sum - Item.Time;
		std::swap(Item.InTime, Item.OutTime);
		std::swap(Item.InValue, Item.OutValue);
		Item.InTime = -Item.InTime;
		Item.OutTime = -Item.OutTime;
		Item.Mode = Index + 1 < Count ? Mirror[static_cast<u32>(Part.Keys[Index + 1].Mode)] : Last;
	}
}

void ScaleCurve(Curve& Part, float Factor)
{
	const float Pivot = Part.Keys.empty() ? 0.0f : Part.Keys.front().Time;
	for (Key& Item : Part.Keys)
	{
		Item.Time = Pivot + (Item.Time - Pivot) * Factor;
		Item.InTime *= Factor;
		Item.OutTime *= Factor;
	}
}

void SmoothCurve(Curve& Part, xr_vector<float>& Values)
{
	Values.resize(Part.Keys.size());
	for (size_t Index = 0; Index < Part.Keys.size(); ++Index)
	{
		Values[Index] = Part.Keys[Index].Value;
	}
	for (size_t Index = 1; Index + 1 < Part.Keys.size(); ++Index)
	{
		Part.Keys[Index].Value = 0.25f * Values[Index - 1] + 0.5f * Values[Index] + 0.25f * Values[Index + 1];
	}
}

void ReduceCurve(Curve& Part, float Tolerance, xr_vector<std::pair<u32, u32>>& Spans, xr_vector<u8>& Kept)
{
	const u32 Count = static_cast<u32>(Part.Keys.size());
	if (Count < 3)
	{
		return;
	}
	Kept.assign(Count, 0);
	Kept.front() = Kept.back() = 1;
	Spans.assign(1, { 0u, Count - 1 });
	while (!Spans.empty())
	{
		const auto [From, To] = Spans.back();
		Spans.pop_back();
		const Key &A = Part.Keys[From], &B = Part.Keys[To];
		float Worst = Tolerance;
		u32 Split = 0;
		for (u32 Index = From + 1; Index < To; ++Index)
		{
			const float Error = std::fabs(A.Value + (B.Value - A.Value) * (Part.Keys[Index].Time - A.Time) / std::max(B.Time - A.Time, 1e-6f) - Part.Keys[Index].Value);
			Split = Error > Worst ? Index : Split;
			Worst = std::max(Worst, Error);
		}
		if (Split)
		{
			Kept[Split] = 1;
			Spans.push_back({ From, Split });
			Spans.push_back({ Split, To });
		}
	}
	u32 Write = 0;
	for (u32 Index = 0; Index < Count; ++Index)
	{
		Part.Keys[Write] = Part.Keys[Index];
		Write += Kept[Index];
	}
	Part.Keys.resize(Write);
}

constexpr float Speeds[4] = { 0.25f, 0.5f, 1.0f, 2.0f };
constexpr const char* SpeedLabels[4] = { "0.25x", "0.5x", "1x", "2x" };
constexpr float SnapValues[7] = { 1.0f, 2.0f, 5.0f, 10.0f, 0.5f, 1.0f, 5.0f };
constexpr const char* SnapLabels[7] = { "1 f", "2 f", "5 f", "10 f", "1/2 s", "1 s", "5 s" };
constexpr u32 FrameTicks[4] = { 1, 2, 5, 10 };
constexpr u32 SecondTicks[10] = { 1, 2, 5, 10, 30, 60, 120, 300, 600, 1800 };
constexpr u32 Separator = IM_COL32(0, 0, 0, 140), Shade = IM_COL32(0, 0, 0, 90), Corner = IM_COL32(255, 255, 255, 170);
constexpr float EdgeZone = 6.0f, MagnetZone = 8.0f, CornerZone = 8.0f, KeyRadius = 4.5f, KeyZone = 6.0f, SegmentZone = 4.0f, SizeZone = 4.0f, PoseRadius = 5.0f, PoseZone = 7.0f, BraceHeight = 6.0f, OverviewHeight = 10.0f, TickRadius = 2.5f, OutZone = 4.0f, GripZone = 5.0f;
constexpr DragKind HitDrags[5] = { DragKind::LaneKeys, DragKind::HandleIn, DragKind::HandleOut, DragKind::LaneSegment, DragKind::LaneBend };
constexpr const char* ShotLabels[8] = { "Move clip", "Trim clip", "Trim clip", "Move clip", "Clip length", "Stretch clip", "Move pose key", "Copy clip" };
constexpr const char* ShakeLabels[5] = { "Move shake", "Resize shake", "Resize shake", "Fade in", "Fade out" };
constexpr const char* RowModes[2] = { "Add", "Override" };
constexpr DragKind BlockParts[8] = { DragKind::None, DragKind::BlockMove, DragKind::BlockIn, DragKind::BlockOut, DragKind::BlockFadeIn, DragKind::BlockFadeOut, DragKind::BlockBlend, DragKind::BlockMove };
constexpr DragKind ShotParts[8] = { DragKind::None, DragKind::BlockMove, DragKind::BlockIn, DragKind::BlockLength, DragKind::BlockMove, DragKind::BlockMove, DragKind::BlockBlend, DragKind::PoseKey };
constexpr DragKind ShakeParts[8] = { DragKind::ShakeMove, DragKind::ShakeMove, DragKind::ShakeStart, DragKind::ShakeEnd, DragKind::ShakeFadeIn, DragKind::ShakeFadeOut, DragKind::ShakeMove, DragKind::ShakeMove };
constexpr const char* PartHints[3][8] = {
	{ nullptr, "Drag: move, up or down to another row | Ctrl: copy | Shift: no snap", "Drag: trim the start | Ctrl: speed | Shift: no snap", "Drag: trim the end or loop | Shift: hold | Ctrl: speed", "Drag: fade in", "Drag: fade out", "Drag: slide the crossfade | Right-click: ease", nullptr },
	{ nullptr, "Drag: move | Ctrl: copy | Shift: no snap", "Drag: trim the start | Shift: no snap", "Drag: length | Ctrl: stretch | Shift: no snap", nullptr, nullptr, "Drag: slide the crossfade | Right-click: ease, remove", "Drag: retime the key | Right-click: key menu | Delete: remove" },
	{ nullptr, "Drag: move, up or down to another row | Ctrl: copy | Shift: no snap", "Drag: trim the start | Shift: no snap", "Drag: trim the end | Shift: no snap", "Drag: fade in", "Drag: fade out", "Drag: slide the crossfade", nullptr }
};
constexpr u32 AxisColors[3] = { Rgb(230, 90, 90), Rgb(90, 200, 90), Rgb(90, 140, 230) };
constexpr const char* AxisNames[2][3] = { { "X", "Y", "Z" }, { "R", "G", "B" } };
constexpr const char* WaveNames[11] = { "None", "Sine", "Triangle", "Square", "Sawtooth", "Noise stepped", "Noise smooth", "Noise fractal", "Noise Perlin", "Noise continuous", "Oscillator" };
constexpr const char* ModeNames[6] = { "Linear", "Ease in", "Ease out", "Ease in-out", "Bezier", "Hold" };
constexpr Param DefaultLanes[8] = { Param::Position, Param::Fov, Param::Intensity, Param::Speed, Param::DecalWidth, Param::SoundVolume, Param::WeatherTime, Param::PostFade };
const Curve EmptyCurve {};

struct KeyHit
{
	u32 Target = 0;
	u8 Component = 0;
	u32 Index = 0;
	u8 Handle = 0;
	float Score = FLT_MAX;
	bool HasHit = false;
	ImVec2 Point;
};

struct DropSpot
{
	u32 Parent = 0;
	s32 Row = -1;
	bool IsBefore = false;
	bool IsInto = false;
};

u32 WithAlpha(u32 Color, u32 Alpha)
{
	return (Color & 0x00FFFFFFu) | (Alpha << 24);
}

u8 LabelAxis(u8 Shown)
{
	return (Shown & 1) || !Shown ? 0 : (Shown & 2) ? 1 : 2;
}

float DragRange(Param Id, float Span, float Height)
{
	const ParamInfo& Meta = Info(Id);
	const float Rate = Id == Param::Position ? 0.01f : Id == Param::BonePosition ? 0.002f : Id == Param::Rotation || Id == Param::BoneRotation ? 0.25f : Id == Param::Scale || Id == Param::Size || Id == Param::BoneScale ? 0.005f : (Meta.Max - Meta.Min) / 200.0f;
	return std::min(Span, Height * Rate);
}

float Band(float RowHeight)
{
	return std::max(5.0f, std::round(0.3f * (RowHeight - 2.0f)));
}

struct PartBox
{
	float Begin = 0.0f;
	float End = 0.0f;
	float Top = 0.0f;
	float Bottom = 0.0f;
	float In = 0.0f;
	float Out = 0.0f;
	float Cross = 0.0f;
	bool IsLeftFree = true;
	bool IsRightFree = true;
	bool HasFades = false;
};

float GripIn(float Begin, float In)
{
	return std::max(In, Begin + EdgeZone + GripZone);
}

float GripOut(float End, float Out)
{
	return std::min(Out, End - EdgeZone - GripZone);
}

ClipPart PartAt(const PartBox& Box, const ImVec2& Mouse, ImRect& Area)
{
	const float Width = Box.End - Box.Begin, Zone = std::min(EdgeZone, 0.25f * Width), X = Mouse.x;
	if (X < Box.Begin - (Box.IsLeftFree ? OutZone : 0.0f) || X > Box.End + (Box.IsRightFree ? OutZone : 0.0f) || Mouse.y < Box.Top || Mouse.y > Box.Bottom)
	{
		return ClipPart::None;
	}
	if (X <= Box.Begin + Zone || X >= Box.End - Zone)
	{
		const bool IsStart = X <= Box.Begin + Zone;
		Area = IsStart ? ImRect(Box.Begin, Box.Top, Box.Begin + 2.0f, Box.Bottom) : ImRect(Box.End - 2.0f, Box.Top, Box.End, Box.Bottom);
		return IsStart ? ClipPart::Start : ClipPart::End;
	}
	const bool HasGrips = Box.HasFades && Width >= 4.0f * EdgeZone && Mouse.y - Box.Top <= CornerZone;
	const float In = GripIn(Box.Begin, Box.In), Out = GripOut(Box.End, Box.Out);
	if (HasGrips && (std::fabs(X - In) <= GripZone || std::fabs(X - Out) <= GripZone))
	{
		const bool IsIn = std::fabs(X - In) <= GripZone;
		const float At = IsIn ? In : Out;
		Area = ImRect(At - 3.5f, Box.Top, At + 3.5f, Box.Top + 7.0f);
		return IsIn ? ClipPart::FadeIn : ClipPart::FadeOut;
	}
	const bool IsCross = X < Box.Cross;
	Area = ImRect(Box.Begin, Box.Top, IsCross ? Box.Cross : Box.End, Box.Bottom);
	return IsCross ? ClipPart::Cross : ClipPart::Body;
}

void Grips(ImDrawList* List, const ImRect& Box, float In, float Out)
{
	if (Box.GetWidth() < 4.0f * EdgeZone)
	{
		return;
	}
	for (const float X : { GripIn(Box.Min.x, In), GripOut(Box.Max.x, Out) })
	{
		List->AddRectFilled(ImVec2(X - 3.5f, Box.Min.y), ImVec2(X + 3.5f, Box.Min.y + 7.0f), Corner);
	}
}

Cover Covers(float A, float B, float C, float D, float Min)
{
	const float Slack = 1e-4f;
	if (D <= A + Slack || C >= B - Slack)
	{
		return Cover::Keep;
	}
	const bool HasHead = A - C >= Min, HasTail = D - B >= Min;
	if (C < A - Slack && D >= B - Slack)
	{
		return HasHead && HasTail ? Cover::Split : HasHead ? Cover::CutTail : HasTail ? Cover::CutHead : Cover::Remove;
	}
	if (C < A - Slack)
	{
		return Cover::Keep;
	}
	return D <= B + Slack ? Cover::Remove : C <= A + Slack ? (HasTail ? Cover::CutHead : Cover::Remove) : Cover::Keep;
}

void PlanOverwrite(const xr_vector<Cameras::ClipSpan>& Spans, float A, float B, float Min, xr_vector<u8>& Plan)
{
	Plan.assign(Spans.size(), static_cast<u8>(Cover::Keep));
	s32 Before = -1, After = -1;
	for (u32 Index = 0; Index < Spans.size(); ++Index)
	{
		const float C = Spans[Index].Start, D = C + Spans[Index].Length;
		const Cover Action = Spans[Index].Length > 0.0f ? Covers(A, B, C, D, Min) : Cover::Keep;
		Plan[Index] = static_cast<u8>(Action);
		if (Action != Cover::Keep || !(Spans[Index].Length > 0.0f) || D <= A + 1e-4f || C >= B - 1e-4f)
		{
			continue;
		}
		const bool IsEarly = C < A;
		s32& Slot = IsEarly ? Before : After;
		const bool IsNearer = Slot < 0 || (IsEarly ? C > Spans[Slot].Start : C < Spans[Slot].Start);
		if (Slot >= 0)
		{
			Plan[IsNearer ? Slot : Index] = static_cast<u8>(IsEarly ? Cover::CutTail : Cover::CutHead);
		}
		Slot = IsNearer ? static_cast<s32>(Index) : Slot;
	}
	if (Before >= 0 && After >= 0 && Spans[Before].Start + Spans[Before].Length > Spans[After].Start + 1e-4f)
	{
		Plan[Before] = static_cast<u8>(Cover::CutTail);
	}
}

u32 TailOf(const ActorBlock& Block, float Length, float Cut, float Frame, ActorBlock* Parts, float* Offsets)
{
	float In = 0.0f, Out = 0.0f;
	const float Play = Actors::Measure(Block, Length, In, Out);
	const float Total = Play * std::clamp(Block.Loops, 1.0f, MaxLoops), Width = Total + std::max(Block.Hold, 0.0f);
	if (!(Play > 0.0f) || !(Cut < Width))
	{
		return 0;
	}
	const bool IsForward = Block.Speed >= 0.0f, IsPlaying = Cut < Total;
	const float From = IsForward ? In : Out, To = IsForward ? Out : In;
	const auto Moment = [From, To, Play](float Offset) { return From + (To - From) * Offset / Play; };
	const float Phase = IsPlaying ? Cut - Play * std::floor(Cut / Play) : std::max(Play - Frame, 0.0f), Left = Total - Cut;
	const bool IsSplit = IsPlaying && Left > Play - Phase + 0.5f * Frame;
	ActorBlock& First = Parts[0] = Block;
	Offsets[0] = Cut;
	First.FadeIn = 0.0f;
	First.Loops = 1.0f;
	First.Hold = IsSplit ? 0.0f : IsPlaying ? Block.Hold : std::max(Width - Cut - (Play - Phase), 0.0f);
	(IsForward ? First.In : First.Out) = Moment(Phase);
	(IsForward ? First.Out : First.In) = IsPlaying && !IsSplit ? Moment(std::min(Phase + Left, Play)) : (IsForward ? First.Out : First.In);
	First.FadeOut = IsSplit ? 0.0f : First.FadeOut;
	if (!IsSplit)
	{
		return 1;
	}
	const float Rest = (Left - Play + Phase) / Play;
	ActorBlock& Second = Parts[1] = Block;
	Offsets[1] = Cut + Play - Phase;
	Second.FadeIn = 0.0f;
	Second.Loops = std::max(Rest, 1.0f);
	(IsForward ? Second.Out : Second.In) = Rest < 1.0f ? Moment(Rest * Play) : (IsForward ? Second.Out : Second.In);
	return 2;
}

void Label(ImDrawList* List, const ImRect& Box, u32 Color, const char* Text, float Size = 0.0f)
{
	const ImVec4 Bounds(Box.Min.x, Box.Min.y, Box.Max.x - 2.0f, Box.Max.y);
	const ImVec2 At(std::max(Box.Min.x, List->GetClipRectMin().x) + 3.0f, 0.5f * (Box.Min.y + Box.Max.y - (Size > 0.0f ? Size : ImGui::GetFontSize())));
	if (!Text || Bounds.z <= At.x + 1.0f)
	{
		return;
	}
	if (Size > 0.0f)
	{
		List->AddText(nullptr, Size, At, Color, Text, nullptr, 0.0f, &Bounds);
		return;
	}
	ImGui::PushStyleColor(ImGuiCol_Text, Color);
	ImGui::RenderTextEllipsis(List, At, ImVec2(Bounds.z, Bounds.w), Bounds.z, Text, nullptr, nullptr);
	ImGui::PopStyleColor();
}

bool OpenCombo(const char* Name, const char* Preview)
{
	ImDrawList* List = ImGui::GetWindowDrawList();
	const ImVec2 At = ImGui::GetCursorScreenPos();
	const float Height = ImGui::GetFrameHeight(), Right = At.x + ImGui::CalcItemWidth() - Height;
	const bool IsOpen = ImGui::BeginCombo(Name, "");
	Label(List, ImRect(At.x + ImGui::GetStyle().FramePadding.x - 3.0f, At.y, Right, At.y + Height), ImGui::GetColorU32(ImGuiCol_Text), Preview);
	return IsOpen;
}

void Icon(ImDrawList* List, const ImVec2& At, u32 Kind, u32 Color)
{
	if (Kind == static_cast<u32>(ObjectType::Actor))
	{
		List->AddCircleFilled(ImVec2(At.x, At.y - 4.5f), 2.5f, Color);
		List->AddLine(ImVec2(At.x, At.y - 2.0f), ImVec2(At.x, At.y + 2.0f), Color, 1.5f);
		List->AddLine(ImVec2(At.x - 4.0f, At.y - 1.0f), ImVec2(At.x + 4.0f, At.y - 1.0f), Color, 1.5f);
		List->AddLine(ImVec2(At.x - 3.0f, At.y + 6.0f), ImVec2(At.x, At.y + 2.0f), Color, 1.5f);
		List->AddLine(ImVec2(At.x, At.y + 2.0f), ImVec2(At.x + 3.0f, At.y + 6.0f), Color, 1.5f);
		return;
	}
	if (Kind == static_cast<u32>(ObjectType::Camera))
	{
		List->AddRectFilled(ImVec2(At.x - 6.0f, At.y - 3.0f), ImVec2(At.x + 2.0f, At.y + 4.0f), Color);
		List->AddTriangleFilled(ImVec2(At.x + 2.0f, At.y + 0.5f), ImVec2(At.x + 6.0f, At.y - 3.0f), ImVec2(At.x + 6.0f, At.y + 4.0f), Color);
		return;
	}
	if (Kind == static_cast<u32>(ObjectType::Light))
	{
		List->AddCircleFilled(At, 3.0f, Color);
		for (const float X : { -1.0f, 1.0f })
		{
			for (const float Y : { -1.0f, 1.0f })
			{
				List->AddLine(ImVec2(At.x + 3.5f * X, At.y + 3.5f * Y), ImVec2(At.x + 5.0f * X, At.y + 5.0f * Y), Color, 1.5f);
			}
		}
		return;
	}
	if (Kind == static_cast<u32>(ObjectType::Decal))
	{
		List->AddRect(ImVec2(At.x - 5.0f, At.y - 5.0f), ImVec2(At.x + 5.0f, At.y + 5.0f), Color, 0.0f, 0, 1.5f);
		List->AddCircleFilled(At, 2.0f, Color);
		return;
	}
	if (Kind == static_cast<u32>(ObjectType::Sound))
	{
		List->AddRectFilled(ImVec2(At.x - 6.0f, At.y - 2.0f), ImVec2(At.x - 3.0f, At.y + 2.0f), Color);
		List->AddTriangleFilled(ImVec2(At.x - 3.0f, At.y), ImVec2(At.x + 1.0f, At.y - 5.0f), ImVec2(At.x + 1.0f, At.y + 5.0f), Color);
		List->AddLine(ImVec2(At.x + 4.0f, At.y - 3.0f), ImVec2(At.x + 4.0f, At.y + 3.0f), Color, 1.5f);
		return;
	}
	if (Kind == static_cast<u32>(ObjectType::Post))
	{
		List->AddRect(ImVec2(At.x - 6.0f, At.y - 5.0f), ImVec2(At.x + 6.0f, At.y + 5.0f), Color, 0.0f, 0, 1.5f);
		List->AddTriangleFilled(ImVec2(At.x - 6.0f, At.y + 5.0f), ImVec2(At.x + 6.0f, At.y - 5.0f), ImVec2(At.x + 6.0f, At.y + 5.0f), Color);
		return;
	}
	if (Kind == static_cast<u32>(ObjectType::Weather))
	{
		List->AddCircle(ImVec2(At.x + 1.0f, At.y - 1.0f), 4.0f, Color, 0, 1.5f);
		List->AddLine(ImVec2(At.x - 6.0f, At.y + 5.0f), ImVec2(At.x + 6.0f, At.y + 5.0f), Color, 1.5f);
		return;
	}
	if (Kind == static_cast<u32>(ObjectType::Particle))
	{
		for (const ImVec2& Dot : { ImVec2(-4.0f, 3.0f), ImVec2(4.0f, 3.0f), ImVec2(0.0f, -4.0f) })
		{
			List->AddCircleFilled(ImVec2(At.x + Dot.x, At.y + Dot.y), 1.5f, Color);
		}
		return;
	}
	List->AddRectFilled(ImVec2(At.x - 6.0f, At.y - 5.0f), ImVec2(At.x - 1.0f, At.y - 3.0f), Color);
	List->AddRectFilled(ImVec2(At.x - 6.0f, At.y - 3.0f), ImVec2(At.x + 6.0f, At.y + 5.0f), Color);
}

void Hatch(ImDrawList* List, const ImRect& Box, u32 Color)
{
	ImRect Visible = Box;
	Visible.ClipWith(ImRect(List->GetClipRectMin(), List->GetClipRectMax()));
	if (Visible.GetWidth() <= 0.0f)
	{
		return;
	}
	const float Height = Box.GetHeight();
	List->PushClipRect(Visible.Min, Visible.Max, true);
	for (float X = Visible.Min.x - Height - std::fmod(Visible.Min.x - Box.Min.x, 6.0f); X < Visible.Max.x; X += 6.0f)
	{
		List->AddLine(ImVec2(X, Box.Max.y), ImVec2(X + Height, Box.Min.y), Color);
	}
	List->PopClipRect();
}

void Dotted(ImDrawList* List, float From, float To, float Y, u32 Color)
{
	const float Lo = std::max(From, List->GetClipRectMin().x - 6.0f), Hi = std::min(To, List->GetClipRectMax().x);
	for (float X = Lo - std::fmod(Lo - From, 6.0f); X < Hi; X += 6.0f)
	{
		List->AddLine(ImVec2(X, Y), ImVec2(std::min(X + 3.0f, To), Y), Color);
	}
}

void Boundary(ImDrawList* List, float X, const ImRect& Row)
{
	List->AddLine(ImVec2(X, Row.Min.y), ImVec2(X, Row.Max.y), Separator);
}

void CutMark(ImDrawList* List, float X, float Top)
{
	List->AddLine(ImVec2(X - 3.0f, Top + 1.0f), ImVec2(X + 3.0f, Top + 6.0f), Corner, 1.5f);
	List->AddLine(ImVec2(X + 3.0f, Top + 1.0f), ImVec2(X - 3.0f, Top + 6.0f), Corner, 1.5f);
	List->AddCircle(ImVec2(X - 3.5f, Top + 7.5f), 1.5f, Corner);
	List->AddCircle(ImVec2(X + 3.5f, Top + 7.5f), 1.5f, Corner);
}

void Missing(ImDrawList* List, float X, const ImRect& Row)
{
	List->AddRectFilled(ImVec2(X - 1.5f, Row.Min.y), ImVec2(X + 1.5f, Row.Max.y), ImGui::GetColorU32(ErrorColor));
}

float Fallback(const Object& Target, s32 Block, Param Id, u8 Layer, u8 Component)
{
	const float* Field = StaticField(const_cast<Object&>(Target), Block, Id, Layer, Component);
	return !Field ? 0.0f : Id == Param::Fov && !(*Field > 0.0f) ? GameFov() : *Field;
}

bool HasField(const Object& Target, s32 Block, Param Id, u8 Layer)
{
	return StaticField(const_cast<Object&>(Target), Block, Id, Layer, 0) != nullptr;
}

u16 LaneWhere(const Object& Target)
{
	return Target.Type == ObjectType::Camera ? WhereCameraBlock | WhereShakeClip : ObjectWhere[static_cast<u32>(Target.Type)];
}

float Unwrap(float Raw, float Near)
{
	return Raw + 360.0f * std::round((Near - Raw) / 360.0f);
}

u8 Nearest(const float* Values, u8 From, u8 To, float Value)
{
	u8 Best = From;
	for (u8 Axis = From; Axis < To; ++Axis)
	{
		Best = std::fabs(Values[Axis] - Value) < std::fabs(Values[Best] - Value) ? Axis : Best;
	}
	return Best;
}

void LinkLabel(string1024& Out, const Scene& Data, const char* Name, const Attachment& Links)
{
	const auto Part = [&Data](string512& Text, const char* Arrow, const AttachLink& Link)
	{
		Text[0] = 0;
		if (Link.ObjectId)
		{
			xr_sprintf(Text, " %s %s%s%s", Arrow, DisplayName(Find(Data, Link.ObjectId)), Link.Bone.size() ? "." : "", Link.Bone.size() ? Link.Bone.c_str() : "");
		}
	};
	string512 Parent, Look;
	Part(Parent, "->", Links.Parent);
	Part(Look, "@", Links.LookAt);
	xr_sprintf(Out, "%s%s%s", Name, Parent, Look);
}

bool IsWarned(const Player& Core, const Object& Target)
{
	const ObjectState* State = Core.FindState(Target.Id);
	bool IsBad = State && State->Frames.Problems;
	for (u32 Index = 0; !IsBad && Index < Target.Camera.Blocks.size(); ++Index)
	{
		const BlockCache* Cache = Core.FindCameraBlock(Index);
		IsBad = Cache && Cache->Frames.Problems;
	}
	return IsBad;
}

u8 ShakeLayers(const Object& Target)
{
	return Target.Type == ObjectType::Actor ? static_cast<u8>(Target.Actor.Layers.size()) : ShakeRows(Target.Camera);
}

s32 FindTarget(const xr_vector<LaneTarget>& Targets, const Object& Owner, const LaneSlot& Slot, float Time, bool IsFallback)
{
	s32 First = -1, Found = -1;
	for (u32 Index = 0; Index < Targets.size(); ++Index)
	{
		const LaneTarget& Item = Targets[Index];
		if (!HasField(Owner, Item.Block, Slot.Id, Slot.Layer))
		{
			continue;
		}
		if (Time >= Item.Start && Time <= Item.Start + Item.Length && (Found < 0 || (Item.Block >= ShakeBase && Item.Start >= Targets[Found].Start)))
		{
			Found = static_cast<s32>(Index);
		}
		First = First < 0 ? static_cast<s32>(Index) : First;
	}
	return Found >= 0 ? Found : IsFallback ? First : -1;

}

template <typename TKey>
void CutKeys(xr_vector<TKey>& Head, xr_vector<TKey>& Tail, TKey Middle, TKey Rest)
{
	const auto ModeAt = [](const xr_vector<TKey>& Keys, float Moment)
	{
		const auto After = std::upper_bound(Keys.begin(), Keys.end(), Moment, [](float Point, const TKey& Item) { return Point < Item.Time; });
		return After == Keys.begin() ? After->Mode : (After - 1)->Mode;
	};
	Middle.Mode = ModeAt(Head, Middle.Time);
	Rest.Mode = ModeAt(Tail, Rest.Time);
	const size_t HeadCount = Head.size(), TailCount = Tail.size();
	std::erase_if(Head, [&Middle](const TKey& Item) { return Item.Time >= Middle.Time; });
	std::erase_if(Tail, [&Rest](const TKey& Item) { return Item.Time <= Rest.Time; });
	if (Head.size() < HeadCount)
	{
		Head.push_back(Middle);
	}
	if (Tail.size() < TailCount)
	{
		Tail.insert(Tail.begin(), Rest);
	}
	for (TKey& Item : Tail)
	{
		Item.Time -= Rest.Time;
	}
}

bool ShiftLanes(xr_vector<Lane>& Lanes, float At, float Amount, float HalfFrame, bool ShouldApply)
{
	const float Until = At - std::min(Amount, 0.0f);
	const auto Before = [](const Key& Item, float Moment) { return Item.Time < Moment; };
	bool IsValid = true;
	for (Lane& Track : Lanes)
	{
		for (Curve& Part : Track.Curves)
		{
			xr_vector<Key>& Keys = Part.Keys;
			IsValid = IsValid && (Keys.empty() || (Keys.size() + 2 <= MaxLaneKeys && Keys.back().Time + Amount <= MaxTime));
			if (Keys.empty() || !ShouldApply)
			{
				continue;
			}
			const auto Last = std::lower_bound(Keys.begin(), Keys.end(), Amount > 0.0f ? At + HalfFrame : Until, Before);
			const auto First = Amount > 0.0f ? Last : std::lower_bound(Keys.begin(), Last, At, Before);
			const bool IsJoined = Last != Keys.begin() && (Last == Keys.end() ? First != Last : Amount > 0.0f || Last->Time > Until + HalfFrame);
			Key Join = !IsJoined ? Key {} : Amount > 0.0f && (Last - 1)->Time >= At - HalfFrame ? *(Last - 1) : Key { At, Curves::Value(Part, Until, 0.0f), (Last - 1)->Mode };
			for (auto Item = Keys.erase(First, Last); Item != Keys.end(); ++Item)
			{
				Item->Time += Amount;
			}
			if (!IsJoined)
			{
				continue;
			}
			if (Amount > 0.0f)
			{
				Key Paused = Join;
				Paused.Mode = Interp::Hold;
				Curves::PlaceKey(Part, Paused, HalfFrame);
				Join.Time += Amount;
			}
			Curves::PlaceKey(Part, Join, HalfFrame);
		}
	}
	return IsValid;
}

Curve* FindCurve(Scene& Data, const KeyRef& Ref)
{
	Object* Owner = Find(Data, Ref.ObjectId);
	const Lane* Found = Owner && HasOwner(*Owner, Ref.Block) ? Curves::FindLane(LaneList(*Owner, Ref.Block), Ref.Id, Ref.Layer) : nullptr;
	return Found ? const_cast<Curve*>(&Found->Curves[Ref.Component]) : nullptr;
}

Key* FindKey(Scene& Data, const KeyRef& Ref)
{
	Curve* Part = FindCurve(Data, Ref);
	return Part && Ref.Index < Part->Keys.size() ? &Part->Keys[Ref.Index] : nullptr;
}

void SlotLabel(string64& Out, Param Id, u8 Layer, const Object* Target = nullptr, s32 Block = -1)
{
	if (Id == Param::PoseWeight)
	{
		xr_sprintf(Out, "Pose %u weight", Layer + 1u);
		return;
	}
	if (IsBoneParam(Id))
	{
		const PoseBone* Joint = Target && HasOwner(*Target, Block) ? BoneEntry(const_cast<Object&>(*Target), Block, Layer) : nullptr;
		xr_sprintf(Out, "%s %s", Joint ? Joint->Name.c_str() : "Bone", Info(Id).Label);
		return;
	}
	if (Id == Param::LayerWeight)
	{
		xr_sprintf(Out, "Animation %u weight", Layer + 1u);
		return;
	}
	if (Id == Param::ConstraintWeight)
	{
		const bool IsKnown = Target && Layer < Target->Actor.Constraints.size();
		xr_sprintf(Out, "%s constraint weight", IsKnown ? Target->Actor.Constraints[Layer].Bone.c_str() : "Bone");
		return;
	}
	if (Id >= Param::IkTarget && Id <= Param::IkWeight)
	{
		xr_sprintf(Out, "%s %s", Layer < RigChainCount ? RigChainKeys[Layer] : "Limb", Info(Id).Label);
		return;
	}
	if (Id >= Param::ShakeInfluence)
	{
		xr_sprintf(Out, "Shakes %u %s", Layer + 1u, Info(Id).Label);
		return;
	}
	xr_sprintf(Out, "%s", Info(Id).Label);
}

void Test(KeyHit& Hit, const ImVec2& Mouse, const ImVec2& Point, u32 Target, u8 Component, u32 Index, u8 Kind)
{
	const float Distance = std::max(std::fabs(Mouse.x - Point.x), std::fabs(Mouse.y - Point.y));
	const float Score = Kind ? Distance - KeyZone : Distance <= KeyRadius ? -KeyZone - 1.0f : Distance;
	if (Distance <= KeyZone && Score < Hit.Score)
	{
		Hit = { Target, Component, Index, Kind, Score, true, Point };
	}
}

template <typename TVisit>
void WalkMoves(const xr_vector<LaneDrag>& Drags, const xr_vector<KeyMove>& Moves, const TVisit& Visit)
{
	u32 Move = 0;
	for (u32 Index = 0; Index < Drags.size(); ++Index)
	{
		for (u32 At = 0; At < Drags[Index].Base.Keys.size(); ++At)
		{
			const bool IsMoved = Move < Moves.size() && Moves[Move].Source == Index && Moves[Move].Index == At;
			Move += IsMoved ? 1 : 0;
			Visit(Drags[Index], At, IsMoved);
		}
	}
}

template <typename TFunc>
void Plot(ImDrawList* List, float Begin, float End, const TFunc& YAt, u32 Color, float Thickness)
{
	for (float X = Begin; End > Begin && X < End + 2.0f; X += 2.0f)
	{
		List->PathLineTo(ImVec2(std::min(X, End), YAt(std::min(X, End))));
	}
	List->PathStroke(Color, ImDrawFlags_None, Thickness);
}

void DrawHandle(ImDrawList* List, KeyHit& Hit, const ImVec2& Mouse, const ImVec2& Point, const ImVec2& Tip, u32 Target, u8 Component, u32 Index, u8 Kind)
{
	if (std::fabs(Tip.x - Point.x) + std::fabs(Tip.y - Point.y) < 3.0f)
	{
		return;
	}
	const u32 Color = ImGui::GetColorU32(ImGuiCol_Text);
	List->AddLine(Point, Tip, Color);
	List->AddCircleFilled(Tip, 3.0f, Color);
	Test(Hit, Mouse, Tip, Target, Component, Index, Kind);
}

void RangeLabels(ImDrawList* List, const ImRect& Rect, float Lo, float Hi, float Top, float Bottom)
{
	const u32 Color = ImGui::GetColorU32(ImGuiCol_TextDisabled);
	if (Lo < 0.0f && Hi > 0.0f)
	{
		const float Y = Bottom + Lo / (Hi - Lo) * (Bottom - Top);
		List->AddLine(ImVec2(Rect.Min.x, Y), ImVec2(Rect.Max.x, Y), WithAlpha(Color, 60u));
	}
	string64 Text;
	const bool IsShort = Rect.GetHeight() < 2.0f * ImGui::GetFontSize();
	xr_sprintf(Text, IsShort ? "%.3g .. %.3g" : "%.3g", IsShort ? Lo : Hi, Hi);
	List->AddText(ImVec2(Rect.Min.x + 2.0f, Rect.Min.y), Color, Text);
	if (!IsShort)
	{
		xr_sprintf(Text, "%.3g", Lo);
		List->AddText(ImVec2(Rect.Min.x + 2.0f, Rect.Max.y - ImGui::GetFontSize()), Color, Text);
	}
}

void EndTag(ImDrawList* List, const ImRect& Rect, ImVec2 Point, float Time, float Value, u32 Rate, u32 Color)
{
	string32 Code;
	string64 Text;
	Timecode(Code, Time, Rate);
	xr_sprintf(Text, "%s | %.3g", Code, Value);
	const ImVec2 Size = ImGui::CalcTextSize(Text);
	Point.x = std::clamp(Point.x + 4.0f, Rect.Min.x, std::max(Rect.Max.x - Size.x, Rect.Min.x));
	Point.y = std::clamp(Point.y - Size.y - 2.0f, Rect.Min.y, Rect.Max.y - Size.y);
	List->AddText(Point, Color, Text);
}

ImVec2 TangentOf(const Curve& Part, u32 At, bool IsOut, float Static)
{
	const Key& Head = Part.Keys[IsOut ? At : At - 1];
	const Key& Item = Part.Keys[At];
	const float Width = Part.Keys[IsOut ? At + 1 : At].Time - Head.Time, Sign = IsOut ? 1.0f : -1.0f, Step = 0.01f * Width;
	if (Head.Mode == Interp::Bezier)
	{
		return IsOut ? ImVec2(std::clamp(Item.OutTime, 0.0f, Width), Item.OutValue) : ImVec2(std::clamp(Item.InTime, -Width, 0.0f), Item.InValue);
	}
	const float Slope = Head.Mode == Interp::Hold || !(Step > 0.0f) ? 0.0f : (Curves::Value(Part, Item.Time + Sign * Step, Static) - Item.Value) / Step;
	return ImVec2(Sign * Width / 3.0f, Slope * Width / 3.0f);
}

template <typename TFunc>
bool PickParam(const char* Name, const char* Preview, u16 Allowed, u8 Layers, const TFunc& IsFree, Param& Picked, u8& PickedLayer)
{
	constexpr u32 Shakes = static_cast<u32>(Param::ShakeInfluence);
	bool IsPicked = false;
	const auto Offer = [&](Param Id, u8 Layer)
	{
		string64 Label;
		SlotLabel(Label, Id, Layer);
		if ((Info(Id).Where & Allowed) && IsFree(Id, Layer) && ImGui::Selectable(Label))
		{
			Picked = Id;
			PickedLayer = Layer;
			IsPicked = true;
		}
	};
	if (OpenCombo(Name, Preview))
	{
		for (u32 Entry = 0; Entry < Shakes; ++Entry)
		{
			const bool IsWeight = Entry == static_cast<u32>(Param::LayerWeight);
			for (u32 Layer = 0; Layer <= (IsWeight ? Layers : 0u); ++Layer)
			{
				Offer(static_cast<Param>(Entry), static_cast<u8>(Layer));
			}
		}
		for (u32 Entry = 0; Entry < 3u * Layers; ++Entry)
		{
			Offer(static_cast<Param>(Shakes + Entry % 3), static_cast<u8>(Entry / 3));
		}
		ImGui::EndCombo();
	}
	return IsPicked;
}

void Siblings(const Scene& Data, u32 FolderId, xr_vector<RowItem>& Out)
{
	const xr_vector<Folder>& Folders = Data.Header.Folders;
	const auto HasFolder = [&Folders](u32 Id) { return std::any_of(Folders.begin(), Folders.end(), [Id](const Folder& Item) { return Item.Id == Id; }); };
	Out.clear();
	for (const Folder& Item : Folders)
	{
		if (!FolderId)
		{
			Out.push_back({ Item.Id, true, 0, Item.Row });
		}
	}
	for (const Object& Item : Data.Objects)
	{
		if (FolderId ? Item.FolderId == FolderId : !Item.FolderId || !HasFolder(Item.FolderId))
		{
			Out.push_back({ Item.Id, false, static_cast<u8>(FolderId ? 1 : 0), Item.Row });
		}
	}
	std::sort(Out.begin(), Out.end(), [](const RowItem& A, const RowItem& B) { return std::make_tuple(A.Order, !A.IsFolder, A.Id) < std::make_tuple(B.Order, !B.IsFolder, B.Id); });
}

DropSpot Drop(const xr_vector<RowItem>& Rows, float Top, float MouseY, bool IsFolder)
{
	DropSpot Spot;
	const float Local = MouseY - Top;
	const s32 Index = static_cast<s32>(std::upper_bound(Rows.begin(), Rows.end(), Local, [](float Y, const RowItem& Item) { return Y < Item.Y; }) - Rows.begin()) - 1;
	if (Index < 0 || Local >= Rows[Index].Y + Rows[Index].Height)
	{
		Spot.Row = Index < 0 && !Rows.empty() ? 0 : -1;
		Spot.IsBefore = Index < 0;
		return Spot;
	}
	const float Fraction = (Local - Rows[Index].Y) / Rows[Index].Height;
	Spot.Row = Index;
	Spot.IsBefore = Fraction < 0.5f;
	if (Rows[Index].IsFolder && !IsFolder && Fraction > 0.25f && Fraction < 0.75f)
	{
		Spot.Parent = Rows[Index].Id;
		Spot.IsInto = true;
		return Spot;
	}
	s32 Head = Index;
	while (Rows[Head].Depth)
	{
		--Head;
	}
	if (Head != Index && IsFolder)
	{
		Spot.Row = Head;
		Spot.IsBefore = false;
	}
	else if (Head != Index)
	{
		Spot.Parent = Rows[Head].Id;
	}
	return Spot;
}
}

namespace Cinema
{
void Editor::DrawTimeline(Player& Core, bool& IsOpen)
{
	const ImGuiViewport* View = ImGui::GetMainViewport();
	const bool IsAnimation = ShowAnimator;
	const float Share = IsAnimation ? 0.4f : 0.28f;
	ImGui::SetNextWindowPos(ImVec2(View->Pos.x, View->Pos.y + View->Size.y * (1.0f - Share)), LayoutCond());
	ImGui::SetNextWindowSize(ImVec2(View->Size.x, View->Size.y * Share), LayoutCond());
	if (LayoutReset)
	{
		ImGui::SetNextWindowDockID(0, ImGuiCond_Always);
	}
	string256 Title;
	xr_sprintf(Title, "%.128s%s###Cinema%s", SceneName.empty() ? "Untitled" : SceneName.c_str(), Core.HasUnsavedChanges ? " *" : "", IsAnimation ? "Animation" : "Timeline");
	const bool IsVisible = ImGui::Begin(Title, &IsOpen, ImGuiWindowFlags_MenuBar | ImGuiWindowFlags_NoNavInputs);
	IsOwnWindowFocused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	IsAnimFocused = false;
	MainBox = ImRect(ImGui::GetWindowPos(), ImVec2(ImGui::GetWindowPos().x + ImGui::GetWindowSize().x, ImGui::GetWindowPos().y + ImGui::GetWindowSize().y));
	HoverPose = HoverMarker = -1;
	HoverLaneObject = 0;
	const float Step = TimelineUi::SnapValues[Settings.SnapStep];
	SnapGrid = Settings.SnapStep < 4 ? Step / static_cast<float>(Core.Data.Header.FrameRate) : Step;
	if (IsVisible && IsAnimation)
	{
		DrawMenuBar(Core);
		DrawTransport(Core);
		DrawAnimatorBody(Core);
	}
	else if (IsVisible)
	{
		DrawMenuBar(Core);
		DrawTransport(Core);
		RowHeight = ImGui::GetFrameHeight() + 2.0f;
		const ImVec2 Origin = ImGui::GetCursorScreenPos();
		TrackLeft = Origin.x + Settings.HeaderWidth;
		TrackWidth = std::max(ImGui::GetContentRegionAvail().x - Settings.HeaderWidth, 1.0f);
		ImGui::SetCursorScreenPos(ImVec2(TrackLeft, Origin.y));
		if (Core.IsPlaying && Settings.IsFollowing && !IsFollowPaused && (TimeToX(Core.Time) > TrackLeft + 0.9f * TrackWidth || TimeToX(Core.Time) < TrackLeft))
		{
			ScrollTime = std::clamp(Core.Time - 0.1f * TrackWidth / Zoom, 0.0f, MaxTime);
		}
		DrawRuler(Core, ImRect(TrackLeft, Origin.y, TrackLeft + TrackWidth, Origin.y + TimelineUi::OverviewHeight + TimelineUi::BraceHeight + std::max(RowHeight, ImGui::GetFontSize() + std::round(0.8f * ImGui::GetFontSize()) + 2.0f)));
		ImGui::BeginChild("##Rows", ImVec2(0.0f, -ImGui::GetTextLineHeightWithSpacing()), ImGuiChildFlags_None, ImGuiWindowFlags_NoScrollWithMouse);
		const ImGuiIO& Input = ImGui::GetIO();
		if (ImGui::IsWindowHovered() && Input.MouseWheel != 0.0f)
		{
			const float Pivot = XToTime(Input.MousePos.x);
			const bool IsOverTracks = Input.MousePos.x >= TrackLeft;
			IsFollowPaused = IsFollowPaused || (Core.IsPlaying && !Input.KeyShift && IsOverTracks);
			if (Input.KeyCtrl && !IsOverTracks)
			{
				for (const LaneSlot& Slot : Slots)
				{
					const float Rows = std::clamp(std::round(SizeOf(Core.Data, Slot.ObjectId, SizeLane, Slot.Id, Slot.Layer) / RowHeight + Input.MouseWheel), 1.0f, MaxLaneSize);
					std::erase_if(Core.Data.Sizes, [&Slot](const RowSize& Item) { return Item.ObjectId == Slot.ObjectId && Item.Kind == SizeLane && Item.Id == Slot.Id && Item.Layer == Slot.Layer; });
					Core.Data.Sizes.push_back({ Slot.ObjectId, SizeLane, Slot.Id, Slot.Layer, Rows });
				}
				IsLayoutDirty = IsLayoutDirty || !Slots.empty();
			}
			else if (Input.KeyCtrl)
			{
				ScrollTime = std::clamp(ScrollTime - Input.MouseWheel * 80.0f / Zoom, 0.0f, MaxTime);
			}
			else if (!Input.KeyShift && IsOverTracks)
			{
				Zoom = std::clamp(Zoom * std::pow(1.2f, Input.MouseWheel), 4.0f, 2000.0f);
				ScrollTime = std::clamp(Pivot - (Input.MousePos.x - TrackLeft) / Zoom, 0.0f, MaxTime);
			}
			else
			{
				ImGui::SetScrollY(ImGui::GetScrollY() - Input.MouseWheel * RowHeight * 3.0f);
			}
		}
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0.0f, 0.0f));
		ImDrawList* List = ImGui::GetWindowDrawList();
		BuildRows(Core.Data);
		RowsTop = ImGui::GetCursorScreenPos().y;
		const float RowsLeft = ImGui::GetCursorScreenPos().x, Total = Rows.empty() ? 0.0f : Rows.back().Y + Rows.back().Height;
		const float ViewTop = ImGui::GetWindowPos().y, ViewBottom = ViewTop + ImGui::GetWindowHeight();
		const ImVec2 BoxStart(TimeToX(DragTime), RowsTop + DragOriginY);
		Marquee = ImRect(ImMin(BoxStart, Input.MousePos), ImMax(BoxStart, Input.MousePos));
		Marquee.ClipWithFull(ImRect(TrackLeft, -FLT_MAX, TrackLeft + TrackWidth, FLT_MAX));
		ImGui::SetCursorScreenPos(ImVec2(TrackLeft - 4.0f, ViewTop));
		ImGui::InvisibleButton("##Splitter", ImVec2(4.0f, ViewBottom - ViewTop));
		if (ImGui::IsItemActive() && Input.MouseDelta.x != 0.0f)
		{
			Settings.HeaderWidth = std::clamp(Input.MousePos.x - RowsLeft + 2.0f, MinHeaderWidth, MaxHeaderWidth);
		}
		if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
		{
			Settings.HeaderWidth = EditorSettings {}.HeaderWidth;
		}
		if (ImGui::IsItemHovered() || ImGui::IsItemActive())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
			List->AddLine(ImVec2(TrackLeft - 2.0f, ViewTop), ImVec2(TrackLeft - 2.0f, ViewBottom), ImGui::GetColorU32(ImGui::IsItemActive() ? ImGuiCol_SeparatorActive : ImGuiCol_SeparatorHovered), 2.0f);
		}
		for (const RowItem& Row : Rows)
		{
			const ImVec2 Corner(RowsLeft, RowsTop + Row.Y);
			if (Corner.y > ViewBottom || Corner.y + Row.Height < ViewTop)
			{
				continue;
			}
			const Object* Target = Row.IsFolder ? nullptr : Find(Core.Data, Row.Id);
			ImGui::PushID(static_cast<s32>(Row.IsFolder));
			ImGui::PushID(static_cast<s32>(Row.Id));
			if (Target)
			{
				SizeEdge(Core, { Row.Id, SizeRow }, Corner.x, TrackLeft - 4.0f, Corner.y, Corner.y + Row.Track);
			}
			DrawRowHeader(Core, Row, ImRect(Corner.x, Corner.y, TrackLeft - 4.0f, Corner.y + Row.Track));
			ImGui::SetCursorScreenPos(ImVec2(TrackLeft, Corner.y));
			ImGui::InvisibleButton("##Track", ImVec2(TrackWidth, Row.Track), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
			HandleTrack(Core, Target);
			if (Target)
			{
				DrawClip(Core, *Target, ImRect(TimeToX(Target->Span.Start), Corner.y + 1.0f, TimeToX(ClipEnd(*Target)), Corner.y + Row.Track - 1.0f));
			}
			const u8 Subs = Target ? SubRows(*Target) : 0;
			float Y = Corner.y + Row.Track;
			for (u8 Sub = 0; Sub < Subs; ++Sub)
			{
				const float Height = SizeOf(Core.Data, Row.Id, SizeSub, Param::Position, Sub);
				DrawSubRow(Core, *Target, Sub, ImRect(Corner.x + (Row.Depth + 1) * RowHeight, Y, TrackLeft - 4.0f, Y + Height), ImRect(TrackLeft, Y, TrackLeft + TrackWidth, Y + Height));
				Y += Height;
			}
			if (Target && Corner.y + Row.Height > Y + 0.5f)
			{
				DrawLanes(Core, *Target, Corner.x + (Row.Depth + 1) * RowHeight, Y);
			}
			ImGui::PopID();
			ImGui::PopID();
		}
		ImGui::SetCursorScreenPos(ImVec2(RowsLeft, RowsTop + Total));
		ImGui::InvisibleButton("##Below", ImVec2(Settings.HeaderWidth + TrackWidth, std::max(ImGui::GetContentRegionAvail().y, RowHeight)), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
		HandleTrack(Core, nullptr);
		ImGui::PopStyleVar();
		UpdateDrag(Core);
		if (Drag >= DragKind::BoxClips && HasDragMoved)
		{
			List->PushClipRect(ImVec2(TrackLeft, ViewTop), ImVec2(TrackLeft + TrackWidth, ViewBottom), true);
			List->AddRectFilled(Marquee.Min, Marquee.Max, ImGui::GetColorU32(ImGuiCol_Header, 0.4f));
			List->AddRect(Marquee.Min, Marquee.Max, ImGui::GetColorU32(ImGuiCol_HeaderActive));
			List->PopClipRect();
		}
		if (DragLimit >= 0.0f && Drag != DragKind::None)
		{
			const float X = TimeToX(DragLimit);
			List->AddLine(ImVec2(X, Input.MousePos.y - 0.5f * RowHeight), ImVec2(X, Input.MousePos.y + 0.5f * RowHeight), ImGui::GetColorU32(ErrorColor), 2.0f);
		}
		if (Drag == DragKind::RowMove && HasDragMoved)
		{
			const TimelineUi::DropSpot Spot = TimelineUi::Drop(Rows, RowsTop, Input.MousePos.y, IsDragFolder);
			const s32 Count = static_cast<s32>(Rows.size());
			const auto RowY = [&](s32 Index) { return RowsTop + (Index < Count ? Rows[Index].Y : Total); };
			s32 Line = Spot.Row < 0 ? Count : Spot.Row + (Spot.IsBefore ? 0 : 1);
			while (!Spot.IsInto && !Spot.IsBefore && Spot.Row >= 0 && Rows[Spot.Row].IsFolder && Line < Count && Rows[Line].Depth)
			{
				++Line;
			}
			const float Left = ImGui::GetWindowPos().x, Right = TrackLeft + TrackWidth;
			const u32 Color = ImGui::GetColorU32(ImGuiCol_DragDropTarget);
			if (Spot.IsInto)
			{
				List->AddRect(ImVec2(Left, RowY(Spot.Row)), ImVec2(Right, RowY(Spot.Row) + Rows[Spot.Row].Track), Color, 0.0f, 0, 2.0f);
			}
			else
			{
				List->AddLine(ImVec2(Left, RowY(Line)), ImVec2(Right, RowY(Line)), Color, 2.0f);
			}
		}
		DrawTimelinePopups(Core);
		const ImVec2 Top = ImGui::GetWindowPos();
		DrawMarkers(Core, ImRect(TrackLeft, Top.y, TrackLeft + TrackWidth, Top.y + ImGui::GetWindowHeight()), false);
		ImGui::EndChild();
		const ImVec2 Line = ImGui::GetCursorScreenPos();
		const float Room = ImGui::GetContentRegionAvail().x, Told = Settings.ShowHints && Hint ? ImGui::CalcTextSize(Hint).x + 8.0f : 0.0f;
		TimelineUi::Label(ImGui::GetWindowDrawList(), ImRect(Line.x - 3.0f, Line.y, Line.x + std::max(Room - Told, 1.0f), Line.y + ImGui::GetTextLineHeight()), ImGui::GetColorU32(IsStatusError ? ErrorColor : ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled)), StatusText.c_str());
		if (Told > 0.0f)
		{
			ImGui::GetWindowDrawList()->AddText(ImVec2(Line.x + Room - Told + 8.0f, Line.y), ImGui::GetColorU32(ImGuiCol_TextDisabled), Hint);
		}
		Hint = nullptr;
		ImGui::Dummy(ImVec2(1.0f, ImGui::GetTextLineHeight()));
	}
	ImGui::End();
}

void Editor::DrawTransport(Player& Core)
{
	const ImGuiStyle& Style = ImGui::GetStyle();
	const ImGuiIO& Input = ImGui::GetIO();
	const auto Flow = [](float Width)
	{
		ImGui::SameLine();
		if (ImGui::GetContentRegionAvail().x < Width)
		{
			ImGui::NewLine();
		}
	};
	const auto Wide = [&Style](const char* Text) { return ImGui::CalcTextSize(Text).x + 2.0f * Style.FramePadding.x; };
	const auto Check = [&Style](const char* Text) { return ImGui::GetFrameHeight() + Style.ItemInnerSpacing.x + ImGui::CalcTextSize(Text).x; };
	const bool IsAlone = ShowAnimator && AnimClipIndex >= 0 && !IsAnimFollowing && !Core.IsPlaying;
	if (ImGui::Button("|<"))
	{
		IsAlone ? (void)(AnimTime = 0.0f) : SeekBoth(Core, 0.0f);
	}
	ImGui::SetItemTooltip("Start (Home)");
	Flow(Wide("Pause"));
	if (ImGui::Button(Core.IsPlaying || (IsAlone && IsAnimPlaying) ? "Pause" : "Play"))
	{
		IsAlone ? (void)(IsAnimPlaying = !IsAnimPlaying) : PlayPause(Core, Core.IsPlaying);
	}
	ImGui::SetItemTooltip("Play from marker (Space) / Continue (Shift+Space)");
	Flow(Wide("Stop"));
	if (ImGui::Button("Stop"))
	{
		IsAnimPlaying = false;
		AnimTime = IsAlone ? 0.0f : AnimTime;
		Core.Stop();
	}
	ImGui::SetItemTooltip("Stop (Space)");
	if (!ShowAnimator)
	{
		Flow(Check("Follow"));
		if (ImGui::Checkbox("Follow", &Settings.IsFollowing))
		{
			IsFollowPaused = false;
		}
		ImGui::SetItemTooltip("Follow");
	}
	Flow(Check("Loop"));
	bool IsLooping = Core.IsLooping;
	if (ImGui::Checkbox("Loop", &IsLooping))
	{
		SetLooping(Core, IsLooping);
	}
	ImGui::SetItemTooltip("Loop (Ctrl+L)");
	string32 Now, Last;
	string64 Clock;
	Timecode(Now, Core.Time, Core.Data.Header.FrameRate);
	Timecode(Last, ContentEnd(Core.Data), Core.Data.Header.FrameRate);
	xr_sprintf(Clock, "%s / %s", Now, Last);
	Flow(ImGui::CalcTextSize(Clock).x);
	ImGui::TextUnformatted(Clock);
	s32 Choice = static_cast<s32>(std::find(std::begin(TimelineUi::Speeds), std::end(TimelineUi::Speeds), Core.Speed) - std::begin(TimelineUi::Speeds));
	Flow(ImGui::GetFontSize() * 4.5f);
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 4.5f);
	if (ImGui::Combo("##Speed", &Choice, TimelineUi::SpeedLabels, 4))
	{
		Core.Speed = TimelineUi::Speeds[Choice];
	}
	ImGui::SetItemTooltip("Playback speed");
	if (!ShowAnimator)
	{
		Flow(Check("Snap") + Style.ItemSpacing.x + ImGui::GetFontSize() * 4.5f);
		ImGui::Checkbox("Snap", &Settings.IsSnapping);
		ImGui::SameLine();
		s32 Step = Settings.SnapStep;
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 4.5f);
		if (ImGui::Combo("##SnapStep", &Step, TimelineUi::SnapLabels, 7))
		{
			Settings.SnapStep = static_cast<u8>(Step);
		}
		ImGui::SetItemTooltip("Snap step");
		Flow(ImGui::GetFontSize() * 6.0f + Style.ItemSpacing.x + Wide("Fit"));
		const float PlayX = TimeToX(Core.Time);
		const float AnchorX = PlayX >= TrackLeft && PlayX <= TrackLeft + TrackWidth ? PlayX - TrackLeft : 0.5f * TrackWidth;
		string32 ZoomText;
		xr_sprintf(ZoomText, "%.0f px/s##Zoom", Zoom);
		ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_FrameBg));
		ImGui::Button(ZoomText, ImVec2(ImGui::GetFontSize() * 6.0f, 0.0f));
		ImGui::PopStyleColor();
		if (ImGui::IsItemHovered() || ImGui::IsItemActive())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeNS);
		}
		if (ImGui::IsItemActivated())
		{
			ZoomAnchor[0] = ScrollTime + AnchorX / Zoom;
			ZoomAnchor[1] = AnchorX;
		}
		if (ImGui::IsItemActive() && Input.MouseDelta.y != 0.0f)
		{
			Zoom = std::clamp(Zoom * std::pow(Input.KeyShift ? 1.002f : 1.02f, -Input.MouseDelta.y), 4.0f, 2000.0f);
			ScrollTime = std::clamp(ZoomAnchor[0] - ZoomAnchor[1] / Zoom, 0.0f, MaxTime);
			IsFollowPaused = IsFollowPaused || Core.IsPlaying;
		}
		if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
		{
			ZoomTo(0.0f, std::max(ContentEnd(Core.Data), 1.0f));
		}
		ImGui::SetItemTooltip("Zoom");
		ImGui::SameLine();
		if (ImGui::Button("Fit"))
		{
			ZoomTo(0.0f, std::max(ContentEnd(Core.Data), 1.0f));
		}
	}
	Flow(Wide("Auto-key"));
	ImGui::PushStyleColor(ImGuiCol_Text, IsAutoKey ? ErrorColor : ImGui::GetStyleColorVec4(ImGuiCol_Text));
	if (ImGui::Button("Auto-key"))
	{
		IsAutoKey = !IsAutoKey;
	}
	ImGui::PopStyleColor();
	ImGui::SetItemTooltip("Auto-key (K)");
	Flow(ImGui::GetFontSize() * 5.0f);
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 5.0f);
	ImGui::SliderFloat("##Opacity", &Settings.Opacity, 0.1f, 1.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	ImGui::SetItemTooltip("Window opacity");

	const char* State = Core.IsPlaying ? "Playing" : "Paused";
	Flow(ImGui::CalcTextSize(State).x);
	ImGui::TextDisabled("%s", State);
	if (Core.View() == ViewSource::Busy)
	{
		Flow(ImGui::CalcTextSize("Camera busy").x);
		ImGui::TextColored(ErrorColor, "Camera busy");
	}
}

void Editor::SetLooping(Player& Core, bool IsOn)
{
	const float End = ContentEnd(Core.Data);
	Core.IsLooping = IsOn;
	if (IsOn && !Core.HasLoopRange && End > 0.0f)
	{
		Core.LoopStart = 0.0f;
		Core.LoopEnd = End;
		Core.HasLoopRange = true;
	}
}

void Editor::PlayPause(Player& Core, bool IsContinue)
{
	IsFollowPaused = false;
	if (Core.IsPlaying)
	{
		IsContinue ? Core.Pause() : Core.Stop();
		return;
	}
	const float Marker = Core.PlayStart;
	Core.Seek(IsContinue ? Core.Time : Marker);
	Core.Play();
	Core.PlayStart = IsContinue ? Marker : Core.PlayStart;
}

void Editor::SeekBoth(Player& Core, float Time)
{
	Core.Seek(Time);
	Core.PlayStart = Core.IsPlaying ? Core.PlayStart : Core.Time;
}

void Editor::ZoomTo(float Lo, float Hi)
{
	const float Width = std::max(Hi - Lo, 0.1f);
	Zoom = std::clamp(TrackWidth / (Width * 1.05f), 4.0f, 2000.0f);
	ScrollTime = std::clamp(Lo - 0.025f * Width, 0.0f, MaxTime);
}

void Editor::ZoomAround(float Factor, float Pivot)
{
	const float X = std::clamp(TimeToX(Pivot), TrackLeft, TrackLeft + TrackWidth);
	Zoom = std::clamp(Zoom * Factor, 4.0f, 2000.0f);
	ScrollTime = std::clamp(Pivot - (X - TrackLeft) / Zoom, 0.0f, MaxTime);
}

bool Editor::SelectionSpan(const Player& Core, float& Lo, float& Hi) const
{
	const bool IsKeys = Focus == FocusKind::Keys;
	Lo = MaxTime;
	Hi = 0.0f;
	for (const KeyRef& Ref : SelectedKeys)
	{
		const Key* Item = IsKeys ? TimelineUi::FindKey((Cinema::Scene&)Core.Data, Ref) : nullptr;
		const float At = Item ? TargetStart(*Find(Core.Data, Ref.ObjectId), Ref.Block) + Item->Time : Lo;
		Lo = std::min(Lo, At);
		Hi = Item ? std::max(Hi, At) : Hi;
	}
	for (u32 Id : Selected)
	{
		const Object* Item = IsKeys ? nullptr : Find(Core.Data, Id);
		Lo = Item ? std::min(Lo, Item->Span.Start) : Lo;
		Hi = Item ? std::max(Hi, ClipEnd(*Item)) : Hi;
	}
	return Hi > Lo;
}

void Editor::LoopSelection(Player& Core)
{
	float Lo = 0.0f, Hi = 0.0f;
	if (!SelectionSpan(Core, Lo, Hi))
	{
		SetLooping(Core, !Core.IsLooping);
		return;
	}
	Core.LoopStart = Lo;
	Core.LoopEnd = Hi;
	Core.HasLoopRange = Core.IsLooping = true;
	Core.Seek(Lo);
}

void Editor::BuildRows(const Scene& Data)
{
	std::erase_if(Expanded, [&Data](u32 Id) { return !Find(Data, Id); });
	std::erase_if(Slots, [this, &Data](const LaneSlot& Slot) { return std::find(Expanded.begin(), Expanded.end(), Slot.ObjectId) == Expanded.end() || !IsParamShown(*Find(Data, Slot.ObjectId), Slot.Id) || (Slot.Id >= Param::ShakeInfluence && Slot.Layer >= TimelineUi::ShakeLayers(*Find(Data, Slot.ObjectId))) || (Slot.Id == Param::LayerWeight && Slot.Layer > TimelineUi::ShakeLayers(*Find(Data, Slot.ObjectId))); });
	for (u32 Id : Expanded)
	{
		Object& Target = const_cast<Object&>(*Find(Data, Id));
		CollectLaneTargets(Target);
		for (const LaneTarget& Where : LaneTargets)
		{
			for (const Lane& Item : LaneList(Target, Where.Block))
			{
				if (IsParamShown(Target, Item.Id) && !FindSlot(Id, Item.Id, Item.Layer))
				{
					Slots.push_back({ Id, Item.Id, Item.Layer });
				}
			}
		}
	}
	Rows.clear();
	TimelineUi::Siblings(Data, 0, RowScratch);
	for (const RowItem& Item : RowScratch)
	{
		Rows.push_back(Item);
		if (Item.IsFolder && std::find(Collapsed.begin(), Collapsed.end(), Item.Id) == Collapsed.end())
		{
			TimelineUi::Siblings(Data, Item.Id, MemberScratch);
			Rows.insert(Rows.end(), MemberScratch.begin(), MemberScratch.end());
		}
	}
	float Y = 0.0f;
	for (RowItem& Item : Rows)
	{
		Item.Y = Y;
		const Object* Target = Item.IsFolder ? nullptr : Find(Data, Item.Id);
		Item.Track = Target ? SizeOf(Data, Item.Id, SizeRow, Param::Position, 0) : RowHeight;
		Item.Height = Item.Track + (Target ? LanesHeight(Data, Item.Id) : 0.0f);
		for (u8 Sub = 0; Target && Sub < SubRows(*Target); ++Sub)
		{
			Item.Height += SizeOf(Data, Item.Id, SizeSub, Param::Position, Sub);
		}
		Y += Item.Height;
	}
}

void Editor::DrawRuler(Player& Core, const ImRect& Full)
{
	const ImRect Rect(Full.Min.x, Full.Min.y + TimelineUi::OverviewHeight, Full.Max.x, Full.Max.y);
	xr_vector<Marker>& Markers = Core.Data.Header.Markers;
	if (RenameMarker >= 0)
	{
		ImGui::SetNextItemAllowOverlap();
	}
	ImGui::InvisibleButton("##Ruler", Full.GetSize(), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
	CanvasId = ImGui::IsItemActive() ? ImGui::GetItemID() : CanvasId;
	const ImGuiIO& Input = ImGui::GetIO();
	const u32 Rate = Core.Data.Header.FrameRate;
	const bool IsActivated = ImGui::IsItemActivated();
	const float BraceIn = TimeToX(Core.LoopStart), BraceOut = TimeToX(Core.LoopEnd), Grip = std::min(TimelineUi::EdgeZone, (BraceOut - BraceIn) / 3.0f);
	const bool IsOverview = ImGui::IsItemHovered() && Input.MousePos.y < Rect.Min.y;
	const bool IsBrace = ImGui::IsItemHovered() && !IsOverview && Input.MousePos.y < Rect.Min.y + TimelineUi::BraceHeight;
	const bool IsOverviewDrag = ImGui::IsItemActive() && Drag == DragKind::Scrub && RulerMode >= 3;
	const float Whole = IsOverviewDrag ? OverviewWhole : std::max({ ContentEnd(Core.Data), ScrollTime + TrackWidth / Zoom, 1.0f });
	OverviewWhole = Whole;
	const auto OverX = [&](float Time) { return Full.Min.x + Time / Whole * Full.GetWidth(); };
	const float ViewIn = OverX(ScrollTime), ViewOut = OverX(ScrollTime + TrackWidth / Zoom), OverTime = (Input.MousePos.x - Full.Min.x) / Full.GetWidth() * Whole;
	const bool IsOnBrace = IsBrace && Core.HasLoopRange && Input.MousePos.x >= BraceIn - TimelineUi::EdgeZone && Input.MousePos.x <= BraceOut + TimelineUi::EdgeZone;
	const DragKind BraceKind = Input.MousePos.x <= BraceIn + Grip ? DragKind::LoopIn : Input.MousePos.x >= BraceOut - Grip ? DragKind::LoopOut : DragKind::LoopMove;
	HoverMarker = Drag == DragKind::MarkerMove ? DragMarker : ImGui::IsItemHovered() && !IsBrace && !IsOverview ? MarkerAt(Core.Data.Header, Input.MousePos.x) : -1;
	if ((IsOnBrace && BraceKind != DragKind::LoopMove) || Drag == DragKind::LoopIn || Drag == DragKind::LoopOut)
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
	}
	if (IsActivated)
	{
		const bool IsLeft = ImGui::IsMouseClicked(ImGuiMouseButton_Left);
		Drag = IsOverview ? (IsLeft ? DragKind::Scrub : DragKind::None) : !IsLeft ? DragKind::LoopRange : IsOnBrace ? BraceKind : HoverMarker >= 0 ? DragKind::MarkerMove : DragKind::Scrub;
		DragMarker = PopupMarker = HoverMarker;
		DragLoop[0] = IsOverview ? ScrollTime : Core.LoopStart;
		DragLoop[1] = IsOverview ? ScrollTime + TrackWidth / Zoom : Core.LoopEnd;
		const float Near = std::fabs(Input.MousePos.x - ViewIn), Far = std::fabs(Input.MousePos.x - ViewOut);
		RulerMode = static_cast<u8>(!IsOverview ? 0 : Near <= TimelineUi::EdgeZone ? 4 : Far <= TimelineUi::EdgeZone ? 5 : Input.MousePos.x > ViewIn && Input.MousePos.x < ViewOut ? 3 : 6);
		HasDragMoved = false;
		DragIds.clear();
		CollectSnapTargets(Core, false);
	}
	const float Snapped = std::max(SnapTime(XToTime(Input.MousePos.x), Input.KeyShift), 0.0f);
	if (IsActivated)
	{
		DragTime = PopupTime = Snapped;
	}
	if (IsActivated && IsBrace && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		Core.HasLoopRange = false;
		SetLooping(Core, true);
		Drag = DragKind::None;
	}
	if (ImGui::IsItemActive() && Drag == DragKind::Scrub)
	{
		const ImVec2 Pulled(Input.MousePos.x - Input.MouseClickedPos[ImGuiMouseButton_Left].x, Input.MousePos.y - Input.MouseClickedPos[ImGuiMouseButton_Left].y);
		IsFollowPaused = IsFollowPaused || (Core.IsPlaying && RulerMode);
		const float In = std::clamp(OverTime, 0.0f, DragLoop[1] - 0.1f), Out = std::clamp(OverTime, DragLoop[0] + 0.1f, MaxTime);
		switch (RulerMode)
		{
		case 0:
			SeekBoth(Core, Snapped);
			break;
		case 3:
			ScrollTime = std::clamp(DragLoop[0] + Pulled.x / Full.GetWidth() * Whole, 0.0f, MaxTime);
			break;
		case 4:
			ZoomTo(In, DragLoop[1]);
			break;
		case 5:
			ZoomTo(DragLoop[0], Out);
			break;
		default:
			ScrollTime = std::clamp(OverTime - 0.5f * (DragLoop[1] - DragLoop[0]), 0.0f, MaxTime);
			break;
		}
	}
	if (ImGui::IsItemActive() && Drag == DragKind::LoopRange && ImGui::IsMouseDragging(ImGuiMouseButton_Right, 4.0f))
	{
		Core.HasLoopRange = Core.IsLooping = true;
		Core.LoopStart = std::min(DragTime, Snapped);
		Core.LoopEnd = std::max(DragTime, Snapped);
	}
	const auto IsBraceDrag = [this] { return Drag >= DragKind::LoopMove && Drag <= DragKind::LoopOut; };
	if (IsBraceDrag() && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		Core.LoopStart = DragLoop[0];
		Core.LoopEnd = DragLoop[1];
		Drag = DragKind::None;
	}
	if (ImGui::IsItemActive() && IsBraceDrag() && (HasDragMoved || ImGui::IsMouseDragging(ImGuiMouseButton_Left, 4.0f)))
	{
		const float Frame = 1.0f / static_cast<float>(Rate), Length = DragLoop[1] - DragLoop[0];
		const float Moved = std::clamp(SnapSpan(DragLoop[0] + XToTime(Input.MousePos.x) - XToTime(Input.MouseClickedPos[ImGuiMouseButton_Left].x), Length, Input.KeyShift), 0.0f, std::max(MaxTime - Length, 0.0f));
		HasDragMoved = true;
		Core.LoopStart = Drag == DragKind::LoopMove ? Moved : Drag == DragKind::LoopIn ? std::clamp(Snapped, 0.0f, DragLoop[1] - Frame) : DragLoop[0];
		Core.LoopEnd = Drag == DragKind::LoopMove ? Moved + Length : Drag == DragKind::LoopOut ? std::clamp(Snapped, DragLoop[0] + Frame, MaxTime) : DragLoop[1];
	}
	if (Drag == DragKind::MarkerMove && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		if (HasDragMoved)
		{
			Undo.Cancel(Core.Data);
		}
		Drag = DragKind::None;
	}
	if (ImGui::IsItemActive() && Drag == DragKind::MarkerMove && (HasDragMoved || ImGui::IsMouseDragging(ImGuiMouseButton_Left, 4.0f)))
	{
		if (!HasDragMoved)
		{
			Undo.Begin("Move marker");
			Undo.TouchHeader(Core.Data);
		}
		HasDragMoved = true;
		Markers[DragMarker].Time = std::min(Snapped, MaxTime);
	}
	if (ImGui::IsItemDeactivated())
	{
		if (IsBraceDrag() && !HasDragMoved)
		{
			SetLooping(Core, !Core.IsLooping);
		}
		if (Drag == DragKind::LoopRange && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Right] < 16.0f)
		{
			ImGui::OpenPopup(PopupMarker >= 0 ? "##MarkerMenu" : "##RulerMenu");
		}
		if (Drag == DragKind::MarkerMove && !HasDragMoved)
		{
			Core.Seek(Markers[DragMarker].Time);
		}
		else if (Drag == DragKind::MarkerMove && !IsMarkerFree(Core.Data.Header, Markers[DragMarker].Time, DragMarker))
		{
			Undo.Cancel(Core.Data);
			SetStatus(true, "Another marker is on that frame.");
		}
		else if (Drag == DragKind::MarkerMove)
		{
			std::stable_sort(Markers.begin(), Markers.end(), [](const Marker& A, const Marker& B) { return A.Time < B.Time; });
		}
		Drag = DragKind::None;
	}
	const auto Rename = [&](s32 Index)
	{
		xr_sprintf(RenameBuffer, "%.127s", Markers[Index].Name.c_str());
		RenameMarker = Index;
		RenameId = 0;
		RenameFrames = 3;
	};
	if (HoverMarker >= 0 && ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		Rename(HoverMarker);
	}
	else if (ImGui::IsItemHovered() && !IsBrace && !IsOverview && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		ZoomTo(0.0f, std::max(ContentEnd(Core.Data), 1.0f));
		Drag = DragKind::None;
	}
	if (ImGui::BeginPopup("##MarkerMenu"))
	{
		if (ImGui::MenuItem("Rename", nullptr, false, PopupMarker >= 0))
		{
			Rename(PopupMarker);
		}
		if (ImGui::MenuItem("Remove", "Delete", false, PopupMarker >= 0))
		{
			RemoveMarker(Core, PopupMarker);
		}
		ImGui::EndPopup();
	}
	if (ImGui::BeginPopup("##RulerMenu"))
	{
		if (ImGui::MenuItem("Add marker here"))
		{
			AddMarker(Core, PopupTime);
		}
		if (ImGui::MenuItem("Clear loop", nullptr, false, Core.HasLoopRange))
		{
			Core.HasLoopRange = false;
		}
		ImGui::EndPopup();
	}
	u32 Frames = TimelineUi::SecondTicks[9] * Rate;
	for (u32 Index = 0; Index < 14; ++Index)
	{
		const u32 Candidate = Index < 4 ? TimelineUi::FrameTicks[Index] : TimelineUi::SecondTicks[Index - 4] * Rate;
		if (Candidate * Zoom >= 90.0f * Rate)
		{
			Frames = Candidate;
			break;
		}
	}
	const float Step = static_cast<float>(Frames) / Rate;
	const u32 Text = ImGui::GetColorU32(ImGuiCol_TextDisabled);
	ImDrawList* List = ImGui::GetWindowDrawList();
	List->PushClipRect(Rect.Min, Rect.Max, true);
	List->AddRectFilled(Rect.Min, Rect.Max, ImGui::GetColorU32(ImGuiCol_FrameBg));
	if (Core.HasLoopRange)
	{
		List->AddRectFilled(ImVec2(BraceIn, Rect.Min.y + 1.0f), ImVec2(std::max(BraceOut, BraceIn + 2.0f), Rect.Min.y + TimelineUi::BraceHeight), ImGui::GetColorU32(Core.IsLooping ? ImGuiCol_PlotHistogram : ImGuiCol_TextDisabled), 2.0f);
	}
	for (u32 Tick = static_cast<u32>(ScrollTime / Step); TimeToX(Tick * Step) < Rect.Max.x; ++Tick)
	{
		const float X = TimeToX(Tick * Step);
		string32 Code;
		Timecode(Code, Tick * Step, Rate);
		List->AddLine(ImVec2(X, Rect.Min.y + TimelineUi::BraceHeight + (Rect.GetHeight() - TimelineUi::BraceHeight) * 0.5f), ImVec2(X, Rect.Max.y), Text);
		List->AddText(ImVec2(X + 3.0f, Rect.Min.y + TimelineUi::BraceHeight), Text, Code);
	}
	const float Marker = TimeToX(Core.PlayStart);
	List->AddTriangle(ImVec2(Marker - 5.0f, Rect.Max.y - 8.0f), ImVec2(Marker + 5.0f, Rect.Max.y - 8.0f), ImVec2(Marker, Rect.Max.y - 1.0f), ImGui::GetColorU32(ImGuiCol_Text), 1.5f);
	List->PopClipRect();
	List->PushClipRect(Full.Min, ImVec2(Full.Max.x, Rect.Min.y), true);
	List->AddRectFilled(Full.Min, ImVec2(Full.Max.x, Rect.Min.y), ImGui::GetColorU32(ImGuiCol_WindowBg));
	for (u32 Index = 0; Index < Core.Data.Objects.size(); ++Index)
	{
		const Object& Item = Core.Data.Objects[Index];
		const float Y = Full.Min.y + 1.0f + static_cast<float>(Index % 4) * 2.0f;
		List->AddLine(ImVec2(OverX(Item.Span.Start), Y), ImVec2(std::max(OverX(ClipEnd(Item)), OverX(Item.Span.Start) + 1.0f), Y), TimelineUi::WithAlpha(Item.Color, 200u), 1.5f);
	}
	List->AddRectFilled(ImVec2(ViewIn, Full.Min.y), ImVec2(std::max(ViewOut, ViewIn + 2.0f), Rect.Min.y), ImGui::GetColorU32(ImGuiCol_Header, 0.35f));
	List->AddRect(ImVec2(ViewIn, Full.Min.y), ImVec2(std::max(ViewOut, ViewIn + 2.0f), Rect.Min.y), ImGui::GetColorU32(ImGuiCol_HeaderActive));
	List->AddLine(ImVec2(OverX(Core.Time), Full.Min.y), ImVec2(OverX(Core.Time), Rect.Min.y), ImGui::GetColorU32(ImGuiCol_Text));
	List->PopClipRect();
	if (IsOverview && (std::fabs(Input.MousePos.x - ViewIn) <= TimelineUi::EdgeZone || std::fabs(Input.MousePos.x - ViewOut) <= TimelineUi::EdgeZone))
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
	}
	DrawMarkers(Core, Rect, true);
	if (RenameMarker < 0 || RenameMarker >= static_cast<s32>(Markers.size()))
	{
		return;
	}
	const ImVec2 Cursor = ImGui::GetCursorScreenPos();
	ImGui::SetCursorScreenPos(ImVec2(TimeToX(Markers[RenameMarker].Time) + 4.0f, Rect.Min.y));
	if (RenameFrames == 3)
	{
		ImGui::SetKeyboardFocusHere();
	}
	ImGui::SetNextItemWidth(120.0f);
	ImGui::InputText("##MarkerName", RenameBuffer, sizeof(RenameBuffer), ImGuiInputTextFlags_AutoSelectAll);
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		CommitRename(Core);
	}
	if (RenameFrames)
	{
		--RenameFrames;
	}
	else if (!ImGui::IsItemActive())
	{
		RenameMarker = -1;
	}
	ImGui::SetCursorScreenPos(Cursor);
}

void Editor::DrawMarkers(const Player& Core, const ImRect& Track, bool IsRuler) const
{
	ImDrawList* List = ImGui::GetWindowDrawList();
	const xr_vector<Marker>& Markers = Core.Data.Header.Markers;
	const float Size = std::round(0.8f * ImGui::GetFontSize());
	List->PushClipRect(Track.Min, Track.Max, true);
	if (Core.HasLoopRange && Core.IsLooping)
	{
		List->AddRectFilled(ImVec2(TimeToX(Core.LoopStart), Track.Min.y), ImVec2(TimeToX(Core.LoopEnd), Track.Max.y), ImGui::GetColorU32(ImGuiCol_PlotHistogram, 60.0f / 255.0f));
	}
	for (u32 Index = 0; Index < Markers.size(); ++Index)
	{
		const float X = TimeToX(Markers[Index].Time), Next = Index + 1 < Markers.size() ? TimeToX(Markers[Index + 1].Time) : Track.Max.x;
		const u32 Color = ImGui::GetColorU32(IsRuler && static_cast<s32>(Index) == HoverMarker ? ImGuiCol_PlotHistogramHovered : ImGuiCol_PlotHistogram, IsRuler ? 1.0f : 120.0f / 255.0f);
		if (X > Track.Max.x || Next < Track.Min.x)
		{
			continue;
		}
		if (!IsRuler)
		{
			List->AddLine(ImVec2(X, Track.Min.y), ImVec2(X, Track.Max.y), Color);
			continue;
		}
		List->AddTriangleFilled(ImVec2(X - 4.0f, Track.Max.y - 7.0f), ImVec2(X + 4.0f, Track.Max.y - 7.0f), ImVec2(X, Track.Max.y), Color);
		TimelineUi::Label(List, ImRect(X + 5.0f, Track.Max.y - Size, Next, Track.Max.y), Color, Markers[Index].Name.c_str(), Size);
	}
	const float End = TimeToX(ContentEnd(Core.Data)), Head = TimeToX(Core.Time);
	List->AddLine(ImVec2(End, Track.Min.y), ImVec2(End, Track.Max.y), ImGui::GetColorU32(ImGuiCol_TextDisabled));
	List->AddLine(ImVec2(Head, Track.Min.y), ImVec2(Head, Track.Max.y), ImGui::GetColorU32(ImGuiCol_PlotLinesHovered), 2.0f);
	List->PopClipRect();
}

s32 Editor::MarkerAt(const SceneHeader& Header, float X) const
{
	s32 Found = -1;
	float Best = TimelineUi::EdgeZone;
	for (u32 Index = 0; Index < Header.Markers.size(); ++Index)
	{
		const float Distance = std::fabs(TimeToX(Header.Markers[Index].Time) - X);
		Found = Distance <= Best ? static_cast<s32>(Index) : Found;
		Best = std::min(Best, Distance);
	}
	return Found;
}

void Editor::DrawRowHeader(Player& Core, const RowItem& Row, const ImRect& Rect)
{
	const Object* Target = Row.IsFolder ? nullptr : Find(Core.Data, Row.Id);
	const Folder* Group = Row.IsFolder ? FindFolder(Core.Data, Row.Id) : nullptr;
	if (!Target && !Group)
	{
		return;
	}
	ImDrawList* List = ImGui::GetWindowDrawList();
	const ImGuiIO& Input = ImGui::GetIO();
	const float Size = RowHeight;
	const bool IsBroken = Target && (RuntimeError(Core, *Target) || TimelineUi::IsWarned(Core, *Target));
	const u32 Text = ImGui::GetColorU32(IsBroken ? ErrorColor : ImGui::GetStyleColorVec4(Target && Core.IsSilent(*Target) ? ImGuiCol_TextDisabled : ImGuiCol_Text));
	if (Row.IsFolder ? SelectedFolder == Row.Id : IsSelected(Row.Id))
	{
		List->AddRectFilled(Rect.Min, Rect.Max, ImGui::GetColorU32(ImGuiCol_Header));
	}
	float X = Rect.Min.x + 2.0f + Row.Depth * Size;
	ImGui::SetCursorScreenPos(ImVec2(X, Rect.Min.y + 1.0f));
	if (Group)
	{
		const auto Found = std::find(Collapsed.begin(), Collapsed.end(), Row.Id);
		if (ImGui::ArrowButton("##Fold", Found == Collapsed.end() ? ImGuiDir_Down : ImGuiDir_Right))
		{
			Found == Collapsed.end() ? Collapsed.push_back(Row.Id) : (void)Collapsed.erase(Found);
		}
		ImGui::SetItemTooltip("Expand");
	}
	else
	{
		const auto Found = std::find(Expanded.begin(), Expanded.end(), Row.Id);
		if (ImGui::ArrowButton("##Lanes", Found == Expanded.end() ? ImGuiDir_Right : ImGuiDir_Down))
		{
			const bool HasLanes = !Target->Lanes.empty() || std::any_of(Target->Camera.Blocks.begin(), Target->Camera.Blocks.end(), [](const CameraBlock& Shot) { return !Shot.Lanes.empty(); }) || std::any_of(Target->Camera.Shakes.begin(), Target->Camera.Shakes.end(), [](const ShakeClip& Clip) { return !Clip.Lanes.empty(); });
			if (Found == Expanded.end() && !HasLanes)
			{
				Slots.push_back({ Row.Id, TimelineUi::DefaultLanes[static_cast<u32>(Target->Type)] });
			}
			Found == Expanded.end() ? Expanded.push_back(Row.Id) : (void)Expanded.erase(Found);
		}
		ImGui::SetItemTooltip("Lanes");
		X += Size;
		ImGui::SetCursorScreenPos(ImVec2(X, Rect.Min.y + 1.0f));
		if (ImGui::ColorButton("##Colour", ImGui::ColorConvertU32ToFloat4(Target->Color), ImGuiColorEditFlags_NoTooltip, ImVec2(Size - 2.0f, Size - 2.0f)))
		{
			PopupRow = Row.Id;
			PopupRequest = "##Colour";
		}
		ImGui::SetItemTooltip("Colour");
	}
	TimelineUi::Icon(List, ImVec2(X + Size * 1.5f, Rect.Min.y + 0.5f * Size), Group ? 4u : static_cast<u32>(Target->Type), Text);
	X += Size * 2.0f;
	const float Right = std::max(Rect.Max.x - (Target ? 2.0f * Size : 0.0f), X + 1.0f);
	if (RenameId == Row.Id && IsRenameFolder == Row.IsFolder)
	{
		ImGui::SetCursorScreenPos(ImVec2(X, Rect.Min.y + 1.0f));
		if (RenameFrames == 3)
		{
			ImGui::SetKeyboardFocusHere();
		}
		ImGui::SetNextItemWidth(Right - X);
		ImGui::InputText("##Rename", RenameBuffer, sizeof(RenameBuffer), ImGuiInputTextFlags_AutoSelectAll);
		if (ImGui::IsItemDeactivatedAfterEdit())
		{
			CommitRename(Core);
		}
		if (RenameFrames)
		{
			--RenameFrames;
		}
		else if (!ImGui::IsItemActive())
		{
			RenameId = 0;
		}
	}
	else
	{
		ImGui::SetCursorScreenPos(ImVec2(X, Rect.Min.y));
		ImGui::InvisibleButton("##Name", ImVec2(Right - X, Size), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
		CanvasId = ImGui::IsItemActive() ? ImGui::GetItemID() : CanvasId;
		if (ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		{
			Press(Core, DragKind::RowMove, Row.Id, Row.IsFolder);
		}
		if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
		{
			StartRename(Core.Data, Row.Id, Row.IsFolder);
		}
		if (ImGui::IsItemDeactivated() && ImGui::IsMouseReleased(ImGuiMouseButton_Right) && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Right] < 16.0f)
		{
			OpenRowMenu(Row.Id, Row.IsFolder);
		}
		TimelineUi::Label(List, ImRect(X, Rect.Min.y, Right, Rect.Min.y + Size), Text, Group ? Group->Name.c_str() : Target->Name.empty() ? TypeNames[static_cast<u32>(Target->Type)] : Target->Name.c_str());
	}
	if (!Target)
	{
		return;
	}
	const bool IsSolo = std::find(Core.Soloed.begin(), Core.Soloed.end(), Row.Id) != Core.Soloed.end();
	ImGui::SetCursorScreenPos(ImVec2(Right, Rect.Min.y + 1.0f));
	ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(IsSolo ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
	if (ImGui::Button("S", ImVec2(Size - 2.0f, Size - 2.0f)))
	{
		const bool IsGroup = IsSelected(Row.Id);
		std::erase_if(Core.Soloed, [&](u32 Id) { return Id == Row.Id || (IsGroup && IsSelected(Id)); });
		for (const Object& Item : Core.Data.Objects)
		{
			if (!IsSolo && (Item.Id == Row.Id || (IsGroup && IsSelected(Item.Id))))
			{
				Core.Soloed.push_back(Item.Id);
			}
		}
	}
	ImGui::SetItemTooltip("Solo");
	ImGui::SetCursorScreenPos(ImVec2(Right + Size, Rect.Min.y + 1.0f));
	ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(Target->IsMuted ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
	if (ImGui::Button("M", ImVec2(Size - 2.0f, Size - 2.0f)))
	{
		SetMuted(Core, Row.Id, !Target->IsMuted);
	}
	ImGui::PopStyleColor(2);
	ImGui::SetItemTooltip("Mute");
}

void Editor::HandleTrack(Player& Core, const Object* Target)
{
	CanvasId = ImGui::IsItemActive() ? ImGui::GetItemID() : CanvasId;
	const ImGuiIO& Input = ImGui::GetIO();
	const float X = Input.MousePos.x;
	const float From = Target ? TimeToX(Target->Span.Start) : 0.0f, To = Target ? TimeToX(ClipEnd(*Target)) : 0.0f;
	const ActorRuntime* Runtime = Target && Target->Type == ObjectType::Actor ? Core.FindActor(Target->Id) : nullptr;
	const bool IsCamera = Target && Target->Type == ObjectType::Camera;
	const float Blocks = ImGui::GetItemRectMin().y + 1.0f + TimelineUi::Band(RowHeight);
	const bool IsBelow = Input.MousePos.y >= Blocks;
	DragKind Kind = DragKind::None;
	s32 Pose = -1;
	ClipPart Part = ClipPart::None;
	ImRect Area;
	const s32 Shot = IsCamera && IsBelow ? CameraHit(Core, *Target, Input.MousePos, Blocks, ImGui::GetItemRectMax().y - 1.0f, Kind, Pose, Part, Area) : -1;
	const bool IsDerived = Runtime && !Target->Actor.IsRelative && HasClips(Target->Actor);
	const bool IsEdge = Target && !IsCamera && !IsDerived && std::fabs(X - To) <= TimelineUi::EdgeZone && X > 0.5f * (From + To);
	const bool IsOnClip = IsEdge || Shot >= 0 || (Target && X >= From && X <= To && !(IsCamera && (IsBelow || Target->Camera.Blocks.empty())));
	if (IsEdge && ImGui::IsItemHovered())
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
	}
	if (Shot >= 0 && ImGui::IsItemHovered())
	{
		HoverBlock = Shot;
		HoverPose = Pose;
		if (Drag == DragKind::None)
		{
			ShowPart(Part, Area, Lighten(Target->Color), 1);
		}
	}
	if (ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		if (Shot >= 0)
		{
			Press(Core, Kind == DragKind::BlockBlend ? DragKind::BlockMove : Kind == DragKind::BlockLength && Input.KeyCtrl ? DragKind::BlockStretch : Kind, Target->Id, false);
			DragBlock = FocusBlock = Shot;
			DragPose = Pose;
			if (Pose >= 0)
			{
				SetTangentKey(Core, Shot, Pose);
			}
			DragShot = Target->Camera.Blocks[Shot];
			PinShakes(*Target);
			DragShots = Target->Camera.Blocks;
			DragShakes = Target->Camera.Shakes;
			DragPins = Pins;
			DragShotIndex = Shot;
			DragIds.clear();
			CollectSnapTargets(Core, true);
			for (u32 Index = 0; Pose >= 0 && Index < Target->Camera.Blocks.size(); ++Index)
			{
				const CameraBlock& Item = Target->Camera.Blocks[Index];
				const BlockCache* Other = Core.FindCameraBlock(Index);
				for (u32 At = 0; Other && At < Item.Keys.size(); ++At)
				{
					if (Index != static_cast<u32>(Shot) || At != static_cast<u32>(Pose))
					{
						SnapTargets.push_back(Target->Span.Start + Item.Start + Cameras::LocalFromPath(Item, *Other, Item.Keys[At].Time));
					}
				}
			}
			FocusObject = Target->Id;
			Focus = FocusKind::Block;
			if (const BlockCache* Cache = Pose >= 0 ? Core.FindCameraBlock(static_cast<u32>(Shot)) : nullptr)
			{
				Core.Seek(TargetStart(*Target, Shot) + Cameras::LocalFromPath(DragShot, *Cache, DragShot.Keys[Pose].Time));
			}
		}
		else if (IsOnClip)
		{
			Press(Core, IsEdge ? DragKind::ClipLength : DragKind::ClipMove, Target->Id, false);
			IsDragCopy = IsDragCopy && Target->Type != ObjectType::Camera;
		}
		else
		{
			StartBox(DragKind::BoxClips);
		}
	}
	if (ImGui::IsItemDeactivated() && ImGui::IsMouseReleased(ImGuiMouseButton_Right) && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Right] < 16.0f)
	{
		PopupTime = std::max(XToTime(X), 0.0f);
		if (IsOnClip)
		{
			OpenRowMenu(Target->Id, false);
			PopupBlock = Shot;
			PopupPose = Pose;
			PopupRequest = Pose >= 0 ? "##PoseMenu" : Shot >= 0 && Kind == DragKind::BlockBlend ? "##CutMenu" : PopupRequest;
			Core.IsLookingThrough = Core.IsLookingThrough || IsCamera;
		}
		else
		{
			PopupRequest = "##Add";
			IsPopupChoosing = false;
		}
	}
}

void Editor::DrawClip(const Player& Core, const Object& Target, const ImRect& Rect)
{
	ImDrawList* List = ImGui::GetWindowDrawList();
	const ActorRuntime* Runtime = Target.Type == ObjectType::Actor ? Core.FindActor(Target.Id) : nullptr;
	const bool IsCamera = Target.Type == ObjectType::Camera;
	const bool HasSpans = IsCamera && !Target.Camera.Blocks.empty();
	const char* Error = RuntimeError(Core, Target);
	const bool IsBroken = Error && !HasSpans && !(Runtime && Runtime->IsComplete());
	const float Band = HasSpans ? TimelineUi::Band(RowHeight) : 0.0f;
	const ImRect Below(Rect.Min.x, Rect.Min.y + Band, Rect.Max.x, Rect.Max.y);
	List->PushClipRect(ImVec2(TrackLeft, Rect.Min.y), ImVec2(TrackLeft + TrackWidth, Rect.Max.y), true);
	const bool IsSilent = Core.IsSilent(Target);
	List->AddRectFilled(Rect.Min, IsCamera ? ImVec2(Rect.Max.x, Rect.Min.y + Band) : Rect.Max, TimelineUi::WithAlpha(IsBroken ? ImGui::GetColorU32(ErrorColor) : Target.Color, IsSilent ? 60 : 170), 2.0f);
	if (IsCamera)
	{
		DrawCameraBlocks(Core, Target, Below);
	}
	const u32 Tick = TimelineUi::WithAlpha(ImGui::GetColorU32(ImGuiCol_Text), 200u);
	const bool IsCollapsed = std::find(Expanded.begin(), Expanded.end(), Target.Id) == Expanded.end();
	for (u32 Index = 0; IsCollapsed && Index < Target.Camera.Blocks.size(); ++Index)
	{
		DrawKeyTicks(List, Target.Camera.Blocks[Index].Lanes, Target.Span.Start + Target.Camera.Blocks[Index].Start, Rect.Max.y - 3.5f, Tick, &Target.Camera.Blocks[Index]);
	}
	if (IsCollapsed)
	{
		DrawKeyTicks(List, Target.Lanes, Target.Span.Start, Rect.Max.y - 3.5f, Tick);
	}
	string1024 Text;
	const ObjectState* State = Core.FindState(Target.Id);
	TimelineUi::LinkLabel(Text, Core.Data, Error && (!Runtime || Runtime->Error.size()) ? Error : Target.Name.c_str(), Target.Attach);
	const u32 Ink = State && State->Frames.Problems ? ImGui::GetColorU32(ErrorColor) : ImGui::GetColorU32(IsSilent ? ImGuiCol_TextDisabled : ImGuiCol_Text);
	if (!IsCamera || HasSpans)
	{
		TimelineUi::Label(List, HasSpans ? ImRect(Rect.Min.x, Rect.Min.y, Rect.Max.x, Rect.Min.y + Band) : Rect, Ink, Text, Band);
	}
	if (IsSelected(Target.Id))
	{
		List->AddRect(Rect.Min, Rect.Max, Lighten(Target.Color), 2.0f, 0, 2.0f);
	}
	List->PopClipRect();
}

void Editor::DrawActorBlocks(const Object& Target, const ActorRuntime& Runtime, const ImRect& Rect, u8 Layer, u32 Fill)
{
	const float Length = Runtime.SequenceLength;
	if (!(Length > 0.0f) || Layer > Target.Actor.Layers.size())
	{
		return;
	}
	ImDrawList* List = ImGui::GetWindowDrawList();
	const ActorData& Setup = Target.Actor;
	const LayerRuntime& Track = Runtime.Track(Layer);
	const xr_vector<ActorBlock>& Blocks = BlocksOf(Setup, Layer);
	const Clip& Span = Target.Span;
	const auto AreaAt = [&](float Local) { return Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Local, Setup.TimeScale); };
	const bool IsRepeating = Setup.End != EndMode::Hold;
	const double Near = AreaAt(std::clamp(XToTime(TrackLeft) - Span.Start, 0.0f, Span.Length)), Far = AreaAt(std::clamp(XToTime(TrackLeft + TrackWidth) - Span.Start, 0.0f, Span.Length));
	const double First = IsRepeating ? std::floor(Near / Length) : 0.0;
	const double Last = IsRepeating ? std::floor(Far / Length) : 0.0;
	const u32 Count = static_cast<u32>(std::clamp(Last - First, 0.0, static_cast<double>(TrackWidth) * 0.5));
	const u32 Text = ImGui::GetColorU32(Target.IsMuted || RowOf(Setup, Layer).IsMuted ? ImGuiCol_TextDisabled : ImGuiCol_Text);
	const ImVec2 Mouse = ImGui::GetIO().MousePos;
	bool IsHovered = ImGui::IsItemHovered() && Rect.Contains(Mouse);
	for (u32 Offset = 0; Offset <= Count; ++Offset)
	{
		const double Rep = First + Offset;
		const bool IsReversed = Setup.End == EndMode::PingPong && std::fmod(Rep, 2.0) >= 1.0;
		const auto At = [&](float Seq) { return SequenceToX(Target, Runtime, Rep, Seq); };
		const float Lo = std::clamp(static_cast<float>(Near - Rep * Length), 0.0f, Length), Hi = std::clamp(static_cast<float>(Far - Rep * Length), 0.0f, Length);
		const float From = IsReversed ? Length - Hi : Lo, To = IsReversed ? Length - Lo : Hi;
		for (auto Tick = std::lower_bound(Track.Ticks.begin(), Track.Ticks.end(), From, [](const MarkTick& Item, float Moment) { return Item.Sequence < Moment; }); Tick != Track.Ticks.end() && Tick->Sequence <= To; ++Tick)
		{
			const float X = At(Tick->Sequence);
			List->AddLine(ImVec2(X, Rect.Max.y - 0.4f * Rect.GetHeight()), ImVec2(X, Rect.Max.y), TimelineUi::Corner);
			if (IsHovered && std::fabs(Mouse.x - X) <= 3.0f)
			{
				ImGui::SetTooltip("%s", Tick->Name.size() ? Tick->Name.c_str() : "");
				IsHovered = false;
			}
		}
		TimelineUi::Boundary(List, At(Length), Rect);
		u32 Expected = 0;
		float Reach = -1.0f;
		const auto Band = [&Rect](float A, float B) { return ImRect(std::min(A, B), Rect.Min.y, std::max(A, B), Rect.Max.y); };
		const bool IsGhosted = !Offset && HasDragMoved && (Drag == DragKind::BlockMove || Drag == DragKind::BlockBlend) && DragObject == Target.Id && Layer < DragRows && Layer <= MaxActorLayers && DragBlockStarts[Layer].size() == DragBlocks[Layer].size();
		for (u32 Index = 0; IsGhosted && Index < DragBlocks[Layer].size(); ++Index)
		{
			if (Layer != DragLayer || IsDragCopy || static_cast<s32>(Index) != DragBlock)
			{
				const float Early = DragBlockStarts[Layer][Index];
				const ImRect Ghost = Band(At(Early), At(Early + Runtime.BlockDuration(DragBlocks[Layer][Index])));
				List->AddRectFilled(Ghost.Min, Ghost.Max, TimelineUi::WithAlpha(Target.Color, 60u), 2.0f);
				TimelineUi::Hatch(List, Ghost, TimelineUi::Shade);
			}
		}
		for (const BlockSpan& Item : Track.Spans)
		{
			const float Begin = At(Item.Start), Played = At(Item.Start + Item.Play * Item.Loops), Finish = At(Item.Start + Item.Duration);
			const ImRect Box = Band(Begin, Finish), Fade = Band(Begin, At(Item.Start + Item.Overlap));
			const float In = At(Item.Start + std::min(Item.FadeIn, Item.Duration)), Out = At(Item.Start + Item.Duration - std::min(Item.FadeOut, Item.Duration));
			List->AddRectFilled(Box.Min, Box.Max, Fill, 2.0f);
			TimelineUi::Hatch(List, Band(Played, Finish), TimelineUi::Shade);
			if (Reach >= 0.0f && Item.Start > Reach)
			{
				TimelineUi::Dotted(List, At(Reach), Begin, Rect.GetCenter().y, TimelineUi::WithAlpha(Target.Color, 200u));
			}
			Reach = std::max(Reach, Item.Start + Item.Duration);
			for (float Lap = 1.0f; Lap < Item.Loops && Lap < MaxLoops && std::fabs(At(Item.Start + Item.Play) - Begin) >= 3.0f; Lap += 1.0f)
			{
				const float Seam = At(Item.Start + Lap * Item.Play);
				List->AddLine(ImVec2(Seam, Rect.Min.y), ImVec2(Seam, Rect.Min.y + 0.4f * Rect.GetHeight()), TimelineUi::Corner);
			}
			if (Item.Overlap > 0.0f)
			{
				List->AddLine(Fade.Min, Fade.Max, Lighten(Target.Color), 1.5f);
				List->AddLine(ImVec2(Fade.Min.x, Fade.Max.y), ImVec2(Fade.Max.x, Fade.Min.y), Lighten(Target.Color), 1.5f);
			}
			List->AddTriangleFilled(Box.Min, ImVec2(In, Box.Min.y), ImVec2(Box.Min.x, Box.Max.y), TimelineUi::Shade);
			List->AddTriangleFilled(ImVec2(Out, Box.Min.y), ImVec2(Box.Max.x, Box.Min.y), Box.Max, TimelineUi::Shade);
			if ((FocusObject == Target.Id && FocusLayer == Layer && static_cast<s32>(Item.Block) == FocusBlock) || (IsHovered && Box.Contains(Mouse)))
			{
				TimelineUi::Grips(List, Box, In, Out);
			}
			TimelineUi::Boundary(List, Begin, Rect);
			TimelineUi::Label(List, Box, Text, Item.Block < Blocks.size() ? Blocks[Item.Block].Name.c_str() : nullptr);
			if (FocusObject == Target.Id && FocusLayer == Layer && static_cast<s32>(Item.Block) == FocusBlock)
			{
				List->AddRect(Box.Min, Box.Max, Lighten(Target.Color));
			}
			if (Item.Block != Expected)
			{
				TimelineUi::Missing(List, Begin, Rect);
			}
			Expected = Item.Block + 1;
		}
		if (Expected != Blocks.size())
		{
			TimelineUi::Missing(List, At(Length), Rect);
		}
	}
}

void Editor::DrawCameraBlocks(const Player& Core, const Object& Target, const ImRect& Rect)
{
	ImDrawList* List = ImGui::GetWindowDrawList();
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate), Middle = Rect.GetCenter().y;
	const u32 Text = ImGui::GetColorU32(ImGuiCol_Text), Fill = TimelineUi::WithAlpha(Target.Color, Core.IsSilent(Target) ? 60u : 170u);
	const xr_vector<CameraBlock>& Blocks = Target.Camera.Blocks;
	const auto BoxOf = [&](const CameraBlock& Shot) { return ImRect(TimeToX(Target.Span.Start + Shot.Start), Rect.Min.y, TimeToX(Target.Span.Start + Shot.Start + Shot.Length), Rect.Max.y); };
	for (u32 Index = 0; HasDragMoved && Drag == DragKind::BlockMove && DragObject == Target.Id && Index < DragShots.size(); ++Index)
	{
		if (IsDragCopy || static_cast<s32>(Index) != DragShotIndex)
		{
			const ImRect Ghost = BoxOf(DragShots[Index]);
			List->AddRectFilled(Ghost.Min, Ghost.Max, TimelineUi::WithAlpha(Target.Color, 60u));
			TimelineUi::Hatch(List, Ghost, TimelineUi::Shade);
		}
	}
	for (const CameraBlock& Shot : Blocks)
	{
		const ImRect Box = BoxOf(Shot);
		List->AddRectFilled(Box.Min, Box.Max, Fill);
		if (Shot.Keys.empty())
		{
			TimelineUi::Hatch(List, Box, TimelineUi::Shade);
		}
	}
	for (u32 Index = 0; Index < Blocks.size(); ++Index)
	{
		const CameraBlock& Shot = Blocks[Index];
		const BlockCache* Cache = Core.FindCameraBlock(Index);
		const float Start = Target.Span.Start + Shot.Start;
		const ImRect Box = BoxOf(Shot);
		const float Overlap = Index > 0 ? Blocks[Index - 1].Start + Blocks[Index - 1].Length - Shot.Start : -1.0f;
		TimelineUi::Boundary(List, Box.Min.x, Rect);
		TimelineUi::Boundary(List, Box.Max.x, Rect);
		if (Overlap > 0.0f)
		{
			const ImRect Fade(Box.Min.x, Rect.Min.y, TimeToX(Start + Overlap), Rect.Max.y);
			List->AddLine(Fade.Min, Fade.Max, Lighten(Target.Color), 1.5f);
			List->AddLine(ImVec2(Fade.Min.x, Fade.Max.y), ImVec2(Fade.Max.x, Fade.Min.y), Lighten(Target.Color), 1.5f);
		}
		else if (Index > 0 && Overlap > -1e-4f)
		{
			TimelineUi::CutMark(List, Box.Min.x, Rect.Min.y);
		}
		if (FocusObject == Target.Id && static_cast<s32>(Index) == FocusBlock)
		{
			List->AddRect(Box.Min, Box.Max, Lighten(Target.Color));
		}
		if (Shot.Name.size())
		{
			TimelineUi::Label(List, ImRect(Box.Min.x + 3.0f, Rect.Min.y, Box.Max.x, Middle - TimelineUi::PoseRadius), Text, Shot.Name.c_str());
		}
		if (Shot.Attach.Parent.ObjectId || Shot.Attach.LookAt.ObjectId)
		{
			string1024 Caption;
			TimelineUi::LinkLabel(Caption, Core.Data, "", Shot.Attach);
			TimelineUi::Label(List, ImRect(Box.Min.x, Middle + TimelineUi::PoseRadius, Box.Max.x, Rect.Max.y), Cache && Cache->Frames.Problems ? ImGui::GetColorU32(ErrorColor) : Text, Caption);
		}
		for (u32 At = 0; Cache && At < Shot.Keys.size(); ++At)
		{
			const float Local = Cameras::LocalFromPath(Shot, *Cache, Shot.Keys[At].Time);
			const ImVec2 Point(TimeToX(Start + Local), Middle);
			const u32 Color = TimelineUi::WithAlpha(Text, Local > Shot.Length ? 128u : 255u);
			if (std::fabs(Start + Local - Core.Time) <= HalfFrame || (Drag == DragKind::PoseKey && DragBlock == static_cast<s32>(Index) && DragPose == static_cast<s32>(At)))
			{
				List->AddNgonFilled(Point, TimelineUi::PoseRadius, Color, 4);
			}
			List->AddNgon(Point, TimelineUi::PoseRadius, Color, 4, 1.5f);
		}
	}
}

u8 Editor::SubRows(const Object& Target) const
{
	const bool IsDragged = Drag == DragKind::ShakeMove && !IsShakeSlide && DragObject == Target.Id;
	const bool IsMoved = Drag == DragKind::BlockMove && DragObject == Target.Id;
	return Target.Type == ObjectType::Camera ? static_cast<u8>(std::min<u32>(ShakeRows(Target.Camera) + (IsDragged ? 1u : 0u), MaxShakeRows)) : Target.Type == ObjectType::Actor ? static_cast<u8>(std::min<u32>(static_cast<u32>(Target.Actor.Layers.size()) + (IsMoved ? 2u : 1u), MaxActorLayers + 1) + Target.Actor.Poses.size()) : 0;
}

void Editor::DrawSubRow(Player& Core, const Object& Target, u8 Sub, const ImRect& Header, const ImRect& Track)
{
	ImGui::PushID(Sub + 1);
	SizeEdge(Core, { Target.Id, SizeSub, Param::Position, Sub }, Header.Min.x, Header.Max.x, Header.Min.y, Header.Max.y);
	if (Target.Type == ObjectType::Camera)
	{
		DrawShakeRow(Core, Target, Sub, Header, Track);
	}
	else if (Target.Type == ObjectType::Actor)
	{
		const u8 Layers = static_cast<u8>(SubRows(Target) - Target.Actor.Poses.size());
		Sub < Layers ? DrawLayerRow(Core, Target, Sub, Header, Track) : DrawPoseRow(Core, Target, static_cast<u8>(Sub - Layers), Header, Track);
	}
	ImGui::PopID();
}

void Editor::DrawLayerRow(Player& Core, const Object& Target, u8 Layer, const ImRect& Header, const ImRect& Track)
{
	ImDrawList* List = ImGui::GetWindowDrawList();
	const ImGuiIO& Input = ImGui::GetIO();
	const float Size = RowHeight, Caption = ImGui::CalcTextSize("Animation 8 (add)").x + 0.5f * Size;
	const bool IsExtra = Layer > Target.Actor.Layers.size();
	const ActorLayer& Row = RowOf(Target.Actor, IsExtra ? 0 : Layer);
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const bool IsMuted = !IsExtra && Row.IsMuted, IsOff = IsExtra || IsMuted || Core.IsSilent(Target), IsAdd = !IsExtra && Row.Mode == RowMode::Add;
	string32 Name;
	xr_sprintf(Name, IsAdd ? "Animation %u (add)" : "Animation %u", Layer + 1u);
	TimelineUi::Label(List, ImRect(Header.Min.x, Header.Min.y, Header.Min.x + Caption, Header.Min.y + Size), ImGui::GetColorU32(IsOff ? ImGuiCol_TextDisabled : ImGuiCol_Text), Name);
	if (IsExtra)
	{
		return;
	}
	ImGui::SetCursorScreenPos(Header.Min);
	ImGui::InvisibleButton("##RowHead", ImVec2(Caption, Size), ImGuiButtonFlags_MouseButtonRight);
	if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
	{
		ImGui::OpenPopup("##AnimationRow");
	}
	if (ImGui::BeginPopup("##AnimationRow"))
	{
		for (u32 Mode = 0; Mode < 2; ++Mode)
		{
			if (ImGui::MenuItem(TimelineUi::RowModes[Mode], nullptr, IsAdd == !Mode) && IsAdd != !Mode)
			{
				SetRowMode(Core, Target.Id, Layer, !Mode);
			}
		}
		ImGui::Separator();
		if (ImGui::MenuItem("Add row", nullptr, false, Target.Actor.Layers.size() < MaxActorLayers))
		{
			AddLayer(Core, Target.Id);
		}
		if (ImGui::MenuItem("Remove row", nullptr, false, !Target.Actor.Layers.empty()))
		{
			RemoveLayer(Core, Target.Id, Layer);
			ImGui::EndPopup();
			return;
		}
		ImGui::EndPopup();
	}
	ImGui::SetCursorScreenPos(ImVec2(Header.Min.x + Caption, Header.Min.y + 1.0f));
	PartsCombo(Core, Target, Layer, Header.Max.x - 2.0f * Size - Header.Min.x - Caption);
	ImGui::SetCursorScreenPos(ImVec2(Header.Max.x - 2.0f * Size, Header.Min.y + 1.0f));
	ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(IsMuted ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
	const bool ShouldMute = ImGui::Button("M", ImVec2(Size - 2.0f, Size - 2.0f));
	ImGui::PopStyleColor();
	ImGui::SetItemTooltip("Mute");
	ImGui::SetCursorScreenPos(ImVec2(Header.Max.x - Size, Header.Min.y + 1.0f));
	ImGui::BeginDisabled(Target.Actor.Layers.empty());
	const bool ShouldRemove = ImGui::Button("x", ImVec2(Size - 2.0f, Size - 2.0f));
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Remove row");
	if (ShouldMute)
	{
		Object& Item = *Edit(Core, Target.Id, IsMuted ? "Unmute row" : "Mute row");
		RowOf(Item.Actor, Layer).IsMuted = !IsMuted;
		Refit(Core, Item);
	}
	if (ShouldRemove)
	{
		RemoveLayer(Core, Target.Id, Layer);
		return;
	}
	ImGui::SetCursorScreenPos(Track.Min);
	ImGui::InvisibleButton("##SubTrack", Track.GetSize(), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
	CanvasId = ImGui::IsItemActive() ? ImGui::GetItemID() : CanvasId;
	const ImRect Clip(TimeToX(Target.Span.Start), Track.Min.y + 1.0f, TimeToX(ClipEnd(Target)), Track.Max.y - 1.0f);
	List->PushClipRect(Track.Min, Track.Max, true);
	if (Runtime)
	{
		DrawActorBlocks(Target, *Runtime, Clip, Layer, TimelineUi::WithAlpha(Target.Color, IsOff ? 60u : 170u));
	}
	List->PopClipRect();
	DragKind Kind = DragKind::None;
	s32 Hit = -1;
	double Rep = 0.0;
	ClipPart Part = ClipPart::None;
	ImRect Area;
	const s32 Span = Runtime ? BlockHit(Target, *Runtime, Layer, Input.MousePos, Clip, Kind, Hit, Rep, Part, Area) : -1;
	if (Span >= 0 && Drag == DragKind::None && ImGui::IsItemHovered())
	{
		ShowPart(Part, Area, Lighten(Target.Color), 0);
	}
	if (ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		Span >= 0 ? PressBlock(Core, Target, *Runtime, Layer, Kind, Hit, Rep) : StartBox(DragKind::BoxClips);
	}
	if (ImGui::IsItemDeactivated() && ImGui::IsMouseReleased(ImGuiMouseButton_Right) && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Right] < 16.0f)
	{
		OpenRowMenu(Target.Id, false);
		PopupTime = std::max(XToTime(Input.MousePos.x), 0.0f);
		PopupLayer = Layer;
		PopupBlock = Span >= 0 ? static_cast<s32>(Runtime->Track(Layer).Spans[Span].Block) : -1;
		PopupRequest = Span >= 0 && Kind == DragKind::BlockBlend ? "##CutMenu" : PopupRequest;
	}
}

u8 Editor::LayerAt(const Scene& Data, const Object& Target, float Y) const
{
	const auto Found = std::find_if(Rows.begin(), Rows.end(), [&Target](const RowItem& Item) { return !Item.IsFolder && Item.Id == Target.Id; });
	float Top = Found == Rows.end() ? FLT_MAX : RowsTop + Found->Y + Found->Track + SizeOf(Data, Target.Id, SizeSub, Param::Position, 0);
	u8 Layer = 0;
	while (Layer + 1u + Target.Actor.Poses.size() < SubRows(Target) && Y >= Top)
	{
		Top += SizeOf(Data, Target.Id, SizeSub, Param::Position, ++Layer);
	}
	return Layer;
}

void Editor::DrawPoseRow(Player& Core, const Object& Target, u8 Row, const ImRect& Header, const ImRect& Track)
{
	ImDrawList* List = ImGui::GetWindowDrawList();
	const ImGuiIO& Input = ImGui::GetIO();
	const float Size = RowHeight, Caption = ImGui::CalcTextSize("Pose 4 (add)").x + 0.5f * Size, Start = Target.Span.Start, Middle = Track.GetCenter().y;
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	const bool IsAdd = Target.Actor.Poses[Row].Mode == RowMode::Add, IsMuted = Target.Actor.Poses[Row].IsMuted;
	const u32 Text = ImGui::GetColorU32(IsMuted || Core.IsSilent(Target) ? ImGuiCol_TextDisabled : ImGuiCol_Text);
	string32 Name;
	xr_sprintf(Name, IsAdd ? "Pose %u (add)" : "Pose %u", Row + 1u);
	TimelineUi::Label(List, ImRect(Header.Min.x, Header.Min.y, Header.Min.x + Caption, Header.Min.y + Size), Text, Name);
	ImGui::SetCursorScreenPos(Header.Min);
	ImGui::InvisibleButton("##RowHead", ImVec2(Caption, Size), ImGuiButtonFlags_MouseButtonRight);
	bool ShouldOpen = ImGui::IsItemClicked(ImGuiMouseButton_Right);
	PopupTime = ShouldOpen ? Core.Time : PopupTime;
	ImGui::SetCursorScreenPos(ImVec2(Header.Max.x - 2.0f * Size, Header.Min.y + 1.0f));
	ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(IsMuted ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
	const bool ShouldMute = ImGui::Button("M", ImVec2(Size - 2.0f, Size - 2.0f));
	ImGui::PopStyleColor();
	ImGui::SetItemTooltip("Mute");
	ImGui::SetCursorScreenPos(ImVec2(Header.Max.x - Size, Header.Min.y + 1.0f));
	const bool ShouldRemove = ImGui::Button("x", ImVec2(Size - 2.0f, Size - 2.0f));
	ImGui::SetItemTooltip("Remove pose row");
	if (ShouldRemove)
	{
		RemovePoseRow(Core, Target.Id, Row);
		return;
	}
	if (ShouldMute)
	{
		Edit(Core, Target.Id, IsMuted ? "Unmute pose row" : "Mute pose row")->Actor.Poses[Row].IsMuted = !IsMuted;
	}
	ImGui::SetCursorScreenPos(Track.Min);
	ImGui::InvisibleButton("##SubTrack", Track.GetSize(), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
	CanvasId = ImGui::IsItemActive() ? ImGui::GetItemID() : CanvasId;
	const auto IsOwn = [Row](const Lane& Item) { return IsBoneParam(Item.Id) && Item.Layer / MaxPoseBones == Row; };
	const float Earliest = XToTime(Track.Min.x) - Start, Latest = XToTime(Track.Max.x) - Start;
	float Best = TimelineUi::PoseZone, HitTime = 0.0f;
	KeyRef Hit {};
	List->PushClipRect(Track.Min, Track.Max, true);
	List->AddRectFilled(ImVec2(TimeToX(Start), Track.Min.y + 1.0f), ImVec2(TimeToX(ClipEnd(Target)), Track.Max.y - 1.0f), TimelineUi::WithAlpha(Target.Color, IsMuted || Core.IsSilent(Target) ? 60u : 170u), 2.0f);
	for (const Lane& Item : Target.Lanes)
	{
		for (u8 Axis = 0; IsOwn(Item) && Axis < 3; ++Axis)
		{
			const xr_vector<Key>& Keys = Item.Curves[Axis].Keys;
			for (auto Entry = std::lower_bound(Keys.begin(), Keys.end(), Earliest, [](const Key& Other, float Moment) { return Other.Time < Moment; }); Entry != Keys.end() && Entry->Time <= Latest; ++Entry)
			{
				const KeyRef Ref { Target.Id, -1, Item.Id, Item.Layer, Axis, static_cast<u32>(Entry - Keys.begin()) };
				const ImVec2 Point(TimeToX(Start + Entry->Time), Middle);
				const float Gap = std::fabs(Point.x - Input.MousePos.x);
				IsKeySelected(Ref) ? List->AddNgonFilled(Point, TimelineUi::PoseRadius, Text, 4) : List->AddNgon(Point, TimelineUi::PoseRadius, Text, 4, 1.5f);
				if (Gap < Best)
				{
					Best = Gap;
					Hit = Ref;
					HitTime = Entry->Time;
				}
			}
		}
	}
	List->PopClipRect();
	const bool HasHit = ImGui::IsItemHovered() && Best < TimelineUi::PoseZone;
	const auto Group = [&]()
	{
		KeyGroup.assign(1, Hit);
		for (const Lane& Item : Target.Lanes)
		{
			for (u8 Axis = 0; IsOwn(Item) && Axis < 3; ++Axis)
			{
				const KeyRef Ref { Target.Id, -1, Item.Id, Item.Layer, Axis, Curves::KeyAt(Item.Curves[Axis], HitTime, HalfFrame) };
				if (Ref.Index != u32(-1) && !(Ref == Hit))
				{
					KeyGroup.push_back(Ref);
				}
			}
		}
	};
	if (ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left) && !HasHit)
	{
		StartBox(DragKind::BoxClips);
	}
	else if (ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		Group();
		Drag = DragKind::LaneKeys;
		DragObject = Target.Id;
		DragKey = Hit;
		DragKeyStart = *TimelineUi::FindKey(Core.Data, Hit);
		DragKeyTime = LaneMoment(Target, -1, Hit.Id, HitTime);
		DragTime = XToTime(Input.MousePos.x);
		DragOriginY = Input.MousePos.y;
		DragLaneHeight = DragSpan = 0.0f;
		IsDragCopy = Input.KeyCtrl;
		HasDragMoved = false;
		WasPressSelected = IsKeySelected(Hit);
		if (!WasPressSelected)
		{
			SelectKeys(Input.KeyCtrl);
		}
		Focus = FocusKind::Keys;
	}
	if (ImGui::IsItemDeactivated() && ImGui::IsMouseReleased(ImGuiMouseButton_Right) && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Right] < 16.0f)
	{
		PopupTime = std::max(XToTime(Input.MousePos.x), 0.0f);
		ShouldOpen = !HasHit;
		if (HasHit)
		{
			Group();
			if (!IsKeySelected(Hit))
			{
				SelectKeys(false);
			}
			PopupRequest = "##KeyMenu";
		}
	}
	if (ShouldOpen)
	{
		ImGui::OpenPopup("##PoseRow");
	}
	if (!ImGui::BeginPopup("##PoseRow"))
	{
		return;
	}
	for (u32 Mode = 0; Mode < 2; ++Mode)
	{
		if (ImGui::MenuItem(TimelineUi::RowModes[Mode], nullptr, IsAdd == !Mode) && IsAdd != !Mode)
		{
			Edit(Core, Target.Id, "Pose row mode")->Actor.Poses[Row].Mode = Mode ? RowMode::Override : RowMode::Add;
		}
	}
	ImGui::Separator();
	if (ImGui::MenuItem("Key pose", nullptr, false, !Target.Actor.Poses[Row].Bones.empty()))
	{
		Object& Item = *Edit(Core, Target.Id, "Key pose");
		const float Local = std::clamp(PopupTime - Start, 0.0f, Item.Span.Length);
		const xr_vector<PoseBone>& Bones = Item.Actor.Poses[Row].Bones;
		for (u32 Bone = 0; Bone < Bones.size(); ++Bone)
		{
			for (Param Id : { Param::BoneRotation, Param::BonePosition, Param::BoneScale })
			{
				const u8 Layer = PoseLayer(Row, Bone);
				const Fvector Value = Curves::LaneVector(Item.Lanes, Id, Local, Id == Param::BoneRotation ? Bones[Bone].Rotation : Id == Param::BonePosition ? Bones[Bone].Offset : Bones[Bone].Scale, Layer);
				Lane& Line = Curves::EnsureLane(Item.Lanes, Id, Layer);
				for (u8 Axis = 0; Axis < 3; ++Axis)
				{
					Curves::SetKey(Line.Curves[Axis], Local, Value[Axis], HalfFrame);
				}
			}
		}
	}
	if (ImGui::MenuItem("Make animation clip", nullptr, false, !Target.Actor.Poses[Row].Bones.empty()))
	{
		PoseToClip(Core, Target.Id, Row);
		ImGui::EndPopup();
		return;
	}
	if (ImGui::MenuItem("Save as animation..."))
	{
		MotionObject = Target.Id;
		MotionRow = Row;
		NameBuffer[0] = 0;
		Modal = ModalKind::SaveMotion;
	}
	if (ImGui::MenuItem("Remove pose row"))
	{
		RemovePoseRow(Core, Target.Id, Row);
	}
	ImGui::EndPopup();
}

void Editor::DrawShakeRow(Player& Core, const Object& Rig, u8 Row, const ImRect& Header, const ImRect& Track)
{
	ImDrawList* List = ImGui::GetWindowDrawList();
	const ImGuiIO& Input = ImGui::GetIO();
	const float Size = RowHeight;
	const xr_vector<ShakeClip>& Shakes = Rig.Camera.Shakes;
	const bool HasClips = std::any_of(Shakes.begin(), Shakes.end(), [Row](const ShakeClip& Clip) { return Clip.Row == Row; });
	const bool IsMuted = HasClips && std::none_of(Shakes.begin(), Shakes.end(), [Row](const ShakeClip& Clip) { return Clip.Row == Row && Clip.Setup.IsEnabled; });
	const bool IsOverride = (Rig.Camera.OverrideRows >> Row) & 1;
	string32 Name;
	xr_sprintf(Name, IsOverride ? "Shakes %u (override)" : "Shakes %u", Row + 1u);
	TimelineUi::Label(List, ImRect(Header.Min.x, Header.Min.y, Header.Max.x - Size, Header.Min.y + Size), ImGui::GetColorU32(IsMuted || Core.IsSilent(Rig) ? ImGuiCol_TextDisabled : ImGuiCol_Text), Name);
	ImGui::SetCursorScreenPos(Header.Min);
	ImGui::InvisibleButton("##RowHead", ImVec2(std::max(Header.GetWidth() - Size, 1.0f), Size), ImGuiButtonFlags_MouseButtonRight);
	if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
	{
		ImGui::OpenPopup("##RowMode");
	}
	if (ImGui::BeginPopup("##RowMode"))
	{
		for (u32 Mode = 0; Mode < 2; ++Mode)
		{
			if (ImGui::MenuItem(TimelineUi::RowModes[Mode], nullptr, IsOverride == (Mode == 1)) && IsOverride != (Mode == 1))
			{
				u8& Modes = Edit(Core, Rig.Id, "Shake row mode")->Camera.OverrideRows;
				Modes = static_cast<u8>(Modes ^ (1u << Row));
			}
		}
		ImGui::EndPopup();
	}
	ImGui::SetCursorScreenPos(ImVec2(Header.Max.x - Size, Header.Min.y + 1.0f));
	ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(IsMuted ? ImGuiCol_ButtonActive : ImGuiCol_FrameBg));
	if (ImGui::Button("M", ImVec2(Size - 2.0f, Size - 2.0f)) && HasClips)
	{
		for (ShakeClip& Clip : Edit(Core, Rig.Id, IsMuted ? "Unmute shakes" : "Mute shakes")->Camera.Shakes)
		{
			Clip.Setup.IsEnabled = Clip.Row == Row ? IsMuted : Clip.Setup.IsEnabled;
		}
	}
	ImGui::PopStyleColor();
	ImGui::SetItemTooltip("Mute");
	ImGui::SetCursorScreenPos(Track.Min);
	ImGui::InvisibleButton("##SubTrack", Track.GetSize(), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
	CanvasId = ImGui::IsItemActive() ? ImGui::GetItemID() : CanvasId;
	const bool IsHovered = ImGui::IsItemHovered();
	s32 Hit = -1;
	ClipPart Part = ClipPart::None;
	ImRect Area, CrossArea;
	List->PushClipRect(Track.Min, Track.Max, true);
	for (u32 Index = 0; Index < Shakes.size(); ++Index)
	{
		const ShakeClip& Clip = Shakes[Index];
		if (Clip.Row != Row)
		{
			continue;
		}
		const float Begin = Rig.Span.Start + Clip.Start;
		const ImRect Box(TimeToX(Begin), Track.Min.y + 1.0f, TimeToX(Begin + Clip.Length), Track.Max.y - 1.0f);
		const float In = TimeToX(Begin + std::min(Clip.FadeIn, Clip.Length)), Out = TimeToX(Begin + Clip.Length - std::min(Clip.FadeOut, Clip.Length));
		const bool IsOff = !Clip.Setup.IsEnabled || Core.IsSilent(Rig);
		List->AddRectFilled(Box.Min, Box.Max, TimelineUi::WithAlpha(Rig.Color, IsOff ? 60u : 170u), 2.0f);
		List->AddTriangleFilled(Box.Min, ImVec2(In, Box.Min.y), ImVec2(Box.Min.x, Box.Max.y), TimelineUi::Shade);
		List->AddTriangleFilled(ImVec2(Out, Box.Min.y), ImVec2(Box.Max.x, Box.Min.y), Box.Max, TimelineUi::Shade);
		List->AddLine(ImVec2(Box.Min.x, Box.Max.y), ImVec2(In, Box.Min.y), TimelineUi::Corner);
		List->AddLine(ImVec2(Out, Box.Min.y), Box.Max, TimelineUi::Corner);
		if ((FocusObject == Rig.Id && FocusBlock == ShakeBase + static_cast<s32>(Index)) || (IsHovered && Box.Contains(Input.MousePos)))
		{
			TimelineUi::Grips(List, Box, In, Out);
		}
		TimelineUi::Label(List, Box, ImGui::GetColorU32(IsOff ? ImGuiCol_TextDisabled : ImGuiCol_Text), ShakePresets[std::min<u32>(Clip.Setup.Preset, ShakePresetCount - 1)].Name);
		DrawKeyTicks(List, Clip.Lanes, Begin, Box.Max.y - 3.5f, TimelineUi::WithAlpha(ImGui::GetColorU32(ImGuiCol_Text), 200u));
		if (FocusObject == Rig.Id && FocusBlock == ShakeBase + static_cast<s32>(Index))
		{
			List->AddRect(Box.Min, Box.Max, Lighten(Rig.Color), 2.0f);
		}
		ImRect Mine;
		const ClipPart Theirs = IsHovered ? TimelineUi::PartAt({ Box.Min.x, Box.Max.x, Box.Min.y, Box.Max.y, In, Out, Box.Min.x, true, true, true }, Input.MousePos, Mine) : ClipPart::None;
		if (Theirs != ClipPart::None && (Hit < 0 || Part == ClipPart::Body || Theirs != ClipPart::Body))
		{
			Hit = static_cast<s32>(Index);
			Part = Theirs;
			Area = Mine;
		}
	}

	s32 Cross = -1;
	for (u32 Index = 0; Index < Shakes.size(); ++Index)
	{
		const ShakeClip& Early = Shakes[Index];
		for (u32 Other = 0; Early.Row == Row && Other < Shakes.size(); ++Other)
		{
			const ShakeClip& Late = Shakes[Other];
			if (Other == Index || Late.Row != Row || !(Late.Start > Early.Start || (Late.Start == Early.Start && Other > Index)) || !(Late.Start < Early.Start + Early.Length))
			{
				continue;
			}
			const ImRect Box(TimeToX(Rig.Span.Start + Late.Start), Track.Min.y + 1.0f, TimeToX(Rig.Span.Start + std::min(Early.Start + Early.Length, Late.Start + Late.Length)), Track.Max.y - 1.0f);
			List->AddLine(Box.Min, Box.Max, TimelineUi::Corner, 1.5f);
			List->AddLine(ImVec2(Box.Min.x, Box.Max.y), ImVec2(Box.Max.x, Box.Min.y), TimelineUi::Corner, 1.5f);
			const float Zone = std::min(TimelineUi::EdgeZone, Box.GetWidth() / 3.0f);
			if (IsHovered && Box.Contains(Input.MousePos) && Input.MousePos.y - Box.Min.y > TimelineUi::CornerZone && Input.MousePos.x - Box.Min.x > Zone && Box.Max.x - Input.MousePos.x > Zone)
			{
				Cross = static_cast<s32>(Other);
				CrossArea = Box;
			}
		}
	}
	List->PopClipRect();
	Hit = Cross >= 0 ? Cross : Hit;
	Part = Cross >= 0 ? ClipPart::Cross : Part;
	Area = Cross >= 0 ? CrossArea : Area;
	const DragKind Kind = TimelineUi::ShakeParts[static_cast<u32>(Part)];
	if (Hit >= 0 && Drag == DragKind::None)
	{
		ShowPart(Part, Area, Lighten(Rig.Color), 2);
	}
	if (ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left) && Hit < 0)
	{
		StartBox(DragKind::BoxClips);
	}
	else if (ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		Press(Core, Kind, Rig.Id, false);
		IsShakeSlide = Cross >= 0;
		IsDragCopy = Kind == DragKind::ShakeMove && Input.KeyCtrl && !IsShakeSlide;
		DragShakeIndex = DragShakePress = Hit;
		DragShakeStart = Shakes[Hit];
		DragShakes = Shakes;
		DragRow = Row;
		DragOriginY = Input.MousePos.y;
		DragIds.clear();
		CollectSnapTargets(Core, true);
	}
	if (ImGui::IsItemDeactivated() && ImGui::IsMouseReleased(ImGuiMouseButton_Right) && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Right] < 16.0f)
	{
		OpenRowMenu(Rig.Id, false);
		PopupBlock = Hit >= 0 ? ShakeBase + Hit : -1;
		PopupShakeRow = Row;
		IsPopupCross = Cross >= 0;
		PopupTime = std::max(XToTime(Input.MousePos.x), 0.0f);
		PopupRequest = "##ShakeMenu";
	}
	if (Hit >= 0 && (ImGui::IsItemActivated() || (ImGui::IsItemDeactivated() && PopupRequest)))
	{
		FocusObject = Rig.Id;
		FocusBlock = ShakeBase + Hit;
		Focus = FocusKind::Block;
	}
}

void Editor::TrimShake(ShakeClip& Clip, float Trim, float Rate)
{
	const double Ahead = Trim > 0.0f ? Curves::LaneArea(Clip.Lanes, nullptr, Param::ShakeSpeed, Clip.Row, Trim, Clip.Setup.Speed) : Trim * Curves::LaneScalar(Clip.Lanes, Param::ShakeSpeed, Clip.Row, 0.0f, Clip.Setup.Speed);
	TimelineUi::ShiftLanes(Clip.Lanes, 0.0f, -Trim, 0.5f / Rate, true);
	for (Lane& Track : Clip.Lanes)
	{
		for (Curve& Part : Track.Curves)
		{
			Part.Gen.Phase += Trim * Part.Gen.Frequency;
		}
	}
	Clip.Setup.Offset = Clip.Setup.IsStretched ? Clip.Setup.Offset : std::clamp(Clip.Setup.Offset + static_cast<float>(Ahead), -MaxTime, MaxTime);
	Clip.Start += Trim;
	Clip.Length -= Trim;
}

void Editor::MoveShake(Player& Core, const Object& Found)
{
	const ShakeClip& Base = DragShakeStart;
	if (DragShakePress < 0 || static_cast<u32>(DragShakePress) >= DragShakes.size())
	{
		return;
	}
	if (IsDragCopy && DragShakes.size() >= MaxShakeClips)
	{
		SetStatus(true, "The camera already has %u shakes.", MaxShakeClips);
		Drag = DragKind::None;
		return;
	}
	const ImGuiIO& Input = ImGui::GetIO();
	const float Rate = static_cast<float>(Core.Data.Header.FrameRate), Origin = Found.Span.Start, Slack = 0.25f * RowHeight;
	const float A = std::max(SnapSpan(Origin + Base.Start + XToTime(Input.MousePos.x) - DragTime, Base.Length, Input.KeyShift) - Origin, 0.0f), B = A + Base.Length;
	const s32 Home = Base.Row, Aimed = SubAt(Core.Data, Found, Input.MousePos.y);
	const bool IsLeaving = Aimed != Home && SubAt(Core.Data, Found, Input.MousePos.y + (Aimed > Home ? -Slack : Slack)) != Home;
	const s32 Row = !IsShakeSlide && IsLeaving ? Aimed : Home;
	Object& Rig = *Edit(Core, DragObject, IsDragCopy ? "Copy shake" : TimelineUi::ShakeLabels[0]);
	xr_vector<ShakeClip>& Shakes = Rig.Camera.Shakes;
	Shakes = DragShakes;
	SelectedKeys = DragKeyPrevious;
	s32 Moved = DragShakePress;
	if (IsDragCopy)
	{
		Shakes.push_back(Base);
		Moved = static_cast<s32>(Shakes.size() - 1);
	}
	SpanScratch.clear();
	for (s32 Index = 0; Index < static_cast<s32>(Shakes.size()); ++Index)
	{
		SpanScratch.push_back(Index == Moved || Shakes[Index].Row != Row ? Cameras::ClipSpan { -1.0f, 0.0f } : Cameras::ClipSpan { Shakes[Index].Start, Shakes[Index].Length });
	}
	TimelineUi::PlanOverwrite(SpanScratch, A, B, MinLength, CoverPlan);
	bool IsCut = false;
	for (s32 Index = static_cast<s32>(CoverPlan.size()); Index-- > 0;)
	{
		const Cover Action = static_cast<Cover>(CoverPlan[Index]);
		if (Action == Cover::Keep)
		{
			continue;
		}
		const float C = Shakes[Index].Start, D = C + Shakes[Index].Length;
		const bool HasHead = (Action == Cover::Split || Action == Cover::CutTail) && A - C >= MinLength;
		const bool HasTail = (Action == Cover::Split || Action == Cover::CutHead) && D - B >= MinLength && (!HasHead || Shakes.size() < MaxShakeClips);
		IsCut = true;
		if (HasTail)
		{
			ShakeClip Tail = Shakes[Index];
			TrimShake(Tail, B - C, Rate);
			Tail.FadeIn = 0.0f;
			Tail.FadeOut = std::min(Tail.FadeOut, Tail.Length);
			if (HasHead)
			{
				Shakes.push_back(std::move(Tail));
			}
			else
			{
				Shakes[Index] = std::move(Tail);
			}
		}
		if (HasHead)
		{
			Shakes[Index].Length = A - C;
			Shakes[Index].FadeIn = std::min(Shakes[Index].FadeIn, A - C);
			Shakes[Index].FadeOut = 0.0f;
		}
		if (!HasHead && !HasTail)
		{
			Shakes.erase(Shakes.begin() + Index);
			Moved -= Moved > Index ? 1 : 0;
		}
	}
	if (IsCut)
	{
		std::erase_if(SelectedKeys, [&Rig](const KeyRef& Ref) { return Ref.ObjectId == Rig.Id; });
	}
	Shakes[Moved].Start = A;
	SetShakeRow(Shakes[Moved], static_cast<u8>(Row));
	if (!Cameras::IsShakeFree(Rig.Camera, static_cast<u32>(Moved), static_cast<u8>(Row), A, Base.Length, MaxShakeClips))
	{
		Shakes = DragShakes;
		SelectedKeys = DragKeyPrevious;
		Moved = DragShakePress;
	}
	DragShakeIndex = Moved;
	FocusBlock = ShakeBase + Moved;
}

void Editor::DragShake(Player& Core)
{
	const Object* Found = Find(Core.Data, DragObject);
	if (!Found || DragShakeIndex < 0 || DragShakeIndex >= static_cast<s32>(Found->Camera.Shakes.size()))
	{
		return;
	}
	if (Drag == DragKind::ShakeMove)
	{
		MoveShake(Core, *Found);
		return;
	}
	const ImGuiIO& Input = ImGui::GetIO();
	const ShakeClip& Base = DragShakeStart;
	const float Rate = static_cast<float>(Core.Data.Header.FrameRate), Origin = Found->Span.Start, Delta = XToTime(Input.MousePos.x) - DragTime, End = Base.Start + Base.Length;
	const bool IsFree = Input.KeyShift;
	Object& Rig = *Edit(Core, DragObject, TimelineUi::ShakeLabels[static_cast<u32>(Drag) - static_cast<u32>(DragKind::ShakeMove)]);
	ShakeClip& Clip = Rig.Camera.Shakes[DragShakeIndex];
	if (Drag >= DragKind::ShakeFadeIn)
	{
		const float Grown = Drag == DragKind::ShakeFadeIn ? Base.FadeIn + Delta : Base.FadeOut - Delta;
		(Drag == DragKind::ShakeFadeIn ? Clip.FadeIn : Clip.FadeOut) = std::clamp(IsFree || !Settings.IsSnapping ? Grown : std::round(Grown * Rate) / Rate, 0.0f, Base.Length);
		return;
	}
	const auto Place = [&](float Shift)
	{
		const float Start = Drag == DragKind::ShakeStart ? std::clamp(SnapTime(Origin + Base.Start + Shift, IsFree) - Origin, 0.0f, End - MinLength) : Base.Start;
		const float Length = Drag == DragKind::ShakeStart ? End - Start : std::clamp(SnapTime(Origin + End + Shift, IsFree) - Origin - Start, MinLength, MaxTime - Start);
		if (!Cameras::IsShakeFree(Rig.Camera, static_cast<u32>(DragShakeIndex), Base.Row, Start, Length, MaxShakeClips))
		{
			return false;
		}
		Clip = Base;
		if (Drag == DragKind::ShakeStart)
		{
			TrimShake(Clip, Start - Base.Start, Rate);
		}
		Clip.Length = Length;
		return true;
	};
	DragLimit = -1.0f;
	if (Place(Delta))
	{
		return;
	}
	float Lo = 0.0f, Hi = Delta;
	for (u32 Step = 0; Step < 12; ++Step)
	{
		const float Mid = 0.5f * (Lo + Hi);
		(Place(Mid) ? Lo : Hi) = Mid;
	}
	Place(Lo);
	DragLimit = Origin + Clip.Start + (Drag == DragKind::ShakeStart ? 0.0f : Clip.Length);
}

void Editor::DrawShakeMenu(Player& Core)
{
	if (!ImGui::BeginPopup("##ShakeMenu"))
	{
		return;
	}
	const Object* Rig = Find(Core.Data, PopupRow);
	const bool IsClip = Rig && Rig->Type == ObjectType::Camera && PopupBlock >= ShakeBase && HasOwner(*Rig, PopupBlock);
	const u32 Index = static_cast<u32>(std::max(PopupBlock - ShakeBase, 0));
	if (IsClip && IsPopupCross)
	{
		const ShakeClip& Late = Rig->Camera.Shakes[Index];
		float Joint = -1.0f;
		for (u32 Other = 0; Other < Rig->Camera.Shakes.size(); ++Other)
		{
			const ShakeClip& Early = Rig->Camera.Shakes[Other];
			const bool IsEarlier = Early.Start < Late.Start || (Early.Start == Late.Start && Other < Index);
			Joint = Other != Index && Early.Row == Late.Row && IsEarlier && Early.Start + Early.Length > Late.Start ? std::max(Joint, Early.Start + Early.Length) : Joint;
		}
		if (ImGui::BeginMenu("Ease"))
		{
			for (u32 Mode = 0; Mode < 4; ++Mode)
			{
				if (ImGui::MenuItem(TimelineUi::ModeNames[Mode], nullptr, Late.BlendEase == static_cast<Interp>(Mode)))
				{
					Edit(Core, PopupRow, "Crossfade ease")->Camera.Shakes[Index].BlendEase = static_cast<Interp>(Mode);
				}
			}
			ImGui::EndMenu();
		}
		const bool IsRoom = Joint >= 0.0f && Joint + Late.Length <= MaxTime && Cameras::IsShakeFree(Rig->Camera, Index, Late.Row, Joint, Late.Length, MaxShakeClips);
		if (ImGui::MenuItem("Remove crossfade", nullptr, false, IsRoom))
		{
			Edit(Core, PopupRow, "Remove crossfade")->Camera.Shakes[Index].Start = Joint;
		}
		ImGui::EndPopup();
		return;
	}
	if (IsClip && ImGui::BeginMenu("Preset"))

	{
		const u8 Current = Rig->Camera.Shakes[Index].Setup.Preset;
		for (u32 Preset = 0; Preset < ShakePresetCount; ++Preset)
		{
			if (ImGui::MenuItem(ShakePresets[Preset].Name, nullptr, Current == Preset))
			{
				Edit(Core, PopupRow, "Shake preset")->Camera.Shakes[Index].Setup.Preset = static_cast<u8>(Preset);
			}
		}
		ImGui::EndMenu();
	}
	if (IsClip && ImGui::MenuItem("Stretch", nullptr, Rig->Camera.Shakes[Index].Setup.IsStretched))
	{
		bool& IsStretched = Edit(Core, PopupRow, "Stretch")->Camera.Shakes[Index].Setup.IsStretched;
		IsStretched = !IsStretched;
	}
	if (IsClip && ImGui::MenuItem("Remove", "Delete"))
	{
		RemoveBlock(Core, PopupRow, PopupBlock);
	}
	if (!IsClip && Rig && ImGui::MenuItem("Add shake here"))
	{
		AddShake(Core, PopupTime, PopupShakeRow);
	}
	ImGui::EndPopup();
}

s32 Editor::CameraHit(const Player& Core, const Object& Target, const ImVec2& Mouse, float Top, float Bottom, DragKind& Kind, s32& Pose, ClipPart& Part, ImRect& Area) const
{
	const float Middle = 0.5f * (Top + Bottom), Reach = TimelineUi::PoseRadius + 2.0f;
	const xr_vector<CameraBlock>& Blocks = Target.Camera.Blocks;
	float Best = TimelineUi::PoseZone;
	s32 Found = -1, Keyed = -1;
	ImVec2 Spot;
	Pose = -1;
	Part = ClipPart::None;
	for (u32 Index = 0; Index < Blocks.size(); ++Index)
	{
		const CameraBlock& Shot = Blocks[Index];
		const BlockCache* Cache = Core.FindCameraBlock(Index);
		const float Start = Target.Span.Start + Shot.Start;
		const float Begin = TimeToX(Start), End = TimeToX(Start + Shot.Length);
		const float Seam = Index > 0 ? TimeToX(Target.Span.Start + Blocks[Index - 1].Start + Blocks[Index - 1].Length) : Begin - 2.0f;
		const float Next = Index + 1 < Blocks.size() ? TimeToX(Target.Span.Start + Blocks[Index + 1].Start) : End + 2.0f;
		for (u32 At = 0; Cache && At < Shot.Keys.size(); ++At)
		{
			const ImVec2 Point(TimeToX(Start + Cameras::LocalFromPath(Shot, *Cache, Shot.Keys[At].Time)), Middle);
			const float Distance = std::hypot(Mouse.x - Point.x, Mouse.y - Point.y);
			if (Distance <= Best)
			{
				Best = Distance;
				Keyed = static_cast<s32>(Index);
				Pose = static_cast<s32>(At);
				Spot = Point;
			}
		}
		ImRect Mine;
		ClipPart Theirs = TimelineUi::PartAt({ Begin, End, Top, Bottom, Begin, End, Seam > Begin + 1.0f ? Seam : Begin, Seam < Begin - 1.0f, Next > End + 1.0f, false }, Mouse, Mine);
		if (Index > 0 && std::fabs(Seam - Begin) <= 1.0f && Mouse.x >= Begin && Mouse.x - Begin <= TimelineUi::CornerZone && Mouse.y >= Top && Mouse.y - Top <= TimelineUi::CornerZone)
		{
			Theirs = ClipPart::Cross;
			Mine = ImRect(Begin - TimelineUi::CornerZone, Top, Begin + TimelineUi::CornerZone, Top + TimelineUi::CornerZone);
		}
		if (Theirs != ClipPart::None && !(Part == ClipPart::End && (Theirs == ClipPart::Cross || Theirs == ClipPart::Body)))
		{
			Part = Theirs;
			Area = Mine;
			Found = static_cast<s32>(Index);
		}
	}
	const bool IsMiddle = std::fabs(Mouse.y - Middle) <= (Bottom - Top) / 6.0f;
	if (Pose >= 0 && (IsMiddle || Part == ClipPart::None || Part == ClipPart::Body || Part == ClipPart::Cross))
	{
		Part = ClipPart::Key;
		Found = Keyed;
		Area = ImRect(Spot.x - Reach, Spot.y - Reach, Spot.x + Reach, Spot.y + Reach);
	}
	Pose = Part == ClipPart::Key ? Pose : -1;
	Kind = TimelineUi::ShotParts[static_cast<u32>(Part)];
	return Found;
}

void Editor::MoveShot(Player& Core, const Object& Target)
{
	const CameraBlock& Base = DragShot;
	if (DragShotIndex < 0 || static_cast<u32>(DragShotIndex) >= DragShots.size() || (IsDragCopy && DragShots.size() >= MaxBlocks))
	{
		return;
	}
	const ImGuiIO& Input = ImGui::GetIO();
	const float Delta = XToTime(Input.MousePos.x) - DragTime;
	const float A = std::clamp(SnapSpan(Target.Span.Start + Base.Start + Delta, Base.Length, Input.KeyShift) - Target.Span.Start, 0.0f, std::max(MaxTime - Base.Length, 0.0f)), B = A + Base.Length;
	Object& Rig = *Edit(Core, Target.Id, TimelineUi::ShotLabels[IsDragCopy ? 7u : 0u]);
	xr_vector<CameraBlock>& Blocks = Rig.Camera.Blocks;
	s32 Moved = DragShotIndex;
	const auto Restore = [&]
	{
		Blocks = DragShots;
		Rig.Camera.Shakes = DragShakes;
		Pins = DragPins;
		SelectedKeys = DragKeyPrevious;
		PickFocusBlock = DragPickFocus;
		Moved = DragShotIndex;
	};
	const auto Renumber = [&](s32 From, s32 Step)
	{
		for (ShakePin& Pin : Pins)
		{
			Pin.Block += Pin.Block >= From ? Step : 0;
		}
		Moved += Moved >= From ? Step : 0;
	};
	Restore();
	if (IsDragCopy)
	{
		const u32 Pinned = static_cast<u32>(std::min(Pins.size(), Rig.Camera.Shakes.size()));
		Blocks.insert(Blocks.begin() + Moved + 1, Base);
		for (u32 Pin = 0; Pin < Pinned; ++Pin)
		{
			Pins[Pin].Block += Pins[Pin].Block > Moved ? 1 : 0;
			if (Pins[Pin].Block == Moved && Rig.Camera.Shakes.size() < MaxShakeClips)
			{
				ShakePin Copy = Pins[Pin];
				Copy.Block = Moved + 1;
				Pins.push_back(Copy);
				Rig.Camera.Shakes.push_back(Rig.Camera.Shakes[Pin]);
			}
		}
		++Moved;
	}
	SpanScratch.clear();
	for (s32 Index = 0; Index < static_cast<s32>(Blocks.size()); ++Index)
	{
		SpanScratch.push_back(Index == Moved ? Cameras::ClipSpan { -1.0f, 0.0f } : Cameras::ClipSpan { Blocks[Index].Start, Blocks[Index].Length });
	}
	TimelineUi::PlanOverwrite(SpanScratch, A, B, MinLength, CoverPlan);
	bool IsCut = false;
	for (s32 Index = static_cast<s32>(CoverPlan.size()); Index-- > 0;)
	{
		const Cover Action = static_cast<Cover>(CoverPlan[Index]);
		if (Action == Cover::Keep)
		{
			continue;
		}
		const float C = Blocks[Index].Start, D = C + Blocks[Index].Length;
		const bool HasHead = (Action == Cover::Split || Action == Cover::CutTail) && A - C >= MinLength;
		const bool HasTail = (Action == Cover::Split || Action == Cover::CutHead) && D - B >= MinLength && (!HasHead || Blocks.size() < MaxBlocks);
		IsCut = true;
		if (HasTail)
		{
			CameraBlock Tail = Blocks[Index];
			TrimHead(Tail, Blocks[Index], B - C);
			if (HasHead)
			{
				Renumber(Index + 1, 1);
				Blocks.insert(Blocks.begin() + Index + 1, std::move(Tail));
			}
			else
			{
				for (ShakePin& Pin : Pins)
				{
					Pin.Offset -= Pin.Block == Index ? B - C : 0.0f;
				}
				Blocks[Index] = std::move(Tail);
			}
		}
		if (HasHead)
		{
			Blocks[Index].Length = A - C;
		}
		if (!HasHead && !HasTail)
		{
			for (ShakePin& Pin : Pins)
			{
				Pin.Offset += Pin.Block == Index ? C : 0.0f;
				Pin.Block = Pin.Block == Index ? -1 : Pin.Block;
			}
			Renumber(Index + 1, -1);
			Blocks.erase(Blocks.begin() + Index);
		}
	}
	if (IsCut)
	{
		std::erase_if(SelectedKeys, [&Rig](const KeyRef& Ref) { return Ref.ObjectId == Rig.Id; });
		PickFocusBlock = -1;
	}
	Blocks[Moved].Start = A;
	Moved = SortShot(Rig, Moved);
	SpanScratch.clear();
	for (const CameraBlock& Item : Blocks)
	{
		SpanScratch.push_back({ Item.Start, Item.Length });
	}
	if (!Cameras::IsTrackValid(SpanScratch))
	{
		Restore();
	}
	DragBlock = FocusBlock = Moved;
	FollowShakes(Rig);
}

void Editor::DragCameraBlock(Player& Core, const Object& Target)
{
	if (Drag == DragKind::BlockMove)
	{
		MoveShot(Core, Target);
		return;
	}
	const u32 Count = static_cast<u32>(Target.Camera.Blocks.size()), Index = static_cast<u32>(DragBlock);
	const BlockCache* Cache = Core.FindCameraBlock(Index);
	const CameraBlock& Base = DragShot;
	if (Index >= Count || (Drag == DragKind::PoseKey && (!Cache || static_cast<u32>(DragPose) >= Base.Keys.size())))
	{
		return;
	}
	const ImGuiIO& Input = ImGui::GetIO();
	const float Delta = XToTime(Input.MousePos.x) - DragTime;
	Object& Rig = *Edit(Core, Target.Id, TimelineUi::ShotLabels[static_cast<u32>(Drag) - static_cast<u32>(DragKind::BlockMove)]);
	xr_vector<CameraBlock>& Blocks = Rig.Camera.Blocks;
	CameraBlock& Shot = Blocks[DragBlock];
	Shot = Base;
	const float Start = Rig.Span.Start + Base.Start;
	if (Drag == DragKind::BlockIn)
	{
		const float End = Base.Start + Base.Length;
		const float Picked = FitClip(Rig.Camera, DragBlock, std::clamp(SnapTime(Start + Delta, Input.KeyShift) - Rig.Span.Start, 0.0f, End - MinLength), End, 1);
		TrimHead(Shot, Base, (Picked < 0.0f ? Base.Start : Picked) - Base.Start);
		return;
	}
	if (Drag == DragKind::PoseKey)
	{
		const u32 At = static_cast<u32>(DragPose);
		const float Path = Cameras::PathTime(Base, SnapTime(Start + Cameras::LocalFromPath(Base, Base.Keys[At].Time) + Delta, Input.KeyShift) - Start);
		const float Lo = At > 0 ? Base.Keys[At - 1].Time + 1e-3f : 0.0f, Hi = At + 1 < Base.Keys.size() ? Base.Keys[At + 1].Time - 1e-3f : MaxTime;
		Cameras::MovePoseAt(Shot, Base.Keys[At].Time, Lo <= Hi ? std::clamp(Path, Lo, Hi) : Base.Keys[At].Time, 1e-4f);
		return;
	}
	float Last = Base.Keys.empty() ? 0.0f : Base.Keys.back().Time;
	for (const Lane& Track : Base.Lanes)
	{
		for (const Curve& Part : Track.Curves)
		{
			Last = Part.Keys.empty() ? Last : std::max(Last, Part.Keys.back().Time);
		}
	}
	const float Limit = Drag == DragKind::BlockStretch && Last > 0.0f ? std::min(MaxTime, Base.Length * MaxTime / Last) : MaxTime;
	const float Wish = std::clamp(SnapTime(Start + Base.Length + Delta, Input.KeyShift) - Rig.Span.Start, Base.Start + MinLength, Base.Start + std::max(Limit, MinLength));
	const float Picked = FitClip(Rig.Camera, DragBlock, Base.Start, Wish, 2);
	Shot.Length = (Picked < 0.0f ? Base.Start + Base.Length : Picked) - Base.Start;
	const float Factor = Shot.Length / Base.Length;
	FollowShakes(Rig, Drag == DragKind::BlockStretch ? DragBlock : -1, Factor);
	if (Drag != DragKind::BlockStretch)
	{
		return;
	}
	for (Lane& Track : Shot.Lanes)
	{
		for (Curve& Part : Track.Curves)
		{
			for (Key& Item : Part.Keys)
			{
				Item.Time = std::min(Item.Time * Factor, MaxTime);
				Item.InTime = std::max(Item.InTime * Factor, -MaxTime);
				Item.OutTime = std::min(Item.OutTime * Factor, MaxTime);
			}
		}
	}
	Cameras::Rebuild(Shot);
}

bool Editor::CutShot(Player& Core, s32 Block, float From, float To, bool ShouldApply)
{
	const Object* Rig = FindCamera(Core.Data);
	const s32 Count = Rig ? static_cast<s32>(Rig->Camera.Blocks.size()) : 0;
	const BlockCache* Cache = Block >= 0 && Block < Count ? Core.FindCameraBlock(static_cast<u32>(Block)) : nullptr;
	const float Length = Cache ? Rig->Camera.Blocks[Block].Length : 0.0f;
	const bool IsSplit = From == To;
	From = IsSplit || From >= MinLength ? From : 0.0f;
	To = IsSplit || To <= Length - MinLength ? To : Length;
	const bool HasHead = From > 0.0f, HasTail = To < Length;
	const bool IsValid = Cache && Cache->Keys == Rig->Camera.Blocks[Block].Keys && (!IsSplit || (From >= MinLength && From <= Length - MinLength)) && (!HasHead || !HasTail || Count < static_cast<s32>(MaxBlocks));
	if (!IsValid || !ShouldApply)
	{
		return IsValid;
	}
	xr_vector<CameraBlock>& Blocks = Edit(Core, Rig->Id, "Split clip")->Camera.Blocks;
	CameraBlock& First = Blocks[Block];
	CameraBlock Second = First;
	const float PathFrom = Cameras::PathTime(First, *Cache, From), PathTo = Cameras::PathTime(First, *Cache, To);
	for (u32 Index = 0; Index < First.Lanes.size(); ++Index)
	{
		const bool IsPath = First.Lanes[Index].Id <= Param::Rotation;
		const float Lo = IsPath ? PathFrom : From, Hi = IsPath ? PathTo : To;
		for (u32 Axis = 0; Axis < Info(First.Lanes[Index].Id).Components; ++Axis)
		{
			Curve& Head = First.Lanes[Index].Curves[Axis];
			Curve& Tail = Second.Lanes[Index].Curves[Axis];
			Tail.Gen.Phase += To * Tail.Gen.Frequency;
			if (!Head.Keys.empty())
			{
				TimelineUi::CutKeys(Head.Keys, Tail.Keys, Key { Lo, Curves::Value(Head, Lo, 0.0f) }, Key { Hi, Curves::Value(Head, Hi, 0.0f) });
			}
		}
	}
	Cameras::Rebuild(First);
	Cameras::Rebuild(Second);
	Second.Length = Length - To;
	Second.Start = First.Start + To;
	First.Length = From;
	if (!HasHead && !HasTail)
	{
		Blocks.erase(Blocks.begin() + Block);
	}
	else if (!HasHead)
	{
		First = std::move(Second);
	}
	else if (HasTail)
	{
		Blocks.insert(Blocks.begin() + Block + 1, std::move(Second));
	}
	SelectedKeys.clear();
	return true;
}

void Editor::DrawPoseMenu(Player& Core)
{
	if (!ImGui::BeginPopup("##PoseMenu"))
	{
		return;
	}
	const Object* Rig = Find(Core.Data, PopupRow);
	const bool IsValid = Rig && Rig->Type == ObjectType::Camera && PopupBlock >= 0 && PopupBlock < static_cast<s32>(Rig->Camera.Blocks.size()) && PopupPose >= 0 && PopupPose < static_cast<s32>(Rig->Camera.Blocks[PopupBlock].Keys.size());
	const auto Target = [this, &Core](const char* Label) -> CameraBlock& { return Edit(Core, PopupRow, Label)->Camera.Blocks[PopupBlock]; };
	if (IsValid && ImGui::MenuItem("Update from view", nullptr, false, Core.View() != ViewSource::Global))
	{
		PoseKey Pose;
		ViewPose(Core, false, Pose);
		if (const BlockCache* Cache = Core.FindCameraBlock(static_cast<u32>(PopupBlock)))
		{
			ToSpace(Cache->Frames, Pose);
		}
		CameraBlock& Item = Target("Update from view");
		Cameras::SetPoseAt(Item, Item.Keys[PopupPose].Time, Pose.Position, Pose.Rotation, Item.Keys[PopupPose].Mode, 1e-4f);
	}
	if (IsValid && ImGui::BeginMenu("Interpolation"))
	{
		const Interp Current = Rig->Camera.Blocks[PopupBlock].Keys[PopupPose].Mode;
		for (u32 Mode = 0; Mode < 6; ++Mode)
		{
			if (ImGui::MenuItem(TimelineUi::ModeNames[Mode], nullptr, Current == static_cast<Interp>(Mode)))
			{
				CameraBlock& Item = Target("Interpolation");
				Cameras::ModeAt(Item, Item.Keys[PopupPose].Time, static_cast<Interp>(Mode), 1e-4f);
			}
		}
		ImGui::EndMenu();
	}
	if (IsValid && ImGui::MenuItem("Remove", "Delete", false, Rig->Camera.Blocks[PopupBlock].Keys.size() > 1))
	{
		RemovePose(Core, PopupBlock, static_cast<u32>(PopupPose));
	}
	ImGui::EndPopup();
}

void Editor::DrawTimelinePopups(Player& Core)
{
	if (PopupRequest)
	{
		ImGui::OpenPopup(PopupRequest);
		PopupRequest = nullptr;
	}
	shared_str Source, Name;
	if (ImGui::BeginPopup("##RowMenu"))
	{
		const Object* Target = IsPopupFolder ? nullptr : Find(Core.Data, PopupRow);
		const bool IsLayered = Target && Target->Type == ObjectType::Actor && PopupLayer <= Target->Actor.Layers.size();
		const bool HasBlock = IsLayered && PopupBlock >= 0 && PopupBlock < static_cast<s32>(BlocksOf(Target->Actor, PopupLayer).size());
		ChooserOwners.assign(1, PopupRow);
		if (IsLayered && !HasBlock && IsPopupChoosing)
		{
			if (ChooserList(Core, ChooserKind::Animation, ChooserOwners, Source, Name))
			{
				AddAnimationAt(Core, PopupRow, PopupLayer, PopupTime, Source, Name);
				ImGui::CloseCurrentPopup();
			}
			ImGui::EndPopup();
			return;
		}
		if (IsLayered && !HasBlock && ImGui::Selectable("Add animation here...", false, ImGuiSelectableFlags_NoAutoClosePopups))
		{
			IsPopupChoosing = IsChooserFresh = true;
		}
		if (HasBlock && IsPopupChoosing)
		{
			if (ChooserList(Core, ChooserKind::Animation, ChooserOwners, Source, Name))
			{
				Object& Item = *Edit(Core, PopupRow, "Replace animation");
				ActorBlock& Block = BlocksOf(Item.Actor, PopupLayer)[PopupBlock];
				Block.Source = Source;
				Block.Name = Name;
				Block.In = Block.Out = 0.0f;
				Refit(Core, Item);
				ImGui::CloseCurrentPopup();
			}
			ImGui::EndPopup();
			return;
		}
		if (HasBlock && IsClipBlock(BlocksOf(Target->Actor, PopupLayer)[PopupBlock].Source) && ImGui::MenuItem("Edit animation"))
		{
			OpenAnimator(Core, PopupRow, FindClip(Target->Actor.Clips, BlocksOf(Target->Actor, PopupLayer)[PopupBlock].Name));
		}
		if (HasBlock && !IsClipBlock(BlocksOf(Target->Actor, PopupLayer)[PopupBlock].Source) && ImGui::MenuItem("Import as keys"))
		{
			ImportClip(Core, PopupRow, PopupLayer, static_cast<u32>(PopupBlock));
		}
		ImGui::SetItemTooltip("Turns this animation into an editable clip of the Animator and makes the block play it");
		if (HasBlock && ImGui::Selectable("Replace animation...", false, ImGuiSelectableFlags_NoAutoClosePopups))
		{
			IsPopupChoosing = IsChooserFresh = true;
		}
		if (HasBlock && ImGui::MenuItem("Split at playhead", nullptr, false, SplitBlock(Core, PopupRow, PopupBlock, false, PopupLayer)))
		{
			SplitBlock(Core, PopupRow, PopupBlock, true, PopupLayer);
		}
		if (HasBlock && ImGui::BeginMenu("Ease"))
		{
			const Interp Current = BlocksOf(Target->Actor, PopupLayer)[PopupBlock].BlendEase;
			for (u32 Mode = 0; Mode < 4; ++Mode)
			{
				if (ImGui::MenuItem(TimelineUi::ModeNames[Mode], nullptr, Current == static_cast<Interp>(Mode)))
				{
					BlocksOf(Edit(Core, PopupRow, "Ease")->Actor, PopupLayer)[PopupBlock].BlendEase = static_cast<Interp>(Mode);
				}
			}
			ImGui::EndMenu();
		}
		if (HasBlock && ImGui::MenuItem("Remove"))
		{
			RemoveBlock(Core, PopupRow, PopupBlock, PopupLayer);
		}
		const bool HasShot = Target && Target->Type == ObjectType::Camera && PopupBlock >= 0 && PopupBlock < static_cast<s32>(Target->Camera.Blocks.size());
		if (HasShot && ImGui::MenuItem("Key from view here", nullptr, false, Core.View() != ViewSource::Global))
		{
			KeyFromView(Core, PopupTime, false);
		}
		const float Local = HasShot ? Core.Time - TargetStart(*Target, PopupBlock) : 0.0f;
		if (HasShot && ImGui::MenuItem("Split at playhead", nullptr, false, CutShot(Core, PopupBlock, Local, Local, false)))
		{
			CutShot(Core, PopupBlock, Local, Local, true);
		}
		if (HasShot && ImGui::MenuItem("Even speed", nullptr, false, Target->Camera.Blocks[PopupBlock].Keys.size() > 2))
		{
			EvenSpeed(Core, PopupBlock);
		}
		if (HasShot && ImGui::MenuItem("Remove clip"))
		{
			RemoveBlock(Core, PopupRow, PopupBlock);
		}
		if (Target && Target->Type == ObjectType::Camera && ImGui::MenuItem("Add shake here"))
		{
			AddShake(Core, PopupTime, -1);
		}
		if (IsLayered && ImGui::MenuItem("Add row", nullptr, false, Target->Actor.Layers.size() < MaxActorLayers))
		{
			AddLayer(Core, PopupRow);
		}
		if (HasBlock || HasShot)
		{
			ImGui::Separator();
		}
		if (ImGui::MenuItem("Rename", "F2"))
		{
			StartRename(Core.Data, PopupRow, IsPopupFolder);
		}
		if (Target && ImGui::MenuItem("Colour..."))
		{
			PopupRequest = "##Colour";
		}
		if (Target && ImGui::MenuItem("Mute", nullptr, Target->IsMuted))
		{
			SetMuted(Core, PopupRow, !Target->IsMuted);
		}
		if (Target && Target->Type == ObjectType::Actor && (Target->Actor.IsRelative || !HasClips(Target->Actor)) && ImGui::MenuItem("Reset length"))
		{
			ResetLength(Core);
		}
		if (ImGui::MenuItem("Delete", "Delete", false, !(Target && Target->Type == ObjectType::Camera && Selected.size() < 2)))
		{
			Focus = FocusKind::Objects;
			DeleteSelection(Core);
		}
		ImGui::EndPopup();
	}
	if (ImGui::BeginPopup("##Colour"))
	{
		const Object* Target = Find(Core.Data, PopupRow);
		float Color[3] = {};
		for (u32 Channel = 0; Target && Channel < 3; ++Channel)
		{
			Color[Channel] = static_cast<float>((Target->Color >> (Channel * 8)) & 255u) / 255.0f;
		}
		if (Target && ImGui::ColorPicker3("##Pick", Color, ImGuiColorEditFlags_NoSidePreview | ImGuiColorEditFlags_NoAlpha))
		{
			const auto Byte = [](float Value) { return static_cast<u32>(std::clamp(Value, 0.0f, 1.0f) * 255.0f + 0.5f); };
			Edit(Core, PopupRow, "Colour")->Color = Rgb(Byte(Color[0]), Byte(Color[1]), Byte(Color[2]));
		}
		ImGui::EndPopup();
	}
	if (ImGui::BeginPopup("##Add"))
	{
		LoadPresets();
		ChooserOwners.clear();
		if (IsPopupChoosing && ChooserList(Core, PopupKind, ChooserOwners, Source, Name))
		{
			Fvector Position, Rotation;
			SpawnPose(Position, Rotation);
			PopupKind == ChooserKind::Preset ? AddActorFromPreset(Core, Source.c_str(), PopupTime, Position, Rotation) : (void)AddObject(Core, AddedType(PopupKind), Source, PopupTime, Position, Rotation);
			ImGui::CloseCurrentPopup();
		}
		for (u32 Index = 0; !IsPopupChoosing && Index < std::size(AddChoices); ++Index)
		{
			if (ImGui::Selectable(AddChoices[Index], false, ImGuiSelectableFlags_NoAutoClosePopups | (Index == 1 && Presets.empty() ? ImGuiSelectableFlags_Disabled : 0)))
			{
				IsPopupChoosing = IsChooserFresh = true;
				PopupKind = AddKinds[Index];
			}
		}
		if (!IsPopupChoosing && ImGui::MenuItem("Light"))
		{
			AddLight(Core, PopupTime, Fvector().mad(Device.vCameraPosition, Device.vCameraDirection, 3.0f));
		}
		if (!IsPopupChoosing && ImGui::MenuItem("Sound"))
		{
			AddSound(Core, PopupTime);
		}
		if (!IsPopupChoosing && ImGui::MenuItem("Weather"))
		{
			AddWeather(Core, PopupTime);
		}
		if (!IsPopupChoosing && ImGui::MenuItem("Post"))
		{
			AddPost(Core, PopupTime);
		}
		if (!IsPopupChoosing && ImGui::MenuItem("Camera move"))
		{
			AddCameraMove(Core, PopupTime);
		}
		if (!IsPopupChoosing && ImGui::MenuItem("Folder"))
		{
			AddFolder(Core);
		}
		ImGui::EndPopup();
	}
	if (ImGui::BeginPopup("##CutMenu"))
	{
		const Object* Rig = Find(Core.Data, PopupRow);
		const CameraBlock* Shot = Rig && Rig->Type == ObjectType::Camera && PopupBlock > 0 && PopupBlock < static_cast<s32>(Rig->Camera.Blocks.size()) ? &Rig->Camera.Blocks[PopupBlock] : nullptr;
		const ActorRuntime* Runtime = Rig && Rig->Type == ObjectType::Actor && PopupLayer <= Rig->Actor.Layers.size() ? Core.FindActor(PopupRow) : nullptr;
		const xr_vector<ActorBlock>* Lineup = Runtime ? &BlocksOf(Rig->Actor, PopupLayer) : nullptr;
		const ActorBlock* Faded = Lineup && PopupBlock > 0 && PopupBlock < static_cast<s32>(Lineup->size()) ? &(*Lineup)[PopupBlock] : nullptr;
		if (Faded && ImGui::BeginMenu("Ease"))
		{
			const Interp Current = Faded->BlendEase;
			for (u32 Mode = 0; Mode < 4; ++Mode)
			{
				if (ImGui::MenuItem(TimelineUi::ModeNames[Mode], nullptr, Current == static_cast<Interp>(Mode)))
				{
					BlocksOf(Edit(Core, PopupRow, "Crossfade ease")->Actor, PopupLayer)[PopupBlock].BlendEase = static_cast<Interp>(Mode);
				}
			}
			ImGui::EndMenu();
		}
		if (Faded && ImGui::MenuItem("Remove crossfade", nullptr, false, RemoveCrossfade(Core, PopupRow, PopupLayer, PopupBlock, false)))
		{
			RemoveCrossfade(Core, PopupRow, PopupLayer, PopupBlock, true);
		}
		const CameraBlock* Previous = Shot ? &Rig->Camera.Blocks[PopupBlock - 1] : nullptr;
		const float Overlap = Previous ? Previous->Start + Previous->Length - Shot->Start : -1.0f;
		if (Shot && Overlap <= 0.0f && ImGui::MenuItem("Crossfade"))
		{
			Crossfade(Core, PopupBlock, true);
		}
		if (Shot && Overlap > 0.0f && ImGui::MenuItem("Remove crossfade"))
		{
			Crossfade(Core, PopupBlock, false);
		}
		if (Shot && Overlap > 0.0f && ImGui::BeginMenu("Ease"))
		{
			const Interp Current = Shot->BlendEase;
			for (u32 Mode = 0; Mode < 4; ++Mode)
			{
				if (ImGui::MenuItem(TimelineUi::ModeNames[Mode], nullptr, Current == static_cast<Interp>(Mode)))
				{
					Edit(Core, PopupRow, "Crossfade ease")->Camera.Blocks[PopupBlock].BlendEase = static_cast<Interp>(Mode);
				}
			}
			ImGui::EndMenu();
		}
		ImGui::EndPopup();
	}
	DrawShakeMenu(Core);
	DrawKeyMenu(Core);
	DrawCurveMenu(Core);
	DrawPoseMenu(Core);
}

void Editor::Press(Player& Core, DragKind Kind, u32 Id, bool IsFolder)
{
	const ImGuiIO& Input = ImGui::GetIO();
	const Object* Target = IsFolder ? nullptr : Find(Core.Data, Id);
	Core.IsLookingThrough = Core.IsLookingThrough || (Target && Target->Type == ObjectType::Camera);
	WasPressSelected = IsFolder ? SelectedFolder == Id : IsSelected(Id);
	if (IsFolder)
	{
		Selected.clear();
		SelectedFolder = Id;
	}
	else if (!WasPressSelected || Input.KeyShift)
	{
		Select(Id, Input.KeyCtrl, Input.KeyShift);
	}
	Drag = Kind;
	DragObject = Id;
	DragTime = XToTime(Input.MousePos.x);
	IsDragFolder = IsFolder;
	IsDragCopy = (Kind == DragKind::ClipMove || Kind == DragKind::BlockMove) && Input.KeyCtrl;
	DragKeyPrevious = SelectedKeys;
	DragPickFocus = PickFocusBlock;
	DragLimit = -1.0f;
	HasDragMoved = false;
	Focus = FocusKind::Objects;
}

void Editor::OpenRowMenu(u32 Id, bool IsFolder)
{
	if (IsFolder)
	{
		Selected.clear();
		SelectedFolder = Id;
	}
	else if (!IsSelected(Id))
	{
		Select(Id, false, false);
	}
	PopupRow = Id;
	PopupBlock = -1;
	PopupLayer = 0;
	IsPopupFolder = IsFolder;
	IsPopupChoosing = false;
	PopupRequest = "##RowMenu";
}

void Editor::UpdateDrag(Player& Core)
{
	if (Drag < DragKind::RowMove)
	{
		return;
	}
	const ImGuiIO& Input = ImGui::GetIO();
	if (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right))
	{
		Undo.Cancel(Core.Data);
		if ((Drag == DragKind::ClipMove && IsDragCopy && HasDragMoved) || Drag == DragKind::BoxClips)
		{
			Selected = DragPrevious;
		}
		if ((Drag == DragKind::LaneKeys && HasDragMoved) || Drag == DragKind::BoxKeys)
		{
			SelectedKeys = DragKeyPrevious;
		}
		const bool IsBlockMove = (Drag == DragKind::BlockMove || Drag == DragKind::BlockBlend) && HasDragMoved;
		const Object* Dragged = Find(Core.Data, DragObject);
		if (HasDragMoved && (Drag == DragKind::BlockMove || Drag == DragKind::ShakeMove))
		{
			SelectedKeys = DragKeyPrevious;
			PickFocusBlock = DragPickFocus;
		}
		FocusBlock = Drag == DragKind::ShakeMove && HasDragMoved ? ShakeBase + DragShakePress : IsBlockMove && Dragged && Dragged->Type == ObjectType::Camera ? DragShotIndex : IsBlockMove ? DragBlock : FocusBlock;
		FocusLayer = IsBlockMove ? DragLayer : FocusLayer;
		PruneSelection(Core.Data);
		Drag = DragKind::None;
		DragLimit = -1.0f;
		return;
	}
	if (Drag >= DragKind::BoxClips)
	{
		UpdateBox(Core);
		return;
	}
	if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		if (Drag == DragKind::RowMove && HasDragMoved)
		{
			DropRow(Core);
		}
		DragLimit = -1.0f;
		if (Drag == DragKind::LaneKeys && !HasDragMoved && WasPressSelected)
		{
			SelectKeys(Input.KeyCtrl);
		}
		else if (Drag < DragKind::LaneKeys && !HasDragMoved && WasPressSelected && !IsDragFolder && !Input.KeyShift)
		{
			Select(DragObject, Input.KeyCtrl, false);
		}
		Drag = DragKind::None;
		return;
	}
	if (!HasDragMoved && !ImGui::IsMouseDragging(ImGuiMouseButton_Left, TimelineUi::EdgeZone))
	{
		return;
	}
	if (Drag >= DragKind::LaneKeys)
	{
		DragLane(Core);
		HasDragMoved = true;
		return;
	}
	if (!HasDragMoved && (Drag == DragKind::ClipMove || Drag == DragKind::ClipLength))
	{
		StartClipDrag(Core);
	}
	HasDragMoved = true;
	if (Drag >= DragKind::BlockMove)
	{
		DragBlockEdge(Core);
	}
	if (Drag == DragKind::RowMove || Drag >= DragKind::BlockMove)
	{
		return;
	}
	const float Delta = XToTime(Input.MousePos.x) - DragTime;
	if (Drag == DragKind::ClipLength)
	{
		Object* Target = Find(Core.Data, DragObject);
		if (Target)
		{
			Target->Span.Length = Sanitize(SnapTime(DragStarts[0] + Delta, Input.KeyShift) - Target->Span.Start, MinLength, std::max(MaxTime - Target->Span.Start, MinLength), Target->Span.Length);
			Target->Span.IsLengthLocked = Target->Span.IsLengthLocked || Target->Type == ObjectType::Actor;
		}
		return;
	}
	const auto Found = std::find(DragIds.begin(), DragIds.end(), DragObject);
	const u32 Lead = Found == DragIds.end() ? 0 : static_cast<u32>(Found - DragIds.begin());
	const Object* Head = Find(Core.Data, DragIds.empty() ? 0 : DragIds[Lead]);
	if (Head)
	{
		ShiftClips(Core, SnapSpan(DragStarts[Lead] + Delta, Head->Type == ObjectType::Camera ? DragClipSpan : Head->Span.Length, Input.KeyShift) - DragStarts[Lead]);
	}
}

void Editor::StartBox(DragKind Kind)
{
	const ImGuiIO& Input = ImGui::GetIO();
	Drag = Kind;
	DragTime = XToTime(Input.MousePos.x);
	DragOriginY = Input.MousePos.y - RowsTop;
	HasDragMoved = false;
	DragPrevious = Selected;
	DragKeyPrevious = SelectedKeys;
}

void Editor::UpdateBox(Player& Core)
{
	const ImGuiIO& Input = ImGui::GetIO();
	HasDragMoved = Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Left] >= TimelineUi::EdgeZone * TimelineUi::EdgeZone;
	std::sort(KeyGroup.begin(), KeyGroup.end());
	KeyGroup.erase(std::unique(KeyGroup.begin(), KeyGroup.end()), KeyGroup.end());
	if (HasDragMoved && Drag == DragKind::BoxKeys)
	{
		SelectedKeys.clear();
		std::set_symmetric_difference(DragKeyPrevious.begin(), Input.KeyCtrl ? DragKeyPrevious.end() : DragKeyPrevious.begin(), KeyGroup.begin(), KeyGroup.end(), std::back_inserter(SelectedKeys));
	}
	KeyGroup.clear();
	if (HasDragMoved && Drag == DragKind::BoxClips)
	{
		Selected.assign(DragPrevious.begin(), Input.KeyCtrl ? DragPrevious.end() : DragPrevious.begin());
		for (const RowItem& Row : Rows)
		{
			const Object* Item = Row.IsFolder ? nullptr : Find(Core.Data, Row.Id);
			if (Item && Marquee.Overlaps(ImRect(TimeToX(Item->Span.Start), RowsTop + Row.Y + 1.0f, TimeToX(ClipEnd(*Item)), RowsTop + Row.Y + Row.Track - 1.0f)))
			{
				const auto Found = std::find(Selected.begin(), Selected.end(), Row.Id);
				Found == Selected.end() ? Selected.push_back(Row.Id) : (void)Selected.erase(Found);
			}
		}
	}
	const float Top = ImGui::GetWindowPos().y + RowHeight, Bottom = ImGui::GetWindowPos().y + ImGui::GetWindowHeight() - RowHeight;
	const float Edge = std::min(Input.MousePos.y - Top, 0.0f) + std::max(Input.MousePos.y - Bottom, 0.0f);
	if (HasDragMoved && Edge != 0.0f)
	{
		ImGui::SetScrollY(ImGui::GetScrollY() + Edge * Input.DeltaTime * 10.0f);
	}
	if (ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		return;
	}
	if (Drag == DragKind::BoxClips && !HasDragMoved)
	{
		Selected.clear();
	}
	if (Drag == DragKind::BoxClips)
	{
		SelectedFolder = HasDragMoved && Input.KeyCtrl ? SelectedFolder : 0;
		Focus = FocusKind::Objects;
	}
	else if (HasDragMoved)
	{
		Focus = SelectedKeys.empty() ? FocusKind::Objects : FocusKind::Keys;
	}
	Drag = DragKind::None;
}

void Editor::ShiftClips(Player& Core, float Offset)
{
	float Lo = MaxTime, Hi = 0.0f;
	for (u32 Index = 0; Index < DragIds.size(); ++Index)
	{
		const Object* Item = Find(Core.Data, DragIds[Index]);
		Lo = std::min(Lo, DragStarts[Index]);
		Hi = std::max(Hi, DragStarts[Index] + (!Item ? 0.0f : Item->Type == ObjectType::Camera ? DragClipSpan : Item->Span.Length));
	}
	Offset = std::clamp(Offset, -Lo, std::max(MaxTime - Hi, 0.0f));
	for (u32 Index = 0; Index < DragIds.size(); ++Index)
	{
		Object* Item = Find(Core.Data, DragIds[Index]);
		if (Item && Item->Type == ObjectType::Camera)
		{
			u32 At = 0;
			for (CameraBlock& Shot : Item->Camera.Blocks)
			{
				Shot.Start = At < DragClipStarts.size() ? DragClipStarts[At++] + Offset : Shot.Start;
			}
			for (ShakeClip& Clip : Item->Camera.Shakes)
			{
				Clip.Start = At < DragClipStarts.size() ? DragClipStarts[At++] + Offset : Clip.Start;
			}
		}
		else if (Item)
		{
			Item->Span.Start = DragStarts[Index] + Offset;
		}
	}
}

void Editor::StartClipDrag(Player& Core)
{
	Scene& Data = Core.Data;
	Undo.Begin(Drag == DragKind::ClipLength ? "Resize clip" : IsDragCopy ? "Copy clips" : "Move clips");
	if (IsDragCopy)
	{
		DragPrevious = Selected;
		Selected.clear();
		for (u32 Id : DragPrevious)
		{
			const Object* Source = Find(Data, Id);
			if (!Source || Source->Type == ObjectType::Camera || Data.Objects.size() >= MaxObjects)
			{
				continue;
			}
			Object Copy = *Source;
			Copy.Id = Data.NextId++;
			Copy.Color = NextColor(Data.Header);
			Undo.Touch(Data, Copy.Id);
			DragObject = Id == DragObject ? Copy.Id : DragObject;
			Selected.push_back(Copy.Id);
			Data.Objects.push_back(std::move(Copy));
		}
	}
	if (Drag == DragKind::ClipLength)
	{
		DragIds.assign(1, DragObject);
	}
	else
	{
		DragIds = Selected;
	}
	CaptureStarts(Data, Drag == DragKind::ClipLength);
	CollectSnapTargets(Core, true);
}

void Editor::CaptureStarts(Scene& Data, bool IsLength)
{
	DragStarts.clear();
	DragClipStarts.clear();
	for (u32 Id : DragIds)
	{
		const Object* Item = Find(Data, Id);
		const bool IsRig = Item && Item->Type == ObjectType::Camera && !IsLength;
		float First = Item ? ClipEnd(*Item) : 0.0f;
		for (u32 Index = 0; IsRig && Index < Item->Camera.Blocks.size() + Item->Camera.Shakes.size(); ++Index)
		{
			const float Begin = Index < Item->Camera.Blocks.size() ? Item->Camera.Blocks[Index].Start : Item->Camera.Shakes[Index - Item->Camera.Blocks.size()].Start;
			DragClipStarts.push_back(Begin);
			First = std::min(First, Begin);
		}
		DragClipSpan = IsRig ? ClipEnd(*Item) - First : DragClipSpan;
		DragStarts.push_back(!Item ? 0.0f : IsLength ? ClipEnd(*Item) : IsRig ? First : Item->Span.Start);
		Undo.Touch(Data, Id);
	}
}

void Editor::DropRow(Player& Core)
{
	const TimelineUi::DropSpot Spot = TimelineUi::Drop(Rows, RowsTop, ImGui::GetIO().MousePos.y, IsDragFolder);
	const RowItem* Pivot = Spot.Row >= 0 && !Spot.IsInto ? &Rows[Spot.Row] : nullptr;
	const auto IsMoved = [this](const RowItem& Item) { return Item.IsFolder == IsDragFolder && (Item.Id == DragObject || (!IsDragFolder && IsSelected(Item.Id))); };
	if (Pivot && Pivot->Id == DragObject && Pivot->IsFolder == IsDragFolder && Selected.size() < 2)
	{
		return;
	}
	MemberScratch.clear();
	std::copy_if(Rows.begin(), Rows.end(), std::back_inserter(MemberScratch), IsMoved);
	TimelineUi::Siblings(Core.Data, Spot.Parent, RowScratch);
	const auto Line = Pivot ? std::find_if(RowScratch.begin(), RowScratch.end(), [Pivot](const RowItem& Item) { return Item.Id == Pivot->Id && Item.IsFolder == Pivot->IsFolder; }) : RowScratch.end();
	const auto At = std::count_if(RowScratch.begin(), Line + (Line != RowScratch.end() && !Spot.IsBefore ? 1 : 0), [&IsMoved](const RowItem& Item) { return !IsMoved(Item); });
	std::erase_if(RowScratch, IsMoved);
	RowScratch.insert(RowScratch.begin() + At, MemberScratch.begin(), MemberScratch.end());
	Undo.Begin(MemberScratch.size() > 1 ? "Move rows" : "Move row");
	for (u32 Index = 0; Index < RowScratch.size(); ++Index)
	{
		const RowItem& Item = RowScratch[Index];
		Folder* Group = Item.IsFolder ? FindFolder(Core.Data, Item.Id) : nullptr;
		Object* Target = Item.IsFolder ? nullptr : Find(Core.Data, Item.Id);
		if (Group && Group->Row != Index)
		{
			Undo.TouchHeader(Core.Data);
			Group->Row = Index;
		}
		if (Target && (Target->Row != Index || Target->FolderId != Spot.Parent))
		{
			Undo.Touch(Core.Data, Item.Id);
			Target->Row = Index;
			Target->FolderId = Spot.Parent;
		}
	}
}

s32 Editor::SpanAt(const Object& Target, const ActorRuntime& Runtime, float Local, double& Rep, float& Sequence, u8 Layer) const
{
	const float Length = Runtime.SequenceLength;
	const xr_vector<BlockSpan>& Spans = Runtime.Track(Layer).Spans;
	if (Spans.empty() || !(Length > 0.0f) || Layer > Target.Actor.Layers.size())
	{
		return -1;
	}
	const double Area = Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Local, Target.Actor.TimeScale);
	Rep = Target.Actor.End == EndMode::Hold ? 0.0 : std::floor(Area / Length);
	Sequence = static_cast<float>(Area - Rep * Length);
	Sequence = Target.Actor.End == EndMode::PingPong && std::fmod(Rep, 2.0) >= 1.0 ? Length - Sequence : Sequence;
	if (!(Local >= 0.0f && Local < Target.Span.Length && Sequence >= 0.0f && Sequence < Length))
	{
		return -1;
	}
	const auto Upper = std::upper_bound(Spans.begin(), Spans.end(), Sequence, [](float Moment, const BlockSpan& Item) { return Moment < Item.Start; });
	const s32 Index = static_cast<s32>(Upper - Spans.begin()) - 1;
	return Index >= 0 && Spans[Index].Block < BlocksOf(Target.Actor, Layer).size() && Sequence < Spans[Index].Start + Spans[Index].Duration ? Index : -1;
}

s32 Editor::BlockHit(const Object& Target, const ActorRuntime& Runtime, u8 Layer, const ImVec2& Mouse, const ImRect& Row, DragKind& Kind, s32& Hit, double& Rep, ClipPart& Part, ImRect& Area) const
{
	float Sequence = 0.0f;
	s32 Span = -1;
	for (const float Shift : { 0.0f, TimelineUi::OutZone, -TimelineUi::OutZone })
	{
		Span = Span >= 0 ? Span : SpanAt(Target, Runtime, XToTime(Mouse.x + Shift) - Target.Span.Start, Rep, Sequence, Layer);
	}
	if (Span < 0)
	{
		return -1;
	}
	Hit = Span;
	if (Rep != 0.0)
	{
		Part = ClipPart::Body;
		Area = Row;
		Kind = DragKind::BlockMove;
		return Span;
	}
	const xr_vector<BlockSpan>& Spans = Runtime.Track(Layer).Spans;
	const BlockSpan& Item = Spans[Span];
	const BlockSpan* Before = Span > 0 && Item.Overlap > 0.0f ? &Spans[Span - 1] : nullptr;
	const bool IsCrowded = Span + 1 < static_cast<s32>(Spans.size()) && Spans[Span + 1].Overlap > 0.0f;
	const auto At = [&](float Moment) { return SequenceToX(Target, Runtime, 0.0, Moment); };
	const auto BoxOf = [&](const BlockSpan& Clip, float Cross, bool IsLeftFree, bool IsRightFree)
	{
		const float End = Clip.Start + Clip.Duration;
		return TimelineUi::PartBox { At(Clip.Start), At(End), Row.Min.y, Row.Max.y, At(Clip.Start + std::min(Clip.FadeIn, Clip.Duration)), At(End - std::min(Clip.FadeOut, Clip.Duration)), Cross, IsLeftFree, IsRightFree, true };
	};
	Part = TimelineUi::PartAt(BoxOf(Item, Before ? At(Before->Start + Before->Duration) : 0.0f, !Before, !IsCrowded), Mouse, Area);
	ImRect Other;
	const ClipPart Theirs = Before && (Part == ClipPart::Cross || Part == ClipPart::Body) ? TimelineUi::PartAt(BoxOf(*Before, 0.0f, false, false), Mouse, Other) : ClipPart::None;
	if (Theirs == ClipPart::End || Theirs == ClipPart::FadeOut)
	{
		Part = Theirs;
		Area = Other;
		Hit = Span - 1;
	}
	Kind = TimelineUi::BlockParts[static_cast<u32>(Part)];
	return Part == ClipPart::None ? -1 : Span;
}

void Editor::ShowPart(ClipPart Part, const ImRect& Area, u32 Color, u32 Type)
{
	ImDrawList* List = ImGui::GetWindowDrawList();
	const bool IsEdge = Part == ClipPart::Start || Part == ClipPart::End, IsFade = Part == ClipPart::FadeIn || Part == ClipPart::FadeOut;
	if (Part == ClipPart::Key)
	{
		List->AddCircle(Area.GetCenter(), 0.5f * Area.GetWidth(), Color, 0, 1.5f);
	}
	else if (IsEdge || IsFade)
	{
		List->AddRectFilled(Area.Min, Area.Max, Color);
	}
	else
	{
		List->AddRect(Area.Min, Area.Max, Color);
	}
	ImGui::SetMouseCursor(IsEdge ? ImGuiMouseCursor_ResizeEW : IsFade ? ImGuiMouseCursor_ResizeNWSE : Part == ClipPart::Key ? ImGuiMouseCursor_Arrow : ImGuiMouseCursor_Hand);
	Hint = TimelineUi::PartHints[Type][static_cast<u32>(Part)];
}

void Editor::PressBlock(Player& Core, const Object& Target, const ActorRuntime& Runtime, u8 Layer, DragKind Kind, s32 Hit, double Rep)
{
	const ImGuiIO& Input = ImGui::GetIO();
	const LayerRuntime& Track = Runtime.Track(Layer);
	Press(Core, Kind, Target.Id, false);
	DragLayer = FocusLayer = DragValidLayer = Layer;
	DragRows = static_cast<u8>(Target.Actor.Layers.size() + 1);
	DragBlock = static_cast<s32>(Track.Spans[Hit].Block);
	DragValidStart = BlocksOf(Target.Actor, Layer)[DragBlock].Start;
	for (u8 Other = 0; Other <= Target.Actor.Layers.size() && Other <= MaxActorLayers; ++Other)
	{
		DragBlocks[Other] = BlocksOf(Target.Actor, Other);
		DragBlockStarts[Other] = Runtime.Track(Other).Starts;
	}
	DragSequence = static_cast<float>(AreaAt(Target, Runtime, XToTime(Input.MousePos.x)));
	DragSign = Target.Actor.End == EndMode::PingPong && std::fmod(Rep, 2.0) >= 1.0 ? -1.0f : 1.0f;
	IsDragSpeed = Input.KeyCtrl && (Kind == DragKind::BlockIn || Kind == DragKind::BlockOut);
	DragIds.clear();
	CollectSnapTargets(Core, true);
	for (const BlockSpan& Other : Track.Spans)
	{
		if (static_cast<s32>(Other.Block) != DragBlock)
		{
			SnapTargets.push_back(TimeAt(Target, Runtime, Other.Start));
			SnapTargets.push_back(TimeAt(Target, Runtime, Other.Start + Other.Duration));
		}
	}
	FocusObject = Target.Id;
	FocusBlock = DragBlock;
	Focus = FocusKind::Block;
}

float Editor::SequenceToX(const Object& Target, const ActorRuntime& Runtime, double Rep, float Sequence) const
{
	const float Length = Runtime.SequenceLength;
	const bool IsReversed = Target.Actor.End == EndMode::PingPong && std::fmod(Rep, 2.0) >= 1.0;
	const double Area = Rep * Length + (IsReversed ? Length - Sequence : Sequence);
	return TimeToX(Target.Actor.IsRelative ? Target.Span.Start + Curves::LaneInverseArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Target.Actor.TimeScale, Area, Target.Span.Length) : TimeAt(Target, Runtime, Area));
}

double Editor::AreaAt(const Object& Target, const ActorRuntime& Runtime, float Time) const
{
	const float Length = Target.Actor.IsRelative ? Target.Span.Length : MaxTime, Local = std::max(Time - Target.Span.Start, 0.0f);
	const double Inside = Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, std::min(Local, Length), Target.Actor.TimeScale);
	return Inside + std::max(Local - Length, 0.0f) * Curves::LaneScalar(Target.Lanes, Param::TimeScale, 0, Length, Target.Actor.TimeScale);
}

float Editor::TimeAt(const Object& Target, const ActorRuntime& Runtime, double Area) const
{
	const float Length = Target.Actor.IsRelative ? Target.Span.Length : MaxTime, Rate = Curves::LaneScalar(Target.Lanes, Param::TimeScale, 0, Length, Target.Actor.TimeScale);
	const double Inside = Curves::LaneArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Length, Target.Actor.TimeScale);
	const float Beyond = Area > Inside && Rate > EPS_S ? static_cast<float>((Area - Inside) / Rate) : 0.0f;
	return Target.Span.Start + Curves::LaneInverseArea(Target.Lanes, &Runtime.TimeScaleArea, Param::TimeScale, 0, Target.Actor.TimeScale, std::min(Area, Inside), Length) + Beyond;
}

void Editor::PlaceBlocks(const Object& Target, const ActorRuntime& Runtime, xr_vector<ActorBlock>& Blocks, const xr_vector<float>& Starts) const
{
	for (u32 Index = 0; !Target.Actor.IsRelative && Index < Blocks.size() && Index < Starts.size(); ++Index)
	{
		ActorBlock& Block = Blocks[Index];
		if (std::fabs(AreaAt(Target, Runtime, Target.Span.Start + Block.Start) - Starts[Index]) > 1e-5)
		{
			Block.Start = std::clamp(std::round((TimeAt(Target, Runtime, Starts[Index]) - Target.Span.Start) * 1e5f) * 1e-5f, 0.0f, MaxTime);
		}
	}
}

bool Editor::IsTrackValid(const Object& Target, const ActorRuntime& Runtime, const xr_vector<ActorBlock>& Blocks) const
{
	float Begin = 0.0f, Ends[2] = { 0.0f, 0.0f };
	for (u32 Index = 0; Index < Blocks.size(); ++Index)
	{
		const float Next = static_cast<float>(AreaAt(Target, Runtime, Target.Span.Start + Blocks[Index].Start)), End = Next + Runtime.BlockDuration(Blocks[Index]);
		if ((Index && (Next <= Begin + 1e-4f || End <= Ends[1] + 1e-4f)) || (Index > 1 && Next < Ends[0] - 1e-4f))
		{
			return false;
		}
		Begin = Next;
		Ends[0] = Ends[1];
		Ends[1] = End;
	}
	return true;
}

bool Editor::RemoveCrossfade(Player& Core, u32 Id, u8 Layer, s32 Block, bool ShouldApply)
{
	const Object* Target = Find(Core.Data, Id);
	const ActorRuntime* Runtime = Core.FindActor(Id);
	if (!Target || !Runtime || !Runtime->IsComplete() || Target->Actor.IsRelative || Layer > Target->Actor.Layers.size() || Block < 1 || Block >= static_cast<s32>(BlocksOf(Target->Actor, Layer).size()))
	{
		return false;
	}
	CutBlocks = BlocksOf(Target->Actor, Layer);
	ActorBlock& Head = CutBlocks[Block - 1];
	const float Joint = static_cast<float>(AreaAt(*Target, *Runtime, Target->Span.Start + CutBlocks[Block].Start)), Begin = static_cast<float>(AreaAt(*Target, *Runtime, Target->Span.Start + Head.Start));
	if (!(Begin + Runtime->BlockDuration(Head) - Joint > 1e-4f))
	{
		return false;
	}
	Actors::TrimBlock(Head, Runtime->MotionLength(Head.Source, Head.Name), Joint - Begin);
	if (!IsTrackValid(*Target, *Runtime, CutBlocks))
	{
		return false;
	}
	if (ShouldApply)
	{
		BlocksOf(Edit(Core, Id, "Remove crossfade")->Actor, Layer) = CutBlocks;
	}
	return true;
}

u8 Editor::PlaceRow(Object& Item, const ActorRuntime& Runtime, u8 Row, const ActorBlock& Block, bool IsStrict, bool IsSearching)
{
	ActorData& Setup = Item.Actor;
	const auto ByStart = [](const ActorBlock& A, const ActorBlock& B) { return A.Start < B.Start; };
	const auto SeqOf = [&](const ActorBlock& Other) { return static_cast<float>(AreaAt(Item, Runtime, Item.Span.Start + Other.Start)); };
	const float From = SeqOf(Block), To = From + Runtime.BlockDuration(Block);
	const u32 Rows = static_cast<u32>(Setup.Layers.size()) + 1;
	for (u32 Step = 0; Step <= Rows && (IsSearching || !Step); ++Step)
	{
		const u32 At = (Row + Step) % (Rows + 1);
		const bool IsNew = At == Rows;
		if ((IsNew && Rows > MaxActorLayers) || (!IsNew && BlocksOf(Setup, static_cast<u8>(At)).size() >= MaxBlocks))
		{
			continue;
		}
		CutBlocks.clear();
		if (!IsNew)
		{
			CutBlocks = BlocksOf(Setup, static_cast<u8>(At));
		}
		const auto SpanOf = [&](u32 Index) { const float Begin = SeqOf(CutBlocks[Index]); return RowSpan { true, 0, Begin, Begin + Runtime.BlockDuration(CutBlocks[Index]) }; };
		const bool IsFree = IsStrict ? IsSpanFree(static_cast<u32>(CutBlocks.size()), SpanOf, 0, From, std::max(To, From + 1e-4f), true) : true;
		CutBlocks.insert(std::upper_bound(CutBlocks.begin(), CutBlocks.end(), Block, ByStart), Block);
		if (!IsFree || !IsTrackValid(Item, Runtime, CutBlocks))
		{
			continue;
		}
		if (IsNew)
		{
			Setup.Layers.push_back(NewRow(&Runtime));
		}
		BlocksOf(Setup, static_cast<u8>(At)) = CutBlocks;
		return static_cast<u8>(At);
	}
	return u8(-1);
}

void Editor::DragBlockEdge(Player& Core)
{
	const Object* Target = Find(Core.Data, DragObject);
	if (Drag >= DragKind::ShakeMove)
	{
		DragShake(Core);
		return;
	}
	if (Target && Target->Type == ObjectType::Camera)
	{
		DragCameraBlock(Core, *Target);
		return;
	}
	const ActorRuntime* Runtime = Core.FindActor(DragObject);
	if (!Target || !Runtime || !Runtime->IsComplete() || Target->Actor.IsRelative)
	{
		SetStatus(true, "The model or an animation of this actor is missing or still loading; its blocks cannot be edited.");
		Drag = DragKind::None;
		return;
	}
	DragActorBlock(Core, *Target, *Runtime);
}

void Editor::DragActorBlock(Player& Core, const Object& Target, const ActorRuntime& Runtime)
{
	const u8 Layers = static_cast<u8>(std::min<u32>(DragRows, MaxActorLayers + 1) - 1);
	const u32 Count = DragLayer <= Layers ? static_cast<u32>(DragBlocks[DragLayer].size()) : 0;
	if (DragBlock < 0 || static_cast<u32>(DragBlock) >= Count || DragBlockStarts[DragLayer].size() != Count)
	{
		return;
	}
	const ActorBlock Base = DragBlocks[DragLayer][DragBlock];
	const float Length = Runtime.MotionLength(Base.Source, Base.Name);
	const float Rate = static_cast<float>(Core.Data.Header.FrameRate), Frame = 1.0f / Rate;
	const bool IsMove = Drag == DragKind::BlockMove || Drag == DragKind::BlockBlend, IsFade = Drag == DragKind::BlockFadeIn || Drag == DragKind::BlockFadeOut;
	const ImGuiIO& Input = ImGui::GetIO();
	if (!IsMove && !IsFade && !(Length > 2.0f * Frame))
	{
		SetStatus(true, "The animation is too short to trim.");
		Drag = DragKind::None;
		return;
	}
	const bool IsFree = Input.KeyShift || !Settings.IsSnapping;
	const auto Snap = [IsFree, Rate](float Value) { return IsFree ? Value : std::round(Value * Rate) / Rate; };
	const float Delta = (static_cast<float>(AreaAt(Target, Runtime, XToTime(Input.MousePos.x))) - DragSequence) * DragSign;
	const float Begin = DragBlockStarts[DragLayer][DragBlock], Width = Runtime.BlockDuration(Base), End = Begin + Width;
	const char* const Label = Drag == DragKind::BlockMove ? (IsDragCopy ? "Copy block" : "Move block") : Drag == DragKind::BlockBlend ? "Crossfade" : IsFade ? "Fade" : IsDragSpeed ? "Block speed" : "Trim block";
	Object& Item = *Edit(Core, Target.Id, Label);
	Item.Actor.Layers.resize(Layers);
	for (u8 Layer = 0; Layer <= Layers; ++Layer)
	{
		BlocksOf(Item.Actor, Layer) = DragBlocks[Layer];
	}
	if (IsMove)
	{
		const float Wanted = std::max(Begin + Delta, 0.0f), Moment = TimeAt(Target, Runtime, Wanted);
		const float At = static_cast<float>(AreaAt(Target, Runtime, SnapSpan(Moment, TimeAt(Target, Runtime, Wanted + Width) - Moment, Input.KeyShift)));
		if (!IsDragCopy)
		{
			BlocksOf(Item.Actor, DragLayer).erase(BlocksOf(Item.Actor, DragLayer).begin() + DragBlock);
		}
		const u8 Aimed = std::min<u8>(LayerAt(Core.Data, Target, Input.MousePos.y), static_cast<u8>(Layers + 1));
		const float Slack = 0.25f * RowHeight;
		const bool IsLeaving = Aimed != DragLayer && LayerAt(Core.Data, Target, Input.MousePos.y + (Aimed > DragLayer ? -Slack : Slack)) != DragLayer;
		const auto Drop = [&](u8 Row, float Start)
		{
			ActorBlock Moved = Base;
			Moved.Start = Start;
			const u8 Into = Overwrite(Item, Runtime, Row, Moved, Frame);
			if (Into > MaxActorLayers)
			{
				return false;
			}
			const xr_vector<ActorBlock>& Placed = BlocksOf(Item.Actor, Into);
			DragValidLayer = FocusLayer = Into;
			DragValidStart = Start;
			FocusBlock = static_cast<s32>(std::find(Placed.begin(), Placed.end(), Moved) - Placed.begin());
			return true;
		};
		if (!Drop(Drag == DragKind::BlockMove && IsLeaving ? Aimed : DragLayer, std::clamp(std::round((TimeAt(Item, Runtime, At) - Item.Span.Start) * 1e5f) * 1e-5f, 0.0f, MaxTime)) && !Drop(DragValidLayer, DragValidStart))
		{
			BlocksOf(Item.Actor, DragLayer) = DragBlocks[DragLayer];
		}
		return;
	}
	xr_vector<ActorBlock>& Blocks = BlocksOf(Item.Actor, DragLayer);
	if (IsFade)
	{
		ActorBlock& Block = Blocks[DragBlock];
		(Drag == DragKind::BlockFadeIn ? Block.FadeIn : Block.FadeOut) = std::clamp(Snap(Drag == DragKind::BlockFadeIn ? Base.FadeIn + Delta : Base.FadeOut - Delta), 0.0f, Width);
		return;
	}
	const float Speed = std::max(std::fabs(Base.Speed), 0.05f), Out = Base.Out > 0.0f ? std::min(Base.Out, Length) : Length;
	const bool IsForward = Base.Speed >= 0.0f, IsLeft = Drag == DragKind::BlockIn;
	const float Played = Width - std::max(Base.Hold, 0.0f), Loops = std::clamp(Base.Loops, 1.0f, MaxLoops);
	const auto Whole = [Length, Frame](float Value) { return Value >= Length - 0.5f * Frame ? 0.0f : Value; };
	const auto Trim = [&](float Shift)
	{
		Blocks = DragBlocks[DragLayer];
		BlockStarts = DragBlockStarts[DragLayer];
		ActorBlock& Block = Blocks[DragBlock];
		if (IsDragSpeed)
		{
			const float Fill = std::max(Snap(IsLeft ? Played - Shift : Played + Shift), Frame);
			Block.Speed = std::copysign(std::clamp(Speed * Played / Fill, 0.05f, 4.0f), Base.Speed);
		}
		else if (IsLeft && IsForward)
		{
			Block.In = std::clamp(Snap(Base.In + Shift * Speed / Loops), 0.0f, Out - Frame);
		}
		else if (IsLeft)
		{
			Block.Out = Whole(std::clamp(Snap(Out - Shift * Speed / Loops), Base.In + Frame, Length));
		}
		else if (Input.KeyShift)
		{
			Block.Hold = std::max(Width + Shift - Played, 0.0f);
		}
		else if (Loops > 1.0f)
		{
			const float Unit = Played / Loops;
			Block.Loops = std::clamp(Settings.IsSnapping ? Loops + std::round(Shift / Unit) : (Played + Shift) / Unit, 1.0f, MaxLoops);
		}
		else
		{
			const float Room = (IsForward ? Length - Base.In : Out) / Speed, Wanted = std::max(Played + Shift, Frame);
			const bool IsLooping = Wanted > Room;
			Block.Loops = IsLooping ? std::clamp(Settings.IsSnapping ? std::round(Wanted / Room) : Wanted / Room, 1.0f, MaxLoops) : 1.0f;
			(IsForward ? Block.Out : Block.In) = IsLooping ? 0.0f : IsForward ? Whole(std::clamp(Snap(Base.In + Wanted * Speed), Base.In + Frame, Length)) : std::clamp(Snap(Out - Wanted * Speed), 0.0f, Out - Frame);
		}
		BlockStarts[DragBlock] = IsLeft ? std::max(End - Runtime.BlockDuration(Block), 0.0f) : Begin;
		PlaceBlocks(Item, Runtime, Blocks, BlockStarts);
		return IsTrackValid(Item, Runtime, Blocks);
	};
	DragLimit = -1.0f;
	if (!Trim(Delta))
	{
		float Lo = 0.0f, Hi = Delta;
		for (u32 Step = 0; Step < 12; ++Step)
		{
			const float Mid = 0.5f * (Lo + Hi);
			(Trim(Mid) ? Lo : Hi) = Mid;
		}
		if (!Trim(Lo))
		{
			Blocks = DragBlocks[DragLayer];
		}
		DragLimit = TimeAt(Item, Runtime, BlockStarts[DragBlock] + (IsLeft ? 0.0f : Runtime.BlockDuration(Blocks[DragBlock])));
	}
	FocusBlock = DragBlock;
	FocusLayer = DragLayer;
	Refit(Core, Item);
}

u8 Editor::Overwrite(Object& Item, const ActorRuntime& Runtime, u8 Row, const ActorBlock& Moved, float Frame)
{
	ActorData& Setup = Item.Actor;
	const u32 Rows = static_cast<u32>(Setup.Layers.size()) + 1;
	const bool IsNew = Row >= Rows;
	if (IsNew && Rows > MaxActorLayers)
	{
		return u8(-1);
	}
	const auto SeqOf = [&](const ActorBlock& Other) { return static_cast<float>(AreaAt(Item, Runtime, Item.Span.Start + Other.Start)); };
	const float A = SeqOf(Moved), B = A + Runtime.BlockDuration(Moved);
	CutBlocks.clear();
	BlockStarts.clear();
	SpanScratch.clear();
	const xr_vector<ActorBlock>* Source = IsNew ? nullptr : &BlocksOf(Setup, Row);
	for (u32 Index = 0; Source && Index < Source->size(); ++Index)
	{
		SpanScratch.push_back({ SeqOf((*Source)[Index]), Runtime.BlockDuration((*Source)[Index]) });
	}
	TimelineUi::PlanOverwrite(SpanScratch, A, B, Frame, CoverPlan);
	for (u32 Index = 0; Source && Index < Source->size(); ++Index)
	{
		const ActorBlock& Block = (*Source)[Index];
		const float C = SpanScratch[Index].Start, Length = Runtime.MotionLength(Block.Source, Block.Name);
		const Cover Planned = static_cast<Cover>(CoverPlan[Index]);
		const Cover Action = Planned == Cover::Split && Source->size() + 3 > MaxBlocks ? Cover::CutTail : Planned;
		if (Action == Cover::Keep)
		{
			CutBlocks.push_back(Block);
			BlockStarts.push_back(C);
			continue;
		}
		if ((Action == Cover::Split || Action == Cover::CutTail) && A - C >= Frame)
		{
			ActorBlock& Head = CutBlocks.emplace_back(Block);
			Actors::TrimBlock(Head, Length, A - C);
			Head.FadeOut = 0.0f;
			BlockStarts.push_back(C);
		}
		if (Action == Cover::Split || Action == Cover::CutHead)
		{
			ActorBlock Parts[2];
			float Offsets[2] = {};
			const u32 Count = TimelineUi::TailOf(Block, Length, B - C, Frame, Parts, Offsets);
			for (u32 Part = 0; Part < Count; ++Part)
			{
				CutBlocks.push_back(Parts[Part]);
				BlockStarts.push_back(C + Offsets[Part]);
			}
		}
	}
	const size_t At = static_cast<size_t>(std::upper_bound(BlockStarts.begin(), BlockStarts.end(), A) - BlockStarts.begin());
	CutBlocks.insert(CutBlocks.begin() + At, Moved);
	BlockStarts.insert(BlockStarts.begin() + At, A);
	PlaceBlocks(Item, Runtime, CutBlocks, BlockStarts);
	if (CutBlocks.size() > MaxBlocks || !IsTrackValid(Item, Runtime, CutBlocks))
	{
		return u8(-1);
	}
	if (IsNew)
	{
		Setup.Layers.push_back(NewRow(&Runtime));
	}
	BlocksOf(Setup, IsNew ? static_cast<u8>(Rows) : Row) = CutBlocks;
	return IsNew ? static_cast<u8>(Rows) : Row;
}

bool Editor::SplitBlock(Player& Core, u32 Id, s32 Block, bool ShouldApply, u8 Layer)
{
	const Object* Target = Find(Core.Data, Id);
	const ActorRuntime* Runtime = Core.FindActor(Id);
	double Rep = 0.0;
	float Sequence = 0.0f;
	const s32 Span = Target && Runtime ? SpanAt(*Target, *Runtime, Core.Time - Target->Span.Start, Rep, Sequence, Layer) : -1;
	const BlockSpan* Item = Span < 0 ? nullptr : &Runtime->Track(Layer).Spans[Span];
	const float Offset = Item ? Sequence - Item->Start : 0.0f;
	const bool IsDone = Item && static_cast<s32>(Item->Block) == Block && Offset > 0.0f && Offset < Item->Play * Item->Loops && CutSequence(Core, Id, Sequence, Sequence, ShouldApply, Layer, Block);
	if (IsDone && ShouldApply)
	{
		Refit(Core, *Target);
	}
	return IsDone;
}

bool Editor::CutSequence(Player& Core, u32 Id, float From, float To, bool ShouldApply, u8 Layer, s32 Only)
{
	const Object* Target = Find(Core.Data, Id);
	const ActorRuntime* Runtime = Core.FindActor(Id);
	if (!Target || !Runtime || !Runtime->IsComplete() || Target->Actor.IsRelative || Layer > Target->Actor.Layers.size() || Runtime->Track(Layer).Starts.size() != BlocksOf(Target->Actor, Layer).size())
	{
		return false;
	}
	const float Frame = 1.0f / static_cast<float>(Core.Data.Header.FrameRate);
	const auto Map = [From, To](float At) { return At <= From ? At : At < To ? From : At - (To - From); };
	const xr_vector<ActorBlock>& Source = BlocksOf(Target->Actor, Layer);
	const LayerRuntime& Track = Runtime->Track(Layer);
	auto Next = Track.Spans.begin();
	CutBlocks.clear();
	BlockStarts.clear();
	for (u32 Index = 0; Index < Source.size(); ++Index)
	{
		const ActorBlock& Block = Source[Index];
		const float Begin = Track.Starts[Index];
		const BlockSpan* Span = Next != Track.Spans.end() && Next->Block == Index ? &*Next++ : nullptr;
		const float Width = Span ? Span->Duration : 0.0f, Total = Span ? Span->Play * Span->Loops : 0.0f;
		const float Lo = std::clamp(From - Begin, 0.0f, Width), Hi = std::clamp(To - Begin, 0.0f, Width);
		const bool IsCut = Span && (Only < 0 || static_cast<s32>(Index) == Only) && (Lo < Hi || (Lo == Hi && Lo > 0.0f && Lo < Total));
		if (!IsCut || Lo >= Total)
		{
			CutBlocks.push_back(Block);
			BlockStarts.push_back(Map(Begin));
			CutBlocks.back().Hold = IsCut ? std::max(Block.Hold - (Hi - Lo), 0.0f) : Block.Hold;
			continue;
		}
		const bool IsForward = Block.Speed >= 0.0f;
		const float Play = Span->Play;
		const auto Moment = [Span](float Offset) { return Span->From + (Span->To - Span->From) * Offset / Span->Play; };
		if (Lo > 0.0f)
		{
			ActorBlock& Head = CutBlocks.emplace_back(Block);
			BlockStarts.push_back(Map(Begin));
			Head.Hold = 0.0f;
			Head.Loops = std::max(Lo / Play, 1.0f);
			(IsForward ? Head.Out : Head.In) = Lo < Play ? Moment(Lo) : (IsForward ? Head.Out : Head.In);
		}
		if (!(Hi < Width))
		{
			continue;
		}
		const bool IsPlaying = Hi < Total;
		const float Phase = IsPlaying ? Hi - Play * std::floor(Hi / Play) : std::max(Play - Frame, 0.0f), Left = Total - Hi;
		const bool IsSplit = IsPlaying && Left > Play - Phase + 0.5f * Frame;
		ActorBlock& First = CutBlocks.emplace_back(Block);
		BlockStarts.push_back(Map(Begin + Hi));
		First.Loops = 1.0f;
		First.Hold = IsSplit ? 0.0f : IsPlaying ? Block.Hold : std::max(Width - Hi - (Play - Phase), 0.0f);
		(IsForward ? First.In : First.Out) = Moment(Phase);
		(IsForward ? First.Out : First.In) = IsPlaying && !IsSplit ? Moment(std::min(Phase + Left, Play)) : (IsForward ? First.Out : First.In);
		if (IsSplit)
		{
			const float After = BlockStarts.back() + Runtime->BlockDuration(First), Rest = (Left - Play + Phase) / Play;
			ActorBlock& Second = CutBlocks.emplace_back(Block);
			BlockStarts.push_back(After);
			Second.Loops = std::max(Rest, 1.0f);
			(IsForward ? Second.Out : Second.In) = Rest < 1.0f ? Moment(Rest * Play) : (IsForward ? Second.Out : Second.In);
		}
	}
	if (CutBlocks.size() > MaxBlocks)
	{
		return false;
	}
	for (u32 Index = 1; Index < BlockStarts.size(); ++Index)
	{
		for (u32 At = Index; At > 0 && BlockStarts[At] < BlockStarts[At - 1]; --At)
		{
			std::swap(BlockStarts[At], BlockStarts[At - 1]);
			std::swap(CutBlocks[At], CutBlocks[At - 1]);
		}
	}
	if (ShouldApply)
	{
		Object& Item = *Edit(Core, Id, "Split block");
		xr_vector<ActorBlock>& Blocks = BlocksOf(Item.Actor, Layer);
		Blocks = CutBlocks;
		PlaceBlocks(Item, *Runtime, Blocks, BlockStarts);
	}
	return true;
}

void Editor::ShiftTime(Player& Core, float At, float Amount)
{
	Scene& Data = Core.Data;
	xr_vector<Marker>& Markers = Data.Header.Markers;
	const float HalfFrame = 0.5f / static_cast<float>(Data.Header.FrameRate), Pivot = std::max(At, At - Amount);
	const char* const Late = "The last edit is still being applied; try again.";
	const char* const Past = "The scene would pass 60:00:00.";
	const char* Problem = Undo.IsOpen() || Core.HasRequests() ? Late : !Markers.empty() && Markers.back().Time >= Pivot && Markers.back().Time + Amount > MaxTime ? Past : nullptr;
	u32 Deleted = 0, Kept = 0;
	for (u32 Pass = 0; Pass < 2 && !Problem; ++Pass)
	{
		const bool ShouldApply = Pass > 0;
		if (ShouldApply)
		{
			Undo.Begin(Amount > 0.0f ? "Insert time" : "Remove time");
			Undo.TouchHeader(Data);
			std::erase_if(Markers, [At, Pivot](const Marker& Item) { return Item.Time >= At && Item.Time < Pivot; });
			for (Marker& Item : Markers)
			{
				Item.Time += Item.Time >= Pivot ? Amount : 0.0f;
			}
			Markers.erase(std::unique(Markers.begin(), Markers.end(), [HalfFrame](const Marker& A, const Marker& B) { return B.Time - A.Time < HalfFrame; }), Markers.end());
		}
		for (u32 Index = 0; Index < Data.Objects.size() && !Problem;)
		{
			Object& Item = Data.Objects[Index];
			const float Start = Item.Span.Start, Length = Item.Span.Length, End = Start + Length;
			const float From = std::max(At - Start, 0.0f), To = Pivot - Start, Inner = std::min(To, Length);
			const bool IsRig = Item.Type == ObjectType::Camera;
			const bool IsMoved = !IsRig && Start >= Pivot, IsAcross = !IsMoved && End > At;
			bool IsGone = !IsRig && IsAcross && Length - (Inner - From) < MinLength;
			Problem = End > At && End + Amount > MaxTime ? Past : nullptr;
			if (ShouldApply && (IsMoved || IsAcross))
			{
				Undo.Touch(Data, Item.Id);
				Item.Span.Start = IsMoved ? Start + Amount : std::min(Start, At);
			}
			if (IsAcross && !Problem && IsRig)
			{
				xr_vector<CameraBlock>& Blocks = Item.Camera.Blocks;
				const char* const Refusal = Blocks.size() >= MaxBlocks ? "The camera would have more than 256 clips." : Late;
				for (s32 Block = static_cast<s32>(Blocks.size()) - 1; Block >= 0 && !Problem; --Block)
				{
					const float Begin = Blocks[Block].Start, Size = Blocks[Block].Length;
					const float Lo = std::max(From - Begin, 0.0f), Hi = Amount > 0.0f ? Lo : std::min(To - Begin, Size);
					const bool IsCut = Amount > 0.0f ? !Blocks[Block].Keys.empty() && Lo >= MinLength && Lo <= Size - MinLength : Lo < Hi;
					Problem = IsCut && !CutShot(Core, Block, Lo, Hi, ShouldApply) ? Refusal : nullptr;
				}
				for (CameraBlock& Shot : Blocks)
				{
					Shot.Start += ShouldApply && !Problem && Shot.Start >= To - 1e-4f ? Amount : 0.0f;
				}
				if (ShouldApply && Item.Span.IsLengthLocked)
				{
					Item.Span.Length = std::clamp(Length + Amount, MinLength, MaxTime);
				}
			}
			else if (IsAcross && !IsGone && !Problem)
			{
				const bool IsActor = Item.Type == ObjectType::Actor;
				const ActorRuntime* Runtime = IsActor ? Core.FindActor(Item.Id) : nullptr;
				const bool IsMapped = Runtime && Runtime->IsComplete() && Runtime->HasSpans() && Item.Actor.End == EndMode::Hold;
				Problem = Runtime || !IsActor ? nullptr : Late;
				Kept += ShouldApply && !IsMapped && HasClips(Item.Actor);
				const auto AreaAt = [&Item, Runtime](float Local) { return static_cast<float>(Curves::LaneArea(Item.Lanes, &Runtime->TimeScaleArea, Param::TimeScale, 0, Local, Item.Actor.TimeScale)); };
				const float First = IsMapped ? AreaAt(From) : 0.0f, Last = IsMapped ? std::min(AreaAt(Inner), Runtime->SequenceLength) : 0.0f;
				const float Pause = Amount * Curves::LaneScalar(Item.Lanes, Param::TimeScale, 0, From, Item.Actor.TimeScale);
				for (u8 Layer = 0; IsMapped && !Problem && Layer <= Item.Actor.Layers.size(); ++Layer)
				{
					const LayerRuntime& Track = Runtime->Track(Layer);
					const auto Upper = std::upper_bound(Track.Spans.begin(), Track.Spans.end(), First, [](float Moment, const BlockSpan& Other) { return Moment < Other.Start; });
					const bool IsPaused = Amount > 0.0f && First > 0.0f && First < (Layer ? Track.Length : Runtime->SequenceLength);
					const BlockSpan* Current = Upper == Track.Spans.begin() ? nullptr : &*(Upper - 1);
					const float Offset = Current ? First - Current->Start : 0.0f;
					const bool IsSplit = IsPaused && Current && Offset > 0.0f && Offset < Current->Play * Current->Loops;
					if ((IsSplit || (Amount < 0.0f && First < Last)) && !CutSequence(Core, Item.Id, First, IsSplit ? First : Last, ShouldApply, Layer, IsSplit ? static_cast<s32>(Current->Block) : -1))
					{
						Problem = "The actor would have more than 256 animation blocks.";
					}
					xr_vector<ActorBlock>& Blocks = BlocksOf(Item.Actor, Layer);
					if (ShouldApply && IsPaused && !Problem && (IsSplit || Track.Starts.size() == Blocks.size()))
					{
						BlockStarts = IsSplit ? BlockStarts : Track.Starts;
						for (float& Begin : BlockStarts)
						{
							Begin += Begin >= First ? Pause : 0.0f;
						}
						if (!IsSplit && Current && Offset < Current->Duration)
						{
							Blocks[Current->Block].Hold = std::min(Blocks[Current->Block].Hold + Pause, MaxTime);
						}
						PlaceBlocks(Item, *Runtime, Blocks, BlockStarts);
					}
				}
				if (ShouldApply && IsMapped)
				{
					Refit(Core, Item);
				}
				if (!Problem && !TimelineUi::ShiftLanes(Item.Lanes, From, std::max(Amount, 0.0f) + From - To, HalfFrame, ShouldApply))
				{
					Problem = "A lane would pass 60:00:00 or 4096 keys.";
				}
				if (ShouldApply)
				{
					Item.Span.Length = std::max(Length + std::max(Amount, 0.0f) + From - Inner, MinLength);
				}
			}
			for (u32 Number = 0; Item.Type == ObjectType::Camera && !IsMoved && !IsGone && Number < Item.Camera.Shakes.size() && !Problem;)
			{
				ShakeClip& Clip = Item.Camera.Shakes[Number];
				const float Begin = Start + Clip.Start, Lo = std::max(At - Begin, 0.0f), Hi = Pivot - Begin, Rest = Clip.Length + std::max(Amount, 0.0f) + Lo - std::min(Hi, Clip.Length);
				const bool IsLater = Begin >= Pivot, IsCut = !IsLater && Begin + Clip.Length > At;
				if (ShouldApply && (IsLater || IsCut))
				{
					Undo.Touch(Data, Item.Id);
				}
				Problem = IsCut && !TimelineUi::ShiftLanes(Clip.Lanes, Lo, std::max(Amount, 0.0f) + Lo - Hi, HalfFrame, ShouldApply) ? "A lane would pass 60:00:00 or 4096 keys." : (IsLater || IsCut) && Begin + Clip.Length + Amount > MaxTime ? Past : nullptr;
				if (ShouldApply && IsCut && Rest < MinLength)
				{
					Item.Camera.Shakes.erase(Item.Camera.Shakes.begin() + Number);
					continue;
				}
				if (ShouldApply && (IsLater || IsCut))
				{
					Clip.Start = std::max((IsLater ? Begin + Amount : std::min(Begin, At)) - Item.Span.Start, 0.0f);
					Clip.Length = IsCut ? Rest : Clip.Length;
				}
				++Number;
			}
			if (ShouldApply && IsGone)
			{
				Data.Objects.erase(Data.Objects.begin() + Index);
				++Deleted;
				continue;
			}
			++Index;
		}
	}
	if (const Object* Rig = FindCamera(Data); Rig && !Problem)
	{
		SpanScratch.clear();
		for (const CameraBlock& Shot : Rig->Camera.Blocks)
		{
			SpanScratch.push_back({ Shot.Start, Shot.Length });
		}
		if (!Cameras::IsTrackValid(SpanScratch))
		{
			Undo.Cancel(Data);
			Problem = "Camera clips would lie inside another or overlap two others there.";
		}
	}
	if (Problem)
	{
		SetStatus(true, "%s", Problem);
		return;
	}
	const float Edges[2] = { Core.LoopStart, Core.LoopEnd };
	for (float* Edge : { &Core.LoopStart, &Core.LoopEnd })
	{
		*Edge = std::min(*Edge >= Pivot ? *Edge + Amount : *Edge >= At ? At : *Edge, MaxTime);
	}
	if (Core.HasLoopRange && (Edges[0] != Core.LoopStart || Edges[1] != Core.LoopEnd))
	{
		Undo.TouchLoop(Edges[0], Edges[1]);
	}
	SelectedKeys.clear();
	SetStatus(false, "%s: %u clips deleted, %u looping or unloaded actors keep their sequence.", Amount > 0.0f ? "Insert time" : "Remove time", Deleted, Kept);
}

void Editor::CollectSnapTargets(const Player& Core, bool HasPlayhead)
{
	SnapTargets.clear();
	if (HasPlayhead)
	{
		SnapTargets.push_back(Core.Time);
	}
	const xr_vector<Marker>& Markers = Core.Data.Header.Markers;
	for (u32 Index = 0; Index < Markers.size(); ++Index)
	{
		if (Drag != DragKind::MarkerMove || static_cast<s32>(Index) != DragMarker)
		{
			SnapTargets.push_back(Markers[Index].Time);
		}
	}
	u32 Ticks = 0;
	for (const Object& Item : Core.Data.Objects)
	{
		if (std::find(DragIds.begin(), DragIds.end(), Item.Id) != DragIds.end())
		{
			continue;
		}
		SnapTargets.push_back(Item.Span.Start);
		for (u32 Index = 0; Index < Item.Camera.Blocks.size(); ++Index)
		{
			const CameraBlock& Block = Item.Camera.Blocks[Index];
			if (Drag < DragKind::BlockMove || Drag > DragKind::BlockStretch || Item.Id != DragObject || static_cast<s32>(Index) != DragBlock)
			{
				SnapTargets.push_back(Item.Span.Start + Block.Start);
				SnapTargets.push_back(Item.Span.Start + Block.Start + Block.Length);
			}
		}
		for (u32 Index = 0; Index < Item.Camera.Shakes.size(); ++Index)
		{
			const ShakeClip& Clip = Item.Camera.Shakes[Index];
			if (Drag < DragKind::ShakeMove || Drag > DragKind::ShakeFadeOut || static_cast<s32>(Index) != DragShakeIndex)
			{
				SnapTargets.push_back(Item.Span.Start + Clip.Start);
				SnapTargets.push_back(Item.Span.Start + Clip.Start + Clip.Length);
			}
		}
		SnapTargets.push_back(ClipEnd(Item));
		const ActorRuntime* Runtime = Item.Type == ObjectType::Actor ? Core.FindActor(Item.Id) : nullptr;
		const float Length = Runtime ? Runtime->SequenceLength : 0.0f;
		const double Total = Length > 0.0f ? Curves::LaneArea(Item.Lanes, &Runtime->TimeScaleArea, Param::TimeScale, 0, Item.Span.Length, Item.Actor.TimeScale) : 0.0;
		for (u8 Layer = 0; Length > 0.0f && Layer <= std::min<size_t>(Item.Actor.Layers.size(), MaxActorLayers); ++Layer)
		{
			const xr_vector<MarkTick>& Marks = Runtime->Track(Layer).Ticks;
			for (double Rep = 0.0; !Marks.empty() && Rep * Length < Total && Ticks < MaxTicks && (Rep == 0.0 || Item.Actor.End != EndMode::Hold); Rep += 1.0)
			{
				const bool IsReversed = Item.Actor.End == EndMode::PingPong && std::fmod(Rep, 2.0) >= 1.0;
				for (u32 At = 0; At < Marks.size() && Ticks < MaxTicks; ++At)
				{
					const double Area = Rep * Length + (IsReversed ? Length - Marks[At].Sequence : Marks[At].Sequence);
					if (Area <= Total)
					{
						SnapTargets.push_back(Item.Span.Start + Curves::LaneInverseArea(Item.Lanes, &Runtime->TimeScaleArea, Param::TimeScale, 0, Item.Actor.TimeScale, Area, Item.Span.Length));
						++Ticks;
					}
				}
			}
		}
	}
}

bool Editor::Magnet(float Time, float& Offset) const
{
	float Best = TimelineUi::MagnetZone / Zoom;
	bool HasHit = false;
	for (float Target : SnapTargets)
	{
		if (std::fabs(Target - Time) <= Best)
		{
			Best = std::fabs(Target - Time);
			Offset = Target - Time;
			HasHit = true;
		}
	}
	return HasHit;
}

float Editor::SnapTime(float Time, bool IsShift) const
{
	float Offset = 0.0f;
	return IsShift || !Settings.IsSnapping ? Time : Magnet(Time, Offset) ? Time + Offset : std::round(Time / SnapGrid) * SnapGrid;
}

float Editor::SnapSpan(float Start, float Length, bool IsShift) const
{
	float Front = 0.0f, Back = 0.0f;
	if (IsShift || !Settings.IsSnapping)
	{
		return Start;
	}
	const bool HasFront = Magnet(Start, Front), HasBack = Magnet(Start + Length, Back);
	if (!HasFront && !HasBack)
	{
		return std::round(Start / SnapGrid) * SnapGrid;
	}
	return Start + (HasFront && (!HasBack || std::fabs(Front) <= std::fabs(Back)) ? Front : Back);
}

float Editor::TimeToX(float Time) const
{
	return TrackLeft + (Time - ScrollTime) * Zoom;
}

float Editor::XToTime(float X) const
{
	return ScrollTime + (X - TrackLeft) / Zoom;
}

template <typename TFunc>
void Editor::ForSelectedKeys(Player& Core, const char* Label, const TFunc& Visit)
{
	for (auto Ref = SelectedKeys.rbegin(); Ref != SelectedKeys.rend(); ++Ref)
	{
		Curve* Part = TimelineUi::FindCurve(Core.Data, *Ref);
		if (Part && Ref->Index < Part->Keys.size())
		{
			Object& Owner = *Edit(Core, Ref->ObjectId, Label);
			Visit(Owner, *Ref, *Part);
			if (Ref->Id == Param::TimeScale)
			{
				Refit(Core, Owner);
			}
		}
	}
}

float Editor::LanesHeight(const Scene& Data, u32 Id) const
{
	if (std::find(Expanded.begin(), Expanded.end(), Id) == Expanded.end())
	{
		return 0.0f;
	}
	float Height = RowHeight;
	for (const LaneSlot& Slot : Slots)
	{
		Height += Slot.ObjectId == Id ? SizeOf(Data, Id, SizeLane, Slot.Id, Slot.Layer) : 0.0f;
	}
	return Height;
}

float Editor::SizeOf(const Scene& Data, u32 ObjectId, u8 Kind, Param Id, u8 Layer) const
{
	const auto Found = std::find_if(Data.Sizes.begin(), Data.Sizes.end(), [&](const RowSize& Item) { return Item.ObjectId == ObjectId && Item.Kind == Kind && Item.Id == Id && Item.Layer == Layer; });
	return RowHeight * (Found != Data.Sizes.end() ? Found->Rows : Kind < SizeLane ? Settings.DefaultRow : Settings.DefaultLane);
}

void Editor::SizeEdge(Player& Core, const RowSize& Key, float Left, float Right, float Top, float Bottom)
{
	xr_vector<RowSize>& Sizes = Core.Data.Sizes;
	const ImGuiIO& Input = ImGui::GetIO();
	const bool IsGroup = Key.Kind == SizeRow && IsSelected(Key.ObjectId);
	const auto IsTouched = [&](const RowSize& Item) { return Item.Kind == Key.Kind && Item.Id == Key.Id && Item.Layer == Key.Layer && (Item.ObjectId == Key.ObjectId || (IsGroup && IsSelected(Item.ObjectId))); };
	ImGui::SetCursorScreenPos(ImVec2(Left, Bottom - TimelineUi::SizeZone));
	ImGui::InvisibleButton("##Size", ImVec2(std::max(Right - Left, 1.0f), TimelineUi::SizeZone));
	const bool IsActive = ImGui::IsItemActive();
	if (ImGui::IsItemHovered() || IsActive)
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeNS);
		ImGui::GetWindowDrawList()->AddLine(ImVec2(Left, Bottom - 1.0f), ImVec2(Right, Bottom - 1.0f), ImGui::GetColorU32(IsActive ? ImGuiCol_SeparatorActive : ImGuiCol_SeparatorHovered), 2.0f);
	}
	if (ImGui::IsItemActivated())
	{
		SizesBefore = Sizes;
		SizeTop = Top + ImGui::GetScrollY();
		SizeGrab = Bottom - Input.MousePos.y;
		WasLayoutDirty = IsLayoutDirty;
		IsSizeCancelled = false;
	}
	if (ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left))
	{
		IsLayoutDirty = std::erase_if(Sizes, IsTouched) > 0 || IsLayoutDirty;
		return;
	}
	if (IsActive && !IsSizeCancelled && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		Sizes = SizesBefore;
		IsLayoutDirty = WasLayoutDirty;
		IsSizeCancelled = true;
		return;
	}
	if (!IsActive || IsSizeCancelled || Input.MouseDelta.y == 0.0f)
	{
		return;
	}
	const float Rows = std::clamp(std::round(Input.MousePos.y + SizeGrab + ImGui::GetScrollY() - SizeTop) / RowHeight, 1.0f, Key.Kind < SizeLane ? MaxRowSize : MaxLaneSize);
	std::erase_if(Sizes, IsTouched);
	if (!IsGroup)
	{
		Sizes.push_back({ Key.ObjectId, Key.Kind, Key.Id, Key.Layer, Rows });
	}
	for (u32 Index = 0; IsGroup && Index < Selected.size(); ++Index)
	{
		Sizes.push_back({ Selected[Index], Key.Kind, Key.Id, Key.Layer, Rows });
	}
	IsLayoutDirty = true;
}

u8 Editor::SubAt(const Scene& Data, const Object& Target, float Y) const
{
	const auto Found = std::find_if(Rows.begin(), Rows.end(), [&Target](const RowItem& Item) { return !Item.IsFolder && Item.Id == Target.Id; });
	float Top = Found == Rows.end() ? Y : RowsTop + Found->Y + Found->Track;
	for (u8 Sub = 0; Sub + 1u < MaxShakeRows; ++Sub)
	{
		Top += SizeOf(Data, Target.Id, SizeSub, Param::Position, Sub);
		if (Y < Top)
		{
			return Sub;
		}
	}
	return static_cast<u8>(MaxShakeRows - 1);
}

LaneSlot* Editor::FindSlot(u32 ObjectId, Param Id, u8 Layer)
{
	const auto Found = std::find_if(Slots.begin(), Slots.end(), [&](const LaneSlot& Slot) { return Slot.ObjectId == ObjectId && Slot.Id == Id && Slot.Layer == Layer; });
	return Found == Slots.end() ? nullptr : &*Found;
}

void Editor::CollectLaneTargets(const Object& Target)
{
	LaneTargets.clear();
	if (Target.Type != ObjectType::Camera)
	{
		LaneTargets.push_back({ -1, Target.Span.Start, Target.Span.Length });
		return;
	}
	for (u32 Index = 0; Index < Target.Camera.Blocks.size(); ++Index)
	{
		LaneTargets.push_back({ static_cast<s16>(Index), Target.Span.Start + Target.Camera.Blocks[Index].Start, Target.Camera.Blocks[Index].Length });
	}
	for (u32 Index = 0; Index < Target.Camera.Shakes.size(); ++Index)
	{
		const ShakeClip& Clip = Target.Camera.Shakes[Index];
		LaneTargets.push_back({ static_cast<s16>(ShakeBase + static_cast<s32>(Index)), Target.Span.Start + Clip.Start, Clip.Length });
	}
}

void Editor::DrawLanes(Player& Core, const Object& Target, float Left, float Top)
{
	CollectLaneTargets(Target);
	s32 Removed = -1;
	float Y = Top;
	for (u32 Index = 0; Index < Slots.size(); ++Index)
	{
		LaneSlot& Slot = Slots[Index];
		if (Slot.ObjectId != Target.Id)
		{
			continue;
		}
		const s32 Count = 0;
		const auto Height = [&](s32 Component) { return SizeOf(Core.Data, Target.Id, static_cast<u8>(SizeLane + 1 + Component), Slot.Id, Slot.Layer); };
		float Total = 0.0f;
		for (s32 Component = -1; Component < Count; ++Component)
		{
			Total += Height(Component);
		}
		if (Drag < DragKind::LaneKeys && Drag != DragKind::PoseKey && !(Slot.IsFitted && ImGui::IsWindowHovered() && ImGui::IsMouseHoveringRect(ImVec2(TrackLeft, Y), ImVec2(TrackLeft + TrackWidth, Y + Total))))
		{
			FitLane(Target, Slot);
		}
		ImGui::PushID(static_cast<s32>(Slot.Id) * 256 + Slot.Layer);
		for (s32 Component = -1; Component < Count; ++Component)
		{
			bool ShouldRemove = false;
			const float Bottom = Y + Height(Component);
			ImGui::PushID(Component);
			SizeEdge(Core, { Target.Id, static_cast<u8>(SizeLane + 1 + Component), Slot.Id, Slot.Layer }, Left, TrackLeft - 4.0f, Y, Bottom);
			DrawLaneHeader(Core, Target, Slot, Component, ImRect(Left, Y, TrackLeft - 4.0f, Bottom), ShouldRemove);
			const ImRect Area(TrackLeft, Y + 1.0f, TrackLeft + TrackWidth, Bottom - 1.0f);
			DrawLane(Core, Target, Slot, Component, Area);
			ImGui::PopID();
			Removed = ShouldRemove ? static_cast<s32>(Index) : Removed;
			Y = Bottom;
		}
		ImGui::PopID();
	}
	Param Picked = Param::Count;
	u8 PickedLayer = 0;
	ImGui::SetCursorScreenPos(ImVec2(Left, Y + 1.0f));
	ImGui::SetNextItemWidth(std::max(TrackLeft - 4.0f - Left, 1.0f));
	if (TimelineUi::PickParam("##AddLane", "+ Lane", TimelineUi::LaneWhere(Target), TimelineUi::ShakeLayers(Target), [&](Param Id, u8 Layer) { return IsParamShown(Target, Id) && !FindSlot(Target.Id, Id, Layer); }, Picked, PickedLayer))
	{
		Slots.push_back({ Target.Id, Picked, PickedLayer });
	}
	ImGui::SetItemTooltip("Add lane");
	if (Removed < 0)
	{
		return;
	}
	const LaneSlot Gone = Slots[Removed];
	Object& Item = *Edit(Core, Target.Id, "Remove lane");
	for (const LaneTarget& Where : LaneTargets)
	{
		RemoveLane(Item, Where.Block, Gone.Id, Gone.Layer, LaneLocal(Item, Where.Block, Gone.Id, Core.Time));
	}
	Slots.erase(Slots.begin() + Removed);
	std::erase_if(SelectedKeys, [&Gone](const KeyRef& Ref) { return Ref.ObjectId == Gone.ObjectId && Ref.Id == Gone.Id && Ref.Layer == Gone.Layer; });
	if (Gone.Id == Param::TimeScale)
	{
		Refit(Core, Item);
	}
}

void Editor::DrawLaneHeader(Player& Core, const Object& Target, LaneSlot& Slot, s32 Component, const ImRect& Rect, bool& ShouldRemove)
{
	const ParamInfo& Meta = Info(Slot.Id);
	const bool IsVector = Meta.Components == 3, IsMain = Component < 0;
	const s32 Current = TimelineUi::FindTarget(LaneTargets, Target, Slot, Core.Time, true);
	const float Size = RowHeight, Chip = 0.6f * Size, Lead = IsMain ? (IsVector ? 3.0f * Chip + 2.0f : 0.0f) : 2.0f * Size;
	const float Free = std::max(Rect.GetWidth() - Lead - (Current >= 0 ? Size : 0.0f) - (IsMain ? Size : 0.0f), 2.0f);
	const float Split = !IsMain ? 0.0f : Current >= 0 ? 0.5f * Free : Free;
	const auto Place = [&Rect](float X, float ItemWidth)
	{
		ImGui::SetCursorScreenPos(ImVec2(Rect.Min.x + X, Rect.Min.y + 1.0f));
		ImGui::SetNextItemWidth(std::max(ItemWidth, 1.0f));
	};
	Place(IsMain ? 0.0f : Size, 0.0f);
	if (!IsMain)
	{
		ImGui::TextColored(ImGui::ColorConvertU32ToFloat4(TimelineUi::AxisColors[Component]), "%s", TimelineUi::AxisNames[Slot.Id == Param::Color][Component]);
	}
	for (u8 Axis = 0; IsMain && IsVector && Axis < 3; ++Axis)
	{
		const u8 Bit = static_cast<u8>(1u << Axis);
		Place(Axis * Chip, 0.0f);
		ImGui::PushID(Axis);
		ImGui::PushStyleColor(ImGuiCol_Button, (Slot.Shown & Bit) ? ImGui::ColorConvertU32ToFloat4(TimelineUi::WithAlpha(TimelineUi::AxisColors[Axis], 170)) : ImGui::GetStyleColorVec4(ImGuiCol_FrameBg));
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0.0f, ImGui::GetStyle().FramePadding.y));
		if (ImGui::Button(TimelineUi::AxisNames[Slot.Id == Param::Color][Axis], ImVec2(Chip, Size - 2.0f)))
		{
			Slot.Shown = ImGui::GetIO().KeyCtrl ? static_cast<u8>(Slot.Shown ^ Bit) : Slot.Shown == Bit ? u8(7) : Bit;
			Slot.Shown = Slot.Shown ? Slot.Shown : Bit;
		}
		ImGui::PopStyleVar();
		ImGui::PopStyleColor();
		ImGui::SetItemTooltip("Show axis");
		ImGui::PopID();
	}
	if (IsMain)
	{
		string64 Label;
		TimelineUi::SlotLabel(Label, Slot.Id, Slot.Layer, &Target);
		Param Picked = Slot.Id;
		u8 PickedLayer = Slot.Layer;
		Place(Lead, Split);
		if (TimelineUi::PickParam("##Param", Label, TimelineUi::LaneWhere(Target), TimelineUi::ShakeLayers(Target), [&](Param Id, u8 Layer) { return IsParamShown(Target, Id) && !FindSlot(Target.Id, Id, Layer); }, Picked, PickedLayer))
		{
			Slot.Id = Picked;
			Slot.Layer = PickedLayer;
		}
		ImGui::SetItemTooltip("Parameter");
		Place(Rect.GetWidth() - Size, 0.0f);
		ShouldRemove = ImGui::Button("x", ImVec2(Size - 2.0f, Size - 2.0f));
		ImGui::SetItemTooltip("Remove lane");
	}
	if (Current < 0)
	{
		return;
	}
	const LaneTarget& Where = LaneTargets[Current];
	const u8 Mask = Component >= 0 ? static_cast<u8>(1u << Component) : IsVector ? Slot.Mask : 1;
	const u8 First = (Mask & 1) ? 0 : (Mask & 2) ? 1 : 2;
	const Lane* Track = Curves::FindLane(LaneList(const_cast<Object&>(Target), Where.Block), Slot.Id, Slot.Layer);
	Generator Shown = Track ? Track->Curves[First].Gen : Generator {};
	Shown.Min = std::clamp(Shown.Min, Meta.Min, Meta.Max);
	Shown.Max = std::clamp(Shown.Max, Shown.Min, Meta.Max);
	Generator Value = Shown;
	const float Range = Meta.Max - Meta.Min, Speed = (Slot.Hi[First] - Slot.Lo[First]) * 0.005f;
	s32 Type = static_cast<s32>(Value.Type);
	Place(Lead + Split, Free - Split);
	if (TimelineUi::OpenCombo("##Wave", TimelineUi::WaveNames[Type]))
	{
		for (s32 Index = 0; Index < 11; ++Index)
		{
			Type = ImGui::Selectable(TimelineUi::WaveNames[Index], Index == Type) ? Index : Type;
		}
		ImGui::EndCombo();
	}
	ImGui::SetItemTooltip("Generator");
	Place(Lead + Free, 0.0f);
	if (ImGui::Button("~", ImVec2(Size - 2.0f, Size - 2.0f)))
	{
		ImGui::OpenPopup("##Shape");
	}
	ImGui::SetItemTooltip("Generator settings");
	if (ImGui::BeginPopup("##Shape"))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImGui::GetStyle().ItemInnerSpacing);
		for (u8 Axis = 0; IsMain && IsVector && Axis < 3; ++Axis)
		{
			const u8 Bit = static_cast<u8>(1u << Axis);
			ImGui::PushStyleColor(ImGuiCol_Button, (Slot.Mask & Bit) ? ImGui::ColorConvertU32ToFloat4(TimelineUi::WithAlpha(TimelineUi::AxisColors[Axis], 170)) : ImGui::GetStyleColorVec4(ImGuiCol_FrameBg));
			if (ImGui::Button(TimelineUi::AxisNames[Slot.Id == Param::Color][Axis], ImVec2(ImGui::GetFrameHeight(), 0.0f)) && Slot.Mask != Bit)
			{
				Slot.Mask = static_cast<u8>(Slot.Mask ^ Bit);
			}
			ImGui::PopStyleColor();
			ImGui::SetItemTooltip("Generator axis");
			Axis < 2 ? ImGui::SameLine() : void();
		}
		ImGui::PushItemWidth(ImGui::GetFontSize() * 7.0f);
		ImGui::DragFloat("Amplitude", &Value.Amplitude, Speed, -Range, Range, "%.3g", ImGuiSliderFlags_AlwaysClamp);
		ImGui::DragFloat("Frequency", &Value.Frequency, 0.01f, 0.0f, 100.0f, "%.2f Hz", ImGuiSliderFlags_AlwaysClamp);
		ImGui::DragFloat("Phase", &Value.Phase, 0.01f, -1000.0f, 1000.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
		ImGui::DragFloat("Min", &Value.Min, Speed, Meta.Min, Shown.Max, "%.3g", ImGuiSliderFlags_AlwaysClamp);
		ImGui::DragFloat("Max", &Value.Max, Speed, Shown.Min, Meta.Max, "%.3g", ImGuiSliderFlags_AlwaysClamp);
		ImGui::PopItemWidth();
		ImGui::PopStyleVar();
		ImGui::EndPopup();
	}
	Value.Type = static_cast<Wave>(Type);
	if (Value == Shown || !TimelineUi::HasField(Target, Where.Block, Slot.Id, Slot.Layer))
	{
		return;
	}
	Object& Item = *Edit(Core, Target.Id, "Generator");
	UseGameFov(Item, Where.Block, Slot.Id);
	Lane& Edited = Curves::EnsureLane(LaneList(Item, Where.Block), Slot.Id, Slot.Layer);
	const auto Apply = [&Value, &Shown](Generator& Gen, auto Field) { Gen.*Field = Value.*Field == Shown.*Field ? Gen.*Field : Value.*Field; };
	for (u8 Axis = 0; Axis < Meta.Components; ++Axis)
	{
		if ((Mask >> Axis) & 1)
		{
			Generator& Gen = Edited.Curves[Axis].Gen;
			Apply(Gen, &Generator::Type);
			Apply(Gen, &Generator::Amplitude);
			Apply(Gen, &Generator::Frequency);
			Apply(Gen, &Generator::Phase);
			Apply(Gen, &Generator::Min);
			Apply(Gen, &Generator::Max);
			Gen.Amplitude = Sanitize(Gen.Amplitude, -Range, Range, 0.0f);
			Gen.Frequency = Sanitize(Gen.Frequency, 0.0f, 100.0f, 1.0f);
			Gen.Phase = Sanitize(Gen.Phase, -1000.0f, 1000.0f, 0.0f);
			Gen.Min = Sanitize(Gen.Min, Meta.Min, Meta.Max, Meta.Min);
			Gen.Max = Sanitize(Gen.Max, Gen.Min, Meta.Max, Meta.Max);
		}
	}
	if (Slot.Id == Param::TimeScale)
	{
		Refit(Core, Item);
	}
}

void Editor::DrawLane(Player& Core, const Object& Target, LaneSlot& Slot, s32 Component, const ImRect& Rect)
{
	const ParamInfo& Meta = Info(Slot.Id);
	const ImGuiIO& Input = ImGui::GetIO();
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	const float Top = Rect.Min.y + 4.0f, Bottom = Rect.Max.y - 4.0f;
	u8 YAxis = 0;
	const auto ToY = [&](float Value) { return Bottom - (Value - Slot.Lo[YAxis]) / (Slot.Hi[YAxis] - Slot.Lo[YAxis]) * (Bottom - Top); };
	const u8 From = static_cast<u8>(std::max(Component, 0)), To = Component < 0 ? Meta.Components : static_cast<u8>(Component + 1);
	ImGui::SetCursorScreenPos(Rect.Min);
	ImGui::InvisibleButton("##Lane", Rect.GetSize(), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
	CanvasId = ImGui::IsItemActive() ? ImGui::GetItemID() : CanvasId;
	const bool IsHovered = ImGui::IsItemHovered() && Drag == DragKind::None;
	HoverLane(Target.Id, Slot);
	ImDrawList* List = ImGui::GetWindowDrawList();
	List->PushClipRect(Rect.Min, Rect.Max, true);
	List->AddRectFilled(Rect.Min, Rect.Max, ImGui::GetColorU32(ImGuiCol_FrameBg));
	TimelineUi::KeyHit Hit;

	const Curve* HotPart = nullptr;
	u8 HotAxis = 0;
	s32 HotBlock = -1;
	float HotStatic = 0.0f, HotFrom = 0.0f, HotTo = 0.0f;
	for (u32 Index = 0; Index < LaneTargets.size(); ++Index)
	{
		const LaneTarget& Where = LaneTargets[Index];
		if (!TimelineUi::HasField(Target, Where.Block, Slot.Id, Slot.Layer))
		{
			continue;
		}
		const Lane* Found = Curves::FindLane(LaneList(Target, Where.Block), Slot.Id, Slot.Layer);
		const float Begin = std::max(TimeToX(Where.Start), Rect.Min.x), End = std::min(TimeToX(Where.Start + Where.Length), Rect.Max.x);
		const auto TimeAt = [&](float X) { return LaneLocal(Target, Where.Block, Slot.Id, XToTime(X)); };
		const auto XAt = [&](float Time) { return TimeToX(LaneMoment(Target, Where.Block, Slot.Id, Time)); };
		const float Earliest = TimeAt(Rect.Min.x - TimelineUi::KeyZone), Latest = TimeAt(Rect.Max.x + TimelineUi::KeyZone);
		const float Tension = Target.Type == ObjectType::Camera && Slot.Id == Param::Position && Where.Block >= 0 && Where.Block < static_cast<s32>(Target.Camera.Blocks.size()) ? Target.Camera.Blocks[Where.Block].Tension : 1.0f;
		for (u8 Axis = From; Axis < To; ++Axis)
		{
			YAxis = Axis;
			const Curve& Part = Found ? Found->Curves[Axis] : TimelineUi::EmptyCurve;
			const float Static = TimelineUi::Fallback(Target, Where.Block, Slot.Id, Slot.Layer, Axis);
			const bool IsOn = Meta.Components != 3 || ((Slot.Shown >> Axis) & 1);
			const u32 Color = TimelineUi::WithAlpha(Meta.Components == 3 ? TimelineUi::AxisColors[Axis] : Target.Color, IsOn ? 255u : 60u);
			const auto YAt = [&](float X) { return ToY(Curves::Value(Part, TimeAt(X), Static, Tension)); };
			TimelineUi::Plot(List, Begin, End, YAt, Color, 2.0f);
			if (Part.Gen.Type != Wave::None)
			{
				TimelineUi::Plot(List, Begin, End, [&](float X) { return ToY(Curves::Result(Part, TimeAt(X), Static, Meta, Tension)); }, Lighten(Color), 1.0f);
			}
			const auto Next = std::upper_bound(Part.Keys.begin(), Part.Keys.end(), TimeAt(Input.MousePos.x), [](float Moment, const Key& Item) { return Moment < Item.Time; });
			if (IsHovered && IsOn && Next != Part.Keys.begin() && Next != Part.Keys.end() && Input.MousePos.x >= Begin && Input.MousePos.x <= End)
			{
				const float Left = XAt((Next - 1)->Time), Right = XAt(Next->Time);
				float Distance = FLT_MAX;
				for (float Step = -TimelineUi::SegmentZone; Step <= TimelineUi::SegmentZone; Step += 1.0f)
				{
					const float X = std::clamp(Input.MousePos.x + Step, Left, Right);
					Distance = std::min(Distance, std::hypot(X - Input.MousePos.x, YAt(X) - Input.MousePos.y));
				}
				if (Distance <= TimelineUi::SegmentZone && TimelineUi::KeyZone + 1.0f + Distance < Hit.Score)
				{
					Hit = { Index, Axis, static_cast<u32>(Next - Part.Keys.begin()) - 1, 3, TimelineUi::KeyZone + 1.0f + Distance, true, Input.MousePos };
					HotPart = &Part;
					HotAxis = Axis;
					HotBlock = Where.Block;
					HotStatic = Static;
					HotFrom = std::max(Left, Begin);
					HotTo = std::min(Right, End);
				}
			}
			const auto First = std::lower_bound(Part.Keys.begin(), Part.Keys.end(), Earliest, [](const Key& Item, float Moment) { return Item.Time < Moment; });
			for (u32 At = static_cast<u32>(First - Part.Keys.begin()); At < Part.Keys.size() && Part.Keys[At].Time <= Latest; ++At)
			{
				const Key& Item = Part.Keys[At];
				const ImVec2 Point(XAt(Item.Time), ToY(Item.Value));
				const KeyRef Ref { Target.Id, Where.Block, Slot.Id, Slot.Layer, Axis, At };
				const bool IsPicked = IsOn && IsKeySelected(Ref);
				const bool IsNear = IsOn && IsHovered && std::fabs(Point.x - Input.MousePos.x) + std::fabs(Point.y - Input.MousePos.y) <= 2.0f * TimelineUi::KeyZone;
				if (IsOn && Drag == DragKind::BoxKeys && Marquee.Contains(Point))
				{
					KeyGroup.push_back(Ref);
				}
				for (u8 Side = 1; (IsPicked || IsNear) && Side < 3; ++Side)
				{
					if (Side == 2 ? At + 1 < Part.Keys.size() : At > 0)
					{
						const ImVec2 Arm = TimelineUi::TangentOf(Part, At, Side == 2, Static);
						TimelineUi::DrawHandle(List, Hit, Input.MousePos, Point, ImVec2(XAt(Item.Time + Arm.x), ToY(Item.Value + Arm.y)), Index, Axis, At, Side);
					}
				}
				const u32 Fill = TimelineUi::WithAlpha(IsPicked ? ImGui::GetColorU32(ImGuiCol_Text) : Color, !IsOn ? 60u : Item.Time > Where.Length ? 110u : 255u);
				List->AddNgonFilled(Point, TimelineUi::KeyRadius, Fill, 4);
				List->AddNgon(Point, TimelineUi::KeyRadius, TimelineUi::Separator, 4);
				if (IsOn)
				{
					TimelineUi::Test(Hit, Input.MousePos, Point, Index, Axis, At, 0);
				}
			}
		}
	}
	if (IsHovered && Hit.HasHit && Hit.Handle == 3)
	{
		YAxis = HotAxis;
		TimelineUi::Plot(List, HotFrom, HotTo, [&](float X) { return ToY(Curves::Value(*HotPart, LaneLocal(Target, HotBlock, Slot.Id, XToTime(X)), HotStatic)); }, ImGui::GetColorU32(ImGuiCol_Text), 2.0f);
	}
	else if (IsHovered && Hit.HasHit)
	{
		List->AddCircle(Hit.Point, TimelineUi::KeyRadius + 3.0f, ImGui::GetColorU32(ImGuiCol_Text));
	}
	if (IsHovered && Hit.HasHit)
	{
		ImGui::SetMouseCursor(Hit.Handle == 3 ? ImGuiMouseCursor_ResizeNS : ImGuiMouseCursor_Hand);
	}
	for (const LaneTarget& Where : LaneTargets)
	{
		const Lane* Found = Rect.GetHeight() >= 2.0f * RowHeight ? Curves::FindLane(LaneList(Target, Where.Block), Slot.Id, Slot.Layer) : nullptr;
		for (u8 Axis = From; Found && Axis < To; ++Axis)
		{
			const xr_vector<Key>& Keys = Found->Curves[Axis].Keys;
			if (Keys.empty() || (Meta.Components == 3 && !((Slot.Shown >> Axis) & 1)))
			{
				continue;
			}
			const u32 Color = Meta.Components == 3 ? TimelineUi::AxisColors[Axis] : ImGui::GetColorU32(ImGuiCol_Text);
			YAxis = Axis;
			const float Head = LaneMoment(Target, Where.Block, Slot.Id, Keys.front().Time), Tail = LaneMoment(Target, Where.Block, Slot.Id, Keys.back().Time);
			TimelineUi::EndTag(List, Rect, ImVec2(TimeToX(Head), ToY(Keys.front().Value)), Head, Keys.front().Value, Core.Data.Header.FrameRate, Color);
			if (Keys.size() > 1)
			{
				TimelineUi::EndTag(List, Rect, ImVec2(TimeToX(Tail), ToY(Keys.back().Value)), Tail, Keys.back().Value, Core.Data.Header.FrameRate, Color);
			}
			break;
		}
	}
	const u8 Label = TimelineUi::LabelAxis(Meta.Components == 3 ? Slot.Shown : u8(1));
	TimelineUi::RangeLabels(List, Rect, Slot.Lo[Label], Slot.Hi[Label], Top, Bottom);
	List->PopClipRect();
	const bool IsPressed = ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left);
	if (IsPressed && Hit.HasHit)
	{
		const LaneTarget& Where = LaneTargets[Hit.Target];
		const KeyRef Ref { Target.Id, Where.Block, Slot.Id, Slot.Layer, Hit.Component, Hit.Index };
		const Curve* Source = TimelineUi::FindCurve(Core.Data, Ref);
		const u32 Head = Hit.Handle == 1 ? Hit.Index - 1 : Hit.Index;
		if ((Hit.Handle == 1 || Hit.Handle == 2) && Source && Head + 1 < Source->Keys.size() && Source->Keys[Head].Mode != Interp::Bezier)
		{
			const float Static = TimelineUi::Fallback(Target, Where.Block, Slot.Id, Slot.Layer, Hit.Component);
			const ImVec2 Out = TimelineUi::TangentOf(*Source, Head, true, Static), In = TimelineUi::TangentOf(*Source, Head + 1, false, Static);
			Edit(Core, Target.Id, "Tangent");
			Curve* Live = TimelineUi::FindCurve(Core.Data, Ref);
			Live->Keys[Head].Mode = Interp::Bezier;
			Live->Keys[Head].OutTime = Out.x;
			Live->Keys[Head].OutValue = Out.y;
			Live->Keys[Head + 1].InTime = In.x;
			Live->Keys[Head + 1].InValue = In.y;
		}
		Drag = TimelineUi::HitDrags[Hit.Handle + (Hit.Handle == 3 && Input.KeyCtrl ? 1 : 0)];
		DragObject = Target.Id;
		DragKey = Ref;
		DragKeyStart = *TimelineUi::FindKey(Core.Data, Ref);
		DragKeyTime = LaneMoment(Target, Where.Block, Slot.Id, DragKeyStart.Time);
		DragTime = XToTime(Input.MousePos.x);
		DragOriginY = Input.MousePos.y;
		DragLaneHeight = std::max(Bottom - Top, 1.0f);
		DragSpan = TimelineUi::DragRange(Slot.Id, Slot.Hi[Ref.Component] - Slot.Lo[Ref.Component], DragLaneHeight);
		IsDragCopy = Drag == DragKind::LaneKeys && Input.KeyCtrl;
		HasDragMoved = false;
		GroupKeys(Target, Where.Block, Slot, static_cast<s32>(Hit.Component), Ref, HalfFrame);
		WasPressSelected = Drag == DragKind::LaneKeys && IsKeySelected(Ref);
		if (Hit.Handle == 3)
		{
			KeyGroup.push_back({ Ref.ObjectId, Ref.Block, Ref.Id, Ref.Layer, Ref.Component, Ref.Index + 1 });
			SelectKeys(false);
		}
		else if (Drag == DragKind::LaneKeys && !WasPressSelected)
		{
			SelectKeys(Input.KeyCtrl);
		}
		Focus = FocusKind::Keys;
		if (Drag == DragKind::LaneKeys && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) && ImGui::GetTime() - AddedKeyTime > Input.MouseDoubleClickTime)
		{
			SelectKeys(false);
			RemoveKeys(Core);
			Drag = DragKind::None;
		}
	}
	else if (IsPressed)
	{
		StartBox(DragKind::BoxKeys);
	}
	if (ImGui::IsItemDeactivated() && Drag == DragKind::BoxKeys && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Left] < TimelineUi::EdgeZone * TimelineUi::EdgeZone && ImGui::IsMouseReleased(ImGuiMouseButton_Left))
	{
		const ImVec2 At = Input.MouseClickedPos[ImGuiMouseButton_Left];
		AddLaneKey(Core, Target, Slot, Component, XToTime(At.x), (Bottom - At.y) / std::max(Bottom - Top, 1.0f));
		AddedKeyTime = ImGui::GetTime();
	}
	if (ImGui::IsItemDeactivated() && ImGui::IsMouseReleased(ImGuiMouseButton_Right) && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Right] < 16.0f && Hit.HasHit)
	{
		const LaneTarget& Where = LaneTargets[Hit.Target];
		GroupKeys(Target, Where.Block, Slot, static_cast<s32>(Hit.Component), { Target.Id, Where.Block, Slot.Id, Slot.Layer, Hit.Component, Hit.Index }, HalfFrame);
		if (!IsKeySelected(KeyGroup[0]))
		{
			SelectKeys(false);
		}
		PopupRequest = "##KeyMenu";
	}
	if (ImGui::IsItemDeactivated() && ImGui::IsMouseReleased(ImGuiMouseButton_Right) && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Right] < 16.0f && !Hit.HasHit)
	{
		const float At = XToTime(Input.MouseClickedPos[ImGuiMouseButton_Right].x);
		const s32 Index = TimelineUi::FindTarget(LaneTargets, Target, Slot, At, false);
		if (Index >= 0)
		{
			CurveMenu = { Target.Id, LaneTargets[Index].Block, Slot.Id, Slot.Layer, Info(Slot.Id).Components == 3 ? Slot.Shown : u8(1), LaneLocal(Target, LaneTargets[Index].Block, Slot.Id, At), 0.25f * (Slot.Hi[TimelineUi::LabelAxis(Slot.Shown)] - Slot.Lo[TimelineUi::LabelAxis(Slot.Shown)]) };
			PopupRequest = "##CurveMenu";
		}
	}
}

void Editor::DrawKeyTicks(ImDrawList* List, const xr_vector<Lane>& Lanes, float Start, float Y, u32 Color, const CameraBlock* Path) const
{
	const float Earliest = XToTime(TrackLeft) - Start, Latest = XToTime(TrackLeft + TrackWidth) - Start;
	for (const Lane& Track : Lanes)
	{
		const CameraBlock* Warp = Track.Id <= Param::Rotation ? Path : nullptr;
		const float Lo = Warp ? Cameras::PathTime(*Warp, Earliest) : Earliest, Hi = Warp ? Cameras::PathTime(*Warp, Latest) : Latest;
		for (const Curve& Part : Track.Curves)
		{
			for (auto Item = std::lower_bound(Part.Keys.begin(), Part.Keys.end(), Lo, [](const Key& Other, float Moment) { return Other.Time < Moment; }); Item != Part.Keys.end() && Item->Time <= Hi; ++Item)
			{
				List->AddNgonFilled(ImVec2(TimeToX(Start + (Warp ? Cameras::LocalFromPath(*Warp, Item->Time) : Item->Time)), Y), TimelineUi::TickRadius, Color, 4);
			}
		}
	}
}

void Editor::FitLane(const Object& Target, LaneSlot& Slot)
{
	const ParamInfo& Meta = Info(Slot.Id);
	float Lo[3] = { FLT_MAX, FLT_MAX, FLT_MAX }, Hi[3] = { -FLT_MAX, -FLT_MAX, -FLT_MAX };
	for (const LaneTarget& Where : LaneTargets)
	{
		const Lane* Found = Curves::FindLane(LaneList(Target, Where.Block), Slot.Id, Slot.Layer);
		for (u8 Axis = 0; TimelineUi::HasField(Target, Where.Block, Slot.Id, Slot.Layer) && Axis < Meta.Components; ++Axis)
		{
			const Curve& Part = Found ? Found->Curves[Axis] : TimelineUi::EmptyCurve;
			const float Static = TimelineUi::Fallback(Target, Where.Block, Slot.Id, Slot.Layer, Axis);
			float PartLo = Part.Keys.empty() ? Static : FLT_MAX, PartHi = Part.Keys.empty() ? Static : -FLT_MAX;
			for (const Key& Item : Part.Keys)
			{
				PartLo = std::min({ PartLo, Item.Value, Item.Value + Item.InValue, Item.Value + Item.OutValue });
				PartHi = std::max({ PartHi, Item.Value, Item.Value + Item.InValue, Item.Value + Item.OutValue });
			}
			const float Amplitude = Part.Gen.Type == Wave::None ? 0.0f : std::fabs(Part.Gen.Amplitude);
			Lo[Axis] = std::min({ Lo[Axis], PartLo, std::max(PartLo - Amplitude, std::max(Meta.Min, Part.Gen.Min)) });
			Hi[Axis] = std::max({ Hi[Axis], PartHi, std::min(PartHi + Amplitude, std::min(Meta.Max, Part.Gen.Max)) });
		}
	}
	const float MinSpan = Slot.Id == Param::Position ? 2.0f : Slot.Id == Param::BonePosition || Slot.Id == Param::BoneScale ? 0.2f : std::min(0.25f * (Meta.Max - Meta.Min), 10.0f);
	for (u32 Axis = 0; Axis < 3; ++Axis)
	{
		const bool IsSeen = Lo[Axis] <= Hi[Axis];
		const float Middle = IsSeen ? 0.5f * (Lo[Axis] + Hi[Axis]) : Meta.Min + 0.5f * MinSpan, Reach = 0.75f * (IsSeen ? std::max(Hi[Axis] - Lo[Axis], MinSpan) : MinSpan);
		const float Shift = std::max(Meta.Min - (Middle - Reach), 0.0f) + std::min(Meta.Max - (Middle + Reach), 0.0f);
		Slot.Lo[Axis] = Middle - Reach + Shift;
		Slot.Hi[Axis] = Middle + Reach + Shift;
	}
	Slot.IsFitted = true;
}

void Editor::AddLaneKey(Player& Core, const Object& Target, const LaneSlot& Slot, s32 Component, float Time, float NewValue)
{
	s32 Index = TimelineUi::FindTarget(LaneTargets, Target, Slot, Time, false);
	if (Index < 0)
	{
		SetStatus(false, Slot.Id >= Param::ShakeInfluence ? "Keys go inside a shake on row %u." : "Keys go inside the clip.", Slot.Layer + 1u);
		return;
	}
	const auto Gap = [&](const LaneTarget& Where)
	{
		const Lane* Track = Curves::FindLane(LaneList(Target, Where.Block), Slot.Id, Slot.Layer);
		const float Static = TimelineUi::Fallback(Target, Where.Block, Slot.Id, Slot.Layer, 0);
		return std::fabs(((Track ? Curves::Value(Track->Curves[0], LaneLocal(Target, Where.Block, Slot.Id, Time), Static) : Static) - Slot.Lo[0]) / (Slot.Hi[0] - Slot.Lo[0]) - NewValue);
	};
	for (u32 Other = 0; LaneTargets[Index].Block >= ShakeBase && Other < LaneTargets.size(); ++Other)
	{
		const LaneTarget& Item = LaneTargets[Other];
		const bool IsInside = Item.Block >= ShakeBase && Time >= Item.Start && Time <= Item.Start + Item.Length && TimelineUi::HasField(Target, Item.Block, Slot.Id, Slot.Layer);
		Index = IsInside && Gap(Item) < Gap(LaneTargets[Index]) ? static_cast<s32>(Other) : Index;
	}

	const LaneTarget* Found = &LaneTargets[Index];
	DragIds.clear();
	CollectSnapTargets(Core, true);
	Time = std::clamp(SnapTime(Time, ImGui::GetIO().KeyShift), Found->Start, Found->Start + Found->Length);
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate), Local = LaneLocal(Target, Found->Block, Slot.Id, Time);
	Object& Item = *Edit(Core, Target.Id, "Add key");
	UseGameFov(Item, Found->Block, Slot.Id);
	Lane& Track = Curves::EnsureLane(LaneList(Item, Found->Block), Slot.Id, Slot.Layer);
	const ParamInfo& Meta = Info(Slot.Id);
	const u8 First = static_cast<u8>(std::max(Component, 0)), Last = Component < 0 ? Meta.Components : static_cast<u8>(Component + 1);
	float Values[3] = {}, Levels[3] = {};
	const auto IsOn = [&](u8 Axis) { return Meta.Components != 3 || ((Slot.Shown >> Axis) & 1); };
	for (u8 Axis = First; Axis < Last; ++Axis)
	{
		Values[Axis] = IsOn(Axis) ? Curves::Value(Track.Curves[Axis], Local, TimelineUi::Fallback(Item, Found->Block, Slot.Id, Slot.Layer, Axis)) : FLT_MAX;
		Levels[Axis] = IsOn(Axis) ? (Values[Axis] - Slot.Lo[Axis]) / (Slot.Hi[Axis] - Slot.Lo[Axis]) : FLT_MAX;
	}
	const u8 Picked = TimelineUi::Nearest(Levels, First, Last, NewValue);
	KeyGroup.clear();
	for (u8 Axis = First; Axis < Last; ++Axis)
	{
		if (!IsOn(Axis) || Axis != Picked)
		{
			continue;
		}
		Curve& Part = Track.Curves[Axis];
		Curves::SetKey(Part, Local, std::clamp(Slot.Lo[Axis] + NewValue * (Slot.Hi[Axis] - Slot.Lo[Axis]), Meta.Min, Meta.Max), HalfFrame);
		const u32 At = Curves::KeyAt(Part, Local, HalfFrame);
		if (At != u32(-1))
		{
			KeyGroup.push_back({ Target.Id, Found->Block, Slot.Id, Slot.Layer, Axis, At });
		}
	}
	SelectKeys(ImGui::GetIO().KeyCtrl);
	if (Slot.Id == Param::TimeScale)
	{
		Refit(Core, Item);
	}
}

void Editor::GroupKeys(const Object& Target, s32 Block, const LaneSlot& Slot, s32 Component, const KeyRef& Hit, float HalfFrame)
{
	KeyGroup.assign(1, Hit);
	const Lane* Found = Curves::FindLane(LaneList(const_cast<Object&>(Target), Block), Slot.Id, Slot.Layer);
	for (u8 Axis = 0; Found && Component < 0 && Axis < Info(Slot.Id).Components; ++Axis)
	{
		const u32 At = Curves::KeyAt(Found->Curves[Axis], Found->Curves[Hit.Component].Keys[Hit.Index].Time, HalfFrame);
		if (Axis != Hit.Component && At != u32(-1) && ((Slot.Shown >> Axis) & 1))
		{
			KeyGroup.push_back({ Hit.ObjectId, Hit.Block, Hit.Id, Hit.Layer, Axis, At });
		}
	}
}

void Editor::SelectKeys(bool IsToggle)
{
	const bool IsAdding = !IsToggle || KeyGroup.empty() || !IsKeySelected(KeyGroup[0]);
	if (!IsToggle)
	{
		SelectedKeys.clear();
	}
	for (const KeyRef& Ref : KeyGroup)
	{
		const auto Found = std::lower_bound(SelectedKeys.begin(), SelectedKeys.end(), Ref);
		const bool IsPresent = Found != SelectedKeys.end() && *Found == Ref;
		if (IsAdding && !IsPresent)
		{
			SelectedKeys.insert(Found, Ref);
		}
		else if (!IsAdding && IsPresent)
		{
			SelectedKeys.erase(Found);
		}
	}
	Focus = SelectedKeys.empty() ? FocusKind::Objects : FocusKind::Keys;
}

bool Editor::IsKeySelected(const KeyRef& Ref) const
{
	return std::binary_search(SelectedKeys.begin(), SelectedKeys.end(), Ref);
}

void Editor::DragLane(Player& Core)
{
	const ImGuiIO& Input = ImGui::GetIO();
	const float Rise = DragLaneHeight > 0.0f ? (DragOriginY - Input.MousePos.y) / DragLaneHeight * (Input.KeyShift ? 0.1f : 1.0f) : 0.0f;
	if (Drag >= DragKind::LaneBend)
	{
		Object* Owner = Edit(Core, DragKey.ObjectId, Drag == DragKind::LaneBend ? "Bend segment" : "Bezier handle");
		Curve* Part = TimelineUi::FindCurve(Core.Data, DragKey);
		Key* Item = TimelineUi::FindKey(Core.Data, DragKey);
		const bool IsIn = Drag == DragKind::HandleIn;
		const u32 Other = IsIn ? DragKey.Index - 1 : DragKey.Index + 1;
		if (!Owner || !Item || Other >= Part->Keys.size())
		{
			return;
		}
		Key& Near = Part->Keys[Other];
		const float Width = std::fabs(Near.Time - Item->Time), Shift = XToTime(Input.MousePos.x) - DragTime;
		if (Drag == DragKind::LaneBend)
		{
			const float Delta = Near.Value - Item->Value, Third = 3.0f * DragKeyStart.OutTime / Width;
			const float Start = DragKeyStart.Mode != Interp::Bezier ? 0.0f : Third >= 1.0f ? 0.5f * (Third - 1.0f) : Third - 1.0f;
			const float Bend = std::clamp(Start + (Delta > 0.0f ? -2.0f : 2.0f) * Rise, -1.0f, 1.0f), Amount = std::fabs(Bend);
			const float CornerTime = Bend > 0.0f ? Width : 0.0f, CornerValue = Bend > 0.0f ? Item->Value : Near.Value;
			Item->Mode = Interp::Bezier;
			Item->OutTime = std::lerp(Width / 3.0f, CornerTime, Amount);
			Item->OutValue = std::lerp(Delta / 3.0f, CornerValue - Item->Value, Amount);
			Near.InTime = std::lerp(-Width / 3.0f, CornerTime - Width, Amount);
			Near.InValue = std::lerp(-Delta / 3.0f, CornerValue - Near.Value, Amount);
		}
		else
		{
			const float Time = IsIn ? std::clamp(DragKeyStart.InTime + Shift, -Width, 0.0f) : std::clamp(DragKeyStart.OutTime + Shift, 0.0f, Width);
			const float Value = Sanitize((IsIn ? DragKeyStart.InValue : DragKeyStart.OutValue) + Rise * DragSpan, -100000.0f, 100000.0f, 0.0f);
			const bool HasOpposite = IsIn ? DragKey.Index + 1 < Part->Keys.size() && Item->Mode == Interp::Bezier : DragKey.Index > 0 && Part->Keys[DragKey.Index - 1].Mode == Interp::Bezier;
			(IsIn ? Item->InTime : Item->OutTime) = Time;
			(IsIn ? Item->InValue : Item->OutValue) = Value;
			Item->IsBroken = Item->IsBroken || Input.KeyCtrl;
			if (HasOpposite && !Item->IsBroken && std::fabs(Time) > 1e-5f)
			{
				(IsIn ? Item->OutValue : Item->InValue) = Sanitize((IsIn ? DragKeyStart.OutTime : DragKeyStart.InTime) * Value / Time, -100000.0f, 100000.0f, 0.0f);
			}
		}
		if (DragKey.Id == Param::TimeScale)
		{
			Refit(Core, *Owner);
		}
		return;
	}
	if (!HasDragMoved)
	{
		BeginKeyMove(Core, Drag == DragKind::LaneSegment ? "Move segment" : IsDragCopy ? "Copy keys" : "Move keys");
	}
	MoveKeys(Core, Drag == DragKind::LaneSegment ? 0.0f : SnapTime(DragKeyTime + XToTime(Input.MousePos.x) - DragTime, Input.KeyShift) - DragKeyTime, Rise);
}

void Editor::BeginKeyMove(Player& Core, const char* Label)
{
	DragKeyPrevious = SelectedKeys;
	DragCurves.clear();
	DragMoves.clear();
	DragIds.clear();
	CollectSnapTargets(Core, true);
	for (const KeyRef& Ref : SelectedKeys)
	{
		const Curve* Part = TimelineUi::FindCurve(Core.Data, Ref);
		KeyRef Owner = Ref;
		Owner.Index = 0;
		if (!Part || Ref.Index >= Part->Keys.size())
		{
			continue;
		}
		if (DragCurves.empty() || !(DragCurves.back().Ref == Owner))
		{
			Edit(Core, Ref.ObjectId, Label);
			const LaneSlot* Slot = FindSlot(Ref.ObjectId, Ref.Id, Ref.Layer);
			DragCurves.push_back({ Owner, Slot ? TimelineUi::DragRange(Ref.Id, Slot->Hi[Ref.Component] - Slot->Lo[Ref.Component], DragLaneHeight) : DragSpan, *Part });
		}
		DragMoves.push_back({ static_cast<u32>(DragCurves.size() - 1), Ref.Index });
	}
	TimelineUi::WalkMoves(DragCurves, DragMoves, [this, &Core](const LaneDrag& Item, u32 At, bool IsMoved)
	{
		if (!IsMoved)
		{
			SnapTargets.push_back(LaneMoment(*Find(Core.Data, Item.Ref.ObjectId), Item.Ref.Block, Item.Ref.Id, Item.Base.Keys[At].Time));
		}
	});
}

void Editor::MoveKeys(Player& Core, float Shift, float Rise)
{
	float Earliest = MaxTime, Latest = 0.0f;
	for (const KeyMove& Move : DragMoves)
	{
		Earliest = std::min(Earliest, DragCurves[Move.Source].Base.Keys[Move.Index].Time);
		Latest = std::max(Latest, DragCurves[Move.Source].Base.Keys[Move.Index].Time);
	}
	Shift = std::clamp(Shift, -Earliest, MaxTime - Latest);
	DragTimes.clear();
	for (const KeyMove& Move : DragMoves)
	{
		const LaneDrag& Item = DragCurves[Move.Source];
		const Object& Owner = *Find(Core.Data, Item.Ref.ObjectId);
		DragTimes.push_back(std::clamp(LaneLocal(Owner, Item.Ref.Block, Item.Ref.Id, LaneMoment(Owner, Item.Ref.Block, Item.Ref.Id, Item.Base.Keys[Move.Index].Time) + Shift), 0.0f, MaxTime));
	}
	for (LaneDrag& Item : DragCurves)
	{
		Item.Live = TimelineUi::FindCurve(Core.Data, Item.Ref);
		if (Item.Live)
		{
			Item.Live->Keys.clear();
		}
	}
	TimelineUi::WalkMoves(DragCurves, DragMoves, [this](const LaneDrag& Item, u32 At, bool IsMoved)
	{
		if (Item.Live && (!IsMoved || IsDragCopy))
		{
			Item.Live->Keys.push_back(Item.Base.Keys[At]);
		}
	});
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	for (u32 Index = 0; Index < DragMoves.size(); ++Index)
	{
		const KeyMove& Move = DragMoves[Index];
		const LaneDrag& Item = DragCurves[Move.Source];
		const ParamInfo& Meta = Info(Item.Ref.Id);
		Key Moved = Item.Base.Keys[Move.Index];
		Moved.Time = DragTimes[Index];
		Moved.Value = std::clamp(Moved.Value + Rise * Item.Span, Meta.Min, Meta.Max);
		if (Item.Live)
		{
			Curves::PlaceKey(*Item.Live, Moved, HalfFrame);
		}
	}
	SelectedKeys.clear();
	for (u32 Index = 0; Index < DragMoves.size(); ++Index)
	{
		const LaneDrag& Item = DragCurves[DragMoves[Index].Source];
		const u32 At = Item.Live ? Curves::KeyAt(*Item.Live, DragTimes[Index], 0.0f) : u32(-1);
		if (At != u32(-1))
		{
			SelectedKeys.push_back(Item.Ref);
			SelectedKeys.back().Index = At;
		}
	}
	std::sort(SelectedKeys.begin(), SelectedKeys.end());
	SelectedKeys.erase(std::unique(SelectedKeys.begin(), SelectedKeys.end()), SelectedKeys.end());
	for (const LaneDrag& Item : DragCurves)
	{
		if (Item.Live && Item.Ref.Id == Param::TimeScale)
		{
			Refit(Core, *Find(Core.Data, Item.Ref.ObjectId));
		}
	}
}

void Editor::DrawKeyMenu(Player& Core)
{
	if (!ImGui::BeginPopup("##KeyMenu"))
	{
		return;
	}
	const Key* First = SelectedKeys.empty() ? nullptr : TimelineUi::FindKey(Core.Data, SelectedKeys[0]);
	if (ImGui::BeginMenu("Interpolation", First != nullptr))
	{
		for (u32 Mode = 0; Mode < 6; ++Mode)
		{
			if (ImGui::MenuItem(TimelineUi::ModeNames[Mode], nullptr, First->Mode == static_cast<Interp>(Mode)))
			{
				ForSelectedKeys(Core, "Interpolation", [Mode](Object&, const KeyRef& Ref, Curve& Part) { Curves::SetMode(Part, Ref.Index, static_cast<Interp>(Mode)); });
			}
		}
		ImGui::EndMenu();
	}
	if (ImGui::MenuItem("Broken tangents", nullptr, First && First->IsBroken, First != nullptr))
	{
		const bool IsBroken = !First->IsBroken;
		ForSelectedKeys(Core, "Tangents", [IsBroken](Object&, const KeyRef& Ref, Curve& Part) { Part.Keys[Ref.Index].IsBroken = IsBroken; });
	}
	if (ImGui::MenuItem("Easy ease", nullptr, false, First != nullptr))
	{
		ForSelectedKeys(Core, "Easy ease", [](Object&, const KeyRef& Ref, Curve& Part) { Curves::EasyEase(Part, Ref.Index); });
	}
	if (ImGui::MenuItem("Remove", "Delete", false, First != nullptr))
	{
		RemoveKeys(Core);
	}
	ImGui::EndPopup();
}

template <typename TFunc>
void Editor::ForMenuCurves(Player& Core, const char* Label, const TFunc& Visit)
{
	const Object* Owner = Find(Core.Data, CurveMenu.ObjectId);
	if (!Owner || !HasOwner(*Owner, CurveMenu.Block))
	{
		return;
	}
	Object& Item = *Edit(Core, CurveMenu.ObjectId, Label);
	UseGameFov(Item, CurveMenu.Block, CurveMenu.Id);
	Lane& Track = Curves::EnsureLane(LaneList(Item, CurveMenu.Block), CurveMenu.Id, CurveMenu.Layer);
	for (u8 Axis = 0; Axis < Info(CurveMenu.Id).Components; ++Axis)
	{
		if ((CurveMenu.Mask >> Axis) & 1)
		{
			Visit(Item, Track.Curves[Axis], Axis);
		}
	}
	SelectedKeys.clear();
	if (CurveMenu.Id == Param::TimeScale)
	{
		Refit(Core, Item);
	}
}

void Editor::DrawCurveMenu(Player& Core)
{
	if (!ImGui::BeginPopup("##CurveMenu"))
	{
		return;
	}
	const Object* Owner = Find(Core.Data, CurveMenu.ObjectId);
	const Lane* Track = Owner && HasOwner(*Owner, CurveMenu.Block) ? Curves::FindLane(LaneList(*Owner, CurveMenu.Block), CurveMenu.Id, CurveMenu.Layer) : nullptr;
	const bool HasKeys = Track && std::any_of(std::begin(Track->Curves), std::end(Track->Curves), [](const Curve& Part) { return !Part.Keys.empty(); });
	const ParamInfo& Meta = Info(CurveMenu.Id);
	const float HalfFrame = 0.5f / static_cast<float>(Core.Data.Header.FrameRate);
	if (ImGui::MenuItem("Copy curve", nullptr, false, Track != nullptr))
	{
		CurveClip = *Track;
		HasCurveClip = true;
	}
	if (ImGui::MenuItem("Paste curve", nullptr, false, HasCurveClip && Info(CurveClip.Id).Components == Meta.Components))
	{
		ForMenuCurves(Core, "Paste curve", [&](Object&, Curve& Part, u8 Axis)
		{
			const Curve& Source = CurveClip.Curves[Axis];
			const float Shift = Source.Keys.empty() ? 0.0f : CurveMenu.Local - Source.Keys.front().Time;
			for (Key Item : Source.Keys)
			{
				Item.Time += Shift;
				Item.Value = std::clamp(Item.Value, Meta.Min, Meta.Max);
				Curves::PlaceKey(Part, Item, HalfFrame);
			}
			Part.Gen = Source.Gen;
		});
	}
	ImGui::Separator();
	if (ImGui::MenuItem("Reverse", nullptr, false, HasKeys))
	{
		ForMenuCurves(Core, "Reverse curve", [](Object&, Curve& Part, u8) { TimelineUi::ReverseCurve(Part); });
	}
	if (ImGui::BeginMenu("Scale time", HasKeys))
	{
		for (u32 Index = 0; Index < 4; ++Index)
		{
			if (ImGui::MenuItem(TimelineUi::ScaleLabels[Index]))
			{
				ForMenuCurves(Core, "Scale time", [Index](Object&, Curve& Part, u8) { TimelineUi::ScaleCurve(Part, TimelineUi::ScaleFactors[Index]); });
			}
		}
		ImGui::EndMenu();
	}
	if (ImGui::MenuItem("Smooth keys", nullptr, false, HasKeys))
	{
		ForMenuCurves(Core, "Smooth keys", [this](Object&, Curve& Part, u8) { TimelineUi::SmoothCurve(Part, CurveScratch); });
	}
	if (ImGui::MenuItem("Reduce keys", nullptr, false, HasKeys))
	{
		ForMenuCurves(Core, "Reduce keys", [this](Object&, Curve& Part, u8) { TimelineUi::ReduceCurve(Part, 0.04f * CurveMenu.Amount, RdpSpans, Kept); });
	}
	if (ImGui::BeginMenu("Presets"))
	{
		for (u8 Preset = 0; Preset < 5; ++Preset)
		{
			if (!ImGui::MenuItem(TimelineUi::PresetNames[Preset]))
			{
				continue;
			}
			ForMenuCurves(Core, "Curve preset", [&](Object& Item, Curve& Part, u8 Axis)
			{
				const float Base = Curves::Value(Part, CurveMenu.Local, TimelineUi::Fallback(Item, CurveMenu.Block, CurveMenu.Id, CurveMenu.Layer, Axis));
				if (Preset == 4)
				{
					Part.Gen.Type = Wave::NoiseSmooth;
					Part.Gen.Amplitude = 0.2f * CurveMenu.Amount;
					Part.Gen.Frequency = 4.0f;
					return;
				}
				for (const TimelineUi::PresetKey& Step : TimelineUi::PresetKeys)
				{
					if (Step.Preset == Preset)
					{
						Curves::PlaceKey(Part, Key { CurveMenu.Local + Step.Time, std::clamp(Base + Step.Share * CurveMenu.Amount, Meta.Min, Meta.Max), Step.Mode }, HalfFrame);
					}
				}
			});
		}
		ImGui::EndMenu();
	}
	ImGui::EndPopup();
}

void Editor::RemoveKeys(Player& Core)
{
	ForSelectedKeys(Core, "Remove keys", [](Object& Owner, const KeyRef& Ref, Curve& Part)
	{
		float* Field = StaticField(Owner, Ref.Block, Ref.Id, Ref.Layer, Ref.Component);
		if (Field && Part.Keys.size() == 1)
		{
			*Field = Part.Keys[0].Value;
		}
		Part.Keys.erase(Part.Keys.begin() + Ref.Index);
	});
	SelectedKeys.clear();
	Focus = FocusKind::Objects;
}

void Editor::RemoveLane(Object& Target, s32 Block, Param Id, u8 Layer, float Local)
{
	xr_vector<Lane>& Lanes = LaneList(Target, Block);
	const Lane* Found = Curves::FindLane(Lanes, Id, Layer);
	if (!Found)
	{
		return;
	}
	for (u8 Axis = 0; Axis < Info(Id).Components; ++Axis)
	{
		if (float* Field = StaticField(Target, Block, Id, Layer, Axis))
		{
			*Field = Curves::Value(Found->Curves[Axis], Local, *Field);
		}
	}
	Lanes.erase(Lanes.begin() + (Found - Lanes.data()));
}

const GraphCurve* Editor::GraphCurveOf(const KeyRef& Ref) const
{
	const auto Found = std::find_if(GraphCurves.begin(), GraphCurves.end(), [&Ref](const GraphCurve& Item) { return Item.Ref.ObjectId == Ref.ObjectId && Item.Ref.Block == Ref.Block && Item.Ref.Id == Ref.Id && Item.Ref.Layer == Ref.Layer && Item.Ref.Component == Ref.Component; });
	return Found == GraphCurves.end() ? nullptr : &*Found;
}

bool Editor::IsGraphHidden(const KeyRef& Ref) const
{
	return std::find(GraphHidden.begin(), GraphHidden.end(), Ref) != GraphHidden.end();
}

void Editor::SnapGraphKeys(Player& Core)
{
	GraphStart.clear();
	for (const KeyRef& Ref : SelectedKeys)
	{
		if (const Key* Item = TimelineUi::FindKey(Core.Data, Ref))
		{
			GraphStart.push_back({ Ref, Item->Time, Item->Value });
		}
	}
}

void Editor::PlaceGraphKeys(Player& Core, float Shift, float Rise, bool IsView, float Factor, float Pivot)
{
	for (const GraphKeyStart& Start : GraphStart)
	{
		Object* Owner = Edit(Core, Start.Ref.ObjectId, "Move keys");
		Curve* Part = Owner ? TimelineUi::FindCurve(Core.Data, Start.Ref) : nullptr;
		const GraphCurve* Shown = GraphCurveOf(Start.Ref);
		if (!Part || !Shown || Start.Ref.Index >= Part->Keys.size())
		{
			continue;
		}
		const ParamInfo& Meta = Info(Start.Ref.Id);
		const float Unit = IsView && IsGraphNormalized ? Shown->Hi - Shown->Lo : 1.0f, Base = Shown->Start;
		const auto Neighbour = [&](u32 At, float Fallback) { KeyRef Other = Start.Ref; Other.Index = At; return At < Part->Keys.size() && !IsKeySelected(Other) ? Part->Keys[At].Time : Fallback; };
		const float Lo = Start.Ref.Index ? Neighbour(Start.Ref.Index - 1, -FLT_MAX) + 1e-3f : -FLT_MAX, Hi = Neighbour(Start.Ref.Index + 1, FLT_MAX) - 1e-3f;
		Key& Item = Part->Keys[Start.Ref.Index];
		const float Time = Factor > 0.0f ? Pivot - Base + (Start.Time + Base - Pivot) * Factor : Start.Time + Shift;
		const float Value = Start.Value + Rise * Unit;
		Item.Time = std::min(std::max(Time, std::max(Lo, 0.0f)), std::min(Hi, MaxTime));
		Item.Value = std::clamp(Value, Meta.Min, Meta.Max);
	}
}

void Editor::SetGraphTangents(Player& Core, u8 Kind)
{
	for (const KeyRef& Ref : SelectedKeys)
	{
		Curve* Part = Edit(Core, Ref.ObjectId, "Tangents") ? TimelineUi::FindCurve(Core.Data, Ref) : nullptr;
		if (!Part || Ref.Index >= Part->Keys.size())
		{
			continue;
		}
		xr_vector<Key>& Keys = Part->Keys;
		const u32 At = Ref.Index, Count = static_cast<u32>(Keys.size());
		Key& Item = Keys[At];
		const float Before = At ? Item.Time - Keys[At - 1].Time : 0.0f, After = At + 1 < Count ? Keys[At + 1].Time - Item.Time : 0.0f;
		if (Kind == 2 || Kind == 4 || Kind >= 6)
		{
			Item.Mode = Kind == 2 ? Interp::Linear : Kind == 4 ? Interp::Hold : Item.Mode;
			Item.IsBroken = Kind == 6 || (Kind != 7 && Item.IsBroken);
			Item.InValue = Kind == 7 && Item.OutTime > 0.0f ? Item.OutValue * Item.InTime / Item.OutTime : Item.InValue;
			continue;
		}
		const ImVec2 In = At ? TimelineUi::TangentOf(*Part, At, false, Item.Value) : ImVec2(), Out = At + 1 < Count ? TimelineUi::TangentOf(*Part, At, true, Item.Value) : ImVec2();
		const float Prev = At ? Keys[At - 1].Value : Item.Value, Next = At + 1 < Count ? Keys[At + 1].Value : Item.Value;
		float Slope = Before + After > 0.0f ? (Next - Prev) / (Before + After) : 0.0f;
		Slope = Kind == 3 || (Kind == 0 && (Item.Value - Prev) * (Next - Item.Value) <= 0.0f) ? 0.0f : Slope;
		Item.OutTime = Kind == 5 ? Out.x : After / 3.0f;
		Item.OutValue = Kind == 5 ? Out.y : Slope * After / 3.0f;
		Item.InTime = Kind == 5 ? In.x : -Before / 3.0f;
		Item.InValue = Kind == 5 ? In.y : -Slope * Before / 3.0f;
		Item.OutValue = Kind == 0 ? std::clamp(Item.OutValue, -std::fabs(Next - Item.Value), std::fabs(Next - Item.Value)) : Item.OutValue;
		Item.InValue = Kind == 0 ? std::clamp(Item.InValue, -std::fabs(Item.Value - Prev), std::fabs(Item.Value - Prev)) : Item.InValue;
		Item.Mode = Interp::Bezier;
		if (At && Keys[At - 1].Mode != Interp::Bezier)
		{
			Key& Head = Keys[At - 1];
			Head.Mode = Interp::Bezier;
			Head.OutTime = Before / 3.0f;
			Head.OutValue = (Item.Value - Head.Value) / 3.0f;
		}
	}
}

template <typename TVisit>
void Editor::ForGraphCurves(Player& Core, const char* Label, const TVisit& Visit)
{
	for (const GraphCurve& Item : GraphCurves)
	{
		const bool IsPicked = std::any_of(SelectedKeys.begin(), SelectedKeys.end(), [&Item](const KeyRef& Ref) { return Ref.ObjectId == Item.Ref.ObjectId && Ref.Block == Item.Ref.Block && Ref.Id == Item.Ref.Id && Ref.Layer == Item.Ref.Layer && Ref.Component == Item.Ref.Component; });
		Curve* Part = (SelectedKeys.empty() ? !IsGraphHidden(Item.Ref) : IsPicked) && Edit(Core, Item.Ref.ObjectId, Label) ? TimelineUi::FindCurve(Core.Data, Item.Ref) : nullptr;
		if (Part)
		{
			Visit(Item, *Part);
		}
	}
}

void Editor::FrameGraph(Player& Core, bool IsSelection)
{
	float Lo[2] = { FLT_MAX, FLT_MAX }, Hi[2] = { -FLT_MAX, -FLT_MAX };
	const auto Grow = [&](const GraphCurve& Item, const Key& Point)
	{
		const float Value = IsGraphNormalized ? (Point.Value - Item.Lo) / (Item.Hi - Item.Lo) : Point.Value;
		Lo[0] = std::min(Lo[0], Item.Start + Point.Time);
		Hi[0] = std::max(Hi[0], Item.Start + Point.Time);
		Lo[1] = std::min(Lo[1], Value);
		Hi[1] = std::max(Hi[1], Value);
	};
	for (const GraphCurve& Item : GraphCurves)
	{
		const Curve* Part = IsGraphHidden(Item.Ref) ? nullptr : TimelineUi::FindCurve(Core.Data, Item.Ref);
		for (u32 At = 0; Part && At < Part->Keys.size(); ++At)
		{
			KeyRef Ref = Item.Ref;
			Ref.Index = At;
			if (!IsSelection || IsKeySelected(Ref))
			{
				Grow(Item, Part->Keys[At]);
			}
		}
	}
	if (Lo[0] > Hi[0])
	{
		return;
	}
	const float Width = std::max(Hi[0] - Lo[0], 0.5f), Height = std::max(Hi[1] - Lo[1], IsGraphNormalized ? 0.1f : 0.01f);
	GraphView[0] = Lo[0] - 0.1f * Width;
	GraphView[1] = Hi[0] + 0.1f * Width;
	GraphView[2] = Lo[1] - 0.15f * Height;
	GraphView[3] = Hi[1] + 0.15f * Height;
}

void Editor::DrawGraph(Player& Core)
{
	IsGraphFocused = false;
	if (!Settings.ShowGraph)
	{
		return;
	}
	ImGui::SetNextWindowSize(ImVec2(760.0f, 360.0f), LayoutCond());
	if (LayoutReset)
	{
		ImGui::SetNextWindowDockID(0, ImGuiCond_Always);
	}
	if (!ImGui::Begin(ShowAnimator ? "Graph Editor###CinemaGraphAnimation" : "Graph Editor###CinemaGraph", &Settings.ShowGraph, ImGuiWindowFlags_NoNavInputs))
	{
		ImGui::End();
		return;
	}
	IsGraphFocused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	IsOwnWindowFocused |= IsGraphFocused;
	const ImGuiIO& Input = ImGui::GetIO();
	if (!GraphDrag)
	{
		GraphCurves.clear();
	}
	for (u32 Id : Selected)
	{
		const Object* Target = GraphDrag ? nullptr : Find(Core.Data, Id);
		if (!Target)
		{
			continue;
		}
		CollectLaneTargets(*Target);
		if (const s32 Block = PoseBlock(*Target); Block >= 0)
		{
			LaneTargets.push_back({ static_cast<s16>(Block), ClipOrigin, Target->Actor.Clips[ClipIndex(Block)].Length });
		}
		for (const LaneTarget& Where : LaneTargets)
		{
			for (const Lane& Track : LaneList(*Target, Where.Block))
			{
				const PoseBone* Joint = Where.Block >= ClipBase ? BoneEntry(const_cast<Object&>(*Target), Where.Block, Track.Layer) : nullptr;
				const ActorRuntime* Body = Joint && !SelectedBones.empty() ? Core.FindActor(Id) : nullptr;
				if (Body && std::find(SelectedBones.begin(), SelectedBones.end(), Body->FindBone(Joint->Name)) == SelectedBones.end())
				{
					continue;
				}
				for (u8 Axis = 0; Axis < Info(Track.Id).Components; ++Axis)
				{
					const Curve& Part = Track.Curves[Axis];
					float Lo = FLT_MAX, Hi = -FLT_MAX;
					for (const Key& Point : Part.Keys)
					{
						Lo = std::min(Lo, Point.Value);
						Hi = std::max(Hi, Point.Value);
					}
					if (!Part.Keys.empty())
					{
						GraphCurves.push_back({ { Id, Where.Block, Track.Id, Track.Layer, Axis, 0 }, Where.Start, Lo, std::max(Hi, Lo + 1e-3f), Info(Track.Id).Components == 3 ? TimelineUi::AxisColors[Axis] : Target->Color });
					}
				}
			}
		}
	}
	if (ImGui::Checkbox("Normalize", &IsGraphNormalized))
	{
		FrameGraph(Core, false);
	}
	ImGui::SameLine();
	if (ImGui::Button("Frame") || (IsGraphFocused && !Input.WantTextInput && ImGui::IsKeyChordPressed(ImGuiKey_F)))
	{
		FrameGraph(Core, !SelectedKeys.empty());
	}
	ImGui::SetItemTooltip("Frame selected (F)");
	ImGui::SameLine();
	if (ImGui::Button("All") || (IsGraphFocused && !Input.WantTextInput && ImGui::IsKeyChordPressed(ImGuiKey_A)))
	{
		FrameGraph(Core, false);
	}
	ImGui::SetItemTooltip("Frame all (A)");
	static constexpr const char* TangentNames[8] = { "Auto", "Spline", "Linear", "Flat", "Step", "Bezier", "Break", "Unify" };
	for (u8 Kind = 0; Kind < 8; ++Kind)
	{
		ImGui::SameLine(0.0f, Kind ? ImGui::GetStyle().ItemInnerSpacing.x : -1.0f);
		ImGui::BeginDisabled(SelectedKeys.empty());
		if (ImGui::Button(TangentNames[Kind]))
		{
			SetGraphTangents(Core, Kind);
		}
		ImGui::EndDisabled();
	}
	static constexpr const char* InfinityNames[5] = { "Constant", "Linear", "Cycle", "Cycle offset", "Oscillate" };
	for (u8 Side = 0; Side < 2; ++Side)
	{
		ImGui::SameLine();
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 6.0f);
		if (ImGui::BeginCombo(Side ? "##Post" : "##Pre", Side ? "Post" : "Pre"))
		{
			for (u8 Mode = 0; Mode < 5; ++Mode)
			{
				if (ImGui::Selectable(InfinityNames[Mode]))
				{
					ForGraphCurves(Core, "Infinity", [Side, Mode](const GraphCurve&, Curve& Part) { (Side ? Part.Post : Part.Pre) = Mode; });
				}
			}
			ImGui::EndCombo();
		}
		ImGui::SetItemTooltip(Side ? "Post infinity" : "Pre infinity");
	}
	ImGui::SameLine();
	if (ImGui::Button("Euler"))
	{
		ForGraphCurves(Core, "Euler filter", [](const GraphCurve& Item, Curve& Part)
		{
			for (u32 At = 1; Item.Ref.Id == Param::Rotation && At < Part.Keys.size(); ++At)
			{
				Part.Keys[At].Value = Part.Keys[At - 1].Value + std::remainder(Part.Keys[At].Value - Part.Keys[At - 1].Value, 360.0f);
			}
		});
	}
	ImGui::SetItemTooltip("Euler filter");
	ImGui::SameLine();
	if (ImGui::Button("Buffer"))
	{
		GraphBuffer.clear();
		for (const GraphCurve& Item : GraphCurves)
		{
			const Curve* Part = IsGraphHidden(Item.Ref) ? nullptr : TimelineUi::FindCurve(Core.Data, Item.Ref);
			if (Part)
			{
				GraphBuffer.push_back({ Item.Ref, *Part });
			}
		}
	}
	ImGui::SetItemTooltip("Store buffer curves");
	ImGui::SameLine();
	ImGui::BeginDisabled(GraphBuffer.empty());
	if (ImGui::Button("Swap"))
	{
		for (GraphBuffered& Stored : GraphBuffer)
		{
			Curve* Part = Edit(Core, Stored.Ref.ObjectId, "Swap buffer") ? TimelineUi::FindCurve(Core.Data, Stored.Ref) : nullptr;
			if (Part)
			{
				std::swap(*Part, Stored.Shape);
			}
		}
		SelectedKeys.clear();
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Swap with buffer");
	const GraphCurve* Lead = SelectedKeys.empty() ? nullptr : GraphCurveOf(SelectedKeys.front());
	const Key* LeadKey = Lead ? TimelineUi::FindKey(Core.Data, SelectedKeys.front()) : nullptr;
	if (LeadKey)
	{
		float Fields[2] = { Lead->Start + LeadKey->Time, LeadKey->Value };
		for (u32 Field = 0; Field < 2; ++Field)
		{
			ImGui::SameLine();
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 5.0f);
			const float Was = Fields[Field];
			const bool IsChanged = ImGui::DragFloat(Field ? "##Value" : "##Time", &Fields[Field], Field ? 0.01f : 1.0f / static_cast<float>(Core.Data.Header.FrameRate), 0.0f, 0.0f, Field ? "%.3f" : "%.3f s");
			ImGui::SetItemTooltip(Field ? "Value" : "Time");
			if (ImGui::IsItemActivated())
			{
				SnapGraphKeys(Core);
				GraphField = Was;
			}
			if (IsChanged && !GraphStart.empty())
			{
				PlaceGraphKeys(Core, Field ? 0.0f : Fields[0] - GraphField, Field ? Fields[1] - GraphField : 0.0f, false, 0.0f, 0.0f);
			}
		}
	}
	const float ListWidth = std::min(ImGui::GetFontSize() * 12.0f, 0.3f * ImGui::GetContentRegionAvail().x);
	ImGui::BeginChild("##GraphList", ImVec2(ListWidth, 0.0f), ImGuiChildFlags_Borders);
	for (u32 Index = 0; Index < GraphCurves.size(); ++Index)
	{
		const GraphCurve& Item = GraphCurves[Index];
		const Object* Owner = Find(Core.Data, Item.Ref.ObjectId);
		string64 Label;
		TimelineUi::SlotLabel(Label, Item.Ref.Id, Item.Ref.Layer, Owner, Item.Ref.Block);
		string256 Line;
		xr_sprintf(Line, "%s %s %s##%u", DisplayName(Owner), Label, Info(Item.Ref.Id).Components == 3 ? TimelineUi::AxisNames[Item.Ref.Id == Param::Color][Item.Ref.Component] : "", Index);
		bool IsShown = !IsGraphHidden(Item.Ref);
		ImGui::PushID(static_cast<s32>(Index));
		if (ImGui::Checkbox("##Shown", &IsShown))
		{
			std::erase(GraphHidden, Item.Ref);
			if (!IsShown)
			{
				GraphHidden.push_back(Item.Ref);
			}
		}
		ImGui::PopID();
		ImGui::SameLine();
		ImGui::PushStyleColor(ImGuiCol_Text, ImGui::ColorConvertU32ToFloat4(Item.Color | 0xFF000000u));
		if (ImGui::Selectable(Line, false))
		{
			GraphHidden.clear();
			for (const GraphCurve& Other : GraphCurves)
			{
				if (&Other != &Item)
				{
					GraphHidden.push_back(Other.Ref);
				}
			}
		}
		ImGui::PopStyleColor();
	}
	ImGui::EndChild();
	ImGui::SameLine();
	const ImVec2 Corner = ImGui::GetCursorScreenPos(), Size = ImGui::GetContentRegionAvail();
	const ImRect Canvas(Corner, ImVec2(Corner.x + std::max(Size.x, 8.0f), Corner.y + std::max(Size.y, 8.0f)));
	ImGui::InvisibleButton("##GraphCanvas", Canvas.GetSize(), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight | ImGuiButtonFlags_MouseButtonMiddle);
	const bool IsHovered = ImGui::IsItemHovered(), IsActive = ImGui::IsItemActive();
	float* View = GraphView;
	const auto ToX = [&](float Time) { return Canvas.Min.x + (Time - View[0]) / (View[1] - View[0]) * Canvas.GetWidth(); };
	const auto ToY = [&](float Value) { return Canvas.Max.y - (Value - View[2]) / (View[3] - View[2]) * Canvas.GetHeight(); };
	const auto TimeAt = [&](float X) { return View[0] + (X - Canvas.Min.x) / Canvas.GetWidth() * (View[1] - View[0]); };
	const auto Shown = [this](const GraphCurve& Item, float Value) { return IsGraphNormalized ? (Value - Item.Lo) / (Item.Hi - Item.Lo) : Value; };
	if (IsHovered && Input.MouseWheel != 0.0f)
	{
		const float Factor = std::pow(0.85f, Input.MouseWheel), Time = TimeAt(Input.MousePos.x), Value = View[2] + (Canvas.Max.y - Input.MousePos.y) / Canvas.GetHeight() * (View[3] - View[2]);
		for (u32 Axis = Input.KeyCtrl ? 1 : 0; Axis < (Input.KeyShift ? 1u : 2u); ++Axis)
		{
			const float Pivot = Axis ? Value : Time;
			View[Axis * 2] = Pivot + (View[Axis * 2] - Pivot) * Factor;
			View[Axis * 2 + 1] = Pivot + (View[Axis * 2 + 1] - Pivot) * Factor;
		}
	}
	if (IsActive && ImGui::IsMouseDragging(ImGuiMouseButton_Middle, 0.0f))
	{
		const float Dt = Input.MouseDelta.x / Canvas.GetWidth() * (View[1] - View[0]), Dv = Input.MouseDelta.y / Canvas.GetHeight() * (View[3] - View[2]);
		View[0] -= Dt;
		View[1] -= Dt;
		View[2] += Dv;
		View[3] += Dv;
	}
	ImDrawList* List = ImGui::GetWindowDrawList();
	List->PushClipRect(Canvas.Min, Canvas.Max, true);
	List->AddRectFilled(Canvas.Min, Canvas.Max, ImGui::GetColorU32(ImGuiCol_FrameBg));
	List->AddLine(ImVec2(Canvas.Min.x, ToY(0.0f)), ImVec2(Canvas.Max.x, ToY(0.0f)), TimelineUi::Separator);
	List->AddLine(ImVec2(ToX(Core.Time), Canvas.Min.y), ImVec2(ToX(Core.Time), Canvas.Max.y), ImGui::GetColorU32(ImGuiCol_PlotLines));
	for (const GraphBuffered& Stored : GraphBuffer)
	{
		const GraphCurve* Item = GraphCurveOf(Stored.Ref);
		if (Item && !IsGraphHidden(Item->Ref))
		{
			TimelineUi::Plot(List, Canvas.Min.x, Canvas.Max.x, [&](float X) { return ToY(Shown(*Item, Curves::Value(Stored.Shape, TimeAt(X) - Item->Start, 0.0f))); }, ImGui::GetColorU32(ImGuiCol_TextDisabled), 1.0f);
		}
	}
	KeyRef Hot, Tipped;
	float HotScore = 7.0f, TipScore = 7.0f;
	u8 TipSide = 0;
	ImRect Bounds(FLT_MAX, FLT_MAX, -FLT_MAX, -FLT_MAX);
	for (const GraphCurve& Item : GraphCurves)
	{
		const Curve* Part = IsGraphHidden(Item.Ref) ? nullptr : TimelineUi::FindCurve(Core.Data, Item.Ref);
		if (!Part)
		{
			continue;
		}
		TimelineUi::Plot(List, Canvas.Min.x, Canvas.Max.x, [&](float X) { return ToY(Shown(Item, Curves::Value(*Part, TimeAt(X) - Item.Start, 0.0f))); }, Item.Color | 0xFF000000u, 2.0f);
		for (u32 At = 0; At < Part->Keys.size(); ++At)
		{
			const Key& Point = Part->Keys[At];
			KeyRef Ref = Item.Ref;
			Ref.Index = At;
			const ImVec2 Spot(ToX(Item.Start + Point.Time), ToY(Shown(Item, Point.Value)));
			const bool IsPicked = IsKeySelected(Ref);
			for (u8 Side = 0; IsPicked && Side < 2; ++Side)
			{
				if (Side ? At + 1 < Part->Keys.size() : At > 0)
				{
					const ImVec2 Arm = TimelineUi::TangentOf(*Part, At, Side == 1, Point.Value);
					const ImVec2 Tip(ToX(Item.Start + Point.Time + Arm.x), ToY(Shown(Item, Point.Value + Arm.y)));
					const float Reach = std::fabs(Tip.x - Input.MousePos.x) + std::fabs(Tip.y - Input.MousePos.y);
					const bool IsHeld = GraphDrag == 3 && GraphTip == Ref && GraphTipSide == Side;
					if (IsHovered && !GraphDrag && Reach < TipScore)
					{
						TipScore = Reach;
						Tipped = Ref;
						TipSide = Side;
					}
					List->AddLine(Spot, Tip, ImGui::GetColorU32(IsHeld ? ImGuiCol_Text : ImGuiCol_TextDisabled));
					List->AddCircleFilled(Tip, IsHeld ? 4.0f : 3.0f, ImGui::GetColorU32(IsHeld ? ImGuiCol_Text : ImGuiCol_TextDisabled));
				}
			}
			List->AddNgonFilled(Spot, TimelineUi::KeyRadius, IsPicked ? ImGui::GetColorU32(ImGuiCol_Text) : Item.Color | 0xFF000000u, 4);
			List->AddNgon(Spot, TimelineUi::KeyRadius, TimelineUi::Separator, 4);
			const float Score = std::fabs(Spot.x - Input.MousePos.x) + std::fabs(Spot.y - Input.MousePos.y);
			if (IsHovered && Score < HotScore)
			{
				HotScore = Score;
				Hot = Ref;
			}
			if (IsPicked)
			{
				Bounds.Add(Spot);
			}
			if (GraphDrag == 2 && GraphBox.Contains(Spot))
			{
				KeyGroup.push_back(Ref);
			}
		}
	}
	const ImVec2 Mouse = Input.MousePos;
	const bool IsWide = Bounds.GetWidth() > 4.0f, IsTall = Bounds.GetHeight() > 4.0f, HasHot = HotScore < 7.0f, HasBox = SelectedKeys.size() > 1 && (IsWide || IsTall);
	const bool IsNearX = Mouse.x > Bounds.Min.x - 5.0f && Mouse.x < Bounds.Max.x + 5.0f, IsNearY = Mouse.y > Bounds.Min.y - 5.0f && Mouse.y < Bounds.Max.y + 5.0f;
	const s32 Edge = !HasBox || HasHot ? -1 : IsWide && IsNearY && std::fabs(Mouse.x - Bounds.Min.x) < 5.0f ? 4 : IsWide && IsNearY && std::fabs(Mouse.x - Bounds.Max.x) < 5.0f ? 5 : IsTall && IsNearX && std::fabs(Mouse.y - Bounds.Max.y) < 5.0f ? 6 : IsTall && IsNearX && std::fabs(Mouse.y - Bounds.Min.y) < 5.0f ? 7 : -1;
	if (HasBox)
	{
		List->AddRect(ImVec2(Bounds.Min.x - 3.0f, Bounds.Min.y - 3.0f), ImVec2(Bounds.Max.x + 3.0f, Bounds.Max.y + 3.0f), ImGui::GetColorU32(ImGuiCol_TextDisabled));
	}
	if (IsHovered && (HasHot || Edge >= 0))
	{
		ImGui::SetMouseCursor(HasHot ? ImGuiMouseCursor_Hand : Edge < 6 ? ImGuiMouseCursor_ResizeEW : ImGuiMouseCursor_ResizeNS);
	}
	const bool HasTip = TipScore < 7.0f && TipScore <= HotScore;
	if (IsHovered && HasTip)
	{
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
	}
	if (HasTip && ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		GraphDrag = 3;
		GraphTip = Tipped;
		GraphTipSide = TipSide;
	}
	if (IsActive && GraphDrag == 3)
	{
		const GraphCurve* Item = GraphCurveOf(GraphTip);
		Curve* Part = Item && Edit(Core, GraphTip.ObjectId, "Move tangent") ? TimelineUi::FindCurve(Core.Data, GraphTip) : nullptr;
		const u32 At = GraphTip.Index, Count = Part ? static_cast<u32>(Part->Keys.size()) : 0;
		const bool IsOut = GraphTipSide == 1;
		if (At < Count && (IsOut ? At + 1 < Count : At > 0))
		{
			const u32 Head = IsOut ? At : At - 1;
			Curves::SetMode(*Part, Head, Interp::Bezier);
			Key& Point = Part->Keys[At];
			const float Width = Part->Keys[Head + 1].Time - Part->Keys[Head].Time, Sign = IsOut ? 1.0f : -1.0f;
			const float Level = View[2] + (Canvas.Max.y - Mouse.y) / Canvas.GetHeight() * (View[3] - View[2]);
			const float Rise = (IsGraphNormalized ? Item->Lo + Level * (Item->Hi - Item->Lo) : Level) - Point.Value;
			const float Reach = Sign * std::clamp(Sign * (TimeAt(Mouse.x) - Item->Start - Point.Time), 0.01f * Width, Width);
			(IsOut ? Point.OutTime : Point.InTime) = Reach;
			(IsOut ? Point.OutValue : Point.InValue) = Rise;
			Point.IsBroken |= Input.KeyAlt;
			if (!Point.IsBroken)
			{
				(IsOut ? Point.InValue : Point.OutValue) = (IsOut ? Point.InTime : Point.OutTime) * Rise / Reach;
			}
		}
	}
	if (!HasTip && ImGui::IsItemActivated() && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		GraphPress = Mouse;
		GraphBounds = Bounds;
		if (HasHot && !IsKeySelected(Hot))
		{
			SelectedKeys.resize(Input.KeyCtrl ? SelectedKeys.size() : 0);
			SelectedKeys.push_back(Hot);
			std::sort(SelectedKeys.begin(), SelectedKeys.end());
		}
		else if (HasHot && Input.KeyCtrl)
		{
			std::erase(SelectedKeys, Hot);
		}
		Focus = FocusKind::Keys;
		GraphDrag = HasHot && !Input.KeyCtrl ? 1 : Edge >= 0 ? static_cast<u8>(Edge) : HasHot ? 0 : 2;
		DragKeyPrevious = SelectedKeys;
		SnapGraphKeys(Core);
	}
	if (GraphDrag && (ImGui::IsKeyPressed(ImGuiKey_Escape, false) || ImGui::IsMouseClicked(ImGuiMouseButton_Right)))
	{
		if (GraphDrag == 2)
		{
			SelectedKeys = DragKeyPrevious;
		}
		else
		{
			Undo.Cancel(Core.Data);
		}
		GraphDrag = 0;
	}
	const float Fine = Input.KeyShift ? 0.1f : 1.0f;
	const float Shift = (Mouse.x - GraphPress.x) / Canvas.GetWidth() * (View[1] - View[0]) * Fine, Rise = (GraphPress.y - Mouse.y) / Canvas.GetHeight() * (View[3] - View[2]) * Fine;
	if (IsActive && GraphDrag == 1 && ImGui::IsMouseDragging(ImGuiMouseButton_Left, 3.0f))
	{
		PlaceGraphKeys(Core, Shift, Rise, true, 0.0f, 0.0f);
	}
	if (IsActive && GraphDrag >= 4 && ImGui::IsMouseDragging(ImGuiMouseButton_Left, 1.0f))
	{
		const bool IsTime = GraphDrag < 6;
		const float From = IsTime ? (GraphDrag == 4 ? GraphBounds.Min.x : GraphBounds.Max.x) : (GraphDrag == 6 ? GraphBounds.Max.y : GraphBounds.Min.y);
		const float Pivot = IsTime ? (GraphDrag == 4 ? GraphBounds.Max.x : GraphBounds.Min.x) : (GraphDrag == 6 ? GraphBounds.Min.y : GraphBounds.Max.y);
		const float Factor = std::max((IsTime ? Mouse.x - Pivot : Mouse.y - Pivot) / (From - Pivot), 0.01f);
		const float PivotValue = View[2] + (Canvas.Max.y - Pivot) / Canvas.GetHeight() * (View[3] - View[2]);
		if (IsTime)
		{
			PlaceGraphKeys(Core, 0.0f, 0.0f, true, Factor, TimeAt(Pivot));
		}
		else
		{
			for (const GraphKeyStart& Start : GraphStart)
			{
				const GraphCurve* Item = GraphCurveOf(Start.Ref);
				Key* Point = Item && Edit(Core, Start.Ref.ObjectId, "Scale keys") ? TimelineUi::FindKey(Core.Data, Start.Ref) : nullptr;
				if (Point)
				{
					const float Base = IsGraphNormalized ? Item->Lo + PivotValue * (Item->Hi - Item->Lo) : PivotValue;
					Point->Value = std::clamp(Base + (Start.Value - Base) * Factor, Info(Start.Ref.Id).Min, Info(Start.Ref.Id).Max);
				}
			}
		}
	}
	if (GraphDrag == 2)
	{
		GraphBox = ImRect(ImMin(GraphPress, Mouse), ImMax(GraphPress, Mouse));
		List->AddRectFilled(GraphBox.Min, GraphBox.Max, ImGui::GetColorU32(ImGuiCol_Header, 0.4f));
		std::sort(KeyGroup.begin(), KeyGroup.end());
		if (Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Left] >= 9.0f)
		{
			SelectedKeys.clear();
			std::set_symmetric_difference(DragKeyPrevious.begin(), Input.KeyCtrl ? DragKeyPrevious.end() : DragKeyPrevious.begin(), KeyGroup.begin(), KeyGroup.end(), std::back_inserter(SelectedKeys));
		}
		else if (!ImGui::IsMouseDown(ImGuiMouseButton_Left) && !Input.KeyCtrl)
		{
			SelectedKeys.clear();
		}
		KeyGroup.clear();
	}
	GraphDrag = ImGui::IsMouseDown(ImGuiMouseButton_Left) ? GraphDrag : 0;
	List->PopClipRect();
	ImGui::End();
}
}
