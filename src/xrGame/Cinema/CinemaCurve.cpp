#include "StdAfx.h"
#include "CinemaCurve.h"

namespace Cinema::Curves
{
static constexpr double GaussNodes[8] = { -0.9602898564975363, -0.7966664774136267, -0.5255324099163290, -0.1834346424956498, 0.1834346424956498, 0.5255324099163290, 0.7966664774136267, 0.9602898564975363 };
static constexpr double GaussWeights[8] = { 0.1012285362903763, 0.2223810344533745, 0.3137066458778873, 0.3626837833783620, 0.3626837833783620, 0.3137066458778873, 0.2223810344533745, 0.1012285362903763 };
static const Curve NoCurve {};

static constexpr double BezierSlope(double A, double B, double C, double D, double S)
{
	const double Left = 1.0 - S;
	return 3.0 * (Left * Left * (B - A) + 2.0 * Left * S * (C - B) + S * S * (D - C));
}

static constexpr u32 NoiseHash(u32 Input)
{
	Input ^= Input >> 16;
	Input *= 0x7feb352du;
	Input ^= Input >> 15;
	Input *= 0x846ca68bu;
	return Input ^ (Input >> 16);
}

static float NoiseSample(s64 Cell)
{
	return static_cast<float>(NoiseHash(static_cast<u32>(Cell)) >> 8) * (2.0f / 16777215.0f) - 1.0f;
}

static float SmoothNoise(float Cycle)
{
	const float Whole = floorf(Cycle);
	const float Fraction = Cycle - Whole;
	const float Blend = Fraction * Fraction * (3.0f - 2.0f * Fraction);
	const s64 Cell = static_cast<s64>(Whole);
	const float A = NoiseSample(Cell);
	return A + (NoiseSample(Cell + 1) - A) * Blend;
}

static double BezierAt(const Key& A, const Key& B, double Local, double Tension = 1.0)
{
	const double Span = static_cast<double>(B.Time) - A.Time;
	const double X1 = std::clamp<double>(A.OutTime, 0.0, Span);
	const double X2 = Span + std::clamp<double>(B.InTime, -Span, 0.0);
	double S = std::clamp(Local / Span, 0.0, 1.0), Lo = 0.0, Hi = 1.0;
	for (u32 Iteration = 0; Iteration < 8; ++Iteration)
	{
		const double Error = BezierValue(0.0, X1, X2, Span, S) - Local;
		(Error > 0.0 ? Hi : Lo) = S;
		const double Slope = BezierSlope(0.0, X1, X2, Span, S);
		const double Next = Slope > 1e-6 ? S - Error / Slope : -1.0;
		S = Next >= Lo && Next <= Hi ? Next : 0.5 * (Lo + Hi);
	}
	return BezierValue(A.Value, A.Value + A.OutValue * Tension, B.Value + B.InValue * Tension, B.Value, S);
}

static float SegmentValue(const Key& A, const Key& B, float Time, float Tension)
{
	if (A.Mode == Interp::Bezier)
	{
		return static_cast<float>(BezierAt(A, B, static_cast<double>(Time) - A.Time, Tension));
	}
	return A.Value + (B.Value - A.Value) * Ease(A.Mode, (Time - A.Time) / (B.Time - A.Time));
}

static double SegmentArea(const Key& A, const Key& B, float End, const ParamInfo& Meta)
{
	const double Span = static_cast<double>(B.Time) - A.Time;
	const double Width = static_cast<double>(End) - A.Time;
	if (A.Mode != Interp::Bezier)
	{
		return Width * A.Value + Span * (static_cast<double>(B.Value) - A.Value) * EaseArea(A.Mode, static_cast<float>(Width / Span));
	}
	double Total = 0.0;
	for (u32 Index = 0; Index < 8; ++Index)
	{
		Total += GaussWeights[Index] * std::clamp<double>(BezierAt(A, B, 0.5 * Width * (1.0 + GaussNodes[Index])), Meta.Min, Meta.Max);
	}
	return 0.5 * Width * Total;
}

static double KeysArea(const Curve& Source, float Time, float Rate, const ParamInfo& Meta)
{
	const xr_vector<Key>& Keys = Source.Keys;
	if (!(Time > 0.0f))
	{
		return 0.0;
	}
	if (Keys.empty())
	{
		return static_cast<double>(Rate) * Time;
	}
	double Total = static_cast<double>(Keys.front().Value) * std::min(Time, Keys.front().Time);
	for (size_t Index = 1; Index < Keys.size() && Time > Keys[Index - 1].Time; ++Index)
	{
		Total += SegmentArea(Keys[Index - 1], Keys[Index], std::min(Time, Keys[Index].Time), Meta);
	}
	return Time > Keys.back().Time ? Total + static_cast<double>(Keys.back().Value) * (Time - Keys.back().Time) : Total;
}

static bool UsesTable(const Curve& Source, const AreaTable* Table)
{
	VERIFY(Source.Gen.Type == Wave::None || Table);
	return Source.Gen.Type != Wave::None && Table && !Table->Prefix.empty();
}

static double TableArea(const AreaTable& Table, float Time, const ParamInfo& Meta)
{
	if (!(Time > 0.0f))
	{
		return 0.0;
	}
	const size_t Last = Table.Prefix.size() - 1;
	const double Cell = static_cast<double>(Time) / Table.Step;
	if (Cell >= static_cast<double>(Last))
	{
		return Table.Prefix[Last] + Result(Table.Source, Table.Cover, Table.Rate, Meta) * (Time - Last * static_cast<double>(Table.Step));
	}
	const size_t Index = static_cast<size_t>(Cell);
	return Table.Prefix[Index] + (Table.Prefix[Index + 1] - Table.Prefix[Index]) * (Cell - static_cast<double>(Index));
}

static float TableInverse(const AreaTable& Table, double TargetArea, float Limit, const ParamInfo& Meta)
{
	const auto Upper = std::lower_bound(Table.Prefix.begin(), Table.Prefix.end(), TargetArea);
	if (Upper == Table.Prefix.end())
	{
		const double Speed = Result(Table.Source, Table.Cover, Table.Rate, Meta);
		const double End = (Table.Prefix.size() - 1) * static_cast<double>(Table.Step);
		return Speed > 0.0 ? static_cast<float>(std::min<double>(End + (TargetArea - Table.Prefix.back()) / Speed, Limit)) : Limit;
	}
	const size_t Index = static_cast<size_t>(Upper - Table.Prefix.begin());
	const double Below = Table.Prefix[Index - 1];
	const double Cell = static_cast<double>(Index - 1) + (TargetArea - Below) / (*Upper - Below);
	return static_cast<float>(std::min<double>(Cell * Table.Step, Limit));
}

static const Curve& LaneCurve(const xr_vector<Lane>& Lanes, Param Id, u8 Layer)
{
	const Lane* Found = FindLane(Lanes, Id, Layer);
	return Found ? Found->Curves[0] : NoCurve;
}

float Ease(Interp Mode, float U)
{
	switch (Mode)
	{
		case Interp::EaseIn: return U * U;
		case Interp::EaseOut: return U * (2.0f - U);
		case Interp::EaseInOut: return U * U * (3.0f - 2.0f * U);
		case Interp::Hold: return 0.0f;
		default: return U;
	}
}

float EaseArea(Interp Mode, float U)
{
	switch (Mode)
	{
		case Interp::EaseIn: return U * U * U / 3.0f;
		case Interp::EaseOut: return U * U - U * U * U / 3.0f;
		case Interp::EaseInOut: return U * U * U - 0.5f * U * U * U * U;
		case Interp::Hold: return 0.0f;
		default: return 0.5f * U * U;
	}
}

float WaveSignal(Wave Type, float Cycle)
{
	const float Whole = floorf(Cycle);
	const float Fraction = Cycle - Whole;
	switch (Type)
	{
		case Wave::Sine: return sinf(Cycle * PI_MUL_2);
		case Wave::Triangle: return Fraction < 0.5f ? 4.0f * Fraction - 1.0f : 3.0f - 4.0f * Fraction;
		case Wave::Square: return Fraction < 0.5f ? 1.0f : -1.0f;
		case Wave::Sawtooth: return 2.0f * Fraction - 1.0f;
		case Wave::NoiseStepped: return NoiseSample(static_cast<s64>(Whole));
		case Wave::NoiseSmooth: return SmoothNoise(Cycle);
		case Wave::NoiseFractal: return (SmoothNoise(Cycle) + 0.5f * SmoothNoise(Cycle * 2.0f) + 0.25f * SmoothNoise(Cycle * 4.0f) + 0.125f * SmoothNoise(Cycle * 8.0f)) / 1.875f;
		case Wave::NoisePerlin:
		{
			const s64 Cell = static_cast<s64>(Whole);
			const float Left = (NoiseHash(static_cast<u32>(Cell)) & 1) ? Fraction : -Fraction;
			const float Right = (NoiseHash(static_cast<u32>(Cell + 1)) & 1) ? Fraction - 1.0f : 1.0f - Fraction;
			const float Blend = Fraction * Fraction * Fraction * (Fraction * (Fraction * 6.0f - 15.0f) + 10.0f);
			return 2.0f * (Left + (Right - Left) * Blend);
		}
		case Wave::NoiseContinuous:
		{
			float Signal = 0.0f;
			for (u32 Partial = 0; Partial < 32; ++Partial)
			{
				const float Phase = static_cast<float>(NoiseHash(Partial + 1) >> 8) / 16777216.0f;
				const float Offset = static_cast<float>(NoiseHash(Partial + 33) >> 8) / 16777216.0f;
				const float Cycles = Cycle * (static_cast<float>(Partial) + Offset) / 32.0f + Phase;
				Signal += sinf((Cycles - floorf(Cycles)) * PI_MUL_2);
			}
			return std::clamp(Signal * 0.25f, -1.0f, 1.0f);
		}
		case Wave::Oscillator:
		{
			const float Amplitude = 0.6f + 0.4f * SmoothNoise(Cycle * 0.5f + 19.0f);
			const float Phase = Cycle + 0.3f * SmoothNoise(Cycle * 0.25f + 47.0f);
			return Amplitude * sinf((Phase - floorf(Phase)) * PI_MUL_2);
		}
		default: return 0.0f;
	}
}

float Value(const Curve& Source, float Time, float Fallback, float Tension)
{
	float Offset = 0.0f;
	const xr_vector<Key>& Keys = Source.Keys;
	if (Keys.empty())
	{
		return Fallback;
	}
	const float First = Keys.front().Time, Span = Keys.back().Time - First;
	const u8 Mode = Time < First ? Source.Pre : Time > Keys.back().Time ? Source.Post : 0;
	if (Mode && Span > 0.0f && Keys.size() > 1)
	{
		const bool IsBefore = Time < First;
		const Key &Near = IsBefore ? Keys[0] : Keys.back(), &Far = IsBefore ? Keys[1] : Keys[Keys.size() - 2];
		const double Turns = std::floor((Time - First) / Span), Inside = Time - First - Turns * Span;
		const bool IsMirror = Mode == 4 && (static_cast<s64>(Turns) & 1);
		const float Folded = First + static_cast<float>(IsMirror ? Span - Inside : Inside);
		const float Slope = (IsBefore ? Near.Mode : Far.Mode) == Interp::Hold ? 0.0f : (Near.Value - Far.Value) / (Near.Time - Far.Time);
		if (Mode == 1)
		{
			return Near.Value + Slope * (Time - Near.Time);
		}
		Offset = Mode == 3 ? static_cast<float>(Turns) * (Keys.back().Value - Keys.front().Value) : 0.0f;
		Time = Folded;
	}
	const auto Next = std::upper_bound(Keys.begin(), Keys.end(), Time, [](float Moment, const Key& Item) { return Moment < Item.Time; });
	if (Next == Keys.begin())
	{
		return Keys.front().Value + Offset;
	}
	return Offset + (Next == Keys.end() ? Keys.back().Value : SegmentValue(*(Next - 1), *Next, Time, Tension));
}

float Result(const Curve& Source, float Time, float Fallback, const ParamInfo& Meta, float Tension)
{
	const Generator& Gen = Source.Gen;
	const float Lo = std::max(Meta.Min, Gen.Min);
	const float Hi = std::max(Lo, std::min(Meta.Max, Gen.Max));
	return std::clamp(Value(Source, Time, Fallback, Tension) + Gen.Amplitude * WaveSignal(Gen.Type, Time * Gen.Frequency + Gen.Phase), Lo, Hi);
}

void Refresh(AreaTable& Table, const Curve& Source, float Rate, float Step, float Cover, const ParamInfo& Meta)
{
	if (Table.Source == Source && Table.Rate == Rate && Table.Step == Step && Table.Cover == Cover)
	{
		return;
	}
	Table.Source = Source;
	Table.Rate = Rate;
	Table.Step = Step;
	Table.Cover = Cover;
	Table.Prefix.clear();
	if (Source.Gen.Type == Wave::None || !std::isfinite(Step) || !(Step > 0.0f))
	{
		return;
	}
	const double Cells = Cover > 0.0f ? std::min(std::ceil(static_cast<double>(Cover) / Step), static_cast<double>(MaxTime) * 60.0) : 0.0;
	Table.Prefix.resize(static_cast<size_t>(Cells) + 1);
	double Previous = Result(Source, 0.0f, Rate, Meta);
	for (size_t Index = 1; Index < Table.Prefix.size(); ++Index)
	{
		const double Current = Result(Source, static_cast<float>(Index * static_cast<double>(Step)), Rate, Meta);
		Table.Prefix[Index] = Table.Prefix[Index - 1] + 0.5 * (Previous + Current) * Step;
		Previous = Current;
	}
}

double Area(const Curve& Source, const AreaTable* Table, float Time, float Rate, const ParamInfo& Meta)
{
	return UsesTable(Source, Table) ? TableArea(*Table, Time, Meta) : KeysArea(Source, Time, Rate, Meta);
}

float InverseArea(const Curve& Source, const AreaTable* Table, float Rate, const ParamInfo& Meta, double TargetArea, float Limit)
{
	if (!(TargetArea > 0.0))
	{
		return 0.0f;
	}
	if (UsesTable(Source, Table))
	{
		return TableInverse(*Table, TargetArea, Limit, Meta);
	}
	if (Source.Keys.empty())
	{
		return Rate > 0.0f ? static_cast<float>(std::min<double>(TargetArea / Rate, Limit)) : Limit;
	}
	if (KeysArea(Source, Limit, Rate, Meta) < TargetArea)
	{
		return Limit;
	}
	float Lo = 0.0f, Hi = Limit;
	for (u32 Iteration = 0; Iteration < 32; ++Iteration)
	{
		const float Mid = 0.5f * (Lo + Hi);
		(KeysArea(Source, Mid, Rate, Meta) < TargetArea ? Lo : Hi) = Mid;
	}
	return Hi;
}

const Lane* FindLane(const xr_vector<Lane>& Lanes, Param Id, u8 Layer)
{
	const auto Found = std::find_if(Lanes.begin(), Lanes.end(), [Id, Layer](const Lane& Item) { return Item.Id == Id && Item.Layer == Layer; });
	return Found == Lanes.end() ? nullptr : &*Found;
}

Lane& EnsureLane(xr_vector<Lane>& Lanes, Param Id, u8 Layer)
{
	if (const Lane* Found = FindLane(Lanes, Id, Layer))
	{
		return Lanes[static_cast<size_t>(Found - Lanes.data())];
	}
	return Lanes.emplace_back(Lane { Id, Layer });
}

float LaneScalar(const xr_vector<Lane>& Lanes, Param Id, u8 Layer, float Time, float Fallback)
{
	const Lane* Found = FindLane(Lanes, Id, Layer);
	return Found ? Result(Found->Curves[0], Time, Fallback, Info(Id)) : Fallback;
}

Fvector LaneVector(const xr_vector<Lane>& Lanes, Param Id, float Time, const Fvector& Fallback, u8 Layer, float Tension)
{
	const Lane* Found = FindLane(Lanes, Id, Layer);
	if (!Found)
	{
		return Fallback;
	}
	const ParamInfo& Meta = Info(Id);
	return Fvector().set(Result(Found->Curves[0], Time, Fallback.x, Meta, Tension), Result(Found->Curves[1], Time, Fallback.y, Meta, Tension), Result(Found->Curves[2], Time, Fallback.z, Meta, Tension));
}

double LaneArea(const xr_vector<Lane>& Lanes, const AreaTable* Table, Param Id, u8 Layer, float Time, float Rate)
{
	return Area(LaneCurve(Lanes, Id, Layer), Table, Time, Rate, Info(Id));
}

float LaneInverseArea(const xr_vector<Lane>& Lanes, const AreaTable* Table, Param Id, u8 Layer, float Rate, double TargetArea, float Limit)
{
	return InverseArea(LaneCurve(Lanes, Id, Layer), Table, Rate, Info(Id), TargetArea, Limit);
}

u32 KeyAt(const Curve& Source, float Time, float HalfFrame)
{
	const auto Found = std::lower_bound(Source.Keys.begin(), Source.Keys.end(), Time - HalfFrame, [](const Key& Item, float Moment) { return Item.Time < Moment; });
	return Found != Source.Keys.end() && Found->Time <= Time + HalfFrame ? static_cast<u32>(Found - Source.Keys.begin()) : u32(-1);
}

void SetKey(Curve& Target, float Time, float NewValue, float HalfFrame)
{
	const u32 Index = KeyAt(Target, Time, HalfFrame);
	if (Index != u32(-1))
	{
		Target.Keys[Index].Value = NewValue;
		return;
	}
	if (Target.Keys.size() >= MaxLaneKeys)
	{
		return;
	}
	const auto Next = std::upper_bound(Target.Keys.begin(), Target.Keys.end(), Time, [](float Moment, const Key& Item) { return Moment < Item.Time; });
	Target.Keys.insert(Next, Key { Time, NewValue });
}

void PlaceKey(Curve& Target, const Key& Item, float HalfFrame)
{
	const auto First = std::lower_bound(Target.Keys.begin(), Target.Keys.end(), Item.Time - HalfFrame, [](const Key& Other, float Moment) { return Other.Time < Moment; });
	Target.Keys.erase(First, std::upper_bound(First, Target.Keys.end(), Item.Time + HalfFrame, [](float Moment, const Key& Other) { return Moment < Other.Time; }));
	if (Target.Keys.size() < MaxLaneKeys)
	{
		Target.Keys.insert(std::upper_bound(Target.Keys.begin(), Target.Keys.end(), Item.Time, [](float Moment, const Key& Other) { return Moment < Other.Time; }), Item);
	}
}

void SetMode(Curve& Target, u32 Index, Interp Mode)
{
	Key& Item = Target.Keys[Index];
	Item.Mode = Mode;
	if (Mode != Interp::Bezier || Index + 1 >= Target.Keys.size())
	{
		return;
	}
	Key& Next = Target.Keys[Index + 1];
	const float Width = (Next.Time - Item.Time) / 3.0f, Rise = (Next.Value - Item.Value) / 3.0f;
	if (Item.OutTime == 0.0f && Item.OutValue == 0.0f)
	{
		Item.OutTime = Width;
		Item.OutValue = Rise;
	}
	if (Next.InTime == 0.0f && Next.InValue == 0.0f)
	{
		Next.InTime = -Width;
		Next.InValue = -Rise;
	}
}

u32 Fit(Curve& Target, float From, float To, float Tolerance)
{
	xr_vector<Key>& Keys = Target.Keys;
	const auto Begin = std::lower_bound(Keys.begin(), Keys.end(), From - 1e-4f, [](const Key& Item, float Moment) { return Item.Time < Moment; });
	const auto End = std::upper_bound(Begin, Keys.end(), To + 1e-4f, [](float Moment, const Key& Item) { return Moment < Item.Time; });
	const u32 Count = static_cast<u32>(End - Begin);
	if (Count < 3)
	{
		return 0;
	}
	const xr_vector<Key> Old(Begin, End);
	xr_vector<u8> Kept(Count, 0);
	Kept.front() = Kept.back() = 1;
	const auto Slope = [&Old, Count](u32 Index)
	{
		const Key &A = Old[Index ? Index - 1 : 0], &B = Old[std::min(Index + 1, Count - 1)];
		return (B.Value - A.Value) / std::max(B.Time - A.Time, 1e-6f);
	};
	const auto Cubic = [&Old, &Slope](u32 A, u32 B, float Time)
	{
		const float Width = Old[B].Time - Old[A].Time, U = (Time - Old[A].Time) / std::max(Width, 1e-6f), V = 1.0f - U;
		const float P1 = Old[A].Value + Slope(A) * Width / 3.0f, P2 = Old[B].Value - Slope(B) * Width / 3.0f;
		return V * V * V * Old[A].Value + 3.0f * V * V * U * P1 + 3.0f * V * U * U * P2 + U * U * U * Old[B].Value;
	};
	for (bool IsSplit = true; IsSplit;)
	{
		IsSplit = false;
		for (u32 A = 0, B = 1; A + 1 < Count; A = B, B = A + 1)
		{
			while (!Kept[B])
			{
				++B;
			}
			float Worst = Tolerance;
			u32 At = 0;
			for (u32 Index = A + 1; Index < B; ++Index)
			{
				const float Miss = std::fabs(Cubic(A, B, Old[Index].Time) - Old[Index].Value);
				At = Miss > Worst ? Index : At;
				Worst = std::max(Worst, Miss);
			}
			if (At)
			{
				Kept[At] = 1;
				IsSplit = true;
			}
		}
	}
	xr_vector<Key> Made;
	for (u32 Index = 0, Last = 0; Index < Count; ++Index)
	{
		if (!Kept[Index])
		{
			continue;
		}
		Key Item = Old[Index];
		Item.IsBroken = Item.IsBroken && (Index == 0 || Index + 1 == Count);
		if (Index)
		{
			const float Third = (Item.Time - Old[Last].Time) / 3.0f;
			Made.back().Mode = Interp::Bezier;
			Made.back().OutTime = Third;
			Made.back().OutValue = Slope(Last) * Third;
			Item.InTime = -Third;
			Item.InValue = -Slope(Index) * Third;
		}
		Made.push_back(Item);
		Last = Index;
	}
	const u32 Removed = Count - static_cast<u32>(Made.size());
	Keys.insert(Keys.erase(Begin, End), Made.begin(), Made.end());
	return Removed;
}

void EasyEase(Curve& Target, u32 Index)
{
	if (Index > 0)
	{
		SetMode(Target, Index - 1, Interp::Bezier);
	}
	Key& Item = Target.Keys[Index];
	Item.Mode = Interp::Bezier;
	Item.InTime = Index > 0 ? (Target.Keys[Index - 1].Time - Item.Time) / 3.0f : 0.0f;
	Item.OutTime = Index + 1 < Target.Keys.size() ? (Target.Keys[Index + 1].Time - Item.Time) / 3.0f : 0.0f;
	Item.InValue = Item.OutValue = 0.0f;
}
}
