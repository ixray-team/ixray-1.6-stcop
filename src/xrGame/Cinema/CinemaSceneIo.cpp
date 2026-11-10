#include "StdAfx.h"
#include "CinemaSceneIo.h"
#include "CinemaCamera.h"
#include "CinemaShake.h"
#include "CinemaOmf.h"

namespace Cinema::Io
{
struct Range
{
	float Lo;
	float Hi;
	float Fallback;
	bool IsWhole;
};

constexpr u32 IdCap = 0x7FFFFFFFu;
constexpr size_t MaxFileSize = 16u << 20;
constexpr Range SecondsRange = { 0.0f, MaxTime, 0.0f, false };
constexpr Range MarkerRange = { 0.0f, MaxTime, -1.0f, false };
constexpr Range LengthRange = { MinLength, MaxTime, 5.0f, false };
constexpr Range DistanceRange = { -100000.0f, 100000.0f, 0.0f, false };
constexpr Range AngleRange = { -3600.0f, 3600.0f, 0.0f, false };
constexpr Range ScaleRange = { 0.1f, 5.0f, 1.0f, false };
constexpr Range RateRange = { 0.0f, 4.0f, 1.0f, false };
constexpr Range ModeRange = { 0.0f, 5.0f, 3.0f, true };
constexpr Range EaseRange = { 0.0f, 3.0f, 3.0f, true };
constexpr Range KeyRanges[5] = { ModeRange, { -MaxTime, 0.0f, 0.0f, false }, DistanceRange, SecondsRange, DistanceRange };
constexpr Range PoseRanges[4] = { AngleRange, AngleRange, AngleRange, ModeRange };
constexpr Range RestRanges[6] = { DistanceRange, DistanceRange, DistanceRange, AngleRange, AngleRange, AngleRange };
constexpr Range UnitRange = { -1.0f, 1.0f, 0.0f, false }, HandleRange = { -MaxHandle, MaxHandle, 0.0f, false };
constexpr Range TangentRanges[10] = { { 0.0f, 2.0f, 0.0f, true }, UnitRange, UnitRange, UnitRange, HandleRange, HandleRange, HandleRange, HandleRange, HandleRange, HandleRange };
constexpr Range BlockRanges[9] = { SecondsRange, SecondsRange, { -4.0f, 4.0f, 1.0f, false }, SecondsRange, { -MaxTime, MaxTime, 0.0f, false }, EaseRange, { 1.0f, MaxLoops, 1.0f, false }, SecondsRange, SecondsRange };
constexpr Range PackedRanges[6] = { SecondsRange, SecondsRange, { -4.0f, 4.0f, 1.0f, false }, SecondsRange, { 0.0f, 1.0f, 0.0f, false }, EaseRange };
constexpr Range ShotRanges[5] = { LengthRange, { 0.0f, MaxCameraBlend, 0.0f, false }, EaseRange, { 0.0f, 179.0f, 0.0f, false }, RateRange };
constexpr Range TrackRanges[5] = { LengthRange, EaseRange, { 0.0f, 179.0f, 0.0f, false }, RateRange, SecondsRange };
constexpr Range ClipRanges[6] = { SecondsRange, { MinLength, MaxTime, 2.0f, false }, SecondsRange, SecondsRange, { 0.0f, static_cast<float>(MaxShakeRows - 1), 0.0f, true }, EaseRange };
constexpr Range ShakeRanges[7] = { { 0.0f, static_cast<float>(ShakePresetCount - 1), 0.0f, true }, { 0.0f, 1.0f, 1.0f, true }, { 0.0f, 4.0f, 1.0f, false }, { 0.0f, 100.0f, 1.0f, false }, { 0.0f, 100.0f, 1.0f, false }, { -MaxTime, MaxTime, 0.0f, false }, { 0.0f, 1.0f, 0.0f, true } };
constexpr Range DofRanges[6] = { { 0.0f, 2.0f, 0.0f, true }, { 0.05f, 10000.0f, 5.0f, false }, { -1000.0f, 1000.0f, 0.0f, false }, { 0.01f, 1000.0f, 2.0f, false }, { 0.01f, 10000.0f, 20.0f, false }, { 0.0f, 10.0f, 5.0f, false } };
constexpr Range ColorRange = { 0.0f, 100000.0f, 1.0f, false };
constexpr Range LightRanges[7] = { ColorRange, ColorRange, ColorRange, { 0.0f, 100.0f, 1.0f, false }, { 0.1f, 1000.0f, 8.0f, false }, { 1.0f, 120.0f, 60.0f, false }, { 0.0f, 100.0f, 0.1f, false } };
constexpr Range VolumeRanges[3] = { { 0.0f, 100.0f, 1.0f, false }, { 0.01f, 1.0f, 1.0f, false }, { 0.01f, 1.0f, 1.0f, false } };
constexpr Range EffectRanges[2] = { { 0.01f, 25.0f, 1.0f, false }, RateRange };
constexpr Range WeightRange = { 0.0f, 1.0f, 1.0f, false };
constexpr u32 MaxPartNames = 8;
constexpr const char* TypeNames[7] = { "actor", "camera", "light", "particle", "decal", "weather", "post" };
constexpr const char* KindNames[2] = { "point", "spot" };
constexpr const char* EndNames[3] = { "hold", "loop", "pingpong" };
constexpr u32 FrameRates[4] = { 24, 25, 30, 60 };

void Warnings::Add(const char* Format, ...)
{
	string512 Text;
	va_list Args;
	va_start(Args, Format);
	std::vsnprintf(Text, sizeof(Text), Format, Args);
	va_end(Args);
	if (std::any_of(Lines.begin(), Lines.end(), [&](const xr_string& Line) { return !xr_strcmp(Line.c_str(), Text); }))
	{
		return;
	}
	if (Lines.size() < 64)
	{
		Lines.emplace_back(Text);
		return;
	}
	++Dropped;
}

float ToFloat(double Value)
{
	return std::fabs(Value) <= FLT_MAX ? static_cast<float>(Value) : std::numeric_limits<float>::quiet_NaN();
}

bool ParseNumbers(const char* Text, double* Out, u32 Count)
{
	for (u32 Index = 0; Index < Count; ++Index)
	{
		char* End = nullptr;
		Out[Index] = std::strtod(Text, &End);
		if (End == Text || *End != (Index + 1 < Count ? ',' : '\0'))
		{
			return false;
		}
		Text = End + 1;
	}
	return true;
}

template <typename T>
bool ReadList(const CInifile& Ini, const char* Section, const char* Line, T* Out, u32 Count, Warnings& Notes)
{
	const char* Text = Ini.read_if_exists<str_c>(Section, Line, nullptr);
	double Values[16] = {};
	if (!Text)
	{
		return false;
	}
	bool IsGood = Count <= 16 && ParseNumbers(Text, Values, Count);
	for (u32 Index = 0; IsGood && Index < Count; ++Index)
	{
		IsGood = std::is_floating_point_v<T> || (Values[Index] >= 0.0 && Values[Index] <= 4294967295.0 && Values[Index] == std::floor(Values[Index]));
	}
	if (!IsGood)
	{
		Notes.Add("[%s] %s = %s is not readable, ignored", Section, Line, Text);
		return false;
	}
	for (u32 Index = 0; Index < Count; ++Index)
	{
		if constexpr (std::is_floating_point_v<T>)
		{
			Out[Index] = ToFloat(Values[Index]);
		}
		else
		{
			Out[Index] = static_cast<T>(Values[Index]);
		}
	}
	return true;
}

bool ReadFloats(const CInifile& Ini, const char* Section, const char* Line, float* Out, u32 Count, Warnings& Notes)
{
	return ReadList(Ini, Section, Line, Out, Count, Notes);
}

u32 ReadU32(const CInifile& Ini, const char* Section, const char* Line, u32 Cap, Warnings& Notes)
{
	u32 Value = 0;
	ReadList(Ini, Section, Line, &Value, 1, Notes);
	if (Value > Cap)
	{
		Notes.Add("[%s] %s = %u is above %u, %u is used", Section, Line, Value, Cap, Cap);
		Value = Cap;
	}
	return Value;
}

bool ClampAll(float* Values, const Range* Ranges, u32 Count)
{
	bool IsChanged = false;
	for (u32 Index = 0; Index < Count; ++Index)
	{
		const float Old = Values[Index];
		const float New = Sanitize(Old, Ranges[Index].Lo, Ranges[Index].Hi, Ranges[Index].Fallback);
		Values[Index] = Ranges[Index].IsWhole ? std::floor(New) : New;
		IsChanged = IsChanged || !(Values[Index] == Old);
	}
	return IsChanged;
}

bool ReadRanged(const CInifile& Ini, const char* Section, const char* Line, float* Out, const Range* Ranges, u32 Count, Warnings& Notes)
{
	if (!ReadFloats(Ini, Section, Line, Out, Count, Notes))
	{
		return false;
	}
	if (ClampAll(Out, Ranges, Count))
	{
		Notes.Add("[%s] %s is out of range, clamped", Section, Line);
	}
	return true;
}

void ReadVector(const CInifile& Ini, const char* Section, const char* Line, const Range& Bounds, Fvector& Out, Warnings& Notes)
{
	float Values[3] = { Out.x, Out.y, Out.z };
	const Range Ranges[3] = { Bounds, Bounds, Bounds };
	if (ReadRanged(Ini, Section, Line, Values, Ranges, 3, Notes))
	{
		Out.set(Values[0], Values[1], Values[2]);
	}
}

bool IniSafe(const char* Text)
{
	const u32 Length = Text ? xr_strlen(Text) : 0;
	if (Length == 0 || Length > MaxIdentifier || strstr(Text, "//"))
	{
		return false;
	}
	return std::none_of(Text, Text + Length, [](char Symbol) { return static_cast<u8>(Symbol) <= ' ' || Symbol == 127 || strchr(",;\"|=[]#!", Symbol); });
}

bool IsOmfPath(const char* Path)
{
	const u32 Length = Path ? xr_strlen(Path) : 0;
	return Length > 4 && !xr_stricmp(Path + Length - 4, ".omf");
}

bool DecodeName(const char* Hex, xr_string& Out)
{
	const auto Digit = [](char Symbol) { return Symbol >= '0' && Symbol <= '9' ? Symbol - '0' : Symbol >= 'a' && Symbol <= 'f' ? Symbol - 'a' + 10 : -1; };
	const u32 Length = Hex ? xr_strlen(Hex) : 0;
	Out.clear();
	if (Length > MaxNameBytes * 2 || Length % 2)
	{
		return false;
	}
	for (u32 Index = 0; Index < Length; Index += 2)
	{
		const s32 High = Digit(Hex[Index]);
		const s32 Low = Digit(Hex[Index + 1]);
		if (High < 0 || Low < 0 || (High | Low) == 0)
		{
			Out.clear();
			return false;
		}
		Out.push_back(static_cast<char>((High << 4) | Low));
	}
	return true;
}

shared_str ReadSafe(const CInifile& Ini, const char* Section, const char* Line, Warnings& Notes)
{
	const char* Text = Ini.read_if_exists<str_c>(Section, Line, nullptr);
	if (Text && !IniSafe(Text))
	{
		Notes.Add("[%s] %s has unsupported characters, ignored", Section, Line);
		return {};
	}
	return Text;
}

xr_string ReadName(const CInifile& Ini, const char* Section, const char* Line, Warnings& Notes)
{
	xr_string Name;
	if (!DecodeName(Ini.read_if_exists<str_c>(Section, Line, nullptr), Name))
	{
		Notes.Add("[%s] %s is not a hex name, ignored", Section, Line);
	}
	return Name;
}

s32 BoolWord(const char* Text)
{
	static constexpr const char* Words[8] = { "on", "off", "true", "false", "yes", "no", "1", "0" };
	for (u32 Index = 0; Text && Index < 8; ++Index)
	{
		if (!xr_stricmp(Text, Words[Index]))
		{
			return Index % 2 == 0;
		}
	}
	return -1;
}

bool ReadBool(const CInifile& Ini, const char* Section, const char* Line, bool Default, Warnings& Notes)
{
	const char* Text = Ini.read_if_exists<str_c>(Section, Line, nullptr);
	const s32 Word = BoolWord(Text);
	if (Text && Word < 0)
	{
		Notes.Add("[%s] %s = %s is not on or off, %s is used", Section, Line, Text, Default ? "on" : "off");
	}
	return Word < 0 ? Default : Word == 1;
}

bool ReadLink(const CInifile& Ini, const char* Section, const char* Line, AttachLink& Link, bool* Flags, u32 FlagCount, Warnings& Notes)
{
	const char* Text = Ini.read_if_exists<str_c>(Section, Line, nullptr);
	string512 Copy = {};
	char* Fields[5] = {};
	u32 Count = 0;
	if (!Text)
	{
		return false;
	}
	if (xr_strlen(Text) < sizeof(Copy))
	{
		xr_strcpy(Copy, Text);
		for (char* Cursor = Copy; Cursor && Count < 5; ++Count)
		{
			Fields[Count] = Cursor;
			Cursor = strchr(Cursor, ',');
			Cursor = Cursor ? (*Cursor = 0, Cursor + 1) : nullptr;
		}
	}
	double Id = 0.0;
	bool Words[2] = {};
	bool IsGood = Count == 2 + FlagCount && ParseNumbers(Fields[0], &Id, 1) && Id >= 1.0 && Id <= 4294967295.0 && Id == std::floor(Id) && (!Fields[1][0] || IniSafe(Fields[1]));
	for (u32 Index = 0; IsGood && Index < FlagCount; ++Index)
	{
		const s32 Word = BoolWord(Fields[2 + Index]);
		IsGood = Word >= 0;
		Words[Index] = Word == 1;
	}
	if (!IsGood)
	{
		Notes.Add("[%s] %s = %s is not readable, link dropped", Section, Line, Text);
		return false;
	}
	Link.ObjectId = static_cast<u32>(Id);
	Link.Bone = Fields[1][0] ? shared_str(Fields[1]) : shared_str();
	std::copy(Words, Words + FlagCount, Flags);
	return true;
}

void ReadLinks(const CInifile& Ini, const char* Section, const char* Prefix, Attachment& Out, Warnings& Notes)
{
	string64 Line;
	bool Flags[2] = {};
	xr_sprintf(Line, "%sparent", Prefix);
	if (ReadLink(Ini, Section, Line, Out.Parent, Flags, 2, Notes))
	{
		Out.IsRollLocked = Flags[0];
		Out.IsPitchLocked = Flags[1];
	}
	xr_sprintf(Line, "%slook_at", Prefix);
	if (ReadLink(Ini, Section, Line, Out.LookAt, Flags, 1, Notes))
	{
		Out.IsYawOnly = Flags[0];
	}
	xr_sprintf(Line, "%sweight", Prefix);
	float Weight = 1.0f;
	Out.Weight = ReadFloats(Ini, Section, Line, &Weight, 1, Notes) ? Sanitize(Weight, 0.0f, 1.0f, 1.0f) : 1.0f;
	xr_sprintf(Line, "%sswitches", Prefix);
	const u32 Count = Ini.line_exist(Section, Line) ? ReadU32(Ini, Section, Line, MaxAttachSwitches, Notes) : 0;
	for (u32 Index = 0; Index < Count; ++Index)
	{
		AttachSwitch Item;
		float Moment = 0.0f;
		xr_sprintf(Line, "%sswitch_%02u", Prefix, Index);
		ReadFloats(Ini, Section, Line, &Moment, 1, Notes);
		Item.Time = Sanitize(Moment, 0.0f, MaxTime, 0.0f);
		xr_sprintf(Line, "%sswitch_%02u_bone", Prefix, Index);
		Item.Bone = ReadSafe(Ini, Section, Line, Notes);
		Out.Switches.push_back(Item);
	}
	std::stable_sort(Out.Switches.begin(), Out.Switches.end(), [](const AttachSwitch& A, const AttachSwitch& B) { return A.Time < B.Time; });
}

void WriteLinks(CInifile& Ini, const char* Section, const char* Prefix, const Attachment& Links)
{
	string64 Line;
	string512 Text;
	const auto Bone = [](const AttachLink& Link) { return Link.Bone.size() ? Link.Bone.c_str() : ""; };
	if (Links.Parent.ObjectId)
	{
		xr_sprintf(Line, "%sparent", Prefix);
		xr_sprintf(Text, "%u,%s,%u,%u", Links.Parent.ObjectId, Bone(Links.Parent), static_cast<u32>(Links.IsRollLocked), static_cast<u32>(Links.IsPitchLocked));
		Ini.w_string(Section, Line, Text);
	}
	if (Links.LookAt.ObjectId)
	{
		xr_sprintf(Line, "%slook_at", Prefix);
		xr_sprintf(Text, "%u,%s,%u", Links.LookAt.ObjectId, Bone(Links.LookAt), static_cast<u32>(Links.IsYawOnly));
		Ini.w_string(Section, Line, Text);
	}
	if (Links.Weight != 1.0f)
	{
		xr_sprintf(Line, "%sweight", Prefix);
		Ini.w_float(Section, Line, Links.Weight);
	}
	if (!Links.Switches.empty())
	{
		xr_sprintf(Line, "%sswitches", Prefix);
		Ini.w_u32(Section, Line, static_cast<u32>(Links.Switches.size()));
	}
	for (u32 Index = 0; Index < Links.Switches.size(); ++Index)
	{
		xr_sprintf(Line, "%sswitch_%02u", Prefix, Index);
		Ini.w_float(Section, Line, Links.Switches[Index].Time);
		if (Links.Switches[Index].Bone.size())
		{
			xr_sprintf(Line, "%sswitch_%02u_bone", Prefix, Index);
			Ini.w_string(Section, Line, Links.Switches[Index].Bone.c_str());
		}
	}
}

void SceneFolder(string_path& Out, const char* Alias, const shared_str& LevelName)
{
	string_path Relative;
	xr_sprintf(Relative, "cinema_scenes\\%s\\", LevelName.size() ? LevelName.c_str() : "");
	FS.update_path(Out, Alias, Relative);
}

void UnsavedPath(string_path& Out, const shared_str& LevelName)
{
	string_path Relative;
	xr_sprintf(Relative, UnsavedFile, LevelName.size() ? LevelName.c_str() : "");
	FS.update_path(Out, LegacyAlias, Relative);
}

bool CleanName(const xr_string& Utf8, xr_string& File, xr_string& Error)
{
	xr_string Clean;
	for (const char Symbol : Utf8)
	{
		if (static_cast<u8>(Symbol) >= 32 && !strchr("\\/:*?\"<>|", Symbol))
		{
			Clean.push_back(Symbol);
		}
	}
	while (!Clean.empty() && (Clean.back() == ' ' || Clean.back() == '.'))
	{
		Clean.pop_back();
	}
	Clean.erase(0, Clean.find_first_not_of(' '));
	File = IsUTF8(Clean.c_str()) ? Platform::UTF8_to_CP1251(Clean) : xr_string("?");
	xr_strlwr(File.data());
	Error = File.empty() ? "Enter a scene name." : File.find('?') != xr_string::npos ? "The name has characters that cannot be used in a file name." : File.size() > 64 ? "The name is longer than 64 bytes." : "";
	return Error.empty();
}

void ListScenes(const char* Alias, const shared_str& LevelName, xr_vector<xr_string>& Names)
{
	string_path Folder;
	SceneFolder(Folder, Alias, LevelName);
	FS.rescan_path(Folder, false, false);
	FS_FileSet Files;
	FS.file_list(Files, Folder, FS_ListFiles | FS_RootOnly, "*.ltx");
	Names.clear();
	for (const FS_File& Entry : Files)
	{
		if (Entry.name.size() > 4)
		{
			Names.push_back(Platform::ANSI_TO_UTF8(Entry.name.substr(0, Entry.name.size() - 4)));
		}
	}
}

template <typename... TValues>
void Write(CInifile& Ini, const char* Section, const char* Line, TValues... Values)
{
	static_assert(sizeof...(TValues) <= 12);
	string256 Text = {};
	s32 Length = 0;
	const auto Append = [&](auto Value)
	{
		if constexpr (std::is_floating_point_v<decltype(Value)>)
		{
			Length += xr_sprintf(Text + Length, sizeof(Text) - Length, "%s%.9g", Length ? "," : "", static_cast<double>(Value));
		}
		else
		{
			Length += xr_sprintf(Text + Length, sizeof(Text) - Length, "%s%u", Length ? "," : "", static_cast<u32>(Value));
		}
	};
	(Append(Values), ...);
	Ini.w_string(Section, Line, Text);
}

void WriteName(CInifile& Ini, const char* Section, const char* Line, const xr_string& Name)
{
	static constexpr char Digits[] = "0123456789abcdef";
	string512 Hex = {};
	const size_t Size = std::min<size_t>(Name.size(), MaxNameBytes);
	for (size_t Index = 0; Index < Size; ++Index)
	{
		Hex[Index * 2] = Digits[static_cast<u8>(Name[Index]) >> 4];
		Hex[Index * 2 + 1] = Digits[static_cast<u8>(Name[Index]) & 15];
	}
	if (Size)
	{
		Ini.w_string(Section, Line, Hex);
	}
}

void ReadLanes(const CInifile& Ini, const char* Section, const char* Prefix, u16 Where, u32 Layers, xr_vector<Lane>& Lanes, Warnings& Notes, u8 LayerBase = 0)
{
	string64 Line;
	xr_sprintf(Line, "%slanes", Prefix);
	const u32 Count = ReadU32(Ini, Section, Line, std::max(MaxLanes, 2 + 3 * Layers), Notes);
	for (u32 Index = 0; Index < Count; ++Index)
	{
		xr_sprintf(Line, "%slane_%02u_param", Prefix, Index);
		const char* Id = Ini.read_if_exists<str_c>(Section, Line, nullptr);
		const ParamInfo* Found = Id ? std::find_if(std::begin(Params), std::end(Params), [&](const ParamInfo& Meta) { return (Meta.Where & Where) && !xr_strcmp(Meta.Id, Id); }) : std::end(Params);
		xr_sprintf(Line, "%slane_%02u_layer", Prefix, Index);
		const u32 Layer = ReadU32(Ini, Section, Line, ~0u, Notes);
		const Param Which = static_cast<Param>(Found - std::begin(Params));
		const bool IsShake = Which >= Param::ShakeInfluence && Which < Param::Count;
		const bool IsPose = Which >= Param::PoseWeight && Which <= Param::BoneScale, IsIk = Which >= Param::IkTarget && Which <= Param::IkWeight;
		const bool IsMisplaced = IsShake ? Layer >= Layers : Which == Param::LayerWeight ? Layer > Layers : IsPose ? Layer > 255 : IsIk ? Layer >= MaxIkLimbs : Which == Param::ConstraintWeight ? Layer >= MaxConstraints : Layer != 0;
		if (Found == std::end(Params) || IsMisplaced || Curves::FindLane(Lanes, Which, static_cast<u8>(Layer + LayerBase)))
		{
			Notes.Add("[%s] %slane_%02u has an unknown, misplaced or repeated parameter, lane skipped", Section, Prefix, Index);
			continue;
		}
		const ParamInfo& Meta = *Found;
		Lane Item;
		Item.Id = Which;
		Item.Layer = static_cast<u8>(Layer + LayerBase);
		bool IsEmpty = true;
		for (u32 Component = 0; Component < Meta.Components; ++Component)
		{
			Curve& Target = Item.Curves[Component];
			xr_sprintf(Line, "%slane_%02u_c%u", Prefix, Index, Component);
			const u32 KeyCount = ReadU32(Ini, Section, Line, MaxLaneKeys, Notes);
			for (u32 KeyIndex = 0; KeyIndex < KeyCount; ++KeyIndex)
			{
				float Values[8] = {};
				xr_sprintf(Line, "%slane_%02u_c%u_%04u", Prefix, Index, Component, KeyIndex);
				const char* Text = Ini.read_if_exists<str_c>(Section, Line, nullptr);
				const u32 Fields = Text && std::count(Text, Text + xr_strlen(Text), ',') == 7 ? 8 : 7;
				if (!ReadFloats(Ini, Section, Line, Values, Fields, Notes))
				{
					continue;
				}
				if (!(Values[0] >= 0.0f && Values[0] <= MaxTime) || (!Target.Keys.empty() && Values[0] <= Target.Keys.back().Time))
				{
					Notes.Add("[%s] %s has a time out of range or order, key dropped", Section, Line);
					continue;
				}
				const float Value = Sanitize(Values[1], Meta.Min, Meta.Max, std::clamp(0.0f, Meta.Min, Meta.Max));
				const bool IsClamped = ClampAll(Values + 2, KeyRanges, 5);
				if (IsClamped || !(Value == Values[1]))
				{
					Notes.Add("[%s] %s is out of range, clamped", Section, Line);
				}
				Target.Keys.push_back({ Values[0], Value, static_cast<Interp>(static_cast<u8>(Values[2])), Values[3], Values[4], Values[5], Values[6], Values[7] != 0.0f });
			}
			float Gen[6];
			xr_sprintf(Line, "%slane_%02u_c%u_gen", Prefix, Index, Component);
			if (ReadFloats(Ini, Section, Line, Gen, 6, Notes))
			{
				bool IsGood = Gen[0] >= 0.0f && Gen[0] <= 10.0f && Gen[0] == std::floor(Gen[0]) && Gen[4] <= Gen[5] && Gen[4] <= Meta.Max && Gen[5] >= Meta.Min;
				for (const float Number : Gen)
				{
					IsGood = IsGood && std::fabs(Number) <= 100000.0f;
				}
				if (IsGood)
				{
					Target.Gen = { static_cast<Wave>(static_cast<u8>(Gen[0])), Gen[1], Gen[2], Gen[3], Gen[4], Gen[5] };
				}
				else
				{
					Notes.Add("[%s] %s is not a valid generator, dropped", Section, Line);
				}
			}
			IsEmpty = IsEmpty && Target.Keys.empty() && Target.Gen.Type == Wave::None;
		}
		float Ends[2];
		xr_sprintf(Line, "%slane_%02u_infinity", Prefix, Index);
		if (ReadFloats(Ini, Section, Line, Ends, 2, Notes) && Ends[0] >= 0.0f && Ends[0] <= 4.0f && Ends[1] >= 0.0f && Ends[1] <= 4.0f && Ends[0] == std::floor(Ends[0]) && Ends[1] == std::floor(Ends[1]))
		{
			for (Curve& Target : Item.Curves)
			{
				Target.Pre = static_cast<u8>(Ends[0]);
				Target.Post = static_cast<u8>(Ends[1]);
			}
		}
		if (!IsEmpty)
		{
			Lanes.push_back(std::move(Item));
		}
	}
}

void WriteLanes(CInifile& Ini, const char* Section, const char* Prefix, const xr_vector<Lane>& Lanes, u8 LayerBase = 0)
{
	string64 Line;
	u32 Count = 0;
	for (const Lane& Item : Lanes)
	{
		const ParamInfo& Meta = Info(Item.Id);
		if (std::all_of(Item.Curves, Item.Curves + Meta.Components, [](const Curve& Source) { return Source.Keys.empty() && Source.Gen.Type == Wave::None; }))
		{
			continue;
		}
		xr_sprintf(Line, "%slane_%02u_param", Prefix, Count);
		Ini.w_string(Section, Line, Meta.Id);
		if (Item.Layer != LayerBase)
		{
			xr_sprintf(Line, "%slane_%02u_layer", Prefix, Count);
			Write(Ini, Section, Line, static_cast<u8>(Item.Layer - LayerBase));
		}
		if (Item.Curves[0].Pre || Item.Curves[0].Post)
		{
			xr_sprintf(Line, "%slane_%02u_infinity", Prefix, Count);
			Write(Ini, Section, Line, Item.Curves[0].Pre, Item.Curves[0].Post);
		}
		for (u32 Component = 0; Component < Meta.Components; ++Component)
		{
			const Curve& Source = Item.Curves[Component];
			if (!Source.Keys.empty())
			{
				xr_sprintf(Line, "%slane_%02u_c%u", Prefix, Count, Component);
				Write(Ini, Section, Line, static_cast<u32>(Source.Keys.size()));
			}
			for (u32 KeyIndex = 0; KeyIndex < Source.Keys.size(); ++KeyIndex)
			{
				const Key& Point = Source.Keys[KeyIndex];
				xr_sprintf(Line, "%slane_%02u_c%u_%04u", Prefix, Count, Component, KeyIndex);
				if (Point.IsBroken)
				{
					Write(Ini, Section, Line, Point.Time, Point.Value, Point.Mode, Point.InTime, Point.InValue, Point.OutTime, Point.OutValue, 1u);
				}
				else
				{
					Write(Ini, Section, Line, Point.Time, Point.Value, Point.Mode, Point.InTime, Point.InValue, Point.OutTime, Point.OutValue);
				}
			}
			if (Source.Gen.Type != Wave::None)
			{
				const Generator& Gen = Source.Gen;
				xr_sprintf(Line, "%slane_%02u_c%u_gen", Prefix, Count, Component);
				Write(Ini, Section, Line, Gen.Type, Gen.Amplitude, Gen.Frequency, Gen.Phase, Gen.Min, Gen.Max);
			}
		}
		++Count;
	}
	if (Count)
	{
		xr_sprintf(Line, "%slanes", Prefix);
		Write(Ini, Section, Line, Count);
	}
}

void ReadTrack(const CInifile& Ini, const char* Section, const char* Prefix, ClipTrack& Out, Warnings& Notes)
{
	string128 Key;
	xr_sprintf(Key, "%sbones", Prefix);
	const u32 Count = ReadU32(Ini, Section, Key, MaxClipBones, Notes);
	for (u32 Bone = 0; Bone < Count; ++Bone)
	{
		PoseBone Entry;
		float Rest[6] = {}, Size[3] = { 1.0f, 1.0f, 1.0f };
		xr_sprintf(Key, "%sbone_%02u", Prefix, Bone);
		Entry.Name = ReadSafe(Ini, Section, Key, Notes);
		xr_sprintf(Key, "%sbone_%02u_rest", Prefix, Bone);
		if (ReadFloats(Ini, Section, Key, Rest, 6, Notes))
		{
			Entry.Rotation.set(Sanitize(Rest[0], -3600.0f, 3600.0f, 0.0f), Sanitize(Rest[1], -3600.0f, 3600.0f, 0.0f), Sanitize(Rest[2], -3600.0f, 3600.0f, 0.0f));
			Entry.Offset.set(Sanitize(Rest[3], -10.0f, 10.0f, 0.0f), Sanitize(Rest[4], -10.0f, 10.0f, 0.0f), Sanitize(Rest[5], -10.0f, 10.0f, 0.0f));
		}
		xr_sprintf(Key, "%sbone_%02u_scale", Prefix, Bone);
		ReadFloats(Ini, Section, Key, Size, 3, Notes);
		Entry.Scale.set(Sanitize(Size[0], 0.01f, 100.0f, 1.0f), Sanitize(Size[1], 0.01f, 100.0f, 1.0f), Sanitize(Size[2], 0.01f, 100.0f, 1.0f));
		Out.Bones.push_back(Entry);
	}
	ReadLanes(Ini, Section, Prefix, WherePoseBone, 0, Out.Lanes, Notes);
	std::erase_if(Out.Lanes, [&Out](const Lane& Track) { return Track.Layer >= Out.Bones.size(); });
}

void WriteTrack(CInifile& Ini, const char* Section, const char* Prefix, const ClipTrack& Track)
{
	string128 Key;
	xr_sprintf(Key, "%sbones", Prefix);
	Write(Ini, Section, Key, static_cast<u32>(Track.Bones.size()));
	for (u32 Bone = 0; Bone < Track.Bones.size(); ++Bone)
	{
		const PoseBone& Entry = Track.Bones[Bone];
		xr_sprintf(Key, "%sbone_%02u", Prefix, Bone);
		Ini.w_string(Section, Key, Entry.Name.c_str());
		xr_sprintf(Key, "%sbone_%02u_rest", Prefix, Bone);
		Write(Ini, Section, Key, Entry.Rotation.x, Entry.Rotation.y, Entry.Rotation.z, Entry.Offset.x, Entry.Offset.y, Entry.Offset.z);
		xr_sprintf(Key, "%sbone_%02u_scale", Prefix, Bone);
		Write(Ini, Section, Key, Entry.Scale.x, Entry.Scale.y, Entry.Scale.z);
	}
	WriteLanes(Ini, Section, Prefix, Track.Lanes);
}

void ReadBlocks(const CInifile& Ini, const char* Section, const char* Prefix, xr_vector<ActorBlock>& Blocks, u32 Fields, Warnings& Notes)
{
	const bool IsPacked = Fields == 6;
	xr_vector<float> Blends;
	string64 Line;
	xr_sprintf(Line, "%sblocks", Prefix);
	const u32 BlockCount = ReadU32(Ini, Section, Line, MaxBlocks, Notes);
	for (u32 Index = 0; Index < BlockCount; ++Index)
	{
		ActorBlock Block;
		xr_sprintf(Line, "%sblock_%03u_source", Prefix, Index);
		Block.Source = ReadSafe(Ini, Section, Line, Notes);
		xr_sprintf(Line, "%sblock_%03u_name", Prefix, Index);
		Block.Name = ReadSafe(Ini, Section, Line, Notes);
		if (!Block.Source.size() || !Block.Name.size())
		{
			Notes.Add("[%s] %sblock_%03u has no source or name, block skipped", Section, Prefix, Index);
			continue;
		}
		float Time[9] = { 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 3.0f, 1.0f, 0.0f, 0.0f };
		xr_sprintf(Line, "%sblock_%03u_time", Prefix, Index);
		ReadRanged(Ini, Section, Line, Time, IsPacked ? PackedRanges : BlockRanges, Fields, Notes);
		const float End = Time[1] > 0.0f ? std::max(Time[1], Time[0]) : 0.0f;
		const float Speed = std::copysign(std::max(std::fabs(Time[2]), 0.05f), Time[2]);
		if (End != Time[1] || Speed != Time[2])
		{
			Notes.Add("[%s] %s has out before in or a speed below 0.05, fixed", Section, Line);
		}
		Block.In = Time[0];
		Block.Out = End;
		Block.Speed = Speed;
		Block.Hold = Time[3];
		Block.Start = IsPacked ? 0.0f : Time[4];
		Block.BlendEase = static_cast<Interp>(static_cast<u8>(Time[5]));
		Block.Loops = Time[6];
		Block.FadeIn = Time[7];
		Block.FadeOut = Time[8];
		Blends.push_back(IsPacked ? Time[4] : 0.0f);
		Blocks.push_back(Block);
	}
	if (IsPacked)
	{
		Actors::Unpack(Blocks, Blends);
	}
}

void WriteBlocks(CInifile& Ini, const char* Section, const char* Prefix, const xr_vector<ActorBlock>& Blocks)
{
	string64 Line;
	xr_sprintf(Line, "%sblocks", Prefix);
	Write(Ini, Section, Line, static_cast<u32>(Blocks.size()));
	for (u32 Index = 0; Index < Blocks.size(); ++Index)
	{
		const ActorBlock& Block = Blocks[Index];
		xr_sprintf(Line, "%sblock_%03u_source", Prefix, Index);
		Ini.w_string(Section, Line, Block.Source.c_str());
		xr_sprintf(Line, "%sblock_%03u_name", Prefix, Index);
		Ini.w_string(Section, Line, Block.Name.c_str());
		xr_sprintf(Line, "%sblock_%03u_time", Prefix, Index);
		Write(Ini, Section, Line, Block.In, Block.Out, Block.Speed, Block.Hold, Block.Start, Block.BlendEase, Block.Loops, Block.FadeIn, Block.FadeOut);
	}
}

void ReadActor(const CInifile& Ini, const char* Section, ActorData& Setup, bool IsPacked, bool IsRelative, bool IsRows, Warnings& Notes)
{
	string64 Line;
	Setup.IsRelative = IsRelative;
	Setup.Visual = ReadSafe(Ini, Section, "visual", Notes);
	Setup.Item = ReadSafe(Ini, Section, "item", Notes);
	Setup.Hud = ReadSafe(Ini, Section, "hud", Notes);
	Setup.IsHudLinked = ReadBool(Ini, Section, "hud_link", false, Notes);
	ReadVector(Ini, Section, "scale", ScaleRange, Setup.Scale, Notes);
	ReadRanged(Ini, Section, "time_scale", &Setup.TimeScale, &RateRange, 1, Notes);
	const char* EndText = Ini.read_if_exists<str_c>(Section, "end", nullptr);
	u32 Mode = 0;
	while (Mode < 3 && (!EndText || xr_strcmp(EndText, EndNames[Mode])))
	{
		++Mode;
	}
	if (IsRelative && EndText && Mode == 3)
	{
		Notes.Add("[%s] end = %s is not hold, loop or pingpong, hold is used", Section, EndText);
	}
	Setup.End = IsRelative && Mode < 3 ? static_cast<EndMode>(Mode) : EndMode::Hold;
	const u32 PackCount = ReadU32(Ini, Section, "packs", MaxPacks, Notes);
	for (u32 Index = 0; Index < PackCount; ++Index)
	{
		xr_sprintf(Line, "pack_%02u", Index);
		const shared_str Pack = ReadSafe(Ini, Section, Line, Notes);
		if (!IsOmfPath(Pack.c_str()) || std::find(Setup.Packs.begin(), Setup.Packs.end(), Pack) != Setup.Packs.end())
		{
			Notes.Add("[%s] %s is not an .omf path or repeats one, skipped", Section, Line);
			continue;
		}
		Setup.Packs.push_back(Pack);
	}
	const u32 Fields = IsPacked ? 6 : IsRows ? 9 : 7;
	if (!IsRows)
	{
		ReadBlocks(Ini, Section, "", Setup.Base.Blocks, Fields, Notes);
	}
	const u32 LayerCount = IsRows ? std::max(ReadU32(Ini, Section, "rows", MaxActorLayers + 1, Notes), 1u) : ReadU32(Ini, Section, "layers", MaxActorLayers, Notes) + 1;
	for (u32 Index = IsRows ? 0 : 1; Index < LayerCount; ++Index)
	{
		string32 Prefix;
		xr_sprintf(Prefix, IsRows ? "row_%02u_" : "layer_%02u_", IsRows ? Index : Index - 1);
		ActorLayer& Layer = Index ? Setup.Layers.emplace_back() : Setup.Base;
		xr_sprintf(Line, "%smode", Prefix);
		const bool IsAdd = IsRows && ReadU32(Ini, Section, Line, 1, Notes) == 1;
		Layer.Mode = IsAdd && AddRows(Setup) < MaxAddRows ? RowMode::Add : RowMode::Override;
		if (IsAdd && Layer.Mode != RowMode::Add)
		{
			Notes.Add("[%s] %smode: at most %u rows add, the row overrides", Section, Prefix, MaxAddRows);
		}
		xr_sprintf(Line, "%sparts", Prefix);
		const u32 PartCount = ReadU32(Ini, Section, Line, MaxPartNames, Notes);
		for (u32 Part = 0; Part < PartCount; ++Part)
		{
			xr_sprintf(Line, "%spart_%u", Prefix, Part);
			const shared_str Name = ReadSafe(Ini, Section, Line, Notes);
			if (Name.size() && std::find(Layer.Parts.begin(), Layer.Parts.end(), Name) == Layer.Parts.end())
			{
				Layer.Parts.push_back(Name);
			}
		}
		xr_sprintf(Line, "%smuted", Prefix);
		Layer.IsMuted = ReadBool(Ini, Section, Line, false, Notes);
		xr_sprintf(Line, "%sweight", Prefix);
		ReadRanged(Ini, Section, Line, &Layer.Weight, &WeightRange, 1, Notes);
		ReadBlocks(Ini, Section, Prefix, Layer.Blocks, Fields, Notes);
	}
	for (u8 Layer = 0; !IsRelative && Layer <= Setup.Layers.size(); ++Layer)
	{
		xr_vector<ActorBlock>& Blocks = BlocksOf(Setup, Layer);
		const bool IsBad = std::any_of(Blocks.begin(), Blocks.end(), [](const ActorBlock& Block) { return Block.Start < 0.0f; }) || !std::is_sorted(Blocks.begin(), Blocks.end(), [](const ActorBlock& A, const ActorBlock& B) { return A.Start < B.Start; });
		for (ActorBlock& Block : Blocks)
		{
			Block.Start = std::max(Block.Start, 0.0f);
		}
		std::stable_sort(Blocks.begin(), Blocks.end(), [](const ActorBlock& A, const ActorBlock& B) { return A.Start < B.Start; });
		if (IsBad)
		{
			Notes.Add("[%s] animation clips start before the actor or out of order, fixed", Section);
		}
	}
	const u32 PoseCount = ReadU32(Ini, Section, "pose_rows", MaxPoseRows, Notes);
	for (u32 Row = 0; Row < PoseCount; ++Row)
	{
		PoseRow& Pose = Setup.Poses.emplace_back();
		float Values[3] = { 1.0f, 0.0f, 1.0f };
		xr_sprintf(Line, "pose_%02u", Row);
		ReadFloats(Ini, Section, Line, Values, 3, Notes);
		Pose.Mode = Values[0] == 0.0f ? RowMode::Override : RowMode::Add;
		Pose.IsMuted = Values[1] != 0.0f;
		Pose.Weight = Sanitize(Values[2], 0.0f, 1.0f, 1.0f);
		xr_sprintf(Line, "pose_%02u_bones", Row);
		const u32 BoneCount = ReadU32(Ini, Section, Line, MaxPoseBones, Notes);
		for (u32 Bone = 0; Bone < BoneCount; ++Bone)
		{
			xr_sprintf(Line, "pose_%02u_bone_%02u", Row, Bone);
			PoseBone Item;
			Item.Name = ReadSafe(Ini, Section, Line, Notes);
			float Rest[6] = {};
			xr_sprintf(Line, "pose_%02u_bone_%02u_rest", Row, Bone);
			if (ReadFloats(Ini, Section, Line, Rest, 6, Notes))
			{
				Item.Rotation.set(Sanitize(Rest[0], -3600.0f, 3600.0f, 0.0f), Sanitize(Rest[1], -3600.0f, 3600.0f, 0.0f), Sanitize(Rest[2], -3600.0f, 3600.0f, 0.0f));
				Item.Offset.set(Sanitize(Rest[3], -10.0f, 10.0f, 0.0f), Sanitize(Rest[4], -10.0f, 10.0f, 0.0f), Sanitize(Rest[5], -10.0f, 10.0f, 0.0f));
			}
			float Size[3] = { 1.0f, 1.0f, 1.0f };
			xr_sprintf(Line, "pose_%02u_bone_%02u_scale", Row, Bone);
			ReadFloats(Ini, Section, Line, Size, 3, Notes);
			Item.Scale.set(Sanitize(Size[0], 0.01f, 100.0f, 1.0f), Sanitize(Size[1], 0.01f, 100.0f, 1.0f), Sanitize(Size[2], 0.01f, 100.0f, 1.0f));
			Pose.Bones.push_back(Item);
		}
	}
	const u32 ClipCount = Ini.line_exist(Section, "clips") ? ReadU32(Ini, Section, "clips", MaxClips, Notes) : 0;
	for (u32 Clip = 0; Clip < ClipCount; ++Clip)
	{
		AnimClip& Item = Setup.Clips.emplace_back();
		string64 Prefix;
		float Values[2] = { 1.0f, 0.0f };
		xr_sprintf(Line, "clip_%02u", Clip);
		Item.Name = ReadSafe(Ini, Section, Line, Notes);
		xr_sprintf(Line, "clip_%02u_info", Clip);
		ReadFloats(Ini, Section, Line, Values, 2, Notes);
		Item.Length = Sanitize(Values[0], MinLength, MaxTime, 1.0f);
		Item.Mode = Values[1] == 0.0f ? RowMode::Override : RowMode::Add;
		xr_sprintf(Line, "clip_%02u_bones", Clip);
		const u32 BoneCount = ReadU32(Ini, Section, Line, MaxClipBones, Notes);
		for (u32 Bone = 0; Bone < BoneCount; ++Bone)
		{
			PoseBone Entry;
			float Rest[6] = {}, Size[3] = { 1.0f, 1.0f, 1.0f };
			xr_sprintf(Line, "clip_%02u_bone_%02u", Clip, Bone);
			Entry.Name = ReadSafe(Ini, Section, Line, Notes);
			xr_sprintf(Line, "clip_%02u_bone_%02u_rest", Clip, Bone);
			if (ReadFloats(Ini, Section, Line, Rest, 6, Notes))
			{
				Entry.Rotation.set(Sanitize(Rest[0], -3600.0f, 3600.0f, 0.0f), Sanitize(Rest[1], -3600.0f, 3600.0f, 0.0f), Sanitize(Rest[2], -3600.0f, 3600.0f, 0.0f));
				Entry.Offset.set(Sanitize(Rest[3], -10.0f, 10.0f, 0.0f), Sanitize(Rest[4], -10.0f, 10.0f, 0.0f), Sanitize(Rest[5], -10.0f, 10.0f, 0.0f));
			}
			xr_sprintf(Line, "clip_%02u_bone_%02u_scale", Clip, Bone);
			ReadFloats(Ini, Section, Line, Size, 3, Notes);
			Entry.Scale.set(Sanitize(Size[0], 0.01f, 100.0f, 1.0f), Sanitize(Size[1], 0.01f, 100.0f, 1.0f), Sanitize(Size[2], 0.01f, 100.0f, 1.0f));
			Item.Bones.push_back(Entry);
		}
		xr_sprintf(Prefix, "clip_%02u_", Clip);
		ReadLanes(Ini, Section, Prefix, WherePoseBone, 0, Item.Lanes, Notes);
		std::erase_if(Item.Lanes, [&Item](const Lane& Track) { return Track.Layer >= Item.Bones.size(); });
		xr_sprintf(Line, "clip_%02u_layers", Clip);
		const u32 LayerCount = Ini.line_exist(Section, Line) ? ReadU32(Ini, Section, Line, MaxClipLayers, Notes) : 0;
		for (u32 Layer = 0; Layer < LayerCount; ++Layer)
		{
			ClipLayer& Over = Item.Layers.emplace_back();
			float Info[3] = { 1.0f, 1.0f, 0.0f };
			xr_sprintf(Prefix, "clip_%02u_layer_%02u_", Clip, Layer);
			xr_sprintf(Line, "%sname", Prefix);
			Over.Name = ReadSafe(Ini, Section, Line, Notes);
			xr_sprintf(Line, "%sinfo", Prefix);
			ReadFloats(Ini, Section, Line, Info, 3, Notes);
			Over.Weight = Sanitize(Info[0], 0.0f, 1.0f, 1.0f);
			Over.Mode = Info[1] == 0.0f ? RowMode::Override : RowMode::Add;
			Over.IsMuted = Info[2] != 0.0f;
			ReadTrack(Ini, Section, Prefix, Over, Notes);
		}
		for (u32 Limb = 0; Limb < MaxIkLimbs; ++Limb)
		{
			IkLimb& Arm = Item.Ik[Limb];
			float State[3] = { 0.0f, 0.0f, 1.0f }, Goal[9] = {};
			string256 Part;
			xr_sprintf(Line, "clip_%02u_ik_%02u_bones", Clip, Limb);
			const char* Names = Ini.read_if_exists<str_c>(Section, Line, nullptr);
			for (u32 Link = 0; Names && _GetItemCount(Names) == 3 && Link < 3; ++Link)
			{
				_GetItem(Names, static_cast<int>(Link), Part);
				Arm.Bones[Link] = IniSafe(Part) ? Part : "";
			}
			if (!Arm.Bones[0].size() || !Arm.Bones[1].size() || !Arm.Bones[2].size())
			{
				Arm = IkLimb {};
				continue;
			}
			xr_sprintf(Line, "clip_%02u_ik_%02u", Clip, Limb);
			ReadFloats(Ini, Section, Line, State, 3, Notes);
			xr_sprintf(Line, "clip_%02u_ik_%02u_goal", Clip, Limb);
			ReadFloats(Ini, Section, Line, Goal, 9, Notes);
			Arm.IsOn = State[0] != 0.0f;
			Arm.Space = State[1] != 0.0f ? IkSpace::World : IkSpace::Actor;
			Arm.Weight = Sanitize(State[2], 0.0f, 1.0f, 1.0f);
			for (u32 Axis = 0; Axis < 3; ++Axis)
			{
				Arm.Target[Axis] = Sanitize(Goal[Axis], -100000.0f, 100000.0f, 0.0f);
				Arm.Turn[Axis] = Sanitize(Goal[3 + Axis], -3600.0f, 3600.0f, 0.0f);
				Arm.Pole[Axis] = Sanitize(Goal[6 + Axis], -100000.0f, 100000.0f, 0.0f);
			}
		}
		xr_sprintf(Prefix, "clip_%02u_ik_", Clip);
		xr_sprintf(Line, "%slanes", Prefix);
		if (Ini.line_exist(Section, Line))
		{
			ReadLanes(Ini, Section, Prefix, WherePoseBone, 0, Item.IkLanes, Notes);
			std::erase_if(Item.IkLanes, [](const Lane& Track) { return Track.Id < Param::IkTarget || Track.Id > Param::IkWeight; });
		}
		xr_sprintf(Line, "clip_%02u_linked", Clip);
		AnimClip Stored;
		if (ReadBool(Ini, Section, Line, false, Notes) && Omf::IsClipName(Item.Name.c_str()))
		{
			Item.IsLinked = true;
			if (Omf::ReadClip(Item.Name.c_str(), Stored))
			{
				Item = Stored;
				Item.IsLinked = true;
			}
			else
			{
				Notes.Add("[%s] library animation %s is missing or has no keys: the copy saved in the scene is used", Section, Item.Name.c_str());
			}
		}
	}
	const u32 LookCount = ReadU32(Ini, Section, "looks", MaxLooks, Notes);
	for (u32 Look = 0; Look < LookCount; ++Look)
	{
		ActorLook& Item = Setup.Looks.emplace_back();
		xr_sprintf(Line, "look_%02u_surfaces", Look);
		const u32 SurfaceCount = ReadU32(Ini, Section, Line, 2 * MaxLookSurfaces, Notes);
		for (u32 Index = 0; Index < SurfaceCount; ++Index)
		{
			SurfaceShader Entry;
			xr_sprintf(Line, "look_%02u_%02u", Look, Index);
			Entry.Surface = static_cast<u8>(ReadU32(Ini, Section, Line, ItemSurfaceBase + MaxLookSurfaces - 1, Notes));
			xr_sprintf(Line, "look_%02u_%02u_shader", Look, Index);
			Entry.Shader = ReadSafe(Ini, Section, Line, Notes);
			xr_sprintf(Line, "look_%02u_%02u_texture", Look, Index);
			Entry.Texture = ReadSafe(Ini, Section, Line, Notes);
			if (FindSurface(Item, Entry.Surface))
			{
				Notes.Add("[%s] look %u names surface %u twice, the repeat is ignored", Section, Look + 1, Entry.Surface + 1u);
			}
			else if (Entry.Shader.size() || Entry.Texture.size())
			{
				Item.Surfaces.push_back(Entry);
			}
		}
	}
	for (u32 Limb = 0; Limb < MaxIkLimbs; ++Limb)
	{
		IkLimb& Item = Setup.Ik[Limb];
		xr_sprintf(Line, "ik_%02u_bones", Limb);
		const char* Names = Ini.read_if_exists<str_c>(Section, Line, nullptr);
		string256 Part;
		for (u32 Link = 0; Names && _GetItemCount(Names) == 3 && Link < 3; ++Link)
		{
			_GetItem(Names, static_cast<int>(Link), Part);
			Item.Bones[Link] = IniSafe(Part) ? Part : "";
		}
		if (Names && (!Item.Bones[0].size() || !Item.Bones[1].size() || !Item.Bones[2].size()))
		{
			Notes.Add("[%s] %s needs three bone names, the limb is ignored", Section, Line);
			Item = IkLimb {};
		}
		if (!Item.Bones[0].size())
		{
			continue;
		}
		float Head[3] = { 0.0f, 0.0f, 1.0f }, Values[9] = {};
		xr_sprintf(Line, "ik_%02u", Limb);
		ReadFloats(Ini, Section, Line, Head, 3, Notes);
		Item.IsOn = Head[0] != 0.0f;
		Item.Space = Head[1] != 0.0f ? IkSpace::World : IkSpace::Actor;
		Item.Weight = Sanitize(Head[2], 0.0f, 1.0f, 1.0f);
		xr_sprintf(Line, "ik_%02u_goal", Limb);
		ReadFloats(Ini, Section, Line, Values, 9, Notes);
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			Item.Target[Axis] = Sanitize(Values[Axis], -100000.0f, 100000.0f, 0.0f);
			Item.Turn[Axis] = Sanitize(Values[3 + Axis], -3600.0f, 3600.0f, 0.0f);
			Item.Pole[Axis] = Sanitize(Values[6 + Axis], -100000.0f, 100000.0f, 0.0f);
		}
	}
	const u32 RuleCount = ReadU32(Ini, Section, "constraints", MaxConstraints, Notes);
	for (u32 Slot = 0; Slot < RuleCount; ++Slot)
	{
		BoneConstraint Rule;
		bool Flags[1] = {};
		float Values[9] = {};
		xr_sprintf(Line, "constraint_%02u_bone", Slot);
		Rule.Bone = ReadSafe(Ini, Section, Line, Notes);
		xr_sprintf(Line, "constraint_%02u_target", Slot);
		ReadLink(Ini, Section, Line, Rule.Target, Flags, 0, Notes);
		xr_sprintf(Line, "constraint_%02u", Slot);
		ReadFloats(Ini, Section, Line, Values, 9, Notes);
		Rule.Kind = static_cast<ConstraintKind>(static_cast<u8>(Sanitize(std::floor(Values[0]), 0.0f, 3.0f, 2.0f)));
		Rule.IsMuted = Values[1] != 0.0f;
		Rule.Weight = Sanitize(Values[2], 0.0f, 1.0f, 1.0f);
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			Rule.Shift[Axis] = Sanitize(Values[3 + Axis], -100000.0f, 100000.0f, 0.0f);
			Rule.Turn[Axis] = Sanitize(Values[6 + Axis], -3600.0f, 3600.0f, 0.0f);
		}
		if (!Rule.Bone.size())
		{
			Notes.Add("[%s] constraint %u has no bone, the slot stays empty", Section, Slot + 1);
		}
		Setup.Constraints.push_back(Rule);
	}
}

Shake MakeShake(const float* Values)
{
	return { static_cast<u8>(Values[0]), Values[1] != 0.0f, Values[2], Values[3], Values[4], Values[5], Values[6] != 0.0f };
}

void MigrateTrack(Object& Rig, const xr_vector<float>& Blends, float Total, const char* Section, Warnings& Notes)
{
	CameraData& Shot = Rig.Camera;
	const float Base = Rig.Span.Start;
	u32 Changed = 0;
	for (CameraBlock& Block : Shot.Blocks)
	{
		Block.Start = std::min(Block.Start + Base, MaxTime);
	}
	for (ShakeClip& Clip : Shot.Shakes)
	{
		Clip.Start = std::min(Clip.Start + Base, MaxTime);
	}
	for (u32 Index = 1; Index < Shot.Blocks.size(); ++Index)
	{
		CameraBlock& Previous = Shot.Blocks[Index - 1];
		const CameraBlock& Block = Shot.Blocks[Index];
		const float Blend = std::min(Blends[Index], Block.Length - MinLength);
		if (!(Blend > 0.0f) || Previous.Keys.empty() || Block.Keys.empty())
		{
			continue;
		}
		const Lane* Fov = Curves::FindLane(Previous.Lanes, Param::Fov, 0);
		const bool HasFov = Fov && (Fov->Curves[0].Gen.Type != Wave::None || (!Fov->Curves[0].Keys.empty() && Fov->Curves[0].Keys.back().Time > Previous.Length));
		const bool HasShakes = std::any_of(Shot.Shakes.begin(), Shot.Shakes.end(), [&](const ShakeClip& Clip) { return Clip.Start + Cameras::ShakeAnchor >= Previous.Start && Clip.Start + Cameras::ShakeAnchor < Block.Start + Block.Length; });
		const Lane* Scale = Curves::FindLane(Previous.Lanes, Param::TimeScale, 0);
		const bool IsWaved = Scale && Scale->Curves[0].Gen.Type != Wave::None;
		Changed += IsWaved || Previous.Keys.back().Time > static_cast<float>(Curves::LaneArea(Previous.Lanes, nullptr, Param::TimeScale, 0, Previous.Length, Previous.TimeScale)) || HasFov || HasShakes;
		Previous.Length = std::min(Previous.Length + Blend, MaxTime - Previous.Start);
	}
	const size_t Count = Shot.Blocks.size();
	std::erase_if(Shot.Blocks, [](const CameraBlock& Block) { return Block.Keys.empty(); });
	float Extent = 0.0f;
	for (const CameraBlock& Block : Shot.Blocks)
	{
		Extent = std::max(Extent, Block.Start + Block.Length);
	}
	for (const ShakeClip& Clip : Shot.Shakes)
	{
		Extent = std::max(Extent, Clip.Start + Clip.Length);
	}
	if (Base + Total > Extent + EPS_L)
	{
		Rig.Span.IsLengthLocked = true;
		Rig.Span.Length = std::clamp(Base + Total, MinLength, MaxTime);
	}
	if (Count > Shot.Blocks.size())
	{
		Notes.Add("[%s] %u camera blocks without pose keys became gaps", Section, static_cast<u32>(Count - Shot.Blocks.size()));
	}
	if (Changed)
	{
		Notes.Add("[%s] %u camera blends became crossfades that can look slightly different (keys past the outgoing block, a FOV generator or shakes)", Section, Changed);
	}
}

void CheckTrack(CameraData& Shot, const char* Section, Warnings& Notes)
{
	std::stable_sort(Shot.Blocks.begin(), Shot.Blocks.end(), [](const CameraBlock& A, const CameraBlock& B) { return A.Start < B.Start; });
	const size_t Count = Shot.Blocks.size();
	for (u32 Index = 0; Index < Shot.Blocks.size();)
	{
		CameraBlock& Block = Shot.Blocks[Index];
		Block.Start = std::min(Block.Start, MaxTime - MinLength);
		Block.Length = std::clamp(Block.Length, MinLength, MaxTime - Block.Start);
		const CameraBlock* Previous = Index > 0 ? &Shot.Blocks[Index - 1] : nullptr;
		const bool IsCrowded = Previous && !(Previous->Start < Block.Start && Previous->Start + Previous->Length < Block.Start + Block.Length);
		if (IsCrowded || (Index > 1 && Shot.Blocks[Index - 2].Start + Shot.Blocks[Index - 2].Length > Block.Start))
		{
			Shot.Blocks.erase(Shot.Blocks.begin() + Index);
			continue;
		}
		++Index;
	}
	if (Count > Shot.Blocks.size())
	{
		Notes.Add("[%s] %u camera clips lie inside another or overlap two others, dropped", Section, static_cast<u32>(Count - Shot.Blocks.size()));
	}
}

void ReadCamera(const CInifile& Ini, const char* Section, Object& Rig, u32 Version, Warnings& Notes)
{
	CameraData& Shot = Rig.Camera;
	u32 Approx = 0;
	string64 Line;
	string32 Prefix;
	u32 Dropped = 0;
	float Start = 0.0f;
	const bool IsPacked = Version < 11;
	xr_vector<float> Blends;
	const u32 BlockCount = ReadU32(Ini, Section, "blocks", MaxBlocks, Notes);
	for (u32 Index = 0; Index < BlockCount; ++Index)
	{
		CameraBlock& Block = Shot.Blocks.emplace_back();
		float Head[5] = { 5.0f, 3.0f, 0.0f, 1.0f, 0.0f }, Old[5] = { 5.0f, 0.0f, 3.0f, 0.0f, 1.0f };
		xr_sprintf(Line, "block_%03u", Index);
		ReadRanged(Ini, Section, Line, IsPacked ? Old : Head, IsPacked ? ShotRanges : TrackRanges, 5, Notes);
		if (IsPacked)
		{
			Head[0] = Old[0];
			Head[1] = Old[2];
			Head[2] = Old[3];
			Head[3] = Old[4];
			Head[4] = Start;
		}
		if (Head[2] > 0.0f && Head[2] < 1.0f)
		{
			Notes.Add("[%s] %s has a FOV below 1, 1 is used", Section, Line);
			Head[2] = 1.0f;
		}
		Blends.push_back(IsPacked ? Old[1] : 0.0f);
		Block.Length = Head[0];
		Block.BlendEase = static_cast<Interp>(static_cast<u8>(Head[1]));
		Block.Fov = Head[2];
		Block.TimeScale = Head[3];
		Block.Start = Head[4];
		xr_sprintf(Line, "block_%03u_keys", Index);
		const u32 KeyCount = Version < 16 ? ReadU32(Ini, Section, Line, MaxPoseKeys, Notes) : 0;
		for (u32 KeyIndex = 0; KeyIndex < KeyCount; ++KeyIndex)
		{
			float Values[8];
			xr_sprintf(Line, "block_%03u_key_%04u", Index, KeyIndex);
			if (!ReadFloats(Ini, Section, Line, Values, 8, Notes))
			{
				continue;
			}
			if (!(Values[0] >= 0.0f && Values[0] <= MaxTime) || (!Block.Keys.empty() && Values[0] <= Block.Keys.back().Time) || !std::isfinite(Values[1]) || !std::isfinite(Values[2]) || !std::isfinite(Values[3]))
			{
				Notes.Add("[%s] %s has a time out of range or order or a broken position, key dropped", Section, Line);
				continue;
			}
			if (ClampAll(Values + 4, PoseRanges, 4))
			{
				Notes.Add("[%s] %s is out of range, clamped", Section, Line);
			}
			PoseKey& Pose = Block.Keys.emplace_back();
			Pose.Time = Values[0];
			Pose.Position.set(Values[1], Values[2], Values[3]);
			Pose.Rotation.set(Values[4], Values[5], Values[6]);
			Pose.Mode = static_cast<Interp>(static_cast<u8>(Values[7]));
			float Shape[10];
			xr_sprintf(Line, "block_%03u_tangent_%04u", Index, KeyIndex);
			if (ReadRanged(Ini, Section, Line, Shape, TangentRanges, 10, Notes))
			{
				Pose.Tangent = static_cast<TangentMode>(static_cast<u8>(Shape[0]));
				Pose.Tension = Shape[1];
				Pose.Continuity = Shape[2];
				Pose.Bias = Shape[3];
				Pose.InHandle.set(Shape[4], Shape[5], Shape[6]);
				Pose.OutHandle.set(Shape[7], Shape[8], Shape[9]);
			}
		}
		xr_sprintf(Line, "block_%03u_shakes", Index);
		const u32 ShakeCount = Version < 6 ? ReadU32(Ini, Section, Line, MaxShakes, Notes) : 0;
		const u32 First = static_cast<u32>(Shot.Shakes.size());
		for (u32 ShakeIndex = 0; ShakeIndex < ShakeCount; ++ShakeIndex)
		{
			float Values[7] = { 0.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.0f, 0.0f };
			xr_sprintf(Line, "block_%03u_shake_%02u", Index, ShakeIndex);
			ReadRanged(Ini, Section, Line, Values, ShakeRanges, 7, Notes);
			const bool IsKept = ShakeIndex < MaxShakeRows && Shot.Shakes.size() < MaxShakeClips;
			Dropped += !IsKept;
			if (IsKept)
			{
				Shot.Shakes.push_back({ Start, Block.Length, 0.0f, 0.0f, static_cast<u8>(ShakeIndex), MakeShake(Values) });
			}
		}
		xr_sprintf(Prefix, "block_%03u_", Index);
		ReadLanes(Ini, Section, Prefix, Version < 6 ? WhereCameraBlock | WhereShakeClip : WhereCameraBlock, ShakeCount, Block.Lanes, Notes);
		for (Lane& Item : Block.Lanes)
		{
			if (Item.Id >= Param::ShakeInfluence && First + Item.Layer < Shot.Shakes.size())
			{
				Shot.Shakes[First + Item.Layer].Lanes.push_back(std::move(Item));
			}
		}
		std::erase_if(Block.Lanes, [](const Lane& Item) { return Item.Id >= Param::ShakeInfluence; });
		ReadLinks(Ini, Section, Prefix, Block.Attach, Notes);
		float Lens[6];
		xr_sprintf(Line, "block_%03u_dof", Index);
		if (ReadRanged(Ini, Section, Line, Lens, DofRanges, 6, Notes))
		{
			Block.Dof = static_cast<DofMode>(static_cast<u8>(Lens[0]));
			Block.Focus = Lens[1];
			Block.FocusOffset = Lens[2];
			Block.NearRange = Lens[3];
			Block.FarRange = Lens[4];
			Block.Blur = Lens[5];
		}
		float Glass[4] = { 0.0f, 4.0f, 0.0f, 0.0f };
		xr_sprintf(Line, "block_%03u_lens", Index);
		if (Ini.line_exist(Section, Line) && ReadFloats(Ini, Section, Line, Glass, 4, Notes))
		{
			Block.IsPhysical = Glass[0] != 0.0f;
			Block.Aperture = Sanitize(Glass[1], 0.7f, 64.0f, 4.0f);
			Block.FocalLength = Sanitize(Glass[2], 0.0f, 1000.0f, 0.0f);
			Block.IsAutofocus = Glass[3] != 0.0f;
		}
		float Smooth = 0.0f;
		xr_sprintf(Line, "block_%03u_smooth", Index);
		if (ReadFloats(Ini, Section, Line, &Smooth, 1, Notes))
		{
			Block.Smooth = Sanitize(Smooth, 0.0f, 2.0f, 0.0f);
		}
		float Tension = 1.0f;
		xr_sprintf(Line, "block_%03u_tension", Index);
		if (ReadFloats(Ini, Section, Line, &Tension, 1, Notes))
		{
			Block.Tension = Sanitize(Tension, 0.1f, 3.0f, 1.0f);
		}
		xr_sprintf(Line, "block_%03u_short_turn", Index);
		Block.IsShortTurn = ReadBool(Ini, Section, Line, false, Notes);
		bool NoFlags[1] = {};
		xr_sprintf(Line, "block_%03u_dof_target", Index);
		ReadLink(Ini, Section, Line, Block.FocusTarget, NoFlags, 0, Notes);
		xr_sprintf(Line, "block_%03u_name", Index);
		Block.Name = ReadName(Ini, Section, Line, Notes);
		float Rest[6] = {};
		xr_sprintf(Line, "block_%03u_pose", Index);
		if (ReadRanged(Ini, Section, Line, Rest, RestRanges, 6, Notes))
		{
			Block.Position.set(Rest[0], Rest[1], Rest[2]);
			Block.Rotation.set(Rest[3], Rest[4], Rest[5]);
		}
		if (Version < 16)
		{
			Approx += Cameras::KeysToLanes(Block) ? 1 : 0;
		}
		Cameras::Rebuild(Block);
		Start += Block.Length;
	}
	if (Approx)
	{
		Notes.Add("[%s] %u camera clips were converted to per-axis curves approximately (smooth rotation or edited tangents)", Section, Approx);
	}
	const u32 ClipCount = Version < 6 ? 0 : ReadU32(Ini, Section, "shakes", MaxShakeClips, Notes);
	for (u32 Index = 0; Index < ClipCount; ++Index)
	{
		float Head[6] = { 0.0f, 2.0f, 0.0f, 0.0f, 0.0f, 3.0f };
		float Values[7] = { 0.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.0f, 0.0f };
		xr_sprintf(Line, "shake_%03u", Index);
		ReadRanged(Ini, Section, Line, Head, ClipRanges, Version < 10 ? 5 : 6, Notes);
		xr_sprintf(Line, "shake_%03u_setup", Index);
		ReadRanged(Ini, Section, Line, Values, ShakeRanges, 7, Notes);
		ShakeClip& Clip = Shot.Shakes.emplace_back(ShakeClip { Head[0], Head[1], Head[2], Head[3], static_cast<u8>(Head[4]), MakeShake(Values), {}, static_cast<Interp>(Head[5]) });
		xr_sprintf(Prefix, "shake_%03u_", Index);
		ReadLanes(Ini, Section, Prefix, WhereShakeClip, 1, Clip.Lanes, Notes, Clip.Row);
	}
	for (u32 Row = 0; Row < MaxShakeRows; ++Row)
	{
		xr_sprintf(Line, "shake_row_%02u_mode", Row);
		Shot.OverrideRows |= static_cast<u8>(ReadU32(Ini, Section, Line, 1, Notes) << Row);
	}
	if (Dropped)
	{
		Notes.Add("[%s] %u shakes beyond the 8th of a block or past 256 in all were dropped", Section, Dropped);
	}
	if (IsPacked)
	{
		MigrateTrack(Rig, Blends, Start, Section, Notes);
	}
	CheckTrack(Shot, Section, Notes);
	Rig.Span.Start = 0.0f;
	if (const u32 Crowded = Cameras::SettleShakes(Shot))
	{
		Notes.Add("[%s] %u shakes overlap others on every row and stay overlapped", Section, Crowded);
	}
}

void ReadLight(const CInifile& Ini, const char* Section, LightData& Setup, Warnings& Notes)
{
	const char* KindText = Ini.read_if_exists<str_c>(Section, "kind", nullptr);
	const bool IsSpot = KindText && !xr_strcmp(KindText, KindNames[1]);
	if (KindText && !IsSpot && xr_strcmp(KindText, KindNames[0]))
	{
		Notes.Add("[%s] kind = %s is not point or spot, point is used", Section, KindText);
	}
	Setup.Kind = IsSpot ? LightKind::Spot : LightKind::Point;
	float Values[7] = { Setup.Color.x, Setup.Color.y, Setup.Color.z, Setup.Intensity, Setup.Range, Setup.Cone, Setup.VirtualSize };
	if (ReadRanged(Ini, Section, "light", Values, LightRanges, 7, Notes))
	{
		Setup.Color.set(Values[0], Values[1], Values[2]);
		Setup.Intensity = Values[3];
		Setup.Range = Values[4];
		Setup.Cone = Values[5];
		Setup.VirtualSize = Values[6];
	}
	Setup.HasShadow = ReadBool(Ini, Section, "shadow", false, Notes);
	Setup.IsVolumetric = ReadBool(Ini, Section, "volumetric", false, Notes);
	float Volume[3] = { Setup.VolumeIntensity, Setup.VolumeDistance, Setup.VolumeQuality };
	if (ReadRanged(Ini, Section, "volume", Volume, VolumeRanges, 3, Notes))
	{
		Setup.VolumeIntensity = Volume[0];
		Setup.VolumeDistance = Volume[1];
		Setup.VolumeQuality = Volume[2];
	}
	Setup.Texture = ReadSafe(Ini, Section, "texture", Notes);
	if (Setup.Texture.size() > MaxTextureBytes)
	{
		Notes.Add("[%s] texture is longer than %u bytes, ignored", Section, MaxTextureBytes);
		Setup.Texture = shared_str();
	}
}

void ReadParticle(const CInifile& Ini, const char* Section, ParticleData& Setup, Warnings& Notes)
{
	float Values[2] = { Setup.Size, Setup.Speed };
	Setup.Effect = ReadSafe(Ini, Section, "effect", Notes);
	if (ReadRanged(Ini, Section, "particle", Values, EffectRanges, 2, Notes))
	{
		Setup.Size = Values[0];
		Setup.Speed = Values[1];
	}
	Setup.ShouldRetrigger = ReadBool(Ini, Section, "retrigger", false, Notes);
}

void SizeKey(string64& Out, const RowSize& Size)
{
	const u32 Layer = Size.Layer, Part = Size.Kind - SizeLane - 1u;
	if (Size.Kind == SizeRow)
	{
		xr_strcpy(Out, "row_height");
	}
	else if (Size.Kind == SizeSub)
	{
		xr_sprintf(Out, "sub_%u_height", Layer);
	}
	else if (Size.Kind == SizeLane)
	{
		xr_sprintf(Out, "lane_%s_%u_height", Info(Size.Id).Id, Layer);
	}
	else
	{
		xr_sprintf(Out, "lane_%s_%u_%u_height", Info(Size.Id).Id, Layer, Part);
	}
}

void ReadSizes(const CInifile& Ini, const char* Section, Scene& Out, u32 ObjectId, Warnings& Notes)
{
	string64 Line;
	const auto Take = [&](u8 Kind, Param Id, u8 Layer)
	{
		RowSize Size { ObjectId, Kind, Id, Layer, 0.0f };
		SizeKey(Line, Size);
		const float Max = Kind < SizeLane ? MaxRowSize : MaxLaneSize;
		if (!ReadFloats(Ini, Section, Line, &Size.Rows, 1, Notes))
		{
			return;
		}
		if (!(Size.Rows >= 1.0f && Size.Rows <= Max))
		{
			Notes.Add("[%s] %s is not between 1 and %.0f, ignored", Section, Line, Max);
			return;
		}
		Out.Sizes.push_back(Size);
	};
	Take(SizeRow, Param::Position, 0);
	for (u8 Layer = 0; Layer < MaxShakeRows; ++Layer)
	{
		Take(SizeSub, Param::Position, Layer);
		for (u32 Id = 0; Id < static_cast<u32>(Param::Count); ++Id)
		{
			const ParamInfo& Meta = Info(static_cast<Param>(Id));
			for (u8 Kind = SizeLane; (!Layer || Meta.Where == WhereShakeClip || Id == static_cast<u32>(Param::LayerWeight)) && Kind < SizeLane + (Meta.Components == 3 ? 4 : 1); ++Kind)
			{
				Take(Kind, static_cast<Param>(Id), Layer);
			}
		}
	}
}

void WriteSizes(CInifile& Ini, const char* Section, const xr_vector<RowSize>& Sizes, u32 ObjectId)
{
	string64 Line;
	for (const RowSize& Size : Sizes)
	{
		if (Size.ObjectId == ObjectId)
		{
			SizeKey(Line, Size);
			Write(Ini, Section, Line, Size.Rows);
		}
	}
}

void ReadObject(const CInifile& Ini, u32 Index, Scene& Out, u32 Version, Warnings& Notes)
{
	string32 Section;
	xr_sprintf(Section, "object_%03u", Index);
	const char* TypeText = Ini.read_if_exists<str_c>(Section, "type", nullptr);
	u32 Type = 0;
	while (Type < 7 && (!TypeText || xr_strcmp(TypeText, TypeNames[Type])))
	{
		++Type;
	}
	if (Type == 7 || (Type == 1 && FindCamera(Out)))
	{
		Notes.Add("[%s] is missing, of an unknown type or a second camera, object skipped", Section);
		return;
	}
	Object& Item = Out.Objects.emplace_back();
	Item.Type = static_cast<ObjectType>(Type);
	Item.Id = ReadU32(Ini, Section, "id", IdCap, Notes);
	Item.Name = ReadName(Ini, Section, "name", Notes);
	ReadSizes(Ini, Section, Out, Item.Id, Notes);
	Item.IsNameLocked = ReadBool(Ini, Section, "name_locked", false, Notes);
	u32 Color[3] = {};
	const bool HasColor = ReadList(Ini, Section, "color", Color, 3, Notes);
	const bool IsColor = HasColor && Color[0] < 256 && Color[1] < 256 && Color[2] < 256;
	if (HasColor && !IsColor)
	{
		Notes.Add("[%s] color is above 255, a palette colour is used", Section);
	}
	Item.Color = IsColor ? Rgb(Color[0], Color[1], Color[2]) : NextColor(Out.Header);
	Item.Row = ReadU32(Ini, Section, "row", ~0u, Notes);
	Item.FolderId = ReadU32(Ini, Section, "folder", ~0u, Notes);
	if (Item.FolderId && std::none_of(Out.Header.Folders.begin(), Out.Header.Folders.end(), [&](const Folder& Other) { return Other.Id == Item.FolderId; }))
	{
		Notes.Add("[%s] folder %u does not exist, the object is moved to the root", Section, Item.FolderId);
		Item.FolderId = 0;
	}
	Item.IsMuted = ReadBool(Ini, Section, "muted", false, Notes);
	ReadRanged(Ini, Section, "clip_start", &Item.Span.Start, &SecondsRange, 1, Notes);
	ReadRanged(Ini, Section, "clip_length", &Item.Span.Length, &LengthRange, 1, Notes);
	Item.Span.IsLengthLocked = ReadBool(Ini, Section, "clip_locked", false, Notes);
	if (Fvector* Place = PlacementOf(Item, 0))
	{
		ReadVector(Ini, Section, "position", DistanceRange, *Place, Notes);
		ReadVector(Ini, Section, "rotation", AngleRange, *PlacementOf(Item, 1), Notes);
		ReadLinks(Ini, Section, "attach_", Item.Attach, Notes);
	}
	if (Item.Type == ObjectType::Actor)
	{
		ReadActor(Ini, Section, Item.Actor, Version < 8, Version < 12 || ReadBool(Ini, Section, "relative", false, Notes), Version >= 12, Notes);
	}
	else if (Item.Type == ObjectType::Light)
	{
		ReadLight(Ini, Section, Item.Light, Notes);
	}
	else if (Item.Type == ObjectType::Particle)
	{
		ReadParticle(Ini, Section, Item.Particle, Notes);
	}
	else if (Item.Type == ObjectType::Decal)
	{
		DecalData& Setup = Item.Decal;
		float Values[2] = { Setup.Width, Setup.Height };
		Setup.Texture = ReadSafe(Ini, Section, "texture", Notes);
		Setup.Shader = ReadSafe(Ini, Section, "shader", Notes);
		ReadFloats(Ini, Section, "decal", Values, 2, Notes);
		Setup.Width = Sanitize(Values[0], 0.01f, 20.0f, 0.5f);
		Setup.Height = Sanitize(Values[1], 0.01f, 20.0f, 0.5f);
		Setup.IsStamp = ReadBool(Ini, Section, "stamp", false, Notes);
	}
	else if (Item.Type == ObjectType::Weather)
	{
		WeatherData& Setup = Item.Weather;
		float Fades[2] = { 0.0f, 0.0f };
		string64 Key;
		Setup.Cycle = ReadSafe(Ini, Section, "weather", Notes);
		Setup.Mask = ReadU32(Ini, Section, "weather_mask", 0xFFFFu, Notes);
		ReadFloats(Ini, Section, "weather_fade", Fades, 2, Notes);
		Setup.FadeIn = Sanitize(Fades[0], 0.0f, MaxTime, 0.0f);
		Setup.FadeOut = Sanitize(Fades[1], 0.0f, MaxTime, 0.0f);
		for (u32 Id = static_cast<u32>(Param::WeatherTime); Id <= static_cast<u32>(Param::SkyRotation); ++Id)
		{
			float Values[3] = {};
			xr_sprintf(Key, "set_%s", Params[Id].Id);
			for (u8 Component = 0; ReadFloats(Ini, Section, Key, Values, Params[Id].Components, Notes) && Component < Params[Id].Components; ++Component)
			{
				float& Field = *WeatherField(Setup, static_cast<Param>(Id), Component);
				Field = Sanitize(Values[Component], Params[Id].Min, Params[Id].Max, Field);
			}
		}
	}
	else if (Item.Type == ObjectType::Post)
	{
		PostData& Setup = Item.Post;
		float Fades[2] = { 0.0f, 0.0f };
		string64 Key;
		Setup.Mask = ReadU32(Ini, Section, "post_mask", 0xFFFFu, Notes);
		ReadFloats(Ini, Section, "post_fade_times", Fades, 2, Notes);
		Setup.FadeIn = Sanitize(Fades[0], 0.0f, MaxTime, 0.0f);
		Setup.FadeOut = Sanitize(Fades[1], 0.0f, MaxTime, 0.0f);
		for (u32 Id = static_cast<u32>(Param::PostBase); Id <= static_cast<u32>(Param::PostMotionBlur); ++Id)
		{
			float Values[3] = {};
			xr_sprintf(Key, "set_%s", Params[Id].Id);
			for (u8 Component = 0; ReadFloats(Ini, Section, Key, Values, Params[Id].Components, Notes) && Component < Params[Id].Components; ++Component)
			{
				float& Field = *PostField(Setup, static_cast<Param>(Id), Component);
				Field = Sanitize(Values[Component], Params[Id].Min, Params[Id].Max, Field);
			}
		}
	}
	else
	{
		ReadCamera(Ini, Section, Item, Version, Notes);
	}
	ReadLanes(Ini, Section, "", ObjectWhere[Type] | (Item.Type == ObjectType::Actor ? WherePoseBone : 0), static_cast<u32>(Item.Actor.Layers.size()), Item.Lanes, Notes);
	const xr_vector<PoseRow>& Poses = Item.Actor.Poses;
	const size_t Before = Item.Lanes.size();
	std::erase_if(Item.Lanes, [&Poses](const Lane& Track)
	{
		const u32 Row = Track.Id == Param::PoseWeight ? Track.Layer : Track.Layer / MaxPoseBones, Bone = Track.Layer % MaxPoseBones;
		return (Track.Id == Param::PoseWeight && Row >= Poses.size()) || (IsBoneParam(Track.Id) && (Row >= Poses.size() || Bone >= Poses[Row].Bones.size()));
	});
	if (Item.Lanes.size() != Before)
	{
		Notes.Add("[%s] %u pose lanes point at a missing pose row or bone, dropped", Section, static_cast<u32>(Before - Item.Lanes.size()));
	}
}

void ReadScene(const CInifile& Ini, Scene& Out, u32 Version, Warnings& Notes)
{
	string64 Line;
	SceneHeader& Header = Out.Header;
	const u32 Rate = ReadU32(Ini, "scene", "frame_rate", ~0u, Notes);
	Header.FrameRate = std::find(std::begin(FrameRates), std::end(FrameRates), Rate) != std::end(FrameRates) ? Rate : 30;
	if (Header.FrameRate != Rate)
	{
		Notes.Add("[scene] frame_rate %u is not 24, 25, 30 or 60, 30 is used", Rate);
	}
	float Jump[2] = { 5.0f, 45.0f };
	if (ReadFloats(Ini, "scene", "cut_jump", Jump, 2, Notes))
	{
		Header.CutDistance = Sanitize(Jump[0], 0.0f, 1000.0f, 5.0f);
		Header.CutAngle = Sanitize(Jump[1], 0.0f, 180.0f, 45.0f);
	}
	Header.IsWorldFollowing = ReadBool(Ini, "scene", "world_follows_camera", false, Notes);
	Out.NextId = ReadU32(Ini, "scene", "next_id", IdCap, Notes);
	Header.NextColor = ReadU32(Ini, "scene", "next_color", ~0u, Notes);
	const u32 FolderCount = ReadU32(Ini, "scene", "folders", MaxFolders, Notes);
	for (u32 Index = 0; Index < FolderCount; ++Index)
	{
		u32 Pair[2] = {};
		xr_sprintf(Line, "folder_%03u", Index);
		if (!ReadList(Ini, "scene", Line, Pair, 2, Notes) || !Pair[0] || Pair[0] > IdCap || std::any_of(Header.Folders.begin(), Header.Folders.end(), [&](const Folder& Other) { return Other.Id == Pair[0]; }))
		{
			Notes.Add("[scene] %s is missing or has a zero, repeated or too large id, folder skipped", Line);
			continue;
		}
		Folder& Item = Header.Folders.emplace_back();
		Item.Id = Pair[0];
		Item.Row = Pair[1];
		xr_sprintf(Line, "folder_%03u_name", Index);
		Item.Name = ReadName(Ini, "scene", Line, Notes);
	}
	xr_vector<Marker> Markers(ReadU32(Ini, "scene", "markers", MaxMarkers, Notes));
	for (u32 Index = 0; Index < Markers.size(); ++Index)
	{
		Markers[Index].Time = -1.0f;
		xr_sprintf(Line, "marker_%03u", Index);
		if (!ReadRanged(Ini, "scene", Line, &Markers[Index].Time, &MarkerRange, 1, Notes) || Markers[Index].Time < 0.0f)
		{
			Notes.Add("[scene] %s is missing or not a number, marker skipped", Line);
			continue;
		}
		xr_sprintf(Line, "marker_%03u_name", Index);
		Markers[Index].Name = ReadName(Ini, "scene", Line, Notes);
	}
	std::erase_if(Markers, [](const Marker& Item) { return Item.Time < 0.0f; });
	std::stable_sort(Markers.begin(), Markers.end(), [](const Marker& A, const Marker& B) { return A.Time < B.Time; });
	for (Marker& Item : Markers)
	{
		if (IsMarkerFree(Header, Item.Time))
		{
			Header.Markers.push_back(std::move(Item));
		}
	}
	if (Header.Markers.size() < Markers.size())
	{
		Notes.Add("[scene] %u markers within half a frame of another marker were dropped", static_cast<u32>(Markers.size() - Header.Markers.size()));
	}
	const u32 ObjectCount = ReadU32(Ini, "scene", "objects", MaxObjects + 1, Notes);
	for (u32 Index = 0; Index < ObjectCount; ++Index)
	{
		ReadObject(Ini, Index, Out, Version, Notes);
	}
	u32 MaxId = 0;
	for (const Object& Item : Out.Objects)
	{
		MaxId = std::max(MaxId, Item.Id);
	}
	for (const Folder& Item : Header.Folders)
	{
		MaxId = std::max(MaxId, Item.Id);
	}
	if (Out.NextId <= MaxId)
	{
		Notes.Add("[scene] next_id %u is not above the largest id, %u is used", Out.NextId, MaxId + 1);
		Out.NextId = MaxId + 1;
	}
	for (size_t Index = 0; Index < Out.Objects.size(); ++Index)
	{
		Object& Item = Out.Objects[Index];
		if (!Item.Id || std::any_of(Out.Objects.begin(), Out.Objects.begin() + Index, [&](const Object& Other) { return Other.Id == Item.Id; }))
		{
			Notes.Add("Object id %u is zero or repeated, %u is used", Item.Id, Out.NextId);
			Item.Id = Out.NextId++;
		}
	}
	xr_vector<Attachment> Wanted;
	for (Object& Item : Out.Objects)
	{
		Wanted.push_back(std::exchange(Item.Attach, Attachment {}));
		for (CameraBlock& Shot : Item.Camera.Blocks)
		{
			Wanted.push_back(std::exchange(Shot.Attach, Attachment {}));
		}
	}
	AttachGraph Graph;
	u32 Cursor = 0;
	const auto Restore = [&](const Object& Owner, Attachment& Slot)
	{
		Slot = Wanted[Cursor++];
		for (AttachLink* Link : { &Slot.Parent, &Slot.LookAt })
		{
			const AttachLink Want = std::exchange(*Link, AttachLink {});
			const Object* Target = Find(Out, Want.ObjectId);
			if (Target && !Graph.Build(Out, Owner.Id, Want.ObjectId))
			{
				Notes.Add("Object %u \"%s\" -> object %u \"%s\": a self link, a loop or a chain deeper than %u, link dropped", Owner.Id, Owner.Name.c_str(), Target->Id, Target->Name.c_str(), MaxAttachDepth);
				continue;
			}
			if (!Target && Want.ObjectId)
			{
				Notes.Add("Object %u \"%s\" links to object %u, which is not in the scene; the link is kept", Owner.Id, Owner.Name.c_str(), Want.ObjectId);
			}
			*Link = Want;
		}
	};
	for (Object& Item : Out.Objects)
	{
		Restore(Item, Item.Attach);
		for (CameraBlock& Shot : Item.Camera.Blocks)
		{
			Restore(Item, Shot.Attach);
		}
	}
}

void WriteActor(CInifile& Ini, const char* Section, const ActorData& Setup)
{
	string64 Line;
	if (Setup.Visual.size())
	{
		Ini.w_string(Section, "visual", Setup.Visual.c_str());
	}
	if (Setup.Item.size())
	{
		Ini.w_string(Section, "item", Setup.Item.c_str());
	}
	if (Setup.Hud.size())
	{
		Ini.w_string(Section, "hud", Setup.Hud.c_str());
	}
	if (Setup.IsHudLinked)
	{
		Ini.w_bool(Section, "hud_link", true);
	}
	Write(Ini, Section, "scale", Setup.Scale.x, Setup.Scale.y, Setup.Scale.z);
	Write(Ini, Section, "time_scale", Setup.TimeScale);
	if (Setup.IsRelative)
	{
		Ini.w_bool(Section, "relative", true);
		Ini.w_string(Section, "end", EndNames[static_cast<u32>(Setup.End)]);
	}
	Write(Ini, Section, "packs", static_cast<u32>(Setup.Packs.size()));
	for (u32 Index = 0; Index < Setup.Packs.size(); ++Index)
	{
		xr_sprintf(Line, "pack_%02u", Index);
		Ini.w_string(Section, Line, Setup.Packs[Index].c_str());
	}
	Write(Ini, Section, "rows", static_cast<u32>(Setup.Layers.size() + 1));
	for (u8 Index = 0; Index <= Setup.Layers.size(); ++Index)
	{
		const ActorLayer& Layer = RowOf(Setup, Index);
		string32 Prefix;
		xr_sprintf(Prefix, "row_%02u_", Index + 0u);
		if (!Layer.Parts.empty())
		{
			xr_sprintf(Line, "%sparts", Prefix);
			Write(Ini, Section, Line, static_cast<u32>(Layer.Parts.size()));
		}
		for (u32 Part = 0; Part < Layer.Parts.size(); ++Part)
		{
			xr_sprintf(Line, "%spart_%u", Prefix, Part);
			Ini.w_string(Section, Line, Layer.Parts[Part].c_str());
		}
		if (Layer.Mode == RowMode::Add)
		{
			xr_sprintf(Line, "%smode", Prefix);
			Write(Ini, Section, Line, 1u);
		}
		if (Layer.IsMuted)
		{
			xr_sprintf(Line, "%smuted", Prefix);
			Ini.w_bool(Section, Line, true);
		}
		if (Layer.Weight != 1.0f)
		{
			xr_sprintf(Line, "%sweight", Prefix);
			Write(Ini, Section, Line, Layer.Weight);
		}
		WriteBlocks(Ini, Section, Prefix, Layer.Blocks);
	}
	if (!Setup.Poses.empty())
	{
		Write(Ini, Section, "pose_rows", static_cast<u32>(Setup.Poses.size()));
	}
	for (u32 Row = 0; Row < Setup.Poses.size(); ++Row)
	{
		const PoseRow& Pose = Setup.Poses[Row];
		xr_sprintf(Line, "pose_%02u", Row);
		Write(Ini, Section, Line, Pose.Mode == RowMode::Add ? 1.0f : 0.0f, Pose.IsMuted ? 1.0f : 0.0f, Pose.Weight);
		xr_sprintf(Line, "pose_%02u_bones", Row);
		Write(Ini, Section, Line, static_cast<u32>(Pose.Bones.size()));
		for (u32 Bone = 0; Bone < Pose.Bones.size(); ++Bone)
		{
			const PoseBone& Item = Pose.Bones[Bone];
			xr_sprintf(Line, "pose_%02u_bone_%02u", Row, Bone);
			Ini.w_string(Section, Line, Item.Name.c_str());
			if (!Item.Rotation.similar(Fvector().set(0.0f, 0.0f, 0.0f), 0.0f) || !Item.Offset.similar(Fvector().set(0.0f, 0.0f, 0.0f), 0.0f))
			{
				xr_sprintf(Line, "pose_%02u_bone_%02u_rest", Row, Bone);
				Write(Ini, Section, Line, Item.Rotation.x, Item.Rotation.y, Item.Rotation.z, Item.Offset.x, Item.Offset.y, Item.Offset.z);
			}
			if (!Item.Scale.similar(Fvector().set(1.0f, 1.0f, 1.0f), 0.0f))
			{
				xr_sprintf(Line, "pose_%02u_bone_%02u_scale", Row, Bone);
				Write(Ini, Section, Line, Item.Scale.x, Item.Scale.y, Item.Scale.z);
			}
		}
	}
	if (!Setup.Clips.empty())
	{
		Write(Ini, Section, "clips", static_cast<u32>(Setup.Clips.size()));
	}
	for (u32 Clip = 0; Clip < Setup.Clips.size(); ++Clip)
	{
		const AnimClip& Item = Setup.Clips[Clip];
		string64 Prefix;
		xr_sprintf(Line, "clip_%02u", Clip);
		Ini.w_string(Section, Line, Item.Name.c_str());
		xr_sprintf(Line, "clip_%02u_info", Clip);
		Write(Ini, Section, Line, Item.Length, Item.Mode == RowMode::Add ? 1.0f : 0.0f);
		xr_sprintf(Line, "clip_%02u_bones", Clip);
		Write(Ini, Section, Line, static_cast<u32>(Item.Bones.size()));
		for (u32 Bone = 0; Bone < Item.Bones.size(); ++Bone)
		{
			const PoseBone& Entry = Item.Bones[Bone];
			xr_sprintf(Line, "clip_%02u_bone_%02u", Clip, Bone);
			Ini.w_string(Section, Line, Entry.Name.c_str());
			xr_sprintf(Line, "clip_%02u_bone_%02u_rest", Clip, Bone);
			Write(Ini, Section, Line, Entry.Rotation.x, Entry.Rotation.y, Entry.Rotation.z, Entry.Offset.x, Entry.Offset.y, Entry.Offset.z);
			xr_sprintf(Line, "clip_%02u_bone_%02u_scale", Clip, Bone);
			Write(Ini, Section, Line, Entry.Scale.x, Entry.Scale.y, Entry.Scale.z);
		}
		xr_sprintf(Prefix, "clip_%02u_", Clip);
		WriteLanes(Ini, Section, Prefix, Item.Lanes);
		if (!Item.Layers.empty())
		{
			xr_sprintf(Line, "clip_%02u_layers", Clip);
			Write(Ini, Section, Line, static_cast<u32>(Item.Layers.size()));
		}
		for (u32 Layer = 0; Layer < Item.Layers.size(); ++Layer)
		{
			const ClipLayer& Over = Item.Layers[Layer];
			xr_sprintf(Prefix, "clip_%02u_layer_%02u_", Clip, Layer);
			xr_sprintf(Line, "%sname", Prefix);
			Ini.w_string(Section, Line, Over.Name.c_str());
			xr_sprintf(Line, "%sinfo", Prefix);
			Write(Ini, Section, Line, Over.Weight, Over.Mode == RowMode::Add ? 1.0f : 0.0f, Over.IsMuted ? 1.0f : 0.0f);
			WriteTrack(Ini, Section, Prefix, Over);
		}
		for (u32 Limb = 0; Limb < MaxIkLimbs; ++Limb)
		{
			const IkLimb& Arm = Item.Ik[Limb];
			if (!Arm.Bones[0].size())
			{
				continue;
			}
			xr_sprintf(Line, "clip_%02u_ik_%02u_bones", Clip, Limb);
			Ini.w_string(Section, Line, (xr_string(Arm.Bones[0].c_str()) + ", " + Arm.Bones[1].c_str() + ", " + Arm.Bones[2].c_str()).c_str());
			xr_sprintf(Line, "clip_%02u_ik_%02u", Clip, Limb);
			Write(Ini, Section, Line, Arm.IsOn ? 1.0f : 0.0f, Arm.Space == IkSpace::World ? 1.0f : 0.0f, Arm.Weight);
			xr_sprintf(Line, "clip_%02u_ik_%02u_goal", Clip, Limb);
			Write(Ini, Section, Line, Arm.Target.x, Arm.Target.y, Arm.Target.z, Arm.Turn.x, Arm.Turn.y, Arm.Turn.z, Arm.Pole.x, Arm.Pole.y, Arm.Pole.z);
		}
		if (!Item.IkLanes.empty())
		{
			xr_sprintf(Prefix, "clip_%02u_ik_", Clip);
			WriteLanes(Ini, Section, Prefix, Item.IkLanes);
		}
		if (Item.IsLinked)
		{
			xr_sprintf(Line, "clip_%02u_linked", Clip);
			Ini.w_bool(Section, Line, true);
		}
	}
	if (!Setup.Looks.empty())
	{
		Write(Ini, Section, "looks", static_cast<u32>(Setup.Looks.size()));
	}
	for (u32 Look = 0; Look < Setup.Looks.size(); ++Look)
	{
		const xr_vector<SurfaceShader>& Surfaces = Setup.Looks[Look].Surfaces;
		xr_sprintf(Line, "look_%02u_surfaces", Look);
		Write(Ini, Section, Line, static_cast<u32>(Surfaces.size()));
		for (u32 Index = 0; Index < Surfaces.size(); ++Index)
		{
			const SurfaceShader& Entry = Surfaces[Index];
			xr_sprintf(Line, "look_%02u_%02u", Look, Index);
			Write(Ini, Section, Line, static_cast<u32>(Entry.Surface));
			xr_sprintf(Line, "look_%02u_%02u_shader", Look, Index);
			Entry.Shader.size() ? Ini.w_string(Section, Line, Entry.Shader.c_str()) : (void)0;
			xr_sprintf(Line, "look_%02u_%02u_texture", Look, Index);
			Entry.Texture.size() ? Ini.w_string(Section, Line, Entry.Texture.c_str()) : (void)0;
		}
	}
	for (u32 Limb = 0; Limb < MaxIkLimbs; ++Limb)
	{
		const IkLimb& Item = Setup.Ik[Limb];
		if (!Item.Bones[0].size())
		{
			continue;
		}
		xr_sprintf(Line, "ik_%02u_bones", Limb);
		Ini.w_string(Section, Line, (xr_string(Item.Bones[0].c_str()) + ", " + Item.Bones[1].c_str() + ", " + Item.Bones[2].c_str()).c_str());
		xr_sprintf(Line, "ik_%02u", Limb);
		Write(Ini, Section, Line, Item.IsOn ? 1.0f : 0.0f, Item.Space == IkSpace::World ? 1.0f : 0.0f, Item.Weight);
		xr_sprintf(Line, "ik_%02u_goal", Limb);
		Write(Ini, Section, Line, Item.Target.x, Item.Target.y, Item.Target.z, Item.Turn.x, Item.Turn.y, Item.Turn.z, Item.Pole.x, Item.Pole.y, Item.Pole.z);
	}
	if (!Setup.Constraints.empty())
	{
		Write(Ini, Section, "constraints", static_cast<u32>(Setup.Constraints.size()));
	}
	for (u32 Slot = 0; Slot < Setup.Constraints.size(); ++Slot)
	{
		const BoneConstraint& Rule = Setup.Constraints[Slot];
		string512 Text;
		xr_sprintf(Line, "constraint_%02u_bone", Slot);
		Rule.Bone.size() ? Ini.w_string(Section, Line, Rule.Bone.c_str()) : (void)0;
		xr_sprintf(Line, "constraint_%02u_target", Slot);
		xr_sprintf(Text, "%u,%s", Rule.Target.ObjectId, Rule.Target.Bone.size() ? Rule.Target.Bone.c_str() : "");
		Rule.Target.ObjectId ? Ini.w_string(Section, Line, Text) : (void)0;
		xr_sprintf(Line, "constraint_%02u", Slot);
		Write(Ini, Section, Line, static_cast<float>(Rule.Kind), Rule.IsMuted ? 1.0f : 0.0f, Rule.Weight, Rule.Shift.x, Rule.Shift.y, Rule.Shift.z, Rule.Turn.x, Rule.Turn.y, Rule.Turn.z);
	}
}

void WriteLight(CInifile& Ini, const char* Section, const LightData& Setup)
{
	Ini.w_string(Section, "kind", KindNames[static_cast<u32>(Setup.Kind)]);
	Write(Ini, Section, "light", Setup.Color.x, Setup.Color.y, Setup.Color.z, Setup.Intensity, Setup.Range, Setup.Cone, Setup.VirtualSize);
	Ini.w_bool(Section, "shadow", Setup.HasShadow);
	Ini.w_bool(Section, "volumetric", Setup.IsVolumetric);
	Write(Ini, Section, "volume", Setup.VolumeIntensity, Setup.VolumeDistance, Setup.VolumeQuality);
	if (Setup.Texture.size())
	{
		Ini.w_string(Section, "texture", Setup.Texture.c_str());
	}
}

void WriteParticle(CInifile& Ini, const char* Section, const ParticleData& Setup)
{
	if (Setup.Effect.size())
	{
		Ini.w_string(Section, "effect", Setup.Effect.c_str());
	}
	Write(Ini, Section, "particle", Setup.Size, Setup.Speed);
	Ini.w_bool(Section, "retrigger", Setup.ShouldRetrigger);
}

void WriteObject(CInifile& Ini, const char* Section, const Object& Item)
{
	string64 Line;
	string32 Prefix;
	Write(Ini, Section, "id", Item.Id);
	Ini.w_string(Section, "type", TypeNames[static_cast<u32>(Item.Type)]);
	WriteName(Ini, Section, "name", Item.Name);
	Ini.w_bool(Section, "name_locked", Item.IsNameLocked);
	Write(Ini, Section, "color", Item.Color & 255u, (Item.Color >> 8) & 255u, (Item.Color >> 16) & 255u);
	Write(Ini, Section, "row", Item.Row);
	Write(Ini, Section, "folder", Item.FolderId);
	Ini.w_bool(Section, "muted", Item.IsMuted);
	Write(Ini, Section, "clip_start", Item.Span.Start);
	Write(Ini, Section, "clip_length", Item.Span.Length);
	Ini.w_bool(Section, "clip_locked", Item.Span.IsLengthLocked);
	WriteLanes(Ini, Section, "", Item.Lanes);
	if (const Fvector* Place = PlacementOf(Item, 0))
	{
		const Fvector& Turn = *PlacementOf(Item, 1);
		Write(Ini, Section, "position", Place->x, Place->y, Place->z);
		Write(Ini, Section, "rotation", Turn.x, Turn.y, Turn.z);
		WriteLinks(Ini, Section, "attach_", Item.Attach);
	}
	if (Item.Type == ObjectType::Actor)
	{
		WriteActor(Ini, Section, Item.Actor);
		return;
	}
	if (Item.Type == ObjectType::Light)
	{
		WriteLight(Ini, Section, Item.Light);
		return;
	}
	if (Item.Type == ObjectType::Particle)
	{
		WriteParticle(Ini, Section, Item.Particle);
		return;
	}
	if (Item.Type == ObjectType::Decal)
	{
		Item.Decal.Texture.size() ? Ini.w_string(Section, "texture", Item.Decal.Texture.c_str()) : (void)0;
		Item.Decal.Shader.size() ? Ini.w_string(Section, "shader", Item.Decal.Shader.c_str()) : (void)0;
		Write(Ini, Section, "decal", Item.Decal.Width, Item.Decal.Height);
		Ini.w_bool(Section, "stamp", Item.Decal.IsStamp);
		return;
	}
	if (Item.Type == ObjectType::Weather)
	{
		WeatherData& Setup = const_cast<WeatherData&>(Item.Weather);
		string64 Key;
		Setup.Cycle.size() ? Ini.w_string(Section, "weather", Setup.Cycle.c_str()) : (void)0;
		Write(Ini, Section, "weather_mask", Setup.Mask);
		Write(Ini, Section, "weather_fade", Setup.FadeIn, Setup.FadeOut);
		for (u32 Id = static_cast<u32>(Param::WeatherTime); Id <= static_cast<u32>(Param::SkyRotation); ++Id)
		{
			const Param Which = static_cast<Param>(Id);
			xr_sprintf(Key, "set_%s", Params[Id].Id);
			if (Params[Id].Components == 3)
			{
				Write(Ini, Section, Key, *WeatherField(Setup, Which, 0), *WeatherField(Setup, Which, 1), *WeatherField(Setup, Which, 2));
			}
			else
			{
				Write(Ini, Section, Key, *WeatherField(Setup, Which, 0));
			}
		}
		return;
	}
	if (Item.Type == ObjectType::Post)
	{
		PostData& Setup = const_cast<PostData&>(Item.Post);
		string64 Key;
		Write(Ini, Section, "post_mask", Setup.Mask);
		Write(Ini, Section, "post_fade_times", Setup.FadeIn, Setup.FadeOut);
		for (u32 Id = static_cast<u32>(Param::PostBase); Id <= static_cast<u32>(Param::PostMotionBlur); ++Id)
		{
			const Param Which = static_cast<Param>(Id);
			xr_sprintf(Key, "set_%s", Params[Id].Id);
			if (Params[Id].Components == 3)
			{
				Write(Ini, Section, Key, *PostField(Setup, Which, 0), *PostField(Setup, Which, 1), *PostField(Setup, Which, 2));
			}
			else
			{
				Write(Ini, Section, Key, *PostField(Setup, Which, 0));
			}
		}
		return;
	}
	const CameraData& Shot = Item.Camera;
	Write(Ini, Section, "blocks", static_cast<u32>(Shot.Blocks.size()));
	for (u32 Index = 0; Index < Shot.Blocks.size(); ++Index)
	{
		const CameraBlock& Block = Shot.Blocks[Index];
		xr_sprintf(Line, "block_%03u", Index);
		Write(Ini, Section, Line, Block.Length, Block.BlendEase, Block.Fov, Block.TimeScale, Block.Start);
		if (!Block.Position.similar(Fvector().set(0.0f, 0.0f, 0.0f), 0.0f) || !Block.Rotation.similar(Fvector().set(0.0f, 0.0f, 0.0f), 0.0f))
		{
			xr_sprintf(Line, "block_%03u_pose", Index);
			Write(Ini, Section, Line, Block.Position.x, Block.Position.y, Block.Position.z, Block.Rotation.x, Block.Rotation.y, Block.Rotation.z);
		}
		xr_sprintf(Prefix, "block_%03u_", Index);
		WriteLanes(Ini, Section, Prefix, Block.Lanes);
		WriteLinks(Ini, Section, Prefix, Block.Attach);
		const CameraBlock Fresh;
		if (Block.Dof != Fresh.Dof || Block.Focus != Fresh.Focus || Block.FocusOffset != Fresh.FocusOffset || Block.NearRange != Fresh.NearRange || Block.FarRange != Fresh.FarRange || Block.Blur != Fresh.Blur)
		{
			xr_sprintf(Line, "block_%03u_dof", Index);
			Write(Ini, Section, Line, Block.Dof, Block.Focus, Block.FocusOffset, Block.NearRange, Block.FarRange, Block.Blur);
		}
		if (Block.IsPhysical != Fresh.IsPhysical || Block.Aperture != Fresh.Aperture || Block.FocalLength != Fresh.FocalLength || Block.IsAutofocus != Fresh.IsAutofocus)
		{
			xr_sprintf(Line, "block_%03u_lens", Index);
			Write(Ini, Section, Line, Block.IsPhysical ? 1.0f : 0.0f, Block.Aperture, Block.FocalLength, Block.IsAutofocus ? 1.0f : 0.0f);
		}
		if (Block.Smooth != Fresh.Smooth)
		{
			xr_sprintf(Line, "block_%03u_smooth", Index);
			Write(Ini, Section, Line, Block.Smooth);
		}
		if (Block.Tension != Fresh.Tension)
		{
			xr_sprintf(Line, "block_%03u_tension", Index);
			Write(Ini, Section, Line, Block.Tension);
		}
		if (Block.IsShortTurn)
		{
			xr_sprintf(Line, "block_%03u_short_turn", Index);
			Ini.w_bool(Section, Line, true);
		}
		if (Block.FocusTarget.ObjectId)
		{
			string512 Text;
			xr_sprintf(Line, "block_%03u_dof_target", Index);
			xr_sprintf(Text, "%u,%s", Block.FocusTarget.ObjectId, Block.FocusTarget.Bone.size() ? Block.FocusTarget.Bone.c_str() : "");
			Ini.w_string(Section, Line, Text);
		}
		if (Block.Name.size())
		{
			xr_sprintf(Line, "block_%03u_name", Index);
			WriteName(Ini, Section, Line, Block.Name);
		}
	}
	if (!Shot.Shakes.empty())
	{
		Write(Ini, Section, "shakes", static_cast<u32>(Shot.Shakes.size()));
	}
	for (u32 Index = 0; Index < Shot.Shakes.size(); ++Index)
	{
		const ShakeClip& Clip = Shot.Shakes[Index];
		const Shake& Layer = Clip.Setup;
		xr_sprintf(Line, "shake_%03u", Index);
		Write(Ini, Section, Line, Clip.Start, Clip.Length, Clip.FadeIn, Clip.FadeOut, Clip.Row, Clip.BlendEase);
		xr_sprintf(Line, "shake_%03u_setup", Index);
		Write(Ini, Section, Line, Layer.Preset, Layer.IsEnabled, Layer.Influence, Layer.PositionScale, Layer.Speed, Layer.Offset, Layer.IsStretched);
		xr_sprintf(Prefix, "shake_%03u_", Index);
		WriteLanes(Ini, Section, Prefix, Clip.Lanes, Clip.Row);
	}
	for (u32 Row = 0; Row < MaxShakeRows; ++Row)
	{
		xr_sprintf(Line, "shake_row_%02u_mode", Row);
		if ((Shot.OverrideRows >> Row) & 1)
		{
			Write(Ini, Section, Line, 1u);
		}
	}
}


bool OptionalSafe(const shared_str& Text)
{
	return !Text.size() || IniSafe(Text.c_str());
}

bool ActorSafe(const ActorData& Setup)
{
	const auto IsBlocksSafe = [](const xr_vector<ActorBlock>& Blocks) { return Blocks.size() <= MaxBlocks && std::all_of(Blocks.begin(), Blocks.end(), [](const ActorBlock& Block) { return IniSafe(Block.Source.c_str()) && IniSafe(Block.Name.c_str()); }); };
	const auto IsLayerSafe = [&](const ActorLayer& Layer) { return Layer.Parts.size() <= MaxPartNames && IsBlocksSafe(Layer.Blocks) && std::all_of(Layer.Parts.begin(), Layer.Parts.end(), [](const shared_str& Part) { return IniSafe(Part.c_str()); }); };
	const auto IsPoseSafe = [](const PoseRow& Pose) { return Pose.Bones.size() <= MaxPoseBones && std::all_of(Pose.Bones.begin(), Pose.Bones.end(), [](const PoseBone& Bone) { return OptionalSafe(Bone.Name); }); };
	const auto IsLookSafe = [](const ActorLook& Look) { return Look.Surfaces.size() <= 2 * MaxLookSurfaces && std::all_of(Look.Surfaces.begin(), Look.Surfaces.end(), [](const SurfaceShader& Item) { return Item.Surface < ItemSurfaceBase + MaxLookSurfaces && OptionalSafe(Item.Shader) && OptionalSafe(Item.Texture); }); };
	const auto IsIkSafe = [](const IkLimb& Limb) { return std::all_of(std::begin(Limb.Bones), std::end(Limb.Bones), OptionalSafe); };
	const auto IsRuleSafe = [](const BoneConstraint& Rule) { return OptionalSafe(Rule.Bone) && OptionalSafe(Rule.Target.Bone); };
	return Setup.Constraints.size() <= MaxConstraints && std::all_of(Setup.Constraints.begin(), Setup.Constraints.end(), IsRuleSafe) && std::all_of(std::begin(Setup.Ik), std::end(Setup.Ik), IsIkSafe) && Setup.Looks.size() <= MaxLooks && std::all_of(Setup.Looks.begin(), Setup.Looks.end(), IsLookSafe) && Setup.Poses.size() <= MaxPoseRows && std::all_of(Setup.Poses.begin(), Setup.Poses.end(), IsPoseSafe) && OptionalSafe(Setup.Visual) && OptionalSafe(Setup.Item) && Setup.Packs.size() <= MaxPacks && Setup.Layers.size() <= MaxActorLayers && IsLayerSafe(Setup.Base) && std::all_of(Setup.Layers.begin(), Setup.Layers.end(), IsLayerSafe) && std::all_of(Setup.Packs.begin(), Setup.Packs.end(), [](const shared_str& Pack) { return IniSafe(Pack.c_str()); });
}

bool CanSave(const Scene& Data, xr_string& Error)
{
	const auto IsLinkSafe = [](const Attachment& Links) { return OptionalSafe(Links.Parent.Bone) && OptionalSafe(Links.LookAt.Bone); };
	const auto IsSmall = [](const xr_vector<Lane>& Lanes) { return Lanes.size() <= MaxLanes && std::all_of(Lanes.begin(), Lanes.end(), [](const Lane& Item) { return std::all_of(std::begin(Item.Curves), std::end(Item.Curves), [](const Curve& Source) { return Source.Keys.size() <= MaxLaneKeys; }); }); };
	const auto IsClipSafe = [&](const ShakeClip& Clip) { return Clip.Row < MaxShakeRows && IsSmall(Clip.Lanes); };
	const auto IsShotSafe = [&](const CameraData& Shot) { return Shot.Blocks.size() <= MaxBlocks && Shot.Shakes.size() <= MaxShakeClips && std::all_of(Shot.Shakes.begin(), Shot.Shakes.end(), IsClipSafe) && std::all_of(Shot.Blocks.begin(), Shot.Blocks.end(), [&](const CameraBlock& Block) { return Block.Keys.size() <= MaxPoseKeys && IsSmall(Block.Lanes) && IsLinkSafe(Block.Attach); }); };
	const SceneHeader& Header = Data.Header;
	const bool HasLongName = std::any_of(Header.Folders.begin(), Header.Folders.end(), [](const Folder& Item) { return Item.Name.size() > MaxNameBytes; }) || std::any_of(Header.Markers.begin(), Header.Markers.end(), [](const Marker& Item) { return Item.Name.size() > MaxNameBytes; });
	if (!OptionalSafe(Data.LevelName) || Data.Objects.size() > MaxObjects + 1 || Header.Folders.size() > MaxFolders || Header.Markers.size() > MaxMarkers || HasLongName)
	{
		Error = "The scene has too many objects, folders or markers, a folder or marker name longer than 127 bytes, or an unusual level name.";
		return false;
	}
	u32 CameraCount = 0;
	for (const Object& Item : Data.Objects)
	{
		CameraCount += Item.Type == ObjectType::Camera;
		const bool IsTypeSafe[7] = { ActorSafe(Item.Actor), CameraCount == 1 && IsShotSafe(Item.Camera), OptionalSafe(Item.Light.Texture) && Item.Light.Texture.size() <= MaxTextureBytes, OptionalSafe(Item.Particle.Effect), OptionalSafe(Item.Decal.Texture) && OptionalSafe(Item.Decal.Shader), OptionalSafe(Item.Weather.Cycle), true };
		const bool IsGood = Item.Name.size() <= MaxNameBytes && IsSmall(Item.Lanes) && IsTypeSafe[static_cast<u32>(Item.Type)] && (Item.Type == ObjectType::Camera || IsLinkSafe(Item.Attach));
		if (!IsGood)
		{
			Error.assign("\"").append(Item.Name).append("\" cannot be saved: its name is longer than 127 bytes, a model, item, pack, animation, effect, texture or bone name has unsupported characters, or it holds more than the format allows.");
			return false;
		}
	}
	return true;
}

bool PreScan(IReader& File, xr_string& Error)
{
	const auto IsName = [](const char* Begin, const char* Stop) { return Stop > Begin && Stop - Begin <= 63 && std::all_of(Begin, Stop, [](char Symbol) { return (Symbol >= 'a' && Symbol <= 'z') || (Symbol >= '0' && Symbol <= '9') || Symbol == '_'; }); };
	const auto IsBlank = [](char Symbol) { return Symbol == ' ' || Symbol == '\t'; };
	const char* Cursor = static_cast<const char*>(File.pointer()) - File.tell();
	const char* const End = Cursor + File.length();
	xr_vector<xr_string_view> Sections;
	string128 Problem = {};
	for (u32 Number = 1; Cursor < End && !Problem[0]; ++Number)
	{
		const char* Begin = Cursor;
		const char* Stop = std::find_if(Cursor, End, [](char Symbol) { return Symbol == '\r' || Symbol == '\n'; });
		Cursor = Stop == End ? End : Stop + (*Stop == '\r' && Stop + 1 < End && Stop[1] == '\n' ? 2 : 1);
		const xr_string_view Text(Begin, static_cast<size_t>(Stop - Begin));
		const bool IsQuoteSafe = Text.find('"') == xr_string_view::npos || (std::count(Begin, Stop, '"') % 2 == 0 && Text.find(';') == xr_string_view::npos && Text.find("//") == xr_string_view::npos);
		const bool HasControl = std::any_of(Begin, Stop, [](char Symbol) { return static_cast<u8>(Symbol) < 32 && Symbol != '\t'; });
		const char* Reason = Text.size() >= 4000 ? "the line is longer than 4000 bytes" : !IsQuoteSafe ? "quotes must be paired and not mixed with comments" : HasControl ? "control characters are not allowed" : nullptr;
		while (Begin < Stop && IsBlank(*Begin))
		{
			++Begin;
		}
		while (Stop > Begin && IsBlank(Stop[-1]))
		{
			--Stop;
		}
		const bool IsContent = !Reason && Begin < Stop && *Begin != ';';
		if (IsContent && *Begin == '[')
		{
			const char* Close = std::find(Begin, Stop, ']');
			const char* After = Close == Stop ? Stop : Close + 1;
			while (After < Stop && IsBlank(*After))
			{
				++After;
			}
			Reason = (Close == Stop || !IsName(Begin + 1, Close) || (After < Stop && *After != ';')) ? "a section must be [a-z0-9_] without inheritance" : nullptr;
			Sections.emplace_back(Begin + 1, static_cast<size_t>(Close - Begin - 1));
		}
		else if (IsContent)
		{
			const char* Equal = std::find(Begin, Stop, '=');
			const char* Name = Equal;
			while (Name > Begin && IsBlank(Name[-1]))
			{
				--Name;
			}
			Reason = Sections.empty() ? "content before the first section" : (Equal == Stop || !IsName(Begin, Name)) ? "a line must be key = value with a [a-z0-9_] key" : nullptr;
		}
		if (Reason)
		{
			xr_sprintf(Problem, "Line %u: %s", Number, Reason);
		}
	}
	std::sort(Sections.begin(), Sections.end());
	const auto Twin = std::adjacent_find(Sections.begin(), Sections.end());
	if (!Problem[0] && Twin != Sections.end())
	{
		xr_sprintf(Problem, "Section [%.*s] appears twice", static_cast<int>(Twin->size()), Twin->data());
	}
	Error = Problem;
	return !Problem[0];
}

template <typename TRead>
LoadResult Parse(const char* Path, const TRead& Read)
{
	LoadResult Result;
	const CReaderGuarded File = FS.rg_open(Path);
	if (!File || static_cast<size_t>(File->length()) > MaxFileSize)
	{
		Result.Error.assign(File ? "The file is larger than 16 MiB: " : "Cannot open ").append(Path);
		return Result;
	}
	if (!PreScan(*File, Result.Error))
	{
		Result.Error.append(" in ").append(Path);
		return Result;
	}
	File->seek(0);
	const CInifile Ini(&*File, nullptr);
	const char* Problem = Read(Ini, Result);
	Result.IsOk = !Problem;
	if (Problem)
	{
		Result.Error.assign(Problem).append(Path);
	}
	return Result;
}

LoadResult Load(const char* Path, Scene& Out)
{
	return Parse(Path, [&](const CInifile& Ini, LoadResult& Result) -> const char*
	{
		Scene Data;
		Data.LevelName = Out.LevelName;
		Result.SourceVersion = ReadU32(Ini, "scene", "version", ~0u, Result.Notes);
		const char* SavedLevel = Ini.read_if_exists<str_c>("scene", "level", nullptr);
		const char* CurrentLevel = Data.LevelName.size() ? Data.LevelName.c_str() : "";
		if (SavedLevel && xr_strcmp(SavedLevel, CurrentLevel))
		{
			Result.Notes.Add("The scene was saved on level %s, this is %s: positions may not match.", SavedLevel, CurrentLevel);
		}
		if (Result.SourceVersion >= 5 && Result.SourceVersion <= FormatVersion)
		{
			ReadScene(Ini, Data, Result.SourceVersion, Result.Notes);
		}
		else if (Result.SourceVersion == 0 || Result.SourceVersion > FormatVersion)
		{
			return Result.SourceVersion ? "The scene was saved by a newer editor: " : "Not a cinema scene: ";
		}
		else if (!V4::Import(Ini, static_cast<s32>(Result.SourceVersion), Data, Result.Notes))
		{
			return "Nothing could be imported from ";
		}
		Out = std::move(Data);
		return nullptr;
	});
}

LoadResult LoadPresets(const char* Path, xr_vector<ActorPreset>& Out, xr_vector<xr_string>& Hidden)
{
	return Parse(Path, [&](const CInifile& Ini, LoadResult& Result) -> const char*
	{
		Warnings& Notes = Result.Notes;
		Result.SourceVersion = ReadU32(Ini, "preset", "version", ~0u, Notes);
		if (Result.SourceVersion > PresetVersion)
		{
			return "The presets were saved by a newer editor: ";
		}
		if (Result.SourceVersion < 2)
		{
			V4::ImportPresets(Ini, Out, Notes);
			return nullptr;
		}
		string32 Section;
		const u32 Count = ReadU32(Ini, "preset", "presets", MaxPresets, Notes);
		for (u32 Index = 0; Index < Count; ++Index)
		{
			ActorPreset Item;
			xr_sprintf(Section, "preset_%03u", Index);
			Item.Name = ReadName(Ini, Section, "name", Notes);
			ReadActor(Ini, Section, Item.Setup, Result.SourceVersion < 3, Result.SourceVersion < 5 || ReadBool(Ini, Section, "relative", false, Notes), Result.SourceVersion >= 5, Notes);
			if (Item.Name.empty() || !Item.Setup.Visual.size() || std::any_of(Out.begin(), Out.end(), [&](const ActorPreset& Other) { return Other.Name == Item.Name; }))
			{
				Notes.Add("[%s] has no name or model, or repeats a name, preset skipped", Section);
				continue;
			}
			Out.push_back(std::move(Item));
		}
		const u32 HiddenCount = ReadU32(Ini, "preset", "hidden", MaxPresets, Notes);
		for (u32 Index = 0; Index < HiddenCount; ++Index)
		{
			xr_sprintf(Section, "hidden_%03u", Index);
			xr_string Name = ReadName(Ini, "preset", Section, Notes);
			if (!Name.empty())
			{
				Hidden.push_back(std::move(Name));
			}
		}
		return nullptr;
	});
}

bool Save(const Scene& Data, const char* Path, xr_string& Error)
{
	if (!CanSave(Data, Error))
	{
		return false;
	}
	CInifile Ini(nullptr, false, false, false);
	string64 Line;
	const SceneHeader& Header = Data.Header;
	Write(Ini, "scene", "version", FormatVersion);
	if (Data.LevelName.size())
	{
		Ini.w_string("scene", "level", Data.LevelName.c_str());
	}
	Write(Ini, "scene", "frame_rate", Header.FrameRate);
	if (Header.CutDistance != 5.0f || Header.CutAngle != 45.0f)
	{
		Write(Ini, "scene", "cut_jump", Header.CutDistance, Header.CutAngle);
	}
	if (Header.IsWorldFollowing)
	{
		Ini.w_bool("scene", "world_follows_camera", true);
	}
	Write(Ini, "scene", "next_id", Data.NextId);
	Write(Ini, "scene", "next_color", Header.NextColor);
	Write(Ini, "scene", "folders", static_cast<u32>(Header.Folders.size()));
	for (u32 Index = 0; Index < Header.Folders.size(); ++Index)
	{
		xr_sprintf(Line, "folder_%03u", Index);
		Write(Ini, "scene", Line, Header.Folders[Index].Id, Header.Folders[Index].Row);
		xr_sprintf(Line, "folder_%03u_name", Index);
		WriteName(Ini, "scene", Line, Header.Folders[Index].Name);
	}
	if (!Header.Markers.empty())
	{
		Write(Ini, "scene", "markers", static_cast<u32>(Header.Markers.size()));
	}
	for (u32 Index = 0; Index < Header.Markers.size(); ++Index)
	{
		xr_sprintf(Line, "marker_%03u", Index);
		Write(Ini, "scene", Line, Header.Markers[Index].Time);
		xr_sprintf(Line, "marker_%03u_name", Index);
		WriteName(Ini, "scene", Line, Header.Markers[Index].Name);
	}
	xr_vector<const Object*> Order;
	Order.reserve(Data.Objects.size());
	for (const Object& Item : Data.Objects)
	{
		Order.push_back(&Item);
	}
	const auto Rank = [&](const Object* Item)
	{
		const auto Owner = std::find_if(Header.Folders.begin(), Header.Folders.end(), [&](const Folder& Other) { return Other.Id == Item->FolderId; });
		const bool IsRoot = Owner == Header.Folders.end();
		return std::make_tuple(IsRoot ? Item->Row : Owner->Row, IsRoot ? 0u : Owner->Id, Item->Row, Item->Id);
	};
	std::sort(Order.begin(), Order.end(), [&](const Object* A, const Object* B) { return Rank(A) < Rank(B); });
	Write(Ini, "scene", "objects", static_cast<u32>(Order.size()));
	for (u32 Index = 0; Index < Order.size(); ++Index)
	{
		xr_sprintf(Line, "object_%03u", Index);
		WriteObject(Ini, Line, *Order[Index]);
		WriteSizes(Ini, Line, Data.Sizes, Order[Index]->Id);
	}
	return SaveIni(Ini, Path, Error);
}

bool SaveIni(const CInifile& Ini, const char* Path, xr_string& Error)
{
	string_path Target;
	string_path Temp;
	if (xr_strlen(Path) + 5 > sizeof(Target))
	{
		Error.assign("The path is too long: ").append(Path);
		return false;
	}
	xr_strcpy(Target, Path);
	xr_strlwr(Target);
	xr_sprintf(Temp, "%s.tmp", Target);
	size_t Size = 0;
	{
		const CWriterGuarded Writer = FS.wg_open(Temp);
		if (!Writer || !Writer->valid())
		{
			Error.assign("Cannot write ").append(Temp);
			return false;
		}
		Ini.save_as(*Writer);
		Size = Writer->tell();
	}
	const std::filesystem::path From(Platform::ANSI_TO_TCHAR_U8(Platform::ValidPath(Temp)));
	const std::filesystem::path To(Platform::ANSI_TO_TCHAR_U8(Platform::ValidPath(Target)));
	std::error_code Code;
	if (std::filesystem::file_size(From, Code) != static_cast<std::uintmax_t>(Size) || Code)
	{
		std::filesystem::remove(From, Code);
		Error.assign("The disk refused part of ").append(Temp).append("; nothing was replaced.");
		return false;
	}
	std::filesystem::rename(From, To, Code);
	if (Code)
	{
		Error.assign("Cannot replace ").append(Target).append(" (read-only or in use); it is unchanged and the new data is in ").append(Temp);
		return false;
	}
	FS.file_update(Target);
	return true;
}

bool SavePresets(const xr_vector<ActorPreset>& Presets, const xr_vector<xr_string>& Hidden, const char* Path, xr_string& Error)
{
	const auto IsBad = [](const ActorPreset& Item) { return !Item.IsOld && (Item.Name.empty() || Item.Name.size() > MaxNameBytes || !Item.Setup.Visual.size() || !ActorSafe(Item.Setup)); };
	const u32 Count = static_cast<u32>(std::count_if(Presets.begin(), Presets.end(), [](const ActorPreset& Item) { return !Item.IsOld; }));
	if (Count > MaxPresets || Hidden.size() > MaxPresets || std::any_of(Presets.begin(), Presets.end(), IsBad))
	{
		Error.assign("The presets cannot be saved to ").append(Path).append(": more than 512 presets, a name that is empty or longer than 127 bytes, no model, or a model, item, pack or animation name with unsupported characters.");
		return false;
	}
	CInifile Ini(nullptr, false, false, false);
	string32 Section;
	Write(Ini, "preset", "version", PresetVersion);
	Write(Ini, "preset", "presets", Count);
	if (!Hidden.empty())
	{
		Write(Ini, "preset", "hidden", static_cast<u32>(Hidden.size()));
	}
	for (u32 Index = 0; Index < Hidden.size(); ++Index)
	{
		xr_sprintf(Section, "hidden_%03u", Index);
		WriteName(Ini, "preset", Section, Hidden[Index]);
	}
	u32 Index = 0;
	for (const ActorPreset& Item : Presets)
	{
		if (!Item.IsOld)
		{
			xr_sprintf(Section, "preset_%03u", Index++);
			WriteName(Ini, Section, "name", Item.Name);
			WriteActor(Ini, Section, Item.Setup);
		}
	}
	return SaveIni(Ini, Path, Error);
}
}

namespace Cinema::Actors
{
void Unpack(xr_vector<ActorBlock>& Blocks, const xr_vector<float>& Blends)
{
	for (u32 Index = 1; Index < Blocks.size() && Index < Blends.size(); ++Index)
	{
		const float Blend = std::clamp(Blends[Index], 0.0f, 1.0f);
		Blocks[Index - 1].Hold = std::min(Blocks[Index - 1].Hold + Blend, MaxTime);
		Blocks[Index].Start = -Blend;
	}
}
}
