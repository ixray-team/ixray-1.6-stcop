#include "stdafx.h"
#include "TextureSequence.h"

#include <charconv>

namespace SequenceText
{
	static xr_string Trim(const xr_string& Text)
	{
		const size_t Start = Text.find_first_not_of(" \t\r\n");
		return Start == xr_string::npos ? xr_string() : Text.substr(Start, Text.find_last_not_of(" \t\r\n") - Start + 1);
	}

	static bool Number(const xr_string& Text, u32& Value)
	{
		const xr_string Clean = Trim(Text);
		if (Clean.empty())
		{
			return false;
		}
		const auto Result = std::from_chars(Clean.data(), Clean.data() + Clean.size(), Value);
		return Result.ec == std::errc() && Result.ptr == Clean.data() + Clean.size();
	}
}

TextureSequence::~TextureSequence()
{
	if (IsRegistered)
	{
		Engine.SequenceSections.Unregister(*this);
	}
}

u32 TextureSequence::AddResource(const xr_string& Name)
{
	const auto It = ResourceLookup.find(Name);
	if (It != ResourceLookup.end())
	{
		return It->second;
	}
	const u32 Index = static_cast<u32>(Resources.size());
	Resources.push_back(Name.c_str());
	ResourceLookup.emplace(Name, Index);
	return Index;
}

s32 TextureSequence::FindSection(const char* Name) const
{
	for (size_t Index = 0; Index < Sections.size(); ++Index)
	{
		if (xr_strcmp(*Sections[Index].Name, Name) == 0)
		{
			return static_cast<s32>(Index);
		}
	}
	return -1;
}

bool TextureSequence::AddFrames(Section& Target, const xr_string& Value, bool IsRange, const char* Source)
{
	const size_t Comma = Value.rfind(',');
	u32 Duration = 0;
	if (Comma == xr_string::npos || !SequenceText::Number(Value.substr(Comma + 1), Duration) || Duration == 0)
	{
		Msg("! SEQ %s [%s]: expected texture, positive milliseconds", Source, *Target.Name);
		return false;
	}
	const xr_string Pattern = SequenceText::Trim(Value.substr(0, Comma));
	if (Pattern.empty())
	{
		return false;
	}
	const auto AppendLambda = [&](const xr_string& Name)
	{
		Target.DurationMs += Duration;
		Target.Frames.push_back({AddResource(Name), Target.DurationMs});
	};
	if (!IsRange)
	{
		AppendLambda(Pattern);
		return true;
	}
	const size_t Open = Pattern.find('{');
	const size_t Close = Pattern.find('}', Open);
	if (Open == xr_string::npos || Close == xr_string::npos || Pattern.find('{', Open + 1) != xr_string::npos)
	{
		Msg("! SEQ %s [%s]: expected one {start..end[..step]} range", Source, *Target.Name);
		return false;
	}
	const xr_string Range = Pattern.substr(Open + 1, Close - Open - 1);
	const size_t Separator = Range.find("..");
	const size_t StepSeparator = Separator == xr_string::npos ? xr_string::npos : Range.find("..", Separator + 2);
	u32 First = 0;
	u32 Last = 0;
	u32 Step = 1;
	if (Separator == xr_string::npos || !SequenceText::Number(Range.substr(0, Separator), First) ||
		!SequenceText::Number(Range.substr(Separator + 2, StepSeparator == xr_string::npos ? xr_string::npos : StepSeparator - Separator - 2), Last) ||
		(StepSeparator != xr_string::npos && !SequenceText::Number(Range.substr(StepSeparator + 2), Step)) || Step == 0 || Last < First ||
		(static_cast<u64>(Last) - First) / Step > 1000000)
	{
		Msg("! SEQ %s [%s]: invalid or excessively large frame range", Source, *Target.Name);
		return false;
	}
	for (u64 Index = First; Index <= Last; Index += Step)
	{
		AppendLambda(Pattern.substr(0, Open) + xr_string::ToString(static_cast<u32>(Index)) + Pattern.substr(Close + 1));
	}
	return true;
}

bool TextureSequence::Load(IReader& Reader, const char* Source)
{
	string4096 Buffer;
	Reader.r_string(Buffer, sizeof(Buffer));
	IsCycled = _stricmp(Buffer, "cycled") == 0;
	if (IsCycled)
	{
		Reader.r_string(Buffer, sizeof(Buffer));
	}
	// Preserve the legacy FPS header and its integer milliseconds-per-frame calculation.
	const int Fps = atoi(Buffer);
	if (Fps <= 0)
	{
		Msg("! SEQ %s: invalid legacy FPS header", Source);
		return false;
	}
	DefaultMs = std::max(1u, 1000u / static_cast<u32>(Fps));
	s32 Parsing = -1;
	bool HasSections = false;
	bool IsValid = true;
	while (!Reader.eof())
	{
		Reader.r_string(Buffer, sizeof(Buffer));
		xr_string Line = SequenceText::Trim(Buffer);
		if (Line.empty())
		{
			continue;
		}
		if (Line.front() == '[')
		{
			HasSections = true;
			const size_t Close = Line.find(']');
			const xr_string Name = Close == xr_string::npos ? xr_string() : SequenceText::Trim(Line.substr(1, Close - 1));
			const xr_string Tail = Close == xr_string::npos ? xr_string() : SequenceText::Trim(Line.substr(Close + 1));
			if (Name.empty() || Name == "default" || FindSection(Name.c_str()) >= 0 || (!Tail.empty() && Tail.front() != ';'))
			{
				Msg("! SEQ %s: invalid, reserved or duplicate section: %s", Source, Line.c_str());
				IsValid = false;
				break;
			}
			Parsing = static_cast<s32>(Sections.size());
			Sections.push_back({});
			Sections.back().Name = Name.c_str();
			continue;
		}
		if (!HasSections)
		{
			// Outside sections every non-empty line is a legacy texture name, unchanged.
			DefaultFrames.push_back(AddResource(Line));
			continue;
		}
		const size_t Comment = Line.find(';');
		Line = SequenceText::Trim(Line.substr(0, Comment));
		if (Line.empty())
		{
			continue;
		}
		const size_t Equal = Line.find('=');
		const xr_string Key = SequenceText::Trim(Line.substr(0, Equal));
		const xr_string Value = Equal == xr_string::npos ? xr_string() : SequenceText::Trim(Line.substr(Equal + 1));
		Section& Target = Sections[Parsing];
		if (Key == "next" && !Value.empty())
		{
			Target.NextName = Value.c_str();
		}
		else if (Key == "loop" && (Value == "true" || Value == "false"))
		{
			Target.Loop = Value == "true";
		}
		else if (Key == "frame" || Key == "frames")
		{
			IsValid = AddFrames(Target, Value, Key == "frames", Source) && IsValid;
		}
		else
		{
			Msg("! SEQ %s [%s]: unknown or malformed entry: %s", Source, *Target.Name, Line.c_str());
			IsValid = false;
		}
	}
	if (DefaultFrames.empty())
	{
		Msg("! SEQ %s: missing legacy default frames", Source);
		return false;
	}
	for (auto& Entry : Sections)
	{
		if (Entry.Loop)
		{
			if (Entry.NextName.size() && Entry.NextName != Entry.Name)
			{
				Msg("! SEQ %s [%s]: loop=true conflicts with next", Source, *Entry.Name);
				IsValid = false;
			}
			Entry.NextName = Entry.Name;
		}
		if (Entry.Frames.empty())
		{
			Msg("! SEQ %s [%s]: empty section", Source, *Entry.Name);
			IsValid = false;
		}
		if (Entry.NextName.size())
		{
			Entry.Next = xr_strcmp(*Entry.NextName, "default") == 0 ? -1 : FindSection(*Entry.NextName);
			if (Entry.Next < 0 && xr_strcmp(*Entry.NextName, "default") != 0)
			{
				Msg("! SEQ %s [%s]: unknown next section %s", Source, *Entry.Name, *Entry.NextName);
				IsValid = false;
			}
		}
	}
	// Invalid extensions fall back to the original block, without loading unused section textures.
	if (!IsValid)
	{
		Sections.clear();
		xr_vector<shared_str> Original;
		for (const u32 Index : DefaultFrames)
		{
			Original.push_back(Resources[Index]);
		}
		Resources.clear();
		ResourceLookup.clear();
		DefaultFrames.clear();
		for (const auto& Name : Original)
		{
			DefaultFrames.push_back(AddResource(*Name));
		}
	}
	xr_map<xr_string, u32>().swap(ResourceLookup);
	if (!Sections.empty())
	{
		xr_vector<shared_str> Names;
		for (const auto& Entry : Sections)
		{
			Names.push_back(Entry.Name);
		}
		Engine.SequenceSections.Register(*this, Names);
		IsRegistered = true;
	}
	return true;
}

void TextureSequence::QueueSequenceSection(const shared_str& Name, bool WaitForEnd)
{
	xrCriticalSectionGuard Lock(CommandGuard);
	if (WaitForEnd && HasPending.load(std::memory_order_relaxed) && !PendingWait)
	{
		// Preserve an immediate start followed by a deferred command in the same game frame.
		PendingAfterName = Name;
		HasPendingAfter = true;
		return;
	}
	PendingName = Name;
	PendingWait = WaitForEnd;
	HasPendingAfter = false;
	HasPending.store(true, std::memory_order_release);
}

u64 TextureSequence::GetSequenceSectionDuration(const char* Name) const
{
	s32 Index = FindSection(Name);
	u64 Duration = 0;
	for (size_t Count = 0; Index >= 0 && Count <= Sections.size(); ++Count)
	{
		const Section& Entry = Sections[Index];
		Duration += Entry.DurationMs;
		if (!Entry.NextName.size() || Entry.Next < 0)
		{
			return Duration;
		}
		Index = Entry.Next;
	}
	return Index >= 0 ? std::numeric_limits<u64>::max() : 0;
}

void TextureSequence::Switch(const shared_str& Name, bool PreserveTime)
{
	Current = xr_strcmp(*Name, "default") == 0 ? -1 : FindSection(*Name);
	if (!PreserveTime)
	{
		ElapsedMs = 0;
	}
	HasLocalDefaultClock = true;
	IsHolding = false;
}

u64 TextureSequence::DefaultDuration() const
{
	return static_cast<u64>(DefaultFrames.size()) * DefaultMs * (IsCycled ? 2 : 1);
}

u32 TextureSequence::GetFrame(u32 Now)
{
	const u32 Delta = HasClock ? Now - LastTime : 0;
	LastTime = Now;
	HasClock = true;
	ElapsedMs = Current < 0 && !HasLocalDefaultClock ? Now % DefaultDuration() : ElapsedMs + (IsHolding ? 0 : Delta);
	if (HasPending.load(std::memory_order_acquire))
	{
		xrCriticalSectionGuard Lock(CommandGuard);
		if (HasPending.load(std::memory_order_relaxed))
		{
			WaitingName = PendingName;
			HasWaiting = PendingWait;
			if (!PendingWait || IsHolding)
			{
				Switch(PendingName, false);
				HasWaiting = false;
			}
			if (HasPendingAfter)
			{
				WaitingName = PendingAfterName;
				HasWaiting = true;
				HasPendingAfter = false;
			}
			// A deferred request must advance beyond the current legacy loop boundary.
			HasLocalDefaultClock = true;
			HasPending.store(false, std::memory_order_release);
		}
	}
	// The command is consumed once. No registry lookup or file access occurs during playback.
	if (Current < 0)
	{
		const u64 Duration = DefaultDuration();
		if (HasWaiting && ElapsedMs >= Duration)
		{
			ElapsedMs -= Duration;
			Switch(WaitingName, true);
			HasWaiting = false;
		}
		else
		{
			ElapsedMs %= Duration;
		}
	}
	// A visited-state table skips complete cycles after long stalls, preserving the remainder.
	xr_vector<std::pair<s32, u64>> Visited;
	while (Current >= 0 && ElapsedMs >= Sections[Current].DurationMs)
	{
		const auto It = std::find_if(Visited.begin(), Visited.end(), [this](const auto& Entry) { return Entry.first == Current; });
		if (It != Visited.end())
		{
			const u64 CycleMs = It->second - ElapsedMs;
			if (CycleMs > 0)
			{
				ElapsedMs %= CycleMs;
				Visited.clear();
				continue;
			}
		}
		Visited.emplace_back(Current, ElapsedMs);
		const Section& Entry = Sections[Current];
		ElapsedMs -= Entry.DurationMs;
		if (HasWaiting)
		{
			Switch(WaitingName, true);
			HasWaiting = false;
			Visited.clear();
		}
		else if (Entry.NextName.size())
		{
			Current = Entry.Next;
			HasLocalDefaultClock = true;
		}
		else
		{
			ElapsedMs = Entry.DurationMs - 1;
			IsHolding = true;
			break;
		}
	}
	if (Current < 0)
	{
		ElapsedMs %= DefaultDuration();
		u32 Index = static_cast<u32>(ElapsedMs / DefaultMs);
		if (IsCycled && Index >= DefaultFrames.size())
		{
			Index = static_cast<u32>(DefaultFrames.size() * 2 - 1) - Index;
		}
		return DefaultFrames[Index];
	}
	const auto& Frames = Sections[Current].Frames;
	const auto It = std::upper_bound(Frames.begin(), Frames.end(), ElapsedMs, [](u64 Time, const Frame& Entry) { return Time < Entry.EndMs; });
	return (It == Frames.end() ? Frames.back() : *It).Resource;
}
