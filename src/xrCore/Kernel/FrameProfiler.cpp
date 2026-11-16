#include "stdafx.h"

#ifdef DEBUG_DRAW
XRCORE_API CFrameProfiler g_FrameProfiler;

struct ProfilerThread
{
	u32 Track = CFrameProfiler::None;
	u32 Depth = 0;
	u32 Skipped = 0;
	u32 Nodes[CFrameProfiler::MaxDepth];
	u64 Start[CFrameProfiler::MaxDepth];
};

static thread_local ProfilerThread CurrentThread;

void CFrameProfiler::SetEnabled(bool Value)
{
	if (Value && !Enabled.load(std::memory_order_relaxed))
	{
		memset(FrameBegin, 0, sizeof(FrameBegin));
		memset(FrameEnd, 0, sizeof(FrameEnd));
	}

	Enabled.store(Value, std::memory_order_relaxed);
}

void CFrameProfiler::Frame()
{
	const bool CollectFrame = Enabled.load(std::memory_order_relaxed);
	const u64 Now = CollectFrame ? CPU::QPC() : 0;
	// The previous frame's work has joined before the primary thread advances the history.
	FlushScriptMarkers();
	SetThreadName("Primary");
	if (!CollectFrame)
	{
		return;
	}

	const u64 Index = FrameIndex.load(std::memory_order_relaxed);
	const u32 Next = u32((Index + 1) % HistorySize);
	const u32 Count = NodeCount();
	FrameEnd[Index % HistorySize] = Now;
	memset(Ticks[Next], 0, Count * sizeof(u64));
	memset(Calls[Next], 0, Count * sizeof(u32));
	FrameBegin[Next] = Now;
	FrameEnd[Next] = 0;
	FrameIndex.store(Index + 1, std::memory_order_release);
}

void CFrameProfiler::Begin(const char* Name)
{
	ProfilerThread& Thread = CurrentThread;
	if (Thread.Track == None)
	{
		Thread.Track = AddTrack(nullptr);
	}

	u32 Node = None;
	if (Thread.Skipped == 0 && Thread.Track < MaxTracks && Thread.Depth < MaxDepth)
	{
		Node = Child(Thread.Depth != 0 ? Thread.Nodes[Thread.Depth - 1] : Tracks[Thread.Track].Root, Name, false);
	}

	if (Node == None)
	{
		Thread.Skipped++;
		return;
	}

	Thread.Nodes[Thread.Depth] = Node;
	Thread.Start[Thread.Depth++] = CPU::QPC();
}

void CFrameProfiler::End()
{
	ProfilerThread& Thread = CurrentThread;
	if (Thread.Skipped != 0)
	{
		Thread.Skipped--;
		return;
	}

	Thread.Depth--;
	Add(Thread.Nodes[Thread.Depth], CPU::QPC() - Thread.Start[Thread.Depth]);
}

void CFrameProfiler::SetThreadName(const char* Name)
{
	ProfilerThread& Thread = CurrentThread;
	if (Thread.Track == None)
	{
		Thread.Track = AddTrack(Name);
	}
	else if (Thread.Track < MaxTracks)
	{
		xr_strcpy(Tracks[Thread.Track].Name, Name);
	}
}

u32 CFrameProfiler::AddTrack(const char* Name)
{
	xrCriticalSectionGuard Guard(TrackLock);
	const u32 TrackId = TracksUsed.load(std::memory_order_relaxed);
	const u32 Root = TrackId < MaxTracks && NodesUsed.load(std::memory_order_relaxed) < MaxNodes ? NodesUsed.fetch_add(1) : MaxNodes;
	if (Root >= MaxNodes)
	{
		return MaxTracks;
	}

	Track& Item = Tracks[TrackId];
	if (Name != nullptr)
	{
		xr_strcpy(Item.Name, Name);
	}
	else
	{
		xr_sprintf(Item.Name, "Thread %u", TrackId);
	}

	Item.Root = Root;
	Nodes[Root] = { Item.Name, None, None, None, TrackId, 0 };
	TracksUsed.store(TrackId + 1, std::memory_order_release);
	return TrackId;
}

void CFrameProfiler::Record(u32 TrackId, u32 Depth, const char* Name, u64 Duration)
{
	if (TrackId >= MaxTracks || Depth >= MaxDepth)
	{
		return;
	}

	Track& Item = Tracks[TrackId];
	const u32 Parent = Depth != 0 ? Item.Stack[Depth - 1] : Item.Root;
	const u32 Node = Parent != None ? Child(Parent, Name, true) : None;
	Item.Stack[Depth] = Node;
	if (Node != None)
	{
		Add(Node, Duration);
	}
}

u32 CFrameProfiler::RegisterScriptMarker(const char* Name)
{
	if (Name == nullptr || *Name == '\0')
	{
		return None;
	}

	xrCriticalSectionGuard Guard(ScriptMarkerLock);
	const u32 Count = ScriptMarkersUsed.load(std::memory_order_relaxed);
	for (u32 Index = 0; Index < Count; Index++)
	{
		if (xr_strcmp(ScriptMarkers[Index].Name, Name) == 0)
		{
			return Index;
		}
	}

	const size_t Length = strlen(Name) + 1;
	if (Count >= MaxNodes || Length > sizeof(ScriptNames) - ScriptNamesUsed)
	{
		return None;
	}

	char* Stored = ScriptNames + ScriptNamesUsed;
	memcpy(Stored, Name, Length);
	ScriptNamesUsed += u32(Length);
	ScriptMarkers[Count].Name = Stored;
	ScriptMarkersUsed.store(Count + 1, std::memory_order_release);
	return Count;
}

u32 CFrameProfiler::ScriptSampleIndex(double Ticket) const
{
	// Tickets are exact Lua numbers and distinguish the slots reused in each frame.
	const double Offset = Ticket - double(FirstScriptTicket);
	if (Offset >= 0 && Offset < MaxScriptSamples && Offset < ScriptSamplesUsed.load(std::memory_order_relaxed))
	{
		return u32(Offset);
	}
	return None;
}

double CFrameProfiler::BeginScriptMarker(u32 Marker, double ParentTicket)
{
	if (!Enabled.load(std::memory_order_relaxed) ||
		Marker >= ScriptMarkersUsed.load(std::memory_order_acquire) ||
		ScriptSamplesUsed.load(std::memory_order_relaxed) >= MaxScriptSamples)
	{
		return -1;
	}

	const u32 Parent = ScriptSampleIndex(ParentTicket);
	const u32 Depth = Parent != None ? ScriptSamples[Parent].Depth + 1 : 0;
	if (Depth >= MaxDepth)
	{
		return -1;
	}

	const u32 Index = ScriptSamplesUsed.fetch_add(1, std::memory_order_relaxed);
	if (Index >= MaxScriptSamples)
	{
		return -1;
	}

	ScriptSamples[Index] = { Marker, Parent, Depth, false, CPU::QPC(), 0 };
	return double(FirstScriptTicket + Index);
}

void CFrameProfiler::EndScriptMarker(double Ticket)
{
	const u32 Index = ScriptSampleIndex(Ticket);
	if (Index != None && !ScriptSamples[Index].Finished)
	{
		ScriptSample& Sample = ScriptSamples[Index];
		Sample.Duration = CPU::QPC() - Sample.Start;
		Sample.Finished = true;
	}
}

void CFrameProfiler::FlushScriptMarkers()
{
	const u32 Count = std::min(ScriptSamplesUsed.load(std::memory_order_relaxed), MaxScriptSamples);
	if (Count == 0)
	{
		return;
	}

	ScriptSamplesUsed.store(0, std::memory_order_relaxed);
	FirstScriptTicket += MaxScriptSamples;
	const u32 Slot = u32(FrameIndex.load(std::memory_order_relaxed) % HistorySize);
	if (!Enabled.load(std::memory_order_relaxed) || FrameBegin[Slot] == 0)
	{
		return;
	}

	u32 SampleNodes[MaxScriptSamples];
	for (u32 Index = 0; Index < Count; Index++)
	{
		const ScriptSample& Sample = ScriptSamples[Index];
		const u32 ParentNode = Sample.Parent != None ? SampleNodes[Sample.Parent] : None;
		SampleNodes[Index] = ParentNode;
		if (!Sample.Finished)
		{
			continue;
		}

		if (ScriptTrack == None)
		{
			ScriptTrack = AddTrack("Lua");
		}
		if (ScriptTrack >= MaxTracks)
		{
			break;
		}

		const u32 Parent = ParentNode != None ? ParentNode : Tracks[ScriptTrack].Root;
		ScriptMarker& Marker = ScriptMarkers[Sample.Marker];
		if (Marker.LastParent != Parent || Marker.LastNode == None)
		{
			Marker.LastParent = Parent;
			Marker.LastNode = Child(Parent, Marker.Name, false);
		}
		if (Marker.LastNode != None)
		{
			SampleNodes[Index] = Marker.LastNode;
			Add(Marker.LastNode, Sample.Duration);
		}
	}
}

u32 CFrameProfiler::NodeCount() const
{
	return std::min(NodesUsed.load(std::memory_order_acquire), MaxNodes);
}

u32 CFrameProfiler::TrackCount() const
{
	return TracksUsed.load(std::memory_order_acquire);
}

u32 CFrameProfiler::Child(u32 Parent, const char* Name, bool Copy)
{
	u32 Last = None;
	for (u32 Index = Nodes[Parent].FirstChild; Index != None; Index = Nodes[Index].NextSibling)
	{
		if (Nodes[Index].Name == Name || (Copy && xr_strcmp(Nodes[Index].Name, Name) == 0))
		{
			return Index;
		}

		Last = Index;
	}

	const u32 Length = Copy ? xr_strlen(Name) + 1 : 0;
	if ((Copy && NamesUsed + Length > sizeof(Names)) || NodesUsed.load(std::memory_order_relaxed) >= MaxNodes)
	{
		return None;
	}

	const u32 Index = NodesUsed.fetch_add(1);
	if (Index >= MaxNodes)
	{
		return None;
	}

	const char* Stored = Copy ? static_cast<const char*>(memcpy(Names + NamesUsed, Name, Length)) : Name;
	NamesUsed += Length;
	Nodes[Index] = { Stored, Parent, None, None, Nodes[Parent].Track, Nodes[Parent].Depth + 1 };
	std::atomic_thread_fence(std::memory_order_release);
	(Last == None ? Nodes[Parent].FirstChild : Nodes[Last].NextSibling) = Index;
	return Index;
}

void CFrameProfiler::Add(u32 Node, u64 Duration)
{
	const u32 Slot = u32(FrameIndex.load(std::memory_order_relaxed) % HistorySize);
	Ticks[Slot][Node] += Duration;
	Calls[Slot][Node]++;
}
#endif
