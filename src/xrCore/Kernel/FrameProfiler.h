#pragma once

#ifdef DEBUG_DRAW
class XRCORE_API CFrameProfiler
{
public:
	static constexpr u32 HistorySize = 256;
	static constexpr u32 MaxNodes = 4096;
	static constexpr u32 MaxTracks = 64;
	static constexpr u32 MaxDepth = 32;
	static constexpr u32 None = u32(-1);

	struct Node
	{
		const char* Name;
		u32 Parent;
		u32 FirstChild;
		u32 NextSibling;
		u32 Track;
		u32 Depth;
	};

	struct Track
	{
		char Name[64];
		u32 Root;
		u32 Stack[MaxDepth];
	};

	u64 FrameBegin[HistorySize];
	u64 FrameEnd[HistorySize];
	u64 Ticks[HistorySize][MaxNodes];
	u32 Calls[HistorySize][MaxNodes];
	Node Nodes[MaxNodes];
	Track Tracks[MaxTracks];
	xr_atomic_u64 FrameIndex = 0;
	xr_atomic_bool Enabled = false;

	void SetEnabled(bool Value);
	void Frame();
	void Begin(const char* Name);
	void End();
	void SetThreadName(const char* Name);
	u32 AddTrack(const char* Name);
	void Record(u32 TrackId, u32 Depth, const char* Name, u64 Duration);
	u32 NodeCount() const;
	u32 TrackCount() const;

private:
	xrCriticalSection TrackLock;
	xr_atomic_u32 NodesUsed = 0;
	xr_atomic_u32 TracksUsed = 0;
	char Names[64 * 1024];
	u32 NamesUsed = 0;

	u32 Child(u32 Parent, const char* Name, bool Copy);
	void Add(u32 Node, u64 Duration);
};

extern XRCORE_API CFrameProfiler g_FrameProfiler;

class CProfileScope
{
	bool Active;

public:
	CProfileScope(const char* Name) : Active(g_FrameProfiler.Enabled.load(std::memory_order_relaxed))
	{
		if (Active)
		{
			g_FrameProfiler.Begin(Name);
		}
	}

	~CProfileScope()
	{
		if (Active)
		{
			g_FrameProfiler.End();
		}
	}
};
#endif
