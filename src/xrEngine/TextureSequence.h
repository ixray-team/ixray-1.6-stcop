#pragma once

#include "SequenceSectionRegistry.h"

class IReader;

// Parsed once at resource load. Playback owns no file handles or GPU resources.
class ENGINE_API TextureSequence final : public ISequenceSectionListener
{
	struct Frame
	{
		u32 Resource;
		u64 EndMs;
	};
	struct Section
	{
		shared_str Name;
		xr_vector<Frame> Frames;
		u64 DurationMs = 0;
		s32 Next = -1;
		shared_str NextName;
		bool Loop = false;
	};
	xr_vector<shared_str> Resources;
	xr_vector<u32> DefaultFrames;
	xr_vector<Section> Sections;
	xr_map<xr_string, u32> ResourceLookup;
	u32 DefaultMs = 1;
	bool IsCycled = false;
	bool IsRegistered = false;
	xrCriticalSection CommandGuard;
	shared_str PendingName;
	xr_atomic_bool HasPending = false;
	bool PendingWait = false;
	shared_str PendingAfterName;
	bool HasPendingAfter = false;
	shared_str WaitingName;
	bool HasWaiting = false;
	s32 Current = -1;
	u64 ElapsedMs = 0;
	u32 LastTime = 0;
	bool HasClock = false;
	bool HasLocalDefaultClock = false;
	bool IsHolding = false;

	u32 AddResource(const xr_string& Name);
	bool AddFrames(Section& Target, const xr_string& Value, bool IsRange, const char* Source);
	s32 FindSection(const char* Name) const;
	void Switch(const shared_str& Name, bool PreserveTime);
	u64 DefaultDuration() const;

public:
	~TextureSequence() override;
	bool Load(IReader& Reader, const char* Source);
	const xr_vector<shared_str>& GetResources() const { return Resources; }
	u32 GetFrame(u32 Now);
	void QueueSequenceSection(const shared_str& Name, bool WaitForEnd) override;
	u64 GetSequenceSectionDuration(const char* Name) const override;
};
