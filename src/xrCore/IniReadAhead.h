#pragma once
#include "xr_ini.h"

class XRCORE_API CIniReadAhead final
{
public:
	CIniReadAhead() = default;
	~CIniReadAhead();

	CIniReadAhead(const CIniReadAhead&) = delete;
	CIniReadAhead& operator=(const CIniReadAhead&) = delete;

	void Start(xr_vector<xr_string>&& Paths);
	void Stop();
	bool Take(const char* Path, CInifile::Root& Target);

private:
	enum class EState : u8
	{
		Queued,
		Parsing,
		Ready,
		Taken
	};

	struct Entry
	{
		xr_string				Path;
		std::atomic<EState>		State = EState::Queued;
		CInifile::Root			Data;
	};

	static xr_string MakeKey(const char* Path);
	static void Parse(Entry& Item);

	xrSRWLock								Lock;
	xr_hash_map<xr_string, Entry*>			Entries;
	xr_vector<xr_unique_ptr<Entry>>			Storage;
	xr_task_group							Tasks;
	std::atomic<bool>						IsActive = false;
	std::atomic<size_t>						Next = 0;
	std::atomic<u32>						TakenCount = 0;
	std::atomic<u32>						OnDemandCount = 0;
};

extern XRCORE_API CIniReadAhead* g_IniReadAhead;
