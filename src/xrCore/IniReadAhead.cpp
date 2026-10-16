#include "stdafx.h"
#include "IniReadAhead.h"

XRCORE_API CIniReadAhead* g_IniReadAhead = nullptr;

CIniReadAhead::~CIniReadAhead()
{
	Stop();
}

xr_string CIniReadAhead::MakeKey(const char* Path)
{
	xr_string Key = Path;
	for (char& Symbol : Key)
	{
		Symbol = (Symbol == '/') ? '\\' : char(tolower(u8(Symbol)));
	}
	return Key;
}

void CIniReadAhead::Parse(Entry& Item)
{
	PROF_EVENT("CIniReadAhead::Parse");
	CInifile Ini(Item.Path.c_str(), true, false, false);
	if (IReader* Reader = FS.r_open(Item.Path.c_str()))
	{
		string_path Folder, Directory;
		_splitpath(Item.Path.c_str(), Folder, Directory, nullptr, nullptr);
		xr_strcat(Folder, sizeof(Folder), Directory);
		Ini.Load(Reader, Folder, nullptr);
		FS.r_close(Reader);
	}
	Item.Data.swap(Ini.DATA);

	Item.State.store(EState::Ready, std::memory_order_release);
	Item.State.notify_all();
}

void CIniReadAhead::Start(xr_vector<xr_string>&& Paths)
{
	Stop();

	{
		xrSRWLockGuard Guard(Lock);
		Storage.reserve(Paths.size());
		for (xr_string& Path : Paths)
		{
			xr_string Key = MakeKey(Path.c_str());
			if (Entries.contains(Key))
			{
				continue;
			}

			Entry* Item = Storage.emplace_back(xr_make_unique<Entry>()).get();
			Item->Path = std::move(Path);
			Entries.emplace(std::move(Key), Item);
		}
	}

	TakenCount = 0;
	OnDemandCount = 0;
	IsActive.store(true, std::memory_order_release);

	Tasks.run([this]()
	{
		xr_parallel_for(size_t(0), Storage.size(), [this](size_t Index)
		{
			if (!IsActive.load(std::memory_order_acquire))
			{
				return;
			}

			Entry& Item = *Storage[Index];
			EState Expected = EState::Queued;
			if (Item.State.compare_exchange_strong(Expected, EState::Parsing))
			{
				xrTaskBlockingScope Blocking;
				Parse(Item);
			}
		});
	});
}

void CIniReadAhead::Stop()
{
	if (!IsActive.exchange(false))
	{
		return;
	}

	PROF_EVENT("CIniReadAhead::Stop");
	Tasks.wait();

	xrSRWLockGuard Guard(Lock);
	Msg("* Ini read-ahead: %u files, %u taken, %u parsed on demand", u32(Storage.size()), TakenCount.load(), OnDemandCount.load());
	Entries.clear();
	Storage.clear();
}

bool CIniReadAhead::Take(const char* Path, CInifile::Root& Target)
{
	if (Path == nullptr || !IsActive.load(std::memory_order_acquire))
	{
		return false;
	}

	xrSRWLockGuard Guard(Lock, true);
	auto Iter = Entries.find(MakeKey(Path));
	if (Iter == Entries.end())
	{
		return false;
	}

	Entry& Item = *Iter->second;
	EState Expected = EState::Queued;
	if (Item.State.compare_exchange_strong(Expected, EState::Parsing))
	{
		++OnDemandCount;
		Parse(Item);
	}
	else if (Expected == EState::Parsing)
	{
		xrTaskBlockingScope Blocking;
		Item.State.wait(EState::Parsing, std::memory_order_acquire);
	}

	Expected = EState::Ready;
	if (!Item.State.compare_exchange_strong(Expected, EState::Taken))
	{
		return false;
	}

	Target.swap(Item.Data);
	++TakenCount;
	return true;
}
