#include "stdafx.h"
#include "IniReadAhead.h"
#include "BackgroundLoad.h"

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

namespace
{
	constexpr u32 LearnedVersion = 1;
}

void CIniReadAhead::GetLearnedPath(string_path& Path)
{
	FS.update_path(Path, "$app_data_root$", "ini_read_ahead.bin");
}

xr_vector<xr_string> CIniReadAhead::ReadLearned()
{
	xr_vector<xr_string> Keys;
	string_path Path;
	GetLearnedPath(Path);
	if (!FS.exist(Path))
	{
		return Keys;
	}

	IReader* Reader = FS.r_open(Path);
	if (Reader == nullptr)
	{
		return Keys;
	}

	if (Reader->length() >= 2 * sizeof(u32) && Reader->r_u32() == LearnedVersion)
	{
		const u32 Count = Reader->r_u32();
		Keys.reserve(Count);
		for (u32 Index = 0; Index < Count && !Reader->eof(); ++Index)
		{
			Reader->r_stringZ(Keys.emplace_back());
		}
	}
	FS.r_close(Reader);
	return Keys;
}

void CIniReadAhead::WriteLearned()
{
	xr_vector<const Entry*> Taken;
	for (const xr_unique_ptr<Entry>& Item : Storage)
	{
		if (Item->TakeOrder != u32(-1))
		{
			Taken.push_back(Item.get());
		}
	}
	if (Taken.empty())
	{
		return;
	}
	std::sort(Taken.begin(), Taken.end(), [](const Entry* Left, const Entry* Right) { return Left->TakeOrder < Right->TakeOrder; });

	xr_vector<xr_string> Keys;
	xr_hash_set<xr_string> Written;
	for (const Entry* Item : Taken)
	{
		xr_string Key = MakeKey(Item->Path.c_str());
		if (Written.insert(Key).second)
		{
			Keys.push_back(std::move(Key));
		}
	}
	for (const xr_string& Key : PreviousLearned)
	{
		if (Written.insert(Key).second)
		{
			Keys.push_back(Key);
		}
	}

	string_path Path;
	GetLearnedPath(Path);
	IWriter* Writer = FS.w_open(Path);
	if (Writer == nullptr)
	{
		return;
	}
	Writer->w_u32(LearnedVersion);
	Writer->w_u32(u32(Keys.size()));
	for (const xr_string& Key : Keys)
	{
		Writer->w_stringZ(Key);
	}
	FS.w_close(Writer);
}

void CIniReadAhead::Start(xr_vector<xr_string>&& Paths)
{
	Stop();

	{
		xrSRWLockGuard Guard(Lock);
		xr_hash_map<xr_string, xr_string*> Available;
		for (xr_string& Path : Paths)
		{
			Available.emplace(MakeKey(Path.c_str()), &Path);
		}

		auto Add = [this](xr_string&& Key, xr_string& Path)
		{
			if (Entries.contains(Key))
			{
				return;
			}
			Entry* Item = Storage.emplace_back(xr_make_unique<Entry>()).get();
			Item->Path = std::move(Path);
			Entries.emplace(std::move(Key), Item);
		};

		PreviousLearned.clear();
		for (xr_string& Key : ReadLearned())
		{
			auto Iter = Available.find(Key);
			if (Iter != Available.end())
			{
				PreviousLearned.push_back(Key);
				Add(std::move(Key), *Iter->second);
			}
		}

		if (Storage.empty())
		{
			Storage.reserve(Paths.size());
			for (xr_string& Path : Paths)
			{
				Add(MakeKey(Path.c_str()), Path);
			}
		}
	}

	TakenCount = 0;
	OnDemandCount = 0;
	IsActive.store(true, std::memory_order_release);

	Next.store(0, std::memory_order_relaxed);
	const size_t Workers = std::min<size_t>(BackgroundLoad::WorkerCount(), Storage.size());
	for (size_t Worker = 0; Worker < Workers; ++Worker)
	{
		Tasks.run([this]()
		{
			xrBackgroundWorkScope Background;
			for (size_t Index = Next.fetch_add(1); Index < Storage.size(); Index = Next.fetch_add(1))
			{
				if (!IsActive.load(std::memory_order_acquire))
				{
					return;
				}

				Entry& Item = *Storage[Index];
				EState Expected = EState::Queued;
				if (Item.State.compare_exchange_strong(Expected, EState::Parsing))
				{
					Parse(Item);
				}
			}
		});
	}
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
	WriteLearned();
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
		Item.State.wait(EState::Parsing, std::memory_order_acquire);
	}

	Expected = EState::Ready;
	if (!Item.State.compare_exchange_strong(Expected, EState::Taken))
	{
		return false;
	}

	Target.swap(Item.Data);
	Item.TakeOrder = TakenCount++;
	return true;
}
