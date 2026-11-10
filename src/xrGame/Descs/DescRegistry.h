#pragma once

// Shared read-only data built from ltx once per section ("Default Object").
// TDesc must provide: void Load(const shared_str& Section);
template <typename TDesc>
class TDescRegistry final
{
public:
	static const TDesc& Get(const shared_str& Section)
	{
		{
			xrSRWLockGuard Guard(Lock, true);

			auto It = Storage.find(Section);
			if (It != Storage.end())
			{
				return *It->second;
			}
		}

		xrSRWLockGuard Guard(Lock);

		auto It = Storage.find(Section);
		if (It != Storage.end())
		{
			return *It->second;
		}

		xr_unique_ptr<TDesc> Desc = xr_make_unique<TDesc>();
		Desc->Load(Section);
		return *Storage.emplace(Section, std::move(Desc)).first->second;
	}

	static void Clear()
	{
		xrSRWLockGuard Guard(Lock);
		Storage.clear();
	}

	// The caller must exclude frame readers. Keep descriptor addresses stable.
	static void ReloadCached()
	{
		xr_vector<shared_str> Sections;
		{
			xrSRWLockGuard Guard(Lock, true);
			for (const auto& [Section, Desc] : Storage)
			{
				Sections.push_back(Section);
			}
		}
		for (const auto& Section : Sections)
		{
			TDesc Replacement;
			Replacement.Load(Section);
			xrSRWLockGuard Guard(Lock);
			*Storage.at(Section) = std::move(Replacement);
		}
	}

private:
	inline static xr_hash_map<shared_str, xr_unique_ptr<TDesc>> Storage;
	inline static xrSRWLock Lock;
};

void ClearDescRegistries();
