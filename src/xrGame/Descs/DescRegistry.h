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

private:
	inline static xr_hash_map<shared_str, xr_unique_ptr<TDesc>> Storage;
	inline static xrSRWLock Lock;
};

void ClearDescRegistries();
