#pragma once

class IECSComponentStorage
{
public:
	virtual ~IECSComponentStorage() = default;
	virtual void DestroyAll() = 0;
	virtual void Destroy(void* Owner) = 0;
	virtual const char* ECS_GetName() const = 0;
};

template <typename T>
class CECSComponentStorage final :
	public IECSComponentStorage
{
public:
	T& Create(void* Owner)
	{
		xrSRWLockGuard guard(RWMutex, false);
		size_t Index = Components.size();
		T& NewComponent = Components.emplace_back();

		Owners.emplace_back(Owner);
		Lookup[Owner] = Index;

		return NewComponent;
	}

	T* Get(void* Owner)
	{
		xrSRWLockGuard guard(RWMutex, true);
		auto Iter = Lookup.find(Owner);
		return Iter != Lookup.end() ? &Components[Iter->second] : nullptr;
	}

	virtual void Destroy(void* Owner) override
	{
		xrSRWLockGuard guard(RWMutex, false);
		auto Iter = Lookup.find(Owner);
		if (Iter == Lookup.end())
		{
			return;
		}

		size_t Index = Iter->second;
		size_t Last = Components.size() - 1;

		Components[Index] = std::move(Components[Last]);
		Owners[Index] = Owners[Last];
		Lookup[Owners[Index]] = Index;

		Components.pop_back();
		Owners.pop_back();
		Lookup.erase(Iter);
	}

	virtual void DestroyAll() override
	{
		xrSRWLockGuard guard(RWMutex, false);

		Components.clear();
		Owners.clear();
		Lookup.clear();
	}

	inline const xr_vector<T>& Data() const & { return Components; }
	inline const xr_vector<void*>& Entities() const & { return Owners; }

	virtual const char* ECS_GetName() const override { return T::ECS_Name(); }
private:
	xr_vector<T> Components;
	xr_vector<void*> Owners;
	xr_hash_map<void*, size_t> Lookup;
	xrSRWLock RWMutex;
};
