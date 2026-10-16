#pragma once

class IECSOwner;

class IECSComponentStorage
{
public:
	virtual ~IECSComponentStorage() = default;
	virtual void DestroyAll() = 0;
	virtual void Destroy(IECSOwner* Owner) = 0;
	virtual const char* ECS_GetName() const = 0;
};

template <typename T>
concept ECSHasSetupOwner = requires(T t, IECSOwner* owner)
{
	{ t.SetupOwner(owner) };
};

template <typename T>
concept ECSHasBeginComponent = requires(T t, IECSOwner* owner)
{
	{ t.BeginComponent(owner) };
};
template <typename T>
concept ECSHasEndComponent = requires(T t)
{
	{ t.EndComponent() };
};

template <typename T>
class CECSComponentStorage final :
	public IECSComponentStorage
{
public:
	T& Create(IECSOwner* Owner)
	{
		xrSRWLockGuard guard(RWMutex, false);
		size_t Index = Components.size();
		T& NewComponent = *Components.emplace_back(xr_make_unique<T>());
		ComponentIndices[&NewComponent] = Index;

		if constexpr (ECSHasSetupOwner<T>)
		{
			NewComponent.SetupOwner(Owner);
		}

		if constexpr (ECSHasBeginComponent<T>)
		{
			NewComponent.BeginComponent(Owner);
		}

		Owners.emplace_back(Owner);
		Lookup[Owner] = Index;

		return NewComponent;
	}

	T* Get(IECSOwner* Owner)
	{
		xrSRWLockGuard guard(RWMutex, true);
		auto Iter = Lookup.find(Owner);
		return Iter != Lookup.end() ? Components[Iter->second].get() : nullptr;
	}

	const T* Get(const IECSOwner* Owner) const
	{
		xrSRWLockGuard guard(RWMutex, true);
		auto Iter = Lookup.find(const_cast<IECSOwner*>(Owner));
		return Iter != Lookup.end() ? Components[Iter->second].get() : nullptr;
	}

	IECSOwner* GetOwnerFromComponent(const T* Component) const
	{
		xrSRWLockGuard guard(RWMutex, true);

		auto Iter = ComponentIndices.find(Component);
		if (Iter != ComponentIndices.end())
		{
			return static_cast<IECSOwner*>(Owners[Iter->second]);
		}

		return nullptr;
	}

	virtual void Destroy(IECSOwner* Owner) override
	{
		xrSRWLockGuard guard(RWMutex, false);
		auto Iter = Lookup.find(Owner);
		if (Iter == Lookup.end())
		{
			return;
		}

		size_t Index = Iter->second;
		size_t Last = Components.size() - 1;

		if constexpr (ECSHasEndComponent<T>)
		{
			Components[Index]->EndComponent();
		}

		ComponentIndices.erase(Components[Index].get());
		if (Index != Last)
		{
			Components[Index] = std::move(Components[Last]);
			Owners[Index] = Owners[Last];
			Lookup[Owners[Index]] = Index;
			ComponentIndices[Components[Index].get()] = Index;
		}

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
		ComponentIndices.clear();
	}

	inline const xr_vector<xr_unique_ptr<T>>& Data() const & { return Components; }
	inline const xr_vector<void*>& Entities() const & { return Owners; }

	virtual const char* ECS_GetName() const override { return T::ECS_Name(); }
private:
	xr_vector<xr_unique_ptr<T>> Components;
	xr_vector<void*> Owners;
	xr_hash_map<void*, size_t> Lookup;
	xr_hash_map<const T*, size_t> ComponentIndices;
	mutable xrSRWLock RWMutex;
};
