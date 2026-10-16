#pragma once
#include "EntityComponentStorage.h"

class XRCORE_API CECSManager final
{
private:
	using ECSComponentTypeID = size_t;
	friend class IECSOwner;
	friend void ECSViewDraw();

public:
	CECSManager() = default;
	~CECSManager();

	template <typename T>
	T& CreateComponent(IECSOwner* Owner)
	{
		CECSComponentStorage<T>& Storage = GetOrCreateStorage<T>();
		return Storage.Create(Owner);
	}

	template <typename T>
	T* GetComponent(IECSOwner* Owner)
	{
		CECSComponentStorage<T>& Storage = GetOrCreateStorage<T>();
		return Storage.Get(Owner);
	}

	template <typename T>
	void DestroyComponent(IECSOwner* Owner)
	{
		CECSComponentStorage<T>& Storage = GetOrCreateStorage<T>();
		Storage.Destroy(Owner);
	}

	void DestroyAll();
	void DestroyAllForOwner(IECSOwner* Owner);

	template <typename T>
	IECSOwner* GetComponentOwner(T Component)
	{
		auto& Storage = GetOrCreateStorage<std::remove_pointer_t<T>>();
		return Storage.GetOwnerFromComponent(Component);
	}

private:
	inline ECSComponentTypeID GenerateComponentTypeID()
	{
		static std::atomic<ECSComponentTypeID> LastID = 0;
		return LastID++;
	}

	template <typename T>
	ECSComponentTypeID GetComponentTypeID()
	{
		static ECSComponentTypeID TypeID = GenerateComponentTypeID();
		return TypeID;
	}

	template <typename T>
	CECSComponentStorage<T>& GetOrCreateStorage()
	{
		static const ECSComponentTypeID TypeID = GetComponentTypeID<T>();

		{
			xrSRWLockGuard guard(RWMutex, true);
			auto Iter = ComponentStorages.find(TypeID);
			if (Iter != ComponentStorages.end())
			{
				return *static_cast<CECSComponentStorage<T>*>(Iter->second);
			}
		}

		xrSRWLockGuard guard(RWMutex, false);
		IECSComponentStorage*& Storage = ComponentStorages[TypeID];
		if (Storage == nullptr)
		{
			Storage = new CECSComponentStorage<T>();
		}
		return *static_cast<CECSComponentStorage<T>*>(Storage);
	}

private:
	xrSRWLock RWMutex;
	xr_hash_map<ECSComponentTypeID, IECSComponentStorage*> ComponentStorages;

#ifdef DEBUG_DRAW
public:
	xr_hash_map<size_t, std::function<void(IECSComponentStorage*)>> ECS_DrawFuncs;

	template<typename T>
	void RegisterDrawFunc(std::function<void(CECSComponentStorage<T>*)> func)
	{
		size_t typeID = GetComponentTypeID<T>();
		ECS_DrawFuncs[typeID] = [func](IECSComponentStorage* storageBase)
		{
			func(static_cast<CECSComponentStorage<T>*>(storageBase));
		};
	}

	void DrawAllComponents(const char* filter = nullptr)
	{
		for (auto& [TypeID, StorageBase] : ComponentStorages)
		{
			auto Iter = ECS_DrawFuncs.find(TypeID);
			if (Iter != ECS_DrawFuncs.end())
			{
				Iter->second(StorageBase);
			}
		}
	}
#endif
};

extern XRCORE_API CECSManager* GECSManager;
#include "EntityDebugView.h"
