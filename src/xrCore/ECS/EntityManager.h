#pragma once
#include "EntityComponentStorage.h"

class XRCORE_API CECSManager final
{
private:
	using ECSComponentTypeID = size_t;

public:
	CECSManager() = default;
	~CECSManager();

	template <typename T>
	T& CreateComponent(void* Owner)
	{
		CECSComponentStorage<T>& Storage = GetOrCreateStorage<T>();
		return Storage.Create(Owner);
	}

	template <typename T>
	T* GetComponent(void* Owner)
	{
		CECSComponentStorage<T>& Storage = GetOrCreateStorage<T>();
		return Storage.Get(Owner);
	}

	template <typename T>
	void DestroyComponent(void* Owner)
	{
		CECSComponentStorage<T>& Storage = GetOrCreateStorage<T>();
		Storage.Destroy(Owner);
	}

	void DestroyAll();
	void DestroyAllForOwner(void* Owner);

private:
	inline size_t GenerateComponentTypeID()
	{
		static std::atomic<size_t> LastID = 0;
		return LastID++;
	}

	template <typename T>
	size_t GetComponentTypeID()
	{
		static size_t TypeID = GenerateComponentTypeID();
		return TypeID;
	}

	template <typename T>
	CECSComponentStorage<T>& GetOrCreateStorage()
	{
		static const ECSComponentTypeID TypeID = GetComponentTypeID<T>();

		auto Iter = ComponentStorages.find(TypeID);
		if (Iter == ComponentStorages.end())
		{
			CECSComponentStorage<T>* NewStorage = new CECSComponentStorage<T>();
			ComponentStorages[TypeID] = NewStorage;
			return *NewStorage;
		}

		return *static_cast<CECSComponentStorage<T>*>(Iter->second);
	}

private:
	xr_hash_map<ECSComponentTypeID, IECSComponentStorage*> ComponentStorages;
};

extern XRCORE_API CECSManager* GECSManager;