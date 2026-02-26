#pragma once
#include "EntityManager.h"

class XRCORE_API IECSOwner
{
public:
	IECSOwner();
	virtual ~IECSOwner();

	template <typename T>
	T& CreateComponent()
	{
		CECSComponentStorage<T>& Storage = GECSManager->GetOrCreateStorage<T>();
		return Storage.Create(this);
	}

	template <typename T>
	T* GetComponent()
	{
		CECSComponentStorage<T>& Storage = GECSManager->GetOrCreateStorage<T>();
		return Storage.Get(this);
	}

	template <typename T>
	const T* GetComponent() const
	{
		CECSComponentStorage<T>& Storage = GECSManager->GetOrCreateStorage<T>();
		return Storage.Get(this);
	}

	template <typename T>
	void DestroyComponent()
	{
		CECSComponentStorage<T>& Storage = GECSManager->GetOrCreateStorage<T>();
		Storage.Destroy(this);
	}

	template <typename T>
	T* GetOrCreateComponent()
	{
		CECSComponentStorage<T>& Storage = GECSManager->GetOrCreateStorage<T>();
		T* Comp = Storage.Get(this);

		if (Comp == nullptr)
		{
			Comp = &Storage.Create(this);
		}

		return Comp;
	}
};