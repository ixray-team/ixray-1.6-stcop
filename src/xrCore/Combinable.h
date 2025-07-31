#pragma once

template <typename T>
class Combinable
{
public:
	// Локальное хранилище для каждого потока
	T& Local()
	{
		thread_local T LocalValue = {};
		thread_local bool Initialized = false;

		if (!Initialized)
		{
			xrCriticalSectionGuard lock(Lock);
			Storage.push_back(&LocalValue);
			Initialized = true;
		}

		return LocalValue;
	}

	// Объединение всех локальных данных
	template <typename CombineFunc>
	void CombineEach(CombineFunc func) 
	{
		xrCriticalSectionGuard lock(Lock);
		for (T* Instance : Storage)
		{
			func(*Instance);
		}
	}

private:
	xr_vector<T*> Storage;
	xrCriticalSection Lock;
};

template <typename T>
using xr_combinable = Combinable<T>;