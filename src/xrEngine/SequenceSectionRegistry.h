#pragma once

#include "ExportDefines.h"

class ENGINE_API ISequenceSectionListener
{
public:
	virtual ~ISequenceSectionListener() = default;
	// Called under the registry lock. Only enqueue a command; never call back into the registry.
	virtual void QueueSequenceSection(const shared_str& Name, bool WaitForEnd) = 0;
	virtual u64 GetSequenceSectionDuration(const char* Name) const = 0;
};

class ENGINE_API SequenceSectionRegistry
{
	struct Registration
	{
		ISequenceSectionListener* Listener;
		xr_vector<shared_str> Names;
		shared_str Selected;
	};
	xrCriticalSection Guard;
	xr_vector<shared_str> Active;
	xr_vector<Registration> Listeners;
	void Notify(Registration& Entry, bool WaitForEnd, const char* RestartName = nullptr);

public:
	bool Activate(const char* Name, bool WaitForEnd, bool Restart);
	bool Deactivate(const char* Name, bool WaitForEnd);
	void Register(ISequenceSectionListener& Listener, const xr_vector<shared_str>& Names);
	void Unregister(ISequenceSectionListener& Listener);
	void Clear();
	u64 GetDuration(const char* Name);
};

ENGINE_API bool ActivateSequenceSection(const char* Name, bool WaitForEnd = false, bool Restart = false);
ENGINE_API bool DeactivateSequenceSection(const char* Name, bool WaitForEnd = false);
