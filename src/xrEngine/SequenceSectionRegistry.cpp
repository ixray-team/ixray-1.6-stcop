#include "stdafx.h"
#include "SequenceSectionRegistry.h"

void SequenceSectionRegistry::Notify(Registration& Entry, bool WaitForEnd, const char* RestartName)
{
	shared_str Selected = "default";
	// Registration follows SEQ declaration order: the first active section wins.
	for (const auto& Name : Entry.Names)
	{
		if (std::find(Active.begin(), Active.end(), Name) != Active.end())
		{
			Selected = Name;
			break;
		}
	}
	if (Entry.Selected != Selected || (RestartName && xr_strcmp(*Selected, RestartName) == 0))
	{
		Entry.Selected = Selected;
		Entry.Listener->QueueSequenceSection(Selected, WaitForEnd);
	}
}

bool SequenceSectionRegistry::Activate(const char* Name, bool WaitForEnd, bool Restart)
{
	if (!Name || !*Name || xr_strcmp(Name, "default") == 0)
	{
		return false;
	}
	xrCriticalSectionGuard Lock(Guard);
	const shared_str Section = Name;
	const auto It = std::find(Active.begin(), Active.end(), Section);
	if (It != Active.end())
	{
		if (!Restart)
		{
			return false;
		}
		Active.erase(It);
	}
	Active.push_back(Section);
	for (auto& Entry : Listeners)
	{
		Notify(Entry, WaitForEnd, Restart ? Name : nullptr);
	}
	return true;
}

bool SequenceSectionRegistry::Deactivate(const char* Name, bool WaitForEnd)
{
	if (!Name || !*Name)
	{
		return false;
	}
	xrCriticalSectionGuard Lock(Guard);
	const auto It = std::find(Active.begin(), Active.end(), shared_str(Name));
	if (It == Active.end())
	{
		return false;
	}
	Active.erase(It);
	for (auto& Entry : Listeners)
	{
		Notify(Entry, WaitForEnd);
	}
	return true;
}

void SequenceSectionRegistry::Register(ISequenceSectionListener& Listener, const xr_vector<shared_str>& Names)
{
	xrCriticalSectionGuard Lock(Guard);
	Listeners.push_back({&Listener, Names, "default"});
	Notify(Listeners.back(), false);
}

void SequenceSectionRegistry::Unregister(ISequenceSectionListener& Listener)
{
	xrCriticalSectionGuard Lock(Guard);
	Listeners.erase(std::remove_if(Listeners.begin(), Listeners.end(), [&Listener](const Registration& Entry) { return Entry.Listener == &Listener; }), Listeners.end());
}

void SequenceSectionRegistry::Clear()
{
	xrCriticalSectionGuard Lock(Guard);
	Active.clear();
	for (auto& Entry : Listeners)
	{
		Notify(Entry, false);
	}
	// Release interned strings and allocations while the engine allocator is still alive.
	xr_vector<shared_str>().swap(Active);
	xr_vector<Registration>().swap(Listeners);
}

bool ActivateSequenceSection(const char* Name, bool WaitForEnd, bool Restart)
{
	return Engine.SequenceSections.Activate(Name, WaitForEnd, Restart);
}

u64 SequenceSectionRegistry::GetDuration(const char* Name)
{
	xrCriticalSectionGuard Lock(Guard);
	u64 Duration = 0;
	for (const auto& Entry : Listeners)
	{
		Duration = std::max(Duration, Entry.Listener->GetSequenceSectionDuration(Name));
	}
	return Duration;
}

bool DeactivateSequenceSection(const char* Name, bool WaitForEnd)
{
	return Engine.SequenceSections.Deactivate(Name, WaitForEnd);
}
