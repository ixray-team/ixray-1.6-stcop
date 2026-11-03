#include "stdafx.h"
#include "UndoJournal.h"
#include "EditorPreferences.h"

CUndoJournal::Blob CUndoJournal::MakeBlob(CMemoryWriter& Writer)
{
	const u8* Begin = Writer.pointer();
	return std::make_shared<const xr_vector<u8>>(Begin, Begin + Writer.size());
}

void CUndoJournal::Add(State& Parts, u32 Key, const shared_str& Name, CMemoryWriter& Writer)
{
	Parts.push_back({Key, Name, MakeBlob(Writer)});
}

void CUndoJournal::Clear()
{
	Baseline.clear();
	UndoSteps.clear();
	RedoSteps.clear();
}

void CUndoJournal::Reset(State&& NewBaseline)
{
	Clear();
	for (SPart& Part : NewBaseline)
	{
		SetBaseline(Part.Key, Part.Name, Part.Data);
	}
}

void CUndoJournal::Track(State&& Parts)
{
	for (SPart& Part : Parts)
	{
		PartMap& Map = Baseline[Part.Key];
		if (Map.find(Part.Name) == Map.end())
		{
			Map.emplace(Part.Name, std::move(Part.Data));
		}
	}
}

bool CUndoJournal::Commit(State&& Parts, bool Complete)
{
	Step Changes;
	if (!Complete)
	{
		for (SPart& Part : Parts)
		{
			Blob& Current = Baseline[Part.Key][Part.Name];
			if (!Current)
			{
				Current = std::move(Part.Data);
			}
			else if (!IsEqual(Current, Part.Data))
			{
				Changes.push_back({Part.Key, Part.Name, Current, Part.Data});
				Current = std::move(Part.Data);
			}
		}

		const bool Changed = !Changes.empty();
		PushStep(std::move(Changes));
		return Changed;
	}

	xr_map<u32, PartMap> NewBaseline;
	for (SPart& Part : Parts)
	{
		Blob Data = std::move(Part.Data);
		auto KeyIt = Baseline.find(Part.Key);
		if (KeyIt != Baseline.end())
		{
			auto PartIt = KeyIt->second.find(Part.Name);
			if (PartIt != KeyIt->second.end())
			{
				if (IsEqual(PartIt->second, Data))
				{
					// Unchanged parts share the data with the history
					Data = PartIt->second;
				}
				else
				{
					Changes.push_back({Part.Key, Part.Name, PartIt->second, Data});
				}
				KeyIt->second.erase(PartIt);
				NewBaseline[Part.Key].emplace(Part.Name, std::move(Data));
				continue;
			}
		}

		Changes.push_back({Part.Key, Part.Name, nullptr, Data});
		NewBaseline[Part.Key].emplace(Part.Name, std::move(Data));
	}

	// What is left of the old baseline was deleted
	for (auto& [Key, Map] : Baseline)
	{
		for (auto& [Name, Data] : Map)
		{
			Changes.push_back({Key, Name, Data, nullptr});
		}
	}
	Baseline = std::move(NewBaseline);

	const bool Changed = !Changes.empty();
	PushStep(std::move(Changes));
	return Changed;
}

bool CUndoJournal::Undo(State& Restore)
{
	if (UndoSteps.empty())
	{
		return false;
	}

	Apply(UndoSteps.back(), false, Restore);
	RedoSteps.push_back(std::move(UndoSteps.back()));
	UndoSteps.pop_back();
	return true;
}

bool CUndoJournal::Redo(State& Restore)
{
	if (RedoSteps.empty())
	{
		return false;
	}

	Apply(RedoSteps.back(), true, Restore);
	UndoSteps.push_back(std::move(RedoSteps.back()));
	RedoSteps.pop_back();
	return true;
}

size_t CUndoJournal::MemoryUsage() const
{
	xr_hash_set<const xr_vector<u8>*> Counted;
	size_t Bytes = 0;
	auto Count = [&Counted, &Bytes](const Blob& Data)
	{
		if (Data && Counted.insert(Data.get()).second)
		{
			Bytes += Data->size();
		}
	};

	for (const auto& [Key, Map] : Baseline)
	{
		for (const auto& [Name, Data] : Map)
		{
			Count(Data);
		}
	}

	for (const xr_deque<Step>* Steps : {&UndoSteps, &RedoSteps})
	{
		for (const Step& Changes : *Steps)
		{
			for (const SChange& Change : Changes)
			{
				Count(Change.Before);
				Count(Change.After);
			}
		}
	}
	return Bytes;
}

CUndoJournal::Blob CUndoJournal::Find(u32 Key, const shared_str& Name) const
{
	auto KeyIt = Baseline.find(Key);
	if (KeyIt == Baseline.end())
	{
		return nullptr;
	}

	auto PartIt = KeyIt->second.find(Name);
	return PartIt != KeyIt->second.end() ? PartIt->second : nullptr;
}

void CUndoJournal::Collect(u32 Key, State& Parts) const
{
	auto KeyIt = Baseline.find(Key);
	if (KeyIt == Baseline.end())
	{
		return;
	}

	for (const auto& [Name, Data] : KeyIt->second)
	{
		Parts.push_back({Key, Name, Data});
	}
}

void CUndoJournal::Apply(const Step& Changes, bool Forward, State& Restore)
{
	Restore.clear();
	for (const SChange& Change : Changes)
	{
		const Blob& Data = Forward ? Change.After : Change.Before;
		SetBaseline(Change.Key, Change.Name, Data);
		Restore.push_back({Change.Key, Change.Name, Data});
	}
}

void CUndoJournal::SetBaseline(u32 Key, const shared_str& Name, const Blob& Data)
{
	if (Data)
	{
		Baseline[Key][Name] = Data;
		return;
	}

	auto KeyIt = Baseline.find(Key);
	if (KeyIt != Baseline.end())
	{
		KeyIt->second.erase(Name);
	}
}

void CUndoJournal::PushStep(Step&& Changes)
{
	const u32 MaxSteps = Limit();
	if (Changes.empty() || MaxSteps == 0)
	{
		return;
	}

	UndoSteps.push_back(std::move(Changes));
	RedoSteps.clear();
	while (UndoSteps.size() > MaxSteps)
	{
		UndoSteps.pop_front();
	}
}

bool CUndoJournal::IsEqual(const Blob& Left, const Blob& Right)
{
	if (Left == Right)
	{
		return true;
	}

	if (!Left || !Right || Left->size() != Right->size())
	{
		return false;
	}
	return Left->empty() || memcmp(Left->data(), Right->data(), Left->size()) == 0;
}

u32 CUndoJournal::Limit()
{
	return EPrefs != nullptr ? EPrefs->scene_undo_level : 0;
}
