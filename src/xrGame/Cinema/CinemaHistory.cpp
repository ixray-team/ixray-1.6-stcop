#include "StdAfx.h"
#include "CinemaHistory.h"

namespace Cinema
{
void History::Begin(const char* Label)
{
	if (IsStepOpen)
	{
		return;
	}
	IsStepOpen = true;
	Pending.Label = Label;
	Pending.Changes.clear();
	Pending.HasHeader = Pending.HasLoop = false;
}

void History::Touch(const Scene& Data, u32 Id)
{
	if (!IsStepOpen || std::any_of(Pending.Changes.begin(), Pending.Changes.end(), [Id](const Change& Item) { return Item.Id == Id; }))
	{
		return;
	}
	const Object* Live = Find(Data, Id);
	Pending.Changes.push_back({ Id, Live != nullptr, Live ? *Live : Object {} });
}

void History::TouchHeader(const Scene& Data)
{
	if (IsStepOpen && !Pending.HasHeader)
	{
		Pending.HasHeader = true;
		Pending.Header = Data.Header;
	}
}

void History::TouchLoop(float LoopStart, float LoopEnd)
{
	if (IsStepOpen && !Pending.HasLoop)
	{
		Pending.HasLoop = true;
		Pending.LoopStart = LoopStart;
		Pending.LoopEnd = LoopEnd;
	}
}

void History::SetLabel(const char* Label)
{
	if (IsStepOpen)
	{
		Pending.Label = Label;
	}
}

void History::End(Scene& Data)
{
	if (!IsStepOpen)
	{
		return;
	}
	IsStepOpen = false;
	const auto IsEmpty = [](const Lane& Item) { return std::all_of(std::begin(Item.Curves), std::end(Item.Curves), [](const Curve& Part) { return Part.Keys.empty() && Part.Gen.Type == Wave::None; }); };
	for (const Change& Item : Pending.Changes)
	{
		if (Object* Live = Find(Data, Item.Id))
		{
			std::erase_if(Live->Lanes, IsEmpty);
			for (CameraBlock& Block : Live->Camera.Blocks)
			{
				std::erase_if(Block.Lanes, IsEmpty);
			}
		}
	}
	std::erase_if(Pending.Changes, [&](const Change& Item)
	{
		const Object* Live = Find(Data, Item.Id);
		return Item.IsPresent == (Live != nullptr) && (!Live || *Live == Item.Data);
	});
	Pending.HasHeader = Pending.HasHeader && !(Pending.Header == Data.Header);
	if (Pending.Changes.empty() && !Pending.HasHeader && !Pending.HasLoop)
	{
		return;
	}
	Steps.resize(Cursor);
	Pending.Serial = NextSerial++;
	Steps.push_back(std::move(Pending));
	Pending = {};
	if (Steps.size() > MaxSteps)
	{
		BaseSerial = Steps.front().Serial;
		Steps.erase(Steps.begin());
	}
	Cursor = static_cast<u32>(Steps.size());
}

void History::Cancel(Scene& Data)
{
	if (IsStepOpen)
	{
		IsStepOpen = false;
		Swap(Data, Pending, true);
	}
}

bool History::Undo(Scene& Data, float& LoopStart, float& LoopEnd)
{
	if (IsStepOpen || !Cursor)
	{
		return false;
	}
	Swap(Data, Steps[--Cursor], true);
	SwapLoop(Steps[Cursor], LoopStart, LoopEnd);
	return true;
}

bool History::Redo(Scene& Data, float& LoopStart, float& LoopEnd)
{
	if (IsStepOpen || Cursor == Steps.size())
	{
		return false;
	}
	SwapLoop(Steps[Cursor], LoopStart, LoopEnd);
	Swap(Data, Steps[Cursor++], false);
	return true;
}

void History::Clear()
{
	Steps.clear();
	Cursor = 0;
	IsStepOpen = false;
	BaseSerial = NextSerial++;
}

void History::Swap(Scene& Data, Step& Target, bool IsReverse)
{
	const size_t Count = Target.Changes.size();
	for (size_t Index = 0; Index < Count; ++Index)
	{
		Change& Item = Target.Changes[IsReverse ? Count - 1 - Index : Index];
		const s32 Live = IndexOf(Data, Item.Id);
		if (Live >= 0 && Item.IsPresent)
		{
			std::swap(Data.Objects[Live], Item.Data);
		}
		else if (Item.IsPresent)
		{
			Data.Objects.push_back(std::move(Item.Data));
			Item.IsPresent = false;
		}
		else if (Live >= 0)
		{
			Item.Data = std::move(Data.Objects[Live]);
			Data.Objects.erase(Data.Objects.begin() + Live);
			Item.IsPresent = true;
		}
	}
	if (Target.HasHeader)
	{
		std::swap(Data.Header, Target.Header);
	}
}

void History::SwapLoop(Step& Target, float& LoopStart, float& LoopEnd)
{
	if (Target.HasLoop)
	{
		std::swap(LoopStart, Target.LoopStart);
		std::swap(LoopEnd, Target.LoopEnd);
	}
}
}
