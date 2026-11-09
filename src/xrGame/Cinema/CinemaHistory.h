#pragma once
#include "CinemaScene.h"

namespace Cinema
{
class History
{
public:
	static constexpr u32 MaxSteps = 100;
	void Begin(const char* Label);
	void Touch(const Scene& Data, u32 Id);
	void TouchHeader(const Scene& Data);
	void TouchLoop(float LoopStart, float LoopEnd);
	void SetLabel(const char* Label);
	void End(Scene& Data);
	void Cancel(Scene& Data);
	bool Undo(Scene& Data, float& LoopStart, float& LoopEnd);
	bool Redo(Scene& Data, float& LoopStart, float& LoopEnd);
	void Clear();
	bool IsOpen() const { return IsStepOpen; }
	u64 Serial() const { return Cursor ? Steps[Cursor - 1].Serial : BaseSerial; }
	const char* UndoLabel() const { return Cursor ? Steps[Cursor - 1].Label.c_str() : nullptr; }
	const char* RedoLabel() const { return Cursor < Steps.size() ? Steps[Cursor].Label.c_str() : nullptr; }

private:
	struct Change
	{
		u32 Id = 0;
		bool IsPresent = false;
		Object Data;
	};
	struct Step
	{
		xr_string Label;
		u64 Serial = 0;
		xr_vector<Change> Changes;
		bool HasHeader = false;
		SceneHeader Header;
		bool HasLoop = false;
		float LoopStart = 0.0f, LoopEnd = 0.0f;
	};
	static void Swap(Scene& Data, Step& Target, bool IsReverse);
	static void SwapLoop(Step& Target, float& LoopStart, float& LoopEnd);
	xr_vector<Step> Steps;
	u32 Cursor = 0;
	Step Pending;
	bool IsStepOpen = false;
	u64 NextSerial = 1, BaseSerial = 0;
};
}
