#pragma once

class ECORE_API CUndoJournal
{
public:
	using Blob = std::shared_ptr<const xr_vector<u8>>;

	struct SPart
	{
		u32 Key = 0;
		shared_str Name;
		Blob Data;

		IReader Open() const
		{
			return IReader(const_cast<u8*>(Data->data()), Data->size());
		}
	};
	using State = xr_vector<SPart>;

	static Blob MakeBlob(CMemoryWriter& Writer);
	static void Add(State& Parts, u32 Key, const shared_str& Name, CMemoryWriter& Writer);

	void Clear();
	void Reset(State&& Baseline);
	void Track(State&& Parts);
	bool Commit(State&& Parts, bool Complete = false);

	bool Undo(State& Restore);
	bool Redo(State& Restore);

	bool IsEmpty() const { return Baseline.empty(); }
	bool CanUndo() const { return !UndoSteps.empty(); }
	bool CanRedo() const { return !RedoSteps.empty(); }
	size_t UndoCount() const { return UndoSteps.size(); }
	size_t RedoCount() const { return RedoSteps.size(); }

	size_t MemoryUsage() const;

	Blob Find(u32 Key, const shared_str& Name) const;
	void Collect(u32 Key, State& Parts) const;

private:
	struct SChange
	{
		u32 Key = 0;
		shared_str Name;
		Blob Before;
		Blob After;
	};
	using Step = xr_vector<SChange>;
	using PartMap = xr_hash_map<shared_str, Blob>;

	void Apply(const Step& Changes, bool Forward, State& Restore);
	void SetBaseline(u32 Key, const shared_str& Name, const Blob& Data);
	void PushStep(Step&& Changes);

	static bool IsEqual(const Blob& Left, const Blob& Right);
	static u32 Limit();

	xr_map<u32, PartMap> Baseline;
	xr_deque<Step> UndoSteps;
	xr_deque<Step> RedoSteps;
};
