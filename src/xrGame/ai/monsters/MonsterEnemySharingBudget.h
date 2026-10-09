#pragma once

class CMonsterEnemySharingBudget
{
public:
	static constexpr u32 MaxUpdates = 12;
	static constexpr u32 MaxQueries = 4;
	u32 QueriesRemaining = 0;
	static constexpr u32 MaxRecords = 384;
	static constexpr u32 MaxTransfers = 32;
	static constexpr u32 MaxSourcesPerUpdate = 4;
	static constexpr u32 MaxRecordsPerSource = 8;
	u32 RecordsRemaining = 0;
private:
	u32 Frame = u32(-1);
	u32 UpdatesRemaining = 0;
	u32 TransfersRemaining = 0;
public:
	constexpr void BeginFrame(u32 CurrentFrame)
	{
		if (Frame != CurrentFrame)
		{
			Frame = CurrentFrame;
			UpdatesRemaining = MaxUpdates;
			QueriesRemaining = MaxQueries;
			RecordsRemaining = MaxRecords;
			TransfersRemaining = MaxTransfers;
		}
	}
	constexpr bool TakeQuery()
	{
		if (!QueriesRemaining)
		{
			return false;
		}
		--QueriesRemaining;
		return true;
	}
	constexpr bool TakeUpdate()
	{
		if (!UpdatesRemaining)
		{
			return false;
		}
		--UpdatesRemaining;
		return true;
	}
	constexpr bool TakeRecord()
	{
		if (!RecordsRemaining)
		{
			return false;
		}
		--RecordsRemaining;
		return true;
	}
	constexpr bool TakeTransfer()
	{
		if (!TransfersRemaining)
		{
			return false;
		}
		--TransfersRemaining;
		return true;
	}
	static constexpr bool NeedsRefresh(bool Known, u32 Now, u32 IncomingTime, u32 KnownTime, u32 MemoryTime, float MovementSquared)
	{
		if (!Known)
		{
			return true;
		}
		const u32 Advance = IncomingTime - KnownTime;
		if (!Advance || Advance > 0x7FFFFFFFu)
		{
			return false;
		}
		const u32 RefreshPeriod = MemoryTime / 2 < 1000 ? MemoryTime / 2 : 1000;
		if (Now - KnownTime >= RefreshPeriod)
		{
			return true;
		}
		return IncomingTime - KnownTime >= 1000 && MovementSquared >= 16.f;
	}
};
