#pragma once

#include <atomic>
#include <chrono>

enum class EMonsterLogicTimer : u32
{
	Think, Memory, EnemyMemory, EnemyManager, FSM, Sharing, Distribution, Wireless, ActorHit, Schedule, ClientUpdate, Vision, NewLogicTotal, Count
};

enum class EMonsterLogicCounter : u32
{
	CombatUpdates, SharingUpdates, Transfers, Queries, QueryDeferred, Plans, PlanDeferred, Members, Targets, Calls, Hits, Focus, Panic, SharingChecked, SharingUnchanged, SharingDeferred, NativeTransfers, VisibleTransfers, WirelessTransfers, ActorTransfers, ShotNoise, ExplosionNoise, NoisePanic, NoiseAttract, Count
};

class ENGINE_API CMonsterLogicTelemetry
{
public:
	static constexpr u32 TimerCount = u32(EMonsterLogicTimer::Count);
	static constexpr u32 CounterCount = u32(EMonsterLogicCounter::Count);
	std::atomic<bool> Enabled{false};
	std::atomic<bool> Detailed{true};
	std::atomic<u32> Generation{0};
	std::atomic<u64> Microseconds[TimerCount]{};
	std::atomic<u32> Invocations[TimerCount]{};
	std::atomic<u32> Counters[CounterCount]{};
	float Average[TimerCount]{}, Peak[TimerCount]{}, CallsPerFrame[TimerCount]{};
	float PerSecond[CounterCount]{};
	u64 SessionCounters[CounterCount]{};
	u32 CounterPeak[CounterCount]{};
	float SampleSeconds = 0.f;
	float AverageFrameMilliseconds = 0.f;
	std::atomic<u32> PlayerKnownMonsters{0}, PlayerFirstSharedMonsters{0}, PlayerSharedMonsters{0}, PlayerVisibleMonsters{0};
	std::atomic<u32> AliveMonsters{0}, CombatMonsters{0}, PeacefulMonsters{0}, VanillaCrows{0}, StandalonePhantoms{0};

	void Reset();
	void CaptureFrame(float Delta);
	void Count(EMonsterLogicCounter Counter, u32 Amount = 1)
	{
		if (Enabled.load(std::memory_order_relaxed))
		{
			Counters[u32(Counter)].fetch_add(Amount, std::memory_order_relaxed);
		}
	}
private:
	double Sum[TimerCount]{};
	float WindowPeak[TimerCount]{};
	u64 WindowCalls[TimerCount]{}, WindowCounters[CounterCount]{};
	u32 WindowCounterPeak[CounterCount]{};
	u32 Frames = 0;
	float Seconds = 0.f;
};

extern ENGINE_API CMonsterLogicTelemetry g_MonsterLogicTelemetry;

class CMonsterLogicTimerScope
{
	EMonsterLogicTimer Category;
	bool Active;
	u32 Generation;
	std::chrono::steady_clock::time_point Start{};
	bool OwnsTotalDepth = false;
	static inline thread_local u32 TotalDepth = 0;
public:
	explicit CMonsterLogicTimerScope(EMonsterLogicTimer Value, bool Allow = true) : Category(Value),
		Active(Allow && g_MonsterLogicTelemetry.Enabled.load(std::memory_order_relaxed)),
		Generation(g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed))
	{
		if (Active && Category == EMonsterLogicTimer::NewLogicTotal)
		{
			OwnsTotalDepth = true;
			Active = TotalDepth++ == 0;
		}
		if (Active)
		{
			Start = std::chrono::steady_clock::now();
		}
	}
	~CMonsterLogicTimerScope()
	{
		if (Active && Generation == g_MonsterLogicTelemetry.Generation.load(std::memory_order_relaxed))
		{
			const auto Elapsed = std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::steady_clock::now() - Start).count();
			g_MonsterLogicTelemetry.Microseconds[u32(Category)].fetch_add(u64(Elapsed), std::memory_order_relaxed);
			g_MonsterLogicTelemetry.Invocations[u32(Category)].fetch_add(1, std::memory_order_relaxed);
		}
		if (OwnsTotalDepth)
		{
			--TotalDepth;
		}
	}
	CMonsterLogicTimerScope(const CMonsterLogicTimerScope&) = delete;
	CMonsterLogicTimerScope& operator=(const CMonsterLogicTimerScope&) = delete;
};
