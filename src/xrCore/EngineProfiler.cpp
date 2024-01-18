#include "stdafx.h"
#include "EngineProfiler.h"

#define TIME_NOW() std::chrono::high_resolution_clock::now().time_since_epoch().count()

struct ProfilerState
{
	xrCriticalSection WriteMutex;
	xr_vector<Profile::ThreadStatistics> Statistics;
};

class ProfilerThreadState
{
private:
	struct EventStorage
	{
		xr_vector<Profile::TraceEvent> Events;
	};

	EventStorage* Storage = nullptr;
	u64 TimestampFrameBegin = 0;
	u64 TimestampFrameEnd = 0;
	int StackLevel = 0;
	int MaxStackLevel = 0;

public:
	const Profile::TraceEvent& LastEvent() const
	{
		VERIFY(Storage && !Storage->Events.empty());
		return Storage->Events.back();
	}

	const xr_vector<Profile::TraceEvent>& GetEvents() const
	{
		return Storage->Events;
	}

	int GetStackLevel() const
	{
		return StackLevel;
	}
	
	int GetMaxStackLevel() const
	{
		return MaxStackLevel;
	}

	u64 GetBeginFrameTimestamp() const
	{
		return TimestampFrameBegin;
	}
		
	u64 GetEndFrameTimestamp() const
	{
		return TimestampFrameEnd;
	}

	void BeginFrame()
	{
		VERIFY2(TimestampFrameBegin == 0, "Can't call \"Profile::BeginFrame\" twice!");
		TimestampFrameBegin = TIME_NOW();
	}
	
	void EndFrame()
	{
		VERIFY2(TimestampFrameBegin != 0, "Called \"Profile::EndFrame\", but \"Profile::BeginFrame\" was not called!");
		TimestampFrameEnd = TIME_NOW();
	}

	void Reset()
	{
		Storage->Events.resize(0);
		TimestampFrameBegin = 0;
		TimestampFrameEnd = 0;
		StackLevel = 0;
		MaxStackLevel = 0;
	}

	void Register()
	{
		Storage = new EventStorage();
	}

	void Unregister()
	{
		if (Storage != nullptr) {
			delete Storage;
			Storage = nullptr;
		}
	}

	int PushEvent(const char* Name, const char* Function, const char* Group, const char* File, int Line, u32 Color)
	{
		if (Storage == nullptr)
			return -1;

		VERIFY2(TimestampFrameBegin != 0, "Call \"Profile::BeginFrame\" before calling this method!");
		if (Color == 0) {
			u32 Hash = HashValue<u32>(Name);
			Color = Hash | 0x000000FF;
		}

		Profile::TraceEvent Event = {};
		Event.Color = Color;
		Event.Line = Line;
		Event.BeginTimestamp = TIME_NOW();
		Event.StackLevel = StackLevel;

		xr_strcpy(Event.Name, Name);
		if (Group != nullptr) {
			xr_strcpy(Event.Group, Group);
			Event.GroupHash = HashValue<u64>(Group);
		}
		if (Function != nullptr) {
			xr_strcpy(Event.Function, Function);
		}
		if (File != nullptr) {
			xr_strcpy(Event.File, File);
		}

		int Index = (int)Storage->Events.size();
		Storage->Events.emplace_back(std::move(Event));

		StackLevel++;
		MaxStackLevel = std::max(StackLevel, MaxStackLevel);
		return Index;
	}

	void PopEvent(int Index)
	{
		if (Storage == nullptr)
			return;

		VERIFY2(TimestampFrameBegin != 0, "Call \"Profile::BeginFrame\" before calling this method!");
		auto& Event = Storage->Events.at(Index);

		VERIFY(Event.StackLevel == StackLevel - 1);
		Event.Set(Profile::TraceEvent::Flag::Closed);
		Event.EndTimestamp = TIME_NOW();
		StackLevel--;
	}
};

static ProfilerState* EngineProfiler = nullptr;
static thread_local ProfilerThreadState ThreadState;

void 
Profile::Init()
{
	EngineProfiler = new ProfilerState();
}

void 
Profile::Shutdown()
{
	if (EngineProfiler != nullptr) {
		delete EngineProfiler;
		EngineProfiler = nullptr;
	}
}

int
Profile::BeginEvent(const char* Name, u32 Color, const char* Group)
{
	return ThreadState.PushEvent(Name, nullptr, Group, nullptr, 0, Color);
}

int
Profile::BeginCodeEvent(const char* Name, const char* Function, const char* File, int Line, u32 Color, const char* Group)
{
	return ThreadState.PushEvent(Name, Function, Group, File, Line, Color);
}

void 
Profile::EndEvent(int Index)
{
	if (EngineProfiler == nullptr)
		return;

	ThreadState.PopEvent(Index);
}

void 
Profile::RegisterThread(const char* Name)
{
	if (EngineProfiler == nullptr)
		return;

	xrCriticalSectionGuard Guard(&EngineProfiler->WriteMutex);

	ThreadState.Register();

	ThreadStatistics Thread = {};
	Thread.Id = GetThreadId(GetCurrentThread());
	Thread.Name = Name;
	EngineProfiler->Statistics.emplace_back(std::move(Thread));
}

void 
Profile::UnregisterThread()
{
	if (EngineProfiler == nullptr)
		return;

	xrCriticalSectionGuard Guard(&EngineProfiler->WriteMutex);

	for (size_t i = 0; i < EngineProfiler->Statistics.size(); i++) {
		if (EngineProfiler->Statistics[i].Id == GetThreadId(GetCurrentThread())) {
			EngineProfiler->Statistics.erase(EngineProfiler->Statistics.begin() + i);
			return;
		}
	}

	ThreadState.Unregister();
}

void 
Profile::BeginFrame(const char* Name)
{
	if (EngineProfiler == nullptr)
		return;

	ThreadState.BeginFrame();
}

void
Profile::EndFrame()
{
	if (EngineProfiler == nullptr)
		return;

	xrCriticalSectionGuard Guard(&EngineProfiler->WriteMutex);

	ThreadState.EndFrame();
	for (size_t i = 0; i < EngineProfiler->Statistics.size(); i++) {
		if (EngineProfiler->Statistics[i].Id == GetThreadId(GetCurrentThread())) {
			VERIFY2(ThreadState.GetStackLevel() == 0, "Invalid stack (forgot to call \"Profile::EndEvent()\" somewhere?");
			EngineProfiler->Statistics[i].TotalTime.Write(float(EngineProfiler->Statistics[i].TimestampFrameEnd - EngineProfiler->Statistics[i].TimestampFrameBegin) / 1000000.0f);
			EngineProfiler->Statistics[i].Events = ThreadState.GetEvents();
			EngineProfiler->Statistics[i].StackLevels = ThreadState.GetMaxStackLevel();
			EngineProfiler->Statistics[i].TimestampFrameBegin = ThreadState.GetBeginFrameTimestamp();
			EngineProfiler->Statistics[i].TimestampFrameEnd = ThreadState.GetEndFrameTimestamp();
			for (const auto& Event : EngineProfiler->Statistics[i].Events) {
				const double BeginDelta = ilerp((double)(EngineProfiler->Statistics[i].TimestampFrameBegin) * 0.001, (double)(EngineProfiler->Statistics[i].TimestampFrameEnd) * 0.001, (double)(Event.BeginTimestamp) * 0.001);
				const double EndDelta = ilerp((double)(EngineProfiler->Statistics[i].TimestampFrameBegin) * 0.001, (double)(EngineProfiler->Statistics[i].TimestampFrameEnd) * 0.001, (double)(Event.EndTimestamp) * 0.001);
				EngineProfiler->Statistics[i].BeginSmoothTimers[Event.GetHash()].Write((float)BeginDelta);
				EngineProfiler->Statistics[i].EndSmoothTimers[Event.GetHash()].Write((float)EndDelta);
			}

			std::sort(EngineProfiler->Statistics[i].Events.begin(), EngineProfiler->Statistics[i].Events.end(), [](const TraceEvent& Left, const TraceEvent& Right) {
				return (Left.EndTimestamp - Left.BeginTimestamp) > (Right.EndTimestamp - Right.BeginTimestamp);
			});

			break;
		}
	}

	ThreadState.Reset();
}

u32 
Profile::GetThreadCount()
{
	return (u32)EngineProfiler->Statistics.size();
}

u32 
Profile::GetThisThreadId()
{
	for (size_t i = 0; i < EngineProfiler->Statistics.size(); i++) {
		if (EngineProfiler->Statistics[i].Id == GetThreadId(GetCurrentThread())) {
			return (u32) EngineProfiler->Statistics[i].Id;
		}
	}

	return u32(-1);
}

const xr_vector<Profile::TraceEvent>&
Profile::GetEventsByIndex(u32 ThreadIndex)
{
	return EngineProfiler->Statistics[ThreadIndex].Events;
}

const xr_vector<Profile::TraceEvent>&
Profile::GetEvents(u32 ThreadId)
{
	for (size_t i = 0; i < EngineProfiler->Statistics.size(); i++) {
		if (EngineProfiler->Statistics[i].Id == GetThreadId(GetCurrentThread())) {
			return EngineProfiler->Statistics[i].Events;
		}
	}

	return EngineProfiler->Statistics[0].Events;
}

XRCORE_API const Profile::ThreadStatistics& 
Profile::GetThreadStatistics(u32 ThreadIndex)
{
	return EngineProfiler->Statistics[ThreadIndex];
}