#pragma once
#include <bitset>
#include "../xrCore/_math.h"

#ifndef FUNCTION_SIGNATURE
#if defined(__FUNCSIG__)
#define FUNCTION_SIGNATURE __FUNCSIG__
#elif defined(__PRETTY_FUNCTION__) || defined(__GNUC__) || defined(__clang__)
#define FUNCTION_SIGNATURE __PRETTY_FUNCTION__
#else
#define FUNCTION_SIGNATURE __FUNCTION__
#endif
#endif
#define EVENT_CONCAT_IMPL(x, y) x##y
#define EVENT_CONCAT(x, y) EVENT_CONCAT_IMPL(x, y)

template<typename T>
constexpr T HashValue(const char* String)
{
	T Hash = T(5381);
	while (*String) {
		Hash *= 0x21;
		Hash += *String;
		String++;
	}

	return Hash;
}

namespace Profile
{
	struct TraceEvent
	{
		enum class Flag
		{
			Closed
		};

		u32 Color = 0;
		int StackLevel = 0;
		int Line = -1;
		u64 BeginTimestamp = 0;
		u64 EndTimestamp = 0;
		u64 GroupHash = 0;
		std::bitset<64> Flags;
		string32 Name = {};
		string32 Group = {};
		string256 Function = {};
		string256 File = {};

		void Set(Flag Set)
		{
			Flags.set(static_cast<u8>(Set));
		}

		bool Test(Flag Test) const
		{
			return Flags.test(static_cast<u8>(Test));
		}
	};

	void XRCORE_API Init();
	void XRCORE_API Shutdown();

	int XRCORE_API BeginEvent(const char* Name, u32 Color = 0, const char* Group = "Default");
	int XRCORE_API BeginCodeEvent(const char* Name, const char* Function, const char* File, int Line, u32 Color = 0, const char* Group = "Default");
	void XRCORE_API EndEvent(int Index);

	void XRCORE_API RegisterThread(const char* Name);
	void XRCORE_API UnregisterThread();

	void XRCORE_API BeginFrame(const char* Name);
	void XRCORE_API EndFrame();

	u32 XRCORE_API GetThreadCount();
	u32 XRCORE_API GetThisThreadId();
	XRCORE_API const xr_vector<TraceEvent>& GetEvents(u32 ThreadId);
	XRCORE_API const xr_vector<TraceEvent>& GetEventsByIndex(u32 ThreadIndex);

	void TraverseGroup(u32 ThreadId, const char* Group, auto&& Function)
	{
		int Stack = 0;
		int TrueStack = 0;
		u64 GroupHash = HashValue<u64>(Group);
		for (const auto& Event : GetEvents(ThreadId)) {
			if (Event.GroupHash == GroupHash) {
				Function(Event);
			}
		}
	}

	void TraverseGroup(const char* Group, auto&& Function)
	{
		int Stack = 0;
		int TrueStack = 0;
		u64 GroupHash = HashValue<u64>(Group);
		for (size_t i = 0; i < GetThreadCount(); i++) {
			for (const auto& Event : GetEventsByIndex(i)) {
				if (Event.GroupHash == GroupHash) {
					Function(Event);
				}
			}
		}
	}
}

class ScopeProfileEvent
{
private:
	const char* Name = nullptr;
	const char* Group = nullptr;
	const char* Function = nullptr;
	const char* File = nullptr;
	int Index = 0;
	int Line = 0;

public:
	ScopeProfileEvent(const char* EventName, const char* EventGroup)
		: Name(EventName), Group(EventGroup) { Index = Profile::BeginEvent(Name, 0, EventGroup); }
	ScopeProfileEvent(const char* EventName) 
		: Name(EventName) { Index = Profile::BeginEvent(Name); }

	ScopeProfileEvent(const char* EventFunction, const char* EventFile, int EventLine)
		: Name(EventFunction), Function(EventFunction), File(EventFile), Line(EventLine) { Index = Profile::BeginCodeEvent(EventFunction, Function, File, Line); }
	ScopeProfileEvent(const char* EventFunction, const char* EventFile, int EventLine, const char* EventGroup)
		: Name(EventFunction), Group(EventGroup), Function(EventFunction), File(EventFile), Line(EventLine) { Index = Profile::BeginCodeEvent(EventFunction, Function, File, Line, 0, EventGroup); }
		
	ScopeProfileEvent(const char* EventName, const char* EventFunction, const char* EventFile, int EventLine)
		: Name(EventName), Function(EventFunction), File(EventFile), Line(EventLine) { Index = Profile::BeginCodeEvent(Name, Function, File, Line); }
	ScopeProfileEvent(const char* EventName, const char* EventFunction, const char* EventFile, int EventLine, const char* EventGroup)
		: Name(EventName), Group(EventGroup), Function(EventFunction), File(EventFile), Line(EventLine) { Index = Profile::BeginCodeEvent(Name, Function, File, Line, 0, EventGroup); }

	~ScopeProfileEvent() 
	{
		Profile::EndEvent(Index);
	}
};

// Use this if you want to profile something
#ifndef DISABLE_PROFILER
#define PROFILE_BEGIN_FRAME(Name) if (!IsMainThread()) Profile::BeginFrame(Name)
#define PROFILE_END_FRAME(Name) if (!IsMainThread()) Profile::EndFrame()

#define SCOPE_EVENT() \
	ScopeProfileEvent EVENT_CONCAT(autogen_name_, __LINE__)(FUNCTION_SIGNATURE, __FILE__, __LINE__)
#define SCOPE_EVENT_GROUP(Group) \
	ScopeProfileEvent EVENT_CONCAT(autogen_name_, __LINE__)(FUNCTION_SIGNATURE, __FILE__, __LINE__, Group)

#define SCOPE_EVENT_NAME(Name) \
	ScopeProfileEvent EVENT_CONCAT(autogen_name_, __LINE__)(Name, FUNCTION_SIGNATURE, __FILE__, __LINE__)
#define SCOPE_EVENT_NAME_GROUP(Name, Group) \
	ScopeProfileEvent EVENT_CONCAT(autogen_name_, __LINE__)(Name, FUNCTION_SIGNATURE, __FILE__, __LINE__, Group)
#else
#define PROFILE_BEGIN_FRAME(Name)
#define PROFILE_END_FRAME(Name)
#define SCOPE_EVENT()
#define SCOPE_EVENT_GROUP(Group) 
#define SCOPE_EVENT_NAME(Name) 
#define SCOPE_EVENT_NAME_GROUP(Name, Group) 
#endif
