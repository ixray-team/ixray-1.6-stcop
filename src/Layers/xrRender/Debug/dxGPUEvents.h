#pragma once
#include "../../xrCore/_types.h"
#include "../../xrCore/shared_string.h"

#define QUERY_MAX_COUNT 1024

struct gpu_event_stats
{
    u64 freq;
    u64 begin;
    u64 end;
    u64 stack;
    shared_str name;
};

struct gpu_events_perf
{
    u64 count;
    xr_array<gpu_event_stats, QUERY_MAX_COUNT> events;
};

void GPUEvents_BeginRendering();
int GPUEvents_PushEvent(const char* name);
void GPUEvents_PopEvent(int index);
void GPUEvents_EndRendering();
const gpu_events_perf& GPUEvents_Statistics();