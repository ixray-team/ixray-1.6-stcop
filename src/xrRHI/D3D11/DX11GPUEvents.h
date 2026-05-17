#pragma once

#ifdef IXR_WINDOWS
void  GPUEvents_BeginRendering();
int   GPUEvents_PushEvent(const char* name);
void  GPUEvents_PopEvent(int index);
void  GPUEvents_EndRendering();
const RHI_GPU_EVENT& GPUEvents_Statistics();
#endif