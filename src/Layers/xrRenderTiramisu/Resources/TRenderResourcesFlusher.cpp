#include "TRenderResourcesFlusher.h"

TRenderResourcesFlusher::~TRenderResourcesFlusher()
{
    FlushNextFrame();
}

void TRenderResourcesFlusher::FlushNextFrame()
{
    for (nri::Buffer* Buffer : Buffers)
    {
        GRenderDevice.CoreInterface.DestroyBuffer(Buffer);
    }
    Buffers.clear();
    for (nri::Memory* Memory : Memories)
    {
        GRenderDevice.CoreInterface.FreeMemory(Memory);
    }
    Memories.clear();
}

void TRenderResourcesFlusher::Push(nri::Memory* InMemory)
{
    if (!InMemory)
    {
        return;
    }
    Memories.push_back(InMemory);
}

void TRenderResourcesFlusher::Push(nri::Buffer* InBuffer)
{
    if (!InBuffer)
    {
        return;
    }
    Buffers.push_back(InBuffer);
}
