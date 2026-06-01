#pragma once

class TRenderResourcesFlusher
{
public:
    ~TRenderResourcesFlusher();
    void FlushNextFrame();
    
    void Push(nri::Memory* InMemory);
    void Push(nri::Buffer* InBuffer);
private:
    xr_vector<nri::Memory*> Memories;
    xr_vector<nri::Buffer*> Buffers;
    
};
