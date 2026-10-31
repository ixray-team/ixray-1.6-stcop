#include "common_iostructs.hlsli"

VSOutputFullscreen main(uint VertexId : SV_VertexID)
{
    return FullscreenTriangle(VertexId);
}
