#include "common\common_iostructs.hlsli"

VSOutputFullscreen vs_main(uint VertexId : SV_VertexID)
{
    return FullscreenTriangle(VertexId);
}
