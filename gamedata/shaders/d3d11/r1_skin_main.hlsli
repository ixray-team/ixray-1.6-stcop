#include "skin.hlsli"

R1_SKIN_OUTPUT main(PULL_INPUT)
{
    return _main(PullModel(VertexId));
}
