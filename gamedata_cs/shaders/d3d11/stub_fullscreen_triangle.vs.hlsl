#include "common_iostructs.hlsli"

VSOutputFullscreen main(VSInputFullscreen I)
{
    VSOutputFullscreen O;

    O.hpos = I.hpos;
    O.texcoord = I.texcoord;

    return O;
}
