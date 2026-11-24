#ifndef pnoiseH
#define pnoiseH
#include "../RenderAPI.h"

RENDER_API float noise3(const Fvector& vec);
RENDER_API float fractalsum3(const Fvector& v, float freq, int octaves);
RENDER_API float turbulence3(const Fvector& v, float freq, int octaves);
#endif