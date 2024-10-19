#pragma once

PARTICLES_API float noise3(const Fvector& vec);
PARTICLES_API float fractalsum3(const Fvector& v, float freq, int octaves);
PARTICLES_API float turbulence3(const Fvector& v, float freq, int octaves);
