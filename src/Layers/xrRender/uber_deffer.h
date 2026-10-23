#pragma once

void uber_deffer(CBlender_Compile& C, bool hq, const char* vspec, const char* pspec, bool aref, const char* detail_replace = 0, bool DO_NOT_FINISH = false, bool DO_NOT_START = false);
void uber_forward(CBlender_Compile& C, bool hq, const char* vs, const char* ps, bool aref, bool blend, const char* detail_replace = 0, bool DO_NOT_FINISH = false, bool DO_NOT_START = false);
bool UIIconShadersAvailable();

#if RENDER == R_R2
// Forward-lit 3D icon pass for the D3D9 dynamic renderer.
void uber_deffer_ui(CBlender_Compile& C, bool Blend, bool Aref);
#endif
