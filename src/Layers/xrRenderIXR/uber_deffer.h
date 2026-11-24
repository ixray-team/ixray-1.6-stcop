#pragma once
#include "RenderAPI.h"

RENDER_API void uber_deffer(CBlender_Compile& C, bool hq, const char* vspec, const char* pspec, bool aref, const char* detail_replace = 0, bool DO_NOT_FINISH = false, bool DO_NOT_START = false);
void uber_ui(CBlender_Compile& C, bool hq, bool aref, bool blend, const char* env, const char* sky);
