#pragma once
#include "RenderAPI.h"

RENDER_API void uber_deffer(CBlender_Compile& C, bool hq, const char* vspec, const char* pspec, bool aref, const char* detail_replace = 0, bool DO_NOT_FINISH = false, bool DO_NOT_START = false);
// Editor mode: finishes a deferred editor pass like the game elements do. Scene elements mark the
// stencil (lit pixels for the light passes and combine), shadow elements only write depth.
RENDER_API void uber_deffer_editor_end(CBlender_Compile& C);
RENDER_API void uber_forward(CBlender_Compile& C, bool hq, const char* vs, const char* ps, bool aref, bool blend, const char* detail_replace = 0, bool DO_NOT_FINISH = false, bool DO_NOT_START = false);
