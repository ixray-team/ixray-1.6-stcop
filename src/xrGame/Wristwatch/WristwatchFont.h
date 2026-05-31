#pragma once

#include "../../xrEngine/device.h"

namespace WristwatchFont
{
void EnsureLoaded();
void Invalidate();
void UpdateGlyphs(SWristwatchHudData& hudData, u32 digit0, u32 digit1, u32 digit2, u32 digit3);
}
