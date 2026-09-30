#pragma once

#include "UICompassLabelTypes.h"

namespace CompassLabelGenerator
{
	void Generate(const SCompassLabelSettings& settings, xr_vector<SCompassLabelDesc>& outMarks);
}
