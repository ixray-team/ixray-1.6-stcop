#pragma once

#include "../../Include/xrRender/Kinematics.h"
#include "../../xrEngine/WristwatchTypes.h"

namespace WristwatchVisual
{
void ResetForModel(const shared_str& modelPath);
void ApplyDisplayShaders(EWristwatchDisplayType displayType, IKinematics* watchesModel);
void Shutdown();
}
