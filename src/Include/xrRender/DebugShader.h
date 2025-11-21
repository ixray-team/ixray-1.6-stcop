#pragma once

#include "FactoryPtr.h"
#include "UIShader.h"

#ifdef DEBUG_DRAW
typedef FactoryPtr<IUIShader> debug_shader;
#endif