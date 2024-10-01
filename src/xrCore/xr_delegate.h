#pragma once
#include "fastdelegate.h"

//template <typename RetType, typename... Args>
//using xr_delegate = xr_delegate<RetType(Args...)>;
#define xr_delegate fastdelegate::FastDelegate
#define xr_make_delegate fastdelegate::MakeDelegate