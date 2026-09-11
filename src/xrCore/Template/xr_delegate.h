#pragma once
#include "fastdelegate.h"

namespace
{
	//template <typename RetType, typename... Args>
	//using xr_delegate = xr_delegate<RetType(Args...)>;

	template <typename Functor>
	using xr_delegate = fastdelegate::FastDelegate<Functor>;
};

#define xr_make_delegate fastdelegate::MakeDelegate
