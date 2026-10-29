#pragma once
#ifdef XREPROPS_EXPORTS
#define XREPROPS_API __declspec(dllexport)
#else
#define XREPROPS_API __declspec(dllimport)
#endif

#include <limits>
#include "../../xrCore/xrCore.h"
#include "../../xrEngine/stdafx.h"
#include "../Public/xrEProps.h"
#include "ChooseTypes.H"
#include "ItemListHelper.h"
