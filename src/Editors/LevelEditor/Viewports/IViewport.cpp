#include "stdafx.h"
#include "IViewport.h"

IViewport::IViewport()
{
	LTools->AddViewport(this);
}

IViewport::~IViewport()
{
	LTools->RemoveViewport(this);
}
