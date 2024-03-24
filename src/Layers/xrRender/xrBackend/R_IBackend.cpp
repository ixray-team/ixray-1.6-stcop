#include "stdafx.h"
#include "R_IBackend.h"

CBackendBase::CBackendBase() :
	vb(nullptr), ib(nullptr), vb_stride(0)
{
}

CBackendBase::~CBackendBase()
{
}
