#include "stdafx.h"
#include "R_IBackend.h"

ECORE_API CBackendBase* g_rbackend = nullptr;

CBackendBase::CBackendBase() :
	vb(nullptr), ib(nullptr), vb_stride(0)
{
}

CBackendBase::~CBackendBase()
{
}
