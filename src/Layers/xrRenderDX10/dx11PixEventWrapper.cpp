#include "stdafx.h"

#include "../xrRender/Debug/dxPixEventWrapper.h"

#ifdef DEBUG_DRAW

PixEventWrapper::PixEventWrapper(LPCWSTR wszName)
{
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->BeginEvent(wszName);
    }
}

PixEventWrapper::~PixEventWrapper()
{
    ID3DUserDefinedAnnotation* pAnnotation = (ID3DUserDefinedAnnotation*)g_pAnnotation;

    if (pAnnotation)
    {
        pAnnotation->EndEvent();
    }
}

#endif	//	DEBUG