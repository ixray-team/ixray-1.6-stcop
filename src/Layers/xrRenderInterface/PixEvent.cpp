#include "stdafx.h"
#include <d3d9.h>
#include <d3d11_1.h>

ID3DUserDefinedAnnotation* Annotator = nullptr;

CPixEvent::CPixEvent(LPCWSTR wszName)
{
	if (g_RenderRHI->API == IRender_RHI::APILevel::DX9)
	{
		D3DPERF_BeginEvent(color_rgba(127, 0, 0, 255), wszName);
	}
	else
	{
		if (Annotator == nullptr)
		{
			ID3D11DeviceContext* Context = (ID3D11DeviceContext*)g_RenderRHI->GetRenderContext();
			Context->QueryInterface(__uuidof(ID3DUserDefinedAnnotation), (void**)&Annotator);
		}

		Annotator->BeginEvent(wszName);
	}
}

CPixEvent::~CPixEvent()
{
	if (g_RenderRHI->API == IRender_RHI::APILevel::DX9)
	{
		D3DPERF_EndEvent();
	}
	else
	{
		Annotator->EndEvent();
	}
}
