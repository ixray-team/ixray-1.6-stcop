#pragma once

#include "../../xrEngine/device.h"

class CEditorDevice:
	public CRenderDevice
{
public:

public: 
	CEditorDevice();
	~CEditorDevice();

	virtual				void	_BCL		AddSeqFrame(pureFrame* f, bool mt) {};
	virtual				void	_BCL		RemoveSeqFrame(pureFrame* f)  {};

	virtual				void				DestroyRenderDevice()  {};

	virtual				void				ResizeWindow(u32 width, u32 height) {};

	virtual				RENDERDOC_API_1_6_0* GetRenderDocAPI() { return nullptr; };

	virtual				void				BeginRender() {};
	virtual				void				EndRender() {};
	void* m_WireShader;
};

#define EDevice (*((CEditorDevice*)(DevicePtr.get())))