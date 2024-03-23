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

	virtual				void				ResizeWindow(u32 width, u32 height);

	virtual				RENDERDOC_API_1_6_0* GetRenderDocAPI() { return nullptr; };

	virtual				bool				InitRenderDevice(APILevel API) override;
	void* GetRenderDevice() override;
						void				Reset(u32 w, u32 h);
	virtual				void				BeginRender() override;
	virtual				void				EndRender() override;
	virtual				void				Clear();
						void				ProcessEvent(SDL_Event Event);
	void* m_WireShader;
};

#define EDevice (*((CEditorDevice*)(DevicePtr)))