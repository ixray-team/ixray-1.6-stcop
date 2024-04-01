#pragma once

#include "../../xrEngine/device.h"
#include "../EditorRender/stdafx.h"
#include "../../Layers/xrRender/Shader.h"

class CEditorDevice:
	public CRenderDevice
{
public:
	float m_fNearer;

public: 
	CEditorDevice();
	~CEditorDevice();

	virtual				void	_BCL		AddSeqFrame(pureFrame* f, bool mt) {};
	virtual				void	_BCL		RemoveSeqFrame(pureFrame* f)  {};

						void				_Create(IReader* F);
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

	ref_shader m_WireShader;
	ref_shader m_CurrentShader;
	ref_shader m_SelectionShader;

public:
	IC void			   		SetShader(ref_shader sh) { m_CurrentShader = sh; }
	void			   		DP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 startV, u32 pc);
	void 					DIP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC);


	void 					RenderNearer(float f_Near);
	void 					ResetNearer();
};

#define EDevice (*((CEditorDevice*)(DevicePtr)))