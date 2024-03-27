#include "stdafx.h"
#include "imgui_impl_dx9.h"
#include "imgui_impl_sdl3.h"

CEditorDevice::CEditorDevice()
{
	psDeviceFlags.set(rsFullscreen, false);
	DevicePtr = this;
}

CEditorDevice::~CEditorDevice()
{
	DevicePtr = nullptr;
}

void CEditorDevice::_Create(IReader* F)
{
	b_is_Ready = TRUE;

	// General Render States
	_SetupStates();

	RCache.OnDeviceCreate();
	//Resources->OnDeviceCreate(F);

	::RImplementation.OnDeviceCreate();

	m_WireShader.create("editor\\wire");
	m_SelectionShader.create("editor\\selection");

	// signal another objects
	//UI->OnDeviceCreate();

	//pSystemFont = new CGameFont("hud_font_small");
}

void CEditorDevice::ResizeWindow(u32 width, u32 height)
{
	TargetWidth = width;
	TargetHeight = height;
}

bool CEditorDevice::InitRenderDevice(APILevel API)
{
	CRenderDevice::InitRenderDevice(API);

	ImGui_ImplSDL3_InitForD3D(g_AppInfo.Window);
	IDirect3DDevice9* Ptr = (IDirect3DDevice9*)CEditorDevice::GetRenderDevice();
	ImGui_ImplDX9_Init(Ptr);

	return true;
}

void* CEditorDevice::GetRenderDevice()
{
	return CRenderDevice::GetRenderDevice();
}

void CEditorDevice::Reset(u32 w, u32 h)
{
	ImGui_ImplDX9_InvalidateDeviceObjects();
	psCurrentVidMode[0] = Device.TargetWidth = w;
	psCurrentVidMode[1] = Device.TargetHeight = h;

	CRenderDevice::ResizeBuffers(Device.TargetWidth, Device.TargetHeight);
}

void CEditorDevice::BeginRender()
{
	ImGui_ImplDX9_NewFrame();
	ImGui_ImplSDL3_NewFrame();
}

void CEditorDevice::EndRender()
{
	((IDirect3DDevice9*)GetRenderDevice())->BeginScene();
	ImGui_ImplDX9_RenderDrawData(ImGui::GetDrawData());
	HRESULT result = ((IDirect3DDevice9*)GetRenderDevice())->Present(nullptr, nullptr, nullptr, nullptr);
	((IDirect3DDevice9*)GetRenderDevice())->EndScene();

	((IDirect3DDevice9*)GetRenderDevice())->Present(nullptr, nullptr, nullptr, nullptr);
}

void CEditorDevice::Clear()
{
	ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
	((IDirect3DDevice9*)GetRenderDevice())->SetRenderState(D3DRS_ZENABLE, FALSE);
	((IDirect3DDevice9*)GetRenderDevice())->SetRenderState(D3DRS_ALPHABLENDENABLE, FALSE);
	((IDirect3DDevice9*)GetRenderDevice())->SetRenderState(D3DRS_SCISSORTESTENABLE, FALSE);

	D3DCOLOR clear_col_dx = D3DCOLOR_RGBA((int)(clear_color.x * clear_color.w * 255.0f), (int)(clear_color.y * clear_color.w * 255.0f), (int)(clear_color.z * clear_color.w * 255.0f), (int)(clear_color.w * 255.0f));
	((IDirect3DDevice9*)GetRenderDevice())->Clear(0, nullptr, D3DCLEAR_TARGET | D3DCLEAR_ZBUFFER, clear_col_dx, 1.0f, 0);
}

void CEditorDevice::ProcessEvent(SDL_Event Event)
{
	ImGui_ImplSDL3_ProcessEvent(&Event);
}

void CEditorDevice::DP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 vBase, u32 pc)
{
	ref_shader S = m_CurrentShader ? m_CurrentShader : m_WireShader;
	u32 dwRequired = S->E[0]->passes.size();

	RCache.set_Geometry(geom);

	for (u32 dwPass = 0; dwPass < dwRequired; dwPass++) 
	{
		RCache.set_Shader(S, dwPass);
		RCache.Render(pt, vBase, pc);
	}
}

void CEditorDevice::DIP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	ref_shader S = m_CurrentShader ? m_CurrentShader : m_WireShader;
	u32 dwRequired = S->E[0]->passes.size();

	RCache.set_Geometry(geom);

	for (u32 dwPass = 0; dwPass < dwRequired; dwPass++)
	{
		RCache.set_Shader(S, dwPass);
		RCache.Render(pt, baseV, startV, countV, startI, PC);
	}
}

void CEditorDevice::RenderNearer(float f_Near)
{
	mProject._43 = m_fNearer - f_Near;
	RCache.set_xform_project(mProject);
}

void CEditorDevice::ResetNearer()
{
	mProject._43 = m_fNearer;
	RCache.set_xform_project(mProject);
}
