#include "../../RHI.h"
#include <imgui.h>

#ifdef IXR_WINDOWS
#	include <d3d9.h>
#	include <d3d11.h>

#	include "imgui_impl_dx9.h"
#	include "imgui_impl_dx11.h"

	ImGui_ImplDX11_Data* ImGui_ImplDX11_GetBackendData();

#	define DX9Device ((IDirect3DDevice9*)GRHI->DevicePtr->RawDevice)
#	define DX11Device ((ID3D11Device*)GRHI->DevicePtr->RawDevice)
#	define DX11Context ((ID3D11DeviceContext*)GRHI->GetContext())

#endif

RHI_API void RHIUtils::ImGui::Init()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D9:	ImGui_ImplDX9_Init(DX9Device); break;
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_Init(DX11Device, DX11Context);  break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::NewFrame()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D9:	ImGui_ImplDX9_NewFrame(); break;
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_NewFrame();  break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::DrawData()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D9:	ImGui_ImplDX9_RenderDrawData(::ImGui::GetDrawData()); break;
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_RenderDrawData(::ImGui::GetDrawData());  break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::Destroy()
{
	switch (GRHI->APILevel)
	{
#ifdef IXR_WINDOWS
		case ERHI_API_LAYER::D3D9:	ImGui_ImplDX9_Shutdown(); break;
		case ERHI_API_LAYER::D3D11: ImGui_ImplDX11_Shutdown();  break;
#endif
	}
}

RHI_API void RHIUtils::ImGui::Reset()
{
#ifdef IXR_WINDOWS
	if (GRHI->APILevel == ERHI_API_LAYER::D3D9)
	{
		ImGui_ImplDX9_InvalidateDeviceObjects();
	}
#endif
}

RHI_API void* RHIUtils::ImGui::GetBlenderState()
{
#ifdef IXR_WINDOWS
	if (GRHI->APILevel == ERHI_API_LAYER::D3D11)
	{
		if (auto State = ImGui_ImplDX11_GetBackendData())
		{
			return State->pBlendState;
		}
	}
#endif

	return nullptr;
}