#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_sslr()
{
	GPU_EVENT(phase_sslr);

	//groups
	u32 tgroupsX = (RCache.get_width() + 7u) / 8u;
	u32 tgroupsY = (RCache.get_height() + 7u) / 8u;

	{
		GPU_EVENT(sslr_render);

		//Dummy
		ID3D11UnorderedAccessView* uav_dummy[2] = { nullptr, nullptr };
		ID3D11ShaderResourceView* srv_dummy[16] = {};

		//Shader
		RCache.set_Element(s_sslr->E[0]);

		//Bind UAVs
		ID3D11UnorderedAccessView* uavs[] = { rt_sslr->pUAView, rt_sslr_data->pUAView };
		RContext->CSSetUnorderedAccessViews(0, 2, uavs, nullptr);

		//Dispatch
		RCache.Compute(tgroupsX, tgroupsY, 1);

		//Unbind
		RContext->CSSetUnorderedAccessViews(0, 2, uav_dummy, counts);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}

	{
		GPU_EVENT(sslr_filter);

		ID3D11UnorderedAccessView* uav_dummy = nullptr;
		ID3D11ShaderResourceView* srv_dummy[16] = {};

		RCache.set_Element(s_sslr->E[1]);

		RContext->CSSetUnorderedAccessViews(0, 1, &rt_sslr_temp->pUAView, nullptr);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav_dummy, nullptr);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}

	{
		GPU_EVENT(sslr_temporal);

		ID3D11UnorderedAccessView* uav_dummy = nullptr;
		ID3D11ShaderResourceView* srv_dummy[16] = {};

		RCache.set_Element(s_sslr->E[2]);

		RContext->CSSetUnorderedAccessViews(0, 1, &rt_sslr->pUAView, nullptr);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav_dummy, nullptr);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}

	//LVutner: Meh.
	GRHI->CopySurface(rt_sslr_old->pSurface, rt_sslr->pSurface);
}