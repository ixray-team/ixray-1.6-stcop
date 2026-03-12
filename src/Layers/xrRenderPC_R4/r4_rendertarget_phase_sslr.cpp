#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_sslr()
{
	GPU_EVENT(phase_sslr);

	//groups
	const UINT tgroupsX = (RCache.get_width() + 7u) / 8u;
	const UINT tgroupsY = (RCache.get_height() + 7u) / 8u;

	{
		GPU_EVENT(sslr_render);

		//Dummy
		ID3D11UnorderedAccessView* uav_dummy[2] = { nullptr, nullptr };
		ID3D11ShaderResourceView* srv_dummy[16] = {};

		//Shader setup... can't use set_element because of set_PS bullshit
	    ShaderElement* S;
        S = (&*(s_sslr->E[0]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		//Bind UAVs
		UINT UAVInitialCounts = 1;

		ID3D11UnorderedAccessView* our_uav[2] = {
            reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr->pUAView->GetRaw()),
            reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr_data->pUAView->GetRaw())
		};

		RContext->CSSetUnorderedAccessViews(0, 2, our_uav, &UAVInitialCounts);

		//Dispatch
		RCache.Compute(tgroupsX, tgroupsY, 1);

		//Unbind
		RContext->CSSetUnorderedAccessViews(0, 2, uav_dummy, &UAVInitialCounts);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}


	{
		GPU_EVENT(sslr_filter);

		ID3D11UnorderedAccessView* uav_dummy = nullptr;
		ID3D11ShaderResourceView* srv_dummy[16] = {};

	    ShaderElement* S;
        S = (&*(s_sslr->E[1]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		UINT UAVInitialCounts = 1;

		ID3D11UnorderedAccessView* our_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr_temp->pUAView->GetRaw());

		RContext->CSSetUnorderedAccessViews(0, 1, &our_uav, &UAVInitialCounts);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav_dummy, &UAVInitialCounts);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}

	{
		GPU_EVENT(sslr_temporal);

		ID3D11UnorderedAccessView* uav_dummy = nullptr;
		ID3D11ShaderResourceView* srv_dummy[16] = {};

	    ShaderElement* S;
        S = (&*(s_sslr->E[2]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		UINT UAVInitialCounts = 1;

		ID3D11UnorderedAccessView* our_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr->pUAView->GetRaw());

		RContext->CSSetUnorderedAccessViews(0, 1, &our_uav, &UAVInitialCounts);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav_dummy, &UAVInitialCounts);
		RContext->CSSetShaderResources(0, 16, srv_dummy);

		//LVutner: Meh.
		GRHI->CopySurface(rt_sslr_old->pSurface, rt_sslr->pSurface);
	}

}