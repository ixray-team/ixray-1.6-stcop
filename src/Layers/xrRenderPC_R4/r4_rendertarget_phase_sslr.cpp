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

		//Shader setup... can't use set_element because of set_PS bullshit
	    ShaderElement* S;
        S = (&*(s_sslr->E[0]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		//Bind UAVs... using RHI.
		GRHI->SetUnorderedAccessViews(rt_sslr->pUAView, 0);
		GRHI->SetUnorderedAccessViews(rt_sslr_data->pUAView, 1);

		//Dispatch
		RCache.Compute(tgroupsX, tgroupsY, 1);

		//Unbind - is it right for this whole RHI thing? 
		RContext->CSSetUnorderedAccessViews(0, 2, uav_dummy, nullptr);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}

	{
		GPU_EVENT(sslr_filter);

		ID3D11UnorderedAccessView* uav_dummy = nullptr;
		ID3D11ShaderResourceView* srv_dummy[16] = {};
		ID3D11UnorderedAccessView* huj = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr_temp->pUAView);

	    ShaderElement* S;
        S = (&*(s_sslr->E[1]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		GRHI->SetUnorderedAccessViews(rt_sslr_temp->pUAView, 0);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav_dummy, nullptr);
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

		GRHI->SetUnorderedAccessViews(rt_sslr->pUAView, 0);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav_dummy, nullptr);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}

	//LVutner: Meh.
	GRHI->CopySurface(rt_sslr_old->pSurface, rt_sslr->pSurface);
}