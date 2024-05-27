#include "stdafx.h"

void CRenderTarget::phase_gtao()
{
	u32 Offset = 0;
    u32 vertex_color = color_rgba(0, 0, 0, 255);

	//Calculate projection factor, to transform world radius to screen space
	float p_scale = RCache.get_height() / (tan(deg2rad(float(Device.fFOV)) * 0.5f) * 2.0);
	p_scale *= 0.5;

	//Render the AO and view-z into new rendertarget
    u_setrt(rt_gtao_0, nullptr, nullptr, nullptr);
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(FALSE);

	FVF::TL* pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);
	pv->set(-1.0, 1.0, 1.0, 1.0, vertex_color, 0.0, 0.0);
	pv++;
	pv->set(3.0, 1.0, 1.0, 1.0, vertex_color, 2.0, 0.0);
	pv++;
	pv->set(-1.0, -3.0, 1.0, 1.0, vertex_color, 0.0, 2.0);
	pv++;
	RCache.Vertex.Unlock(3, g_combine->vb_stride);

	//Go go power rangers
    RCache.set_Element(s_gtao->E[0]);
	RCache.set_c("gtao_parameters", p_scale);
	RCache.set_Geometry(g_combine);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 3, 0, 1);
	
	//Blur...
    u_setrt(rt_gtao_1, nullptr, nullptr, nullptr);
    RCache.set_CullMode(CULL_NONE);
    RCache.set_Stencil(FALSE);

	pv = (FVF::TL*)RCache.Vertex.Lock(3, g_combine->vb_stride, Offset);
	pv->set(-1.0, 1.0, 1.0, 1.0, vertex_color, 0.0, 0.0);
	pv++;
	pv->set(3.0, 1.0, 1.0, 1.0, vertex_color, 2.0, 0.0);
	pv++;
	pv->set(-1.0, -3.0, 1.0, 1.0, vertex_color, 0.0, 2.0);
	pv++;
	RCache.Vertex.Unlock(3, g_combine->vb_stride);

	//Go go power rangers
    RCache.set_Element(s_gtao->E[1]);
	RCache.set_Geometry(g_combine);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 3, 0, 1);
}