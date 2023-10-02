#include "stdafx.h"
#include "r4_rendertarget.h"

void set_viewport(ID3DDeviceContext* dev, float w, float h);

void	CRenderTarget::u_calc_tc_noise		(Fvector2& p0, Fvector2& p1)
{
	p0.set		(0,	0	);
	p1.set		(1,	1	);
}

void CRenderTarget::u_calc_tc_duality_ss	(Fvector2& r0, Fvector2& r1, Fvector2& l0, Fvector2& l1)
{
	Fvector2			p0,p1;
	p0.set				(0, 0);
	p1.set				(1, 1);

	// Calculate Duality TC
	r0.set(p0.x,p0.y);					
	r1.set(p1.x,p1.y);
	l0.set(p0.x,p0.y);	
	l1.set(p1.x,p1.y);
}

bool CRenderTarget::u_need_CM()
{
	return (param_color_map_influence>0.001f);
}

struct TL_2c3uv		{
	Fvector4	p		;
	u32			color0	;
	u32			color1	;
	Fvector2	uv	[3]	;
	IC void	set	(float x, float y, u32 c0, u32 c1, float u0, float v0, float u1, float v1, float u2, float v2)	{	
		p.set	(x,y,EPS_S,1.f); 
		color0 = c0; 
		color1 = c1;
		uv[0].set(u0,v0); 
		uv[1].set(u1,v1); 
		uv[2].set(u2, v2);
	}
};

void CRenderTarget::phase_pp()
{
	PIX_EVENT(phase_pp);

	// combination/postprocess
	float	_w = RCache.get_target_width();
	float	_h = RCache.get_target_height();
	set_viewport(HW.pContext, _w, _h);
	u_setrt(_w, _h, HW.pBaseRT, NULL, NULL, nullptr);

	//	Element 0 for for normal post-process
	//	Element 4 for color map post-process
	bool	bCMap = u_need_CM();
	RCache.set_Element(s_postprocess->E[bCMap ? 4 : 0]);

	int gblend = clampr(iFloor((1 - param_gray) * 255.f), 0, 255);
	int	nblend = clampr(iFloor((1 - param_noise) * 255.f), 0, 255);
	u32 p_color = subst_alpha(param_color_base, nblend);
	u32 p_gray = subst_alpha(param_color_gray, gblend);
	Fvector	p_brightness = param_color_add;

	Fvector2 n0, n1, r0, r1, l0, l1;
	u_calc_tc_duality_ss(r0, r1, l0, l1);
	u_calc_tc_noise(n0, n1);

	// Fill vertex buffer
	u32	Offset = 0;
	TL_2c3uv* pv = (TL_2c3uv*)RCache.Vertex.Lock(4, g_postprocess.stride(), Offset);
	pv->set(0, float(_h), p_color, p_gray, r0.x, r1.y, l0.x, l1.y, n0.x, n1.y);	pv++;
	pv->set(0, 0, p_color, p_gray, r0.x, r0.y, l0.x, l0.y, n0.x, n0.y);	pv++;
	pv->set(float(_w), float(_h), p_color, p_gray, r1.x, r1.y, l1.x, l1.y, n1.x, n1.y);	pv++;
	pv->set(float(_w), 0, p_color, p_gray, r1.x, r0.y, l1.x, l0.y, n1.x, n0.y);	pv++;
	RCache.Vertex.Unlock(4, g_postprocess.stride());

	// Actual rendering
	static shared_str s_brightness = "c_brightness";
	static shared_str s_colormap = "c_colormap";
	RCache.set_c(s_brightness, p_brightness.x, p_brightness.y, p_brightness.z, 0);
	RCache.set_c(s_colormap, param_color_map_influence, param_color_map_interpolate, 0, 0);
	RCache.set_Geometry(g_postprocess);
	RCache.Render(D3DPT_TRIANGLELIST, Offset, 0, 4, 0, 2);
}
