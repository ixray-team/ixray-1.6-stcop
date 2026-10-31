#include "stdafx.h"
#include "r4_rendertarget.h"

void	CRenderTarget::u_calc_tc_noise(GfxCmdList& Cmd, Fvector2& p0, Fvector2& p1, float Scale, float FPS, bool IsScope)
{
	CTexture*	T					= Cmd.GetTexture	(ShaderBindings::Texture("s_noise"));
	VERIFY2		(T, "s_noise in noise shader should be setted up");
	u32			tw					= iCeil(float(T->get_Width	())*std::max(Scale, EPS_L)+EPS_S);
	u32			th					= iCeil(float(T->get_Height ())*std::max(Scale, EPS_L)+EPS_S);
	VERIFY2		(tw && th, "Noise scale can't be zero in any way");

	float& NoiseTime = IsScope ? ScopeNoiseTime : im_noise_time;
	u32& ShiftWidth = IsScope ? ScopeNoiseShiftWidth : im_noise_shift_w;
	u32& ShiftHeight = IsScope ? ScopeNoiseShiftHeight : im_noise_shift_h;
	// calculate shift from FPSes
	NoiseTime					-= Device.fTimeDelta;
	if (NoiseTime<0)			{
		ShiftWidth			= ::Random.randI(tw?tw:1);
		ShiftHeight			= ::Random.randI(th?th:1);
		float	fps_time			= 1 / std::max(FPS, EPS_L);
		while (NoiseTime<0)		NoiseTime += fps_time;
	}

	u32			shift_w				= ShiftWidth;
	u32			shift_h				= ShiftHeight;
	float		start_u				= (float(shift_w)+.5f)/(tw);
	float		start_v				= (float(shift_h)+.5f)/(th);
	u32			_w					= (u32)Cmd.GetTargetWidth();
	u32			_h					= (u32)Cmd.GetTargetHeight();
	float CountWidth = float(_w) / float(tw);
	float CountHeight = float(_h) / float(th);
	float		end_u				= start_u + CountWidth;
	float		end_v				= start_v + CountHeight;
 
	p0.set		(start_u,	start_v	);
	p1.set		(end_u,		end_v	);
}

void CRenderTarget::u_calc_tc_duality_ss	(Fvector2& r0, Fvector2& r1, Fvector2& l0, Fvector2& l1)
{
	// Calculate ordinaty TCs from blur and SS
	float	tw			= float(dwWidth);
	float	th			= float(dwHeight);
	Fvector2			shift,p0,p1;
	shift.set			(.5f/tw, .5f/th);
	shift.mul			(param_blur);
	p0.set				(.5f/tw, .5f/th).add			(shift);
	p1.set				((tw+.5f)/tw, (th+.5f)/th ).add	(shift);

	// Calculate Duality TC
	float	shift_u		= param_duality_h*.5f;
	float	shift_v		= param_duality_v*.5f;

	r0.set(p0.x,p0.y);					r1.set(p1.x-shift_u,p1.y-shift_v);
	l0.set(p0.x+shift_u,p0.y+shift_v);	l1.set(p1.x,p1.y);
}

bool CRenderTarget::u_need_PP	()
{
	bool	_blur	= (param_blur>0.001f);
	bool	_gray	= (param_gray>0.001f);
	bool	_noise	= (param_noise>0.001f);
	bool	_dual	= (param_duality_h>0.001f)||(param_duality_v>0.001f);

	// bool	_menu_pp= g_pGamePersistent?g_pGamePersistent->OnRenderPPUI_query():false;

	bool	_cbase	= false;
	{
		int		_r	= color_get_R(param_color_base)	; _r=std::abs(_r-int(0x7f));
		int		_g	= color_get_G(param_color_base)	; _g=std::abs(_g-int(0x7f));
		int		_b	= color_get_B(param_color_base)	; _b=std::abs(_b-int(0x7f));
		if (_r>2 || _g>2 || _b>2)	_cbase	= true	;
	}
	bool	_cadd	= false;
	{
		//int		_r	= color_get_R(param_color_add)	;
		//int		_g	= color_get_G(param_color_add)	;
		//int		_b	= color_get_B(param_color_add)	;
		//if (_r>2 || _g>2 || _b>2)	_cadd	= true	;
		int		_r	= std::abs((int)(param_color_add.x*255));
		int		_g	= std::abs((int)(param_color_add.y*255));
		int		_b	= std::abs((int)(param_color_add.z*255));
		if (_r>2 || _g>2 || _b>2)	_cadd	= true	;
	}
	return _blur || _gray || _noise || _dual || _cbase || _cadd || u_need_CM(); 
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
		uv[2].set(u2,v2); 
	}
};

void CRenderTarget::phase_pp(GfxCmdList& Cmd, const SPPInfo* ScopeInfo)
{
	// combination/postprocess
	u32 _w = (u32)Cmd.GetTargetWidth();
	u32 _h = (u32)Cmd.GetTargetHeight();
	u_setrt(Cmd, _w, _h, ScopeInfo ? rt_Accumulator->pRT : rt_BackbufferLUT->pRT, nullptr, nullptr, nullptr);
	const float Gray = ScopeInfo ? ScopeInfo->gray : param_gray;
	const float Noise = ScopeInfo ? ScopeInfo->noise.intensity : param_noise;
	SPPInfo::SColor ScopeBaseColor = ScopeInfo ? ScopeInfo->color_base : pp_identity.color_base;
	const u32 BaseColor = ScopeInfo ? u32(ScopeBaseColor) : param_color_base;
	SPPInfo::SColor ScopeGrayColor = ScopeInfo ? ScopeInfo->color_gray : pp_identity.color_gray;
	const u32 GrayColor = ScopeInfo ? u32(ScopeGrayColor) : param_color_gray;
	Fvector Brightness = param_color_add;
	if (ScopeInfo)
	{
		Brightness.set(ScopeInfo->color_add.r, ScopeInfo->color_add.g, ScopeInfo->color_add.b);
	}
	const float MapInfluence = ScopeInfo ? ScopeInfo->cm_influence : param_color_map_influence;
	const float MapInterpolate = ScopeInfo ? ScopeInfo->cm_interpolate : param_color_map_interpolate;
	if (ScopeInfo)
	{
		ScopeColorMaps.SetTextures(ScopeInfo->cm_tex1, ScopeInfo->cm_tex2.size() ? ScopeInfo->cm_tex2 : ScopeInfo->cm_tex1);
	}
	//	Element 0 for for normal post-process
	//	Element 4 for color map post-process
	bool bCMap = MapInfluence > 0.001f;

	Cmd.SetPass((ScopeInfo ? ScopePostprocessShader : s_postprocess)->E[bCMap ? 4 : 0]);

	int		gblend		= clampr		(iFloor((1-Gray)*255.f),0,255);
	int		nblend		= clampr		(iFloor((1-Noise)*255.f),0,255);
	u32					p_color			= subst_alpha(BaseColor,nblend);
	u32					p_gray			= subst_alpha(GrayColor,gblend);
	Fvector				p_brightness = Brightness	;

	// Draw full-screen quad textured with our scene image
	u32		Offset;
	
	Fvector2			n0,n1,r0,r1,l0,l1;
	u_calc_tc_duality_ss	(r0,r1,l0,l1);
	if (ScopeInfo)
	{
		const float Shift = ScopeInfo->blur * 0.5f;
		r0.set((0.5f + Shift) / _w, (0.5f + Shift) / _h);
		r1.set(1.0f + r0.x - ScopeInfo->duality.h * 0.5f, 1.0f + r0.y - ScopeInfo->duality.v * 0.5f);
		l0.set(r0.x + ScopeInfo->duality.h * 0.5f, r0.y + ScopeInfo->duality.v * 0.5f);
		l1.set(1.0f + r0.x, 1.0f + r0.y);
	}
	u_calc_tc_noise(Cmd, n0, n1, ScopeInfo ? ScopeInfo->noise.grain : param_noise_scale, ScopeInfo ? ScopeInfo->noise.fps : param_noise_fps, ScopeInfo != nullptr);

	// Fill vertex buffer
	float				du	= ps_r1_pps_u, dv = ps_r1_pps_v;
	TL_2c3uv* pv			= (TL_2c3uv*) Cmd.Vertex.Lock	(4,g_postprocess.stride(),Offset);
	pv->set(du+0,			dv+float(_h),	p_color, p_gray, r0.x, r1.y, l0.x, l1.y, n0.x, n1.y);	pv++;
	pv->set(du+0,			dv+0,			p_color, p_gray, r0.x, r0.y, l0.x, l0.y, n0.x, n0.y);	pv++;
	pv->set(du+float(_w),	dv+float(_h),	p_color, p_gray, r1.x, r1.y, l1.x, l1.y, n1.x, n1.y);	pv++;
	pv->set(du+float(_w),	dv+0,			p_color, p_gray, r1.x, r0.y, l1.x, l0.y, n1.x, n0.y);	pv++;
	Cmd.Vertex.Unlock										(4,g_postprocess.stride());

	// Actual rendering
	Cmd.Constants.Write<CBFrame>().c_brightness.set(p_brightness.x, p_brightness.y, p_brightness.z, 0);
	Cmd.Constants.Write<CBFrame>().c_colormap.set(MapInfluence, MapInterpolate, 0, 0);
	if (ScopeInfo)
	{
		// The scope shader converts HDR to display colors the same way as combine_2
		Cmd.Constants.Write<CBPost>().bloom_params.set(ps_r2_bloom_amount, ps_r2_bloom_desaturation, ps_r2_bloom_tint_amount, 0.0f);
	}
	Cmd.SetGeometry	(g_postprocess);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 6, 0, Offset);
}
