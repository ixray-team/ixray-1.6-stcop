#include "stdafx.h"
#include "r1_LightShadows.h"
#include "Blender_Shadow_World.h"
#include "Blender_Blur.h"
#include "LightTrack.h"
#include "../../xrEngine/xr_object.h"
#include "FBasicVisual.h"
#include "../../xrEngine/CustomHUD.h"

const	float		S_distance		= 48;
const	float		S_distance2		= S_distance*S_distance;
const	float		S_ideal_size	= 4.f;		
const	float		S_fade			= 4.5;
const	float		S_fade2			= S_fade*S_fade;

const	float		S_level			= .05f;		
const	int			S_size			= 85;
const	int			S_rt_size		= 512;
const	int			batch_size		= 256;
const	float		S_tess			= .5f;
const	int 		S_ambient		= 32;
const	int 		S_clip			= 256-8;
const	ERHI_FORMAT	S_rtf = ERHI_FORMAT::B8G8R8A8_UNORM;
const	float		S_blur_kernel	= 0.75f;

const	u32			cache_old		= 30*1000;	

static void ApplyBlur4		(FVF::TL4uv* pv, u32 w, u32 h, float k)
{
	float	_w					= float(w);
	float	_h					= float(h);
	float	kw					= (1.f/_w)*k;
	float	kh					= (1.f/_h)*k;
	Fvector2					p0,p1;
	p0.set						(.5f/_w, .5f/_h);
	p1.set						((_w+.5f)/_w, (_h+.5f)/_h );
	u32		_c					= 0xffffffff;

	pv->p.set(EPS,			float(_h+EPS),	EPS,1.f); pv->color=_c; pv->uv[0].set(p0.x-kw,p1.y-kh);pv->uv[1].set(p0.x+kw,p1.y+kh);pv->uv[2].set(p0.x+kw,p1.y-kh);pv->uv[3].set(p0.x-kw,p1.y+kh);pv++;
	pv->p.set(EPS,			EPS,			EPS,1.f); pv->color=_c; pv->uv[0].set(p0.x-kw,p0.y-kh);pv->uv[1].set(p0.x+kw,p0.y+kh);pv->uv[2].set(p0.x+kw,p0.y-kh);pv->uv[3].set(p0.x-kw,p0.y+kh);pv++;
	pv->p.set(float(_w+EPS),float(_h+EPS),	EPS,1.f); pv->color=_c; pv->uv[0].set(p1.x-kw,p1.y-kh);pv->uv[1].set(p1.x+kw,p1.y+kh);pv->uv[2].set(p1.x+kw,p1.y-kh);pv->uv[3].set(p1.x-kw,p1.y+kh);pv++;
	pv->p.set(float(_w+EPS),EPS,			EPS,1.f); pv->color=_c; pv->uv[0].set(p1.x-kw,p0.y-kh);pv->uv[1].set(p1.x+kw,p0.y+kh);pv->uv[2].set(p1.x+kw,p0.y-kh);pv->uv[3].set(p1.x-kw,p0.y+kh);pv++;
}

CLightShadows::CLightShadows()
{
	current	= 0;
	RT		= 0;

	LPCSTR	RTname			= "$user$shadow";
	LPCSTR	RTtemp			= "$user$temp";
	string128 RTname2;		xr_strconcat(RTname2,RTname,",",RTname);
	string128 RTtemp2;		xr_strconcat(RTtemp2,RTtemp,",",RTtemp);

	RT.create				(RTname,S_rt_size,S_rt_size,S_rtf);
	RT_temp.create			(RTtemp,S_rt_size,S_rt_size,S_rtf);
	CBlender_ShWorld world_blender;
	sh_World.create(&world_blender, "effects\\shadow_world", RTname);
	geom_World.create		(FVF::F_LIT,	RCache.Vertex.Buffer(), nullptr);
	CBlender_Blur blur_blender;
	sh_BlurTR.create(&blur_blender, "blur4", RTtemp2);
	sh_BlurRT.create(&blur_blender, "blur4", RTname2);
	geom_Blur.create		(FVF::F_TL4uv,	RCache.Vertex.Buffer(), RCache.QuadIB);

}

CLightShadows::~CLightShadows()
{
	
	sh_Screen.destroy		();
	geom_Screen.destroy		();

	geom_Blur.destroy		();
	geom_World.destroy		();

	sh_BlurRT.destroy		();
	sh_BlurTR.destroy		();
	sh_World.destroy		();
	RT_temp.destroy			();
	RT.destroy				();

	for (u32 it=0; it<casters_pool.size(); it++)
		xr_delete(casters_pool[it]);
	casters_pool.clear		();

	for (u32 it=0; it<cache.size(); it++)
		xr_free	(cache[it].tris);
	cache.clear				();
}

void CLightShadows::set_object	(IRenderable* O)
{
	if (0==O)	current		= 0;
	else 
	{
		if (!O->renderable_ShadowGenerate()	|| RImplementation.val_bHUD || ((CROS_impl*)O->renderable_ROS())->shadow_gen_frame==Device.dwFrame)
		{
			current		= 0;
			return;
		}

		const vis_data	&vis = O->renderable.visual->getVisData();
		Fvector		C;	O->renderable.xform.transform_tiny		(C,vis.sphere.P);
		float		R				= vis.sphere.R;
		float		D				= C.distance_to(Device.vCameraPosition)+R;

		float		_priority		= (D/S_distance)*(S_ideal_size/(R+EPS));
		if (_priority<1.f)		current	= O;
		else					current = 0;
		
		if (current)
		{
			((CROS_impl*)O->renderable_ROS())->shadow_gen_frame	=	Device.dwFrame;

			caster*	cs		= nullptr;
			if (casters_pool.empty())	cs	= new caster ();
			else {
				cs	= casters_pool.back	();
				casters_pool.pop_back	();
			}

			casters.push_back	(cs);
			cs->O				= current;
			cs->C				= C;
			cs->D				= D;
			cs->nodes.clear		();
		}
	}
}

void CLightShadows::add_element	(NODE& N)
{
	if (0==current)										return;
	VERIFY2	(casters.back()->nodes.size()<24,"Object exceeds limit of 24 renderable parts/materials");
	if (0==N.pVisual->shader->E[SE_R1_LMODELS]._get())	return;
	casters.back()->nodes.push_back		(N);
}

void CLightShadows::calculate	()
{
	if (casters.empty())		return;

	bool bRTS = false;
	Device.Statistic->RenderDUMP_Scalc.Begin	();


	int	slot_id		= 0;
	int slot_line	= S_rt_size/S_size;
	int slot_max	= slot_line*slot_line;
	const float	eps = 2*EPS_L;
	for (u32 o_it=0; o_it<casters.size(); o_it++)
	{
		caster&	C	= *casters	[o_it];
		if (C.nodes.empty())	continue;

		CROS_impl* LT			= (CROS_impl*)C.O->renderable_ROS();
		xr_vector<CROS_impl::Light>& lights = LT->lights;

		for (u32 l_it=0; (l_it<lights.size()) && (slot_id<slot_max); l_it++)
		{
			CROS_impl::Light&	L			=	lights[l_it];
			if (L.energy<S_level)			continue;

			if (!bRTS)	{
				bRTS						= true;
				RImplementation.Target->u_setrt(RT_temp, nullptr, nullptr);
				const float clear_color[] = {1.f, 1.f, 1.f, 1.f};
				GRHI->ClearTarget(RT_temp->pRT, clear_color);
			}

			Fvector		Lpos	= L.source->position;
			float		Lrange	= L.source->range;

			if (L.source->flags.type==IRender_Light::DIRECT)
			{
				
				Lpos.mul	(L.source->direction,-100);
				Lpos.add	(C.C);
				Lrange		= 120;
			} else {
				VERIFY		(_valid(Lpos));
				VERIFY		(_valid(C.C));
				float		_dist	;
				while		(true)	{
					_dist	=	C.C.distance_to	(Lpos);
					
					if (_dist>EPS_L)		break;
					Lpos.y					+=	.01f;	
				}
				float		_R		=	C.O->renderable.visual->getVisData().sphere.R+0.1f;
				
				if (_dist<_R)		{
					Fvector			Ldir;
					Ldir.sub		(C.C,Lpos);
					Ldir.normalize	();
					Lpos.mad		(Lpos,Ldir,_dist-_R);
					
				}
			}

			Fmatrix		mProject,mProjectR;
			float		p_dist	=	C.C.distance_to(Lpos);
			float		p_R		=	C.O->renderable.visual->getVisData().sphere.R;
			float		p_hat	=	p_R/p_dist;
			float		p_asp	=	1.f;
			float		p_near	=	p_dist-p_R-eps;	
			float		p_far	= std::min(Lrange, std::max(p_dist+S_fade,p_dist+p_R));
			if (p_near<eps)			continue;
			if (p_far<(p_near+eps))	continue;
			
			if (!(std::abs(p_far-p_near) > eps)) continue;
			if (p_hat>0.9f)			continue;
			if (p_hat<0.01f)		continue;

			mProject.build_projection_HAT	(p_hat,p_asp,p_near,	p_far);

			mProjectR = mProject;
			RCache.set_xform_project		(mProject);

			Fmatrix		mView;
			Fvector		v_D,v_N,v_R;
			v_D.sub					(C.C,Lpos);
			v_D.normalize			();
			if(1- std::abs(v_D.y)<EPS)	v_N.set(1,0,0);
			else            		v_N.set(0,1,0);
			v_R.crossproduct		(v_N,v_D);
			v_N.crossproduct		(v_D,v_R);
			mView.build_camera		(Lpos,C.C,v_N);
			RCache.set_xform_view	(mView);

			Fmatrix					mCombine,mCombineR;
			mCombine.mul			(mProject,mView);
			mCombineR.mul			(mProjectR,mView);

			int		s_x			=	slot_id%slot_line;
			int		s_y			=	slot_id/slot_line;
			RHIViewport VP = { (float)s_x * S_size, (float)s_y * S_size, (float)S_size, (float)S_size, 0, 1 };
			GRHI->SetViewport(VP);

			for (u32 n_it=0; n_it<C.nodes.size(); n_it++)
			{
				NODE& N					=	C.nodes[n_it];
				dxRender_Visual *V		=	N.pVisual;
				RCache.set_Element		(V->shader->E[SE_R1_LMODELS]);
				RCache.set_xform_world	(N.Matrix);
				V->Render				(-1.0f);
			}

			shadows.push_back		(shadow());
			shadows.back().O		=	C.O;
			shadows.back().slot		=	slot_id;
			shadows.back().C		=	C.C;
			shadows.back().M		=	mCombineR;
			shadows.back().L		=	L.source;
			shadows.back().E		=	L.energy;
#ifdef DEBUG
			shadows.back().dbg_HAT	=	p_hat;
#endif
			slot_id	++;
		}
	}

	for (u32 cs=0; cs<casters.size(); cs++)
		casters_pool.push_back(casters[cs]);
	casters.clear	();

	if (bRTS)
	{
		
		u32							Offset;
		FVF::TL4uv* pv				= (FVF::TL4uv*) RCache.Vertex.Lock	(4,geom_Blur.stride(),Offset);
		ApplyBlur4	(pv,S_rt_size,S_rt_size,S_blur_kernel);
		RCache.Vertex.Unlock		(4,geom_Blur.stride());

		RImplementation.Target->u_setrt(RT, nullptr, nullptr);
		RHIViewport blur_viewport = {0, 0, float(S_rt_size), float(S_rt_size), 0, 1};
		GRHI->SetViewport(blur_viewport);
		RCache.set_Shader			(sh_BlurTR	);
		RCache.set_Geometry			(geom_Blur	);
		RCache.Render				(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,Offset,0,4,0,2);
	}


	Device.Statistic->RenderDUMP_Scalc.End	();
	
	RCache.set_xform_project	(Device.mProject);
	RCache.set_xform_view		(Device.mView);
}

#define CLS(a)	color_rgba	(a,a,a,a)

IC	bool		cache_search(const CLightShadows::cache_item& A, const CLightShadows::cache_item& B)
{
	if (A.O < B.O)	return true;
	if (A.O > B.O)	return false;
	if (A.L < B.L)	return true;
	if (A.L > B.L)	return false;
	return			false;	
}

IC float PLC_energy	(Fvector& P, Fvector& N, light* L, float E)
{
	Fvector Ldir;
	if (L->flags.type==IRender_Light::DIRECT)
	{
		
		Ldir.invert	(L->direction);
		float D		= Ldir.dotproduct( N );
		if( D <=0 )						return 0;
		return E;
	} else {
		
		float sqD	= P.distance_to_sqr(L->position);
		if (sqD > (L->range*L->range))	return 0;

		Ldir.sub	(L->position,P);
		Ldir.normalize_safe();
		float D		= Ldir.dotproduct( N );
		if( D <=0 )						return 0;

		float R		= _sqrt		(sqD);
		float att	= 1-(1/(1+R));
		return (E * att);
	}
}

IC int PLC_calc	(Fvector& P, Fvector& N, light* L, float energy, Fvector& O)
{
	float	E		= PLC_energy(P,N,L,energy);
	float	C1		= clampr(Device.vCameraPosition.distance_to_sqr(P)/S_distance2,	0.f,1.f);
	float	C2		= clampr(O.distance_to_sqr(P)/S_fade2,							0.f,1.f);
	float	A		= 1.f-1.5f*E*(1.f-C1)*(1.f-C2);
	return			iCeil(255.f*A);
}

__forceinline float PLC_energy_SSE(Fvector& P, Fvector& N, light* L, float E)
{
	Fvector Ldir;
	if (L->flags.type==IRender_Light::DIRECT)
	{
		
		Ldir.invert	(L->direction);
		float D		= Ldir.dotproduct( N );
		if( D <=0 )						return 0;
		return E;
	} else {
		
		float sqD	= P.distance_to_sqr(L->position);
		if (sqD > (L->range*L->range))	return 0;

		Ldir.sub	(L->position,P);
		Ldir.normalize_safe();
		float D		= Ldir.dotproduct( N );
		if( D <=0 )						return 0;

		float att;
		__m128 rcpr = _mm_rsqrt_ss( _mm_load_ss( &sqD ) );
		rcpr = _mm_rcp_ss( _mm_add_ss( rcpr , _mm_set_ss( 1.0f ) ) );
		_mm_store_ss( &att , rcpr );

		return (E * att);
	}
}

__forceinline int iCeil_SSE( float const x ) 
{
	return _mm_cvt_ss2si( _mm_set_ss( x ) );
}

void  PLC_calc3_SSE(int& c0, int& c1, int& c2, CRenderDevice& Device_, Fvector* P, Fvector& N, light* L, float energy, Fvector& O)
{
	float	E		= PLC_energy_SSE(P[0],N,L,energy);
	float	C1		= clampr(Device_.vCameraPosition.distance_to_sqr(P[0])/S_distance2,	0.f,1.f);
	float	C2		= clampr(O.distance_to_sqr(P[0])/S_fade2,							0.f,1.f);
	float	A		= 1.f-1.5f*E*(1.f-C1)*(1.f-C2);
	c0 = iCeil_SSE(255.f*A);
	E		= PLC_energy_SSE(P[1],N,L,energy);
	C1		= clampr(Device_.vCameraPosition.distance_to_sqr(P[1])/S_distance2,	0.f,1.f);
	C2		= clampr(O.distance_to_sqr(P[1])/S_fade2,							0.f,1.f);
	A		= 1.f-1.5f*E*(1.f-C1)*(1.f-C2);
	c1 = iCeil_SSE(255.f*A);
	E		= PLC_energy_SSE(P[2],N,L,energy);
	C1		= clampr(Device_.vCameraPosition.distance_to_sqr(P[2])/S_distance2,	0.f,1.f);
	C2		= clampr(O.distance_to_sqr(P[2])/S_fade2,							0.f,1.f);
	A		= 1.f-1.5f*E*(1.f-C1)*(1.f-C2);
	c2 = iCeil_SSE(255.f*A);
}

void CLightShadows::render	()
{
	
	CDB::MODEL*		DB		= g_pGameLevel->ObjectSpace.GetStaticModel();
	xr_vector<CDB::TRI>& TRIS	= DB->get_tris();
	xr_vector<Fvector>& VERTS	= DB->get_verts();

	int			slot_line	= S_rt_size/S_size;

	float _43					=	Device.mProject._43;

	const float fMinNear = 0.1f;
	const float fMaxNear = 0.2f;
	const float fMinNearBias = 0.0002f;
	const float fMaxNearBias = 0.002f;
	float	fLerpCoeff	= (_43 - fMinNear) / (fMaxNear - fMinNear);
	clamp( fLerpCoeff, 0.0f, 1.0f );
	
	Device.mProject._43			-=	fMinNearBias + (fMaxNearBias-fMinNearBias) * fLerpCoeff;
	
	Device.mProject._43			-=	0.002f; 
	
	RCache.set_xform_world		(Fidentity);
	RCache.set_xform_project	(Device.mProject);
	Fvector	View				= Device.vCameraPosition;

	RCache.set_Shader			(sh_World);
	RCache.set_Geometry			(geom_World);
	int batch					= 0;
	u32 Offset					= 0;
	FVF::LIT* pv				= (FVF::LIT*) RCache.Vertex.Lock	(batch_size*3,geom_World->vb_stride,Offset);
	for (u32 s_it=0; s_it<shadows.size(); s_it++)
	{
		Device.Statistic->RenderDUMP_Srender.Begin	();
		shadow&		S			=	shadows[s_it];
		float		Le			=	S.L->color.intensity()*S.E;
		int			s_x			=	S.slot%slot_line;
		int			s_y			=	S.slot/slot_line;
		Fvector2	t_scale, t_offset;
		t_scale.set	(float(S_size)/float(S_rt_size),float(S_size)/float(S_rt_size));
		t_scale.mul (.5f);
		t_offset.set(float(s_x)/float(slot_line),float(s_y)/float(slot_line));
		t_offset.x	+= .5f/S_rt_size;
		t_offset.y	+= .5f/S_rt_size;

		cache_item*						CI		= nullptr; bool	bValid = false;
		cache_item						CI_what; CI_what.O	= S.O; CI_what.L = S.L; CI_what.tris=nullptr;
		xr_vector<cache_item>::iterator	CI_ptr	= std::lower_bound(cache.begin(),cache.end(),CI_what,cache_search);
		if (CI_ptr==cache.end())		
		{	
			CI_ptr	= cache.insert		(CI_ptr,CI_what);
			CI		= &*CI_ptr;
			bValid	= false;
		} else {
			if (CI_ptr->O != CI_what.O  || CI_ptr->L != CI_what.L)	
			{	
				CI_ptr	= cache.insert		(CI_ptr,CI_what);
				CI		= &*CI_ptr;
				bValid	= false;
			} else {
				
				CI		= &*CI_ptr;
				bValid	= true;
				if (!CI->Op.similar(CI->O->renderable.xform.c))	bValid = false;
				else if (!CI->Lp.similar(CI->L->position))		bValid = false;
			}
		}
		CI->time				= Device.dwTimeGlobal;	

		if (!bValid)			{
			
			CFrustum				F;
			F.CreateFromMatrix		(S.M,FRUSTUM_P_ALL);

			xrc.frustum_options		(0);
			xrc.frustum_query		(DB,F);
			if (0==xrc.r_count())	continue;

			tess.clear				();
			for (CDB::RESULT* p = xrc.r_begin(); p!=xrc.r_end(); p++)
			{
				VERIFY((p->id>=0)&&(p->id<DB->get_tris().size()));
				
				CDB::TRI&	t		= TRIS[p->id];
				if (t.suppress_shadows) continue;
				sPoly		A,B;
				A.push_back			(VERTS[t.verts[0]]);
				A.push_back			(VERTS[t.verts[1]]);
				A.push_back			(VERTS[t.verts[2]]);

				Fplane				P;	float mag = 0;
				Fvector				t1,t2,n;
				t1.sub				(A[0],A[1]);
				t2.sub				(A[0],A[2]);
				n.crossproduct		(t1,t2);
				mag	= n.square_magnitude();
				if (mag<EPS_S)						continue;
				n.mul				(1.f/_sqrt(mag));
				P.build_unit_normal	(A[0],n);
				float	DOT_Fade	= P.classify(S.L->position);
				if (DOT_Fade<0)		continue;

				sPoly*		clip	= F.ClipPoly	(A,B);
				if (0==clip)		continue;

				for (u32 v=2; v<clip->size(); v++)	{
					tess.emplace_back();
					tess_tri& T		= tess.back();
					T.v[0]			= (*clip)[0];
					T.v[1]			= (*clip)[v-1];
					T.v[2]			= (*clip)[v];
					T.N				= P.n;
				}
			}

			CI->O					= S.O;
			CI->Op					= CI->O->renderable.xform.c;
			CI->L					= S.L;
			CI->Lp					= CI->L->position;
			CI->tcnt				= (u32)tess.size();
			
			xr_free					(CI->tris);	VERIFY(nullptr==CI->tris);	
			if (tess.size())		{
				CI->tris			= xr_alloc<tess_tri>(CI->tcnt);
				
				CopyMemory		(CI->tris,&*tess.begin(),CI->tcnt * sizeof(tess_tri));
			}
		}

		for (u32 tid=0; tid<CI->tcnt; tid++)	{
			tess_tri&	TT		= CI->tris[tid];
			Fvector* 	v		= TT.v;
			Fvector		T;
			Fplane		ttp;	ttp.build_unit_normal(v[0],TT.N);

			if (ttp.classify(View)<0)						continue;
			
			int	c0,c1,c2;

			PLC_calc3_SSE(c0,c1,c2,Device,v,TT.N,S.L,Le,S.C);

			if (c0>S_clip && c1>S_clip && c2>S_clip)		continue;	
			clamp		(c0,S_ambient,255);
			clamp		(c1,S_ambient,255);
			clamp		(c2,S_ambient,255);

			S.M.transform(T,v[0]); pv->set(v[0],CLS(c0),(T.x+1)*t_scale.x+t_offset.x,(1-T.y)*t_scale.y+t_offset.y); pv++;
			S.M.transform(T,v[1]); pv->set(v[1],CLS(c1),(T.x+1)*t_scale.x+t_offset.x,(1-T.y)*t_scale.y+t_offset.y); pv++;
			S.M.transform(T,v[2]); pv->set(v[2],CLS(c2),(T.x+1)*t_scale.x+t_offset.x,(1-T.y)*t_scale.y+t_offset.y); pv++;

			batch++;
			if (batch==batch_size)	{
				
				RCache.Vertex.Unlock	(batch*3,geom_World->vb_stride);
				RCache.Render			(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,Offset,batch);

				pv						= (FVF::LIT*) RCache.Vertex.Lock(batch_size*3,geom_World->vb_stride,Offset);
				batch					= 0;
			}
		}
		Device.Statistic->RenderDUMP_Srender.End	();
	}

	RCache.Vertex.Unlock	(batch*3,geom_World->vb_stride);
	if (batch)				{
		RCache.Render			(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,Offset,batch);
	}

	shadows.clear				();
	for (int cit=0; cit<int(cache.size()); cit++)	{
		cache_item&		ci		= cache[cit];
		u32				time	= Device.dwTimeGlobal - ci.time;
		if				(time > cache_old)	{
			
			xr_free		(ci.tris);	VERIFY(nullptr==ci.tris);
			cache.erase (cache.begin()+cit);
			cit			--;
		}
	}

	Device.mProject._43			= _43;
	RCache.set_xform_project	(Device.mProject);
}
