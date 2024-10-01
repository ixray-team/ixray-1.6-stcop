#include "StdAfx.h"
#include "light.h"

static const float	SQRT2		=	1.4142135623730950488016887242097f;
static const float	RSQRTDIV2	=	0.70710678118654752440084436210485f;

light::light		(void)	: ISpatial(g_SpatialSpace)
{
	spatial.type	= STYPE_LIGHTSOURCE;
	flags.type		= POINT;
	flags.bStatic	= false;
	flags.bActive	= false;
	flags.bShadow	= false;
	flags.bVolumetric = false;
	flags.bHudMode	= false;
	flags.bOccq		= true;

	position.set	(0,-1000,0);
	direction.set	(0,-1,0);
	right.set		(0,0,0);
	range			= 8.f;
	virtual_size	= 0.1f;
	cone			= deg2rad(60.f);
	color.set		(1,1,1,1);

	m_volumetric_quality	= 1;
	//m_volumetric_quality	= 0.5;
	m_volumetric_intensity	= 1;
	m_volumetric_distance	= 1;

	frame_render	= 0;

	ignore_object	= nullptr;
	for (int f=0; f<6; f++)decor_object[f] = nullptr;

#if (RENDER==R_R2) || (RENDER==R_R4)
	ZeroMemory		(omnipart,sizeof(omnipart));
	s_spot			= nullptr;
	s_point			= nullptr;
	vis.frame2test	= 0;	// xffffffff;
	vis.query_id	= 0;
	vis.query_order	= 0;
	vis.visible		= true;
	vis.pending		= false;
	m_sectors = {};
	b_need_recompute_xform = true;
	b_need_detect_sectors = true;

	X.S.posX	= 0;
	X.S.posY	= 0;
	X.S.size	= SMAP_adapt_max;
#endif // (RENDER==R_R2) || (RENDER==R_R4)
}

light::~light	()
{
#if (RENDER==R_R2) || (RENDER==R_R4)
	for (int f=0; f<6; f++)	xr_delete(omnipart[f]);
#endif // (RENDER==R_R2) || (RENDER==R_R4)
	set_active		(false);

	// remove from Lights_LastFrame
#if (RENDER==R_R2) || (RENDER==R_R4)
	m_sectors.clear();
#endif // (RENDER==R_R2) || (RENDER==R_R4)
	ignore_object	= nullptr;
	for (int f=0; f<6; f++)decor_object[f] = nullptr;
}

#if (RENDER==R_R2) || (RENDER==R_R4)
void light::set_texture		(LPCSTR name)
{
	if ((0==name) || (0==name[0]))
	{
		// default shaders
		s_spot.destroy		();
		s_point.destroy		();
		s_volumetric.destroy();
		return;
	}

#pragma todo				("Only shadowed spot implements projective texture")
	string256				temp;
	
	xr_strconcat(temp,"r2\\accum_spot_",name);
	s_spot.create			(RImplementation.Target->b_accum_spot,temp,name);

	s_volumetric.create		("accum_volumetric", name);
}
#endif

#if RENDER==R_R1
void light::set_texture		(LPCSTR name)
{
}
#endif

void light::set_shadow				(bool b)						
{ 
	flags.bShadow=b;
#if RENDER!=R_R1
	if (flags.type==IRender_Light::POINT)
	{
		if(flags.bShadow)
		{
			// tough: create 6 shadowed lights
			if (0==omnipart[0])
			{
				for (int f=0; f<6; f++)
					omnipart[f] = new light ();
			}
		}
		else
		{
			// tough: delete 6 shadowed lights
			if (0!=omnipart[0])
			{
				for (int f=0; f<6; f++)	xr_delete(omnipart[f]);
			}
		}
	}
#endif
}

void light::set_active		(bool a)
{
	if (a)
	{
		if (flags.bActive)					return;
		flags.bActive						= true;
		spatial_register					();
		spatial_move						();
		//Msg								("!!! L-register: %X",u32(this));

#ifdef DEBUG
		Fvector	zero = {0,-1000,0}			;
		if (position.similar(zero))			{
			Msg	("- Uninitialized light position.");
		}
#endif // DEBUG
	}
	else
	{
		if (!flags.bActive)					return;
		flags.bActive						= false;
		spatial_move						();
		spatial_unregister					();
		//Msg								("!!! L-unregister: %X",u32(this));
	}
}

void	light::set_position		(const Fvector& P)
{
	float	eps					=	EPS_L;	//_max	(range*0.001f,EPS_L);
	if (position.similar(P,eps))return	;
	position.set				(P);
	spatial_move				();
}

void	light::set_range		(float R)			{
	float	eps					=	_max	(range*0.1f,EPS_L);
	if (fsimilar(range,R,eps))	return	;
	range						= R		;
	spatial_move				();
};

void	light::set_cone			(float angle)		{
	if (fsimilar(cone,angle))	return	;
	VERIFY						(cone < deg2rad(121.f));	// 120 is hard limit for lights
	cone						= angle;
	spatial_move				();
}
void	light::set_rotation		(const Fvector& D, const Fvector& R)	{ 
	Fvector	old_D		= direction;
	direction.normalize	(D);
	right.normalize(R);
	if (!fsimilar(1.f, old_D.dotproduct(D)))	spatial_move	();
}

#if RENDER!=R_R1
void light::get_sectors()
{
	if(0==spatial.sector)
		spatial_updatesector();

	CSector* sector = (CSector*)spatial.sector;
	if(0==sector) return;

	if(flags.type == IRender_Light::SPOT)
	{
		CFrustum temp = CFrustum();
		temp.CreateFromMatrix			(X.S.combine, FRUSTUM_P_ALL);

		m_sectors = std::move(RImplementation.detectSectors_frustum(sector, &temp));
	}
	if(flags.type == IRender_Light::POINT)
	{
		m_sectors = std::move(RImplementation.detectSectors_sphere(sector, position, Fvector().set(range, range, range)));
	}
}
#endif

void	light::spatial_move			()
{
	switch(flags.type)	{
	case IRender_Light::REFLECTED	:	
	case IRender_Light::POINT		:	
		{
			spatial.sphere.set		(position, range);
		} 
		break;
	case IRender_Light::SPOT		:	
		{
			// minimal enclosing sphere around cone
			VERIFY2						(cone < deg2rad(121.f), "Too large light-cone angle. Maybe you have passed it in 'degrees'?");
			if (cone>=PI_DIV_2)			{
				// obtused-angled
				spatial.sphere.P.mad	(position,direction,range);
				spatial.sphere.R		= range * tanf(cone/2.f);
			} else {
				// acute-angled
				spatial.sphere.R		= range / (2.f * _sqr(_cos(cone/2.f)));
				spatial.sphere.P.mad	(position,direction,spatial.sphere.R);
			}
		}
		break;
	case IRender_Light::OMNIPART	:
		{
			// is it optimal? seems to be...
			//spatial.sphere.P.mad		(position,direction,range);
			//spatial.sphere.R			= range;
			// This is optimal.
			const float fSphereR		= range*RSQRTDIV2;
			spatial.sphere.P.mad		(position,direction,fSphereR);
			spatial.sphere.R			= fSphereR;
		}
		break;
	}

	// update spatial DB
	ISpatial::spatial_move			();

#if (RENDER==R_R2) || (RENDER==R_R4)
	b_need_detect_sectors = true;
	b_need_recompute_xform = true;
#endif // (RENDER==R_R2) || (RENDER==R_R4)
}

vis_data&	light::get_homdata		()
{
	// commit vis-data
	hom.sphere.set	(spatial.sphere.P,spatial.sphere.R);
	hom.box.set		(spatial.sphere.P,spatial.sphere.P);
	hom.box.grow	(spatial.sphere.R);
	return			hom;
};

Fvector	light::spatial_sector_point	()	
{ 
	return position; 
}

//////////////////////////////////////////////////////////////////////////
#if (RENDER==R_R2) || (RENDER==R_R4)
// Xforms
void	light::xform_calc			()
{
	if	(Device.dwFrame == m_xform_frame)	return;
	m_xform_frame	= Device.dwFrame;

	// build final rotation / translation
	Fvector					L_dir,L_up,L_right;

	// dir
	L_dir.set				(direction);
	float l_dir_m			= L_dir.magnitude();
	if (_valid(l_dir_m) && l_dir_m>EPS_S)	L_dir.div(l_dir_m);
	else									L_dir.set(0,0,1);

	// R&N
	if (right.square_magnitude()>EPS)				{
		// use specified 'up' and 'right', just enshure ortho-normalization
		L_right.set					(right);				L_right.normalize	();
		L_up.crossproduct			(L_dir,L_right);		L_up.normalize		();
		L_right.crossproduct		(L_up,L_dir);			L_right.normalize	();
	} else {
		// auto find 'up' and 'right' vectors
		L_up.set					(0,1,0);				if (_abs(L_up.dotproduct(L_dir))>.99f)	L_up.set(0,0,1);
		L_right.crossproduct		(L_up,L_dir);			L_right.normalize	();
		L_up.crossproduct			(L_dir,L_right);		L_up.normalize		();
	}

	// matrix
	Fmatrix					mR;
	mR.i					= L_right;	mR._14	= 0;
	mR.j					= L_up;		mR._24	= 0;
	mR.k					= L_dir;	mR._34	= 0;
	mR.c					= position;	mR._44	= 1;

	// switch
	switch(flags.type)	{
	case IRender_Light::REFLECTED	:
	case IRender_Light::POINT		:
		{
			// scale of identity sphere
			float		L_R			= range;
			Fmatrix		mScale;		mScale.scale	(L_R,L_R,L_R);
			m_xform.mul_43			(mR,mScale);
		}
		break;
	case IRender_Light::SPOT		:
		{
			// scale to account range and angle
			float		s			= 2.f*range*tanf(cone/2.f);	
			Fmatrix		mScale;		mScale.scale(s,s,range);	// make range and radius
			m_xform.mul_43			(mR,mScale);
		}
		break;
	case IRender_Light::OMNIPART	:
		{
			float		L_R			= 2*range;		// volume is half-radius
			Fmatrix		mScale;		mScale.scale	(L_R,L_R,L_R);
			m_xform.mul_43			(mR,mScale);
		}
		break;
	default:
		m_xform.identity	();
		break;
	}

	if(flags.type==IRender_Light::SPOT||flags.type==IRender_Light::OMNIPART)
	{
		// make N pixel border
		X.S.view.build_camera_dir	(position,L_dir,L_up);

		// _min(L->cone + deg2rad(4.5f), PI*0.98f) - Here, it is needed to enlarge the shadow map frustum to include also 
		// displaced pixels and the pixels neighbor to the examining one.
		X.S.project.build_projection		(_min(cone + deg2rad(5.f), PI*0.98f), 1.f,virtual_size,range+EPS_S);

		X.S.combine.mul(X.S.project,X.S.view);
	}
}

void	light::optimize_smap_size()
{
	int _cached_size = X.S.size;
	X.S.posX	= 0;
	X.S.posY	= 0;
	X.S.size	= SMAP_adapt_max;
	X.S.transluent = FALSE;
	// Compute approximate screen area (treating it as an point light) - R*R/dist_sq
	// Note: we clamp screen space area to ONE, although it is not correct at all
	float	dist				= Device.vCameraPosition.distance_to(spatial.sphere.P)-spatial.sphere.R;
			if (dist<0)	dist	= 0;
	float	ssa					= clampr	(range*range / (1.f+dist*dist),0.f,1.f);

	// compute intensity
	//float	intensity0			= (L->color.r + L->color.g + L->color.b)/3.f;
	//float	intensity1			= (L->color.r * 0.2125f + L->color.g * 0.7154f + L->color.b * 0.0721f);
	//float	intensity			= (intensity0+intensity1)/2.f;		// intensity1 tends to underestimate...

	// compute how much duelling frusta occurs	[-1..1]-> 1 + [-0.5 .. +0.5]
	float	duel_dot			= 1.f -	0.5f*Device.vCameraDirection.dotproduct(direction);

	// compute how large the light is - give more texels to larger lights, assume 8m as being optimal radius
	float	sizefactor			= range/8.f;				// 4m = .5, 8m=1.f, 16m=2.f, 32m=4.f

	// compute how wide the light frustum is - assume 90deg as being optimal
	float	widefactor			= cone/deg2rad(90.f);	// 

	// factors
	float	factor0				= powf	(ssa,		1.f/2.f);		// ssa is quadratic
	//float	factor1				= powf	(intensity, 1.f/16.f);		// less perceptually important?
	float	factor2				= powf	(duel_dot,	1.f/4.f);		// difficult to fast-change this -> visible
	float	factor3				= powf	(sizefactor,1.f/4.f);		// this shouldn't make much difference
	float	factor4				= powf	(widefactor,1.f/2.f);		// make it linear ???
	float	factor				= ps_r2_ls_squality * factor0 /** factor1*/ * factor2 * factor3 * factor4;
	
	// final size calc
	u32 _size					= iFloor( factor * SMAP_adapt_optimal );
	if (_size<SMAP_adapt_min)	_size	= SMAP_adapt_min;
	if (_size>SMAP_adapt_max)	_size	= SMAP_adapt_max;
	int _epsilon				= iCeil	(float(_size)*0.01f);
	int _diff					= _abs	(int(_size)-int(_cached_size));
	X.S.size					= (_diff>=_epsilon)?_size:_cached_size;
}

//								+X,				-X,				+Y,				-Y,			+Z,				-Z
static	Fvector cmNorm[6]	= {{0.f,1.f,0.f}, {0.f,1.f,0.f}, {0.f,0.f,-1.f},{0.f,0.f,1.f}, {0.f,1.f,0.f}, {0.f,1.f,0.f}};
static	Fvector cmDir[6]	= {{1.f,0.f,0.f}, {-1.f,0.f,0.f},{0.f,1.f,0.f}, {0.f,-1.f,0.f},{0.f,0.f,1.f}, {0.f,0.f,-1.f}};

void	light::export_		(light_Package& package)
{
	if(b_need_recompute_xform&&( (!flags.bShadow && flags.type==IRender_Light::POINT) || flags.type==IRender_Light::SPOT ))
	{
		xform_calc();
		b_need_recompute_xform = false;
	}
	float	safe_area					= VIEWPORT_NEAR;
	{
		float	a0	= deg2rad(Device.fFOV*Device.fASPECT*.5f);
		float	a1	= deg2rad(Device.fFOV*.5f);
		float	x0	= VIEWPORT_NEAR/_cos	(a0);
		float	x1	= VIEWPORT_NEAR/_cos	(a1);
		float	c	= _sqrt					(x0*x0 + x1*x1);
		safe_area	= _max(_max(VIEWPORT_NEAR,_max(x0,x1)),c);
	}
	vis.camerainbounds = Device.vCameraPosition.distance_to_sqr(spatial.sphere.P)<=_sqr(spatial.sphere.R*1.01f+safe_area+EPS_L);

	if (flags.bShadow)			{
		switch (flags.type)	{
			case IRender_Light::POINT:
				{
					// tough: update 6 shadowed lights
					for (int f=0; f<6; f++)	{
						light*	L			= omnipart[f];
						Fvector				R;
						R.crossproduct		(cmNorm[f],cmDir[f]);
						L->set_type			(IRender_Light::OMNIPART);
						L->set_shadow		(true);
						L->set_position		(position);
						L->set_rotation		(cmDir[f],	R);
						L->set_cone			(PI_DIV_2);
						L->set_range		(range);
						L->set_virtual_size(virtual_size);
						L->set_color		(color);
						L->spatial.sector	= spatial.sector;	//. dangerous?
						L->s_spot			= s_spot	;
						L->s_point			= s_point	;

						//	Igor: add volumetric support
						L->set_volumetric(flags.bVolumetric);
						L->set_volumetric_quality(m_volumetric_quality);
						L->set_volumetric_intensity(m_volumetric_intensity);
						L->set_volumetric_distance(m_volumetric_distance);

						L->set_ignore_object(ignore_object);
						for (int f=0; f<6; f++)L->set_decor_object(decor_object[f],f);
						
						
						L->set_hud_mode(flags.bHudMode);
						L->set_occq_mode(flags.bOccq);
						if(L->b_need_recompute_xform)
						{
							L->xform_calc();
							L->b_need_recompute_xform = false;
						}

						L->vis.camerainbounds = vis.camerainbounds;

						package.v_shadowed.push_back	(L);
					}
				}
				break;
			case IRender_Light::SPOT:
					package.v_shadowed.push_back			(this);
				break;
		}
	}	else	{
		switch (flags.type)	{
			case IRender_Light::POINT:		package.v_point.push_back	(this);	break;
			case IRender_Light::SPOT:		package.v_spot.push_back	(this);	break;
		}
	}
}

void	light::set_attenuation_params	(float a0, float a1, float a2, float fo)
{
	attenuation0 = a0;
	attenuation1 = a1;
	attenuation2 = a2;
	falloff      = fo;
}

#endif // (RENDER==R_R2) || (RENDER==R_R4)

float	light::get_LOD					()
{
#ifndef _EDITOR
	if	(!flags.bShadow)	return 1;
	extern float		r_ssaGLOD_start, r_ssaGLOD_end;
	extern float		ps_r2_slight_fade;
	float	distSQ			= Device.vCameraPosition.distance_to_sqr(spatial.sphere.P)+EPS;
	float	ssa				= ps_r2_slight_fade * spatial.sphere.R/distSQ;
	float	lod				= _sqrt(clampr((ssa - r_ssaGLOD_end)/(r_ssaGLOD_start-r_ssaGLOD_end),0.f,1.f));
	return	lod	;
#else
	return 1.0f;
#endif
}
