#include "StdAfx.h"
#include "light.h"
#include "QueryHelper.h"
#include "../../xrEngine/IGame_Level.h"
light::light()
{
	ISpatialOwner::spatial_create(g_SpatialSpaceLights, this, STYPE_LIGHTSOURCE);
	//ISpatialOwner::spatial_create(g_SpatialSpace, this, STYPE_LIGHTSOURCE);
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
	m_parent = nullptr;
#if (RENDER==R_R2) || (RENDER==R_R4)
	ZeroMemory		(omnipart,sizeof(omnipart));
	s_spot			= nullptr;
	s_point			= nullptr;
	vis.frame2test	= 0;	// xffffffff;
	CHK_DX(CreateQuery(&vis.Q, D3DQUERYTYPE_OCCLUSION));
	vis.visible		= true;
	vis.pending		= false;
	m_sectors = {};
	X.S.posX	= 0;
	X.S.posY	= 0;
	X.S.size	= SMAP_adapt_max;
	RImplementation.v_all_lights.emplace(this);
#endif // (RENDER==R_R2) || (RENDER==R_R4)
}

light::~light	()
{
	m_parent = nullptr;
#if (RENDER==R_R2) || (RENDER==R_R4)
	RImplementation.v_all_lights.erase(this);
	for (int f=0; f<6; f++)	xr_delete(omnipart[f]);
	_RELEASE(vis.Q);
#endif // (RENDER==R_R2) || (RENDER==R_R4)
	set_active		(false);
	// remove from Lights_LastFrame
#if (RENDER==R_R2) || (RENDER==R_R4)
	m_sectors.clear();
#endif // (RENDER==R_R2) || (RENDER==R_R4)
	ignore_object	= nullptr;
	for (int f=0; f<6; f++)decor_object[f] = nullptr;
}


void light::destroy(bool deffered)
{

	set_active(false);
	if(deffered)
	{
		if(std::find(RImplementation.v_all_lights_dque.begin(),RImplementation.v_all_lights_dque.end(), this)==RImplementation.v_all_lights_dque.end())
			RImplementation.v_all_lights_dque.push_back(this);
	}
	else
		xr_delete(this);

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
//								+X,				-X,				+Y,				-Y,			+Z,				-Z
static	Fvector cmNorm[6] = { {0.f,1.f,0.f}, {0.f,1.f,0.f}, {0.f,0.f,-1.f},{0.f,0.f,1.f}, {0.f,1.f,0.f}, {0.f,1.f,0.f} };
static	Fvector cmDir[6] = { {1.f,0.f,0.f}, {-1.f,0.f,0.f},{0.f,1.f,0.f}, {0.f,-1.f,0.f},{0.f,0.f,1.f}, {0.f,0.f,-1.f} };
void light::set_shadow				(bool b)						
{ 
	flags.bShadow=b;
#if RENDER!=R_R1
	if (flags.type==IRender_Light::POINT)
	{
		if(flags.bShadow)
		{
			// tough: create 6 shadowed lights
			if (!omnipart[0])
			{
				for (int f=0; f<6; f++)
				{
					omnipart[f] = new light();
					omnipart[f]->m_parent = this;
					omnipart[f]->set_type(IRender_Light::OMNIPART);
					omnipart[f]->set_shadow(true);
					omnipart[f]->set_cone(PI_DIV_2);
					omnipart[f]->set_virtual_size(virtual_size);
					Fvector				R;
					R.crossproduct(cmNorm[f], cmDir[f]);
					omnipart[f]->set_rotation(cmDir[f], R);
					omnipart[f]->s_spot = s_spot;
					omnipart[f]->s_point = s_point;
				}
			}
		}
		else
		{
			// tough: delete 6 shadowed lights
			if (omnipart[0])
			{
				for (int f=0; f<6; f++)	xr_delete(omnipart[f]);
			}
		}
	}
#endif
}

void light::set_active(bool a)
{
	if (a)
	{
		if (flags.bActive)
			return;

		flags.bActive = true;
		spatial_register();
		spatial_move();

#ifdef DEBUG
		Fvector	zero = { 0,-1000,0 };
		if (position.similar(zero)) {
			Msg("- Uninitialized light position.");
		}
#endif // DEBUG
	}
	else
	{
		if (!flags.bActive)
			return;

		flags.bActive = false;
		spatial_unregister();
	}
}

void light::set_position(const Fvector& P)
{
	if (position.similar(P, EPS))
		return;

	position.set(P);
	spatial_move();
}

void light::set_range(float R)
{
	float eps = _max(range*0.1f,EPS);

	if (fsimilar(range,R,eps))
		return;

	range = R;
	spatial_move();
};

void light::set_cone(float angle)
{
	if (fsimilar(cone,angle))
		return;

	VERIFY(cone < deg2rad(121.f));	// 120 is hard limit for lights
	cone = angle;
	spatial_move();
}
void light::set_rotation(const Fvector& D, const Fvector& R)
{ 
	Fvector	old_D = direction;
	direction.normalize	(D);
	right.normalize(R);

	if (!fsimilar(1.f, old_D.dotproduct(D)))
		spatial_move();
}

#if RENDER!=R_R1
void light::get_sectors()
{
	if(RImplementation.SectorsCount()<=1) return;
	xrCriticalSectionGuard guard(&sectors_lc);
	if(0== SpatialComponent->spatial.sector)
		SpatialComponent->spatial_updatesector();

	CSector* sector = (CSector*)SpatialComponent->spatial.sector;
	if(0==sector) return;

	if(flags.type == IRender_Light::SPOT/* || flags.type == IRender_Light::OMNIPART*/)
	{
		RImplementation.detectSectors_frustum(sector, m_sectors, &X.S.frustum);
	}
	if(flags.type == IRender_Light::POINT)
	{
		RImplementation.detectSectors_sphere(sector, m_sectors, position, Fvector().set(range, range, range));
	}
}

bool light::has_light_visible_from_sectors(CDSGraphManager& DM)
{
	if (RImplementation.SectorsCount() <= 1) return true;
	xrCriticalSectionGuard guard(&sectors_lc);
	if (!m_sectors.size())get_sectors();
	for (auto& node : m_sectors)
	{
		CSector* sector_ = (CSector*)node.key;
		if (DM.is_sector_visible(sector_))
		{
			return true;
		}
	}
	return false;
}

bool light::has_outdoor_light()
{
	xrCriticalSectionGuard guard(&sectors_lc);
	return m_sectors.find(RImplementation.pOutdoorSector);
}
#endif

void	light::spatial_move			()
{
	switch(flags.type)
	{
		case IRender_Light::DIRECT: return;//sun ?
		case IRender_Light::REFLECTED:
		case IRender_Light::POINT:
		{
			SpatialComponent->spatial.sphere.set(position, range);
		}break;

		case IRender_Light::SPOT:
		{
			// minimal enclosing sphere around cone
			VERIFY2(cone < deg2rad(121.f), "Too large light-cone angle. Maybe you have passed it in 'degrees'?");
			if (cone>=PI_DIV_2)
			{
				// obtused-angled
				SpatialComponent->spatial.sphere.P.mad(position,direction,range);
				SpatialComponent->spatial.sphere.R = range * tanf(cone/2.f);
			}
			else
			{
				// acute-angled
				SpatialComponent->spatial.sphere.R = range / (2.f * _sqr(_cos(cone/2.f)));
				SpatialComponent->spatial.sphere.P.mad(position,direction, SpatialComponent->spatial.sphere.R);
			}
		}break;

		case IRender_Light::OMNIPART:
		{
			// is it optimal? seems to be...
			//spatial.sphere.P.mad		(position,direction,range);
			//spatial.sphere.R			= range;
			// This is optimal.
			const float fSphereR = range*M_SQRT1_2;
			SpatialComponent->spatial.sphere.P.mad(position,direction,fSphereR);
			SpatialComponent->spatial.sphere.R = fSphereR;
		}break;
	}

	// update spatial DB
	ISpatialOwner::spatial_move();

#if (RENDER==R_R2) || (RENDER==R_R4)
	get_sectors();
#endif // (RENDER==R_R2) || (RENDER==R_R4)
	m_moving_frames = 0;
}

vis_data& light::get_homdata()
{
	// commit vis-data
	hom.sphere.set	(SpatialComponent->spatial.sphere.P, SpatialComponent->spatial.sphere.R);
	hom.box.set		(SpatialComponent->spatial.sphere.P, SpatialComponent->spatial.sphere.P);
	hom.box.grow	(SpatialComponent->spatial.sphere.R);
	return			hom;
};

Fvector	light::spatial_sector_point()	
{ 
	return position; 
}

//////////////////////////////////////////////////////////////////////////
#if (RENDER==R_R2) || (RENDER==R_R4)
// Xforms
void	light::xform_calc			()
{
	if	(Device.dwFrame == m_parent_u_frame)	return;
	m_parent_u_frame = Device.dwFrame;

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

	if (flags.bShadow && (flags.type == IRender_Light::SPOT || flags.type == IRender_Light::OMNIPART))
	{
		// make N pixel border
		X.S.view.build_camera_dir	(position,L_dir,L_up);

		// _min(L->cone + deg2rad(4.5f), PI*0.98f) - Here, it is needed to enlarge the shadow map frustum to include also 
		// displaced pixels and the pixels neighbor to the examining one.
		X.S.project.build_projection		(_min(cone + deg2rad(5.f), PI*0.98f), 1.f,virtual_size,range+EPS_S);

		X.S.combine.mul(X.S.project,X.S.view);

		X.S.frustum.CreateFromMatrix(X.S.combine, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
	}
}

void light::optimize_smap_size()
{
	if (flags.bHudMode) return;
	int _cached_size = X.S.size;
	X.S.posX	= 0;
	X.S.posY	= 0;
	X.S.size	= SMAP_adapt_max;
	X.S.transluent = FALSE;

	// Compute approximate screen area (treating it as an point light) - R*R/dist_sq
	// Note: we clamp screen space area to ONE, although it is not correct at all
	// 
	//float	dist = Device.vCameraPosition.distance_to(SpatialComponent->spatial.sphere.P) - SpatialComponent->spatial.sphere.R;
	//if (dist < 0)	dist = 0;
	//float	ssa = clampr(range * range / (1.f + dist * dist), 0.f, 1.f);

	float dist_sqr = Device.vCameraPosition.distance_to_sqr(SpatialComponent->spatial.sphere.P);
	float R = SpatialComponent->spatial.sphere.R;
	float R_sqr = _sqr(R);
	float ssa = dist_sqr < R_sqr ? 1.f : clampr(_sqr(range) / (1.f + (dist_sqr - R_sqr)), 0.f, 1.f);

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
	float	factor0				= powf	(ssa, 0.5f);		// ssa is quadratic
	//float	factor1				= powf	(intensity, 0.0625f);		// less perceptually important?
	float	factor2				= powf	(duel_dot, 0.25f);		// difficult to fast-change this -> visible
	float	factor3				= powf	(sizefactor, 0.25f);		// this shouldn't make much difference
	float	factor4				= powf	(widefactor, 0.5f);		// make it linear ???
	float	factor				= ps_r2_ls_squality * factor0 /** factor1*/ * factor2 * factor3 * factor4;
	
	// final size calc
	u32 _size					= iFloor( factor * SMAP_adapt_optimal );
	if (_size<SMAP_adapt_min)	_size	= SMAP_adapt_min;
	if (_size>SMAP_adapt_max)	_size	= SMAP_adapt_max;
	int _epsilon				= iCeil	(float(_size)*0.01f);
	int _diff					= _abs	(int(_size)-int(_cached_size));
	X.S.size					= (_diff>=_epsilon)?_size:_cached_size;
}

void light::export_()
{
	xform_calc();
	if (flags.bShadow)
	{
		switch (flags.type)
		{
			case IRender_Light::POINT:
			{
				// tough: update 6 shadowed lights
				vis_prepare();
				for (int f=0; f<6; f++)
				{
					light*	L			= omnipart[f];
					L->set_position		(position);
					L->SpatialComponent->spatial.sector = SpatialComponent->spatial.sector;
					L->set_range		(range);
					L->set_color		(color);
					L->flags.bActive = flags.bActive;
					//	Igor: add volumetric support
					L->set_volumetric(flags.bVolumetric);
					L->set_volumetric_quality(m_volumetric_quality);
					L->set_volumetric_intensity(m_volumetric_intensity);
					L->set_volumetric_distance(m_volumetric_distance);

					L->set_ignore_object(ignore_object);
					for (int f=0; f<6; f++)L->set_decor_object(decor_object[f],f);

					L->set_hud_mode(flags.bHudMode);
					L->set_occq_mode(flags.bOccq);

					L->xform_calc();

					L->vis.pending = vis.pending;
					L->vis.visible = vis.visible;

					if (L->vis.pending)
						RImplementation.LP_pending.v_shadowed.push_back(L);
					else
						RImplementation.LP_normal.v_shadowed.push_back(L);
				}
			}
			break;
			case IRender_Light::SPOT:
			{
				vis_prepare();

				if (vis.pending)
					RImplementation.LP_pending.v_shadowed.push_back(this);
				else
					RImplementation.LP_normal.v_shadowed.push_back(this);
			}break;
		}
	}	
	else	
	{
		switch (flags.type)
		{
			case IRender_Light::POINT:
			{
				vis_prepare();

				if (vis.pending)
					RImplementation.LP_pending.v_point.push_back(this);
				else
					RImplementation.LP_normal.v_point.push_back(this);
			}break;
			case IRender_Light::SPOT:
			{
				vis_prepare();

				if (vis.pending)
					RImplementation.LP_pending.v_spot.push_back(this);
				else
					RImplementation.LP_normal.v_spot.push_back(this);				
			}break;
		}
	}
}

void light::set_attenuation_params	(float a0, float a1, float a2, float fo)
{
	attenuation0 = a0;
	attenuation1 = a1;
	attenuation2 = a2;
	falloff      = fo;
}

#endif

float	light::get_LOD					()
{
#ifndef _EDITOR
	if	(!flags.bShadow)	return 1;
	extern float r_ssaGLOD_start, r_ssaGLOD_end;
	extern float ps_r2_slight_fade;
	float	distSQ = Device.vCameraPosition.distance_to_sqr(SpatialComponent->spatial.sphere.P) + EPS;
	float	ssa = ps_r2_slight_fade * SpatialComponent->spatial.sphere.R/distSQ;
	float	lod = _sqrt(clampr((ssa - r_ssaGLOD_end)/(r_ssaGLOD_start-r_ssaGLOD_end),0.f,1.f));
	return lod;
#else
	return 1.0f;
#endif
}
