#include "stdafx.h"
#include "../../xrEngine/_d3d_extensions.h"
#include "../../xrEngine/xrLevel.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "../../utils/xrLevelBuilder/xrLC/Light/R_light.h"
#include "Light_DB.h"

CLight_DB::CLight_DB()
{
}

CLight_DB::~CLight_DB()
{
}

void CLight_DB::Load			(IReader *fs) 
{
	IReader* F	= 0;

	// Lights itself
	sun_original		= nullptr;
	sun_adapted			= nullptr;

	rain_light			= new light;
	rain_light->set_type(IRender_Light::DIRECT);

	{
		F				= fs->open_chunk		(fsL_LIGHT_DYNAMIC);

		u32 size		= F->length();
		u32 element		= sizeof(Flight)+4;
		u32 count		= size/element;
		VERIFY			(count*element == size);
		v_static.reserve(count);
		for (u32 i=0; i<count; i++) 
		{
			Flight		Ldata;
			light*		L				= Create	();
			L->flags.bStatic			= true;
			L->set_type					(IRender_Light::POINT);

			L->set_shadow(g_RenderFeatures.LampShadows);
			u32 controller				= 0;
			F->r						(&controller,4);
			F->r						(&Ldata,sizeof(Flight));
			if (Ldata.type==D3DLIGHT_DIRECTIONAL)	{
				Fvector tmp_R;		tmp_R.set(1,0,0);

				// directional (base)
				sun_original		= L;
				SunBaked.set(Ldata.direction).normalize_safe();
				L->set_type			(IRender_Light::DIRECT);
				L->set_shadow		(true);
				L->direction.set(Ldata.direction);
				L->right.set(tmp_R);
				// copy to env-sun
				sun_adapted			=	L		= Create();
				L->flags.bStatic	=	true;
				L->set_type			(IRender_Light::DIRECT);
				L->set_shadow		(true);
				L->direction.set(Ldata.direction);
				L->right.set(tmp_R);
			}
			else
			{
				Fvector tmp_D,tmp_R;
				tmp_D.set			(0,0,-1);	// forward
				tmp_R.set			(1,0,0);	// right

				// point
				v_static.push_back	(L);
				L->position.set(Ldata.position);
				L->direction.set(tmp_D);
				L->right.set(tmp_R);
				L->range = Ldata.range;
				L->color.set(Ldata.diffuse);
				L->set_active(true);
			}
		}

		F->close			();
	}
	R_ASSERT2(sun_original && sun_adapted,"Where is sun?");

	// fake spot
	/*
	if (0)
	{
		Fvector	P;			P.set(-5.58f,	-0.00f + 2, -3.63f);
		Fvector	D;			D.set(0,-1,0);
		light*	fake		= Create();
		fake->set_type		(IRender_Light::SPOT);
		fake->set_color		(1,1,1);
		fake->set_cone		(deg2rad(60.f));
		fake->set_direction	(D);
		fake->set_position	(P);
		fake->set_range		(3.f);
		fake->set_active	(true);
	}
	*/
}

void CLight_DB::LoadEditor()
{
	rain_light = new light;
	rain_light->set_type(IRender_Light::DIRECT);

	const Fvector Right = { 1.f, 0.f, 0.f };
	const Fvector Down = { 0.f, -1.f, 0.f };

	light* L = Create();
	L->flags.bStatic = true;
	L->set_type(IRender_Light::DIRECT);
	L->set_shadow(true);
	L->direction.set(Down);
	L->right.set(Right);
	sun_original = L;

	L = Create();
	L->flags.bStatic = true;
	L->set_type(IRender_Light::DIRECT);
	L->set_shadow(true);
	L->direction.set(Down);
	L->right.set(Right);
	sun_adapted = L;
}

void			CLight_DB::Unload	()
{
	v_static.clear			();
	sun_original.destroy	();
	sun_adapted.destroy		();

	if (rain_light)
	{
		rain_light->destroy(false);
		rain_light = nullptr;
	}
}

light*			CLight_DB::Create	()
{
	light*	L			= new light	();
	L->flags.bStatic	= false;
	L->flags.bActive	= false;
	L->flags.bShadow	= true;
	return				L;
}


void CLight_DB::add_light(light* L)
{
	// Several editor viewports render within one frame, each with lights of its own
	if (RFrame.dwFrame == L->frame_render && !Device.IsEditorMode())
	{
		return;
	}
	L->frame_render = RFrame.dwFrame;
	// What is baked into the level's lightmaps is not lit a second time
	if (!L->flags.bStatic)
	{
		RImplementation.SurfaceLights.push_back(L);
	}
}

void			CLight_DB::Update			()
{
	// set sun params
	if (sun_original && sun_adapted)
	{
		light*	_sun_original		= (light*) sun_original._get();
		light*	_sun_adapted		= (light*) sun_adapted._get();
		const CEnvDescriptor&	E			= RFrame.Env();
		VERIFY						(_valid(E.sun_dir));
#ifdef DEBUG
		if(E.sun_dir.y>=0)
		{
//			Log("sect_name", E.sect_name.c_str());
			Log("E.sun_dir", E.sun_dir);
			Msg("E.wind_direction %f",E.wind_direction);
			Msg("E.wind_velocity %f",E.wind_velocity);
			Log("E.sun_color",E.sun_color);
			Log("E.rain_color",E.rain_color);
			Msg("E.rain_density %f",E.rain_density);
			Msg("E.fog_distance %f",E.fog_distance);
			Msg("E.fog_density %f",E.fog_density);
			Log("E.fog_color",E.fog_color);
			Msg("E.far_plane %f",E.far_plane);
			Msg("E.sky_rotation %f",E.sky_rotation);
			Log("E.sky_color",E.sky_color);
		}
#endif

		VERIFY2						(E.sun_dir.y<0,"Invalid sun direction settings in evironment-config");
		Fvector						OD,OP,AD,AP;
		const Fvector Sun = SunDirection();
		OD.set						(Sun).normalize			();
		OP.mad						(RFrame.vCameraPosition,OD,-500.f);
		AD.set(0, IsSunBaked() ? 0.f : -.75f, 0).add(Sun);

		// for some reason E.sun_dir can point-up
		int		counter = 0;
		while	(AD.magnitude()<0.001 && counter<10)	{
			AD.add(Sun); counter++;
		}
		AD.normalize				();
		AP.mad						(RFrame.vCameraPosition,AD,-500.f);
		sun_original->set_rotation	(OD,_sun_original->right	);
		sun_original->set_position	(OP);
		sun_original->set_color		(E.sun_color.x,E.sun_color.y,E.sun_color.z);
		sun_original->set_range		(600.f);
		sun_adapted->set_rotation	(AD, _sun_adapted->right	);
		sun_adapted->set_position	(AP		);
		sun_adapted->set_color		(E.sun_color.x*ps_r2_sun_lumscale,E.sun_color.y*ps_r2_sun_lumscale,E.sun_color.z*ps_r2_sun_lumscale);
		sun_adapted->set_range		(600.f	);
		
		if (!::Render->is_sun_static() && !EngineExternal().ShadowOfChernobylMode())
		{
			sun_adapted->set_rotation(OD, _sun_original->right);
			sun_adapted->set_position (OP);
		}
	}
}

bool CLight_DB::IsSunBaked() const
{
	return sun_original && !ForwardDynamicLight();
}

Fvector CLight_DB::SunDirection() const
{
	return IsSunBaked() ? SunBaked : RFrame.Env().sun_dir;
}
