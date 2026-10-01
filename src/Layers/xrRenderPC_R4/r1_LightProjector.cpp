#include "stdafx.h"
#include "r1_LightProjector.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "../../xrEngine/xr_object.h"
#include "LightTrack.h"
#include "SkeletonCustom.h"

const	float		P_distance		= 50;					
const	float		P_cam_dist		= 200;
const	float		P_cam_range		= 7.f;
const ERHI_FORMAT P_rtf = ERHI_FORMAT::R8G8B8A8_UNORM;
const	float		P_blur_kernel	= .5f;
const	int			time_min		= 30*1000	;
const	int			time_max		= 90*1000	;
const	float		P_ideal_size	= 1.f		;

float	clipD		(float R)		{ return P_distance*(R/P_ideal_size); }

CLightProjector::CLightProjector()
{
	current				= 0;
	RT					= 0;

	RT.create("$user$projector", P_rt_size, P_rt_size, P_rtf);
	depth.create("$user$r1_projector_depth", P_rt_size, P_rt_size, ERHI_FORMAT::D24_UNORM_S8_UINT);
	clear.create("r1_projector_clear");

	c_xform				= "m_plmap_xform";
	c_clamp				= "m_plmap_clamp";
	c_factor			= "m_plmap_factor";

	cache.resize		(P_o_count);
	Device.seqAppActivate.Add		(this);
}

CLightProjector::~CLightProjector()
{
	Device.seqAppActivate.Remove	(this);
	RT.destroy			();
}

void CLightProjector::set_object	(IRenderable* O)
{
	if ((0==O) || (receivers.size()>=P_o_count))	current		= 0;
	else
	{
		if (!O->renderable_ShadowReceive() || RImplementation.val_bInvisible || ((CROS_impl*)O->renderable_ROS())->shadow_recv_frame==Device.dwFrame)	
		{
			current		= 0;
			return;
		}

		const vis_data &vis = O->renderable.visual->getVisData();
		Fvector		C;	O->renderable.xform.transform_tiny		(C,vis.sphere.P);
		float		R	= vis.sphere.R;
		float		D	= C.distance_to	(Device.vCameraPosition)+R;

		if (D < clipD(R))	current	= O;
		else				current = 0;
		
		if (current)
		{
			ISpatial* spatial = O->SpatialComponent.get();
			if (0 == spatial)
			{
				current = 0;
			}
			else
			{
				spatial->spatial_updatesector();
				if (0 == spatial->sector)
				{
					CObject* obj = dynamic_cast<CObject*>(O);
					if (obj)
					{
						Msg("! Invalid object '%s' position. Outside of sector structure.", obj->cName().c_str());
					}
					current = 0;
				}
			}
		}
		if (current)				{
			CROS_impl*	LT			= (CROS_impl*)current->renderable_ROS	();
			LT->shadow_recv_frame	= Device.dwFrame;
			receivers.push_back		(current);
		}
	}
}

void CLightProjector::setup		(int id)
{
	if (id>=int(cache.size()) || id<0)	{
		
		return;
	}
	recv&			R			= cache[id];
	float			Rd			= R.O->renderable.visual->getVisData().sphere.R;
	float			dist		= R.C.distance_to	(Device.vCameraPosition)+Rd;
	float			factor		= _sqr(dist/clipD(Rd))*(1-ps_r1_lmodel_lerp) + ps_r1_lmodel_lerp;
	RCache.set_c	(c_xform,	R.UVgen);
	Fvector&	m	= R.UVclamp_min;
	RCache.set_ca	(c_clamp,	0,m.x,m.y,m.z,factor);
	Fvector&	M	= R.UVclamp_max;
	RCache.set_ca	(c_clamp,	1,M.x,M.y,M.z,0);
}

void CLightProjector::invalidate()
{
	for (u32 c_it=0; c_it<cache.size(); c_it++)
		cache[c_it].dwTimeValid	= 0;
}

void CLightProjector::OnAppActivate()
{
	invalidate					();
}

void CLightProjector::calculate	()
{
	if (receivers.empty())		return;

	for (u32 r_it=0; r_it<receivers.size(); r_it++)
	{
		
		bool bValid	= true;
		IRenderable*		O		= receivers[r_it];
		CROS_impl*			LT		= (CROS_impl*)O->renderable_ROS();
		int					slot	= LT->shadow_recv_slot;
		if (slot<0 || slot>=P_o_count)								bValid = false;	
		else if (cache[slot].O!=O)									bValid = false;	
		else {
			
			Fbox	bb;		bb.xform		(O->renderable.visual->getVisData().box,O->renderable.xform);
			if (cache[slot].BB.contains(bb))	{
				
				if (Device.dwTimeGlobal > cache[slot].dwTimeValid)	bValid = false;	
			} else													bValid = false;	
		}

		if (bValid)			{
			
			cache[slot].dwFrame	= Device.dwFrame;
		} else {
			taskid.push_back	(r_it);
		}
	}
	if (taskid.empty())			return;

	Device.Statistic->RenderDUMP_Pcalc.Begin	();
	RImplementation.Target->u_setrt(RT, nullptr, depth->pZRT);
	GRHI->ClearDepthStencil(depth->pZRT, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);
	RCache.set_xform_world		(Fidentity);

	for (u32 c_it=0; c_it<cache.size(); c_it++)
	{
		if (taskid.empty())							break;
		if (Device.dwFrame==cache[c_it].dwFrame)	continue;

		int				tid		= taskid.back();	taskid.pop_back();
		recv&			R		= cache		[c_it];
		IRenderable*	O		= receivers	[tid];
		const vis_data& vis = O->renderable.visual->getVisData();
		CROS_impl*	LT		= (CROS_impl*)O->renderable_ROS();
		VERIFY2			(_valid(O->renderable.xform),"Invalid object transformation");
		VERIFY2			(_valid(vis.sphere.P),"Invalid object's visual sphere");

		Fvector			C;		O->renderable.xform.transform_tiny		(C,vis.sphere.P);
		R.O						= O;
		R.C						= C;
		R.C.y					+= vis.sphere.R*0.1f;		
		R.BB.xform				(vis.box,O->renderable.xform).scale(0.1f);
		R.dwTimeValid			= Device.dwTimeGlobal + ::Random.randI(time_min,time_max);
		LT->shadow_recv_slot	= c_it; 

		Fmatrix		mProject;
		float		p_R			=	R.O->renderable.visual->getVisData().sphere.R * 1.1f;
		
		VERIFY3		(p_R>EPS_L,"Object has no physical size", R.O->renderable.visual->getDebugName().c_str());
		float		p_hat		=	p_R/P_cam_dist;
		float		p_asp		=	1.f;
		float		p_near		=	P_cam_dist-EPS_L;									
		float		p_far		=	P_cam_dist+p_R+P_cam_range;	
		mProject.build_projection_HAT	(p_hat,p_asp,p_near,p_far);
		RCache.set_xform_project		(mProject);

		Fmatrix		mView;
		Fvector		v_C, v_Cs, v_N;
		v_C.set					(R.C);
		v_Cs					= v_C;
		v_C.y					+=	P_cam_dist;
		v_N.set					(0,0,1);
		VERIFY					(_valid(v_C) && _valid(v_Cs) && _valid(v_N));

		Fvector		v;
		v.sub		(v_Cs,v_C);;
#ifdef DEBUG
		if ((v.x*v.x+v.y*v.y+v.z*v.z)<=flt_zero)	{
			CObject* OO = dynamic_cast<CObject*>(R.O);
			Msg("Object[%s] Visual[%s] has invalid position. ",*OO->cName(),*OO->cNameVisual());
			Fvector cc;
			OO->Center(cc);
			Log("center=",cc);

			Log("visual_center=",OO->Visual()->getVisData().sphere.P);
			
			Log("full_matrix=",OO->XFORM());

			Log	("v_N",v_N);
			Log	("v_C",v_C);
			Log	("v_Cs",v_Cs);

			Log("all bones transform:--------");
			CKinematics* K = dynamic_cast<CKinematics*>(OO->Visual());
			
			for(u16 ii=0; ii<K->LL_BoneCount();++ii){
				Fmatrix tr;

				tr = K->LL_GetTransform(ii);
				Msg("bone %s", K->LL_BoneName_dbg(ii));
				Log("bone_matrix", tr);
			}
			Log("end-------");
		}
#endif
		
		if ((v.x*v.x+v.y*v.y+v.z*v.z)<=flt_zero)	{
			
			R.dwTimeValid			= Device.dwTimeGlobal;
			LT->shadow_recv_frame	= Device.dwFrame-1;
			LT->shadow_recv_slot	= -1; 
			continue				;
		}

		mView.build_camera		(v_C,v_Cs,v_N);
		RCache.set_xform_view	(mView);

		int		s_x				=	c_it%P_o_line;
		int		s_y				=	c_it/P_o_line;
		RHIViewport VP = { (float)s_x * P_o_size, (float)s_y * P_o_size, (float)P_o_size, (float)P_o_size, 0, 1 };
		GRHI->SetViewport(VP);

		Fvector&	cap			=	LT->get_approximate();
		RCache.set_Element(clear->E[0]);
		RCache.set_c("tfactor", cap.x, cap.y, cap.z, (cap.x + cap.y + cap.z) / 4.f);
		RCache.set_Geometry(RImplementation.Target->FSTriangleGeom);
		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

		Fmatrix					mCombine;		mCombine.mul	(mProject,mView);
		Fmatrix					mTemp;
		float					fSlotSize		= float(P_o_size)/float(P_rt_size);
		float					fSlotX			= float(s_x*P_o_size)/float(P_rt_size);
		float					fSlotY			= float(s_y*P_o_size)/float(P_rt_size);
		float					fTexelOffs		= (.5f / P_rt_size);
		Fmatrix					m_TexelAdjust	= 
		{
			0.5f,	0.0f,							0.0f,				0.0f,
			0.0f,				-0.5f,				0.0f,				0.0f,
			0.0f,				0.0f,							1.0f,	0.0f,
			0.5f,		0.5f + fTexelOffs,	0.0f,		1.0f
		};
		R.UVgen.mul				(m_TexelAdjust,mCombine);
		mTemp.scale				(fSlotSize,fSlotSize,1);
		R.UVgen.mulA_44			(mTemp);
		mTemp.translate			(fSlotX+fTexelOffs,fSlotY+fTexelOffs,0);
		R.UVgen.mulA_44			(mTemp);

		Fvector					min,max;
		Fbox					BB;
		min.set					(R.C.x-p_R,	R.C.y-(p_R+P_cam_range),	R.C.z-p_R);
		max.set					(R.C.x+p_R,	R.C.y+0,					R.C.z+p_R);
		BB.set					(min,max);
		R.UVclamp_min.set		(min).add	(.05f);	
		R.UVclamp_max.set		(max).sub	(.05f);	
		ISpatial*	spatial		= O->SpatialComponent.get();
		if (spatial)			{
			spatial->spatial_updatesector			();
			if (spatial->sector)			RImplementation.r_dsgraph_render_R1_box	(spatial->sector,BB,SE_R1_LMODELS);
		}
		
	}

	Device.Statistic->RenderDUMP_Pcalc.End	();
	
	RCache.set_xform_project	(Device.mProject);
	RCache.set_xform_view		(Device.mView);
}

#ifdef DEBUG
void CLightProjector::render	()
{
	
}
#endif
