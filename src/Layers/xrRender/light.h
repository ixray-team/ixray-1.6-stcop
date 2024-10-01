#ifndef LAYERS_XRRENDER_LIGHT_H_INCLUDED
#define LAYERS_XRRENDER_LIGHT_H_INCLUDED

#include "../../xrcdb/ispatial.h"
//#include "../../xrEngine/xr_object.h"
#if (RENDER==R_R2) || (RENDER==R_R4)
#	include "light_package.h"
#endif //(RENDER==R_R2) || (RENDER==R_R4)

class	light		:	public IRender_Light, public ISpatial
{
public:
	struct {
		u32			type	:	4;
		u32			bStatic	:	1;
		u32			bActive	:	1;
		u32			bShadow	:	1;
		u32			bVolumetric:1;
		u32			bHudMode:	1;
		u32			bOccq:		1;
	} flags;

	Fvector			position	;
	Fvector			direction	;
	Fvector			right		;
	float			range		;
	float			virtual_size;
	float			cone		;
	Fcolor			color		;
	CObject			*ignore_object, *decor_object[6];
	vis_data		hom			;
	u32				frame_render;

#if RENDER!=R_R1
	xr_vector<IRender_Sector*> m_sectors;
#endif	//	RENDER!=R_R1
	float			m_volumetric_quality;
	float			m_volumetric_intensity;
	float			m_volumetric_distance;

#if (RENDER==R_R2) || (RENDER==R_R4) || defined(_EDITOR)
	float			falloff;			// precalc to make light equal to zero at light range
	float	        attenuation0;		// Constant attenuation		
	float	        attenuation1;		// Linear attenuation		
	float	        attenuation2;		// Quadratic attenuation	

#ifndef _EDITOR
	light*						omnipart	[6]	;
	light*						h_parent;
	u32				m_test_frame_parent;
	void	parent_vis_prepare();
	void	parent_vis_update();
#endif

	ref_shader		s_spot;
	ref_shader		s_point;
	ref_shader		s_volumetric;

	u32				m_xform_frame;
	Fmatrix			m_xform;
	bool			b_need_recompute_xform;
	bool			b_need_detect_sectors;
	struct _vis		{
		u32			frame2test;		// frame the test is sheduled to
		u32			query_id;		// ID of occlusion query
		u32			query_order;	// order of occlusion query
		bool		visible;		// visible/invisible
		bool		pending;		// test is still pending
		bool		skipfulltest;
		bool		camerainbounds;
		u32			smap_ID;
	}				vis;

	union			_xform	{
		struct		_D		{
			Fmatrix						combine	;
			s32							minX,maxX	;
			s32							minY,maxY	;
			BOOL						transluent	;
		}	D;
		struct		_P		{
			Fmatrix						world		;
			Fmatrix						view		;
			Fmatrix						project		;
			Fmatrix						combine		;
		}	P;
		struct		_S		{
			Fmatrix						view		;
			Fmatrix						project		;
			Fmatrix						combine		;
			u32							size		;
			u32							posX		;
			u32							posY		;
			BOOL						transluent	;
		}	S;
	}	X;
#endif	//	(RENDER==R_R2) || (RENDER==R_R4)

public:
#if RENDER!=R_R1
	void get_sectors();
#endif	//	RENDER!=R_R1
	virtual void	set_type				(LT type)						{ flags.type = type;		}
	virtual void	set_active				(bool b);
	virtual bool	get_active				()								{ return flags.bActive;		}
	virtual void	set_shadow				(bool b);
	virtual void	set_volumetric			(bool b)						
	{ 
		flags.bVolumetric=b;			
	}

	virtual void	set_volumetric_quality(float fValue) {m_volumetric_quality = fValue;}
	virtual void	set_volumetric_intensity(float fValue) {m_volumetric_intensity = fValue;}
	virtual void	set_volumetric_distance(float fValue) {m_volumetric_distance = fValue;}
	
	virtual void	set_position			(const Fvector& P);
	virtual void	set_rotation			(const Fvector& D, const Fvector& R);
	virtual void	set_cone				(float angle);
	virtual void	set_range				(float R);
	virtual void	set_virtual_size		(float R) { virtual_size = R; }
	virtual void	set_color				(const Fcolor& C)				{ color.set(C);				}
	virtual void	set_color				(float r, float g, float b)		{ color.set(r,g,b,1);		}
	virtual void	set_texture				(LPCSTR name);
	virtual void	set_hud_mode			(bool b)						{flags.bHudMode=b;}
	virtual bool	get_hud_mode			()								{return flags.bHudMode;};

	virtual void	set_occq_mode			(bool b)						{flags.bOccq=b;}
	virtual bool	get_occq_mode			()								{return flags.bOccq;};

	virtual void	set_ignore_object		(CObject* O)					{ignore_object=O;};
	virtual CObject* get_ignore_object		()								{return ignore_object;};

	virtual void	set_decor_object		(CObject* O, int index = 0)					{decor_object[index] = O; };
	virtual CObject* get_decor_object		(int index = 0)								{return decor_object[index];};

	virtual	void	spatial_move			();
	virtual	Fvector	spatial_sector_point	();

	virtual IRender_Light*	dcast_Light		()	{ return this; }

	virtual vis_data&		get_homdata		();
#if (RENDER==R_R2) || (RENDER==R_R4) || defined(_EDITOR)
	void			xform_calc				();
#ifndef _EDITOR
	void			optimize_smap_size		();
	void			vis_prepare				();
	void			vis_update				();
	void			export_ 					(light_Package& dest);
	void			set_attenuation_params	(float a0, float a1, float a2, float fo);
#endif // _EDITOR
#endif // (RENDER==R_R2) || (RENDER==R_R4)

	float			get_LOD					();

	light();
	virtual ~light();
};

#endif // #define LAYERS_XRRENDER_LIGHT_H_INCLUDED