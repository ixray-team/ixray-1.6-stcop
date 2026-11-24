#ifndef LAYERS_XRRENDER_LIGHT_H_INCLUDED
#define LAYERS_XRRENDER_LIGHT_H_INCLUDED

#include "../../xrCore/Collision/ISpatial.h"
//#include "../../xrEngine/xr_object.h"

class light :	
	public IRender_Light
{
public:
	struct
	{
		u32 type	:	4;
		u32 bStatic	:	1;
		u32 bActive	:	1;
		u32 bShadow	:	1;
		u32 bVolumetric:1;
		u32 bHudMode:	1;
		u32 bOccq:		1;
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
	u64 SpotShadowCacheId;


	float			m_volumetric_quality;
	float			m_volumetric_intensity;
	float			m_volumetric_distance;

	float			falloff;			// precalc to make light equal to zero at light range
	float	        attenuation0;		// Constant attenuation		
	float	        attenuation1;		// Linear attenuation		
	float	        attenuation2;		// Quadratic attenuation	

#ifndef _EDITOR
	light*						omnipart	[6]	;
#endif


	u32				m_xform_frame;
	// The frame the light's shadow map was last drawn by the forward level frame
	u32 ShadowFrame = u32(-1);
	// What a spot projects in the forward level frame, which has no accumulation shader to hold it
	ref_texture Projector;
	Fmatrix			m_xform;

	struct _vis		
	{
		u16			smap_ID;
	};

	_vis vis;

	union _xform
	{
		struct _D
		{
			Fmatrix						combine = {};
			s32							minX, maxX;
			s32							minY, maxY;
		}	D;

		struct _P
		{
			Fmatrix						world = {};
			Fmatrix						view = {};
			Fmatrix						project = {};
			Fmatrix						combine = {};
		}	P;

		struct _S
		{
			Fmatrix						view = {};
			Fmatrix						project = {};
			Fmatrix						combine = {};
			u32							size;
			u32							posX;
			u32							posY;
		}	S;

	};

	_xform X = {};

public:
	virtual void	set_type				(LT type) override { flags.type = type;		}
	virtual void	set_active				(bool b) override;
	virtual bool	get_active				() override { return flags.bActive;		}
	virtual void	set_shadow				(bool b) override;

	virtual void	set_volumetric			(bool b) override
	{ 
		flags.bVolumetric=b;			
	}

	virtual void	set_volumetric_quality(float fValue) override {m_volumetric_quality = fValue;}
	virtual void	set_volumetric_intensity(float fValue) override {m_volumetric_intensity = fValue;}
	virtual void	set_volumetric_distance(float fValue) override {m_volumetric_distance = fValue;}

	virtual void	set_position			(const Fvector& P) override;
	virtual void	set_rotation			(const Fvector& D, const Fvector& R) override;
	virtual void	set_cone				(float angle) override;
	virtual void	set_range				(float R) override;
	virtual void	set_virtual_size		(float R) override { virtual_size = R; }
	virtual void	set_color				(const Fcolor& C) override { color.set(C);				}
	virtual void	set_color				(float r, float g, float b) override { color.set(r,g,b,1);		}
	virtual void	set_texture				(const char* name) override;
	virtual void	set_hud_mode			(bool b) override {flags.bHudMode=b;}
	virtual bool	get_hud_mode			() override {return flags.bHudMode;};

	virtual void	set_occq_mode			(bool b) override {flags.bOccq=b;}
	virtual bool	get_occq_mode			() override {return flags.bOccq;};

	virtual void	set_ignore_object		(CObject* O) override {ignore_object=O;};
	virtual CObject* get_ignore_object		() override {return ignore_object;};

	virtual void	set_decor_object		(CObject* O, int index = 0) override {decor_object[index] = O; };
	virtual CObject* get_decor_object		(int index = 0) override {return decor_object[index];};

	virtual void	spatial_move			() override;
	virtual Fvector	spatial_sector_point	() override;
	virtual	void	spatial_updatesector_internal ();

	virtual IRender_Light*	dcast_Light		() override { return this; }

	virtual vis_data&		get_homdata		() override;
	void			xform_calc				();
#ifndef _EDITOR
	void			optimize_smap_size		();
	void UpdateOmniparts();
	void			set_attenuation_params	(float a0, float a1, float a2, float fo);
#endif // _EDITOR

	float			get_LOD					();

	light();
	virtual ~light() override;

	virtual void	destroy(bool deffered = true) override;

};

#endif // #define LAYERS_XRRENDER_LIGHT_H_INCLUDED
