#pragma once
#include "RenderAPI.h"

#include "../../xrEngine/vis_common.h"
#include "../../xrEngine/FmeshRender.h"
#include "../../Include/xrRender/RenderVisual.h"

#define VLOAD_NOVERTICES		(1<<0)

// The class itself
class					CKinematicsAnimated;
class					CKinematics;
class					IParticleCustom;

// The class itself
class RENDER_API dxRender_Visual :
	public IRenderVisual
{
public:
    ogf_desc					desc		;

	shared_str					dbg_name	;
    virtual shared_str getDebugName() override { return dbg_name; }
    virtual shared_str getShaderName() override { return shader_name; }
    virtual shared_str getTextureName() override { return texture_name; }
    virtual shared_str getOrigShaderName() override { return orig_shader_name; }
    virtual shared_str getOrigTextureName() override { return orig_texture_name; }
    virtual void set_shader(shared_str sh_name) override { shader_name = sh_name; }
    virtual void set_texture(shared_str tex_name) override { texture_name = tex_name; }
    virtual void reload_shader() override;
    virtual void restore_shader() override;
    virtual void restore_texture() override;

public:
	// Common data for rendering
	u32							Type		;				// visual's type
	vis_data					vis			;				// visibility-data
	ref_shader					shader		;				// pipe state, shared
	xr_atomic_bool IsSpotShadowCacheReferenced = false;
	shared_str					texture_name, orig_texture_name;
	shared_str					shader_name, orig_shader_name;
	virtual void				Render						(float LOD)		{};		// LOD - Level Of Detail  [0..1], Ignored
	virtual void				Load						(const char* N, IReader *data, u32 dwFlags);
	virtual void				Release						();						// Shared memory release
	virtual void				Copy						(dxRender_Visual* from);
	virtual void				Spawn						()				{};
	virtual void				Depart						()				{};

    virtual vis_data&	_BCL	getVisData() override { return vis;}
    virtual u32					getType() override { return Type;}

	dxRender_Visual				();
    virtual ~dxRender_Visual		() override;
};
