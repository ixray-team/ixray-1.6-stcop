#ifndef	RenderVisual_included
#define	RenderVisual_included
#pragma once

class IKinematics;
class IKinematicsAnimated;
class IParticleCustom;
struct vis_data;

class IRenderVisual
{
public:
	IRenderVisual() = default;
	virtual ~IRenderVisual() {;}

	virtual vis_data&	_BCL	getVisData() = 0;
	virtual u32					getType() = 0;

	bool IsIgnoreOptimize = false;


	virtual shared_str getDebugName() = 0;
	virtual shared_str getShaderName() { return shared_str(""); }
	virtual shared_str getTextureName() { return shared_str(""); }
	virtual shared_str getOrigShaderName() { return shared_str(""); }
	virtual shared_str getOrigTextureName() { return shared_str(""); }

	virtual void set_shader(shared_str sh_name) { ; }
	virtual void set_texture(shared_str tex_name) { ; }

	virtual void reload_shader() { ; }
	virtual void restore_shader() { ; }
	virtual void restore_texture() { ; }

	virtual	IKinematics*	_BCL	dcast_PKinematics			()				{ return 0;	}
	virtual	IKinematicsAnimated*	dcast_PKinematicsAnimated	()				{ return 0;	}
	virtual IParticleCustom*		dcast_ParticleCustom		()				{ return 0;	}
};

#endif	//	RenderVisual_included