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


	virtual shared_str	getDebugName() = 0;


	virtual	IKinematics*	_BCL	dcast_PKinematics			()				{ return nullptr;	}
	virtual	IKinematicsAnimated*	dcast_PKinematicsAnimated	()				{ return nullptr;	}
	virtual IParticleCustom*		dcast_ParticleCustom		()				{ return nullptr;	}
};