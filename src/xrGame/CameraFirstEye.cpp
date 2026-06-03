#include "stdafx.h"
#include "CameraFirstEye.h"
#include "../xrEngine/xr_level_controller.h"
#include "../xrEngine/xr_object.h"
#include "object_broker.h"

// CAM_1
CCameraFirstEye::CCameraFirstEye(CObject* p, u32 flags ) : CCameraBase(p, flags)
{
}

CCameraFirstEye::~CCameraFirstEye()
{
}

void CCameraFirstEye::Load(const char* section)
{
	inherited::Load		(section);
	style				= csFirstEye;
}

void CCameraFirstEye::save(NET_Packet& packet) 
{ 
	save_data(pitch, packet); 
}

void CCameraFirstEye::load(IReader& packet)
{
	load_data(pitch, packet); 
}

void CCameraFirstEye::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CCameraFirstEye")
	{
		Object << pitch;
	}
}

void CCameraFirstEye::Update(Fvector& point, Fvector& noise_dangle, bool force_update_pos)
{
	inherited::Update(point, noise_dangle, force_update_pos);

	if (m_Flags.is(flRelativeLink))
	{
		parent->XFORM().transform_dir(vDirection);
		parent->XFORM().transform_dir(vNormal);
	}
}

void CCameraFirstEye::Move( int cmd, float val, float factor )
{
	if (bClampPitch)
	{
		while (pitch < lim_pitch[0])
			pitch += PI_MUL_2;
		while (pitch > lim_pitch[1])
			pitch -= PI_MUL_2;
	};
	switch (cmd){
	case kDOWN:		pitch	-= val?val:(rot_speed.y*Device.fTimeDelta/factor);	break;
	case kUP:		pitch	+= val?val:(rot_speed.y*Device.fTimeDelta/factor);	break;
	case kLEFT:		yaw		-= val?val:(rot_speed.x*Device.fTimeDelta/factor);	break;
	case kRIGHT:	yaw		+= val?val:(rot_speed.x*Device.fTimeDelta/factor);	break;
	}
	if (bClampYaw)		clamp(yaw,lim_yaw[0],lim_yaw[1]);
	if (bClampPitch)	clamp(pitch,lim_pitch[0],lim_pitch[1]);
}

void CCameraFirstEye::OnActivate( CCameraBase* old_cam )
{
	if (old_cam) {
		if (m_Flags.is(flRelativeLink) == old_cam->m_Flags.is(flRelativeLink))
			yaw = (old_cam)->yaw;

		if (m_Flags.is(flKeepPitch))
			pitch = (old_cam)->pitch;
	}
}