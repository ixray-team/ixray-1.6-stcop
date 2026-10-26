#ifndef __CAMERA_FE_H__
#define __CAMERA_FE_H__

#include "../xrEngine/CameraBase.h"

class CCameraFirstEye : public CCameraBase
{
	using inherited = CCameraBase;
public:
					CCameraFirstEye	( CObject* p, u32 flags=0);
	virtual ~CCameraFirstEye( ) override;

	virtual void	Load			(const char* section) override;
	virtual void	Move			( int cmd, float val=0, float factor=1.0f ) override;

	virtual void	OnActivate		( CCameraBase* old_cam ) override;
	virtual void	Update			( Fvector& point, Fvector& noise_angle, bool force_update_pos ) override;

	virtual float	GetWorldYaw		( ) override { return -yaw;	};
	virtual float	GetWorldPitch	( ) override { return pitch; };



	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;
};

#endif // __CAMERALOOK_H__
