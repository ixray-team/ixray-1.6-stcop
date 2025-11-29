#ifndef __CAMERA_FE_H__
#define __CAMERA_FE_H__

#include "../xrEngine/CameraBase.h"

class CCameraFirstEye : public CCameraBase
{
	using inherited = CCameraBase;
public:
					CCameraFirstEye	( CObject* p, u32 flags=0);
	virtual			~CCameraFirstEye( );

	virtual void	Load			(const char* section);
	virtual void	Move			( int cmd, float val=0, float factor=1.0f );

	virtual	void	OnActivate		( CCameraBase* old_cam );
	virtual void	Update			( Fvector& point, Fvector& noise_angle, bool force_update_pos );

	virtual float	GetWorldYaw		( )	{ return -yaw;	};
	virtual float	GetWorldPitch	( )	{ return pitch; };



			void save(NET_Packet& output_packet) override;
			void load(IReader& input_packet) override;
			virtual void Serialize(ISaveObject& Object) override;
};

#endif // __CAMERALOOK_H__
