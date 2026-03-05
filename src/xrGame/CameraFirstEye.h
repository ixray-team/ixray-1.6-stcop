#ifndef __CAMERA_FE_H__
#define __CAMERA_FE_H__

#include "../xrEngine/CameraBase.h"

class CCameraFirstEye : public CCameraBase
{
	typedef CCameraBase inherited;
	Fvector			lookat_point;
	float			turn_speed_min = 1.0f; // Angle per second
	float			turn_speed_max = PI;
	bool			lookat_active;
	void			UpdateLookat	();
public:
					CCameraFirstEye	( CObject* p, u32 flags=0);
	virtual			~CCameraFirstEye( );

	virtual void	Load			(LPCSTR section);
	virtual void	Move			( int cmd, float val=0, float factor=1.0f );

	virtual	void	OnActivate		( CCameraBase* old_cam );
	virtual void	Update			( Fvector& point, Fvector& noise_angle );

	virtual float	GetWorldYaw		( )	{ return -yaw;	};
	virtual float	GetWorldPitch	( )	{ return pitch; };

			void	LookAtPoint		(Fvector p, float turnSpeedMin = 1.0f, float turnSpeedMax = PI);
			void	StopLookingAtPoint() { lookat_active = false;  }
			bool	IsLookingAtPoint() const { return lookat_active; }

			void save(NET_Packet& output_packet) override;
			void load(IReader& input_packet) override;
};

#endif // __CAMERALOOK_H__
