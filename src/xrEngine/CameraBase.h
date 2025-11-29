#pragma once

#include "CameraDefs.h"
#include "CameraManager.h"
#include "device.h"

class CObject;

class ENGINE_API	CCameraBase
{
protected:
	CObject*		parent;

public:
	bool bClampYaw, bClampPitch, bClampRoll;
	float yaw, pitch, roll;
	float prev_yaw, prev_pitch, prev_roll;

	Fvector lookat_point;
	float turn_speed_min = 1.0f; // Angle per second
	float turn_speed_max = PI;
	bool lookat_active;

	enum{
		flRelativeLink		= (1<<0),
		flPositionRigid		= (1<<1),
		flDirectionRigid	= (1<<2),
		flKeepPitch = (1 << 3),
	};
	Flags32			m_Flags;

	ECameraStyle	style;
	Fvector2		lim_yaw,lim_pitch,lim_roll;
	Fvector			rot_speed;

	Fvector			vPosition;
	Fvector			vDirection;
	Fvector			vNormal;
	float			f_fov;
	float			f_aspect;

IC 	Fvector			Position				()	const { return vPosition;	}
IC 	Fvector			Direction				()	const { return vDirection;}
IC 	Fvector			Up						()	const { return vNormal;	}
IC 	Fvector			Right					()	const { return Fvector().crossproduct( vNormal, vDirection ); }
IC 	float			Fov						()	const { return f_fov; }
IC 	float			Aspect					()	const { return f_aspect; }

	int				tag;
public:
					CCameraBase		( CObject* p, u32 flags );
	virtual			~CCameraBase	( );
	virtual void	Load			(const char* section);
	void			SetParent		( CObject* p )								{parent=p; VERIFY(p);}
	virtual	void	OnActivate		( CCameraBase* old_cam )					{;}
	virtual	void	OnDeactivate	( )											{;}
	virtual void	Move			( int cmd, float val=0, float factor=1.0f)	{; }

	virtual void	Update(Fvector& point, Fvector& noise_angle, bool force_update_pos = true)
	{
		UpdateLookat();
		
		if (static bool is_initialized; !is_initialized)
		{
			prev_yaw = yaw;
			prev_pitch = pitch;
			prev_roll = roll;

			is_initialized = true;
		}

		if (!m_Flags.test(flDirectionRigid))
		{
			prev_yaw = _inertion(prev_yaw, yaw, psCamInert);
			prev_pitch = _inertion(prev_pitch, pitch, psCamInert);
			prev_roll = _inertion(prev_roll, roll, psCamInert);
		}
		else
		{
			prev_yaw = yaw;
			prev_pitch = pitch;
			prev_roll = roll;
		}
		
		Fmatrix mR, R;
		Fmatrix rX, rY, rZ;

		rX.rotateX(noise_angle.x);
		rY.rotateY(-noise_angle.y);
		rZ.rotateZ(noise_angle.z);
		R.mul_43(rY, rX);
		R.mulB_43(rZ);

		mR.identity();

		Fquaternion Q;
		Q.rotationYawPitchRoll(prev_roll, prev_yaw, prev_pitch);

		mR.rotation(Q);
		mR.transpose();
		mR.mulB_43(R);
	
		vDirection.set(mR.k);
		vNormal.set(mR.j);
		
		if (force_update_pos)
		{
			vPosition.set(point);
		}
	}

	void UpdateLookat();
	void LookAtPoint(Fvector p, float turnSpeedMin = 1.0f, float turnSpeedMax = PI);
	void StopLookingAtPoint() { lookat_active = false; }
	bool IsLookingAtPoint() const { return lookat_active; }

	virtual void	Get				( Fvector& P, Fvector& D, Fvector& N )		{P.set(vPosition);D.set(vDirection);N.set(vNormal);}
	virtual void	Set				( const Fvector& P, const Fvector& D, const Fvector& N ){vPosition.set(P);vDirection.set(D);vNormal.set(N);}
	virtual void	Set				( float Y, float P, float R )				{yaw=Y;pitch=P;roll=R;}
	
	virtual float	GetWorldYaw		( )	{ return 0; };
	virtual float	GetWorldPitch	( )	{ return 0; };

	virtual float	CheckLimYaw		( );
	virtual float	CheckLimPitch	( );
	virtual float	CheckLimRoll	( );

	virtual void save(NET_Packet& output_packet) {};
	virtual void load(IReader& input_packet) {};
	virtual void Serialize(ISaveObject& Object) {};
};

template<typename T>
IC void tviewport_size(float _viewport_near, const T& cam_info, float& h_w, float& h_h)
{
	h_h = _viewport_near * tan(deg2rad(cam_info.Fov()) / 2.f);
	VERIFY2(_valid(h_h), make_string<const char*>("invalide viewporrt params fov: %f ", cam_info.Fov()));
	float aspect = Device.fASPECT;
	VERIFY(aspect > EPS);
	h_w = h_h / aspect;
}

template<typename T>
IC void viewport_size(float _viewport_near, const T& cam_info, float& h_w, float& h_h)
{
	tviewport_size<T>(_viewport_near, cam_info, h_w, h_h);
}