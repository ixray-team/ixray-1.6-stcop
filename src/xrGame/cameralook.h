#pragma once

#include "../xrEngine/CameraBase.h"

class CCameraLook	: public CCameraBase
{
	typedef CCameraBase inherited;

protected:
	Fvector2		lim_zoom;
	float			dist, prev_d;

public:
					CCameraLook		( CObject* p, u32 flags=0);
	virtual ~CCameraLook	( ) override;
	virtual void	Load			(const char* section) override;
	virtual void	Move			( int cmd, float val=0, float factor=1.0f ) override;

	virtual void	OnActivate		( CCameraBase* old_cam ) override;
	virtual void	Update			( Fvector& point, Fvector& noise_dangle, bool force_update_pos ) override;

	virtual float	GetWorldYaw		( ) override { return -yaw;	};
	virtual float	GetWorldPitch	( ) override { return pitch; };
	virtual float	GetDist		( )	{ return prev_d;	};	
	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

protected:
			void	UpdateDistance	( Fvector& point );
};

class CCameraLook2	: public CCameraLook
{
	using base = CCameraBase;
	using inherited = CCameraLook;
	
public:
	static Fvector	m_cam_offset_r;
	static Fvector	m_cam_offset_l;
	static bool		m_use_inertion;

	CCameraLook2(CObject* p, u32 flags = 0) : CCameraLook(p, flags) {};

	virtual ~CCameraLook2	() override {}
	virtual void	OnActivate		( CCameraBase* old_cam ) override;
	virtual void	Update			( Fvector& point, Fvector& noise_dangle, bool force_update_pos ) override;
	virtual void	Load			(const char* section) override;
protected:
	void	UpdateDistance(Fvector& pivot, Fvector& correction);
};

class CCameraFixedLook : public CCameraLook
{
	using base = CCameraBase;
	using inherited = CCameraLook;
public:
					CCameraFixedLook(CObject* p, u32 flags=0) : CCameraLook(p, flags) {};
	virtual ~CCameraFixedLook() override {};
	virtual void	Load			(const char* section) override;
	virtual void	Move			(int cmd, float val=0, float factor=1.0f) override;
	virtual void	OnActivate		(CCameraBase* old_cam) override;
	virtual void	Update			(Fvector& point, Fvector& noise_dangle, bool force_update_pos ) override;
	virtual void	Set				(float Y, float P, float R) override;
private:
	Fquaternion		m_final_dir;
	Fquaternion		m_current_dir;
};

