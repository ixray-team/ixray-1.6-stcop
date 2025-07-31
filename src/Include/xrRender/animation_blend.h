#pragma once

#include	"animation_motion.h"
//*** Run-time Blend definition *******************************************************************
#ifdef	DEBUG
class	bnon_copy
{

protected:
	bnon_copy(){}
protected:
	bnon_copy( const bnon_copy& ){ }
protected:
	const bnon_copy& operator=( const bnon_copy& ){ return *this; }
};
#endif
class  CBlend 
{
public:
	enum ECurvature
	{
		eFREE_SLOT=0,
		//		eFixed,
		eAccrue,
		eFalloff,
		eFORCEDWORD = u32(-1)
	};
public:
	float			blendAmount = 0.0f;
	float			timeCurrent = 0.0f;
	float			timeTotal = 0.0f;
	MotionID		motionID = {};
	u16				bone_or_part = 0;	// startup parameters
	u8				channel = 0;

	struct NotifyKeyframeData
	{
		float key;
		xr_vector<anim_notify*> assigned;
	};
	
	struct NotifyKeyframes
	{
		xr_vector<NotifyKeyframeData> keyframes;
		s64 current_notify_index = 0;
	};
	
	xr_hash_map<u16, NotifyKeyframes> notifies = {}; // per bone notifies
private:
	ECurvature		blend = ECurvature::eFREE_SLOT;
public:
	float			blendAccrue = 0.0f;	// increasing
	float			blendFalloff = 0.0f;	// decreasing
	float			blendPower = 0.0f;			
	float			speed = 0.0f;

	BOOL			playing = false;
	BOOL			stop_at_end_callback = false;
	BOOL			update_callback = false;
	BOOL			stop_at_end = false;
	BOOL			fall_at_end = false;
	PlayCallback	Callback = nullptr;
	void*			CallbackParam = nullptr;

	u32				dwFrame = 0;

	u32				mem_usage			(){ return sizeof(*this); }
IC	bool			update_time			( float dt );
IC  void			update_play			( float dt, PlayCallback _Callback );
IC	bool			update_falloff		( float dt );
IC	bool			update				( float dt, PlayCallback _Callback );
IC	ECurvature		blend_state			(  )const { return blend ;}
IC	void			set_free_state		( ){ blend = eFREE_SLOT; }
IC	void			set_accrue_state	( ){ blend = eAccrue; }
IC	void			set_falloff_state	( ){ blend = eFalloff; }
IC	void			set					( const CBlend &r ){ *this = r; }
#ifdef	DEBUG
CBlend(  )
{

}

CBlend( const CBlend& r )
{
	*this = r;
}

const CBlend& operator=( const CBlend& r )
{ 
	VERIFY( r.blend_state()!=eFREE_SLOT );
	blendAmount			=	r.blendAmount			;
	timeCurrent			=	r.timeCurrent			;
	timeTotal			=	r.timeTotal				;
	motionID			=	r.motionID				;
	bone_or_part		=	r.bone_or_part			;
	channel				=	r.channel				;
	blend				=	r.blend					;
	blendAccrue			=	r.blendAccrue			;
	blendFalloff		=	r.blendFalloff			;
	blendPower			=	r.blendPower			;
	speed				=	r.speed					;
	playing				=	r.playing				;
	stop_at_end_callback=	r.stop_at_end_callback	;
	update_callback		=	r.update_callback		;
	stop_at_end			=	r.stop_at_end			;
	fall_at_end			=	r.fall_at_end			;
	Callback			=	r.Callback				;
	CallbackParam		=	r.CallbackParam			;
	dwFrame				=	r.dwFrame				;
	return *this; 
}

#endif
};



IC void CBlend::update_play( float dt, PlayCallback _Callback )
{

	float pow_dt = dt;
	if( pow_dt < 0.f )
	{
		pow_dt = 0;
		if( stop_at_end )
		{
			VERIFY( blendAccrue>0.f );
			pow_dt = timeCurrent + dt - 1.f/blendAccrue;
			clamp( pow_dt, dt, 0.f );
		}
	}
	
	blendAmount 		+= pow_dt*blendAccrue*blendPower;

	clamp				( blendAmount, 0.f, blendPower); 

	if (_Callback && update_callback && stop_at_end_callback)
		_Callback( this );


	if( !update_time( dt ) )//reached end 
		return;

	if ( _Callback &&  stop_at_end_callback )	
		_Callback( this );		// callback only once

	stop_at_end_callback		= FALSE;

	if( fall_at_end )
	{
		blend = eFalloff;
		blendFalloff = 2.f;
		//blendAccrue = timeCurrent;
	}
	return ;
}

IC	bool CBlend::update_time			( float dt )
{
	if (!playing) 
			return false;
	float quant = dt*speed;
	timeCurrent += quant; // stop@end - time is not going

	bool	running_fwrd	=  ( quant > 0 );
	bool	at_end			=	running_fwrd && ( timeCurrent > ( timeTotal-END_EPS ) );
	bool	at_begin		=	!running_fwrd && ( timeCurrent < 0.f );
	
	if( !stop_at_end )
	{
		if( at_begin )
			timeCurrent+= timeTotal;
		if( at_end )
			timeCurrent -= ( timeTotal-END_EPS );
		VERIFY( timeCurrent>=0.f );
		return false;
	}
	if( !at_end && !at_begin )
					return false;

	if( at_end )
	{
		timeCurrent	= timeTotal-END_EPS;		// stop@end - time frozen at the end
		if( timeCurrent<0.f ) timeCurrent =0.f; 
	}
	else
		timeCurrent	= 0.f;

	VERIFY( timeCurrent>=0.f );
	return true;
}

IC bool CBlend::update_falloff( float dt )
{
	update_time( dt );
	
	//if(  dt<0.f || timeCurrent >= blendAccrue )
		blendAmount 		-= dt*blendFalloff*blendPower;

	bool ret			= blendAmount<=0;
	clamp				( blendAmount, 0.f, blendPower);
	return ret;
}

IC bool CBlend::update( float dt, PlayCallback _Callback )
{
	switch (blend) 
	{
		case eFREE_SLOT: 
			NODEFAULT;
		case eAccrue:
			update_play( dt, _Callback );
			break;
		case eFalloff:
			if( update_falloff( dt ) )
				return true;
			break;
		default: 
			NODEFAULT;
	}
	return false;
}

class IBlendDestroyCallback
{
	public:
		virtual void BlendDestroy( CBlend& blend )	= 0;
};