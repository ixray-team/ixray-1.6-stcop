#include "StdAfx.h"
#include "animation_movement_controller.h"

#include "../Include/xrRender/Kinematics.h"
#include "game_object_space.h"
#include "../xrPhysics/matrix_utils.h"
#ifdef	 DEBUG
#include "PHDebug.h"
#endif

void	DBG_DrawBones( const Fmatrix &xform,  IKinematics *K );
#ifdef	 DEBUG
BOOL	dbg_draw_animation_movement_controller  = FALSE;
u16		dbg_frame_count = 0;
#endif

animation_movement_controller::animation_movement_controller(Fmatrix* _pObjXForm, const Fmatrix& inital_pose, IKinematics* _pKinematicsC, CBlend* b) :
	m_startObjXForm(inital_pose),
	m_pObjXForm(*_pObjXForm),
	m_pKinematicsC(_pKinematicsC),
	m_pKinematicsA(smart_cast<IKinematicsAnimated*>(_pKinematicsC)),
	inital_position_blending(true),
	stopped(false),
	blend_linear_speed(0),
	blend_angular_speed(0),
	m_control_blend(b),
	m_poses_blending(Fidentity, Fidentity, -1.f)
#ifdef	DEBUG
	, DBG_previous_position(*_pObjXForm)
#endif
{
	VERIFY(_pKinematicsC);
	VERIFY(m_pKinematicsA);
	VERIFY(_pObjXForm);
	VERIFY(b);

#ifdef	DEBUG
	if (dbg_draw_animation_movement_controller)
	{
		m_pKinematicsC->CalculateBones_Invalidate();
		m_pKinematicsC->CalculateBones(TRUE);
		DBG_OpenCashedDraw();
		DBG_DrawBones(*_pObjXForm, _pKinematicsC);
		DBG_ClosedCashedDraw(50000);
	}
#endif	
	CBoneInstance& B = m_pKinematicsC->LL_GetBoneInstance(m_pKinematicsC->LL_GetBoneRoot());
	VERIFY(!B.callback() && !B.callback_param());
	B.set_callback(bctCustom, RootBoneCallback, this, TRUE);
	B.mTransform = Fidentity;
	GetInitalPositionBlenSpeed();

	m_pKinematicsA->SetBlendDestroyCallback(this);
	m_pKinematicsC->CalculateBones_Invalidate();
	m_pKinematicsC->CalculateBones(TRUE);
	SetPosesBlending();
#ifdef	DEBUG
	if (dbg_draw_animation_movement_controller)
	{
		DBG_OpenCashedDraw();
		DBG_DrawMatrix(*_pObjXForm, 3, 100);
		DBG_DrawBones(*_pObjXForm, _pKinematicsC);
		DBG_ClosedCashedDraw(50000);
	}
#endif	
}

animation_movement_controller::~animation_movement_controller()
{
	if (IsActive())
		deinitialize();
}

IC bool is_blending_in(CBlend& b)
{
	return b.blend_state() == CBlend::eAccrue && b.blendPower - EPS > b.blendAmount;
}

void animation_movement_controller::deinitialize()
{
#ifdef	DEBUG
	if (dbg_draw_animation_movement_controller)
	{
		DBG_OpenCashedDraw();
		DBG_DrawMatrix(m_pObjXForm, 3, 100);
		DBG_DrawBones(m_pObjXForm, m_pKinematicsC);
		DBG_ClosedCashedDraw(50000);
	}
#endif


	CBoneInstance& B = m_pKinematicsC->LL_GetBoneInstance(m_pKinematicsC->LL_GetBoneRoot());
	VERIFY(B.callback() == RootBoneCallback);
	VERIFY(B.callback_param() == (void*)this);
	B.reset_callback();
	m_pKinematicsA->SetBlendDestroyCallback(0);
	m_control_blend = 0;

#ifdef	DEBUG
	if (dbg_draw_animation_movement_controller)
	{
		DBG_OpenCashedDraw();
		DBG_DrawMatrix(m_pObjXForm, 3, 100);
		DBG_DrawBones(m_pObjXForm, m_pKinematicsC);
		DBG_ClosedCashedDraw(50000);
	}
#endif
}

void animation_movement_controller::GetInitalPositionBlenSpeed()
{
	float sv_blend_time = m_control_blend->timeCurrent;

	Fmatrix m1;
	animation_root_position(m1);
	m_control_blend->timeCurrent += Device.fTimeDelta;
	clamp(m_control_blend->timeCurrent, 0.f, m_control_blend->timeTotal);

	Fmatrix m0;
	animation_root_position(m0);
	float l, a;
	get_diff_value(m0, m1, l, a);
	blend_linear_speed = l / Device.fTimeDelta;
	blend_angular_speed = a / Device.fTimeDelta;
	m_control_blend->timeCurrent = sv_blend_time;
}

bool animation_movement_controller::IsBlending() const
{
	return  is_blending_in(*m_control_blend);
}

void animation_movement_controller::InitalPositionBlending(const Fmatrix& to)
{
#ifdef	DEBUG
	if (dbg_draw_animation_movement_controller)
	{
		DBG_DrawMatrix(m_pObjXForm, 1);
	}

	if (dbg_draw_animation_movement_controller)
		DBG_DrawMatrix(to, 2);
#endif

	if (!m_poses_blending.target_reached(m_control_blend->timeCurrent))
		m_poses_blending.pose(m_pObjXForm, m_control_blend->timeCurrent);
	else
		m_pObjXForm.set(to);

#ifdef	DEBUG
	DBG_previous_position = m_pObjXForm;
#endif
}

static void get_animation_root_position( Fmatrix &pos, IKinematics* K, IKinematicsAnimated* KA, CBlend *control_blend )
{
	VERIFY( KA );
	VERIFY( K );
	VERIFY( smart_cast<IKinematics*>(KA) == K );

	SKeyTable	keys;
	KA->LL_BuldBoneMatrixDequatize( &K->LL_GetData( 0 ), u8(1<<0), keys );
	
//find
	CKey *key = 0;
	for( int i = 0; i < keys.chanel_blend_conts[0]; ++i )
	{
		if ( keys.blends[0][i] == control_blend )
			key = &keys.keys[0][i];
	}
	VERIFY( key );

	if (key == nullptr)
		return;

	float sv_amount = control_blend->blendAmount;
	control_blend->blendAmount = 1.f;
	keys.blends[0][0] = control_blend;
	keys.chanel_blend_conts[0] = 1;
	keys.keys[0][0] = *key;


	for( int j = 1; j < MAX_CHANNELS; ++j )
		keys.chanel_blend_conts[j] = 0;

	CBoneInstance BI = K->LL_GetBoneInstance( 0 );

	KA->LL_BoneMatrixBuild( BI, &Fidentity, keys );
	pos.set( BI.mTransform );
	control_blend->blendAmount = sv_amount;
}
void animation_movement_controller::animation_root_position	( Fmatrix &pos  )
{
	get_animation_root_position( pos, m_pKinematicsC, m_pKinematicsA, m_control_blend );
}

void animation_movement_controller::OnFrame()
{
	VERIFY(IsActive());
	DBG_verify_position_not_chaged();

#ifdef	DEBUG
	if (dbg_draw_animation_movement_controller && dbg_frame_count < 3)
	{
		DBG_OpenCashedDraw();
		DBG_DrawBones(m_pObjXForm, m_pKinematicsC);
		DBG_ClosedCashedDraw(50000);
	}
#endif	

	Fmatrix root_pos;
	animation_root_position(root_pos);

	Fmatrix obj_pos = Fmatrix().mul_43(m_startObjXForm, root_pos);
	InitalPositionBlending(obj_pos);

#ifdef DEBUG
	DBG_previous_position = m_pObjXForm;
#endif

#ifdef	DEBUG	
	++dbg_frame_count;
#endif
}

void animation_movement_controller::NewBlend(CBlend* B, const Fmatrix& new_matrix, bool local_animation)
{
	VERIFY(IsActive());
#ifdef DEBUG
	DBG_previous_position = m_pObjXForm;
#endif

	bool set_blending = !m_poses_blending.target_reached(m_control_blend->timeCurrent);

	if (stopped)
	{
		m_control_blend = B;
		m_startObjXForm.set(new_matrix);
		GetInitalPositionBlenSpeed();
		inital_position_blending = true;
		set_blending = true;
		stopped = false;
	}
	else if (local_animation)
	{
		float blend_time = m_control_blend->timeCurrent;
		m_control_blend->timeCurrent = m_control_blend->timeTotal - SAMPLE_SPF;//(SAMPLE_SPF+EPS);
		Fmatrix root;
		animation_root_position(root);
		m_startObjXForm.mulB_43(root);
#ifdef	DEBUG
		if (dbg_draw_animation_movement_controller)
		{
			DBG_OpenCashedDraw();
			DBG_DrawMatrix(m_startObjXForm, 1);
			DBG_ClosedCashedDraw(5000);
		}
#endif
		m_control_blend->timeCurrent = blend_time;
	}

	m_control_blend = B;
	if (set_blending)
		SetPosesBlending();
	else
		m_poses_blending = poses_blending(Fidentity, Fidentity, -1.f);
}

void animation_movement_controller::DBG_verify_position_not_chaged() const
{
#ifdef	DEBUG
	VERIFY( !IsActive()||inital_position_blending || cmp_matrix( DBG_previous_position, m_pObjXForm, EPS, EPS ) );
#endif
}

void animation_movement_controller::RootBoneCallback(CBoneInstance* B)
{
	VERIFY(B);
	VERIFY(B->callback_param());

	animation_movement_controller* O = (animation_movement_controller*)(B->callback_param());

	O->DBG_verify_position_not_chaged();

	B->mTransform.set(Fidentity);

	R_ASSERT2(_valid(B->mTransform), "animation_movement_controller::RootBoneCallback");
}

bool animation_movement_controller::IsActive() const
{
	return !!m_control_blend ;
}

void animation_movement_controller::BlendDestroy(CBlend& blend)
{
	VERIFY(m_control_blend);
	//Msg("deinit");
	if (m_control_blend == &blend)
		deinitialize();
}

void animation_movement_controller::stop				( )
{
	stopped			= true;
}

constexpr float percent_blending = 0.2f;
void animation_movement_controller::SetPosesBlending()
{
	VERIFY(IsActive());
	float blending_time = percent_blending * m_control_blend->timeTotal;

	float sv_time = m_control_blend->timeCurrent;
	m_control_blend->timeCurrent = blending_time;

	Fmatrix root;
	animation_root_position(root);

	poses_blending blending(m_pObjXForm, Fmatrix().mul_43(m_startObjXForm, root), blending_time);
	m_poses_blending = blending;
	m_control_blend->timeCurrent = sv_time;
}