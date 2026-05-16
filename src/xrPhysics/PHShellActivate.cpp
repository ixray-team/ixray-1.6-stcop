#include "StdAfx.h"
#include "PHDynamicData.h"
#include "Physics.h"
#include "tri-colliderknoopc/dTriList.h"
#include "PHShellSplitter.h"
#include "PHFracture.h"
#include "PHJointDestroyInfo.h"
#include "PHCollideValidator.h"

#include "IPhysicsShellHolder.h"
#include "PhysicsShellAnimator.h"
#include "../Include/xrRender/Kinematics.h"

#include "ExtendedGeom.h"

#include "PHElement.h"
#include "PHShell.h"

void CPHShell::activate(bool disable)
{
	PresetActive();
	if(!CPHObject::is_active()) vis_update_deactivate();
	if(!disable)EnableObject(0);
}

void CPHShell::Activate(const Fmatrix& m0, float dt01, const Fmatrix& m2, bool disable)
{
	if (isActive())return;
	activate(disable);

	mXFORM.set(m0);

	{
		ELEMENT_I i = elements.begin(), e = elements.end();
		for (; i != e; ++i)(*i)->Activate(mXFORM, disable);
	}
	{
		JOINT_I i = joints.begin(), e = joints.end();
		for (; i != e; ++i) (*i)->Activate();
	}

	Fmatrix m;
	{
		Fmatrix old_m = mXFORM;//+GetGlobalTransformDynamic update mXFORM;
		GetGlobalTransformDynamic(&m);
		mXFORM = old_m;
	}

	m.invert(); m.mulA_43(mXFORM);
	TransformPosition(m, mh_unspecified);
	if (PKinematics())
	{
		SetCallbacks();
	}

	m_flags.set(flActive, true);
	m_flags.set(flActivating, true);
	spatial_register();

	Fvector lin_vel;
	lin_vel.sub(m2.c, m0.c);
	set_LinearVel(lin_vel);
}

void CPHShell::Activate(const Fmatrix& transform, const Fvector& lin_vel, const Fvector& ang_vel, bool disable) {

	if (isActive())return;
	activate(disable);

	ELEMENT_I i;
	mXFORM.set(transform);
	for (i = elements.begin(); elements.end() != i; ++i)
	{
		(*i)->Activate(transform, lin_vel, ang_vel);
	}

	{
		JOINT_I i_ = joints.begin(), e = joints.end();
		for (; i_ != e; ++i_) (*i_)->Activate();
	}

	if (PKinematics())
	{
		SetCallbacks();
	}
	spatial_register();

	m_flags.set(flActivating, true);
	m_flags.set(flActive, true);
}

void CPHShell::Activate(bool disable, bool not_set_bone_callbacks /*= false*/)
{
	if (isActive())return;

	activate(disable);
	{
		IKinematics* K = m_pKinematics;
		if (not_set_bone_callbacks)
			m_pKinematics = 0;
		ELEMENT_I i = elements.begin(), e = elements.end();
		for (; i != e; ++i)(*i)->Activate(mXFORM, disable);
		m_pKinematics = K;
	}

	{
		JOINT_I i = joints.begin(), e = joints.end();
		for (; i != e; ++i) (*i)->Activate();
	}

	if (PKinematics() && !not_set_bone_callbacks)
	{
		SetCallbacks();
	}

	spatial_register();
	m_flags.set(flActivating, true);
	m_flags.set(flActive, true);

}

void CPHShell::Build(bool disable)
{
	if (isActive())return;

	PresetActive();
	m_flags.set(flActivating, true);
	m_flags.set(flActive, true);

	{
		ELEMENT_I i = elements.begin(), e = elements.end();
		for (; i != e; ++i)
		{
			(*i)->build(disable);
		}
	}

	{
		JOINT_I i = joints.begin(), e = joints.end();
		for (; i != e; ++i) (*i)->Create();
	}

}

void CPHShell::RunSimulation(bool place_current_forms/*true*/)
{
	if (!CPHObject::is_active())
	{
		vis_update_deactivate();
	}

	EnableObject(0);
	dSpaceSetCleanup(m_space,0);

	{		
		ELEMENT_I i=elements.begin(),e=elements.end();
		if(place_current_forms) for(;i!=e;++i)(*i)->RunSimulation(mXFORM);
	}
	{
		JOINT_I i=joints.begin(),e=joints.end();
		for(;i!=e;++i) (*i)->RunSimulation();
	}	

	spatial_register();
}

void CPHShell::AfterSetActive()
{
	if(isActive())	return;
	PureActivate();

	m_flags.set(flActive,true);
	ELEMENT_I i=elements.begin(),e=elements.end();
	for(;i!=e;++i)(*i)->PresetActive();
}

void CPHShell::PureActivate()
{
	if(isActive())	return;

	m_flags.set(flActive,true);
	if(!CPHObject::is_active()) vis_update_deactivate();
	EnableObject(0);
	m_object_in_root.identity();
	spatial_register();
}

void CPHShell::PresetActive()
{
	VERIFY(!isActive());
	if(!m_space) 
	{
		m_space=dSimpleSpaceCreate(0);
		dSpaceSetCleanup(m_space,0);

	}
}

void CPHShell::Deactivate()
{
	VERIFY(ph_world);
	ph_world->NetRelcase(this);
	
	if (m_pPhysicsShellAnimatorC)
	{
		VERIFY( PhysicsRefObject( ) );
		PhysicsRefObject( )->ObjectProcessingDeactivate();
		xr_delete<CPhysicsShellAnimator>(m_pPhysicsShellAnimatorC); 
	}

	if(!isActive())return;
	R_ASSERT2(!ph_world->Processing(),"can not deactivate physics shell during physics processing!!!");
	R_ASSERT2(!ph_world->IsFreezed(),"can not deactivate physics shell when ph world is freezed!!!");
	R_ASSERT2(!CPHObject::IsFreezed(),"can not deactivate freezed !!!");
	ZeroCallbacks();
	VERIFY(ph_world&&ph_world->Exist());
	if(isFullActive())
	{
		vis_update_deactivate();
		CPHObject::activate();
		ph_world->Freeze();
		CPHObject::UnFreeze();
		ph_world->StepTouch();
		ph_world->UnFreeze();
	}
	spatial_unregister();
	
	vis_update_activate();
	DisableObject();
	CPHObject::remove_from_recently_deactivated();

	ELEMENT_I i;
	for (i = elements.begin(); elements.end() != i; ++i)
	{
		(*i)->Deactivate();
	}

	JOINT_I j;
	for (j = joints.begin(); joints.end() != j; ++j)
	{
		(*j)->Deactivate();
	}

	if(m_space)
	{
		dSpaceDestroy(m_space);
		m_space=nullptr;
	}

	m_flags.set(flActivating,false);
	m_flags.set(flActive,false);
	m_traced_geoms.clear();
	CPHObject::UnsetRayMotions();
}

#include "PHElementInline.h"

void CPHShell::ActivatingBonePoses(IKinematics& K)
{
	ELEMENT_I i, e;
	i = elements.begin(); e = elements.end();
	for (; i != e; ++i)
	{
		(*i)->ActivatingPos(K.LL_GetTransform((*i)->m_SelfID));
	}
}