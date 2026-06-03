#include "stdafx.h"
#include "MissileSam.h"
#include "../xrCore/_vector3d_ext.h"
#include "Level.h"
#include "../xrPhysics/PhysicsCommon.h"
#include "../xrPhysics/PhysicsShell.h"

void CMissileSam::UpdateEnginePh()
{
	//return;
	if (Level().In_NetCorrectionPrediction()) return;

	Fvector vel;
	m_pPhysicsShell->get_LinearVel(vel);
	vel.normalize_safe();
	/*{
		Msg("vel = [%f, %f, %f], mass = [%f]", vel.x, vel.y, vel.z, m_pPhysicsShell->getMass());
	}*/

	auto SelfTrans = m_pPhysicsShell->XFORM();
	Fvector dir;
	//dir.sub((target->Position() + Fvector{ 0.0f, 1.0f, 0.0f }), Position()).normalize_safe();
	dir.sub((target->Position() + Fvector{0, 2, 0}), Position()).normalize_safe();
	Fvector DeltaDir = dir - vel;
	DeltaDir.normalize_safe();
	if(DeltaDir.square_magnitude() < EPS)
	{
		DeltaDir = dir;
	}
	/*{
		Fmatrix m = m_pPhysicsShell->XFORM();
		Fvector startl = m.c;
		Fvector endl = m.c;
		endl.add(DeltaDir * 10);
		Level().debug_renderer().draw_line(m_pPhysicsShell->XFORM(), startl, endl, color_xrgb(0, 255, 0));
	}*/
	//Msg("DeltaDir = [%f, %f, %f], angle = [%f]", DeltaDir.x, DeltaDir.y, DeltaDir.z, dir.dotproduct(vel));
	SelfTrans.k.set(dir);
	//SelfTrans.k.set(dir);
	Fvector::generate_orthonormal_basis(SelfTrans.k, SelfTrans.j, SelfTrans.i);
	m_pPhysicsShell->SetTransform(SelfTrans, mh_not_clear);
	//m_pPhysicsShell->SetTransform(SelfTrans, mh_clear);

	float force = m_fEngineImpulse * fixed_step;// * Device.fTimeDelta;
	float k_back = 1.f;
	Fvector l_pos, l_dir;
	l_pos.set(0, 0, -2.f);
	//l_dir.set(DeltaDir);
	l_dir.set(XFORM().k);
	/*if(DeltaDir.square_magnitude() > 0){
		l_dir.set(DeltaDir.normalize());
	} else
	{
		l_dir.set(XFORM().k);
	}*/

	l_dir.normalize();

	R_ASSERT(m_pPhysicsShell);
	m_pPhysicsShell->applyImpulse(l_dir, (1.f + k_back) * force);
	m_pPhysicsShell->applyImpulse(DeltaDir, force);
	m_pPhysicsShell->get_LinearVel(l_dir);
	l_dir.normalize_safe();
	l_dir.invert();
	m_pPhysicsShell->applyImpulseTrace(l_pos, l_dir, force);
	//l_dir.set(0, 1.f, 0);
	//force = m_fEngineImpulseUp * fixed_step;// * Device.fTimeDelta;
	//m_pPhysicsShell->applyImpulse(l_dir, force);
}
