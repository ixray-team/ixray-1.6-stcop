#include "stdafx.h"
#include "PHInterpolation.h"
#include "PHDynamicData.h"
#include "Physics.h"
#include "MathUtils.h"
#include "ph_valid_ode.h"
#include "../xrEngine/device.h"

extern CPHWorld *ph_world;

CPHInterpolation::CPHInterpolation():
	m_body(nullptr)
{
}

void CPHInterpolation::SetBody(dBodyID body)
{
	if (!body)
		return;

	m_body = body;
	qPositions.fill_in(*((Fvector*)dBodyGetPosition(m_body)));
	const dReal* dQ = dBodyGetQuaternion(m_body);

	Fquaternion fQ;
	fQ.set(-dQ[0], dQ[1], dQ[2], dQ[3]);
	qRotations.fill_in(fQ);
}

void CPHInterpolation::UpdatePositions()
{
	VERIFY(m_body);
	qPositions.push_back(*((Fvector*) dBodyGetPosition(m_body)));
}

void CPHInterpolation::UpdateRotations()
{
	VERIFY(m_body);
	const dReal* dQ=dBodyGetQuaternion(m_body);
	Fquaternion fQ;
	fQ.set(-dQ[0],dQ[1],dQ[2],dQ[3]);
	qRotations.push_back(fQ);
}

void CPHInterpolation::InterpolatePosition(Fvector& pos)
{
	pos.lerp(qPositions[0], qPositions[1], ph_world->m_frame_time / fixed_step);
}

void CPHInterpolation::InterpolateRotation(Fmatrix& rot) 
{
	Fquaternion q;
	float	t = ph_world->m_frame_time / fixed_step;
	VERIFY(t >= 0.f && t <= 1.f);
	q.slerp(qRotations[0], qRotations[1], t);
	rot.rotation(q);
}

void CPHInterpolation::ResetPositions()
{
#ifdef DEBUG
	if (!Device.IsEditorMode())
	{
		VERIFY2(dV_valid(dBodyGetPosition(m_body)), "Invalid body state: Position");
	}
#endif
	qPositions.fill_in(*((Fvector*) dBodyGetPosition(m_body)));
}

void CPHInterpolation::ResetRotations()
{
	//VERIFY2(dV_valid(dBodyGetQuaternion(m_body)),"Invalid body state: Quaternion");
	const dReal* dQ=dBodyGetQuaternion(m_body);
	Fquaternion fQ;
	fQ.set(-dQ[0],dQ[1],dQ[2],dQ[3]);
	qRotations.fill_in(fQ);
}

void CPHInterpolation::GetRotation(Fquaternion& q, u16 num)
{
	if(!m_body)
		return;

	q.set(qRotations[num]);
}

void CPHInterpolation::GetPosition(Fvector& p,u16 num)
{
	if(!m_body)
		return;

	p.set(qPositions[num]);
}
void CPHInterpolation::SetPosition(const Fvector& p, u16 num)
{
	if(!m_body)
		return;

	qPositions[num].set(p);
}

void CPHInterpolation::SetRotation(const Fquaternion& q, u16 num)
{
	if(!m_body)
		return;

	qRotations[num]=q;
}