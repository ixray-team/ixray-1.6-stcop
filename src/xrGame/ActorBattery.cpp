#include "stdafx.h"
#include "ActorBattery.h"

CBattery::CBattery() : m_fTorchCost{}
{
	m_flags.set(FUsingCondition, TRUE);
}

void CBattery::Load(LPCSTR section)
{
	inherited::Load(section);

	m_fTorchCost = pSettings->r_float(section, "torch_cost");
	m_flags.set(FUsingCondition, READ_IF_EXISTS(pSettings, r_bool, section, "use_condition", TRUE));
}

bool CBattery::TryMakeTorchWork()
{
	float condition = GetCondition() - m_fTorchCost;
	if (condition < EPS_L)
		return false;

	SetCondition(condition);
	return true;
}

bool CBattery::IsEnought4Torch()
{
	return (GetCondition() - m_fTorchCost) >= EPS_L;
}

void CBattery::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);
	P.w_float_q8(GetCondition(), 0.0f, 1.0f);
}

void CBattery::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
	float _cond{};
	P.r_float_q8(_cond, 0.0f, 1.0f);
	SetCondition(_cond);
}