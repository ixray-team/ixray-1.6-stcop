#include "stdafx.h"
#include "FlameCanister.h"
#include "Flamethrower.h"
#include "Inventory.h"
#include "Level.h"

CFlameCanister::CFlameCanister(void)
{
	m_flags.set(FUsingCondition, TRUE);
}

CFlameCanister::~CFlameCanister(void)
{
}

void CFlameCanister::Load(LPCSTR section)
{
	inherited::Load(section);

	cartridge_param.kHit = pSettings->r_float(section, "k_hit");

}

bool CFlameCanister::net_Spawn(CSE_Abstract* DC)
{
	bool bResult = inherited::net_Spawn(DC);
	/*CSE_Abstract* e = (CSE_Abstract*)(DC);
	CSE_ALifeInventoryItem* l_pW = smart_cast<CSE_ALifeInventoryItem*>(e);
	m_boxCurr = l_pW->a_elapsed;

	if (m_boxCurr > m_boxSize)
		l_pW->a_elapsed = m_boxCurr = m_boxSize;*/

	return					bResult;
}

void CFlameCanister::net_Destroy()
{
	inherited::net_Destroy();
}

void CFlameCanister::OnH_B_Chield()
{
	inherited::OnH_B_Chield();
}

void CFlameCanister::OnH_B_Independent(bool just_before_destroy)
{
	if (!Useful()) {

		if (Local()) {
			DestroyObject();
		}
		m_ready_to_destroy = true;
	}
	inherited::OnH_B_Independent(just_before_destroy);
}


bool CFlameCanister::Useful() const
{
	// ���� IItem ��� �� ��������� �������������, ������� true
	return !!GetCondition();
}

void CFlameCanister::renderable_Render()
{
	if (!m_ready_to_destroy)
		inherited::renderable_Render();
}

void CFlameCanister::UpdateCL()
{
	VERIFY2(_valid(renderable.xform), *cName());
	inherited::UpdateCL();
	VERIFY2(_valid(renderable.xform), *cName());

	if (!IsGameTypeSingle())
		make_Interpolation();

	VERIFY2(_valid(renderable.xform), *cName());

}

void CFlameCanister::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);
}

void CFlameCanister::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
}

CInventoryItem* CFlameCanister::can_make_killing(const CInventory* inventory) const
{
	VERIFY(inventory);

	TIItemContainer::const_iterator	I = inventory->m_all.begin();
	TIItemContainer::const_iterator	E = inventory->m_all.end();
	for (; I != E; ++I) {
		CWeapon* weapon = smart_cast<CWeapon*>(*I);
		if (!weapon)
			continue;
		xr_vector<shared_str>::const_iterator	i = std::find(weapon->m_ammoTypes.begin(), weapon->m_ammoTypes.end(), cNameSect());
		if (i != weapon->m_ammoTypes.end())
			return			(weapon);
	}

	return					(nullptr);
}

float CFlameCanister::Weight() const
{
	float res = inherited::Weight();
	res *= (0.05f + 0.95f * GetCondition());
	return res;
}

u32 CFlameCanister::Cost() const
{
	u32 res = inherited::Cost();

	res = iFloor(res * GetCondition() + 0.5f);

	return res;
}

bool CFlameCanister::Repack(PIItem Other)
{
	CFlameCanister* OtherCasted = smart_cast<CFlameCanister*>(Other);
	VERIFY(OtherCasted);
	if (OtherCasted->GetCondition() == 1.0f)
	{
		return true;
	}
	float Sum = OtherCasted->GetCondition() + GetCondition();
	if (Sum > 1.0f)
	{
		SetCondition(Sum - OtherCasted->GetCondition());
		OtherCasted->SetCondition(1.0f);
		return true;
	}
	OtherCasted->SetCondition(Sum);
	if (OnServer()) {
		SetDropManual(TRUE);
	}
	return false;
}

bool CFlameCanister::IsValid() const
{
	return GetCondition();
}
