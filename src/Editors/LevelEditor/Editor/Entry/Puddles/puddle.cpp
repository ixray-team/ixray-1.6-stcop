#include "stdafx.h"
#include "puddle.h"

CPuddle::CPuddle(LPVOID data, LPCSTR name):
	CCustomObject(data,name)
{
	Construct(data);
	FScale.set(1, 0.3f, 1);
}

void CPuddle::Construct(LPVOID data)
{
	FClassID = OBJCLASS_PUDDLES;
}

CPuddle::~CPuddle()
{
}

bool CPuddle::RayPick(float& distance, const Fvector& start, const Fvector& direction, SRayPickInfo* pinf)
{
	Fvector pos, ray2;
	pos.set(GetPosition());
	ray2.sub(pos, start);

	Fbox Box;
	Box.set(FPosition, FScale);
	Box.getcenter(pos);

	Fobb obb;
	obb.invalidate();
	obb.m_translate = FPosition;
	obb.m_halfsize = FScale;
	//obb.m_halfsize.mul(0.5f);

	return CDB::TestRayOBB(start, direction, obb);

	//Fvector ada;
	//ada.mad(start, direction, sqrt(pos.sub(start).dotproduct(direction)));
	//return Box.contains(ada);
}

bool CPuddle::FrustumPick(const CFrustum& frustum)
{
	return false;
}

void CPuddle::Render(int priority, bool strictB2F)
{
	constexpr u32 SelColor = color_rgba(10, 10, 255, 122);
	DU_impl.DrawBox(this->FPosition, this->FScale, Selected(), true, SelColor, Selected() ? 0xfff : 0xddd);
}

bool CPuddle::LoadLTX(CInifile& ini, LPCSTR sect_name)
{
	return CCustomObject::LoadLTX(ini, sect_name);
}

void CPuddle::SaveLTX(CInifile& ini, LPCSTR sect_name)
{
	CCustomObject::SaveLTX	(ini, sect_name);
}

bool CPuddle::LoadStream(IReader& F)
{
	return CCustomObject::LoadStream(F);
}

void CPuddle::SaveStream(IWriter& F)
{
	CCustomObject::SaveStream(F);
}