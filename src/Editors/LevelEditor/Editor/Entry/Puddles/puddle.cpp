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
	const float PSOBJECT_SIZE = Box.getradius();

	float d = ray2.dotproduct(direction);
	if (d > 0) {
		float d2 = ray2.magnitude();
		if (((d2 * d2 - d * d) < (PSOBJECT_SIZE * PSOBJECT_SIZE)) && (d > PSOBJECT_SIZE)) {
			if (d < distance) {
				distance = d;
				return true;
			}
		}
	}
	return false;
}

bool CPuddle::FrustumPick(const CFrustum& frustum)
{
	return false;
}

void CPuddle::Render(int priority, bool strictB2F)
{
	constexpr u32 SelColor = color_rgba(10, 10, 255, 0);
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