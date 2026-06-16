#include "StdAfx.h"
#include "Build.h"
#include "OGF_Face.h"
#pragma warning(disable:4995)
#include "../../3rd-party/MagicSoftware/FreeMagic/Include/MgcCont3DMinSphere.h"

bool	f_valid		(float f)
{
	return _finite(f) && !_isnan(f);
}

bool	SphereValid	(xr_vector<Fvector>& geom, Fsphere& test)
{
	if (!f_valid(test.P.x) || !f_valid(test.R)) 
	{
		clMsg("*** Attention ***: invalid sphere: %f,%f,%f - %f", test.P.x, test.P.y, test.P.z, test.R);
		return false;
	}
	 
	Fsphere	S = test;
	S.R += EPS_L;
	for (xr_vector<Fvector>::iterator I = geom.begin(); I != geom.end(); I++)
	if (!S.contains(*I))	
		return false;
	return true;
}

Fsphere CalculateSphere(xr_vector<Fvector>& V, Fbox &bbox)
{
	Fsphere S2;
	bbox.invalidate();
	for (auto& I : V)
		bbox.modify(I);

	bbox.grow(EPS_L);
	bbox.getsphere(S2.P, S2.R);

	S2.R = -1;
	for (auto& I : V)
	{
		float d = S2.P.distance_to_sqr(I);
		if (d > S2.R)
			S2.R = d;
	}

	S2.R = _sqrt(std::abs(S2.R));
	return S2;
}

Fsphere CalculateMagic(xr_vector<Fvector>& V)
{
	Mgc::Sphere _S3 = Mgc::MinSphere( (u32) V.size(), (const Mgc::Vector3*)&*V.begin());

	Fsphere	S3;
	S3.P.set(_S3.Center().x, _S3.Center().y, _S3.Center().z);
	S3.R = _S3.Radius();
	return S3;
}
 
// Оптимизированная проверка валидности с ранним выходом
bool SphereValid_Fast(const xr_vector<Fvector>& points, const Fsphere& S)
{
	float max_dist_sqr = S.R * S.R + 0.001f; // небольшой запас на погрешность float

	for (const auto& pt : points)
	{
		if (pt.distance_to_sqr(S.P) > max_dist_sqr)
		{
			return false; // Нашли косяк? Мгновенно выходим! Не теряем время.
		}
	}
	return true;
}

void OGF_Base::CalcBounds(bool useProgressBar)
{
	thread_local xr_vector<Fvector> V;
	V.clear();
	V.reserve(4096);
	GetGeometry(V);

	if (V.empty()) return;
 
	// 1. Считаем базовую сферу по Bounding Box (S2 в вашем коде)
	// Она всегда валидна по определению, но часто избыточна.
	Fsphere S_base  = CalculateSphere(V, bbox);

	// 2. Считаем быструю "магическую" сферу Риттера (S3 в вашем коде)
	Fsphere S_magic = CalculateMagic(V);

	// 3. Проверяем магическую сферу быстрым методом
	if (SphereValid_Fast(V, S_magic) && (S_magic.R <= S_base.R))
	{
		// Если магия сработала и она компактнее коробки — это идеальный и быстрый случай
		C.set(S_magic.P);
		R = S_magic.R;
		return;
	}

	// 4. Тот самый тяжелый случай (кривая геометрия локации/заборы/рельсы)
	// Магия не покрыла все точки или раздулась больше BBox.
	// Вот теперь НАДО вызывать точный Miniball. Его валидировать НЕ нужно, он точен по определению.
	Fsphere S_mini;
	Fsphere_compute(S_mini, V.data(), (u32)V.size());

	// Выбираем лучшее между Miniball и Base (на случай редких багов float в Miniball)
	if (S_mini.R < S_base.R)
	{
		C.set(S_mini.P);
		R = S_mini.R;
	}
	else
	{
		C.set(S_base.P);
		R = S_base.R;
	}
}