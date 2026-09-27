#include "stdafx.h"
#include "ComputeMinBox.h"
#include "../WildMagic/WmlMath.h"
#include "../WildMagic/WmlContMinBox3.h"
#include "../WildMagic/WmlContBox3.h"

extern bool RAPIDMinBox(Fobb& B, Fvector* vertices, u32 v_count);

void ComputeOBB_RAPID(Fobb& B, FvectorVec& V, u32 t_cnt)
{
	VERIFY(t_cnt == (V.size() / 3));
	if ((t_cnt < 1) || (V.size() < 3))
	{
		B.invalidate();
		return;
	}
	RAPIDMinBox(B, &V.front(), V.size());

	B.m_rotate.i.crossproduct(B.m_rotate.j, B.m_rotate.k);
	B.m_rotate.j.crossproduct(B.m_rotate.k, B.m_rotate.i);

	VERIFY(_valid(B.m_rotate) && _valid(B.m_translate) && _valid(B.m_halfsize));
}

void ComputeOBB_WML(Fobb& B, FvectorVec& V)
{
	if (V.size() < 3)
	{
		B.invalidate();
		return;
	}
	float HV = flt_max;
	{
		Wml::Box3<float> BOX;
		Wml::MinBox3<float> mb(V.size(), (const Wml::Vector3<float>*)&V.front(), BOX);
		float hv = BOX.Extents()[0] * BOX.Extents()[1] * BOX.Extents()[2];
		if (hv < HV)
		{
			HV = hv;
			B.m_rotate.i.set(BOX.Axis(0));
			B.m_rotate.j.set(BOX.Axis(1));
			B.m_rotate.k.set(BOX.Axis(2));

			B.m_translate.set(BOX.Center());
			B.m_halfsize.set(BOX.Extents()[0], BOX.Extents()[1], BOX.Extents()[2]);
		}
	}

	B.m_rotate.i.crossproduct(B.m_rotate.j, B.m_rotate.k);
	B.m_rotate.j.crossproduct(B.m_rotate.k, B.m_rotate.i);

	VERIFY(_valid(B.m_rotate) && _valid(B.m_translate) && _valid(B.m_halfsize));
}
