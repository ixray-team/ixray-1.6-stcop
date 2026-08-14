#include "stdafx.h"

#include "base_face.h"

#include "tcf.h"
#include "xrLC_GlobalData.h"
#include "utils/xrLC/Build.h"
#include "xrEngine/Shader_xrLC.h"

void TFace::VReplace(TVertex* what, TVertex* to)
{
	if (v[0]==what) 
	{
		v[0]=to;
		what->prep_remove(this); 
		to->prep_add(this);
	}

	if (v[1]==what) 
	{
		v[1]=to; 
		what->prep_remove(this);
		to->prep_add(this);
	}

	if (v[2]==what) 
	{
		v[2]=to;
		what->prep_remove(this); 
		to->prep_add(this);
	}
}

void TFace::VReplace_not_remove(TVertex* what, TVertex* to)
{
	if (v[0]==what) { v[0]=to; to->prep_add(this); }
	if (v[1]==what) { v[1]=to; to->prep_add(this); }
	if (v[2]==what) { v[2]=to; to->prep_add(this); }
}

void TFace::SetVertex(int idx, TVertex* V)
{
	v[idx]=V;
	V->prep_add(this);
}

void TFace::CalcNormal()
{
	Fvector t1,t2;

	Fvector* v0 = &(v[0]->P);
	Fvector* v1 = &(v[1]->P);
	Fvector* v2 = &(v[2]->P);
	t1.sub(*v1,*v0);
	t2.sub(*v2,*v1);
	this->N.crossproduct(t1,t2);
	float mag = this->N.magnitude();

	if (mag<EPS_S)
	{
		Fvector3 save_N	= this->N;
		if (exact_normalize(save_N)) {
			this->N = save_N;
		} else {
			CalcNormal2	();
		}
	} else {
		this->N.div		(mag);
		this->N.normalize	();
	}
}

void TFace::CalcNormal2()
{
	Dvector v0,v1,v2,t1,t2,dN;
	v0.set(v[0]->P);
	v1.set(v[1]->P);
	v2.set(v[2]->P);
	t1.sub(v1,v0);
	t2.sub(v2,v1);
	dN.crossproduct	(t1,t2);
	double mag = dN.magnitude	();
	if (mag<dbl_zero)
	{
		Failure();
		Dvector Nabs;
		Nabs.abs	(dN);

#define SIGN(a) ((a>=0.f)?1.f:-1.f)
		if (Nabs.x>Nabs.y && Nabs.x>Nabs.z)			this->N.set(SIGN(this->N.x),0.f,0.f);
		else if (Nabs.y>Nabs.x && Nabs.y>Nabs.z)	this->N.set(0.f,SIGN(this->N.y),0.f);
		else if (Nabs.z>Nabs.x && Nabs.z>Nabs.y)	this->N.set(0.f,0.f,SIGN(this->N.z));
		else {
			this->N.set	(0,1,0);
		}
#undef SIGN
	} else {
		dN.div	(mag);
		this->N.set	(dN);
	}
}

float TFace::CalcArea() const
{
	auto e1 = Fvector().sub(v[0]->P, v[1]->P);
	auto e2 = Fvector().sub(v[0]->P, v[2]->P);
	float area = Fvector().crossproduct(e1, e2).magnitude() / 2;
	return area;
}

void TFace::CalcCenter(Fvector& C)
{
	C.set(v[0]->P);
	C.add(v[1]->P);
	C.add(v[2]->P);
	C.div(3);
}

Fvector2* TFace::getTC0()
{
	return tc[0].uv;
}

base_Face::base_Face()
{
	basis_tangent[0].set	(0,0,0);
	basis_tangent[1].set	(0,0,0);
	basis_tangent[2].set	(0,0,0);
	basis_binormal[0].set	(0,0,0);
	basis_binormal[1].set	(0,0,0);
	basis_binormal[2].set	(0,0,0);
}

bool TFace::RenderEqualTo(TFace* F) const
{
	if (F->dwMaterial != dwMaterial || F->flags.bSharedMaterial != flags.bSharedMaterial)
	{
		return false;
	}
	return true;
}

void TFace::AddChannel(Fvector2& p1, Fvector2& p2, Fvector2& p3)
{
	_TCF TC;
	TC.uv[0] = p1;
	TC.uv[1] = p2;
	TC.uv[2] = p3;
	tc.push_back(TC);
}

bool TFace::hasImplicitLighting() const
{
	if (!Shader().flags.bRendering)
	{
		return false;
	}
	VERIFY( inlc_global_data() );
	auto& T = CBuild::GetTexture(dwMaterial, flags.bSharedMaterial);
	return (T.THM.flags.test(STextureParams::flImplicitLighted));
};