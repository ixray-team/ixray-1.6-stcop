#include "stdafx.h"

#include "xrFace.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "Lightmap.h"
#include "utils/xrLC/Build.h"
#include "../../xrCore/xrPool.h"

volatile u32					dwInvalidFaces;//= 0;
u32		InvalideFaces()
{
	return dwInvalidFaces;
}

const Shader_xrLC&	TFace::Shader		()const
{
	const auto data = inlc_global_data();
	VERIFY(data);
	if (flags.bSharedMaterial)
	{
		auto& Arr = data->materials_shared();
		VERIFY( dwMaterial < Arr.size());
		return data->shaders().Get(Arr[dwMaterial].reserved);
	}
	auto& Arr = data->materials();
	VERIFY( dwMaterial < Arr.size());
	return data->shaders().Get(Arr[dwMaterial].reserved);
}

void TFace::CacheOpacity()
{
	flags.bOpaque = true;
	VERIFY(inlc_global_data());

	b_BuildTexture& T = CBuild::GetTexture(dwMaterial, flags.bSharedMaterial);
	flags.bOpaque = !T.bHasAlpha;

	// pSurface was possible deleted
	if (!flags.bOpaque && !T.HasSurface())
	{
		flags.bOpaque = true;
		clMsg("Strange face detected... Has alpha without texture... [%s]", T.name);
	}
}

bool g_bUnregister = true;

static void destroy_vertex( TVertex* &v, bool unregister )
{
	bool tmp_unregister = g_bUnregister;
	g_bUnregister = unregister;
	inlc_global_data()->destroy_vertex( v );
	g_bUnregister = tmp_unregister;
}

void destroy_face( TFace* &v, bool unregister )
{
	bool tmp_unregister = g_bUnregister;
	g_bUnregister = unregister;
	inlc_global_data()->destroy_face( v );
	g_bUnregister = tmp_unregister;
}
static poolSS<TVertex, 8 * 1024> mu_vertices;
static poolSS<TFace,   8 * 1024> mu_faces;

poolSS<TVertex,8*1024> &mu_vertices_pool()
{
	return mu_vertices;
}
poolSS<TFace,8*1024> &mu_faces_pool()
{
	return mu_faces;
}

void mu_mesh_clear()
{
	mu_vertices.clear();
	mu_faces.clear();
}

static void destroy_vertex_mu( TVertex* &v, bool unregister )
{
	mu_vertices_pool().destroy(v);
	v = nullptr;
}

TVertex*	TVertex::CreateCopy_NOADJ( vecVertex& vertises_storage, bool IsMU ) const
{
	TVertex* V = nullptr;
	if (IsMU)
	{
		V = mu_vertices_pool().create();
	} else
	{
		R_ASSERT( &vertises_storage == &inlc_global_data()->g_vertices() );
		V	= inlc_global_data()->create_vertex();
	}
	V->P.set	(P);
	V->N.set	(N);
	V->C		= C;
	return		V;
}

void isolate_vertices(bool bProgress, xr_vector<TVertex*>& vertices, bool IsMU)
{
	const u32 verts_old		= (u32)vertices.size();
	u32 vRemoveReal = 0;

	for (auto it = 0; it < verts_old; it++)
	{
		if (vertices[it] && vertices[it]->m_adjacents.empty())
		{
			//_destroy_vertex<typeVertex, ForMU>(vertices[it], false);
			if (IsMU)
			{
				destroy_vertex_mu(vertices[it], false);
			} else
			{
				destroy_vertex(vertices[it], false);
			}
			vRemoveReal++;
		}
	}
	VERIFY( verts_old == vertices.size() );

	auto _end= std::ranges::remove(vertices,nullptr).begin();
	vertices.erase	(_end,vertices.end());
	vertices.shrink_to_fit();
}
 
void TFace::Failure() const
{
	dwInvalidFaces++;

	inlc_global_data()->err_invalid().w_fvector3(v[0]->P);
	inlc_global_data()->err_invalid().w_fvector3(v[1]->P);
	inlc_global_data()->err_invalid().w_fvector3(v[2]->P);
}

void TFace::Verify()
{
	// 1st :: area
	float _a = CalcArea();
	if(!_valid(_a) || (_a<EPS))
	{
		Failure(); 
		return;
	}

	// 2nd :: TC0
	Fvector2*	tc = getTC0();
	float eps = .5f / 4096.f;		// half pixel from 4096 texture (0.0001220703125)
	float e0 = tc[0].distance_to(tc[1]);	
	float e1 = tc[1].distance_to(tc[2]);
	float e2 = tc[2].distance_to(tc[0]);
	float p = e0+e1+e2;
	if (!_valid(_a) || (p<eps))
	{
		Failure(); 
		return;
	}

	// 3rd :: possibility to calc normal
	CalcNormal();
	if (!_valid(N))
	{
		Failure(); 
	}
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int affected = 0;
void start_unwarp_recursion()
{
	affected				= 1;
}

void TFace::OA_Unwarp( CDeflector *D, xr_vector<TFace*>& faces) const
{ 
	// range: no recursive method realisation
	xr_stack<TFace*> st;

	auto f = this;
	while (true)
	{
		for (int i = 0; i < 3; ++i)
		{
			for (auto it : f->v[i]->m_adjacents)
			{
				if (it->pDeflector) continue;
				
				if (!D->OA_Place(it)) continue;

				affected++;
				st.push(it);
				faces.push_back(it);
			}
		}

		if (!st.empty())
		{
			f = st.top();
			st.pop();
		}
		else break;
	}
}

/*bool DataFace::RenderEqualTo(Face *F)
{
	if (F->dwMaterial	!= dwMaterial		)	
		return false;
 	return true;
}

void DataFace::AddChannel(Fvector2 &p1, Fvector2 &p2, Fvector2 &p3) 
{
	_TCF	TC;
	TC.uv[0] = p1;	TC.uv[1] = p2;	TC.uv[2] = p3;
	tc.push_back(TC);
}

bool DataFace::hasImplicitLighting()
{
	if (!Shader().flags.bRendering)
	{
		return false;
	}
	VERIFY( inlc_global_data() );
	auto& T = pBuild->GetTexture(dwMaterial, flags.bSharedMaterial);
	return (T.THM.flags.test(STextureParams::flImplicitLighted));
}*/

void GetBarycentric(TFace* F, Fvector& wP, Fvector& wN, Fvector& B)
{
	auto V1 = F->v[0];
	auto V2 = F->v[1];
	auto V3 = F->v[2];
	wP.from_bary(V1->P, V2->P, V3->P, B);
	wN.from_bary(V1->N, V2->N, V3->N, B);
	wN.normalize();
};

void GetBarycentricNormalized(TFace* F, Fvector& wP, Fvector& wN, Fvector& B)
{
	auto V1 = F->v[0];
	auto V2 = F->v[1];
	auto V3 = F->v[2];
	wP.from_bary(V1->P, V2->P, V3->P, B);
	wN.from_bary(V1->N, V2->N, V3->N, B);   
	exact_normalize(wN);
	wN.add(F->N);							
	exact_normalize(wN);
}

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
}