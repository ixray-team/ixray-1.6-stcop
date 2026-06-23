#include "ExtendedGeom.h"
#include "MathUtils.h"
//#include "Level.h"
#include "Geometry.h"
#include "tri-colliderknoopc/dTriColliderMath.h"
//#include "../xrengine/IGame_Level.h"
#include "ode_redefine.h"
#include "../xrCore/Collision/xr_area.h"
//#include "phworld.h"
#pragma warning(disable:4995)
#pragma warning(disable:4267)
/*ICF void GetNormal(CDB::TRI*XTri,Fvector &n, const Fvector* V_array )
{
	//VERIFY(g_pGameLevel);
	//const Fvector* V_array=inl_ph_world().ObjectSpace().GetStaticVerts();
	Fvector sd1;sd1.sub(V_array[XTri->verts[1]],V_array[XTri->verts[0]]);
	Fvector sd2;sd2.sub(V_array[XTri->verts[2]],V_array[XTri->verts[1]]);
	n.crossproduct(sd1,sd2);
}*/
ICF	void CalculateInitTriangle(const TriabgleCDBData& XTri,Triangle& triangle)
{
	//VERIFY(g_pGameLevel);
	//const Fvector* V_array=inl_ph_world().ObjectSpace().GetStaticVerts();
	Fvector VRT[3];
	XTri.GetVerts(VRT);
	dVectorSub(triangle.side0,(dReal*)&VRT[1],(dReal*)&VRT[0])						;
	dVectorSub(triangle.side1,(dReal*)&VRT[2],(dReal*)&VRT[1])						;
	triangle.T=XTri													;
	dCROSS(triangle.norm,=,triangle.side0,triangle.side1)			;
	cast_fv(triangle.norm).normalize()								;
	triangle.pos=dDOT((dReal*)&VRT[0],triangle.norm)							;
}
ICF void CalculateTriangle(const TriabgleCDBData& XTri,const float* pos,Triangle& triangle)
{
	CalculateInitTriangle(XTri,triangle);
	triangle.dist=dDOT(pos,triangle.norm)-triangle.pos;
}
ICF void CalculateTriangle(const TriabgleCDBData& XTri, dGeomID g, Triangle& triangle)
{
	dVector3	v												;
	dMatrix3	m												;
	const float *p						= nullptr;
	const float *r						= nullptr;
	VERIFY								( g )					;
	CODEGeom::get_final_tx				( g, p, r, v, m )		;
	VERIFY								( p )					;
	CalculateTriangle					( XTri, p, triangle)	;
	
}

inline bool  TriContainPoint(const dReal* v0,const dReal* v1,const dReal* v2,
							 const dReal* triSideAx0,const dReal* triSideAx1,const dReal* triSideAx2,
							 const dReal* triAx, const dReal* pos,u16 &c){
								 c=0;
								 dVector3 cross0, cross1, cross2;
								 dCROSS(cross0,=,triAx,triSideAx0);
								 if(dDOT(cross0,pos)<dDOT(cross0,v0))
								 {
									 c=1;
									 return false;
								 }
								 dCROSS(cross1,=,triAx,triSideAx1);
								 if(dDOT(cross1,pos)<dDOT(cross1,v1))
								 {
									 c=2;
									 return false;
								 }
								 dCROSS(cross2,=,triAx,triSideAx2);
								 if(dDOT(cross2,pos)<dDOT(cross2,v2))
								 {
									 c=3;
									 return false;
								 }
								 return true;
							 }

ICF bool  TriContainPoint(const dReal* v0,const dReal* v1,const dReal* v2,const dReal* triAx,const dReal* triSideAx0,const dReal* triSideAx1, const dReal* pos,u16& c){


	 dVector3 triSideAx2={v0[0]-v2[0],v0[1]-v2[1],v0[2]-v2[2]};
	 return TriContainPoint(v0,v1,v2,triSideAx0,triSideAx1,triSideAx2,triAx,pos,c);
}
ICF bool TriContainPoint(Triangle* T,const float *pos,u16 &c, const Fvector* V_array )
{
	//TriContainPoint(const dReal* v0,const dReal* v1,const dReal* v2,const dReal* triAx,const dReal* triSideAx0,const dReal* triSideAx1, const dReal* pos)
	//VERIFY( g_pGameLevel );
	//const Fvector* V_array=inl_ph_world().ObjectSpace().GetStaticVerts();
	Fvector VRT[3];
	T->T.GetVerts(VRT);
	return TriContainPoint((dReal*)&VRT[0],(dReal*)&VRT[1],(dReal*)&VRT[2],T->norm,T->side0,T->side1,pos,c);
}

enum ETriDist
{
	tdBehind,
	tdPlane,
	tdSide,
	tdVert
};

IC float DistToFragmenton(const dReal	*point,const dReal* pt1, const dReal* pt2,dReal	*p,dReal* to_point,u16 &c)
{
	dVector3 V={pt2[0]-pt1[0],pt2[1]-pt1[1],pt2[2]-pt1[2]};
	dVector3 L={pt1[0]-point[0],pt1[1]-point[1],pt1[2]-point[2]};
	dReal sq_mag_V=dDOT(V,V);
	dReal dot_L_V=dDOT(L,V);
	dReal t=-dot_L_V/sq_mag_V;//t
	if(t<0.f)
	{
		c=1;
		dVectorSet(p,pt1);
		dVectorSet(to_point,L);
		return dSqrt(dDOT(L,L));
	}
	else if (t>1.f)
	{
		c=2;
		dVectorSet(p,pt2);
		dVectorSub(L,pt2,point);
		dVectorSet(to_point,L);
		return dSqrt(dDOT(L,L));
	}
	c=0;
	dVector3 Pc={pt1[0]+t*V[0],pt1[1]+t*V[1],pt1[2]+t*V[2]};
	dVectorSet(p,Pc);
	dVector3 Dc={point[0]-Pc[0],point[1]-Pc[1],point[2]-Pc[2]};
	dVectorSet(to_point,Dc);
	return dSqrt(dDOT(Dc,Dc));
}
ICF float DistToTri(Triangle* T,const float *pos,float *dir,float* p,ETriDist &c,const Fvector *V_array)
{
	if(!TriPlaneContainPoint(T)) 
	{
		c=tdBehind; return -1.f;
	}
	u16 code;
	if(TriContainPoint(T,pos,code, V_array ))
	{
		c=tdPlane;

		cast_fv(p).mad(cast_fv(pos), cast_fv(T->norm), -T->dist);
		cast_fv(dir).invert(cast_fv(T->norm));
		return T->dist;
	}
	Fvector VRT[3];
	T->T.GetVerts(VRT);
	u16 cd=u16(-1);
	float tdist=0.f;
	
	switch (code)
	{
		case 1:	tdist=DistToFragmenton(pos,(dReal*)&VRT[0],(dReal*)&VRT[1],p,dir,cd);break;
		case 2:	tdist=DistToFragmenton(pos,(dReal*)&VRT[1],(dReal*)&VRT[2],p,dir,cd);break;
		case 3:	tdist=DistToFragmenton(pos,(dReal*)&VRT[2],(dReal*)&VRT[0],p,dir,cd);break;
		default: NODEFAULT;
	}
	switch (cd)
	{
		case 0:
			if(tdist>EPS_S) cast_fv(dir).mul(1.f/tdist);
			c=tdSide;
			return tdist;
		case 1:
			dVectorSet(p,(dReal*)&VRT[code-1]);
			break;
		case 2:
			dVectorSet(p,(dReal*)&VRT[code%3]);
			break;
		default: NODEFAULT;
	}
	dVectorSub(dir,p,pos);
	float sqd=dDOT(dir,dir);
	if(sqd>EPS_S) 
	{
		tdist=dSqrt(sqd);
		cast_fv(dir).mul(1.f/tdist);
	}
	else tdist =0.f;
	return tdist;
	//u16 c2;
	//float tdist2=DistToFragmenton(pos,VRT[0],VRT[1],p,dir,c);
	//u16 c3;
	//float tdist3=DistToFragmenton(pos,VRT[0],VRT[1],p,dir,c);
	//u16 cc;float tdist;
	//MIN_OF(tdist1,cc=c1;tdist=tdist1,tdist2,cc=c2;tdist=tdist2,tdist3,cc=c3;tdist=tdist3);

	//return _min(_min(DistToFragmenton(pos)))
}
#pragma warning(default:4995)
#pragma warning(default:4267)