#include "stdafx.h"


#include "tlsprite.h"
#include "ui_main.h"
#include "d3dutils.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CTLSprite::CTLSprite	( )
{
	mesh.setcolor	(0xFFFFFFFF);
	mesh.setdepth	( );
	mesh.settex		(0,0,1,1);
}

CTLSprite::~CTLSprite( )
{
}

void CTLSprite::Render(Fvector &pos, float radius, bool bFixedSize, u32 clr)
{
	FVF::TL			P;
    float 			cx,cy,sz;

	P.transform		( pos, EDevice->mFullTransform );
    cx				= EDevice->_x2real(P.p.x);
    cy				= EDevice->_y2real(P.p.y);
    if (bFixedSize) sz = EDevice->_x2real(radius)-EDevice->_x2real(0);
    else			sz = UI->GetRenderWidth()*radius/P.p.w;
    mesh.m[0].p.x = cx-sz;	mesh.m[0].p.y = cy+sz;
    mesh.m[1].p.x = cx-sz;	mesh.m[1].p.y = cy-sz;
    mesh.m[3].p.x = cx+sz;	mesh.m[3].p.y = cy-sz;
    mesh.m[2].p.x = cx+sz;	mesh.m[2].p.y = cy+sz;
    mesh.setdepth	( P.p.z, P.p.w );
    mesh.setcolor	( clr );

    DU_impl.DrawPrimitiveTL(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_STRIP,2,(FVF::TL*)&mesh,4,false,false);
}

void CTLSprite::Render(Fvector &pos, u32 color, float radius, float angle)
{
	FVF::TL			P;

	P.transform     (pos, EDevice->mFullTransform);
    if (P.p.w<=0) 	return;

	float cx		= EDevice->_x2real(P.p.x);
	float cy		= EDevice->_y2real(P.p.y);

	float sz		= (UI->GetRenderWidth()*radius)/P.p.w;
	if (sz<1.5f) 	return;

	// Rotation
	float			_sin1=_sin(angle),_cos1=_cos(angle);
    float			_sin2=_sin(angle+PI_DIV_2),_cos2=_cos(angle+PI_DIV_2);

	mesh.m[0].p.x = cx+sz*_sin1;	mesh.m[0].p.y = cy+sz*_cos1;
	mesh.m[1].p.x = cx-sz*_sin2;	mesh.m[1].p.y = cy-sz*_cos2;
	mesh.m[2].p.x = cx+sz*_sin2;	mesh.m[2].p.y = cy+sz*_cos2;
	mesh.m[3].p.x = cx-sz*_sin1;	mesh.m[3].p.y = cy-sz*_cos1;

	mesh.setdepth	(P.p.z, P.p.w);
	mesh.setcolor	(color);

    DU_impl.DrawPrimitiveTL(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_STRIP,2,(FVF::TL*)&mesh,4,false,false);
}

void CTLSprite::Render( Fvector &pos, u32 color, float radius, const Fvector& D ){
	Fvector			P1,P2;
    P1.mad			(pos,D,-radius);
    P2.mad 			(pos,D,radius);

	FVF::TL			s1,s2;
	s1.transform	(P1,EDevice->mFullTransform);
	s2.transform	(P2,EDevice->mFullTransform);

    if ((s1.p.w<=0)||(s2.p.w<=0)) return;

	float l1 		= radius/s1.p.w*1.4142f;
    float l2 		= radius/s2.p.w*1.4142f;

	Fvector2		dir,R;
	dir.set			(s2.p.x-s1.p.x,s2.p.y-s1.p.y);
	dir.norm		();
	R.cross			(dir);

	mesh.m[0].p.x 	= EDevice->_x2real(s1.p.x+l1*R.x);	mesh.m[0].p.y = EDevice->_y2real(s1.p.y+l1*R.y); mesh.m[0].p.z = s2.p.z; mesh.m[0].p.w = s2.p.w;
	mesh.m[1].p.x 	= EDevice->_x2real(s2.p.x+l2*R.x);	mesh.m[1].p.y = EDevice->_y2real(s2.p.y+l2*R.y); mesh.m[1].p.z = s2.p.z; mesh.m[1].p.w = s2.p.w;
	mesh.m[2].p.x 	= EDevice->_x2real(s1.p.x-l1*R.x);	mesh.m[2].p.y = EDevice->_y2real(s1.p.y-l1*R.y); mesh.m[2].p.z = s2.p.z; mesh.m[2].p.w = s2.p.w;
	mesh.m[3].p.x 	= EDevice->_x2real(s2.p.x-l2*R.x);	mesh.m[3].p.y = EDevice->_y2real(s2.p.y-l2*R.y); mesh.m[3].p.z = s2.p.z; mesh.m[3].p.w = s2.p.w;

	mesh.setcolor	(color);

    DU_impl.DrawPrimitiveTL(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_STRIP,2,(FVF::TL*)&mesh,4,false,false);
}