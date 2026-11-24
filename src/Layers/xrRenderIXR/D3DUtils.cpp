// file: D3DUtils.cpp

#include "stdafx.h"


#include "../../xrEngine/GameFont.h"
#include "D3DUtils.h"
#include "du_box.h"
#include "du_sphere.h"
#include "du_sphere_part.h"
#include "du_cone.h"
#include "du_cylinder.h"

#include "dxRenderDeviceRender.h"
#include "EditorDrawContext.h"

#include "../../xrCore/FVFLegacy.h"

CDrawUtilities DUImpl;

void DebugDraw::Primitive(GfxCmdList& Cmd, ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount, bool IsStream)
{
	Cmd.SetGeometry(Geom);
	const u32 Count = RHITopologyUtils::GetIndexCount(PrimitiveCount, Topology);

	if (!IsStream)
	{
		Cmd.Draw(Topology, Count, BaseVertex);
		return;
	}

	DynamicGeometry& Stream = RImplementation.Dynamic;
	Topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN ? Stream.DrawFan(Cmd, PrimitiveCount, BaseVertex) : Stream.Draw(Cmd, Topology, Count, BaseVertex);
}

#define LINE_DIVISION  32  // не меньше 6!!!!!
// for drawing sphere
static Fvector circledef1[LINE_DIVISION];
static Fvector circledef2[LINE_DIVISION];
static Fvector circledef3[LINE_DIVISION];

constexpr u32 boxcolor = color_rgba(255,255,255,0);
static const int boxvertcount = 48;
static Fvector boxvert[boxvertcount];

#define DU_DRAW_RS(State, Value) Recorder.Call([] { GEditorDraw.SetRS(State, Value); })
#define DU_DRAW_SH_C(sh,c) RecordShader(sh, c)
#define DU_DRAW_SH(sh) RecordShader(sh, 0xffffffff)
#undef DU_DRAW_DP
#define DU_DRAW_DP(Topology, Geom, BaseVertex, PrimitiveCount) RecordPrimitive(Topology, Geom, BaseVertex, PrimitiveCount)

static void RecordShader(const ref_shader& Shader, u32 Color)
{
	Recorder.SetPass(Shader->E[0]._get());
	Recorder.Call([Color] { RImplementation.Imm().Constants.Write<CBMaterial>().tfactor.set(float(color_get_R(Color))/255.f, float(color_get_G(Color))/255.f, float(color_get_B(Color))/255.f, float(color_get_A(Color))/255.f); });
}

// The recorder draws no fan: a fan's vertices are rewritten as its triangles' list by FanToList first, so a fan here is that list
static void RecordPrimitive(ERHI_PRIMITIVE_TOPOLOGY Topology, const ref_geom& Geom, u32 Offset, u32 PrimitiveCount)
{
	const ERHI_PRIMITIVE_TOPOLOGY List = Topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN ? ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST : Topology;
	Recorder.Draw(Geom._get(), List, RHITopologyUtils::GetIndexCount(PrimitiveCount, List), Offset);
}

// In place, from the back: the lock must hold Triangles * 3 vertices
template <typename T>
static void FanToList(T* Vertices, u32 Triangles)
{
	for (u32 Index = Triangles * 3; Index-- > 0;)
	{
		Vertices[Index] = Vertices[Index % 3 ? Index / 3 + Index % 3 : 0];
	}
}

#define FILL_MODE D3DFILL_SOLID
#define SHADE_MODE D3DSHADE_GOURAUD
#define SCREEN_QUALITY 1.f


// identity box
constexpr u32 identboxcolor = color_rgba(255,255,255,0);
static const int identboxwirecount = 24;
static Fvector identboxwire[identboxwirecount] = {
	{-0.5f, -0.5f, -0.5f},	{-0.5f, +0.5f, -0.5f},    	{-0.5f, +0.5f, -0.5f},	{+0.5f, +0.5f, -0.5f},
    {+0.5f, +0.5f, -0.5f},	{+0.5f, -0.5f, -0.5f},		{+0.5f, -0.5f, -0.5f},	{-0.5f, -0.5f, -0.5f},
    {-0.5f, +0.5f, +0.5f},	{+0.5f, +0.5f, +0.5f},		{+0.5f, +0.5f, +0.5f},	{+0.5f, -0.5f, +0.5f},
    {+0.5f, -0.5f, +0.5f},	{-0.5f, -0.5f, +0.5f},    	{-0.5f, -0.5f, +0.5f},	{-0.5f, +0.5f, +0.5f},
    {-0.5f, +0.5f, -0.5f},	{-0.5f, +0.5f, +0.5f},		{+0.5f, +0.5f, -0.5f},	{+0.5f, +0.5f, +0.5f},
    {+0.5f, -0.5f, -0.5f},	{+0.5f, -0.5f, +0.5f},		{-0.5f, -0.5f, -0.5f},	{-0.5f, -0.5f, +0.5f}
};

/*
static const int identboxindexcount = 36;
static const u16 identboxindices[identboxindexcount] = {
	0, 1, 2,   2, 3, 0,
	3, 2, 6,   6, 7, 3,
	6, 4, 5,   6, 5, 7,
    4, 1, 5,   1, 0, 5,
    3, 5, 0,   3, 7, 5,
    1, 4, 6,   1, 6, 2};
static const int identboxindexwirecount = 24;
static const u16 identboxindiceswire[identboxindexwirecount] = {
	0, 1, 	1, 2,
	2, 3, 	3, 0,
	4, 6, 	6, 7,
    7, 5, 	5, 4,
    1, 4, 	2, 6,
    3, 7, 	0, 5};
*/

#define SIGN(x) ((x<0)?-1:1)

using FLvertexVec = xr_vector<FVF::L>;
using FLvertexIt = FLvertexVec::iterator;

static FLvertexVec 	m_GridPoints;

u32 m_ColorAxis	= 0xff000000;
u32 m_ColorGrid	= 0xff909090;
u32 m_ColorGridTh = 0xffb4b4b4;
constexpr u32 m_SelectionRect = color_rgba(127,255,127,64);

u32 m_ColorSafeRect = 0xffB040B0;

void SPrimitiveBuffer::CreateFromData(ERHI_PRIMITIVE_TOPOLOGY _pt, u32 _p_cnt, const Fvector* vertices, u32 v_cnt, const u16* indices, u32 i_cnt)
{
    p_cnt = _p_cnt;
    p_type = _pt;

    xr_vector<PackedVertex> verts(v_cnt);
    for (u32 k = 0; k < v_cnt; ++k)
        verts[k] = { vertices[k], 0, 0, 0, 0xFFFFFFFF, 0 };

    Alloc = RImplementation.Geometry.Alloc(v_cnt, i_cnt);
    RImplementation.Geometry.Upload(Alloc, 0, verts.data(), v_cnt);
    RImplementation.Geometry.UploadIndices(Alloc, 0, indices, i_cnt);
    pGeom.create(EGeomSource::Static, Alloc.Page);
}

void SPrimitiveBuffer::Destroy()
{
	pGeom.destroy();
	RImplementation.Geometry.Free(Alloc);
}

void SPrimitiveBuffer::Render()
{
	Recorder.Call([Geom = pGeom, Topology = p_type, Alloc = Alloc, Count = p_cnt] { DU_DRAW_DIP(Topology,Geom._get(),Alloc.VertexBase,0,Alloc.VertexCount,Alloc.IndexBase,Count); });
}

void CDrawUtilities::UpdateGrid(int number_of_cell, float square_size, int subdiv){
	m_GridPoints.clear();
// grid
	int m_GridSubDiv[2];
	int m_GridCounts[2];
    Fvector2 m_GridStep;

    m_GridStep.set(square_size,square_size);
	m_GridSubDiv[0] = subdiv;
	m_GridSubDiv[1] = subdiv;
	m_GridCounts[0] = number_of_cell;//iFloor(size/step)*subdiv;
	m_GridCounts[1] = number_of_cell;//iFloor(size/step)*subdiv;

	FVF::L left,right;
	left.p.y = right.p.y = 0;

	for(int thin=0; thin<2; thin++){
		for(int i=-m_GridCounts[0]; i<=m_GridCounts[0]; i++){
			if( (!!thin) != !!(i%m_GridSubDiv[0]) ){
				left.p.z = -m_GridCounts[1]*m_GridStep.y;
				right.p.z = m_GridCounts[1]*m_GridStep.y;
				left.p.x = i*m_GridStep.x;
				right.p.x = left.p.x;
				left.color = (i%m_GridSubDiv[0]) ? m_ColorGrid : m_ColorGridTh;
				right.color = left.color;
				m_GridPoints.push_back( left );
				m_GridPoints.push_back( right );
			}
		}
		for(int i=-m_GridCounts[1]; i<=m_GridCounts[1]; i++){
			if( (!!thin) != !!(i%m_GridSubDiv[1]) ){
				left.p.x = -m_GridCounts[0]*m_GridStep.x;
				right.p.x = m_GridCounts[0]*m_GridStep.x;
				left.p.z = i*m_GridStep.y;
				right.p.z = left.p.z;
				left.color = (i%m_GridSubDiv[1]) ? m_ColorGrid : m_ColorGridTh;
				right.color = left.color;
				m_GridPoints.push_back( left );
				m_GridPoints.push_back( right );
			}
		}
	}
}

void CDrawUtilities::OnDeviceCreate()
{
	Device.seqRender.Add			(this,REG_PRIORITY_LOW-1000);

	m_SolidBox.CreateFromData		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,DU_BOX_NUMFACES,		du_box_vertices,			DU_BOX_NUMVERTEX,			du_box_faces,			DU_BOX_NUMFACES*3);
	m_SolidCone.CreateFromData		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,DU_CONE_NUMFACES,		du_cone_vertices,		DU_CONE_NUMVERTEX,			du_cone_faces,			DU_CONE_NUMFACES*3);
	m_SolidSphere.CreateFromData	(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,DU_SPHERE_NUMFACES,		du_sphere_vertices,		DU_SPHERE_NUMVERTEX,		du_sphere_faces,		DU_SPHERE_NUMFACES*3);
	m_SolidSpherePart.CreateFromData(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,DU_SPHERE_PART_NUMFACES,du_sphere_part_vertices,	DU_SPHERE_PART_NUMVERTEX,	du_sphere_part_faces,	DU_SPHERE_PART_NUMFACES*3);
    m_SolidCylinder.CreateFromData	(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,DU_CYLINDER_NUMFACES,	du_cylinder_vertices,	DU_CYLINDER_NUMVERTEX,		du_cylinder_faces,		DU_CYLINDER_NUMFACES*3);
    m_WireBox.CreateFromData		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,	DU_BOX_NUMLINES,		du_box_vertices,			DU_BOX_NUMVERTEX,			du_box_lines,			DU_BOX_NUMLINES*2);
	m_WireCone.CreateFromData		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,	DU_CONE_NUMLINES,		du_cone_vertices,		DU_CONE_NUMVERTEX,			du_cone_lines,			DU_CONE_NUMLINES*2);
	m_WireSphere.CreateFromData		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,	DU_SPHERE_NUMLINES,		du_sphere_verticesl,		DU_SPHERE_NUMVERTEXL,		du_sphere_lines,		DU_SPHERE_NUMLINES*2);
	m_WireSpherePart.CreateFromData	(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,	DU_SPHERE_PART_NUMLINES,du_sphere_part_vertices,	DU_SPHERE_PART_NUMVERTEX,	du_sphere_part_lines,	DU_SPHERE_PART_NUMLINES*2);
    m_WireCylinder.CreateFromData	(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,	DU_CYLINDER_NUMLINES,	du_cylinder_vertices,	DU_CYLINDER_NUMVERTEX,		du_cylinder_lines,		DU_CYLINDER_NUMLINES*2);

	for(int i=0;i<LINE_DIVISION;i++){                                
		float angle = M_PI * 2.f * (i / (float)LINE_DIVISION);
        float _sa= std::sin(angle), _ca= std::cos(angle);
		circledef1[i].x = _ca;
		circledef1[i].y = _sa;
		circledef1[i].z = 0;
		circledef2[i].x = 0;
		circledef2[i].y = _ca;
		circledef2[i].z = _sa;
		circledef3[i].x = _sa;
		circledef3[i].y = 0;
		circledef3[i].z = _ca;
	}
    // initialize identity box
	Fbox bb;
	bb.set(-0.505f,-0.505f,-0.505f, 0.505f,0.505f,0.505f);
	for (int i=0; i<8; i++){
    	Fvector S;
    	Fvector p;
        bb.getpoint(i,p);
        S.set((float)SIGN(p.x),(float)SIGN(p.y),(float)SIGN(p.z));
    	boxvert[i*6+0].set(p);
    	boxvert[i*6+1].set(p.x-S.x*0.25f,p.y,p.z);
    	boxvert[i*6+2].set(p);
    	boxvert[i*6+3].set(p.x,p.y-S.y*0.25f,p.z);
    	boxvert[i*6+4].set(p);
    	boxvert[i*6+5].set(p.x,p.y,p.z-S.z*0.25f);
    }
    // create render stream
	RingGeom.create(EGeomSource::Ring);

	m_Font						= new CGameFont("stat_font");
}

void CDrawUtilities::OnDeviceDestroy()
{
	Device.seqRender.Remove		(this);
	xr_delete					(m_Font);
    m_SolidBox.Destroy			();
	m_SolidCone.Destroy			();
	m_SolidSphere.Destroy		();
	m_SolidSpherePart.Destroy	();
    m_SolidCylinder.Destroy		();
    m_WireBox.Destroy			();
	m_WireCone.Destroy			();
	m_WireSphere.Destroy		();
	m_WireSpherePart.Destroy	();
    m_WireCylinder.Destroy		();

	RingGeom.destroy		();
}
//----------------

void CDrawUtilities::DrawSpotLight(const Fvector& p, const Fvector& d, float range, float phi, u32 clr)
{
    Fmatrix T;
	Fvector p1;
    float H,P;
    float da	= PI_MUL_2/LINE_DIVISION;
	float b		= range*std::cos(PI_DIV_2-phi/2);
	float a		= range*std::sin(PI_DIV_2-phi/2);
    d.getHP		(H,P);
    T.setHPB	(H,P,0);     
    T.translate_over(p);
    DrawRecorder*	Stream	= &Recorder;
    u32				vBase;
    FVF::L*	pv	 	= (FVF::L*)Stream->Lock(LINE_DIVISION*2+2,EVertexFormat::L,vBase);
	for (float angle=0; angle<PI_MUL_2; angle+=da){
        float _sa	=std::sin(angle);
        float _ca	=std::cos(angle);
		p1.x		= b * _ca;
		p1.y		= b * _sa;
        p1.z		= a;
        T.transform_tiny(p1);
        // fill VB
        pv->set		(p,clr); pv++;
        pv->set		(p1,clr); pv++;
    }
    p1.mad			(p,d,range);
    pv->set			(p,clr); pv++;
    pv->set			(p1,clr); pv++;
    Stream->Unlock	(LINE_DIVISION*2+2);
    // and Render it as triangle list
    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,LINE_DIVISION+1);
}

void CDrawUtilities::DrawDirectionalLight(const Fvector& p, const Fvector& d, float radius, float range, u32 c)
{
    float r=radius*0.71f;
	Fvector R,N,D; D.normalize(d);
	Fmatrix rot;

    N.set		(0,1,0);
	if (std::abs(D.y)>0.99f) N.set(1,0,0);
	R.crossproduct(N,D); R.normalize();
	N.crossproduct(D,R); N.normalize();
    rot.set(R,N,D,p);
	float sz=radius+range;

	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32				vBase;
	FVF::L*	pv	 	= (FVF::L*)Stream->Lock(6,EVertexFormat::L,vBase);
    pv->set			(0,0,r,		c); rot.transform_tiny(pv->p); pv++;
    pv->set			(0,0,sz,	c); rot.transform_tiny(pv->p); pv++;
    pv->set			(-r,0,r,	c); rot.transform_tiny(pv->p); pv++;
    pv->set			(-r,0,sz,	c); rot.transform_tiny(pv->p); pv++;
    pv->set			(r,0,r,		c); rot.transform_tiny(pv->p); pv++;
    pv->set			(r,0,sz,	c); rot.transform_tiny(pv->p); pv++;
	Stream->Unlock	(6);

	// and Render it as triangle list
    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,3);

    Fbox b;
    b.min.set(-r,-r,-r);
    b.max.set(r,r,r);

	DrawLineSphere	( p, radius, c, true );
}

void CDrawUtilities::DrawPointLight(const Fvector& p, float radius, u32 c)
{
	Recorder.SetWorld(Fidentity);
	DrawCross(p, radius,radius,radius, radius,radius,radius, c, true);
}

void CDrawUtilities::DrawEntity(u32 clr, ref_shader s)
{
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase;
	FVF::L*	pv	 	= (FVF::L*)Stream->Lock(5,EVertexFormat::L,vBase);
    pv->set			(0.f,0.f,0.f,clr); pv++;
    pv->set			(0.f,1.f,0.f,clr); pv++;
    pv->set			(0.f,1.f,.5f,clr); pv++;
    pv->set			(0.f,.5f,.5f,clr); pv++;
    pv->set			(0.f,.5f,0.f,clr); pv++;
	Stream->Unlock	(5);
	// render flagshtok
	DU_DRAW_SH		(dxRenderDeviceRender::Instance().m_WireShader);
    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,4);

    if (s) DU_DRAW_SH(s);
    {
        // fill VB
        FVF::LIT*	pv_	 = (FVF::LIT*)Stream->Lock(12,EVertexFormat::LIT,vBase);
        pv_->set		(0.f,1.f,0.f,clr,0.f,0.f);	pv_++;
        pv_->set		(0.f,1.f,.5f,clr,1.f,0.f);	pv_++;
        pv_->set		(0.f,.5f,.5f,clr,1.f,1.f);	pv_++;
        pv_->set		(0.f,.5f,0.f,clr,0.f,1.f);	pv_++;
        pv_->set		(0.f,.5f,.5f,clr,1.f,1.f);	pv_++;
        pv_->set		(0.f,1.f,.5f,clr,1.f,0.f);	pv_++;
        FanToList		(pv_-6,4);
        Stream->Unlock	(12);
        // and Render it as line list
        DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN,RingGeom,vBase,4);
    }
}

void CDrawUtilities::DrawRomboid(const Fvector& p, float r, u32 c)
{
static const u16 IL[24] = {0, 2, 2, 5, 0, 5, 3, 5, 3, 0, 4, 3, 4, 0, 4, 2, 1, 2, 1, 5, 1, 3, 1, 4};
	static const u16 IT[24] = {2, 4, 0, 4, 3, 0, 3, 5, 0, 5, 2, 0, 4, 2, 1, 2, 5, 1, 5, 3, 1, 3, 4, 1};
	u32			vBase;

    Fcolor C;
    C.set			(c);
    C.mul_rgb		(0.75);
    u32 c1 =		C.get();

	// The corners by the indices: the stream draws no index buffer
	const Fvector Corners[6] = { { p.x, p.y + r, p.z }, { p.x, p.y - r, p.z }, { p.x, p.y, p.z - r }, { p.x, p.y, p.z + r }, { p.x - r, p.y, p.z }, { p.x + r, p.y, p.z } };
	DrawRecorder*	Stream	= &Recorder;
	FVF::L*	pv	 	= (FVF::L*)Stream->Lock(24,EVertexFormat::L,vBase);
	for (u32 k = 0; k < 24; ++k)
	{
		pv[k].set(Corners[IT[k]], c1);
	}
	Stream->Unlock	(24);
	DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,RingGeom,vBase,8);

	pv	 			= (FVF::L*)Stream->Lock(24,EVertexFormat::L,vBase);
	for (u32 k = 0; k < 24; ++k)
	{
		pv[k].set(Corners[IL[k]], c);
	}
	Stream->Unlock	(24);
	DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,12);
}
//------------------------------------------------------------------------------

void CDrawUtilities::DrawSound(const Fvector& p, float r, u32 c){
	DrawCross(p, r,r,r, r,r,r, c, true);
}
//------------------------------------------------------------------------------
void CDrawUtilities::DrawIdentCone	(bool bSolid, bool bWire, u32 clr_s, u32 clr_w)
{
    if (bWire){
        DU_DRAW_SH_C		(dxRenderDeviceRender::Instance().m_WireShader, clr_w);
		m_WireCone.Render	();
    }
    if (bSolid){
        DU_DRAW_SH_C		(color_get_A(clr_s)>=254?dxRenderDeviceRender::Instance().m_WireShader:dxRenderDeviceRender::Instance().m_SelectionShader,	clr_s);
    	m_SolidCone.Render	();
    }
	DU_DRAW_RS	(D3DRS_TEXTUREFACTOR,	0xffffffff);
}

void CDrawUtilities::DrawIdentSphere	(bool bSolid, bool bWire, u32 clr_s, u32 clr_w)
{
    if (bWire){
        DU_DRAW_SH_C	(dxRenderDeviceRender::Instance().m_WireShader,clr_w);
     	m_WireSphere.Render	();
    }
    if (bSolid){
        DU_DRAW_SH_C	(color_get_A(clr_s)>=254?dxRenderDeviceRender::Instance().m_WireShader:dxRenderDeviceRender::Instance().m_SelectionShader,clr_s);
    	m_SolidSphere.Render();
    }
	DU_DRAW_RS	(D3DRS_TEXTUREFACTOR,	0xffffffff);
}

void CDrawUtilities::DrawIdentSpherePart(bool bSolid, bool bWire, u32 clr_s, u32 clr_w)
{
    if (bWire){
        DU_DRAW_SH_C	(dxRenderDeviceRender::Instance().m_WireShader,clr_w);
     	m_WireSpherePart.Render	();
    }
    if (bSolid){
        DU_DRAW_SH_C	(color_get_A(clr_s)>=254?dxRenderDeviceRender::Instance().m_WireShader:dxRenderDeviceRender::Instance().m_SelectionShader,clr_s);
    	m_SolidSpherePart.Render();
    }
	DU_DRAW_RS	(D3DRS_TEXTUREFACTOR,	0xffffffff);
}

void CDrawUtilities::DrawIdentCylinder	(bool bSolid, bool bWire, u32 clr_s, u32 clr_w)
{
    if (bWire){
        DU_DRAW_SH_C	(dxRenderDeviceRender::Instance().m_WireShader,clr_w);
    	m_WireCylinder.Render	();
    }
    if (bSolid){
        DU_DRAW_SH_C	(color_get_A(clr_s)>=254?dxRenderDeviceRender::Instance().m_WireShader:dxRenderDeviceRender::Instance().m_SelectionShader,clr_s);
    	m_SolidCylinder.Render	();
    }
	DU_DRAW_RS	(D3DRS_TEXTUREFACTOR,	0xffffffff);
}

void CDrawUtilities::DrawIdentBox(bool bSolid, bool bWire, u32 clr_s, u32 clr_w)
{
    if (bWire){
        DU_DRAW_SH_C	(dxRenderDeviceRender::Instance().m_WireShader,clr_w);
    	m_WireBox.Render	();
    }
    if (bSolid){
        DU_DRAW_SH_C	(color_get_A(clr_s)>=254?dxRenderDeviceRender::Instance().m_WireShader:dxRenderDeviceRender::Instance().m_SelectionShader,clr_s);
    	m_SolidBox.Render	();
    }
	DU_DRAW_RS	(D3DRS_TEXTUREFACTOR,	0xffffffff);
}

void CDrawUtilities::DrawLineSphere(const Fvector& p, float radius, u32 c, bool bCross)
{
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase;
    int i;
	FVF::L*	pv;
    // seg 0
	pv	 			= (FVF::L*)Stream->Lock(LINE_DIVISION+1,EVertexFormat::L,vBase);
	for( i=0; i<LINE_DIVISION; i++,pv++){ pv->p.mad(p,circledef1[i],radius); pv->color=c;}
    pv->set(*(pv-LINE_DIVISION));
	Stream->Unlock	(LINE_DIVISION+1);
	DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,LINE_DIVISION);
    // seg 1
	pv	 			= (FVF::L*)Stream->Lock(LINE_DIVISION+1,EVertexFormat::L,vBase);
	for( i=0; i<LINE_DIVISION; i++){ pv->p.mad(p,circledef2[i],radius); pv->color=c; pv++; }
    pv->set(*(pv-LINE_DIVISION)); pv++;
	Stream->Unlock	(LINE_DIVISION+1);
	DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,LINE_DIVISION);
    // seg 2
	pv	 			= (FVF::L*)Stream->Lock(LINE_DIVISION+1,EVertexFormat::L,vBase);
	for( i=0; i<LINE_DIVISION; i++){ pv->p.mad(p,circledef3[i],radius); pv->color=c; pv++; }
    pv->set(*(pv-LINE_DIVISION)); pv++;
	Stream->Unlock	(LINE_DIVISION+1);
	DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,LINE_DIVISION);

    if (bCross) DrawCross(p, radius,radius,radius, radius,radius,radius, c);
}

IC float _x2real(float x)
{
	return (x + 1) * GfxCmdList::GetWidth() * 0.5f;
}
IC float _y2real(float y)
{
	return (y + 1) * GfxCmdList::GetHeight() * 0.5f;
}

void CDrawUtilities::dbgDrawPlacement(const Fvector& p, int sz, u32 clr, const char* caption, u32 clr_font)
{
	VERIFY( Device.b_is_Ready );
    Fvector c;
	float w	= p.x*RFrame.mFullTransform._14 + p.y*RFrame.mFullTransform._24 + p.z*RFrame.mFullTransform._34 + RFrame.mFullTransform._44;
    if (w<0) return; // culling

	float s = (float)sz;
	RFrame.mFullTransform.transform(c,p);
	c.x = (float)iFloor(_x2real(c.x)); c.y = (float)iFloor(_y2real(-c.y));

	DrawRecorder*	Stream	= &Recorder;
    u32 vBase;
	FVF::TL* pv	= (FVF::TL*)Stream->Lock(5,EVertexFormat::TL,vBase);
	pv->p.set(c.x-s,c.y-s,0,1); pv->color=clr; pv++;
	pv->p.set(c.x+s,c.y-s,0,1); pv->color=clr; pv++;
	pv->p.set(c.x+s,c.y+s,0,1); pv->color=clr; pv++;
	pv->p.set(c.x-s,c.y+s,0,1); pv->color=clr; pv++;
	pv->p.set(c.x-s,c.y-s,0,1); pv->color=clr; pv++;
	Stream->Unlock(5);

	// Render it as line strip
    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,4);
    if (caption){
	    m_Font->SetColor(clr_font);
    	m_Font->Out(c.x,c.y+s,"%s",caption);
    }
}

void CDrawUtilities::dbgDrawVert(const Fvector& p0, u32 clr, const char* caption)
{
	dbgDrawPlacement(p0,1,clr,caption);
	DrawCross		(p0,0.01f,0.01f,0.01f, 0.01f,0.01f,0.01f, clr,false);
}

void CDrawUtilities::dbgDrawEdge(const Fvector& p0,	const Fvector& p1, u32 clr, const char* caption)
{
	dbgDrawPlacement(p0,1,clr,caption);
	DrawCross		(p0,0.01f,0.01f,0.01f, 0.01f,0.01f,0.01f, clr,false);
	DrawCross		(p1,0.01f,0.01f,0.01f, 0.01f,0.01f,0.01f, clr,false);
    DrawLine		(p0,p1,clr);
}

void CDrawUtilities::dbgDrawFace(const Fvector& p0,	const Fvector& p1, const Fvector& p2, u32 clr, const char* caption)
{
	dbgDrawPlacement(p0,1,clr,caption);
	DrawCross		(p0,0.01f,0.01f,0.01f, 0.01f,0.01f,0.01f, clr,false);
	DrawCross		(p1,0.01f,0.01f,0.01f, 0.01f,0.01f,0.01f, clr,false);
	DrawCross		(p2,0.01f,0.01f,0.01f, 0.01f,0.01f,0.01f, clr,false);
    DrawLine		(p0,p1,clr);
    DrawLine		(p1,p2,clr);
    DrawLine		(p2,p0,clr);
}
//----------------------------------------------------

void CDrawUtilities::DrawLine(const Fvector& p0, const Fvector& p1, u32 c){
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase;
	FVF::L*	pv	 	= (FVF::L*)Stream->Lock(2,EVertexFormat::L,vBase);
    pv->set			(p0,c); pv++;
    pv->set			(p1,c); pv++;
	Stream->Unlock	(2);
	// and Render it as triangle list
    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,1);
}

//----------------------------------------------------
void CDrawUtilities::DrawSelectionBox(const Fvector& C, const Fvector& S, u32* c)
{
    u32 cc=(c)?*c:boxcolor;

	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase;
	FVF::L*	pv	 	= (FVF::L*)Stream->Lock(boxvertcount,EVertexFormat::L,vBase);
    for (int i=0; i<boxvertcount; i++,pv++){
	    pv->p.mul(boxvert[i],S);
        pv->p.add(C);
        pv->color	= cc;
    }
	Stream->Unlock	(boxvertcount);

	// and Render it as triangle list
	DU_DRAW_RS	(D3DRS_FILLMODE,D3DFILL_SOLID);
    DU_DRAW_DP	(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,boxvertcount/2);
    DU_DRAW_RS	(D3DRS_FILLMODE,FILL_MODE);
}

void CDrawUtilities::DrawBox(const Fvector& offs, const Fvector& Size, bool bSolid, bool bWire, u32 clr_s, u32 clr_w)
{
	DrawRecorder*	Stream	= &Recorder;
    if (bWire){
        u32			vBase;
		FVF::L*	pv	 	= (FVF::L*)Stream->Lock(identboxwirecount,EVertexFormat::L,vBase);
        for (int i=0; i<identboxwirecount; i++, pv++){
            pv->p.mul	(identboxwire[i],Size);
            pv->p.mul	(2);
            pv->p.add	(offs);
            pv->color	= clr_w;
        }
		Stream->Unlock(identboxwirecount);

	    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,identboxwirecount/2);
    }
    if (bSolid){
        u32			vBase;
		FVF::L*	pv	 	= (FVF::L*)Stream->Lock(DU_BOX_NUMVERTEX2,EVertexFormat::L,vBase);
        for (int i=0; i<DU_BOX_NUMVERTEX2; i++, pv++){
            pv->p.mul	(du_box_vertices2[i],Size);
            pv->p.mul	(2);
            pv->p.add	(offs);
            pv->color	= clr_s;
        }
		Stream->Unlock(DU_BOX_NUMVERTEX2);

	    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,RingGeom,vBase,DU_BOX_NUMFACES);
    }
}
//----------------------------------------------------

void CDrawUtilities::DrawOBB(const Fmatrix& parent, const Fobb& box, u32 clr_s, u32 clr_w)
{
    Fmatrix			R,S,X;
    box.xform_get	(R);
    S.scale			(box.m_halfsize.x*2.f,box.m_halfsize.y*2.f,box.m_halfsize.z*2.f);
    X.mul_43		(R,S);
    R.mul_43		(parent,X); 
	Recorder.SetWorld(R);
	DrawIdentBox	(true,true,clr_s,clr_w);
}
//----------------------------------------------------

void CDrawUtilities::DrawAABB(const Fmatrix& parent, const Fvector& center, const Fvector& size, u32 clr_s, u32 clr_w, bool bSolid, bool bWire)
{
    Fmatrix			R,S;
    S.scale			(size.x*2.f,size.y*2.f,size.z*2.f);
    S.translate_over(center);
    R.mul_43		(parent,S); 
	Recorder.SetWorld(R);
	DrawIdentBox	(bSolid,bWire,clr_s,clr_w);
}

void CDrawUtilities::DrawAABB(const Fvector& p0, const Fvector& p1, u32 clr_s, u32 clr_w, bool bSolid, bool bWire)
{
    Fmatrix			R;
	Fvector	C; C.set((p1.x+p0.x)*0.5f,(p1.y+p0.y)*0.5f,(p1.z+p0.z)*0.5f);
    R.scale			(std::abs(p1.x-p0.x), std::abs(p1.y-p0.y), std::abs(p1.z-p0.z));
    R.translate_over(C);
	Recorder.SetWorld(R);
	DrawIdentBox	(bSolid,bWire,clr_s,clr_w);
}

void CDrawUtilities::DrawSphere(const Fmatrix& parent, const Fvector& center, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire)
{
    Fmatrix B;
    B.scale				(radius,radius,radius);
    B.translate_over	(center);
    B.mulA_43			(parent);
    Recorder.SetWorld(B);
    DrawIdentSphere		(bSolid, bWire, clr_s,clr_w);
}
//----------------------------------------------------

void CDrawUtilities::DrawFace(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr_s, u32 clr_w, bool bSolid, bool bWire)
{
	DrawRecorder*	Stream	= &Recorder;

    u32				vBase;
    if (bSolid)
    {
        FVF::L*	pv	 	= (FVF::L*)Stream->Lock(3,EVertexFormat::L,vBase);
        pv->set			(p0,clr_s); pv++;
        pv->set			(p1,clr_s); pv++;
        pv->set			(p2,clr_s); pv++;
        Stream->Unlock	(3);
	    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,RingGeom,vBase,1);
    }
    if (bWire)
    {
        FVF::L*	pv	 	= (FVF::L*)Stream->Lock(4,EVertexFormat::L,vBase);
        pv->set			(p0,clr_w); pv++;
        pv->set			(p1,clr_w); pv++;
        pv->set			(p2,clr_w); pv++;
        pv->set			(p0,clr_w); pv++;
        Stream->Unlock	(4);
	    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,3);
    }
}
//----------------------------------------------------

static const u32 MAX_VERT_COUNT 	= 0xFFFF;
void CDrawUtilities::DD_DrawFace_begin(bool bWire)
{
	VERIFY				(m_DD_pv_start==0);
    m_DD_wire			= bWire;
    m_DD_pv_start		= (FVF::L*)Recorder.Lock(MAX_VERT_COUNT,EVertexFormat::L,m_DD_base);	
    m_DD_pv				= m_DD_pv_start;
}
void CDrawUtilities::DD_DrawFace_flush(bool try_again)
{
    Recorder.Unlock((u32)(m_DD_pv-m_DD_pv_start));
    if (m_DD_wire)		DU_DRAW_RS(D3DRS_FILLMODE,D3DFILL_WIREFRAME);
    DU_DRAW_DP			(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,RingGeom,m_DD_base,u32(m_DD_pv-m_DD_pv_start)/3);
    if (m_DD_wire)		DU_DRAW_RS(D3DRS_FILLMODE,FILL_MODE);
    if (try_again){
	    m_DD_pv_start	= (FVF::L*)Recorder.Lock(MAX_VERT_COUNT,EVertexFormat::L,m_DD_base);	
        m_DD_pv			= m_DD_pv_start;
    }
}
void CDrawUtilities::DD_DrawFace_push(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr)
{
    m_DD_pv->set		(p0,clr); m_DD_pv++;
    m_DD_pv->set		(p1,clr); m_DD_pv++;
    m_DD_pv->set		(p2,clr); m_DD_pv++;
    if (m_DD_pv-m_DD_pv_start==MAX_VERT_COUNT)
        DD_DrawFace_flush	(true); 
}
void CDrawUtilities::DD_DrawFace_end()
{
    DD_DrawFace_flush	(false); 	
    m_DD_pv_start 		= 0;
}
//----------------------------------------------------

void CDrawUtilities::DrawCylinder(const Fmatrix& parent, const Fvector& center, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire)
{
    Fmatrix mScale;
    mScale.scale		(2.f*radius,2.f*radius,height);
    
    // build final rotation / translation
    Fvector             L_dir,L_up,L_right;
    L_dir.set           (dir);       		    L_dir.normalize			();
    L_up.set            (0,1,0);				if (std::abs(L_up.dotproduct(L_dir))>.99f)  L_up.set(0,0,1);
    L_right.crossproduct(L_up,L_dir);           L_right.normalize       ();
    L_up.crossproduct   (L_dir,L_right);        L_up.normalize          ();

    Fmatrix         	mR;
    mR.i                = L_right;              mR._14          = 0;
    mR.j                = L_up;                 mR._24          = 0;
    mR.k                = L_dir;                mR._34          = 0;
    mR.c                = center;		  		mR._44          = 1;

    // final xform
    Fmatrix xf;			xf.mul (mR,mScale);
    xf.mulA_43			(parent);
    Recorder.SetWorld(xf);
	DrawIdentCylinder	(bSolid,bWire,clr_s,clr_w);
}
//----------------------------------------------------

void CDrawUtilities::DrawCone	(const Fmatrix& parent, const Fvector& apex, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire)
{
    Fmatrix mScale;
    mScale.scale		(2.f*radius,2.f*radius,height);
    
    // build final rotation / translation
    Fvector             L_dir,L_up,L_right;
    L_dir.set           (dir);       		    L_dir.normalize			();
    L_up.set            (0,1,0);				if (std::abs(L_up.dotproduct(L_dir))>.99f)  L_up.set(0,0,1);
    L_right.crossproduct(L_up,L_dir);           L_right.normalize       ();
    L_up.crossproduct   (L_dir,L_right);        L_up.normalize          ();

    Fmatrix         	mR;
    mR.i                = L_right;              mR._14          = 0;
    mR.j                = L_up;                 mR._24          = 0;
    mR.k                = L_dir;                mR._34          = 0;
    mR.c                = apex;			  		mR._44          = 1;

    // final xform
    Fmatrix xf;			xf.mul (mR,mScale);
    xf.mulA_43			(parent);
    Recorder.SetWorld(xf);
	DrawIdentCone		(bSolid,bWire,clr_s,clr_w);
}
//----------------------------------------------------

void CDrawUtilities::DrawPlane	(const Fvector& p, const Fvector& n, const Fvector2& scale, u32 clr_s, u32 clr_w, bool bCull, bool bSolid, bool bWire)
{
	if (n.square_magnitude()<EPS_S) return;
    // build final rotation / translation
    Fvector             L_dir,L_up=n,L_right;
    L_dir.set           (0,0,1);				if (std::abs(L_up.dotproduct(L_dir))>.99f)  L_dir.set(1,0,0);
    L_right.crossproduct(L_up,L_dir);           L_right.normalize	();
    L_dir.crossproduct  (L_right,L_up);        	L_dir.normalize		();

    Fmatrix         	mR;
    mR.i                = L_right;              mR._14          = 0;
    mR.j                = L_up;                 mR._24          = 0;
    mR.k                = L_dir;                mR._34          = 0;
    mR.c                = p;			  		mR._44          = 1;
	
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase;
    
    if (bSolid){
	    DU_DRAW_SH(dxRenderDeviceRender::Instance().m_SelectionShader);
        FVF::L*	pv	 = (FVF::L*)Stream->Lock(6,EVertexFormat::L,vBase);
        pv->set		(-scale.x, 0, -scale.y, clr_s); mR.transform_tiny(pv->p); pv++;
        pv->set		(-scale.x, 0, +scale.y, clr_s); mR.transform_tiny(pv->p); pv++;
        pv->set		(+scale.x, 0, +scale.y, clr_s); mR.transform_tiny(pv->p); pv++;
        pv->set		(+scale.x, 0, -scale.y, clr_s); mR.transform_tiny(pv->p); pv++;
        pv->set		(*(pv-4));
        FanToList	(pv-4,2);
        Stream->Unlock(6);
        if (!bCull) Recorder.SetCull(ERHI_CULLMODE::NONE);
        DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN,RingGeom,vBase,2);
        if (!bCull) Recorder.SetCull(ERHI_CULLMODE::BACK);
    }

    if (bWire){
	    DU_DRAW_SH(dxRenderDeviceRender::Instance().m_WireShader);
        FVF::L*	pv	 = (FVF::L*)Stream->Lock(5,EVertexFormat::L,vBase);
        pv->set		(-scale.x, 0, -scale.y, clr_w); mR.transform_tiny(pv->p); pv++;
        pv->set		(+scale.x, 0, -scale.y, clr_w); mR.transform_tiny(pv->p); pv++;
        pv->set		(+scale.x, 0, +scale.y, clr_w); mR.transform_tiny(pv->p); pv++;
        pv->set		(-scale.x, 0, +scale.y, clr_w); mR.transform_tiny(pv->p); pv++;
        pv->set		(*(pv-4));
        Stream->Unlock(5);
	    DU_DRAW_DP	(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,4);
    }
}
//----------------------------------------------------

void CDrawUtilities::DrawPlane  (const Fvector& center, const Fvector2& scale, const Fvector& rotate, u32 clr_s, u32 clr_w, bool bCull, bool bSolid, bool bWire)
{
    Fmatrix M;
    M.setHPB		(rotate.y,rotate.x,rotate.z);
    M.translate_over(center);
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase;
    
    if (bSolid){
	    DU_DRAW_SH(dxRenderDeviceRender::Instance().m_SelectionShader);
        FVF::L*	pv	 = (FVF::L*)Stream->Lock(6,EVertexFormat::L,vBase);
        pv->set		(-scale.x, 0, -scale.y, clr_s); M.transform_tiny(pv->p); pv++;
        pv->set		(-scale.x, 0, +scale.y, clr_s); M.transform_tiny(pv->p); pv++;
        pv->set		(+scale.x, 0, +scale.y, clr_s); M.transform_tiny(pv->p); pv++;
        pv->set		(+scale.x, 0, -scale.y, clr_s); M.transform_tiny(pv->p); pv++;
        pv->set		(*(pv-4));
        FanToList	(pv-4,2);
        Stream->Unlock(6);
        if (!bCull) Recorder.SetCull(ERHI_CULLMODE::NONE);
        DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN,RingGeom,vBase,2);
        if (!bCull) Recorder.SetCull(ERHI_CULLMODE::BACK);
    }

    if (bWire){
	    DU_DRAW_SH(dxRenderDeviceRender::Instance().m_WireShader);
        FVF::L*	pv	 = (FVF::L*)Stream->Lock(5,EVertexFormat::L,vBase);
        pv->set		(-scale.x, 0, -scale.y, clr_w); M.transform_tiny(pv->p); pv++;
        pv->set		(+scale.x, 0, -scale.y, clr_w); M.transform_tiny(pv->p); pv++;
        pv->set		(+scale.x, 0, +scale.y, clr_w); M.transform_tiny(pv->p); pv++;
        pv->set		(-scale.x, 0, +scale.y, clr_w); M.transform_tiny(pv->p); pv++;
        pv->set		(*(pv-4));
        Stream->Unlock(5);
	    DU_DRAW_DP	(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,4);
    }
}
//----------------------------------------------------

void CDrawUtilities::DrawRectangle(const Fvector& o, const Fvector& u, const Fvector& v, u32 clr_s, u32 clr_w, bool bSolid, bool bWire)
{
	DrawRecorder*	Stream	= &Recorder;

    u32				vBase;
    if (bSolid)
    {
	    DU_DRAW_SH(dxRenderDeviceRender::Instance().m_SelectionShader);
        FVF::L*	pv	 	= (FVF::L*)Stream->Lock(6,EVertexFormat::L,vBase);
        pv->set			(o.x,			o.y,		o.z,		clr_s); pv++;
        pv->set			(o.x+u.x+v.x,	o.y+u.y+v.y,o.z+u.z+v.z,clr_s); pv++;
        pv->set			(o.x+v.x,		o.y+v.y,	o.z+v.z,	clr_s); pv++;
        pv->set			(o.x,			o.y,		o.z,		clr_s); pv++;
        pv->set			(o.x+u.x,		o.y+u.y,	o.z+u.z,	clr_s); pv++;
        pv->set			(o.x+u.x+v.x,	o.y+u.y+v.y,o.z+u.z+v.z,clr_s); pv++;
        Stream->Unlock	(6);
	    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,RingGeom,vBase,2);
    }
    if (bWire)
    {
	    DU_DRAW_SH(dxRenderDeviceRender::Instance().m_WireShader);
        FVF::L*	pv	 	= (FVF::L*)Stream->Lock(5,EVertexFormat::L,vBase);
        pv->set			(o.x,			o.y,		o.z,		clr_w); pv++;
        pv->set			(o.x+u.x,		o.y+u.y,	o.z+u.z,	clr_w); pv++;
        pv->set			(o.x+u.x+v.x,	o.y+u.y+v.y,o.z+u.z+v.z,clr_w); pv++;
        pv->set			(o.x+v.x,		o.y+v.y,	o.z+v.z,	clr_w); pv++;
        pv->set			(o.x,			o.y,		o.z,		clr_w); pv++;
        Stream->Unlock	(5);
	    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_STRIP,RingGeom,vBase,4);
    }
}
//----------------------------------------------------

void CDrawUtilities::DrawCross(const Fvector& p, float szx1, float szy1, float szz1, float szx2, float szy2, float szz2, u32 clr, bool bRot45)
{
	DrawRecorder*	Stream	= &Recorder;
	// actual rendering
	u32			vBase;
	FVF::L*	pv	 	= (FVF::L*)Stream->Lock(bRot45?12:6,EVertexFormat::L,vBase);
    pv->set(p.x+szx2,p.y,p.z,clr); pv++;
    pv->set(p.x-szx1,p.y,p.z,clr); pv++;
    pv->set(p.x,p.y+szy2,p.z,clr); pv++;
    pv->set(p.x,p.y-szy1,p.z,clr); pv++;
    pv->set(p.x,p.y,p.z+szz2,clr); pv++;
    pv->set(p.x,p.y,p.z-szz1,clr); pv++;
    if (bRot45){
    	Fmatrix M;
        M.setHPB(PI_DIV_4,PI_DIV_4,PI_DIV_4);
	    for(int i=0;i<6;i++,pv++){
        	pv->p.sub((pv-6)->p,p);
        	M.transform_dir(pv->p);
        	pv->p.add(p);
            pv->color = clr;
        }
    }
	// unlock VB and Render it as triangle list
	Stream->Unlock(bRot45?12:6);
    DU_DRAW_DP			(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,bRot45?6:3);
}

void CDrawUtilities::DrawPivot(const Fvector& pos, float sz){
	DU_DRAW_SH(dxRenderDeviceRender::Instance().m_WireShader);
    DrawCross(pos, sz, sz, sz, sz, sz, sz, 0xFF7FFF7F);
}

void CDrawUtilities::DrawAxis(const Fmatrix& T)
{
	DrawRecorder*	Stream	= &Recorder;
    Fvector p[6];
    u32 	c[6];

    // colors
    c[0]=c[2]=c[4]=0x00222222; c[1]=0x00FF0000; c[3]=0x0000FF00; c[5]=0x000000FF;

    // position
  	p[0].mad(T.c,T.k,0.25f);
    p[1].set(p[0]); p[1].x+=.015f;
    p[2].set(p[0]);
    p[3].set(p[0]); p[3].y+=.015f;
    p[4].set(p[0]);
    p[5].set(p[0]); p[5].z+=.015f;

    u32 vBase;
	FVF::TL* pv	= (FVF::TL*)Stream->Lock(6,EVertexFormat::TL,vBase);
    // transform to screen
    float dx=-float(Device.TargetWidth)/2.2f;
    float dy=float(Device.TargetHeight)/2.25f;

    for (int i=0; i<6; i++,pv++){
	    pv->color = c[i]; pv->transform(p[i],RFrame.mFullTransform);
	    pv->p.set((float)iFloor(_x2real(pv->p.x)+dx),(float)iFloor(_y2real(pv->p.y)+dy),0,1);
        p[i].set(pv->p.x,pv->p.y,0);
    }

	// unlock VB and Render it as triangle list
	Stream->Unlock(6);
	DU_DRAW_RS(D3DRS_SHADEMODE,D3DSHADE_GOURAUD);
	DU_DRAW_SH(dxRenderDeviceRender::Instance().m_WireShader);
    DU_DRAW_DP(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,3);
	DU_DRAW_RS(D3DRS_SHADEMODE,SHADE_MODE);

    m_Font->SetColor(0xFF909090);
    m_Font->Out(p[1].x,p[1].y,"x");
    m_Font->Out(p[3].x,p[3].y,"y");
    m_Font->Out(p[5].x,p[5].y,"z");
    m_Font->SetColor(0xFF000000);
    m_Font->Out(p[1].x-1,p[1].y-1,"x");
    m_Font->Out(p[3].x-1,p[3].y-1,"y");
    m_Font->Out(p[5].x-1,p[5].y-1,"z");
}

void CDrawUtilities::DrawObjectAxis(const Fmatrix& T, float sz, bool sel)
{
	VERIFY( Device.b_is_Ready );
	DrawRecorder*	Stream	= &Recorder;
    Fvector c,r,n,d;
	float w	= T.c.x*RFrame.mFullTransform._14 + T.c.y*RFrame.mFullTransform._24 + T.c.z*RFrame.mFullTransform._34 + RFrame.mFullTransform._44;
    if (w<0) return; // culling

	float s = w*sz;
								RFrame.mFullTransform.transform(c,T.c);
    r.mul(T.i,s); r.add(T.c); 	RFrame.mFullTransform.transform(r);
    n.mul(T.j,s); n.add(T.c); 	RFrame.mFullTransform.transform(n);
    d.mul(T.k,s); d.add(T.c); 	RFrame.mFullTransform.transform(d);
	c.x = (float)iFloor(_x2real(c.x)); c.y = (float)iFloor(_y2real(-c.y));
    r.x = (float)iFloor(_x2real(r.x)); r.y = (float)iFloor(_y2real(-r.y));
    n.x = (float)iFloor(_x2real(n.x)); n.y = (float)iFloor(_y2real(-n.y));
    d.x = (float)iFloor(_x2real(d.x)); d.y = (float)iFloor(_y2real(-d.y));

    u32 vBase;
	FVF::TL* pv	= (FVF::TL*)Stream->Lock(6,EVertexFormat::TL,vBase);
	pv->set	(c.x,c.y,0,1, 0xFF222222, 0,0); 				pv++;
	pv->set	(d.x,d.y,0,1, sel?0xFF0000FF:0xFF000080, 0,0); 	pv++;
	pv->set	(c.x,c.y,0,1, 0xFF222222, 0,0); 				pv++;
	pv->set	(r.x,r.y,0,1, sel?0xFFFF0000:0xFF800000, 0,0); 	pv++;
	pv->set	(c.x,c.y,0,1, 0xFF222222, 0,0); 				pv++;
	pv->set	(n.x,n.y,0,1, sel?0xFF00FF00:0xFF008000, 0,0);
	Stream->Unlock(6);

	// Render it as line list
	DU_DRAW_RS	(D3DRS_SHADEMODE,D3DSHADE_GOURAUD);
	DU_DRAW_SH(dxRenderDeviceRender::Instance().m_WireShader);
    DU_DRAW_DP		(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase,3);
	DU_DRAW_RS	(D3DRS_SHADEMODE,SHADE_MODE);

    m_Font->SetColor(sel?0xFF000000:0xFF909090);
    m_Font->Out(r.x,r.y,"x");
    m_Font->Out(n.x,n.y,"y");
    m_Font->Out(d.x,d.y,"z");
    m_Font->SetColor(sel?0xFFFFFFFF:0xFF000000);
    m_Font->Out(r.x-1,r.y-1,"x");
    m_Font->Out(n.x-1,n.y-1,"y");
    m_Font->Out(d.x-1,d.y-1,"z");
}

void CDrawUtilities::DrawGrid()
{
	VERIFY( Device.b_is_Ready );
	DrawRecorder*	Stream	= &Recorder;
    u32 vBase;
	// fill VB
	FVF::L*	pv	= (FVF::L*)Stream->Lock((u32)m_GridPoints.size(),EVertexFormat::L,vBase);
    for (FLvertexIt v_it=m_GridPoints.begin(); v_it!=m_GridPoints.end(); v_it++,pv++) pv->set(*v_it);
	Stream->Unlock((u32)m_GridPoints.size());
	// Render it as triangle list
    Fmatrix ddd;
    ddd.identity();
    Recorder.SetWorld(ddd);
	DU_DRAW_SH(dxRenderDeviceRender::Instance().m_WireShader);
    DU_DRAW_DP(ERHI_PRIMITIVE_TOPOLOGY::LINE_LIST,RingGeom,vBase, (u32)m_GridPoints.size()/2);
}

void CDrawUtilities::DrawSelectionRect(const Ivector2& m_SelStart, const Ivector2& m_SelEnd){
	VERIFY( Device.b_is_Ready );
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
    u32 vBase;
	FVF::TL* pv	= (FVF::TL*)Stream->Lock(6,EVertexFormat::TL,vBase);
    pv->set(m_SelStart.x*SCREEN_QUALITY, m_SelStart.y*SCREEN_QUALITY, m_SelectionRect,0.f,0.f); pv++;
    pv->set(m_SelStart.x*SCREEN_QUALITY, m_SelEnd.y*SCREEN_QUALITY,   m_SelectionRect,0.f,0.f); pv++;
    pv->set(m_SelEnd.x*SCREEN_QUALITY,   m_SelEnd.y*SCREEN_QUALITY,   m_SelectionRect,0.f,0.f); pv++;
    pv->set(m_SelEnd.x*SCREEN_QUALITY,   m_SelStart.y*SCREEN_QUALITY, m_SelectionRect,0.f,0.f); pv++;
	FanToList(pv-4,2);
	Stream->Unlock(6);
	// Render it as triangle list
    Recorder.SetCull(ERHI_CULLMODE::NONE);
	DU_DRAW_SH(dxRenderDeviceRender::Instance().m_SelectionShader);
    DU_DRAW_DP(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN,RingGeom,vBase,2);
    Recorder.SetCull(ERHI_CULLMODE::BACK);
}

void CDrawUtilities::DrawPrimitiveL	(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, Fvector* vertices, int vc, u32 color, bool bCull, bool bCycle)
{
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase, dwNeed=(bCycle)?vc+1:vc, dwFan=(pt==ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN)?pc*3:0;
	FVF::L*	pv	= (FVF::L*)Stream->Lock(std::max(dwNeed,dwFan),EVertexFormat::L,vBase);
    for(int k=0; k<vc; k++,pv++)
    	pv->set		(vertices[k],color);
    if (bCycle)		pv->set(*(pv-vc));
    if (dwFan)		FanToList(pv-vc,pc);
	Stream->Unlock(dwFan?dwFan:dwNeed);

    if (!bCull) Recorder.SetCull(ERHI_CULLMODE::NONE);
    DU_DRAW_DP		(pt,RingGeom,vBase,pc);
    if (!bCull) Recorder.SetCull(ERHI_CULLMODE::BACK);
}

void CDrawUtilities::DrawPrimitiveTL(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, FVF::TL* vertices, int vc, bool bCull, bool bCycle)
{
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase, dwNeed=(bCycle)?vc+1:vc, dwFan=(pt==ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN)?pc*3:0;
	FVF::TL* pv		= (FVF::TL*)Stream->Lock(std::max(dwNeed,dwFan),EVertexFormat::TL,vBase);
    for(int k=0; k<vc; k++,pv++)
    	pv->set		(vertices[k]);
    if (bCycle)		pv->set(*(pv-vc));
    if (dwFan)		FanToList(pv-vc,pc);
	Stream->Unlock(dwFan?dwFan:dwNeed);

    if (!bCull) 	Recorder.SetCull(ERHI_CULLMODE::NONE);
    DU_DRAW_DP		(pt,RingGeom,vBase,pc);
    if (!bCull) 	Recorder.SetCull(ERHI_CULLMODE::BACK);
}

void CDrawUtilities::DrawPrimitiveLIT(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, FVF::LIT* vertices, int vc, bool bCull, bool bCycle)
{
	// fill VB
	DrawRecorder*	Stream	= &Recorder;
	u32			vBase, dwNeed=(bCycle)?vc+1:vc, dwFan=(pt==ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN)?pc*3:0;
	FVF::LIT* 	pv	= (FVF::LIT*)Stream->Lock(std::max(dwNeed,dwFan),EVertexFormat::LIT,vBase);
    for(int k=0; k<vc; k++,pv++)
    	pv->set		(vertices[k]);
    if (bCycle)		pv->set(*(pv-vc));
    if (dwFan)		FanToList(pv-vc,pc);
	Stream->Unlock(dwFan?dwFan:dwNeed);

    if (!bCull) Recorder.SetCull(ERHI_CULLMODE::NONE);
    DU_DRAW_DP		(pt,RingGeom,vBase,pc);
    if (!bCull) Recorder.SetCull(ERHI_CULLMODE::BACK);
}

void CDrawUtilities::DrawLink(const Fvector& p0, const Fvector& p1, float sz, u32 clr)
{
	DrawLine(p1,p0,clr);
    Fvector pp[2],D,R,N={0,1,0};
    D.sub(p1,p0); D.normalize();
    R.crossproduct(N,D); R.mul(0.5f); D.mul(2.0f); N.mul(0.5f);
    // LR
	pp[0].add(R,D); pp[0].mul(sz*-0.5f);	pp[0].add(p1);
	R.invert();
	pp[1].add(R,D); pp[1].mul(sz*-0.5f);	pp[1].add(p1);
	DrawLine(p1,pp[0],clr);
 	DrawLine(p1,pp[1],clr);
    // UB
	pp[0].add(N,D); pp[0].mul(sz*-0.5f);	pp[0].add(p1);
    N.invert();
	pp[1].add(N,D); pp[1].mul(sz*-0.5f);	pp[1].add(p1);
    DrawLine(p1,pp[0],clr);
    DrawLine(p1,pp[1],clr);
}

void CDrawUtilities::DrawJoint(const Fvector& p, float radius, u32 clr)
{
	DrawLineSphere(p,radius,clr,false);
}

void CDrawUtilities::OnRender()
{
	m_Font->OnRender();
}

void CDrawUtilities::OutText(const Fvector& pos, const char* text, u32 color, u32 shadow_color)
{
	Fvector p;
	float w	= pos.x*RFrame.mFullTransform._14 + pos.y*RFrame.mFullTransform._24 + pos.z*RFrame.mFullTransform._34 + RFrame.mFullTransform._44;
	if (w>=0){
		RFrame.mFullTransform.transform(p,pos);
		p.x = (float)iFloor(_x2real(p.x)); p.y = (float)iFloor(_y2real(-p.y));

		m_Font->SetColor(shadow_color);
		m_Font->Out(p.x, p.y, (char*)text);
		m_Font->SetColor(color);
		m_Font->Out(p.x - 1, p.y - 1, (char*)text);
	}
}
