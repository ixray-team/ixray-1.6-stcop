//----------------------------------------------------
// file: D3DUtils.h
//----------------------------------------------------

#ifndef D3DUtilsH
#define D3DUtilsH

#include "RenderAPI.h"
#include "GeometryHeap.h"
#include "../../Include/xrRender/DrawUtils.h"
//----------------------------------------------------

namespace DebugDraw
{
	void Primitive(GfxCmdList& Cmd, ERHI_PRIMITIVE_TOPOLOGY Topology, ref_geom Geom, u32 BaseVertex, u32 PrimitiveCount);
}

#define DU_DRAW_DIP(Topology, Geom, BaseVertex, StartVertex, VertexCount, StartIndex, PrimitiveCount) \
	{ RImplementation.Imm().SetGeometry(Geom); RImplementation.Imm().DrawIndexed(Topology, RHITopologyUtils::GetIndexCount(PrimitiveCount, Topology), StartIndex, BaseVertex); }
#define DU_DRAW_DP(Topology, Geom, BaseVertex, PrimitiveCount) DebugDraw::Primitive(RImplementation.Imm(), Topology, Geom, BaseVertex, PrimitiveCount)

struct SPrimitiveBuffer{
    ref_geom				pGeom;
    GeometryAlloc			Alloc = {};
    ERHI_PRIMITIVE_TOPOLOGY 	p_type;
    u32						p_cnt;
public:
    void					CreateFromData(ERHI_PRIMITIVE_TOPOLOGY _pt, u32 _p_cnt, const Fvector* vertices, u32 v_cnt, const u16* indices, u32 i_cnt);
    void					Destroy();
    void					Render();
};

//----------------------------------------------------
// Utilities
//----------------------------------------------------
class RENDER_API CDrawUtilities: 
    public CDUInterface, 
    public pureRender
{
	SPrimitiveBuffer		m_SolidCone;
	SPrimitiveBuffer		m_WireCone;
    SPrimitiveBuffer		m_SolidSphere;
    SPrimitiveBuffer		m_WireSphere;
    SPrimitiveBuffer		m_SolidSpherePart;
    SPrimitiveBuffer		m_WireSpherePart;
    SPrimitiveBuffer		m_SolidCylinder;
    SPrimitiveBuffer		m_WireCylinder;
    SPrimitiveBuffer		m_SolidBox;
    SPrimitiveBuffer		m_WireBox;
	CGameFont* 				m_Font;
public:
	ref_geom 	RingGeom;
protected:
    FVF::L*		m_DD_pv;
    FVF::L*		m_DD_pv_start;
    u32 		m_DD_base;
    bool		m_DD_wire;
	void 		DD_DrawFace_flush		(bool try_again);
public:
	void 		DD_DrawFace_begin		(bool bWire);
	void 		DD_DrawFace_push		(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr);
	void 		DD_DrawFace_end			();
public:
	CDrawUtilities()
    {
        m_DD_pv			= 0;
        m_DD_pv_start	= 0;
        m_DD_base		= 0;
		m_Font	= 0;
    }
    
    void OnDeviceCreate	();
    virtual void  OnDeviceDestroy();

	void UpdateGrid(int number_of_cell, float square_size, int subdiv=10);
    //----------------------------------------------------
    virtual void  DrawCross(const Fvector& p, float szx1, float szy1, float szz1, float szx2, float szy2, float szz2, u32 clr, bool bRot45=false);
    virtual void  DrawCross(const Fvector& p, float sz, u32 clr, bool bRot45=false){ DrawCross(p, sz,sz,sz, sz,sz,sz, clr, bRot45); }
    virtual void  DrawEntity(u32 clr, ref_shader s);
    virtual void  DrawRomboid(const Fvector& p, float radius, u32 clr);
	virtual void  DrawJoint(const Fvector& p, float radius, u32 clr);

    virtual void  DrawSpotLight(const Fvector& p, const Fvector& d, float range, float phi, u32 clr);
    virtual void  DrawDirectionalLight(const Fvector& p, const Fvector& d, float radius, float range, u32 clr);
    virtual void  DrawPointLight(const Fvector& p, float radius, u32 clr);

    virtual void  DrawSound(const Fvector& p, float radius, u32 clr);
    virtual void  DrawLineSphere(const Fvector& p, float radius, u32 clr, bool bCross);

	virtual void  dbgDrawPlacement(const Fvector& p, int sz, u32 clr, const char* caption=0, u32 clr_font=0xffffffff);
    virtual void  dbgDrawVert(const Fvector& p0, u32 clr, const char* caption=0);
    virtual void  dbgDrawEdge(const Fvector& p0,	const Fvector& p1, u32 clr, const char* caption=0);
    virtual void  dbgDrawFace(const Fvector& p0,	const Fvector& p1, const Fvector& p2, u32 clr, const char* caption=0);

    virtual void  DrawFace(const Fvector& p0,	const Fvector& p1, const Fvector& p2, u32 clr_s, u32 clr_w, bool bSolid, bool bWire);
    virtual void  DrawLine(const Fvector& p0,	const Fvector& p1, u32 clr);
    virtual void  DrawLink(const Fvector& p0, const Fvector& p1, float sz, u32 clr);
    IC virtual void  DrawFaceNormal(const Fvector& p0, const Fvector& p1, const Fvector& p2, float size, u32 clr){
        Fvector N,C,P; N.mknormal(p0,p1,p2); C.set(p0);C.add(p1);C.add(p2);C.div(3);
        P.mad(C,N,size);
        DrawLine(C,P,clr);}
    IC virtual void  DrawFaceNormal(const Fvector* p, float size, u32 clr){DrawFaceNormal(p[0],p[1],p[2],size,clr);}
    IC virtual void  DrawFaceNormal(const Fvector& C, const Fvector& N, float size, u32 clr){
        Fvector P; P.mad(C,N,size);DrawLine(C,P,clr);}
    virtual void  DrawSelectionBox(const Fvector& center, const Fvector& size, u32* c=0);
    IC virtual void  DrawSelectionBoxB(const Fbox& box, u32* c=0){
        Fvector S,C;
        box.getsize(S);
        box.getcenter(C);
        DrawSelectionBox(C,S,c);
    }
    virtual void  DrawIdentSphere	(bool bSolid, bool bWire, u32 clr_s, u32 clr_w);
    virtual void  DrawIdentSpherePart(bool bSolid, bool bWire, u32 clr_s, u32 clr_w);
    virtual void  DrawIdentCone		(bool bSolid, bool bWire, u32 clr_s, u32 clr_w);
	virtual void  DrawIdentCylinder	(bool bSolid, bool bWire, u32 clr_s, u32 clr_w);
    virtual void  DrawIdentBox		(bool bSolid, bool bWire, u32 clr_s, u32 clr_w);

    virtual void  DrawBox	(const Fvector& offs, const Fvector& Size, bool bSolid, bool bWire, u32 clr_s, u32 clr_w);
    virtual void  DrawAABB	(const Fvector& p0, const Fvector& p1, u32 clr_s, u32 clr_w, bool bSolid, bool bWire);
    virtual void  DrawAABB	(const Fmatrix& parent, const Fvector& center, const Fvector& size, u32 clr_s, u32 clr_w, bool bSolid, bool bWire);
    virtual void  DrawOBB	(const Fmatrix& parent, const Fobb& box, u32 clr_s, u32 clr_w);
	virtual void  DrawSphere	(const Fmatrix& parent, const Fvector& center, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire);
	virtual void  DrawSphere	(const Fmatrix& parent, const Fsphere& S, u32 clr_s, u32 clr_w, bool bSolid, bool bWire){DrawSphere(parent,S.P,S.R,clr_s,clr_w,bSolid,bWire);}
	virtual void  DrawCylinder(const Fmatrix& parent, const Fvector& center, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire);
	virtual void  DrawCone	(const Fmatrix& parent, const Fvector& apex, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire);
    virtual void  DrawPlane	(const Fvector& center, const Fvector2& scale, const Fvector& rotate, u32 clr_s, u32 clr_w, bool bCull, bool bSolid, bool bWire);
    virtual void  DrawPlane	(const Fvector& p, const Fvector& n, const Fvector2& scale, u32 clr_s, u32 clr_w, bool bCull, bool bSolid, bool bWire);
    virtual void  DrawRectangle(const Fvector& o, const Fvector& u, const Fvector& v, u32 clr_s, u32 clr_w, bool bSolid, bool bWire);

    virtual void  DrawGrid	();
    virtual void  DrawPivot	(const Fvector& pos, float sz=5.f);
	virtual void  DrawAxis	(const Fmatrix& T);
	virtual void  DrawObjectAxis(const Fmatrix& T, float sz, bool sel);
	virtual void  DrawSelectionRect(const Ivector2& m_SelStart, const Ivector2& m_SelEnd);

    virtual void  DrawIndexedPrimitive(ERHI_PRIMITIVE_TOPOLOGY prim_type, u32 pc, const Fvector& pos, const Fvector* vb, const u32& vb_size, const u32* ib, const u32& ib_size, const u32& clr_argb, float scale = 1.0f)
    {}
    virtual void  DrawPrimitiveL(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, Fvector* vertices, int vc, u32 color, bool bCull, bool bCycle);
    virtual void  DrawPrimitiveTL(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, FVF::TL* vertices, int vc, bool bCull, bool bCycle);
    virtual void  DrawPrimitiveLIT(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, FVF::LIT* vertices, int vc, bool bCull, bool bCycle);

    virtual void  OutText	(const Fvector& pos, const char* text, u32 color=0xFF000000, u32 shadow_color=0xFF909090);

	virtual void 			OnRender		();
};
extern RENDER_API CDrawUtilities DUImpl;
//----------------------------------------------------
#endif /*_INCDEF_D3DUtils_H_*/