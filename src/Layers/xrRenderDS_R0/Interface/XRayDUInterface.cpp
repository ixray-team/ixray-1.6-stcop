#include "stdafx.h"

CDS0_DUInterface::CDS0_DUInterface()
{
}

void  CDS0_DUInterface::DrawCross(const Fvector& p, float szx1, float szy1, float szz1, float szx2, float szy2, float szz2, u32 clr, BOOL bRot45)
{
}

void  CDS0_DUInterface::DrawCross(const Fvector& p, float sz, u32 clr, BOOL bRot45)
{
}

void  CDS0_DUInterface::DrawFlag(const Fvector& p, float heading, float height, float sz, float sz_fl, u32 clr, BOOL bDrawEntity)
{
}

void  CDS0_DUInterface::DrawRomboid(const Fvector& p, float radius, u32 clr)
{
}

void  CDS0_DUInterface::DrawJoint(const Fvector& p, float radius, u32 clr)
{
}

void  CDS0_DUInterface::DrawSpotLight(const Fvector& p, const Fvector& d, float range, float phi, u32 clr)
{
}

void  CDS0_DUInterface::DrawDirectionalLight(const Fvector& p, const Fvector& d, float radius, float range, u32 clr)
{
}

void  CDS0_DUInterface::DrawPointLight(const Fvector& p, float radius, u32 clr)
{
}

void  CDS0_DUInterface::DrawSound(const Fvector& p, float radius, u32 clr)
{
}

void  CDS0_DUInterface::DrawLineSphere(const Fvector& p, float radius, u32 clr, BOOL bCross)
{
}

void  CDS0_DUInterface::dbgDrawPlacement(const Fvector& p, int sz, u32 clr, LPCSTR caption, u32 clr_font)
{
}

void  CDS0_DUInterface::dbgDrawVert(const Fvector& p0, u32 clr, LPCSTR caption)
{
}

void  CDS0_DUInterface::dbgDrawEdge(const Fvector& p0, const Fvector& p1, u32 clr, LPCSTR caption)
{
}

void  CDS0_DUInterface::dbgDrawFace(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr, LPCSTR caption)
{
}

void  CDS0_DUInterface::DrawFace(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr_s, u32 clr_w, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawLine(const Fvector& p0, const Fvector& p1, u32 clr)
{
}

void  CDS0_DUInterface::DrawLink(const Fvector& p0, const Fvector& p1, float sz, u32 clr)
{
}

void  CDS0_DUInterface::DrawFaceNormal(const Fvector& p0, const Fvector& p1, const Fvector& p2, float size, u32 clr)
{
}

void  CDS0_DUInterface::DrawFaceNormal(const Fvector* p, float size, u32 clr)
{
}

void  CDS0_DUInterface::DrawFaceNormal(const Fvector& C, const Fvector& N, float size, u32 clr)
{
}

void  CDS0_DUInterface::DrawSelectionBox(const Fvector& center, const Fvector& size, u32* c)
{
}

void  CDS0_DUInterface::DrawSelectionBoxB(const Fbox& box, u32* c)
{
}

void  CDS0_DUInterface::DrawIdentSphere(BOOL bSolid, BOOL bWire, u32 clr_s, u32 clr_w)
{
}

void  CDS0_DUInterface::DrawIdentSpherePart(BOOL bSolid, BOOL bWire, u32 clr_s, u32 clr_w)
{
}

void  CDS0_DUInterface::DrawIdentCone(BOOL bSolid, BOOL bWire, u32 clr_s, u32 clr_w)
{
}

void  CDS0_DUInterface::DrawIdentCylinder(BOOL bSolid, BOOL bWire, u32 clr_s, u32 clr_w)
{
}

void  CDS0_DUInterface::DrawIdentBox(BOOL bSolid, BOOL bWire, u32 clr_s, u32 clr_w)
{
}

void  CDS0_DUInterface::DrawBox(const Fvector& offs, const Fvector& Size, BOOL bSolid, BOOL bWire, u32 clr_s, u32 clr_w)
{
}

void  CDS0_DUInterface::DrawAABB(const Fvector& p0, const Fvector& p1, u32 clr_s, u32 clr_w, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawAABB(const Fmatrix& parent, const Fvector& center, const Fvector& size, u32 clr_s, u32 clr_w, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawOBB(const Fmatrix& parent, const Fobb& box, u32 clr_s, u32 clr_w)
{
}

void  CDS0_DUInterface::DrawSphere(const Fmatrix& parent, const Fvector& center, float radius, u32 clr_s, u32 clr_w, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawSphere(const Fmatrix& parent, const Fsphere& S, u32 clr_s, u32 clr_w, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawCylinder(const Fmatrix& parent, const Fvector& center, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawCone(const Fmatrix& parent, const Fvector& apex, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawPlane(const Fvector& center, const Fvector2& scale, const Fvector& rotate, u32 clr_s, u32 clr_w, BOOL bCull, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawPlane(const Fvector& p, const Fvector& n, const Fvector2& scale, u32 clr_s, u32 clr_w, BOOL bCull, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawRectangle(const Fvector& o, const Fvector& u, const Fvector& v, u32 clr_s, u32 clr_w, BOOL bSolid, BOOL bWire)
{
}

void  CDS0_DUInterface::DrawGrid()
{
}

void  CDS0_DUInterface::DrawPivot(const Fvector& pos, float sz)
{
}

void  CDS0_DUInterface::DrawAxis(const Fmatrix& T)
{
}

void  CDS0_DUInterface::DrawObjectAxis(const Fmatrix& T, float sz, BOOL sel)
{
}

void  CDS0_DUInterface::DrawSelectionRect(const Ivector2& m_SelStart, const Ivector2& m_SelEnd)
{
}

void CDS0_DUInterface::DrawIndexedPrimitive(int prim_type, u32 pc, const Fvector& pos, const Fvector* vb, const u32& vb_size, const u32* ib, const u32& ib_size, const u32& clr_argb, float scale)
{
}



void  CDS0_DUInterface::OutText(const Fvector& pos, LPCSTR text, u32 color, u32 shadow_color)
{
}

void  CDS0_DUInterface::OnDeviceDestroy()
{
}
