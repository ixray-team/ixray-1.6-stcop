#pragma once
class CDS0_DUInterface :public CDUInterface
{
public:
	CDS0_DUInterface();
	//----------------------------------------------------
	virtual void  DrawCross(const Fvector& p, float szx1, float szy1, float szz1, float szx2, float szy2, float szz2, u32 clr, bool bRot45 = false) override;
	virtual void  DrawCross(const Fvector& p, float sz, u32 clr, bool bRot45 = false) override;
	virtual void  DrawFlag(const Fvector& p, float heading, float height, float sz, float sz_fl, u32 clr, bool bDrawEntity);
	virtual void  DrawRomboid(const Fvector& p, float radius, u32 clr) override;
	virtual void  DrawJoint(const Fvector& p, float radius, u32 clr) override;

	virtual void  DrawSpotLight(const Fvector& p, const Fvector& d, float range, float phi, u32 clr) override;
	virtual void  DrawDirectionalLight(const Fvector& p, const Fvector& d, float radius, float range, u32 clr) override;
	virtual void  DrawPointLight(const Fvector& p, float radius, u32 clr) override;

	virtual void  DrawSound(const Fvector& p, float radius, u32 clr) override;
	virtual void  DrawLineSphere(const Fvector& p, float radius, u32 clr, bool bCross) override;

	virtual void  dbgDrawPlacement(const Fvector& p, int sz, u32 clr, str_c caption = nullptr, u32 clr_font = 0xffffffff) override;
	virtual void  dbgDrawVert(const Fvector& p0, u32 clr, str_c caption = nullptr) override;
	virtual void  dbgDrawEdge(const Fvector& p0, const Fvector& p1, u32 clr, str_c caption = nullptr) override;
	virtual void  dbgDrawFace(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr, str_c caption = nullptr) override;

	virtual void  DrawFace(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void  DrawLine(const Fvector& p0, const Fvector& p1, u32 clr) override;
	virtual void  DrawLink(const Fvector& p0, const Fvector& p1, float sz, u32 clr) override;
	virtual void  DrawFaceNormal(const Fvector& p0, const Fvector& p1, const Fvector& p2, float size, u32 clr) override;
	virtual void  DrawFaceNormal(const Fvector* p, float size, u32 clr) override;
	virtual void  DrawFaceNormal(const Fvector& C, const Fvector& N, float size, u32 clr) override;
	virtual void  DrawSelectionBox(const Fvector& center, const Fvector& size, u32* c = nullptr) override;
	virtual void  DrawSelectionBoxB(const Fbox& box, u32* c = nullptr) override;
	virtual void  DrawIdentSphere(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void  DrawIdentSpherePart(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void  DrawIdentCone(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void  DrawIdentCylinder(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void  DrawIdentBox(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;

	virtual void  DrawBox(const Fvector& offs, const Fvector& Size, bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void  DrawAABB(const Fvector& p0, const Fvector& p1, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void  DrawAABB(const Fmatrix& parent, const Fvector& center, const Fvector& size, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void  DrawOBB(const Fmatrix& parent, const Fobb& box, u32 clr_s, u32 clr_w) override;
	virtual void  DrawSphere(const Fmatrix& parent, const Fvector& center, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void  DrawSphere(const Fmatrix& parent, const Fsphere& S, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void  DrawCylinder(const Fmatrix& parent, const Fvector& center, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void  DrawCone(const Fmatrix& parent, const Fvector& apex, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void  DrawPlane(const Fvector& center, const Fvector2& scale, const Fvector& rotate, u32 clr_s, u32 clr_w, bool bCull, bool bSolid, bool bWire) override;
	virtual void  DrawPlane(const Fvector& p, const Fvector& n, const Fvector2& scale, u32 clr_s, u32 clr_w, bool bCull, bool bSolid, bool bWire) override;
	virtual void  DrawRectangle(const Fvector& o, const Fvector& u, const Fvector& v, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;

	virtual void  DrawGrid() override;
	virtual void  DrawPivot(const Fvector& pos, float sz = 5.f) override;
	virtual void  DrawAxis(const Fmatrix& T) override;
	virtual void  DrawObjectAxis(const Fmatrix& T, float sz, bool sel) override;
	virtual void  DrawSelectionRect(const Ivector2& m_SelStart, const Ivector2& m_SelEnd) override;
	virtual void  DrawIndexedPrimitive(ERHI_PRIMITIVE_TOPOLOGY prim_type, u32 pc, const Fvector& pos, const Fvector* vb, const u32& vb_size, const u32* ib, const u32& ib_size, const u32& clr_argb, float scale = 1.0f) override;

	virtual void  OutText(const Fvector& pos, str_c text, u32 color = 0xFF000000, u32 shadow_color = 0xFF909090) override;

	virtual void  OnDeviceDestroy() override;
};