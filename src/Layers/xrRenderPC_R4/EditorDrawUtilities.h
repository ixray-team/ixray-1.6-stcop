#pragma once

#include "RenderAPI.h"
#include "EditorDrawContext.h"
#include "../../Include/xrRender/DrawUtils.h"

// Editor implementation of CDUInterface. Unlike the game CDrawUtilities it draws through
// GEditorDraw (current shader passes, fill mode) and batches lines until FlushDU().

struct RENDER_API SEditorPrimitiveBuffer
{
	ref_geom pGeom;
	u32 v_cnt = 0;
	u32 i_cnt = 0;
	ERHI_PRIMITIVE_TOPOLOGY p_type = ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST;
	u32 p_cnt = 0;
	using TOnRender = xr_delegate<void()>;
	TOnRender OnRender;

	void RenderDIP()
	{
		GEditorDraw.DIP(p_type, pGeom, 0, 0, v_cnt, 0, p_cnt);
	}

	void RenderDP()
	{
		GEditorDraw.DP(p_type, pGeom, 0, p_cnt);
	}

	void CreateFromData(ERHI_PRIMITIVE_TOPOLOGY _pt, u32 _p_cnt, u32 FVF, LPVOID vertices, u32 _v_cnt, u16* indices = 0, u32 _i_cnt = 0);
	void Destroy();
	void Render()
	{
		OnRender();
	}
};

class RENDER_API CEditorDrawUtilities :
	public CDUInterface,
	public pureRender
{
	SEditorPrimitiveBuffer m_SolidCone;
	SEditorPrimitiveBuffer m_WireCone;
	SEditorPrimitiveBuffer m_SolidSphere;
	SEditorPrimitiveBuffer m_WireSphere;
	SEditorPrimitiveBuffer m_SolidSpherePart;
	SEditorPrimitiveBuffer m_WireSpherePart;
	SEditorPrimitiveBuffer m_SolidCylinder;
	SEditorPrimitiveBuffer m_WireCylinder;
	SEditorPrimitiveBuffer m_SolidBox;
	SEditorPrimitiveBuffer m_WireBox;
	CGameFont* m_Font = nullptr;

public:
	ref_geom vs_L;
	ref_geom vs_TL;
	ref_geom vs_LIT;

protected:
	FVF::L* m_DD_pv = nullptr;
	FVF::L* m_DD_pv_start = nullptr;
	u32 m_DD_base = 0;
	bool m_DD_wire = false;
	void DD_DrawFace_flush(bool try_again);

public:
	void DD_DrawFace_begin(bool bWire);
	void DD_DrawFace_push(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr);
	void DD_DrawFace_end();

public:
	void OnDeviceCreate();
	virtual void OnDeviceDestroy() override;

	void UpdateGrid(int number_of_cell, float square_size, int subdiv = 10);
	//----------------------------------------------------
	virtual void DrawCross(const Fvector& p, float szx1, float szy1, float szz1, float szx2, float szy2, float szz2, u32 clr, bool bRot45 = false) override;

	virtual void DrawCross(const Fvector& p, float sz, u32 clr, bool bRot45 = false) override
	{
		DrawCross(p, sz, sz, sz, sz, sz, sz, clr, bRot45);
	}
	virtual void DrawEntity(u32 clr, ref_shader s);
	virtual void DrawRomboid(const Fvector& p, float radius, u32 clr) override;
	virtual void DrawJoint(const Fvector& p, float radius, u32 clr) override;

	virtual void DrawSpotLight(const Fvector& p, const Fvector& d, float range, float phi, u32 clr) override;
	virtual void DrawDirectionalLight(const Fvector& p, const Fvector& d, float radius, float range, u32 clr) override;
	virtual void DrawPointLight(const Fvector& p, float radius, u32 clr) override;

	virtual void DrawSound(const Fvector& p, float radius, u32 clr) override;
	virtual void DrawLineSphere(const Fvector& p, float radius, u32 clr, bool bCross) override;

	virtual void dbgDrawPlacement(const Fvector& p, int sz, u32 clr, const char* caption = 0, u32 clr_font = 0xffffffff) override;
	virtual void dbgDrawVert(const Fvector& p0, u32 clr, const char* caption = 0) override;
	virtual void dbgDrawEdge(const Fvector& p0, const Fvector& p1, u32 clr, const char* caption = 0) override;
	virtual void dbgDrawFace(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr, const char* caption = 0) override;

	virtual void DrawFace(const Fvector& p0, const Fvector& p1, const Fvector& p2, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void DrawLine(const Fvector& p0, const Fvector& p1, u32 clr) override;
	virtual void DrawLink(const Fvector& p0, const Fvector& p1, float sz, u32 clr) override;

	virtual void DrawFaceNormal(const Fvector& p0, const Fvector& p1, const Fvector& p2, float size, u32 clr) override
	{
		Fvector N, C, P;
		N.mknormal(p0, p1, p2);
		C.set(p0);
		C.add(p1);
		C.add(p2);
		C.div(3);
		P.mad(C, N, size);
		DrawLine(C, P, clr);
	}

	virtual void DrawFaceNormal(const Fvector* p, float size, u32 clr) override
	{
		DrawFaceNormal(p[0], p[1], p[2], size, clr);
	}

	virtual void DrawFaceNormal(const Fvector& C, const Fvector& N, float size, u32 clr) override
	{
		Fvector P;
		P.mad(C, N, size);
		DrawLine(C, P, clr);
	}

	virtual void DrawSelectionBox(const Fvector& center, const Fvector& size, u32* c = 0) override;

	virtual void DrawSelectionBoxB(const Fbox& box, u32* c = 0) override
	{
		Fvector S, C;
		box.getsize(S);
		box.getcenter(C);
		DrawSelectionBox(C, S, c);
	}

	virtual void DrawIdentSphere(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void DrawIdentSpherePart(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void DrawIdentCone(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void DrawIdentCylinder(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void DrawIdentBox(bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;

	virtual void DrawBox(const Fvector& offs, const Fvector& Size, bool bSolid, bool bWire, u32 clr_s, u32 clr_w) override;
	virtual void DrawAABB(const Fvector& p0, const Fvector& p1, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void DrawAABB(const Fmatrix& parent, const Fvector& center, const Fvector& size, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void DrawOBB(const Fmatrix& parent, const Fobb& box, u32 clr_s, u32 clr_w) override;
	virtual void DrawSphere(const Fmatrix& parent, const Fvector& center, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;

	virtual void DrawSphere(const Fmatrix& parent, const Fsphere& S, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override
	{
		DrawSphere(parent, S.P, S.R, clr_s, clr_w, bSolid, bWire);
	}

	virtual void DrawCylinder(const Fmatrix& parent, const Fvector& center, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void DrawCone(const Fmatrix& parent, const Fvector& apex, const Fvector& dir, float height, float radius, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;
	virtual void DrawPlane(const Fvector& center, const Fvector2& scale, const Fvector& rotate, u32 clr_s, u32 clr_w, bool bCull, bool bSolid, bool bWire) override;
	virtual void DrawPlane(const Fvector& p, const Fvector& n, const Fvector2& scale, u32 clr_s, u32 clr_w, bool bCull, bool bSolid, bool bWire) override;
	virtual void DrawRectangle(const Fvector& o, const Fvector& u, const Fvector& v, u32 clr_s, u32 clr_w, bool bSolid, bool bWire) override;

	virtual void DrawGrid() override;
	virtual void DrawPivot(const Fvector& pos, float sz = 5.f) override;
	virtual void DrawAxis(const Fmatrix& T) override;
	virtual void DrawObjectAxis(const Fmatrix& T, float sz, bool sel) override;
	virtual void DrawSelectionRect(const Ivector2& m_SelStart, const Ivector2& m_SelEnd) override;

	virtual void DrawIndexedPrimitive(ERHI_PRIMITIVE_TOPOLOGY prim_type, u32 pc, const Fvector& pos, const Fvector* vb, const u32& vb_size, const u32* ib, const u32& ib_size, const u32& clr_argb, float scale = 1.0f) override;
	virtual void DrawPrimitiveL(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, Fvector* vertices, int vc, u32 color, bool bCull, bool bCycle);
	virtual void DrawPrimitiveTL(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, FVF::TL* vertices, int vc, bool bCull, bool bCycle);
	virtual void DrawPrimitiveLIT(ERHI_PRIMITIVE_TOPOLOGY pt, u32 pc, FVF::LIT* vertices, int vc, bool bCull, bool bCycle);

	virtual void OutText(const Fvector& pos, const char* text, u32 color = 0xFF000000, u32 shadow_color = 0xFF909090) override;

	virtual void OnRender() override;
};

extern RENDER_API CEditorDrawUtilities DU_impl;

// Batched line helpers; FlushDU() submits everything collected since the last flush.
RENDER_API void AddCross(const Fvector& p, float szx1, float szy1, float szz1, float szx2, float szy2, float szz2, u32 clr, bool bRot45 = false);
RENDER_API void AddLine(const Fvector& p0, const Fvector& p1, u32 c);
RENDER_API void FlushDU();
