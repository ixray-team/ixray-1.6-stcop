#include "stdafx.h"
#include "dxThunderboltRender.h"

#include "../../xrEngine/thunderbolt.h"
#include "dxThunderboltDescRender.h"
#include "dxLensFlareRender.h"

dxThunderboltRender::dxThunderboltRender()
{
	hGeom_model.create(EGeomSource::Ring, 0, RImplementation.Imm().Index.Buffer());
	hGeom_gradient.create(EGeomSource::Ring);
}

dxThunderboltRender::~dxThunderboltRender()
{
	hGeom_model.destroy();
	hGeom_gradient.destroy();
}

void dxThunderboltRender::Copy(IThunderboltRender &_in)
{
	*this = *(dxThunderboltRender*)&_in;
}

void dxThunderboltRender::Render(CEffect_Thunderbolt &owner)
{
	VERIFY(owner.current);
	GfxCmdList& Cmd = RImplementation.Imm();

	// lightning model
	float dv = owner.lightning_phase*0.5f;
	dv = (owner.lightning_phase>0.5f)?Random.randI(2)*0.5f:dv;

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	u32 v_offset,i_offset;
	
	dxThunderboltDescRender *pThRen = (dxThunderboltDescRender*)&*owner.current->m_pRender;

	u32 vCount_Lock = pThRen->l_model->number_vertices;
	u32 iCount_Lock = pThRen->l_model->number_indices;
	IRender_DetailModel::fvfVertexOut* v_ptr = (IRender_DetailModel::fvfVertexOut*)Cmd.Vertex.Lock(vCount_Lock, EVertexFormat::LIT, v_offset);
	u16* i_ptr = Cmd.Index.Lock(iCount_Lock, i_offset);
	// XForm verts
	pThRen->l_model->transfer(owner.current_xform,v_ptr,0xffffffff,i_ptr,0,0.f,dv);
	// Flush if needed
	Cmd.Vertex.Unlock(vCount_Lock);
	Cmd.Index.Unlock(iCount_Lock);
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetPass(pThRen->l_model->shader);
	Cmd.SetGeometry(hGeom_model);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, iCount_Lock, i_offset, v_offset);
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);

	// gradient
	Fvector vecSx, vecSy;
	u32 VS_Offset;
	struct LITF
	{
		struct
		{
			Fvector p; u32 color; Fvector2 t;
		} buff[4];
	};
	LITF* pv = (LITF*)Cmd.Vertex.Lock(8, EVertexFormat::LIT, VS_Offset);
	// top
	{
		u32 c_val = iFloor(owner.current->m_GradientTop->fOpacity*owner.lightning_phase*255.f);
		u32 c = color_rgba(c_val,c_val,c_val,c_val);
		vecSx.mul(Device.vCameraRight, owner.current->m_GradientTop->fRadius.x*owner.lightning_size);
		vecSy.mul(Device.vCameraTop, -owner.current->m_GradientTop->fRadius.y*owner.lightning_size);
		Fvector& curr_center = owner.current_xform.c;
		*pv =
		{
			curr_center + vecSx - vecSy, c, {0.f, 0.f},
			curr_center + vecSx + vecSy, c, {0.f, 1.f},
			curr_center - vecSx - vecSy, c, {1.f, 0.f},
			curr_center - vecSx + vecSy, c, {1.f, 1.f}
		};
		pv++;
	}
	// center
	{
		u32 c_val = iFloor(owner.current->m_GradientTop->fOpacity*owner.lightning_phase*255.f);
		u32 c = color_rgba(c_val,c_val,c_val,c_val);
		vecSx.mul(Device.vCameraRight, owner.current->m_GradientCenter->fRadius.x*owner.lightning_size);
		vecSy.mul(Device.vCameraTop, -owner.current->m_GradientCenter->fRadius.y*owner.lightning_size);
		Fvector& curr_center = owner.lightning_center;
		*pv =
		{
			curr_center + vecSx - vecSy, c, {0.f, 0.f},
			curr_center + vecSx + vecSy, c, {0.f, 1.f},
			curr_center - vecSx - vecSy, c, {1.f, 0.f},
			curr_center - vecSx + vecSy, c, {1.f, 1.f}
		};
		pv++;
	}
	Cmd.Vertex.Unlock(8);
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetGeometry(hGeom_gradient);
	Cmd.SetPass(((dxFlareRender*)&*owner.current->m_GradientTop->m_pFlare)->hShader);

	//	Hack. Since lightning gradient uses sun shader override z write settings manually
	Cmd.SetDepthEnable(true);
	Cmd.SetDepthFunc(D3DCMP_LESSEQUAL);

	Cmd.DrawQuads(1, VS_Offset);
	Cmd.SetPass(((dxFlareRender*)&*owner.current->m_GradientCenter->m_pFlare)->hShader);

	//	Hack. Since lightning gradient uses sun shader override z write settings manually
	Cmd.SetDepthEnable(true);
	Cmd.SetDepthFunc(D3DCMP_LESSEQUAL);

	Cmd.DrawQuads(1, VS_Offset + 4);
}