

#include "stdafx.h"
#include "XRayTreeVisual.h"
#include "XRayRenderVisual.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/Environment.h"
constexpr int		FTreeVisual_tile = 16;
constexpr int		FTreeVisual_quant = 32768 / FTreeVisual_tile;

//
// void XRayTreeVisual::UpdateUniform(XRayUniformAllocator::EUniformType Type, void* ptr)
// {
// 	if (Type == XRayUniformAllocator::UT_Tree)
// 	{
// 		SUniformBuffer* Buffer = (SUniformBuffer*)ptr;
// 		{
// 			static FTreeVisual_setup	tvs;
// 			if (tvs.dwFrame != Device->dwFrame)	tvs.calculate();
// 			// setup constants
//
// 			Fmatrix xform_v;
// 			xform_v.mul_43(GRenderInterface.GetWorld(), xform);
// 			Buffer->xform_v = xform_v;
// 			Buffer->xform = xform;
//
// 		
//
// 			Buffer->consts.set(tvs.scale, tvs.scale,0,0);									// consts/scale
// 			Buffer->wave.set(tvs.wave);													// wave
// 			Buffer->wind.set(tvs.wind);													// wind
//
// 			float s = XRayRenderConsole::ps_r_Tree_SBC;
// #if 0
// 			s *= 1.3333f;
// 			Buffer->c_scale.set(s * c_scale.rgb.x, s * c_scale.rgb.y, s * c_scale.rgb.z, s * c_scale.hemi);
// 			Buffer->c_bias.set(s * c_bias.rgb.x, s * c_bias.rgb.y, s * c_bias.rgb.z, s * c_bias.hemi);
// #else
// 			IEnvDescriptor& desc = *g_pGamePersistent->Environment().CurrentEnv;
// 			Buffer->c_scale.set(s * c_scale.rgb.x, s * c_scale.rgb.y, s * c_scale.rgb.z, s * c_scale.hemi); // scale
// 			Buffer->c_bias.set(s * c_bias.rgb.x + desc.ambient.x, s * c_bias.rgb.y + desc.ambient.y,
// 				s * c_bias.rgb.z + desc.ambient.z, s * c_bias.hemi); // bias
// #endif
// 		Buffer->c_sun.set(s * c_scale.sun, s * c_bias.sun,0,0);
// 		}
// 	}
// }

void XRayTreeVisual::Load(LPCSTR N, IReader* data, u32 dwFlags)
{
	XRayRenderVisual::Load(N, data, dwFlags);
	R_ASSERT(data->find_chunk(OGF_GCONTAINER));
	{
		// verts
		u32 ID = data->r_u32();
		OffsetVertex = data->r_u32();
		CountVertex = data->r_u32();

		//R_ASSERT(VertexBuffer.empty());

		//VertexBuffer = GRenderInterface.GetVertexBuffer(ID);
		//FVF = GRenderInterface.GetVertexState(ID);


		ID = data->r_u32();
		OffsetIndex = data->r_u32();
		CountIndex = data->r_u32();

		//R_ASSERT(IndexBuffer.empty());

		//IndexBuffer = GRenderInterface.GetIndexBuffer(ID);
	}

	// load tree-def
	R_ASSERT(data->find_chunk(OGF_TREEDEF2));
	{
		data->r(&xform, sizeof(xform));
		data->r(&c_scale, sizeof(c_scale));	c_scale.rgb.mul(.5f);	c_scale.hemi *= .5f;	c_scale.sun *= .5f;
		data->r(&c_bias, sizeof(c_bias));	c_bias.rgb.mul(.5f);	c_bias.hemi *= .5f;	c_bias.sun *= .5f;
		//Msg				("hemi[%f / %f], sun[%f / %f]",c_scale.hemi,c_bias.hemi,c_scale.sun,c_bias.sun);
	}

}
#define PCOPY(a)	a = pFrom->a
void XRayTreeVisual::Copy(XRayRenderVisual* from)
{
	XRayRenderVisual::Copy(from);

	XRayTreeVisual* pFrom = dynamic_cast<XRayTreeVisual*> (from);
	PCOPY(xform);
	PCOPY(c_scale);
	PCOPY(c_bias);
	PCOPY(FVF);

	//PCOPY(VertexBuffer);
	PCOPY(OffsetVertex);
	PCOPY(CountVertex);

	//PCOPY(IndexBuffer);
	PCOPY(OffsetIndex);
	PCOPY(CountIndex);
}


XRayTreeVisual::XRayTreeVisual(void)
{
}

XRayTreeVisual::~XRayTreeVisual(void)
{
}

XRayTreeVisual_ST::XRayTreeVisual_ST(void)
{
}

XRayTreeVisual_ST::~XRayTreeVisual_ST(void)
{
}
//
// bool XRayTreeVisual_ST::Render(float LOD, EShaderElement SEType, XRayObjectRender& Item)
// {
// 	GRenderInterface.UpdateDescriptorHeap(Shader.E[0][SEType]);
// 	if (!Shader.E[0][SEType].Set(Item, FVF)) { return false; }
// 	Item.UseUniform[XRayUniformAllocator::UT_Transformation] = true;
// 	Item.UseUniform[XRayUniformAllocator::UT_Tree] = true;
// 	Item.GraphicsPipelineResource.VertexBuffer = VertexBuffer;
// 	Item.GraphicsPipelineResource.IndexBuffer = IndexBuffer;
// 	Item.GraphicsPipelineResource.CountIndex = CountIndex;
// 	Item.GraphicsPipelineResource.OffsetIndex = OffsetIndex;
// 	Item.GraphicsPipelineResource.OffsetVertex = OffsetVertex;
// 	Item.Visual = this;
// 	return Item.GraphicsPipelineResource.CountIndex;
// }


void XRayTreeVisual_ST::Load(LPCSTR N, IReader* data, u32 dwFlags)
{
	inherited::Load(N, data, dwFlags);

}

void XRayTreeVisual_ST::Copy(XRayRenderVisual* pFrom)
{
	inherited::Copy(pFrom);
}

XRayTreeVisual_PM::XRayTreeVisual_PM(void)
{
}

XRayTreeVisual_PM::~XRayTreeVisual_PM(void)
{
}
//
// bool XRayTreeVisual_PM::Render(float LOD, EShaderElement SEType, XRayObjectRender& Item)
// {
// 	GRenderInterface.UpdateDescriptorHeap(Shader.E[0][SEType]);
// 	if (!Shader.E[0][SEType].Set(Item, FVF)) { return false; }
//
// 	Item.UseUniform[XRayUniformAllocator::UT_Transformation] = true;
// 	Item.UseUniform[XRayUniformAllocator::UT_Tree] = true;
//
// 	Item.GraphicsPipelineResource.VertexBuffer = VertexBuffer;
// 	Item.GraphicsPipelineResource.IndexBuffer = IndexBuffer;
//
// 	int lod_id = last_lod;
// 	if (LOD >= 0.f)
// 	{
// 		lod_id = iFloor((1.f - LOD) * float(pSWI->count - 1) + 0.5f);
// 		last_lod = lod_id;
// 	}
// 	VERIFY(lod_id >= 0 && lod_id<int(pSWI->count));
// 	FSlideWindow& SW = pSWI->sw[lod_id];
//
// 	Item.GraphicsPipelineResource.CountIndex = SW.num_tris * 3;
// 	Item.GraphicsPipelineResource.OffsetIndex = OffsetIndex + SW.offset;
// 	Item.GraphicsPipelineResource.OffsetVertex = OffsetVertex;
// 	Item.Visual = this;
// 	return Item.GraphicsPipelineResource.CountIndex;
// }


void XRayTreeVisual_PM::Load(LPCSTR N, IReader* data, u32 dwFlags)
{
	inherited::Load(N, data, dwFlags);
	R_ASSERT(data->find_chunk(OGF_SWICONTAINER));
	{
		u32 ID = data->r_u32();
		//pSWI = GRenderInterface.GetSWI(ID);
	}
}

void XRayTreeVisual_PM::Copy(XRayRenderVisual* from)
{
	inherited::Copy(from);
	XRayTreeVisual_PM* pFrom = dynamic_cast<XRayTreeVisual_PM*> (from);
	PCOPY(pSWI);
}
