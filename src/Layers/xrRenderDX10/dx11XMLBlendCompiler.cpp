//////////////////////////////////////////////////////////////
// Desc		: XML Blend Compiler
// Author	: ForserX
//////////////////////////////////////////////////////////////
// Oxygen Engine 2016-2019
//////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "dx11XMLBlendCompiler.h"
#include "../xrRender/dxRenderDeviceRender.h"

CXMLBlend::CXMLBlend(const char* FileName)
{
	string256 FixedName;
	for (int i = 0, l = xr_strlen(FileName) + 1; i < l; ++i)
		FixedName[i] = ('\\' == FileName[i]) ? '_' : FileName[i];

	xr_strconcat(FixedName, FixedName, ".xml");
	memcpy(File, FixedName, sizeof(FixedName));

	pCompiler = new CBlender_Compile();
	Parser.Load("$game_shaders$", "d3d11", File);
	pCompiler->detail_texture = nullptr;
	pCompiler->detail_scaler = nullptr;
}

CXMLBlend::~CXMLBlend()
{
	xr_delete(pCompiler);
}

Shader* CXMLBlend::Compile(const char* Texture)
{
	Shader* pShader = xr_new<Shader>();
	XML_NODE* pRoot = Parser.GetRoot();
	for (u32 Iter = 0; Iter < 6; Iter++)
	{
		bool bUseDetail = true;
		string16 buff;
		xr_sprintf(buff, sizeof(buff), "element_%d", Iter);
		XML_NODE* pElement = Parser.NavigateToNode(pRoot, buff);
		if (!pElement)
		{
			string16 buff_nd;
			xr_sprintf(buff_nd, sizeof(buff), "element_nd_%d", Iter);
			pElement = Parser.NavigateToNode(pRoot, buff_nd);
			bUseDetail = false;
		}

		if (pElement)
		{
			dxRenderDeviceRender::Instance().Resources->_ParseList(pCompiler->L_textures, Texture);
			pCompiler->iElement = Iter;
			pCompiler->bDetail = bUseDetail ? dxRenderDeviceRender::Instance().Resources->m_textures_description.GetDetailTexture(pCompiler->L_textures[0], pCompiler->detail_texture, pCompiler->detail_scaler) : false;

			pShader->E[Iter] = MakeShader(Texture, pElement);
		}
	}

	Shader* ResultShader = DEV->_CreateShader(pShader);
	xr_delete(pShader);
	return ResultShader;
}

ShaderElement* CXMLBlend::MakeShader(const char* Texture, XML_NODE* pElement)
{
	pCompiler->SH = new ShaderElement();
	pCompiler->RS.Invalidate();

	// Compile
	LPCSTR t_0 = *pCompiler->L_textures[0] ? *pCompiler->L_textures[0] : "null";

	// Parse root attributes
	bool bFog = Parser.ReadAttribBool(pElement, "fog", true);
	bool bZb[2] =
	{
		Parser.ReadAttribBool(pElement, "zread", true),
		Parser.ReadAttribBool(pElement, "zwrite", true),
	};
	const char* PSName = Parser.ReadAttrib(pElement, "ps", "false");
	const char* VSName = Parser.ReadAttrib(pElement, "vs", "false");
	const char* GSName = Parser.ReadAttrib(pElement, "gs", nullptr);

	if (GSName != nullptr)
	{
		pCompiler->r_Pass(VSName, GSName, PSName, bFog, bZb[0], bZb[1]);
	}
	else
	{
		pCompiler->r_Pass(VSName, PSName, bFog, bZb[0], bZb[1]);
	}

	// Check blend
	XML_NODE* pBlend = Parser.NavigateToNode(pElement, "blend");
	if (pBlend)
	{
		bool bStatus = Parser.ReadAttribBool(pBlend, "status");
		u32 SrcType = BlendValidate(Parser.ReadAttrib(pBlend, "src", "zero"));
		u32 DestType = BlendValidate(Parser.ReadAttrib(pBlend, "dest", "zero"));
		pCompiler->PassSET_ablend_mode(bStatus, SrcType, DestType);
	}

	// Check sorting
	XML_NODE* pSorting = Parser.NavigateToNode(pElement, "sort");
	if (pSorting)
	{
		bool bStatus = Parser.ReadAttribBool(pSorting, "status");
		int Count = Parser.ReadAttribInt(pSorting, "count", 1);
		pCompiler->SetParams(Count, bStatus);
	}

	// Check culling 
	XML_NODE* pCullMode = Parser.NavigateToNode(pElement, "cull_mode");
	if (pCullMode)
	{
		pCompiler->r_CullMode(CullModeValidate(Parser.ReadAttrib(pCullMode, "mode", "none")));
	}

	// Distort, emissive and wmark
	XML_NODE* pFlags = Parser.NavigateToNode(pElement, "flags");
	if (pFlags)
	{
		pCompiler->SH->flags.bDistort = Parser.ReadAttribBool(pFlags, "dist");
		pCompiler->SH->flags.bEmissive = Parser.ReadAttribBool(pFlags, "emissive");
		pCompiler->SH->flags.bWmark = Parser.ReadAttribBool(pFlags, "wmark");
	}

	// Check atoc
	XML_NODE* pAtoc = Parser.NavigateToNode(pElement, "atoc");
	if (pAtoc)
		pCompiler->RS.SetRS(XRDX10RS_ALPHATOCOVERAGE, Parser.ReadAttribBool(pAtoc, "status"));

	// Check aref
	XML_NODE* pAref = Parser.NavigateToNode(pElement, "aref");
	if (pAref)
	{
		bool bStatus = Parser.ReadAttribBool(pAref, "status");
		int Count = Parser.ReadAttribInt(pAref, "count", 0);
		pCompiler->PassSET_ablend_aref(bStatus, Count);
	}

	// Check color
	XML_NODE* pColor = Parser.NavigateToNode(pElement, "color");
	if (pColor)
	{
		pCompiler->r_ColorWriteEnable
		(
			Parser.ReadAttribBool(pColor, "r"),
			Parser.ReadAttribBool(pColor, "g"),
			Parser.ReadAttribBool(pColor, "b"),
			Parser.ReadAttribBool(pColor, "a")
		);
	}

	// Check stencil 
	XML_NODE* pStencil = Parser.NavigateToNode(pElement, "stencil");
	if (pStencil)
	{
		bool bStatus = Parser.ReadAttribBool(pStencil, "status");
		u32 CMP = CMPFunValidate(Parser.ReadAttrib(pStencil, "cmp", "never"));
		u32 Mask = Parser.ReadAttribInt(pStencil, "mask", 0);
		u32 WriteMask = Parser.ReadAttribInt(pStencil, "wmask", 0);;

		u32 Fail = StencilValidate(Parser.ReadAttrib(pStencil, "fail", "zero"));
		u32 Pass = StencilValidate(Parser.ReadAttrib(pStencil, "pass", "zero"));
		u32 zFail = StencilValidate(Parser.ReadAttrib(pStencil, "zfail", "zero"));

		pCompiler->r_Stencil(bStatus, CMP, Mask, WriteMask, Fail, Pass, zFail);

		XML_NODE* pStencilRef = Parser.NavigateToNode(pStencil, "ref");
		if (pStencilRef)
			pCompiler->r_StencilRef(Parser.ReadAttribInt(pStencilRef, "value", 0));
	}

	// Check body 
	int Idx = 1;
	XML_NODE* pTexture = Parser.NavigateToNode(pElement, "texture");
	while (pTexture)
	{
		shared_str TextureName = Parser.ReadAttrib(pTexture, "name", "none");
		shared_str RtName = Parser.ReadAttrib(pTexture, "rt", "none");
		xr_string DestTexName = Parser.ReadAttrib(pTexture, "dest", "none");
		if (TextureName != "none" && RtName != "none")
		{
			if (RtName == "t_base")
			{
				if (DestTexName != "none")
				{
					xr_string TryTexName = TextureName.c_str() + DestTexName;
					pCompiler->r_dx10Texture(TryTexName.c_str(), t_0);
				}
				else pCompiler->r_dx10Texture(TextureName.c_str(), t_0);

			}
			else
				pCompiler->r_dx10Texture(TextureName.c_str(), RtName.c_str());
		}

		pTexture = Parser.NavigateToNode(pElement, "texture", Idx);
		Idx++;
	}

	Idx = 1;
	XML_NODE* pSampler = Parser.NavigateToNode(pElement, "sampler");
	while (pSampler)
	{
		shared_str SamplerName = Parser.ReadAttrib(pSampler, "name", "none");
		if (SamplerName != "none")
			pCompiler->r_dx10Sampler(SamplerName.c_str());

		pSampler = Parser.NavigateToNode(pElement, "sampler", Idx);
		Idx++;
	}

	pCompiler->r_End();
	ShaderElement* pTryElement = DEV->_CreateElement(*pCompiler->SH);
	xr_delete(pCompiler->SH);
	return pTryElement;
}

bool CXMLBlend::Check(const char* FileName)
{
	string256 NewName;
	for (int i = 0, l = xr_strlen(FileName) + 1; i < l; ++i)
		NewName[i] = ('\\' == FileName[i]) ? '_' : FileName[i];

	xr_strconcat(NewName, NewName, ".xml");
	string_path PathAndFile;

	FS.update_path(PathAndFile, "$game_shaders$", "d3d11\\");
	xr_strconcat(PathAndFile, PathAndFile, NewName);

	return FS.exist(PathAndFile);
}

u32 CXMLBlend::BlendValidate(shared_str type)
{
	u32 SrcType = 0;
	if (type == "zero")			SrcType = D3DBLEND::D3DBLEND_ZERO;
	if (type == "one")	        SrcType = D3DBLEND::D3DBLEND_ONE;
	if (type == "srccolor")	    SrcType = D3DBLEND::D3DBLEND_SRCCOLOR;
	if (type == "invsrccolor")  SrcType = D3DBLEND::D3DBLEND_INVSRCCOLOR;
	if (type == "srcalpha")	    SrcType = D3DBLEND::D3DBLEND_SRCALPHA;
	if (type == "invsrcalpha")  SrcType = D3DBLEND::D3DBLEND_INVSRCALPHA;
	if (type == "destalpha")	SrcType = D3DBLEND::D3DBLEND_DESTALPHA;
	if (type == "invdestalpha") SrcType = D3DBLEND::D3DBLEND_INVDESTALPHA;
	if (type == "destcolor")	SrcType = D3DBLEND::D3DBLEND_DESTCOLOR;
	if (type == "invdestcolor")	SrcType = D3DBLEND::D3DBLEND_INVDESTCOLOR;
	if (type == "srcalphasat")	SrcType = D3DBLEND::D3DBLEND_SRCALPHASAT;

	R_ASSERT2(SrcType != 0, make_string<const char*>("FILE: %s - undefined blend function (D3DBLEND) value: %s", File, type.c_str()));

	return SrcType;
}

u32 CXMLBlend::StencilValidate(shared_str type)
{
	u32 SrcType = 0;
	if (type == "keep")			SrcType = D3DSTENCILOP::D3DSTENCILOP_KEEP;
	if (type == "zero")			SrcType = D3DSTENCILOP::D3DSTENCILOP_ZERO;
	if (type == "replace")	    SrcType = D3DSTENCILOP::D3DSTENCILOP_REPLACE;
	if (type == "incrsat")		SrcType = D3DSTENCILOP::D3DSTENCILOP_INCRSAT;
	if (type == "decrsat")	    SrcType = D3DSTENCILOP::D3DSTENCILOP_DECRSAT;
	if (type == "invert")		SrcType = D3DSTENCILOP::D3DSTENCILOP_INVERT;
	if (type == "incr")			SrcType = D3DSTENCILOP::D3DSTENCILOP_INCR;
	if (type == "decr")			SrcType = D3DSTENCILOP::D3DSTENCILOP_DECR;

	R_ASSERT2(SrcType != 0, make_string<const char*>("FILE: %s - undefined stencil operator (D3DSTENCILOP) value: %s", File, type.c_str()));

	return SrcType;
}

u32 CXMLBlend::CMPFunValidate(shared_str type)
{
	u32 SrcType = 0;
	if (type == "never")		SrcType = D3DCMPFUNC::D3DCMP_NEVER;
	if (type == "less")	        SrcType = D3DCMPFUNC::D3DCMP_LESS;
	if (type == "equal")	    SrcType = D3DCMPFUNC::D3DCMP_EQUAL;
	if (type == "lessequal")	SrcType = D3DCMPFUNC::D3DCMP_LESSEQUAL;
	if (type == "greater")	    SrcType = D3DCMPFUNC::D3DCMP_GREATER;
	if (type == "notequal")		SrcType = D3DCMPFUNC::D3DCMP_NOTEQUAL;
	if (type == "greaterequal")	SrcType = D3DCMPFUNC::D3DCMP_GREATEREQUAL;
	if (type == "always")		SrcType = D3DCMPFUNC::D3DCMP_ALWAYS;

	R_ASSERT2(SrcType != 0, make_string<const char*>("FILE: %s - undefined compare function (D3DCMPFUNC) value: %s", File, type.c_str()));

	return SrcType;
}

D3DCULL CXMLBlend::CullModeValidate(shared_str type)
{
	D3DCULL SrcType;
	if (type == "none")			SrcType = D3DCULL::D3DCULL_NONE;
	if (type == "cw")			SrcType = D3DCULL::D3DCULL_CW;
	if (type == "ccw")			SrcType = D3DCULL::D3DCULL_CCW;

	R_ASSERT2(SrcType != 0, make_string<const char*>("FILE: %s - invalid cull mode (D3DCULL) value: %s", File, type.c_str()));

	return SrcType;
}