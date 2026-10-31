#include "stdafx.h"
#include "dx11XMLBlendCompiler.h"
#include "dxRenderDeviceRender.h"
#include "blenders/Blender_Recorder.h"

namespace
{
	struct NamedValue
	{
		std::string_view Name;
		u32 Value;
	};

	constexpr NamedValue Blends[] =
	{
		{ "zero", D3DBLEND_ZERO }, { "one", D3DBLEND_ONE }, { "srccolor", D3DBLEND_SRCCOLOR }, { "invsrccolor", D3DBLEND_INVSRCCOLOR },
		{ "srcalpha", D3DBLEND_SRCALPHA }, { "invsrcalpha", D3DBLEND_INVSRCALPHA }, { "destalpha", D3DBLEND_DESTALPHA }, { "invdestalpha", D3DBLEND_INVDESTALPHA },
		{ "destcolor", D3DBLEND_DESTCOLOR }, { "invdestcolor", D3DBLEND_INVDESTCOLOR }, { "srcalphasat", D3DBLEND_SRCALPHASAT },
	};

	constexpr NamedValue Compares[] =
	{
		{ "never", D3DCMP_NEVER }, { "less", D3DCMP_LESS }, { "equal", D3DCMP_EQUAL }, { "lessequal", D3DCMP_LESSEQUAL },
		{ "greater", D3DCMP_GREATER }, { "notequal", D3DCMP_NOTEQUAL }, { "greaterequal", D3DCMP_GREATEREQUAL }, { "always", D3DCMP_ALWAYS },
	};

	constexpr NamedValue StencilOps[] =
	{
		{ "keep", D3DSTENCILOP_KEEP }, { "zero", D3DSTENCILOP_ZERO }, { "replace", D3DSTENCILOP_REPLACE }, { "incrsat", D3DSTENCILOP_INCRSAT },
		{ "decrsat", D3DSTENCILOP_DECRSAT }, { "invert", D3DSTENCILOP_INVERT }, { "incr", D3DSTENCILOP_INCR }, { "decr", D3DSTENCILOP_DECR },
	};

	constexpr NamedValue CullModes[] = { { "none", D3DCULL_NONE }, { "cw", D3DCULL_CW }, { "ccw", D3DCULL_CCW } };

	template<size_t Count>
	u32 Lookup(const NamedValue (&Table)[Count], std::string_view Name, const char* File)
	{
		for (const NamedValue& Item : Table)
		{
			if (Item.Name == Name)
			{
				return Item.Value;
			}
		}
		R_ASSERT4(false, "Unknown shader state value", xr_string(Name.data(), u32(Name.size())).c_str(), File);
		return 0;
	}

	bool ShaderOption(const char* Name)
	{
#if !defined(_EDITOR)
		if (!xr_strcmp(Name, "USE_R1_STATIC_LIGHTING"))
		{
			return LightingModeIsStaticRender();
		}
#endif
		return EngineExternal().ShadersOptions.contains(xr_string(Name));
	}

	xr_string ResolveTexture(const char* Source, const char* const (&Sources)[3])
	{
		static constexpr std::string_view Prefixes[] = { "@base", "@second", "@detail" };
		for (u32 Index = 0; Index < std::size(Prefixes); ++Index)
		{
			if (std::string_view(Source).starts_with(Prefixes[Index]))
			{
				return xr_string(Sources[Index]) + (Source + Prefixes[Index].size());
			}
		}
		return Source;
	}
}

CXMLBlend::CXMLBlend(const char* FileName)
{
	xr_strcpy(File, FileName);
	xr_string Directory = RImplementation.getShaderPath();
	if (Directory.ends_with('\\'))
	{
		Directory.pop_back();
	}
	Parser.Load(_game_shaders_, Directory.c_str(), File);
}

bool CXMLBlend::IsStatic()
{
	return Parser.ReadAttribBool(Parser.GetRoot(), "r1_static", false);
}

XML_NODE* CXMLBlend::ActiveSet()
{
	XML_NODE* Root = Parser.GetRoot();
	for (int Index = 0, Count = Parser.GetNodesNum(Root, "option"); Index < Count; ++Index)
	{
		XML_NODE* Option = Parser.NavigateToNode(Root, "option", Index);
		if (ShaderOption(Parser.ReadAttrib(Option, "name", "")))
		{
			return Option;
		}
	}
	return Root;
}

Shader* CXMLBlend::Compile(const char* Textures)
{
	XML_NODE* Set = ActiveSet();
	XML_NODE* Variants[] = { Parser.NavigateToNode(Set, "normal_hq"), Parser.NavigateToNode(Set, "normal"), Parser.NavigateToNode(Set, "normal_ui"),
		Parser.NavigateToNode(Set, "normal_hud"), Parser.NavigateToNode(Set, "l_point"), Parser.NavigateToNode(Set, "l_spot"), Parser.NavigateToNode(Set, "l_special") };
	auto [NormalHQ, Normal, NormalUI, NormalHUD, LPoint, LSpot, LSpecial] = Variants;
	if (!NormalHQ && !Normal && !LSpecial)
	{
		return nullptr;
	}

	CBlender_Compile C;
	C.BT = nullptr;
	C.bEditor = false;
	C.bDetail = false;
	C.detail_texture = nullptr;
	C.detail_scaler = nullptr;
	DEV->_ParseList(C.L_textures, Textures);
	const bool Detail = (NormalHQ || Normal || NormalUI || NormalHUD) && DEV->m_textures_description.GetDetailTexture(C.L_textures[0], C.detail_texture, C.detail_scaler);

	const char* const Sources[3] =
	{
		*C.L_textures[0] ? *C.L_textures[0] : "null",
		C.L_textures.size() > 1 ? *C.L_textures[1] : "null",
		C.L_textures.size() > 2 && C.L_textures[2].size() ? *C.L_textures[2] : (C.detail_texture ? C.detail_texture : "null"),
	};
	auto Element = [&](XML_NODE* Node, int Index, bool UseDetail)
	{
		C.iElement = Index;
		C.bDetail = UseDetail && Detail;
		return CompileElement(C, Node, Sources);
	};

	Shader S;
	if (XML_NODE* First = NormalHQ && (Detail || !Normal) ? NormalHQ : Normal)
	{
		S.E[ShaderVariant::NormalHQ] = Element(First, ShaderVariant::NormalHQ, true);
	}
	if (NormalUI)
	{
		S.E[ShaderVariant::UI] = Element(NormalUI, ShaderVariant::UI, true);
	}
	if (NormalHUD && !LightingModeIsStaticRender())
	{
		S.E[ShaderVariant::HUD] = Element(NormalHUD, ShaderVariant::HUD, true);
	}
	if (Normal)
	{
		S.E[ShaderVariant::NormalLQ] = Element(Normal, ShaderVariant::NormalLQ, true);
	}
	if (LPoint)
	{
		S.E[ShaderVariant::PointLight] = Element(LPoint, ShaderVariant::PointLight, false);
	}
	if (LSpot)
	{
		S.E[ShaderVariant::SpotLight] = Element(LSpot, ShaderVariant::SpotLight, false);
	}
	if (LSpecial)
	{
		S.E[ShaderVariant::Special] = Element(LSpecial, ShaderVariant::Special, false);
	}
	return DEV->_CreateShader(&S);
}

ShaderElement* CXMLBlend::CompileElement(CBlender_Compile& C, XML_NODE* Node, const char* const (&Sources)[3])
{
	ShaderElement Element;
	C.SH = &Element;
	C.RS.Invalidate();

	if (const int Sort = Parser.ReadAttribInt(Node, "sort", -1); Sort >= 0)
	{
		C.SetParams(Sort, Parser.ReadAttribBool(Node, "strict"));
	}
	Element.flags.bEmissive = Parser.ReadAttribBool(Node, "emissive");
	Element.flags.bDistort = Parser.ReadAttribBool(Node, "distort");
	Element.flags.bWmark = Parser.ReadAttribBool(Node, "wmark");
	Element.flags.bScopeMask = Parser.ReadAttribBool(Node, "scope");

	for (int Index = 0, Count = Parser.GetNodesNum(Node, "pass"); Index < Count; ++Index)
	{
		CompilePass(C, Parser.NavigateToNode(Node, "pass", Index), Sources);
	}
	return DEV->_CreateElement(Element);
}

void CXMLBlend::SetBlend(CBlender_Compile& C, u32 Index, const char* Value)
{
	if (!Value)
	{
		return;
	}
	const char* Comma = strchr(Value, ',');
	if (!Comma)
	{
		C.PassSET_ablend_mode(Index, false, D3DBLEND_ONE, D3DBLEND_ZERO);
		return;
	}
	C.PassSET_ablend_mode(Index, true, Lookup(Blends, std::string_view(Value, Comma - Value), File), Lookup(Blends, Comma + 1, File));
}

void CXMLBlend::CompilePass(CBlender_Compile& C, XML_NODE* Node, const char* const (&Sources)[3])
{
	C.r_Pass(Parser.ReadAttrib(Node, "vs", "null"), Parser.ReadAttrib(Node, "gs", "null"), Parser.ReadAttrib(Node, "ps", "null"), true);

	if (const char* ZFunc = Parser.ReadAttrib(Node, "zfunc", nullptr))
	{
		C.RS.SetRS(D3DRS_ZFUNC, Lookup(Compares, ZFunc, File));
	}
	if (!Parser.ReadAttribBool(Node, "zwrite", true))
	{
		C.RS.SetRS(D3DRS_ZWRITEENABLE, FALSE);
	}

	SetBlend(C, u32(-1), Parser.ReadAttrib(Node, "blend", nullptr));
	for (u32 Index = 0; Index < 4; ++Index)
	{
		string16 Key;
		xr_sprintf(Key, "blend%u", Index);
		SetBlend(C, Index, Parser.ReadAttrib(Node, Key, nullptr));
	}

	if (const int Aref = Parser.ReadAttribInt(Node, "aref", -1); Aref >= 0)
	{
		C.PassSET_ablend_aref(true, u32(Aref));
	}
	if (const char* Color = Parser.ReadAttrib(Node, "color", nullptr))
	{
		C.r_ColorWriteEnable(strchr(Color, 'r'), strchr(Color, 'g'), strchr(Color, 'b'), strchr(Color, 'a'));
	}
	if (const char* Stencil = Parser.ReadAttrib(Node, "stencil", nullptr))
	{
		string256 Tokens[6] = {};
		const int Count = _GetItemCount(Stencil);
		for (int Index = 0; Index < Count && Index < 6; ++Index)
		{
			_GetItem(Stencil, Index, Tokens[Index]);
		}
		if (Count < 6)
		{
			C.r_Stencil(false);
		}
		else
		{
			C.r_Stencil(true, Lookup(Compares, Tokens[0], File), atoi(Tokens[1]), atoi(Tokens[2]), Lookup(StencilOps, Tokens[3], File), Lookup(StencilOps, Tokens[4], File), Lookup(StencilOps, Tokens[5], File));
		}
	}
	if (const int Ref = Parser.ReadAttribInt(Node, "stencil_ref", -1); Ref >= 0)
	{
		C.r_StencilRef(u32(Ref));
	}
	if (const char* Cull = Parser.ReadAttrib(Node, "cull", nullptr))
	{
		C.r_CullMode(D3DCULL(Lookup(CullModes, Cull, File)));
	}
	if (Parser.ReadAttribBool(Node, "atoc"))
	{
		C.RS.SetRS(XRDX10RS_ALPHATOCOVERAGE, TRUE);
	}

	for (int Index = 0, Count = Parser.GetNodesNum(Node, "texture"); Index < Count; ++Index)
	{
		XML_NODE* Texture = Parser.NavigateToNode(Node, "texture", Index);
		C.r_dx10Texture(Parser.ReadAttrib(Texture, "name", ""), ResolveTexture(Parser.ReadAttrib(Texture, "src", "null"), Sources).c_str());
	}
	for (int Index = 0, Count = Parser.GetNodesNum(Node, "define"); Index < Count; ++Index)
	{
		XML_NODE* Define = Parser.NavigateToNode(Node, "define", Index);
		C.AddDefine(Parser.ReadAttrib(Define, "name", ""), Parser.ReadAttrib(Define, "value", ""));
	}
	C.r_End();
}
