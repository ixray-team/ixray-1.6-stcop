#include "stdafx.h"
#include "XMLBlendV2.h"
#include "dxRenderDeviceRender.h"
#include "blenders/Blender_Recorder.h"
#include "../../xrCore/FormatParsers/XML/xrXMLParser.h"
#include "../../xrEngine/EngineAPI.h"

namespace
{
	template <typename... Args>
	xr_string Format(const char* Text, Args... Arguments)
	{
		string1024 Buffer;
		xr_sprintf(Buffer, Text, Arguments...);
		return Buffer;
	}

	struct NamedValue
	{
		const char* Name;
		u32 Value;
	};

	const NamedValue BlendNames[] =
	{
		{ "zero", D3DBLEND_ZERO }, { "one", D3DBLEND_ONE }, { "srccolor", D3DBLEND_SRCCOLOR },
		{ "invsrccolor", D3DBLEND_INVSRCCOLOR }, { "srcalpha", D3DBLEND_SRCALPHA }, { "invsrcalpha", D3DBLEND_INVSRCALPHA },
		{ "destalpha", D3DBLEND_DESTALPHA }, { "invdestalpha", D3DBLEND_INVDESTALPHA }, { "destcolor", D3DBLEND_DESTCOLOR },
		{ "invdestcolor", D3DBLEND_INVDESTCOLOR }, { "srcalphasat", D3DBLEND_SRCALPHASAT },
	};

	const NamedValue CmpNames[] =
	{
		{ "never", D3DCMP_NEVER }, { "less", D3DCMP_LESS }, { "equal", D3DCMP_EQUAL }, { "lessequal", D3DCMP_LESSEQUAL },
		{ "greater", D3DCMP_GREATER }, { "notequal", D3DCMP_NOTEQUAL }, { "greaterequal", D3DCMP_GREATEREQUAL }, { "always", D3DCMP_ALWAYS },
	};

	const NamedValue StencilNames[] =
	{
		{ "keep", D3DSTENCILOP_KEEP }, { "zero", D3DSTENCILOP_ZERO }, { "replace", D3DSTENCILOP_REPLACE }, { "incrsat", D3DSTENCILOP_INCRSAT },
		{ "decrsat", D3DSTENCILOP_DECRSAT }, { "invert", D3DSTENCILOP_INVERT }, { "incr", D3DSTENCILOP_INCR }, { "decr", D3DSTENCILOP_DECR },
	};

	const NamedValue CullNames[] =
	{
		{ "none", D3DCULL_NONE }, { "cw", D3DCULL_CW }, { "ccw", D3DCULL_CCW },
	};

	enum class EKind : u8
	{
		String,
		Bool,
		Int,
		Blend,
		Cmp,
		Stencil,
		Cull
	};

	struct AttributeDesc
	{
		const char* Name;
		EKind Kind;
		u8 Slot;
		bool IsOptional = false;
	};

	struct OpDesc
	{
		xr_vector<AttributeDesc> Attributes;
	};

	const OpDesc& Describe(EBlendOp Type)
	{
		static const OpDesc Descs[] =
		{
			 				{ { { "vs", EKind::String, 0 }, { "gs", EKind::String, 1, true }, { "ps", EKind::String, 2 } } },
			 			{ { { "vs", EKind::String, 0 }, { "gs", EKind::String, 1, true }, { "ps", EKind::String, 2 }, { "fog", EKind::Bool, 0 }, { "zread", EKind::Bool, 1 }, { "zwrite", EKind::Bool, 2 } } },
			 		{ {} },
			 			{ { { "priority", EKind::Int, 0 }, { "strict", EKind::Bool, 1 } } },
			 			{ { { "value", EKind::Bool, 0 } } },
			 				{ { { "value", EKind::Bool, 0 } } },
			 	{ { { "value", EKind::Bool, 0 } } },
			 			{ { { "value", EKind::Bool, 0 } } },
			 				{ { { "value", EKind::Bool, 0 } } },
			 				{ { { "value", EKind::Bool, 0 } } },
			 				{ { { "test", EKind::Bool, 0 }, { "write", EKind::Bool, 1 } } },
			 				{ { { "enable", EKind::Bool, 0 }, { "src", EKind::Blend, 1 }, { "dest", EKind::Blend, 2 } } },
			 			{ { { "index", EKind::Int, 0 }, { "enable", EKind::Bool, 1 }, { "src", EKind::Blend, 2 }, { "dest", EKind::Blend, 3 } } },
			 				{ { { "enable", EKind::Bool, 0 }, { "value", EKind::Int, 1 } } },
			 		{ { { "r", EKind::Bool, 0 }, { "g", EKind::Bool, 1 }, { "b", EKind::Bool, 2 }, { "a", EKind::Bool, 3 } } },
			 			{ { { "name", EKind::String, 0 }, { "value", EKind::String, 1 } } },
			 			{ { { "name", EKind::String, 0 } } },
			 			{ { { "enable", EKind::Bool, 0 }, { "func", EKind::Cmp, 1 }, { "mask", EKind::Int, 2 }, { "write_mask", EKind::Int, 3 }, { "fail", EKind::Stencil, 4 }, { "pass", EKind::Stencil, 5 }, { "zfail", EKind::Stencil, 6 } } },
			 		{ { { "value", EKind::Int, 0 } } },
			 				{ { { "func", EKind::Cmp, 0 } } },
			 		{ { { "name", EKind::String, 0 }, { "value", EKind::String, 1 } } },
			 { {} },
			 			{ { { "mode", EKind::Cull, 0 } } },
			 	{ { { "value", EKind::Bool, 0 } } },
		};
		static_assert(std::size(Descs) == size_t(EBlendOp::Count));
		return Descs[u32(Type)];
	}

	template <size_t Size>
	bool ParseNamed(const NamedValue (&Table)[Size], const char* Text, u32& Value)
	{
		for (const NamedValue& Item : Table)
		{
			if (0 == xr_strcmp(Item.Name, Text))
			{
				Value = Item.Value;
				return true;
			}
		}

		char* End = nullptr;
		const unsigned long Number = strtoul(Text, &End, 10);
		if (End == Text || *End != 0)
			return false;
		Value = u32(Number);
		return true;
	}

	bool ParseBool(const char* Text, u32& Value)
	{
		if (!xr_strcmp(Text, "1") || !_stricmp(Text, "true"))
		{
			Value = 1;
			return true;
		}
		if (!xr_strcmp(Text, "0") || !_stricmp(Text, "false"))
		{
			Value = 0;
			return true;
		}
		return false;
	}

	bool ParseInt(const char* Text, u32& Value)
	{
		char* End = nullptr;
		const long Number = strtol(Text, &End, 10);
		if (End == Text || *End != 0)
			return false;
		Value = u32(Number);
		return true;
	}

	bool ParseValue(EKind Kind, const char* Text, u32& Value)
	{
		switch (Kind)
		{
		case EKind::Bool:		return ParseBool(Text, Value);
		case EKind::Int:		return ParseInt(Text, Value);
		case EKind::Blend:		return ParseNamed(BlendNames, Text, Value);
		case EKind::Cmp:		return ParseNamed(CmpNames, Text, Value);
		case EKind::Stencil:	return ParseNamed(StencilNames, Text, Value);
		case EKind::Cull:		return ParseNamed(CullNames, Text, Value);
		default:				return false;
		}
	}

	const char* const ElementNames[] = { "normal", "normal_hq", "normal_ui", "normal_hud", "l_point", "l_spot", "l_special", "editor" };

	bool IsElementName(const char* Name)
	{
		for (const char* Known : ElementNames)
		{
			if (0 == xr_strcmp(Known, Name))
				return true;
		}
		return false;
	}

	bool IsOptionMatched(const xr_string& Option)
	{
		if (Option.empty())
			return true;
		if (Option[0] == '!')
			return !BlendOps::IsShaderOptionEnabled(Option.c_str() + 1);
		return BlendOps::IsShaderOptionEnabled(Option.c_str());
	}

	void SetError(xr_string& Error, const char* FileName, const char* Node, const char* Message)
	{
		Error = Format("%s: <%s>: %s", FileName, Node, Message).c_str();
	}

	bool ParseOp(tinyxml2::XMLElement* Node, const char* FileName, BlendOp& Op, xr_string& Error)
	{
		if (!BlendOps::FindType(Node->Name(), Op.Type))
		{
			SetError(Error, FileName, Node->Name(), "unknown operation");
			return false;
		}

		for (const AttributeDesc& Attribute : Describe(Op.Type).Attributes)
		{
			const char* Text = Node->Attribute(Attribute.Name);
			if (Text == nullptr)
			{
				if (Attribute.IsOptional)
					continue;
				SetError(Error, FileName, Node->Name(), Format("missing attribute '%s'", Attribute.Name).c_str());
				return false;
			}

			if (Attribute.Kind == EKind::String)
			{
				Op.Strings[Attribute.Slot] = Text;
			}
			else if (!ParseValue(Attribute.Kind, Text, Op.Values[Attribute.Slot]))
			{
				SetError(Error, FileName, Node->Name(), Format("invalid value '%s' of '%s'", Text, Attribute.Name).c_str());
				return false;
			}
		}
		return true;
	}

	bool ParseSet(tinyxml2::XMLElement* Parent, const char* FileName, XMLBlendV2::ElementSet& Set, XMLBlendV2::Script* Root, xr_string& Error)
	{
		for (tinyxml2::XMLElement* Node = Parent->FirstChildElement(); Node; Node = Node->NextSiblingElement())
		{
			const char* Name = Node->Name();
			if (Root && 0 == xr_strcmp(Name, "static"))
			{
				Root->HasStaticSet = true;
				if (!ParseSet(Node, FileName, Root->Static, nullptr, Error))
					return false;
				continue;
			}

			XMLBlendV2::Element Element;
			if (0 == xr_strcmp(Name, "element"))
			{
				Element.Index = Node->IntAttribute("index", -1);
				Element.UseDetail = Node->BoolAttribute("detail", true);
				if (Element.Index < 0 || Element.Index >= SHADER_ELEMENTS_MAX)
				{
					SetError(Error, FileName, Name, "invalid slot index");
					return false;
				}
			}
			else if (IsElementName(Name))
			{
				Element.Name = Name;
			}
			else
			{
				SetError(Error, FileName, Name, "unknown element");
				return false;
			}

			if (const char* Option = Node->Attribute("option"))
				Element.Option = Option;

			for (tinyxml2::XMLElement* OpNode = Node->FirstChildElement(); OpNode; OpNode = OpNode->NextSiblingElement())
			{
				BlendOp Op;
				if (!ParseOp(OpNode, FileName, Op, Error))
					return false;
				Element.Ops.push_back(Op);
			}
			Set.Elements.push_back(std::move(Element));
		}
		return true;
	}

	ShaderElement* CompileElement(CBlender_Compile& C, const BlendOpList& Ops)
	{
		ShaderElement E;
		C.SH = &E;
		C.RS.Invalidate();

		BlendOps::TextureArgs Args;
		Args.Base = *C.L_textures[0] ? *C.L_textures[0] : "null";
		Args.Second = (C.L_textures.size() > 1) ? *C.L_textures[1] : "null";
		Args.Detail = (C.L_textures.size() > 2 && C.L_textures[2].size() > 0) ? *C.L_textures[2] : (C.detail_texture ? C.detail_texture : "null");

		bool IsFirstPass = true;
		for (const BlendOp& Op : Ops)
			BlendOps::Apply(C, Op, IsFirstPass, Args);

		C.r_End();
		return DEV->_CreateElement(E);
	}

	void MakeFileName(const char* ShaderName, string_path& FileName)
	{
		string_path Name;
		xr_strcpy(Name, ShaderName);
		for (char* Symbol = Name; *Symbol; ++Symbol)
		{
			if (*Symbol == '\\')
				*Symbol = '_';
		}
		xr_strconcat(FileName, Name, ".xml");
	}
}

const XMLBlendV2::Element* XMLBlendV2::ElementSet::Find(const char* Name) const
{
	for (const Element& Item : Elements)
	{
		if (Item.Name == Name && IsOptionMatched(Item.Option))
			return &Item;
	}
	return nullptr;
}

const XMLBlendV2::Element* XMLBlendV2::ElementSet::FindSlot(int Index) const
{
	for (const Element& Item : Elements)
	{
		if (Item.Index == Index && IsOptionMatched(Item.Option))
			return &Item;
	}
	return nullptr;
}

const XMLBlendV2::ElementSet& XMLBlendV2::Script::Active() const
{
	return (HasStaticSet && LightingModeIsStaticRender()) ? Static : Dynamic;
}

bool XMLBlendV2::IsVersion2(CXml& Parser)
{
	XML_NODE* Root = Parser.GetRoot();
	tinyxml2::XMLElement* Element = Root ? Root->ToElement() : nullptr;
	return Element && 0 == xr_strcmp(Element->Name(), "shader") && Element->IntAttribute("version", 1) == 2;
}

namespace
{
	bool ParseRoot(tinyxml2::XMLElement* Root, const char* FileName, XMLBlendV2::Script& Result, xr_string& Error)
	{
		Result = XMLBlendV2::Script();
		if (!Root || 0 != xr_strcmp(Root->Name(), "shader") || Root->IntAttribute("version", 1) != 2)
		{
			Error = Format("%s: not a version 2 blend", FileName).c_str();
			return false;
		}

		Result.IsStaticBlend = Root->BoolAttribute("r1_static", false);
		return ParseSet(Root, FileName, Result.Dynamic, &Result, Error);
	}
}

bool XMLBlendV2::Parse(CXml& Parser, const char* FileName, Script& Result, xr_string& Error)
{
	XML_NODE* Root = Parser.GetRoot();
	return ParseRoot(Root ? Root->ToElement() : nullptr, FileName, Result, Error);
}

Shader* XMLBlendV2::Compile(const Script& Source, const char* Textures)
{
	const ElementSet& Set = Source.Active();

	CBlender_Compile C;
	Shader S;
	C.BT = nullptr;
	C.bEditor = false;
	C.bDetail = false;
	DEV->_ParseList(C.L_textures, Textures);
	C.detail_texture = nullptr;
	C.detail_scaler = nullptr;

	const auto FindDetail = [&C]()
	{
		return DEV->m_textures_description.GetDetailTexture(C.L_textures[0], C.detail_texture, C.detail_scaler);
	};

	const bool HasSlots = std::any_of(Set.Elements.begin(), Set.Elements.end(), [](const Element& Item) { return Item.Index >= 0; });
	if (HasSlots)
	{
		for (int Iter = 0; Iter < 6; ++Iter)
		{
			const Element* Item = Set.FindSlot(Iter);
			if (!Item)
				continue;
			C.iElement = Iter;
			C.bDetail = Item->UseDetail ? FindDetail() : false;
			S.E[Iter] = CompileElement(C, Item->Ops);
		}
	}
	else
	{
		const Element* Normal = Set.Find("normal");
		if (const Element* NormalHQ = Set.Find("normal_hq"))
		{
			C.iElement = 0;
			C.bDetail = FindDetail();
			const Element* Item = C.bDetail ? NormalHQ : Normal;
			if (Item)
				S.E[0] = CompileElement(C, Item->Ops);
		}
		else if (Normal)
		{
			C.iElement = 0;
			C.bDetail = FindDetail();
			S.E[0] = CompileElement(C, Normal->Ops);
		}

		if (const Element* NormalUI = Set.Find("normal_ui"))
		{
			C.iElement = SE_R2_UI;
			C.bDetail = FindDetail();
			S.UIElement = CompileElement(C, NormalUI->Ops);
		}

		if (!LightingModeIsStaticRender())
		{
			if (const Element* NormalHUD = Set.Find("normal_hud"))
			{
				C.iElement = 5;
				C.bDetail = FindDetail();
				S.E[5] = CompileElement(C, NormalHUD->Ops);
			}
		}

		if (Normal)
		{
			C.iElement = 1;
			C.bDetail = FindDetail();
			S.E[1] = CompileElement(C, Normal->Ops);
		}

		const char* const Lights[] = { "l_point", "l_spot", "l_special" };
		for (int Index = 0; Index < 3; ++Index)
		{
			if (const Element* Item = Set.Find(Lights[Index]))
			{
				C.iElement = 2 + Index;
				C.bDetail = false;
				S.E[2 + Index] = CompileElement(C, Item->Ops);
			}
		}
	}

	return DEV->_CreateShader(&S);
}

bool XMLBlendV2::IsStaticBlend(const char* ShaderName)
{
	static xrCriticalSection Guard;
	static xr_hash_map<xr_string, bool> Cache;

	xrCriticalSectionGuard Lock(Guard);
	const auto It = Cache.find(ShaderName);
	if (It != Cache.end())
		return It->second;

	bool Result = false;
	string_path FileName;
	MakeFileName(ShaderName, FileName);

	string_path FullName;
	FS.update_path(FullName, _game_shaders_, RImplementation.getShaderPath());
	xr_strcat(FullName, FileName);
	if (FS.exist(FullName))
	{
		xr_string Dir = RImplementation.getShaderPath();
		if (Dir.ends_with('\\'))
			Dir.pop_back();

		CXml Parser;
		Parser.Load(_game_shaders_, Dir.c_str(), FileName);
		if (IsVersion2(Parser))
			Result = Parser.GetRoot()->ToElement()->BoolAttribute("r1_static", false);
	}

	Cache.emplace(ShaderName, Result);
	return Result;
}
