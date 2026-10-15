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

	template <size_t Size>
	xr_string NameOf(const NamedValue (&Table)[Size], u32 Value)
	{
		for (const NamedValue& Item : Table)
		{
			if (Item.Value == Value)
				return Item.Name;
		}
		return xr_string::ToString(Value);
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

	xr_string WriteValue(EKind Kind, u32 Value)
	{
		switch (Kind)
		{
		case EKind::Bool:		return Value ? "1" : "0";
		case EKind::Int:		return xr_string::ToString(int(Value));
		case EKind::Blend:		return NameOf(BlendNames, Value);
		case EKind::Cmp:		return NameOf(CmpNames, Value);
		case EKind::Stencil:	return NameOf(StencilNames, Value);
		case EKind::Cull:		return NameOf(CullNames, Value);
		default:				return "";
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

	xr_string Escape(const xr_string& Text)
	{
		xr_string Result;
		for (char Symbol : Text)
		{
			switch (Symbol)
			{
			case '&':	Result += "&amp;";	break;
			case '<':	Result += "&lt;";	break;
			case '>':	Result += "&gt;";	break;
			case '"':	Result += "&quot;";	break;
			default:	Result += Symbol;	break;
			}
		}
		return Result;
	}

	void WriteSet(const XMLBlendV2::ElementSet& Set, const char* Indent, xr_string& Text)
	{
		for (const XMLBlendV2::Element& Element : Set.Elements)
		{
			const xr_string Tag = Element.Name.empty() ? xr_string("element") : Element.Name;
			Text += Indent;
			Text += "<" + Tag;
			if (Element.Name.empty())
			{
				Text += Format(" index=\"%d\" detail=\"%d\"", Element.Index, Element.UseDetail ? 1 : 0).c_str();
			}
			if (!Element.Option.empty())
			{
				Text += " option=\"" + Escape(Element.Option) + "\"";
			}
			Text += ">\r\n";

			for (const BlendOp& Op : Element.Ops)
			{
				Text += Indent;
				Text += "\t<";
				Text += BlendOps::GetName(Op.Type);
				for (const AttributeDesc& Attribute : Describe(Op.Type).Attributes)
				{
					xr_string Value;
					if (Attribute.Kind == EKind::String)
					{
						Value = Op.Strings[Attribute.Slot];
						if (Attribute.IsOptional && Value.empty())
							continue;
					}
					else
					{
						Value = WriteValue(Attribute.Kind, Op.Values[Attribute.Slot]);
					}
					Text += " ";
					Text += Attribute.Name;
					Text += "=\"" + Escape(Value) + "\"";
				}
				Text += "/>\r\n";
			}

			Text += Indent;
			Text += "</" + Tag + ">\r\n";
		}
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

bool XMLBlendV2::Parse(CXml& Parser, const char* FileName, Script& Result, xr_string& Error)
{
	Result = Script();
	if (!IsVersion2(Parser))
	{
		Error = Format("%s: not a version 2 blend", FileName).c_str();
		return false;
	}

	tinyxml2::XMLElement* Root = Parser.GetRoot()->ToElement();
	Result.IsStaticBlend = Root->BoolAttribute("r1_static", false);
	return ParseSet(Root, FileName, Result.Dynamic, &Result, Error);
}

bool XMLBlendV2::ParseVersion1(CXml& Parser, const char* FileName, Script& Result, xr_string& Error)
{
	Result = Script();
	XML_NODE* Root = Parser.GetRoot();
	for (int Iter = 0; Iter < 6; ++Iter)
	{
		bool UseDetail = true;
		string16 Buffer;
		xr_sprintf(Buffer, "element_%d", Iter);
		XML_NODE* Node = Parser.NavigateToNode(Root, Buffer);
		if (!Node)
		{
			xr_sprintf(Buffer, "element_nd_%d", Iter);
			Node = Parser.NavigateToNode(Root, Buffer);
			UseDetail = false;
		}
		if (!Node)
			continue;

		Element Item;
		Item.Index = Iter;
		Item.UseDetail = UseDetail;

		BlendOp Pass;
		Pass.Type = EBlendOp::PassFull;
		Pass.Strings[0] = Parser.ReadAttrib(Node, "vs", "false");
		const char* GS = Parser.ReadAttrib(Node, "gs", nullptr);
		Pass.Strings[1] = GS ? GS : "";
		Pass.Strings[2] = Parser.ReadAttrib(Node, "ps", "false");
		Pass.Values[0] = Parser.ReadAttribBool(Node, "fog", true);
		Pass.Values[1] = Parser.ReadAttribBool(Node, "zread", true);
		Pass.Values[2] = Parser.ReadAttribBool(Node, "zwrite", true);
		Item.Ops.push_back(Pass);

		const auto Add = [&Item](EBlendOp Type, std::initializer_list<u32> Values, std::initializer_list<xr_string> Strings = {})
		{
			BlendOp Op;
			Op.Type = Type;
			u32 Index = 0;
			for (u32 Value : Values)
				Op.Values[Index++] = Value;
			Index = 0;
			for (const xr_string& String : Strings)
				Op.Strings[Index++] = String;
			Item.Ops.push_back(Op);
		};

		const auto ReadNamed = [&](auto& Table, XML_NODE* From, const char* Attribute, const char* Default, u32& Value)
		{
			const char* Text = Parser.ReadAttrib(From, Attribute, Default);
			if (!ParseNamed(Table, Text, Value))
			{
				SetError(Error, FileName, Attribute, Format("invalid value '%s'", Text).c_str());
				return false;
			}
			return true;
		};

		if (Parser.ReadAttribBool(Node, "uber_deffer"))
			Add(EBlendOp::UberDeffer, {});

		if (XML_NODE* Blend = Parser.NavigateToNode(Node, "blend"))
		{
			u32 Src = 0, Dest = 0;
			if (!ReadNamed(BlendNames, Blend, "src", "zero", Src) || !ReadNamed(BlendNames, Blend, "dest", "zero", Dest))
				return false;
			Add(EBlendOp::Blend, { u32(Parser.ReadAttribBool(Blend, "status")), Src, Dest });
		}

		if (XML_NODE* Sorting = Parser.NavigateToNode(Node, "sort"))
			Add(EBlendOp::Sorting, { u32(Parser.ReadAttribInt(Sorting, "count", 1)), u32(Parser.ReadAttribBool(Sorting, "status")) });

		if (XML_NODE* Cull = Parser.NavigateToNode(Node, "cull_mode"))
		{
			u32 Mode = 0;
			if (!ReadNamed(CullNames, Cull, "mode", "none", Mode))
				return false;
			Add(EBlendOp::CullMode, { Mode });
		}

		if (XML_NODE* Flags = Parser.NavigateToNode(Node, "flags"))
		{
			Add(EBlendOp::Distort, { u32(Parser.ReadAttribBool(Flags, "dist")) });
			Add(EBlendOp::Emissive, { u32(Parser.ReadAttribBool(Flags, "emissive")) });
			Add(EBlendOp::Wmark, { u32(Parser.ReadAttribBool(Flags, "wmark")) });
		}

		if (XML_NODE* Atoc = Parser.NavigateToNode(Node, "atoc"))
			Add(EBlendOp::AlphaToCoverage, { u32(Parser.ReadAttribBool(Atoc, "status")) });

		if (XML_NODE* Aref = Parser.NavigateToNode(Node, "aref"))
			Add(EBlendOp::Aref, { u32(Parser.ReadAttribBool(Aref, "status")), u32(Parser.ReadAttribInt(Aref, "count", 0)) });

		if (XML_NODE* Color = Parser.NavigateToNode(Node, "color"))
		{
			Add(EBlendOp::ColorWrite, { u32(Parser.ReadAttribBool(Color, "r")), u32(Parser.ReadAttribBool(Color, "g")),
				u32(Parser.ReadAttribBool(Color, "b")), u32(Parser.ReadAttribBool(Color, "a")) });
		}

		if (XML_NODE* Stencil = Parser.NavigateToNode(Node, "stencil"))
		{
			u32 Cmp = 0, Fail = 0, PassOp = 0, ZFail = 0;
			if (!ReadNamed(CmpNames, Stencil, "cmp", "never", Cmp) || !ReadNamed(StencilNames, Stencil, "fail", "zero", Fail) ||
				!ReadNamed(StencilNames, Stencil, "pass", "zero", PassOp) || !ReadNamed(StencilNames, Stencil, "zfail", "zero", ZFail))
				return false;
			Add(EBlendOp::Stencil, { u32(Parser.ReadAttribBool(Stencil, "status")), Cmp, u32(Parser.ReadAttribInt(Stencil, "mask", 0)),
				u32(Parser.ReadAttribInt(Stencil, "wmask", 0)), Fail, PassOp, ZFail });

			if (XML_NODE* StencilRef = Parser.NavigateToNode(Stencil, "ref"))
				Add(EBlendOp::StencilRef, { u32(Parser.ReadAttribInt(StencilRef, "value", 0)) });
		}

		for (int Index = 0; XML_NODE* Texture = Parser.NavigateToNode(Node, "texture", Index); ++Index)
		{
			const xr_string Name = Parser.ReadAttrib(Texture, "name", "none");
			const xr_string Rt = Parser.ReadAttrib(Texture, "rt", "none");
			const xr_string Append = Parser.ReadAttrib(Texture, "append", "none");
			const xr_string Dest = Parser.ReadAttrib(Texture, "dest", "none");
			if (Name == "none" || Rt == "none")
				continue;

			if (Rt == "t_base")
			{
				const xr_string Base = xr_string("{base}") + (Append != "none" ? Append : xr_string());
				const xr_string Resource = Dest != "none" ? xr_string(Name + Dest) : Name;
				Add(EBlendOp::Texture, {}, { Resource, Base });
			}
			else
			{
				Add(EBlendOp::Texture, {}, { Name, Rt });
			}
		}

		for (int Index = 0; XML_NODE* Sampler = Parser.NavigateToNode(Node, "sampler", Index); ++Index)
		{
			const xr_string Name = Parser.ReadAttrib(Sampler, "name", "none");
			if (Name != "none")
				Add(EBlendOp::Sampler, {}, { Name });
		}

		Result.Dynamic.Elements.push_back(std::move(Item));
	}
	return true;
}

void XMLBlendV2::Write(const Script& Source, xr_string& Text)
{
	Text = "<shader version=\"2\"";
	if (Source.IsStaticBlend)
		Text += " r1_static=\"1\"";
	Text += ">\r\n";

	WriteSet(Source.Dynamic, "\t", Text);
	if (Source.HasStaticSet)
	{
		Text += "\t<static>\r\n";
		WriteSet(Source.Static, "\t\t", Text);
		Text += "\t</static>\r\n";
	}
	Text += "</shader>\r\n";
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
