#include "StdAfx.h"
#include "UIItemParam.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrEngine/string_table.h"

namespace ItemParamXml
{
static void ApplyDefaults(tinyxml2::XMLElement& Target, const tinyxml2::XMLElement& Defaults)
{
	for (const auto* Attribute = Defaults.FirstAttribute(); Attribute; Attribute = Attribute->Next())
	{
		if (xr_strcmp(Attribute->Name(), "template") && !Target.Attribute(Attribute->Name()))
		{
			Target.SetAttribute(Attribute->Name(), Attribute->Value());
		}
	}
	if (!Target.GetText() && Defaults.GetText())
	{
		Target.SetText(Defaults.GetText());
	}
	for (const auto* Child = Defaults.FirstChildElement(); Child; Child = Child->NextSiblingElement())
	{
		if (auto* Existing = Target.FirstChildElement(Child->Name()))
		{
			ApplyDefaults(*Existing, *Child);
		}
		else
		{
			Target.InsertEndChild(Child->DeepClone(Target.GetDocument()));
		}
	}
}

static bool Resolve(CUIXml& Xml, const char* Section)
{
	XML_NODE* Node = Xml.NavigateToNode(Section);
	const bool IsPlain = !Node;
	if (IsPlain)
	{
		Node = Xml.NavigateToNode((xr_string("static_") + Section).c_str());
	}
	auto* Row = Node ? Node->ToElement() : nullptr;
	if (!Row || !Row->Attribute("template"))
	{
		return true;
	}
	const char* TemplateName = Row->Attribute("template");
	for (u32 Depth = 0; TemplateName && Depth < 16; ++Depth)
	{
		XML_NODE* TemplateNode = Xml.NavigateToNode((xr_string("param_templates:") + TemplateName).c_str());
		const auto* Template = TemplateNode ? TemplateNode->ToElement() : nullptr;
		if (!Template)
		{
			Msg("! UI parameter %s: missing template %s", Section, TemplateName);
			return false;
		}
		ApplyDefaults(*Row, *Template);
		TemplateName = Template->Attribute("template");
	}
	if (TemplateName)
	{
		Msg("! UI parameter %s: template inheritance exceeds 16 levels", Section);
		return false;
	}
	if (IsPlain && !Row->FirstChildElement("texture"))
	{
		const auto* Caption = Row->FirstChildElement("caption");
		const auto* Texture = Caption ? Caption->FirstChildElement("texture") : nullptr;
		if (Texture)
		{
			Row->InsertEndChild(Texture->DeepClone(Row->GetDocument()));
		}
	}
	return true;
}
} // namespace ItemParamXml

UIItemParam::UIItemParam()
{
	Caption = nullptr;
	ValueText = nullptr;
	m_magnitude = 1.0f;
	IsSignInverse = false;
	LegacyTextWidget = nullptr;
}

UIItemParam::~UIItemParam() = default;

UIItemParam::InitResult UIItemParam::Init(CUIXml& Xml, const char* Section)
{
	if (!ItemParamXml::Resolve(Xml, Section))
	{
		return InitResult::Failed;
	}
	const bool IsAbsolute = !xr_strcmp(Section, "condition") || !xr_strcmp(Section, "af_slots") || !xr_strcmp(Section, "boost_time");
	PositiveColor = CUIXmlInit::GetColor(Xml, "positive_color", 0, color_argb(255, 44, 172, 246));
	ShowSign = !IsAbsolute;
	ColorizeValue = false;
	if (!CUIXmlInit::InitStatic(Xml, Section, 0, this, false))
	{
		return InitPlain(Xml, Section);
	}

	XML_NODE* BaseNode = Xml.GetLocalRoot();

	Xml.SetLocalRoot(Xml.NavigateToNode(Section));

	Caption = UIHelper::CreateStatic(Xml, "caption", this);
	ValueText = UIHelper::CreateStatic(Xml, "value", this);
	m_magnitude = Xml.ReadAttribFlt("value", 0, "magnitude", 1.0f);
	IsSignInverse = (Xml.ReadAttribInt("value", 0, "sign_inverse", 0) == 1);
	ShowSign = Xml.ReadAttribInt("value", 0, "show_sign", ShowSign) != 0;
	ColorizeValue = Xml.ReadAttribInt("value", 0, "colorize", ColorizeValue) != 0;
	ColorizeText = Xml.ReadAttribInt("value", 0, "text_colorize", ColorizeValue) != 0;

	const char* UnitText = Xml.ReadAttrib("value", 0, "unit_str", "");
	Unit._set(g_pStringTable->translate(UnitText));

	NegativeTexture = Xml.Read("texture_minus", 0, "");
	PositiveTexture = Xml.Read("caption:texture", 0, "");
	if (ColorizeValue && PositiveTexture.size())
	{
		Caption->InitTextureEx(PositiveTexture.c_str(), "hud\\font");
	}
	Xml.SetLocalRoot(BaseNode);
	return InitResult::Normal;
}

UIItemParam::InitResult UIItemParam::InitPlain(CUIXml& Xml, const char* Section)
{
	string256 Buffer;
	xr_strconcat(Buffer, "static_", Section);
	if (!CUIXmlInit::InitStatic(Xml, Buffer, 0, this, false))
	{
		return InitResult::Failed;
	}
	Caption = new CUIStatic();
	Caption->SetAutoDelete(true);
	AttachChild(Caption);
	Caption->Show(false); // HACK: Legacy layouts combine the caption and value in one text widget.

	ValueText = new CUIStatic();
	ValueText->SetAutoDelete(true);
	AttachChild(ValueText);
	ValueText->Show(false); // HACK: Legacy layouts combine the caption and value in one text widget.

	ColorizeValue = Xml.ReadAttribInt(Buffer, 0, "colorize", 0) != 0;
	ColorizeText = Xml.ReadAttribInt(Buffer, 0, "text_colorize", ColorizeValue) != 0;
	TextOff();
	LegacyTextWidget = UIHelper::CreateStatic(Xml, Buffer, this);
	PositiveTexture = Xml.Read((xr_string(Buffer) + ":texture").c_str(), 0, "");
	if (ColorizeValue && PositiveTexture.size())
	{
		InitTextureEx(PositiveTexture.c_str(), "hud\\font");
	}
	LegacyTextWidget->TextureOff();
	LegacyTextWidget->SetWidth(1000.f); // HACK: Leave enough width for legacy complex text.

	return InitResult::Plain;
}

void UIItemParam::SetDefaultValuesPlain(float Magnitude, bool IsInverse, const shared_str& UnitValue)
{
	m_magnitude = Magnitude;
	IsSignInverse = IsInverse;
	Unit = UnitValue;
}
void UIItemParam::SetCaption(const char* Name)
{
	if (!Caption || !Name)
	{
		return;
	}

	Caption->TextItemControl()->SetText(Name);
}

void UIItemParam::SetValue(float Value, bool ApplyMagnitude)
{
	if (!ValueText)
	{
		return;
	}

	if (ApplyMagnitude)
	{
		Value *= m_magnitude;
	}
	string32 Buffer;
	xr_sprintf(Buffer, ShowSign ? "%+.0f" : "%.0f", Value);

	string256 Text;
	if (Unit.size())
	{
		xr_strconcat(Text, Buffer, " ", Unit.c_str());
	}
	else
	{
		xr_strconcat(Text, Buffer);
	}
	ValueText->SetText(Text);

	constexpr u32 Red = color_argb(255, 210, 50, 50);
	constexpr u32 Neutral = color_argb(255, 170, 170, 170);
	const bool IsPositive = IsSignInverse ? Value < 0.0f : Value >= 0.0f;
	// Consumable panels historically keep both signs gray; artefacts mark harmful values red.
	const u32 LegacyColor = !ApplyMagnitude || IsPositive ? Neutral : Red;
	const u32 Color = ColorizeValue ? (IsPositive ? PositiveColor : Red) : LegacyColor;
	const u32 TextColor = ColorizeText ? (IsPositive ? PositiveColor : Red) : LegacyColor;
	ValueText->SetTextColor(TextColor);

	if (Caption && PositiveTexture.size() && (ColorizeValue || NegativeTexture.size()))
	{
		const shared_str& Texture = !IsPositive && NegativeTexture.size() ? NegativeTexture : PositiveTexture;
		Caption->InitTextureEx(Texture.c_str(), ColorizeValue ? "hud\\font" : "hud\\default");
		if (ColorizeValue)
		{
			Caption->SetTextureColor(Color);
		}
	}
	if (LegacyTextWidget && ColorizeValue)
	{
		SetTextureColor(Color);
	}

	// HACK: Legacy layouts combine the caption and value in one text widget.
	if (Caption && LegacyTextWidget && !Caption->IsShown() && !ValueText->IsShown())
	{
		string512 LegacyText;
		if (ColorizeValue || ColorizeText)
		{
			xr_sprintf(LegacyText, "%s %%c[%u,%u,%u,%u] %s", Caption->GetText(), color_get_A(TextColor), color_get_R(TextColor), color_get_G(TextColor), color_get_B(TextColor), ValueText->GetText());
		}
		else
		{
			xr_sprintf(LegacyText, "%s %s %s", Caption->GetText(), IsPositive ? "%c[green]" : "%c[red]", ValueText->GetText());
		}
		LegacyTextWidget->SetText(LegacyText);
	}
}

void UIItemParam::SetValueText(const char* Text)
{
	if (ValueText)
	{
		ValueText->SetText(Text ? Text : "");
		ValueText->SetTextColor(color_rgba(170, 170, 170, 255));
	}
}

void UIItemParam::ShowValue(bool IsShown)
{
	if (ValueText)
	{
		ValueText->Show(IsShown);
		if (!IsShown)
		{
			ValueText->SetText("");
		}
	}
}
