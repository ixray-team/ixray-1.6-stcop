#pragma once
#include "../../xrUI/Widgets/UIStatic.h"

class CUIXml;

class UIItemParam : public CUIStatic
{
public:
	UIItemParam();
	~UIItemParam() override;

	enum class InitResult
	{
		Failed,
		Normal,
		Plain
	};

	InitResult Init(CUIXml& Xml, const char* Section);

	void SetDefaultValuesPlain(float Magnitude, bool IsInverse, const shared_str& UnitValue);
	void SetCaption(const char* Name);
	void SetValue(float Value, bool ApplyMagnitude = true);

	void SetValueText(const char* Text);
	void ShowValue(bool IsShown);

	CUIWindow* ui_cast_window() override { return this; }
	bool GetLegacyMode() const { return !(Caption && Caption->IsShown() && ValueText && ValueText->IsShown()); }

	float m_magnitude;

protected:
	InitResult InitPlain(CUIXml& Xml, const char* Section);

private:
	CUIStatic* Caption;
	CUIStatic* ValueText;
	CUIStatic* LegacyTextWidget;
	bool IsSignInverse;
	bool ShowSign = true;
	bool ColorizeValue = false;
	bool ColorizeText = false;
	u32 PositiveColor = color_argb(255, 44, 172, 246);
	shared_str Unit;
	shared_str NegativeTexture;
	shared_str PositiveTexture;

}; // class UIItemParam
