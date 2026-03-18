#pragma once

#include "UIWindow.h"

class CUIProgressBar;
class CUIStatic;

class UI_API CUIItemStateDisplay final : public CUIWindow
{
	friend class CUIXmlInit;
	using inherited = CUIWindow;

public:
	enum class EDisplayMode
	{
		Bar,
		Percent,
		Count
	};

	enum class EPercentFormat
	{
		Percent,
		Number,
		Fraction,
		Portion,
		Count
	};

	void SetState(float state);
	void SetPortion(int current, int max);
	float GetState() const { return _value; }
	EPercentFormat GetPercentFormat() const { return _percentFormat; }

	void SetProgressPos(float pos) { SetState(pos); }
	float GetProgressPos() const { return GetState(); }

	void ShowBackground(bool status);
	bool IsShownBackground() const { return _showBackground; }

	// For bar mode compatibility (e.g. eatable items with uses)
	bool m_bUseGradient;

	virtual void Draw() override;
	virtual void Update() override;

	virtual CUIWindow* ui_cast_window() override { return this; }

	CUIItemStateDisplay();
	virtual ~CUIItemStateDisplay();

protected:
	EDisplayMode _mode;
	float _value;

	// Bar mode
	CUIProgressBar* _progressBar;
	bool _showBackground;

	// Percent mode
	EPercentFormat _percentFormat;
	int _fractionMax;
	int _portionCurrent;
	int _portionMax;
	bool _hasPortionData;
	CUIStatic* _percentBackground;
	CUIStatic* _percentText;

private:
	void updatePercentText();
};
