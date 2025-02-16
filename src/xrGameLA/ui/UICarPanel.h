#pragma once

#include "uiwindow.h"
#include "uipointergage.h"


class CUICarPanel : public CUIWindow
{
private:
	typedef CUIWindow inherited;

	CUIStatic			UIStaticCarHealth;
	CUIProgressBar		UICarHealthBar;
	CUIStatic			UIStaticCarFuel;
	CUIProgressBar		UICarFuelBar;
	CUIPointerGage		UISpeedometer;
	CUIPointerGage		UITachometer;
public: 

	// Установить 
	void				SetCarHealth	(float value);
	void				SetSpeed		(float speed);
	void				SetRPM			(float rmp);
	void				SetFuel			(float fuel);
	void				Init			(float x, float y, float width, float height);
};