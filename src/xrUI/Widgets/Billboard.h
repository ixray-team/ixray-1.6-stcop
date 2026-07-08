#pragma once

//#include "UIWindow.h"
#include "UIStatic.h"

/*
	TODO Emmis:
	ћо€ иде€ реализации билборда така€: написать некий Canvas, в который можно рендерить
	UI, возможно через TargetTexture или еще каким-то неизвестным мне способом. Ѕилборд будет в 3д координатах мира
	и крутить этот Canvas всегда лицом в камеру. 
	“аким образом не надо паритьс€ насчет override рендера UI. ќн просто будет рендеритс€ не на экран, а в Canvas

	Ёто просто временна€ заглушка чисто дл€ рендера текста дл€ субтитров, ну и € там баловалс€ с рендером текстурки
*/
class UI_API CBillboard : public CUIStatic
{
public:
	CBillboard(const char* xml_node);
	~CBillboard();

	void DrawBillboard(const Fvector& world_pos);

	void DrawTexture() override;
	void DrawText() override;

private:
	typedef CUIStatic base;

	Fvector m_currentWorldPosition;

	Fvector3 m_offset;
};