#pragma once

#include "../ui/ArtefactDetectorUI.h"
#include "Dosimeter.h"

class CUIDosimeter : 
	public CUIArtefactDetectorBase,
	public CUIWindow
{
public:
	void update() override;
	void Draw() override;

	void construct(CDosimeter* p);

private:
	CUIStatic* m_wrk_area;
	CUIStatic* m_seg1;
	CUIStatic* m_seg2;
	CUIStatic* m_seg3;
	CUIStatic* m_seg4;
	CDosimeter* m_parent;
	Fmatrix m_map_attach_offset;

	void GetUILocatorMatrix(Fmatrix& _m);

	// Признак работы прибора: мигающая точка в правом нижнем углу
	CUIStatic* m_workIndicator;
	const u32 WORK_PERIOD = 1000; // Период мигания индикатора
	u32 m_workTick; // Время переключения индикатора

	// Эмуляция шума при измерении: младший разряд меняется в пределях 8 единиц
	float m_noise; // Величина шума
	const u32 NOISE_PERIOD = 3000; // Период перерасчета шума
	u32 m_noiseTick; // Время последнего перерасчета шума
};