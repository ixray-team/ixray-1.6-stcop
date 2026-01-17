#pragma once

#include "../ui/ArtefactDetectorUI.h"
#include "Dosimeter.h"

class CUIDosimeter final : 
	public CUIArtefactDetectorBase,
	public CUIWindow
{
public:
	void update() override;
	void Draw() override;

	void construct(CDosimeter* p);

private:
	CUIStatic* m_wrk_area = nullptr;
	CUIStatic* m_seg1 = nullptr;
	CUIStatic* m_seg2 = nullptr;
	CUIStatic* m_seg3 = nullptr;
	CUIStatic* m_seg4 = nullptr;
	CDosimeter* m_parent = nullptr;
	Fmatrix m_map_attach_offset;

	void GetUILocatorMatrix(Fmatrix& _m);

	// Признак работы прибора: мигающая точка в правом нижнем углу
	CUIStatic* m_workIndicator = nullptr;
	const u32 WORK_PERIOD = 1000; // Период мигания индикатора
	u32 m_workTick = 0; // Время переключения индикатора

	// Эмуляция шума при измерении: младший разряд меняется в пределях 8 единиц
	float m_noise = 0.0f; // Величина шума
	const u32 NOISE_PERIOD = 3000; // Период перерасчета шума
	u32 m_noiseTick = 0; // Время последнего перерасчета шума
};