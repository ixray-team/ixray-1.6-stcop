#pragma once

#include "UIStatic.h"

class CUIStatic;

class UI_API CUILoadingScreenProgress : 
	public CUIStatic
{
	friend class CUIXmlInit;
public:
						CUILoadingScreenProgress		();
	virtual				~CUILoadingScreenProgress		();
	void				SetPos					(int pos, int max);
	void				SetPos					(float pos);

	virtual void		Draw					();
	bool				m_double_progress;

protected:
	u32					m_sectorCount;
	float				m_stage;
};