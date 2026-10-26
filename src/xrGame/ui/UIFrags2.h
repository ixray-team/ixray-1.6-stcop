#pragma once

#include "UIFrags.h"

class CUIFrags2 : public CUIFrags{
public:
	CUIFrags2();
	virtual ~CUIFrags2() override;
	void Init(CUIXml& xml_doc, const char* path, const char* backgrnd_path);

protected:
	CUIStats*	m_pStats2;
};