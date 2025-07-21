#pragma once

#include "UIStatic.h"

class UI_API CUIArrow:
	public CUIStatic
{
private:
	typedef CUIStatic	inherited;

public:
					CUIArrow		();
	virtual			~CUIArrow		();

			void	init_from_xml	( CUIXml& xml, LPCSTR path, CUIWindow* parent );
			void	SetNewValue		( float new_value );
			void	SetPos			( float pos );
	IC		float	GetPos			()	{	return m_pos;	}

private:
	float		m_angle_begin;
	float		m_angle_end;
	float		m_ang_velocity;
	float		m_angle_range;

	float		m_temp_pos;
	float		m_pos;

};
