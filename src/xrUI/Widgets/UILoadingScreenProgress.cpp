////////////////////////////////////////////////////////////////////////////
//	Module 		: UILoadingScreenProgress.cpp
//	Created 	: 17.07.2025
//	Author		: St4lker0k765
//	Description : Smooth progress bar for loading screen implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "UILoadingScreenProgress.h"

#include "UIStatic.h"
#include "../Include/xrRender/UIShader.h"
#include "../Include/xrRender/UIRender.h"

CUILoadingScreenProgress::CUILoadingScreenProgress()
{
	m_stage				= 0;
	m_sectorCount		= 40;
	m_double_progress	= false;
};

CUILoadingScreenProgress::~CUILoadingScreenProgress()
{
}	

void CUILoadingScreenProgress::SetPos(int pos, int max)
{
    m_stage = float(pos) / float(max);
}

void CUILoadingScreenProgress::SetPos(float pos) 
{
    m_stage = pos;
}

u32 calc_progress_color(u32 idx, u32 total, float stage, float max_stage, bool useLegacyCount)
{
    if (useLegacyCount && idx > (total / 2))
        idx = total - idx;

	float kk = ( stage/max_stage ) *  (float(total+1));
    float f = 1 / (exp((float(idx) - kk) * 0.9f) + 1.0f);

    return color_argb_f(f, 1.0f, 1.0f, 1.0f);
}

void CUILoadingScreenProgress::Draw()
{
	UIRender->SetShader				(*GetShader());

	Fvector2						tsize;
	UIRender->GetActiveTextureResolution(tsize);

	
	UIRender->StartPrimitive		(2 * (m_sectorCount + 1),IUIRender::ptTriStrip, UI().m_currentPointType);
	
    Frect back_coords = GetWndRect();

    UI().ClientToScreenScaled(back_coords.lt, back_coords.x1, back_coords.y1);
    UI().ClientToScreenScaled(back_coords.rb, back_coords.x2, back_coords.y2);

    Frect back_tex_coords = GetUIStaticItem().GetTextureRect();

    back_tex_coords.lt.x /= tsize.x;
    back_tex_coords.lt.y /= tsize.y;
    back_tex_coords.rb.x /= tsize.x;
    back_tex_coords.rb.y /= tsize.y;

    static float offs = -0.5f;
    float pos_delta = back_coords.width() / m_sectorCount;
    float tc_delta = back_tex_coords.width() / m_sectorCount;

    for (u32 idx = 0; idx < m_sectorCount + 1; ++idx)
    {
        u32 clr = calc_progress_color(idx, m_sectorCount, m_stage, 1.0f, m_double_progress);
        UIRender->PushPoint(back_coords.lt.x + pos_delta * idx + offs, back_coords.rb.y + offs, 0 + EPS_S, clr, back_tex_coords.lt.x + tc_delta * idx, back_tex_coords.rb.y);
        UIRender->PushPoint(back_coords.lt.x + pos_delta * idx + offs, back_coords.lt.y + offs, 0 + EPS_S, clr, back_tex_coords.lt.x + tc_delta * idx, back_tex_coords.lt.y);
    }

	UIRender->FlushPrimitive();
}
