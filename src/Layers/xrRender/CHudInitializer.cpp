#include "stdafx.h"

#include "CHudInitializer.h"

static u32 s_hud_depth = 0;
static Fmatrix s_mView_saved;
static Fmatrix s_mProject_saved;
static Fmatrix s_mFullTransform_saved;

CHudInitializer::CHudInitializer(bool setup, bool ajust)
{
	b_auto_setup = setup;
	b_ajust = ajust;

	if (!b_auto_setup) return;

	if (s_hud_depth == 0)
	{
		s_mView_saved = Device.mView;
		s_mProject_saved = Device.mProject;
		s_mFullTransform_saved = Device.mFullTransform;
		SetHudMode();
	}
	s_hud_depth++;
}

CHudInitializer::~CHudInitializer()
{
	if (!b_auto_setup) return;

	VERIFY(s_hud_depth > 0);
	s_hud_depth--;

	if (s_hud_depth == 0)
	{
		SetDefaultMode();
	}
}

void CHudInitializer::SetHudMode()
{
	Device.mView.set(Device.mView_hud);
	Device.mProject.set(Device.mProject_hud);
	Device.mFullTransform.set(Device.mFullTransform_hud);

	if(Device.m_pRender) 
	{
		Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);
		Device.m_pRender->SetCacheXformOld(Device.mView_hud_old, Device.mProject_hud_old);
	}

	if (b_ajust)
	{
		::Render->rmNear();
	}
}

void CHudInitializer::SetDefaultMode()
{
	if (b_ajust)
	{
		::Render->rmNormal();
	}

	Device.mView.set(s_mView_saved);
	Device.mProject.set(s_mProject_saved);
	Device.mFullTransform.set(s_mFullTransform_saved);

	if(Device.m_pRender) 
	{
		Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);
		Device.m_pRender->SetCacheXformOld(Device.mView_old, Device.mProject_old);
	}
}

