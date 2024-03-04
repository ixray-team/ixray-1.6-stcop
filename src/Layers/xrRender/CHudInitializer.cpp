#include "CHudInitializer.h"
#include "device.h"

CHudInitializer::CHudInitializer(bool setup)
{
	b_auto_setup = setup;

	mView_saved = Device.mView;
	mProject_saved = Device.mProject;
	mFullTransform_saved = Device.mFullTransform;

	if (!b_auto_setup) return;

	SetHudMode();
}

CHudInitializer::~CHudInitializer()
{
	if (!b_auto_setup) return;

	SetDefaultMode();
}

void CHudInitializer::SetHudMode()
{
	Device.mView.set(Device.mView_hud);
	Device.mProject.set(Device.mProject_hud);
	Device.mFullTransform.set(Device.mFullTransform_hud);

	Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);
	Device.m_pRender->SetCacheXformOld(Device.mView_hud_old, Device.mProject_hud_old);
}

void CHudInitializer::SetDefaultMode()
{
	Device.mView.set(mView_saved);
	Device.mProject.set(mProject_saved);
	Device.mFullTransform.set(mFullTransform_saved);

	Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);
	Device.m_pRender->SetCacheXformOld(Device.mView_old, Device.mProject_old);
}
