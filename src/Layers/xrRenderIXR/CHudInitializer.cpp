#include "stdafx.h"
#include "RenderThread.h"

#include "CHudInitializer.h"

static u32 s_hud_depth = 0;
static Fmatrix s_mView_saved;
static Fmatrix s_mProject_saved;
static Fmatrix s_mFullTransform_saved;

CHudInitializer::CHudInitializer(GfxCmdList& Cmd, bool setup, bool ajust) : Cmd(Cmd)
{
	b_auto_setup = setup;
	b_ajust = ajust;

	if (!b_auto_setup) return;

	if (s_hud_depth == 0)
	{
		s_mView_saved = RFrame.mView;
		s_mProject_saved = RFrame.mProject;
		s_mFullTransform_saved = RFrame.mFullTransform;
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

void CHudInitializer::SetHudMode(bool IsCameraRelative)
{
	// Light positions and volumes stay in world space for spatial queries and shadow maps.
	RFrame.mView.set(IsCameraRelative ? RFrame.mView_hud : RFrame.mView_saved);
	RFrame.mProject.set(RFrame.mProject_hud);
	RFrame.mFullTransform.mul(RFrame.mProject, RFrame.mView);
	// The game's code that runs while the HUD is drawn reads the device's camera: it gets the HUD's too
	if (!RenderThread::IsPipelined())
	{
		Device.mView.set(RFrame.mView);
		Device.mProject.set(RFrame.mProject);
		Device.mFullTransform.set(RFrame.mFullTransform);
	}

	if (Device.m_pRender)
	{
		Cmd.SetView({ RFrame.mView, RFrame.mProject, IsCameraRelative ? RFrame.mView_hud_old : RFrame.mView_old, RFrame.mProject_hud_old });
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

	RFrame.mView.set(s_mView_saved);
	RFrame.mProject.set(s_mProject_saved);
	RFrame.mFullTransform.set(s_mFullTransform_saved);
	if (!RenderThread::IsPipelined())
	{
		Device.mView.set(RFrame.mView);
		Device.mProject.set(RFrame.mProject);
		Device.mFullTransform.set(RFrame.mFullTransform);
	}

	if(Device.m_pRender)
	{
		Cmd.SetView({ RFrame.mView, RFrame.mProject, RFrame.mView_old, RFrame.mProject_old });
	}
}

