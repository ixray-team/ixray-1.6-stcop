#include "stdafx.h"
#include "FPSCounter.h"

float fps_smoothing_alpha = .125f; // 1/8 dostato4no plavno
ENGINE_API XRay::Hardware::FPSCounter* pFPSCounter = nullptr;

XRay::Hardware::FPSCounter::FPSCounter()
{
	font_ = g_FontManager->CloneFont("ui_font_console");
	VERIFY(font_);
}

u32 fps_text_current_pos = 1;
xr_token fps_text_pos_tokens[5] = {
	{"top-left", 0},
	{"top-right", 1},
	{"bottom-left", 2},
	{"bottom-right", 3},
	{0, 0}
};

void XRay::Hardware::FPSCounter::OnRender()
{
	float dt = Device.fTimeDelta;

	if (dt < EPS_S)
		return;

	fps = ema_smoothe(1.f / dt, fps, fps_smoothing_alpha);
	ft = ema_smoothe(dt * 1000.f, ft, fps_smoothing_alpha);

	font_->OutNext(*shared_str().printf("FPS: %.0f (%.2fms)", fps, ft));

	if (strcmp(fps_text_pos_tokens[fps_text_current_pos].name, "top-left") == 0)
	{
#ifndef MASTER_GOLD
		font_->OutLeft(text_screen_padding);
		font_->OutTop(CImGuiManager::Instance().IsGUIRendering() ? text_screen_padding + 20 : text_screen_padding);
#else
		font_->OutLeft(text_screen_padding);
		font_->OutTop(text_screen_padding);
#endif
	}
	else if (strcmp(fps_text_pos_tokens[fps_text_current_pos].name, "top-right") == 0)
	{
#ifndef MASTER_GOLD
		font_->OutRight(text_screen_padding);
		font_->OutTop(CImGuiManager::Instance().IsGUIRendering() ? text_screen_padding + 20 : text_screen_padding);
#else
		font_->OutRight(text_screen_padding);
		font_->OutTop(text_screen_padding);
#endif
	}
	else if (strcmp(fps_text_pos_tokens[fps_text_current_pos].name, "bottom-left") == 0)
	{
		font_->OutLeft(text_screen_padding);
		font_->OutBottom(text_screen_padding);
	}
	else if (strcmp(fps_text_pos_tokens[fps_text_current_pos].name, "bottom-right") == 0)
	{
		font_->OutRight(text_screen_padding);
		font_->OutBottom(text_screen_padding);
	}

	font_->SetColor(text_color);
	font_->OnRender();
}
