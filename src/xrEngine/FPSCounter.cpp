#include "stdafx.h"
#include "FPSCounter.h"

int fps_update_per_second = 2;
float fps_smoothing_alpha = 1.f / 3.f;
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
	static float smoothed_fps  = 60.0f;
	static float visible_fps   = 60.0f;
	float		 current_delta = Device.fTimeDelta;
	
	if (current_delta < EPS_S)
		return;
	
	if (!is_initialized_)
	{
		last_time_ = clock::now();
		is_initialized_ = true;
	}
		
	int raw_fps  = static_cast<int>(1.f / current_delta);
	smoothed_fps = static_cast<float>(raw_fps) * fps_smoothing_alpha + 
				   smoothed_fps * (1.f - fps_smoothing_alpha);

	if (time_point now = clock::now(); now - last_time_ >= std::chrono::milliseconds(1000 / fps_update_per_second))
	{
		visible_fps = smoothed_fps;
		last_time_ = now;
	}

	shared_str text = shared_str().printf("FPS: %d", static_cast<int>(visible_fps));
	font_->OutNext(*text);
		
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