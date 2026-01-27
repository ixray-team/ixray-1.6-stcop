#include "stdafx.h"
#include "FPSCounter.h"

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
	static float smoothed_fps = 60.0f;
	static float visible_fps  = 60.0f;

	if (Device.fTimeDelta > EPS_S)
	{
		if (!is_initialized_)
		{
			last_time_ = clock::now();
			is_initialized_ = true;
		}
		
		int raw_fps  = lroundf(1.f / Device.fTimeDelta);
		smoothed_fps = static_cast<float>(raw_fps) * fps_smoothing_alpha + 
					   smoothed_fps * (1.f - fps_smoothing_alpha);

		if (time_point now = clock::now(); now - last_time_ >= std::chrono::milliseconds(1000 / 2))
		{
			visible_fps = smoothed_fps;
			last_time_ = now;
		}

		shared_str text = shared_str().printf("FPS: %d", static_cast<int>(visible_fps));
		
		if (strcmp(fps_text_pos_tokens[fps_text_current_pos].name, "top-left") == 0)
		{
#ifndef MASTER_GOLD
			font_->OutSet(
					10,
					CImGuiManager::Instance().IsGUIRendering() ? text_screen_padding + 20 : text_screen_padding
				);
#else
			font_->OutSet(
					10,
					text_screen_padding
				);
#endif
		}
		else if (strcmp(fps_text_pos_tokens[fps_text_current_pos].name, "top-right") == 0)
		{
#ifndef MASTER_GOLD
			font_->OutSet(
					static_cast<float>(Device.TargetWidth) - font_->SizeOf_(*text) - 10,
					CImGuiManager::Instance().IsGUIRendering() ? text_screen_padding + 20 : text_screen_padding
				);
#else
			font_->OutSet(
					static_cast<float>(Device.TargetWidth) - font_->SizeOf_(*text) - 10,
					text_screen_padding
				);
#endif
		}
		else if (strcmp(fps_text_pos_tokens[fps_text_current_pos].name, "bottom-left") == 0)
		{
			font_->OutSet(
					text_screen_padding,
					static_cast<float>(Device.TargetHeight) - font_->GetHeight() - 10
				);
		}
		else if (strcmp(fps_text_pos_tokens[fps_text_current_pos].name, "bottom-right") == 0)
		{
			font_->OutSet(
					static_cast<float>(Device.TargetWidth) - font_->SizeOf_(*text) - text_screen_padding,
					static_cast<float>(Device.TargetHeight) - font_->GetHeight() - text_screen_padding
				);
		}
		
		font_->SetColor(text_color);
		font_->OutNext(*text);
		font_->OnRender();
	}
}