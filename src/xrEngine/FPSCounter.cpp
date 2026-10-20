#include "stdafx.h"
#include "FPSCounter.h"

float fps_smoothing_alpha = .1f;
ENGINE_API XRay::Hardware::FPSCounter* pFPSCounter = nullptr;

u32 fps_text_current_pos = 1;
u32 fps_text_current_font = 1;
int fps_text_color_r = 0;
int fps_text_color_g = 255;
int fps_text_color_b = 0;
int fps_text_color_a = 255;
bool fps_text_outline = true;

xr_token fps_text_pos_tokens[5] = 
{
	{"top-left", 0},
	{"top-right", 1},
	{"bottom-left", 2},
	{"bottom-right", 3},
	{0, 0}
};

xr_token fps_font_tokens[] = 
{
	{"stat_font", 0},
	{"ui_font_console", 1},
	{"hud_font_medium", 2},
	{"ui_font_letterica16_russian", 3},
	{"ui_font_letterica18_russian", 4},
	{0, 0}
};

constexpr float fps_update_interval = 0.23f;

float accum_time = 0.f;
u32 accum_frames = 0;

XRay::Hardware::FPSCounter::FPSCounter()
{
	for (int i = 0; fps_font_tokens[i].name != nullptr; ++i)
	{
		fonts_.push_back(nullptr);
	}

	UpdateFont();
}

void XRay::Hardware::FPSCounter::UpdateFont()
{
	const u32 index = std::min<u32>(fps_text_current_font, u32(fonts_.size() - 1));

	if (fonts_[index] == nullptr)
	{
		fonts_[index] = g_FontManager->CloneFont(fps_font_tokens[index].name);
		VERIFY(fonts_[index]);
	}

	font_ = fonts_[index];
}

void XRay::Hardware::FPSCounter::OnRender()
{
	float dt = Device.fTimeDeltaContinual;

	if (dt < EPS_S)
	{
		return;
	}

	accum_time += dt;
	accum_frames += 1;

	if (accum_time >= fps_update_interval)
	{
		fps = accum_frames / accum_time;
		ft = (accum_time / accum_frames) * 1000.f;

		accum_time = 0.f;
		accum_frames = 0;
	}

	UpdateFont();

	shared_str text;
	text.printf("FPS: %.0f (%.2fms)", fps, ft);

	float x = text_screen_padding;
	float y = text_screen_padding;

#ifndef MASTER_GOLD
	if (CImGuiManager::Instance().IsGUIRendering())
	{
		y += 20.f;
	}
#endif

	if (fps_text_current_pos == 1u || fps_text_current_pos == 3u)
	{
		x = static_cast<float>(Device.TargetWidth) - font_->SizeOf_(*text) - text_screen_padding;
	}

	if (fps_text_current_pos >= 2u)
	{
		y = static_cast<float>(Device.TargetHeight) - font_->GetHeight() - text_screen_padding;
	}

	font_->SetOutline(fps_text_outline);
	font_->SetOutlineColor(color_rgba(0u, 0u, 0u, fps_text_color_a / 3));
	font_->SetOutlineOffset(1.f);

	font_->SetColor(color_rgba(fps_text_color_r, fps_text_color_g, fps_text_color_b, fps_text_color_a));
	font_->OutSet(x, y);
	font_->OutNext(*text);

	font_->OnRender();
}