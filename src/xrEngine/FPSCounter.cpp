#include "stdafx.h"
#include "FPSCounter.h"
#include "MonsterLogicTelemetry.h"
#include "../xrCore/Kernel/EngineExternal.h"

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

	xr_vector<xr_string> Lines;
	Lines.emplace_back(*text);
	if (g_MonsterLogicTelemetry.Enabled.load(std::memory_order_relaxed))
	{
		auto AddLine = [&](const char* Format, auto... Args)
		{
			string512 Buffer;
			xr_sprintf(Buffer, Format, Args...);
			Lines.emplace_back(Buffer);
		};
		const bool Enabled = EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation];
		AddLine("--- ЛОГИКА МОНСТРОВ ---");
		AddLine("МОНСТРЫ %s | живых %u | в бою %u", Enabled ? "ВКЛ" : "ВАНИЛЬ",
			g_MonsterLogicTelemetry.AliveMonsters.load(std::memory_order_relaxed),
			g_MonsterLogicTelemetry.CombatMonsters.load(std::memory_order_relaxed));
		AddLine("Знают ГГ: %u | впервые от сарафана: %u",
			g_MonsterLogicTelemetry.PlayerKnownMonsters.load(std::memory_order_relaxed),
			g_MonsterLogicTelemetry.PlayerFirstSharedMonsters.load(std::memory_order_relaxed));
		AddLine("Получили сарафан о ГГ: %u | видят ГГ: %u",
			g_MonsterLogicTelemetry.PlayerSharedMonsters.load(std::memory_order_relaxed),
			g_MonsterLogicTelemetry.PlayerVisibleMonsters.load(std::memory_order_relaxed));
		const float Cost = g_MonsterLogicTelemetry.Average[u32(EMonsterLogicTimer::NewLogicTotal)];
		const float FrameTime = g_MonsterLogicTelemetry.AverageFrameMilliseconds;
		AddLine("Новая логика %.2f мс | пик %.2f мс", Cost,
			g_MonsterLogicTelemetry.Peak[u32(EMonsterLogicTimer::NewLogicTotal)]);
		if (FrameTime > EPS_S && Cost < FrameTime)
		{
			const float CurrentFPS = 1000.f / FrameTime;
			const float EstimatedFPS = 1000.f / (FrameTime - Cost);
			AddLine("Доля CPU %.1f%% | оценка потери FPS %.2f", Cost * 100.f / FrameTime, EstimatedFPS - CurrentFPS);
		}
		else
		{
			AddLine("Оценка потери FPS: сбор данных");
		}
		const auto Session = [&](EMonsterLogicCounter Counter)
		{
			return static_cast<unsigned long long>(g_MonsterLogicTelemetry.SessionCounters[u32(Counter)]);
		};
		AddLine("После сброса: сарафан %llu | пси %llu | кличи %llu",
			Session(EMonsterLogicCounter::VisibleTransfers), Session(EMonsterLogicCounter::WirelessTransfers), Session(EMonsterLogicCounter::Calls));
		AddLine("Передач информации о ГГ: %llu", Session(EMonsterLogicCounter::ActorTransfers));
		if (g_MonsterLogicTelemetry.Detailed.load(std::memory_order_relaxed))
		{
			AddLine("--- ПОДРОБНО: ВЕСЬ ИИ МОНСТРОВ ---");
			AddLine("Включая ваниль; вложенные времена пересекаются");
			AddLine("Настройки [%s] друзья:%d распределение:%d кличи:%d",
				EngineExternal().GetMonsterLogicOverride() < 0 ? "конфиг" : "консоль",
				Enabled && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFriendlyEnemySharing],
				Enabled && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterDistributedTargeting],
				Enabled && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterEnemySharingCallSounds]);
			AddLine("Мирных %u / штатных ворон %u / фантомов %u",
				g_MonsterLogicTelemetry.PeacefulMonsters.load(std::memory_order_relaxed),
				g_MonsterLogicTelemetry.VanillaCrows.load(std::memory_order_relaxed),
				g_MonsterLogicTelemetry.StandalonePhantoms.load(std::memory_order_relaxed));
			AddLine("CPU мс/кадр: среднее / пик / вызовы (%.2f с)", g_MonsterLogicTelemetry.SampleSeconds);
			const char* Names[] = {"Мышление", "Память", "Память врагов", "Выбор врага", "Состояния + пути", "Сарафан", "Распределение", "Поиск соседей", "Урон ГГ", "Планировщик", "Обновление кадра", "Зрение + чувства", "Новая логика"};
			static_assert(std::size(Names) == CMonsterLogicTelemetry::TimerCount);
			for (u32 Index = 0; Index < CMonsterLogicTelemetry::TimerCount; ++Index)
			{
				if (Index == u32(EMonsterLogicTimer::NewLogicTotal))
				{
					continue;
				}
				AddLine("%s: %.3f / %.3f / %.1f", Names[Index], g_MonsterLogicTelemetry.Average[Index],
					g_MonsterLogicTelemetry.Peak[Index], g_MonsterLogicTelemetry.CallsPerFrame[Index]);
			}
			auto Rate = [&](EMonsterLogicCounter Counter) { return g_MonsterLogicTelemetry.PerSecond[u32(Counter)]; };
			AddLine("В секунду: бой %.0f / обмен %.0f / передачи %.0f", Rate(EMonsterLogicCounter::CombatUpdates), Rate(EMonsterLogicCounter::SharingUpdates), Rate(EMonsterLogicCounter::Transfers));
			AddLine("Записей проверено %.0f / прежних %.0f / отложено %.0f", Rate(EMonsterLogicCounter::SharingChecked), Rate(EMonsterLogicCounter::SharingUnchanged), Rate(EMonsterLogicCounter::SharingDeferred));
			AddLine("Штатных передач %.0f", Rate(EMonsterLogicCounter::NativeTransfers));
			AddLine("Запросы %.0f (пик %u/4) / отложено %.0f", Rate(EMonsterLogicCounter::Queries), g_MonsterLogicTelemetry.CounterPeak[u32(EMonsterLogicCounter::Queries)], Rate(EMonsterLogicCounter::QueryDeferred));
			AddLine("Планы %.0f (пик %u/2) / отложено %.0f", Rate(EMonsterLogicCounter::Plans), g_MonsterLogicTelemetry.CounterPeak[u32(EMonsterLogicCounter::Plans)], Rate(EMonsterLogicCounter::PlanDeferred));
			AddLine("В плане особей %.0f / целей %.0f / кличей %.0f", Rate(EMonsterLogicCounter::Members), Rate(EMonsterLogicCounter::Targets), Rate(EMonsterLogicCounter::Calls));
			AddLine("Урон %.0f / фокус %.0f / испуг %.0f", Rate(EMonsterLogicCounter::Hits), Rate(EMonsterLogicCounter::Focus), Rate(EMonsterLogicCounter::Panic));
			AddLine("Вложенные таймеры CPU: строки не складывать");
		}
	}
	float Width = 0.f;
	for (const xr_string& Line : Lines)
	{
		Width = std::max(Width, font_->SizeOf_(Line.c_str()));
	}
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
		x = std::max(text_screen_padding, float(Device.TargetWidth) - Width - text_screen_padding);
	}
	if (fps_text_current_pos >= 2u)
	{
		y = std::max(text_screen_padding, float(Device.TargetHeight) - font_->GetHeight() * float(Lines.size()) - text_screen_padding);
	}

	font_->SetOutline(fps_text_outline);
	font_->SetOutlineColor(color_rgba(0u, 0u, 0u, fps_text_color_a / 3));
	font_->SetOutlineOffset(1.f);

	font_->SetColor(color_rgba(fps_text_color_r, fps_text_color_g, fps_text_color_b, fps_text_color_a));
	font_->OutSet(x, y);
	for (const xr_string& Line : Lines)
	{
		font_->OutNext("%s", Line.c_str());
	}

	font_->OnRender();
}

ENGINE_API CMonsterLogicTelemetry g_MonsterLogicTelemetry;

void CMonsterLogicTelemetry::Reset()
{
	Generation.fetch_add(1, std::memory_order_relaxed);
	PlayerKnownMonsters.store(0, std::memory_order_relaxed);
	PlayerFirstSharedMonsters.store(0, std::memory_order_relaxed);
	PlayerSharedMonsters.store(0, std::memory_order_relaxed);
	PlayerVisibleMonsters.store(0, std::memory_order_relaxed);
	AliveMonsters.store(0, std::memory_order_relaxed);
	CombatMonsters.store(0, std::memory_order_relaxed);
	PeacefulMonsters.store(0, std::memory_order_relaxed);
	VanillaCrows.store(0, std::memory_order_relaxed);
	StandalonePhantoms.store(0, std::memory_order_relaxed);
	Frames = 0;
	Seconds = SampleSeconds = AverageFrameMilliseconds = 0.f;
	for (u32 Index = 0; Index < TimerCount; ++Index)
	{
		Microseconds[Index].store(0, std::memory_order_relaxed);
		Invocations[Index].store(0, std::memory_order_relaxed);
		Average[Index] = Peak[Index] = CallsPerFrame[Index] = WindowPeak[Index] = 0.f;
		Sum[Index] = 0.;
		WindowCalls[Index] = 0;
	}
	for (u32 Index = 0; Index < CounterCount; ++Index)
	{
		Counters[Index].store(0, std::memory_order_relaxed);
		SessionCounters[Index] = 0;
		PerSecond[Index] = 0.f;
		CounterPeak[Index] = WindowCounterPeak[Index] = 0;
		WindowCounters[Index] = 0;
	}
}

void CMonsterLogicTelemetry::CaptureFrame(float Delta)
{
	if (!Enabled.load(std::memory_order_relaxed) || Delta <= 0.f)
	{
		return;
	}
	++Frames;
	Seconds += Delta;
	for (u32 Index = 0; Index < TimerCount; ++Index)
	{
		const float Milliseconds = float(Microseconds[Index].exchange(0, std::memory_order_relaxed)) / 1000.f;
		Sum[Index] += Milliseconds;
		WindowPeak[Index] = std::max(WindowPeak[Index], Milliseconds);
		WindowCalls[Index] += Invocations[Index].exchange(0, std::memory_order_relaxed);
	}
	for (u32 Index = 0; Index < CounterCount; ++Index)
	{
		const u32 Amount = Counters[Index].exchange(0, std::memory_order_relaxed);
		WindowCounters[Index] += Amount;
		SessionCounters[Index] += Amount;
		WindowCounterPeak[Index] = std::max(WindowCounterPeak[Index], Amount);
	}
	if (Seconds >= .5f)
	{
		SampleSeconds = Seconds;
		AverageFrameMilliseconds = Seconds * 1000.f / float(Frames);
		for (u32 Index = 0; Index < TimerCount; ++Index)
		{
			Average[Index] = float(Sum[Index] / Frames);
			Peak[Index] = WindowPeak[Index];
			CallsPerFrame[Index] = float(WindowCalls[Index]) / Frames;
			Sum[Index] = 0.;
			WindowCalls[Index] = 0;
			WindowPeak[Index] = 0.f;
		}
		for (u32 Index = 0; Index < CounterCount; ++Index)
		{
			PerSecond[Index] = float(WindowCounters[Index]) / Seconds;
			CounterPeak[Index] = WindowCounterPeak[Index];
			WindowCounters[Index] = 0;
			WindowCounterPeak[Index] = 0;
		}
		Seconds = 0.f;
		Frames = 0;
	}
}
