#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/DebugDrawHud.h"
#include "xrGame/HUDManager.h"

// Shows exclusive extended-AI CPU time; the newest completed sample lags by two frames.
void CELSharedDebugDrawHud::Draw(const xr_vector<SELDebugRecord>& Records, const SELDebugFrame& Frame, const SELDebugPerformance& Performance)
{
	string256 Label;
	const u32 Color = color_rgba(80, 255, 80, 255);
	xr_sprintf(Label, "Логика: %s | ГГ: %s | мс: кадр / пик / среднее", Frame.IsLogicEnabled ? "ВКЛ" : "ШТАТНАЯ", Frame.IgnoresActor ? "игнорируется" : "учитывается");
	HUD().world_prims.append_text2d(20.f, 100.f, Label, Color, CGameFont::alLeft);
	xr_sprintf(Label, "ОНЛАЙН, живые: летуны %u | прочие монстры %u (обновление 250 мс)", Frame.FlyingOnline, Frame.OtherMonstersOnline);
	HUD().world_prims.append_text2d(20.f, 118.f, Label, Color, CGameFont::alLeft);
	HUD().world_prims.append_text2d(20.f, 136.f, "CPU расширенной логики | последний завершённый кадр (-2), пик/среднее за окно до 118 кадров", Color, CGameFont::alLeft);
	const char* Names[] = {"Наземные", "Летуны", "Общие службы"};
	u32 GroupIndex;
	for (GroupIndex = 0; GroupIndex < u32(EELDebugGroup::Count); ++GroupIndex)
	{
		const SELDebugTiming& Timing = Performance.Groups[GroupIndex];
		xr_sprintf(Label, "%s: %.3f / %.3f / %.3f", Names[GroupIndex], Timing.FrameMilliseconds, Timing.PeakMilliseconds, Timing.AverageMilliseconds);
		HUD().world_prims.append_text2d(20.f, 154.f + float(GroupIndex) * 18.f, Label, Color, CGameFont::alLeft);
	}
	xr_sprintf(Label, "Игровые обновления: %.3f / %.3f / %.3f", Performance.Main.FrameMilliseconds, Performance.Main.PeakMilliseconds, Performance.Main.AverageMilliseconds);
	HUD().world_prims.append_text2d(20.f, 208.f, Label, Color, CGameFont::alLeft);
	xr_sprintf(Label, "Рабочие задания: %.3f / %.3f / %.3f", Performance.Worker.FrameMilliseconds, Performance.Worker.PeakMilliseconds, Performance.Worker.AverageMilliseconds);
	HUD().world_prims.append_text2d(20.f, 226.f, Label, Color, CGameFont::alLeft);
	xr_sprintf(Label, "ВСЕГО CPU: %.3f / %.3f / %.3f", Performance.Total.FrameMilliseconds, Performance.Total.PeakMilliseconds, Performance.Total.AverageMilliseconds);
	HUD().world_prims.append_text2d(20.f, 244.f, Label, Color, CGameFont::alLeft);
	HUD().world_prims.append_text2d(20.f, 268.f, "Этап (мс: кадр / пик):                 Наземные                     Летуны                  Общие", Color, CGameFont::alLeft);
	const char* Stages[] = {"Восприятие", "Поведение", "Движение", "Слух", "Обмен", "Мировые службы", "A* / кривые", "Захват графа", "Геометрия"};
	for (u32 StageIndex = 0; StageIndex < u32(EELDebugStage::Count); ++StageIndex)
	{
		HUD().world_prims.append_text2d(20.f, 286.f + float(StageIndex) * 18.f, Stages[StageIndex], Color, CGameFont::alLeft);
		for (GroupIndex = 0; GroupIndex < u32(EELDebugGroup::Count); ++GroupIndex)
		{
			const SELDebugTiming& Timing = Performance.Stages[GroupIndex][StageIndex];
			xr_sprintf(Label, "%.3f / %.3f", Timing.FrameMilliseconds, Timing.PeakMilliseconds);
			HUD().world_prims.append_text2d(260.f + float(GroupIndex) * 200.f, 286.f + float(StageIndex) * 18.f, Label, Color, CGameFont::alLeft);
		}
	}
}
