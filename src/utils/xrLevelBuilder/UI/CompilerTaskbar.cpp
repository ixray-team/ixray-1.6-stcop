#include "CompilerTaskbar.h"

#include "../../../xrCore/xrCore.h"
#include "../Resources/resource.h"

constexpr int PercentMax = 100;
constexpr size_t TooltipBufferSize = 32;

struct SCompilerTaskbarState
{
	xrCriticalSection Lock;

	bool Active = false;
	xr_string LevelName;

	int TotalStages = 0;
	int CompletedStages = 0;

	xr_string StageName;
	xr_string PhaseName;

	// Current stage progress: phases started, progress of the current phase
	// and the estimated stage fraction.
	int PhaseIndex = 0;
	float PhaseProgress = 0.f;
	float StageProgress = 0.f;

	int LastPercent = -1;
};

static SCompilerTaskbarState g_BuildState;

static int GetStageOverlay(ECompilerStage Stage)
{
	switch (Stage)
	{
		case ECompilerStage::XrLC:
			return IDI_OVERLAY_XRLC;
		case ECompilerStage::XrAI:
			return IDI_OVERLAY_XRAI;
		case ECompilerStage::XrDO:
			return IDI_OVERLAY_XRDO;
		default:
			return 0;
	}
}

static const char* GetStageName(ECompilerStage Stage)
{
	switch (Stage)
	{
		case ECompilerStage::XrLC:
			return "xrLC";
		case ECompilerStage::XrAI:
			return "xrAI";
		case ECompilerStage::XrDO:
			return "xrDO";
		default:
			return "";
	}
}

// Requires g_BuildState.Lock. Returns true when the overall percentage has changed.
static bool ApplyProgressLocked()
{
	if (g_BuildState.TotalStages <= 0)
	{
		return false;
	}

	if (g_BuildState.PhaseIndex > 0)
	{
		// The number of phases is not known in advance, so the stage progress
		// is estimated from the finished phases plus the current one. The
		// estimate is continuous across phase boundaries and never reaches
		// 100% before EndStage().
		const float Done = ((float)(g_BuildState.PhaseIndex - 1) + g_BuildState.PhaseProgress);
		g_BuildState.StageProgress = std::clamp(Done / (Done + 2.f), 0.f, 1.f);
	}

	const float Overall = ((float)g_BuildState.CompletedStages + g_BuildState.StageProgress) / (float)g_BuildState.TotalStages;
	const int Percent = std::max(g_BuildState.LastPercent, (int)(std::clamp(Overall, 0.f, 1.f) * PercentMax));
	if (Percent == g_BuildState.LastPercent)
	{
		return false;
	}

	g_BuildState.LastPercent = Percent;
	CTaskbarProgress::Instance().SetState(ETaskbarProgressState::Normal);
	CTaskbarProgress::Instance().SetProgress((u64)Percent, (u64)PercentMax);
	return true;
}

// Requires g_BuildState.Lock.
static void UpdateTooltipLocked()
{
	xr_string Tooltip = "IX-Ray Level Builder - ";
	Tooltip += g_BuildState.LevelName;

	if (!g_BuildState.StageName.empty())
	{
		Tooltip += " | ";
		Tooltip += g_BuildState.StageName;

		if (!g_BuildState.PhaseName.empty())
		{
			Tooltip += ": ";
			Tooltip += g_BuildState.PhaseName;
		}
	}

	if (g_BuildState.LastPercent >= 0)
	{
		char Buffer[TooltipBufferSize];
		xr_sprintf(Buffer, sizeof(Buffer), " (%d%%)", g_BuildState.LastPercent);
		Tooltip += Buffer;
	}

	CTaskbarProgress::Instance().SetTooltip(Tooltip.c_str());
}

void CompilerTaskbar::BeginBuild(const char* LevelName, bool BuildLC, bool BuildAI, bool BuildDO)
{
	xrCriticalSectionGuard Guard(g_BuildState.Lock);

	g_BuildState.Active = true;
	g_BuildState.LevelName = LevelName != nullptr ? LevelName : "";
	g_BuildState.TotalStages = (BuildLC ? 1 : 0) + (BuildAI ? 1 : 0) + (BuildDO ? 1 : 0);
	g_BuildState.CompletedStages = 0;

	g_BuildState.StageName.clear();
	g_BuildState.PhaseName.clear();
	g_BuildState.PhaseIndex = 0;
	g_BuildState.PhaseProgress = 0.f;
	g_BuildState.StageProgress = 0.f;
	g_BuildState.LastPercent = -1;

	// Drop the overlay of the previous build.
	CTaskbarProgress::Instance().ClearOverlayIcon();
	CTaskbarProgress::Instance().SetState(g_BuildState.TotalStages > 0 ? ETaskbarProgressState::Indeterminate : ETaskbarProgressState::NoProgress);
	UpdateTooltipLocked();
}

void CompilerTaskbar::BeginStage(ECompilerStage Stage)
{
	xrCriticalSectionGuard Guard(g_BuildState.Lock);
	if (!g_BuildState.Active)
	{
		return;
	}

	g_BuildState.StageName = GetStageName(Stage);
	g_BuildState.PhaseName.clear();
	g_BuildState.PhaseIndex = 0;
	g_BuildState.PhaseProgress = 0.f;
	g_BuildState.StageProgress = 0.f;

	// Keep a real progress bar instead of a marquee: the overall value is
	// already known from the completed stages.
	CTaskbarProgress::Instance().SetState(ETaskbarProgressState::Normal);
	if (g_BuildState.TotalStages > 0)
	{
		CTaskbarProgress::Instance().SetProgress((u64)g_BuildState.CompletedStages, (u64)g_BuildState.TotalStages);
	}

	CTaskbarProgress::Instance().SetOverlayIcon(GetStageOverlay(Stage), g_BuildState.StageName.c_str());
	UpdateTooltipLocked();
}

void CompilerTaskbar::BeginPhase(const char* PhaseName)
{
	xrCriticalSectionGuard Guard(g_BuildState.Lock);
	if (!g_BuildState.Active)
	{
		return;
	}

	++g_BuildState.PhaseIndex;
	g_BuildState.PhaseProgress = 0.f;
	g_BuildState.PhaseName = PhaseName != nullptr ? PhaseName : "";

	ApplyProgressLocked();
	UpdateTooltipLocked();
}

void CompilerTaskbar::SetPhaseProgress(float Progress)
{
	xrCriticalSectionGuard Guard(g_BuildState.Lock);
	if (!g_BuildState.Active || g_BuildState.TotalStages <= 0)
	{
		return;
	}

	g_BuildState.PhaseProgress = std::clamp(Progress, 0.f, 1.f);

	if (ApplyProgressLocked())
	{
		UpdateTooltipLocked();
	}
}

void CompilerTaskbar::EndStage(ECompilerStage Stage)
{
	xrCriticalSectionGuard Guard(g_BuildState.Lock);
	if (!g_BuildState.Active)
	{
		return;
	}

	if (g_BuildState.CompletedStages < g_BuildState.TotalStages)
	{
		++g_BuildState.CompletedStages;
	}

	g_BuildState.PhaseIndex = 0;
	g_BuildState.PhaseProgress = 0.f;
	g_BuildState.StageProgress = 0.f;
	g_BuildState.PhaseName.clear();

	if (g_BuildState.TotalStages > 0)
	{
		g_BuildState.LastPercent = std::max(g_BuildState.LastPercent, (g_BuildState.CompletedStages * PercentMax) / g_BuildState.TotalStages);
		CTaskbarProgress::Instance().SetProgress((u64)g_BuildState.CompletedStages, (u64)g_BuildState.TotalStages);
	}

	UpdateTooltipLocked();
}

void CompilerTaskbar::EndBuild(bool Failed)
{
	xrCriticalSectionGuard Guard(g_BuildState.Lock);
	if (!g_BuildState.Active)
	{
		return;
	}

	g_BuildState.Active = false;
	g_BuildState.StageName.clear();
	g_BuildState.PhaseName.clear();
	g_BuildState.PhaseIndex = 0;
	g_BuildState.PhaseProgress = 0.f;
	g_BuildState.StageProgress = 0.f;

	if (Failed)
	{
		g_BuildState.LastPercent = PercentMax;
		CTaskbarProgress::Instance().SetProgress((u64)PercentMax, (u64)PercentMax);
		CTaskbarProgress::Instance().SetState(ETaskbarProgressState::Error);
		CTaskbarProgress::Instance().SetOverlayIcon(IDI_OVERLAY_ERROR, "Compilation failed");
		CTaskbarProgress::Instance().SetTooltip("IX-Ray Level Builder - compilation failed");
	}
	else
	{
		CTaskbarProgress::Instance().Reset();
		CTaskbarProgress::Instance().SetOverlayIcon(IDI_OVERLAY_SUCCESS, "Compilation complete");
		CTaskbarProgress::Instance().SetTooltip("IX-Ray Level Builder - compilation complete");
	}

	CTaskbarProgress::Instance().RequestAttention();
}

void CompilerTaskbar::Shutdown()
{
	xrCriticalSectionGuard Guard(g_BuildState.Lock);

	g_BuildState.Active = false;
	g_BuildState.TotalStages = 0;
	g_BuildState.CompletedStages = 0;
	g_BuildState.LastPercent = -1;
	g_BuildState.LevelName.clear();
	g_BuildState.StageName.clear();
	g_BuildState.PhaseName.clear();
	g_BuildState.PhaseIndex = 0;
	g_BuildState.PhaseProgress = 0.f;
	g_BuildState.StageProgress = 0.f;

	CTaskbarProgress::Instance().Reset();
	CTaskbarProgress::Instance().ClearOverlayIcon();
}
