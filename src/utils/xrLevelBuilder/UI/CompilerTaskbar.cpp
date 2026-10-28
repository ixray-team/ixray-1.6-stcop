#include "CompilerTaskbar.h"

#include "../../../xrCore/xrCore.h"
#include "../Resources/resource.h"

namespace
{
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

SCompilerTaskbarState State;

int GetStageOverlay(ECompilerStage Stage)
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

const char* GetStageName(ECompilerStage Stage)
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

// Requires State.Lock. Returns true when the overall percentage has changed.
bool ApplyProgressLocked()
{
	if (State.TotalStages <= 0)
	{
		return false;
	}

	if (State.PhaseIndex > 0)
	{
		// The number of phases is not known in advance, so the stage progress
		// is estimated from the finished phases plus the current one. The
		// estimate is continuous across phase boundaries and never reaches
		// 100% before EndStage().
		const float Done = ((float)(State.PhaseIndex - 1) + State.PhaseProgress);
		State.StageProgress = std::clamp(Done / (Done + 2.f), 0.f, 1.f);
	}

	const float Overall = ((float)State.CompletedStages + State.StageProgress) / (float)State.TotalStages;
	const int Percent = std::max(State.LastPercent, (int)(std::clamp(Overall, 0.f, 1.f) * PercentMax));
	if (Percent == State.LastPercent)
	{
		return false;
	}

	State.LastPercent = Percent;
	CTaskbarProgress::Instance().SetState(ETaskbarProgressState::Normal);
	CTaskbarProgress::Instance().SetProgress((u64)Percent, (u64)PercentMax);
	return true;
}

// Requires State.Lock.
void UpdateTooltipLocked()
{
	xr_string Tooltip = "IX-Ray Level Builder - ";
	Tooltip += State.LevelName;

	if (!State.StageName.empty())
	{
		Tooltip += " | ";
		Tooltip += State.StageName;

		if (!State.PhaseName.empty())
		{
			Tooltip += ": ";
			Tooltip += State.PhaseName;
		}
	}

	if (State.LastPercent >= 0)
	{
		char Buffer[TooltipBufferSize];
		xr_sprintf(Buffer, sizeof(Buffer), " (%d%%)", State.LastPercent);
		Tooltip += Buffer;
	}

	CTaskbarProgress::Instance().SetTooltip(Tooltip.c_str());
}
} // namespace

void CompilerTaskbar::BeginBuild(const char* LevelName, bool BuildLC, bool BuildAI, bool BuildDO)
{
	xrCriticalSectionGuard Guard(State.Lock);

	State.Active = true;
	State.LevelName = LevelName != nullptr ? LevelName : "";
	State.TotalStages = (BuildLC ? 1 : 0) + (BuildAI ? 1 : 0) + (BuildDO ? 1 : 0);
	State.CompletedStages = 0;

	State.StageName.clear();
	State.PhaseName.clear();
	State.PhaseIndex = 0;
	State.PhaseProgress = 0.f;
	State.StageProgress = 0.f;
	State.LastPercent = -1;

	// Drop the overlay of the previous build.
	CTaskbarProgress::Instance().ClearOverlayIcon();
	CTaskbarProgress::Instance().SetState(State.TotalStages > 0 ? ETaskbarProgressState::Indeterminate : ETaskbarProgressState::NoProgress);
	UpdateTooltipLocked();
}

void CompilerTaskbar::BeginStage(ECompilerStage Stage)
{
	xrCriticalSectionGuard Guard(State.Lock);
	if (!State.Active)
	{
		return;
	}

	State.StageName = GetStageName(Stage);
	State.PhaseName.clear();
	State.PhaseIndex = 0;
	State.PhaseProgress = 0.f;
	State.StageProgress = 0.f;

	// Keep a real progress bar instead of a marquee: the overall value is
	// already known from the completed stages.
	CTaskbarProgress::Instance().SetState(ETaskbarProgressState::Normal);
	if (State.TotalStages > 0)
	{
		CTaskbarProgress::Instance().SetProgress((u64)State.CompletedStages, (u64)State.TotalStages);
	}

	CTaskbarProgress::Instance().SetOverlayIcon(GetStageOverlay(Stage), State.StageName.c_str());
	UpdateTooltipLocked();
}

void CompilerTaskbar::BeginPhase(const char* PhaseName)
{
	xrCriticalSectionGuard Guard(State.Lock);
	if (!State.Active)
	{
		return;
	}

	++State.PhaseIndex;
	State.PhaseProgress = 0.f;
	State.PhaseName = PhaseName != nullptr ? PhaseName : "";

	ApplyProgressLocked();
	UpdateTooltipLocked();
}

void CompilerTaskbar::SetPhaseProgress(float Progress)
{
	xrCriticalSectionGuard Guard(State.Lock);
	if (!State.Active || State.TotalStages <= 0)
	{
		return;
	}

	State.PhaseProgress = std::clamp(Progress, 0.f, 1.f);

	if (ApplyProgressLocked())
	{
		UpdateTooltipLocked();
	}
}

void CompilerTaskbar::EndStage(ECompilerStage Stage)
{
	xrCriticalSectionGuard Guard(State.Lock);
	if (!State.Active)
	{
		return;
	}

	if (State.CompletedStages < State.TotalStages)
	{
		++State.CompletedStages;
	}

	State.PhaseIndex = 0;
	State.PhaseProgress = 0.f;
	State.StageProgress = 0.f;
	State.PhaseName.clear();

	if (State.TotalStages > 0)
	{
		State.LastPercent = std::max(State.LastPercent, (State.CompletedStages * PercentMax) / State.TotalStages);
		CTaskbarProgress::Instance().SetProgress((u64)State.CompletedStages, (u64)State.TotalStages);
	}

	UpdateTooltipLocked();
}

void CompilerTaskbar::EndBuild(bool Failed)
{
	xrCriticalSectionGuard Guard(State.Lock);
	if (!State.Active)
	{
		return;
	}

	State.Active = false;
	State.StageName.clear();
	State.PhaseName.clear();
	State.PhaseIndex = 0;
	State.PhaseProgress = 0.f;
	State.StageProgress = 0.f;

	if (Failed)
	{
		State.LastPercent = PercentMax;
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
	xrCriticalSectionGuard Guard(State.Lock);

	State.Active = false;
	State.TotalStages = 0;
	State.CompletedStages = 0;
	State.LastPercent = -1;
	State.LevelName.clear();
	State.StageName.clear();
	State.PhaseName.clear();
	State.PhaseIndex = 0;
	State.PhaseProgress = 0.f;
	State.StageProgress = 0.f;

	CTaskbarProgress::Instance().Reset();
	CTaskbarProgress::Instance().ClearOverlayIcon();
}
