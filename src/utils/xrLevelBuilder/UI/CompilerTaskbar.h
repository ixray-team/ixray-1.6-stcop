#pragma once

#include "../../../xrCore/Kernel/TaskbarProgress.h"

// Taskbar UX for xrLevelBuilder.
//
// Compiler stages run on the build thread, so this module only records the
// build state and forwards it to CTaskbarProgress, which applies it on the UI
// thread. Stage overlays use the icon resources of the builder executable.

enum class ECompilerStage : int
{
	XrLC = 0,
	XrAI,
	XrDO,
	Count,
};

namespace CompilerTaskbar
{
	// Prepares the taskbar for a new build and shows the level name in the tooltip.
	void BeginBuild(const char* LevelName, bool BuildLC, bool BuildAI, bool BuildDO);

	// Switches to the given stage: stage overlay, tooltip and stage progress.
	void BeginStage(ECompilerStage Stage);

	// Called for every compiler phase to refine the tooltip and the progress.
	void BeginPhase(const char* PhaseName);

	// Progress of the current phase in the 0..1 range.
	void SetPhaseProgress(float Progress);

	// Marks the stage as complete and advances the overall progress.
	void EndStage(ECompilerStage Stage);

	// Publishes the final state: success overlay or error state plus attention request.
	void EndBuild(bool Failed);

	// Drops all pending taskbar state.
	void Shutdown();
} // namespace CompilerTaskbar
