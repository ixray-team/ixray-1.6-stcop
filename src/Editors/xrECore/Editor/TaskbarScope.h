#pragma once

#include "../../../xrCore/Kernel/TaskbarProgress.h"

// RAII helper for editor operations that block the UI thread.
// The state is applied immediately, because no frame is drawn while the operation runs.
class CTaskbarScope
{
public:
	explicit CTaskbarScope(const char* Text) :
		Taskbar(&CTaskbarProgress::Instance())
	{
		Active = Taskbar->IsInitialized();
		if (!Active)
		{
			return;
		}

		Taskbar->SetState(ETaskbarProgressState::Indeterminate);
		Taskbar->SetTooltip(Text);
		Taskbar->Update();
	}

	~CTaskbarScope()
	{
		if (!Active)
		{
			return;
		}

		Taskbar->Reset();
		Taskbar->SetTooltip("IX-Ray Level Editor");
		Taskbar->Update();
	}

	CTaskbarScope(const CTaskbarScope&) = delete;
	CTaskbarScope& operator=(const CTaskbarScope&) = delete;

private:
	CTaskbarProgress* Taskbar = nullptr;
	bool Active = false;
};
