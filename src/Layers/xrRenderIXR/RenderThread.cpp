/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "RenderThread.h"
#include "../../xrEngine/VideoSettings.h"

bool RenderThread::IsWanted()
{
	return VideoSettings::Effective(EVideoSetting::RenderThread) != 0 && !Device.IsEditorMode() && !g_dedicated_server;
}

static thread_local bool IsWorker = false;

bool RenderThread::IsCurrent()
{
	return IsWorker;
}

bool RenderThread::IsPipelined()
{
	return VideoSettings::Effective(EVideoSetting::RenderThread) == 2 && !Device.IsEditorMode() && !g_dedicated_server;
}

// One job a wake. While the caller waits (the mode that is not pipelined) this thread is the game's thread too, as
// only one of the two runs; it is the device's thread until it has nothing left
void RenderThread::Next()
{
	IsWorker = true;
	const bool IsShared = !IsPipelined();

	// With the frames one behind the game this is the device's thread from its first wake on: a texture the game's
	// thread makes between two frames waits for it, and is never filled beside a frame that is about to be played
	if (!IsShared)
	{
		g_AppInfo.SetRenderThread(true);
	}

	std::function<void()> Job;

	{
		xrCriticalSectionGuard Guard(Lock);

		if (Jobs.empty())
		{
			return;
		}

		Job = std::move(Jobs.front());
		Jobs.erase(Jobs.begin());
	}

	g_AppInfo.SetPrimaryDelegate(IsShared);
	g_AppInfo.SetRenderThread(true);
	Job();

	xrCriticalSectionGuard Guard(Lock);

	if (Jobs.empty() && IsShared)
	{
		g_AppInfo.SetRenderThread(false);
		g_AppInfo.SetPrimaryDelegate(false);
	}
}

void RenderThread::Post(std::function<void()>&& Job)
{
	if (!Worker)
	{
		Worker = xr_make_unique<XRayWorkerThread>([this] { Next(); }, "X-Ray Render Thread");
		// A wake with nothing to do: the thread takes the device before the first job is left to it
		Worker->Run();
		Worker->Wait();
	}

	{
		xrCriticalSectionGuard Guard(Lock);
		Jobs.push_back(std::move(Job));
	}

	Worker->Run();
}

void RenderThread::Wait()
{
	if (Worker)
	{
		Worker->Wait();
	}
}

void RenderThread::Do(const std::function<void()>& Job)
{
	Post(std::function<void()>(Job));
	Wait();
}
