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
#pragma once

// The thread that draws for the game when r_render_thread is on. It owns the graphics
// device while it runs a job: the thread that hands the job over waits, and takes the device back when it is done
class RenderThread
{
public:
	// Whether the frame hands its work over: the setting, outside the editors
	static bool IsWanted();
	// Whether the game goes on to its next frame while this one is drawn
	static bool IsPipelined();

	// Runs the job on the thread and waits for it
	void Do(const std::function<void()>& Job);
	// Leaves the job to the thread, after those it already has
	void Post(std::function<void()>&& Job);
	// Until the thread has nothing left
	void Wait();
	// Wait, of the device's thread if there is one: before anything else takes what a frame in flight reads
	static void WaitIdle();
	// Whether the calling thread is the render thread
	static bool IsCurrent();

private:
	void Next();

	xrCriticalSection Lock;
	xr_vector<std::function<void()>> Jobs;
	xr_unique_ptr<XRayWorkerThread> Worker;
};
