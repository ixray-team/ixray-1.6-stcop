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

// Holds the start of a frame until the GPU has finished all but the frame queue (r_latency) of the frames before it, and
// measures the time from a frame's input to its end on the GPU, both on the NRI device's frame fence
class FramePacer
{
public:
	// The last thing before the input is read
	void Wait();
	// After the present, and before the frame's input: looks at the frames the GPU finished
	void Collect();
	void Destroy();
	void LogLatency() const;

	// With the frames one behind the game (r_render_thread pipelined) the game's frame runs beside the frame before it.
	// Started as soon as that one is handed over, it would end long before the frames can meet, and its input would be
	// older by that much when its own frame is drawn. Hold, in Wait's place before the input is read, keeps it back so
	// that it ends as the frame in flight does; HandOver marks the hand-over and Land, on the render thread, its end;
	// Meet is Wait's hold on the GPU's frames where the two meet (Entry: when the game's thread came there), and takes
	// both lengths for the next Hold
	void HandOver();
	void Land();
	void Meet(u64 Entry);

private:
	void Hold();

	u64 Polled = 0;
	u64 Handed = 0;
	u64 Landed = 0;
	float Tail = 0.0f;
	float Move = 0.0f;

	static constexpr u32 SlotCount = 4;
	static constexpr u32 WindowSize = 128;

	u64 InputTimes[SlotCount] = {};
	u64 Seen = 0;
	float Latencies[WindowSize] = {};
	u32 LatencyCount = 0;
};
