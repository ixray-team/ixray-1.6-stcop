#pragma once

namespace SmartCoverNpc
{
	struct Candidate
	{
		xr_string Npc;
		xr_string Visual;
		xr_string Animation;
		xr_string State;
		bool Animpoint = false;
	};

	bool Pick(const char* CoverName, Candidate& Out, u32& CandidateCount);
	bool IsAnimpointCover(const char* CoverName, const char* Description);
	xr_string IntoAnimation(const char* State);
	void Reset();
}
