#pragma once

class ENGINE_API CObject;

namespace Feel
{
	class	ENGINE_API Sound
	{
	public:
		virtual float GetHearingRangeFactor() const { return 1.f; }
		// Negative means native asset-based hearing; zero rejects this sound.
		virtual float GetDistanceSoundRange(int Type, CObject* Source = nullptr) const { return -1.f; }
		virtual		void			feel_sound_new		(CObject* who, int type, CSound_UserDataPtr user_data, const Fvector& Position, float power) {};
	};
};
