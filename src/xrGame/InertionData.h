#pragma once

constexpr float PITCH_OFFSET_R = 0.017f;
constexpr float PITCH_OFFSET_N = 0.012f;
constexpr float PITCH_OFFSET_D = 0.02f;
constexpr float ORIGIN_OFFSET = 0.05f;
constexpr float TENDTO_SPEED = 5.0f;

struct InertionData {
	float PitchOffsetR;
	float PitchOffsetN;
	float PitchOffsetD;

	float OriginOffset;
	float TendtoSpeed;

	InertionData() {
		PitchOffsetR = 0.0f;
		PitchOffsetN = 0.0f;
		PitchOffsetD = 0.0f;

		OriginOffset = ORIGIN_OFFSET;
		TendtoSpeed = TENDTO_SPEED;
	}

	InertionData& lerp(InertionData from, InertionData to, float factor) {
		PitchOffsetR = from.PitchOffsetR + factor * (to.PitchOffsetR - from.PitchOffsetR);
		PitchOffsetN = from.PitchOffsetN + factor * (to.PitchOffsetN - from.PitchOffsetN);
		PitchOffsetD = from.PitchOffsetD + factor * (to.PitchOffsetD - from.PitchOffsetD);

		OriginOffset = from.OriginOffset + factor * (to.OriginOffset - from.OriginOffset);
		TendtoSpeed = from.TendtoSpeed + factor * (to.TendtoSpeed - from.TendtoSpeed);

		return *this;
	}
};