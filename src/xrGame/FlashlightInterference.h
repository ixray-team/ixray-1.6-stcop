#pragma once

extern int g_FlashlightInterference;

class FlashlightInterference
{
public:
	void Load(const char* Section);
	void Reset();
	float Update(const Fvector& Position, float TimeDelta);

private:
	float Scan(const Fvector& Position) const;

	bool Enabled = false;
	float Radius = 8.0f;
	float ShockFactor = 1.5f;
	float BlowoutFactor = 1.5f;
	float DimLevel = 0.35f;
	float JitterLevel = 0.12f;
	float FlickerRate = 10.0f;
	float BlackoutRate = 0.6f;
	float ScanInterval = 0.2f;
	float ResponseTime = 0.4f;

	float ScanTimer = 0.0f;
	float Target = 0.0f;
	float Disturbance = 0.0f;
	float DipTime = 0.0f;
	float DipDepth = 0.0f;
	float BlackoutTime = 0.0f;
};
