#pragma once

// Counts online instances independently of rendering visibility and extended AI control.
class CELDebugOnlinePopulation final
{
public:
	void Update(u32 WorldGeneration);
	u32 Flying() const { return FlyingCount; }
	u32 OtherMonsters() const { return OtherCount; }

private:
	u32 Generation = u32(-1);
	u32 NextUpdateAt = 0;
	u32 FlyingCount = 0;
	u32 OtherCount = 0;
};
