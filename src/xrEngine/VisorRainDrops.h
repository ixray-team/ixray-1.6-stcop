#pragma once

class CObject;

class ENGINE_API VisorRainDrops final
{
public:
	VisorRainDrops() = default;
	~VisorRainDrops() = default;

	void Update(bool IsEnabled);
	void Clear();
	void Reset();
	float GetAmount() const { return Amount; }

private:
	float Amount = 0.f;
	u32 LastFrame = u32(-1);
	u32 LastRayPickTime = 0;
	bool HasRayPick = false;
	bool IsUnderCover = true;
	CObject* ViewEntity = nullptr;
};
