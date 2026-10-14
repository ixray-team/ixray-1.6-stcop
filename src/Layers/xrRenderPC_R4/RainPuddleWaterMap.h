#pragma once

#include "SH_Texture.h"

class CObjectSpace;

class RainPuddleWaterMap
{
public:
	RainPuddleWaterMap() = default;
	RainPuddleWaterMap(const RainPuddleWaterMap&) = delete;
	RainPuddleWaterMap& operator=(const RainPuddleWaterMap&) = delete;
	~RainPuddleWaterMap();
	void Build(CObjectSpace* Space = nullptr);
	void Destroy();
	const Fvector4& GetBounds() const { return Bounds; }
	const Fvector4& GetHeightRange() const { return HeightRange; }

private:
	ref_texture Texture;
	Fvector4 Bounds{};
	Fvector4 HeightRange{};
};
