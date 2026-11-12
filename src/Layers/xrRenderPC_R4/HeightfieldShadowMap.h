#pragma once

#include "SH_Texture.h"

class HeightfieldShadowMap final
{
public:
	HeightfieldShadowMap() = default;
	HeightfieldShadowMap(const HeightfieldShadowMap&) = delete;
	HeightfieldShadowMap& operator=(const HeightfieldShadowMap&) = delete;
	~HeightfieldShadowMap();
	void Load(const char* LevelName);
	void Destroy();
	bool IsValid() const { return HasData; }
	const Fvector4& GetWorldToGrid() const { return WorldToGrid; }
	const Fvector4& GetDecode() const { return Decode; }
	const Fvector4& GetGrid() const { return Grid; }

private:
	ref_texture Heights;
	Fvector4 WorldToGrid{};
	Fvector4 Decode{};
	Fvector4 Grid{};
	bool HasData = false;
};
