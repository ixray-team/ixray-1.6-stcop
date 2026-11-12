#pragma once
#include "b_build_texture.h"

// Shared texture loading for xrLC, xrDO and xrAI.
// Reads texture parameters from the .thm file. When the .thm file is missing,
// derives as much as possible (size, alpha usage) from the real .dds texture.
class CBuildTextureLoader
{
public:
	struct SOptions
	{
		// Alpha flag for the merged "level_lods" texture
		bool LevelLodsHasAlpha = true;
		// Reset the alpha flag when the surface that needs it cannot be loaded
		bool DropAlphaWithoutSurface = false;
	};

	CBuildTextureLoader() = default;
	explicit CBuildTextureLoader(const SOptions& InOptions);

	// Strips the extension from BT.name, lowercases it and fills size, alpha, THM and surface
	void Load(b_BuildTexture& BT) const;

private:
	enum class EAlphaKind
	{
		None,
		OneBit,
		Full
	};

	struct SDdsInfo
	{
		u32 Width = 0;
		u32 Height = 0;
		EAlphaKind Alpha = EAlphaKind::None;
	};

	bool ReadThm(b_BuildTexture& BT, const char* Name) const;
	void LoadWithThm(b_BuildTexture& BT, const char* Name) const;
	void LoadWithoutThm(b_BuildTexture& BT, const char* Name) const;
	bool LoadSurface(b_BuildTexture& BT, IReader& Dds) const;

	static IReader* OpenDds(const char* Name);
	static bool ParseDdsHeader(IReader& Dds, SDdsInfo& Info);
	static bool SurfaceUsesAlpha(const b_BuildTexture& BT);
	static bool IsLodTexture(const char* Name);

	SOptions Options;
};
