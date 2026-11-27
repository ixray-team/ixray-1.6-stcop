#pragma once

#include "SandboxTerrain.h"
#include "../xrEngine/LevelResources.h"

// Procedural sandbox level builder.
// Writes a real level folder on disk so the stock loaders read it
// (1:1 parity — no special read paths).
namespace Sandbox
{
	// Writes the bundle under <levels dir>/<folderName>/ using FS path resolution
	// (_game_levels_ alias). Which files are written is driven by the resource
	// mask (ELevelResource bits):
	//   always:      level.ltx
	//   Collision:   level.cform   (via XRay::CForm::CFormatVanilla + XRay::CForm::Write)
	//   AiMap:       level.ai      (hdrNODES version=XRAI_LARGE_VERSION + raw CVertex array)
	//   PatrolPaths: level.game    (chunk format read by CPatrolPathStorage::load_raw)
	// Returns false with a Msg() on failure.
	bool	BuildLevel(const STerrainParams& terrain, const char* folderName, const Flags32& resourceMask);
	// Best-effort delete of the generated folder.
	void	RemoveLevel(const char* folderName);

	// Writes level.ai (XRAI_LARGE_VERSION: hdrNODES + raw LevelGraph::CVertex
	// array) for the regular node grid derived from the terrain params.
	// Exposed for the self-tests; BuildLevel uses it internally.
	bool	WriteLevelAi(const STerrainParams& terrain, const char* outputPath);
	// Writes level.game (patrol paths, CPatrolPathStorage::load_raw format)
	// into an arbitrary writer. Exposed for the self-tests.
	void	WriteLevelGame(IWriter& writer, const STerrainParams& terrain);
}
