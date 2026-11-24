#pragma once

// Editor psDeviceFlags bits (shared by the editor device and the renderer editor mode).
enum
{
	rsFilterLinear = (1ul << 20ul),
	rsEdgedFaces = (1ul << 21ul),
	rsRenderTextures = (1ul << 22ul),
	rsFog = (1ul << 24ul),
	rsRenderRealTime = (1ul << 25ul),
	rsDrawGrid = (1ul << 26ul),
	rsDrawSafeRect = (1ul << 27ul),
	rsMuteSounds = (1ul << 28ul),
	rsEnvironment = (1ul << 29ul),
	rsDrawAxis = (1ul << 30ul),
	rsDisableAxisCube = (1ul << 31ul),
};
