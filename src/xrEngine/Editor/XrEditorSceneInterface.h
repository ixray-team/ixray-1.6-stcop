#pragma once
#include "../../xrCDB/xrCDB.h"
#include "game_graph.h"
class CObjectSpace;
class XrEditorSceneInterface
{
public:
	virtual IReader* LoadSpawn() = 0;
	virtual	void LoadCForm(CObjectSpace*Space, CDB::build_callback)=0;
	virtual class IGameGraph* GetGameGraph() = 0;
	virtual class ILevelGraph* GetLevelGraph() = 0;
	virtual void Stop() = 0;
	virtual bool RayPick(const Fvector& start, const Fvector& dir, float& dist, Fvector* pt = 0, Fvector* n = 0) = 0;
};