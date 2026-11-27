#pragma once

class CAI_Space;
class CEnvironment;
class CScriptEngine;
class IPHWorld;
class CCustomHUD;
class IGame_CustomUI;
class ISpatial_DB;
class CObject;

struct SLevelServices
{
	CAI_Space* AI = nullptr;
	IPHWorld* Physics = nullptr;
	CScriptEngine* ScriptEngine = nullptr;
	CEnvironment* Environment = nullptr;
	CCustomHUD* Hud = nullptr;
	void* PlayerHud = nullptr;
	IGame_CustomUI* GameUI = nullptr;
	ISpatial_DB* Spatial = nullptr;
	ISpatial_DB* SpatialPhysic = nullptr;
	CObject* Actor = nullptr;
};

ENGINE_API SLevelServices* CurrentLevelServices();
