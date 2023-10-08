#include "stdafx.h"

ENGINE_API CEngineExternal* g_pEngineExternal = nullptr;

char* getStringEEngineExternalUI(EEngineExternalUI);
char* getStringEEngineExternalPhysical(EEngineExternalPhysical);
char* getStringEEngineExternalGame(EEngineExternalGame);
char* getStringEEngineExternalRender(EEngineExternalRender);

CEngineExternal::CEngineExternal() {
	string_path fname;
	FS.update_path(fname, "$game_config$", "engine_external.ltx");
	pOptions = new CInifile(fname);
}

CEngineExternal::~CEngineExternal() {
	xr_delete(pOptions);
}

bool CEngineExternal::operator[](const EEngineExternalUI& ID) const {
	return pOptions->r_bool("ui", getStringEEngineExternalUI(ID));
}

bool CEngineExternal::operator[](const EEngineExternalPhysical& ID) const {
	return pOptions->r_bool("physics", getStringEEngineExternalPhysical(ID));
}

bool CEngineExternal::operator[](const EEngineExternalGame& ID) const {
	return pOptions->r_bool("gameplay", getStringEEngineExternalGame(ID));
}

bool CEngineExternal::operator[](const EEngineExternalRender& ID) const {
	return pOptions->r_bool("render", getStringEEngineExternalRender(ID));
}

ENGINE_API CEngineExternal& EngineExternal() {
	if (g_pEngineExternal == nullptr) {
		g_pEngineExternal = new CEngineExternal;
	}
	return *g_pEngineExternal;
}
