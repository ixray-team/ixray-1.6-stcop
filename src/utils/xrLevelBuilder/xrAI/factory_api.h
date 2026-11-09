#pragma once

extern "C" {
	typedef __declspec(dllimport)  ISE_Abstract*	__stdcall SEFactory_Create	(const char* section);
	typedef __declspec(dllimport)  void				__stdcall SEFactory_Destroy	(ISE_Abstract *&);
	typedef __declspec(dllimport)  u16				__cdecl SEFactory_SetSpawnWriteVersion	(u16 Version);
};

extern SEFactory_Create	*create_entity;
extern SEFactory_Destroy	*destroy_entity;
// Entities live in xrSE_Factory.dll, so their writers use the DLL copy of g_SpawnWriteVersion.
extern SEFactory_SetSpawnWriteVersion	*set_spawn_write_version;

IC	CSE_Abstract *F_entity_Create(const char* section)
{
	ISE_Abstract	*i = create_entity(section);
	CSE_Abstract	*j = smart_cast<CSE_Abstract*>(i);
	return			(j);
}

IC	void F_entity_Destroy(CSE_Abstract *&i)
{
	ISE_Abstract	*j = i;
	destroy_entity	(j);
	i				= 0;
}
