#include "stdafx.h"
#include "../../xrServerEntities/xrServer_Objects.h"

extern "C"
{
	__declspec(dllimport) ISE_Abstract* __stdcall create_entity(const char* section);
	__declspec(dllimport) void __stdcall destroy_entity(ISE_Abstract*& abstract);
	__declspec(dllimport) void __cdecl reload();
}

__declspec(dllimport) void SEFactoryEntry();
__declspec(dllimport) void SEFactoryDestroy();

XrSEFactoryManager* g_SEFactoryManager = nullptr;

XrSEFactoryManager::XrSEFactoryManager()
{
	SEFactoryEntry();
}

XrSEFactoryManager::~XrSEFactoryManager()
{
	SEFactoryDestroy();
}

CSE_Abstract* XrSEFactoryManager::create_entity(const char* section)
{
	return static_cast<CSE_Abstract*>(::create_entity(section));
}

void XrSEFactoryManager::destroy_entity(CSE_Abstract*& abstract)
{
	ISE_Abstract* Entity = abstract;
	::destroy_entity(Entity);
	abstract = nullptr;
}

void XrSEFactoryManager::reload()
{
	::reload();
}
