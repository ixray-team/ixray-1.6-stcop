#include "stdafx.h"

XrSEFactoryManager* g_SEFactoryManager = nullptr;

XrSEFactoryManager::XrSEFactoryManager()
{
	m_Module = LoadLibraryA("xrSE_Factory.dll");
	R_CHK(GetLastError());
	R_ASSERT(m_Module);
	//m_pFInitialize = (void(__cdecl*)(void))GetProcAddress(m_Module, "initialize"); R_ASSERT(m_pFInitialize);
	//m_pFDestroy = (void(__cdecl*)(void)) GetProcAddress(m_Module, "destroy"); R_ASSERT(m_pFDestroy);
	m_pFCreateEntity = (CSE_Abstract * (__cdecl*)(LPCSTR)) GetProcAddress(m_Module, "create_entity"); R_ASSERT(m_pFCreateEntity);
	m_pFDestroyEntity = (void(__cdecl*)(CSE_Abstract*&))GetProcAddress(m_Module, "destroy_entity"); R_ASSERT(m_pFDestroyEntity);
	m_pFReload = (void(__cdecl*)(void)) GetProcAddress(m_Module, "reload"); R_ASSERT(m_pFDestroyEntity);
	//m_pFInitialize();
}

XrSEFactoryManager::~XrSEFactoryManager()
{
//	m_pFDestroy();
	FreeLibrary(m_Module);
}


CSE_Abstract* XrSEFactoryManager::create_entity(LPCSTR section)
{
	return m_pFCreateEntity(section);
}

void XrSEFactoryManager::destroy_entity(CSE_Abstract*& abstract)
{
	m_pFDestroyEntity(abstract);
}

void XrSEFactoryManager::reload()
{
	m_pFReload();
}

