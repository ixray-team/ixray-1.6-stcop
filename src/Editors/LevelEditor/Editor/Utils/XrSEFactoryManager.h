#pragma once

class CSE_Abstract;
class XrSEFactoryManager
{
public:
	XrSEFactoryManager();
	~XrSEFactoryManager();
	CSE_Abstract*   create_entity(LPCSTR section);
	void			destroy_entity(CSE_Abstract*& abstract);
	void reload();
private:
	HMODULE m_Module;

	void(__cdecl*m_pFInitialize)(void);
	void(__cdecl* m_pFDestroy)(void);
	void(__cdecl* m_pFReload)(void);
	CSE_Abstract* (__cdecl* m_pFCreateEntity)(LPCSTR);
	void(__cdecl* m_pFDestroyEntity)(CSE_Abstract*& );
};
extern XrSEFactoryManager* g_SEFactoryManager;