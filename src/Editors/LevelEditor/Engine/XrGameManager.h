#pragma once

class XrGameManager
{
public:
	XrGameManager();
	~XrGameManager();
	DLL_Pure* Create(CLASS_ID clsid);
	ISE_Abstract* CreateFromSection(LPCSTR Name);
	void Destroy(ISE_Abstract* p);
private:
	HMODULE				m_hGame;
	Factory_Create*		m_pCreate;
	Factory_Destroy*	m_pDestroy;
	ISE_Abstract* (__cdecl* m_pCreateFromSection)(LPCSTR);
};

extern XrGameManager* g_XrGameManager;