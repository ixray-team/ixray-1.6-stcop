#pragma once

class CSE_Abstract;

class XrSEFactoryManager
{
public:
	XrSEFactoryManager();
	~XrSEFactoryManager();

	CSE_Abstract* create_entity(const char* section);
	void destroy_entity(CSE_Abstract*& abstract);
	void reload();
};

extern XrSEFactoryManager* g_SEFactoryManager;
