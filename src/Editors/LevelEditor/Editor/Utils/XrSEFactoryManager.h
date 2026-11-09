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
	// Sets the spawn version used by the factory writers, returns the previous one.
	u16 SetSpawnWriteVersion(u16 Version);
};

extern XrSEFactoryManager* g_SEFactoryManager;

// Sets the factory spawn write version for the lifetime of the object.
class SEFactorySpawnVersionScope
{
public:
	explicit SEFactorySpawnVersionScope(u16 Version) : Previous(g_SEFactoryManager->SetSpawnWriteVersion(Version))
	{
	}

	~SEFactorySpawnVersionScope()
	{
		g_SEFactoryManager->SetSpawnWriteVersion(Previous);
	}

	SEFactorySpawnVersionScope(const SEFactorySpawnVersionScope&) = delete;
	SEFactorySpawnVersionScope& operator=(const SEFactorySpawnVersionScope&) = delete;

private:
	u16 Previous;
};

extern XrSEFactoryManager* g_SEFactoryManager;
