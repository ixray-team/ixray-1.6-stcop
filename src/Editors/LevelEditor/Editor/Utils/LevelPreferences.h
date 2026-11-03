#pragma once

class CContentView;

class CLevelPreferences : 
	public CCustomPreferences
{
	using inherited = CCustomPreferences;

	void OnEnabledChange(PropValue* prop);
	void OnReadonlyChange(PropValue* prop);

protected:
	virtual void Load();
	virtual void Save();

public:
	virtual void FillProp(PropItemVec& items);

	// Recent objects of the Actor Editor tab, kept apart from the recent levels
	void AppendActorRecentFile(const char* Name);

public:

	bool PIEArtSpawnPos = false;
	bool PreviewRenderLibrary = false;

	// Actor Editor
	bool ActorAlwaysShowKeyBar12 = false;
	bool ActorAlwaysShowKeyBar34 = false;
	AStringVec ActorRecentFiles;
};