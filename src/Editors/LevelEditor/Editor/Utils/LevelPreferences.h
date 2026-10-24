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

	// Applies the saved settings of the Particles tab content browser
	void LoadParticleBrowser(CContentView* View);
	// Applies the saved settings of the Actors tab content browser
	void LoadActorBrowser(CContentView* View);

	// Recent objects of the Actor Editor tab, kept apart from the recent levels
	void AppendActorRecentFile(const char* Name);

private:
	void LoadContentView(const nlohmann::json& Data, CContentView* View);
	void SaveContentView(nlohmann::json& Data, const CContentView* View) const;

	// Raw JSON of the Particles tab browser until it is created
	xr_string ParticleBrowserSettings;
	// Raw JSON of the Actors tab browser until it is created
	xr_string ActorBrowserSettings;

public:

	bool PIEArtSpawnPos = false;
	bool OpenObjectList;
	bool OpenProperties;
	bool OpenWorldProperties;
	bool OpenSnapList;
	bool OpenLightAnim;
	bool OpenWorldPartition = false;
	bool PreviewRenderLibrary = false;

	// Actor Editor
	bool ActorAlwaysShowKeyBar12 = false;
	bool ActorAlwaysShowKeyBar34 = false;
	AStringVec ActorRecentFiles;
};