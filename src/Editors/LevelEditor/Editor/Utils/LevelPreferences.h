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

private:
	void LoadContentView(const nlohmann::json& Data, CContentView* View);
	void SaveContentView(nlohmann::json& Data, const CContentView* View) const;

	// Raw JSON of the Particles tab browser until it is created
	xr_string ParticleBrowserSettings;

public:

	bool PIEArtSpawnPos = false;
	bool OpenObjectList;
	bool OpenProperties;
	bool OpenWorldProperties;
	bool OpenSnapList;
	bool OpenLightAnim;
	bool PreviewRenderLibrary = false;
};