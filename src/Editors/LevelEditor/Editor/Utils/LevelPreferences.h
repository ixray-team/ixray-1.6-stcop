#pragma once

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

public:

	bool OpenObjectList;
	bool OpenProperties;
	bool OpenWorldProperties;
	bool OpenSnapList;
	bool OpenLightAnim;
};