#pragma once

class CLevelPreferences: public CCustomPreferences
{
	typedef CCustomPreferences			inherited;
	void 	OnEnabledChange		(PropValue* prop);
	void 	OnReadonlyChange	(PropValue* prop);
protected:
    virtual void 	Load				();
    virtual void 	Save				();
public:
    virtual void	FillProp          	(PropItemVec& items);
	bool OpenObjectList;
	bool OpenProperties;
	bool OpenWorldProperties;
	bool OpenSnapList;
};