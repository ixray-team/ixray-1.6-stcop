#pragma once
#include "../../Entry/Puddles/puddle.h"

class EScenePuddlesTool: 
	public ESceneCustomOTool
{
	typedef ESceneCustomOTool inherited;
	friend class SceneBuilder;

protected:
	// light control
	int					lcontrol_last_idx;
	RTokenVec			lcontrols;
	void   	OnControlAppendClick		(ButtonValue* sender, bool& bDataModified, bool& bSafe);
	void   	OnControlRenameRemoveClick	(ButtonValue* sender, bool& bDataModified, bool& bSafe);
protected:

	// controls
	virtual void 		CreateControls			();
	virtual void 		RemoveControls			();
public:
						EScenePuddlesTool 	   	();
	virtual        	 	~EScenePuddlesTool		();

	virtual void		Clear					(bool bSpecific=false);

	// definition
	IC LPCSTR			ClassName				(){return "puddles";}
	IC LPCSTR			ClassDesc				(){return "Puddles";}
	IC int				RenderPriority			(){return 11;}

	// IO
	virtual bool   		IsNeedSave				(){return true;}
	//virtual bool   	LoadStream            	(IReader&);
	//virtual bool   	LoadLTX            		(CInifile&);
	//virtual void   	SaveStream            	(IWriter&);
	//virtual void   	SaveLTX            		(CInifile&, int id);
	//virtual bool		LoadSelection      		(IReader&);
	//virtual void		SaveSelection      		(IWriter&);

	// utils
	virtual bool		Validate				(bool full_build);
	
	virtual void		BeforeRender			();
	virtual void		OnRender				(int priority, bool strictB2F);
	virtual void		AfterRender				();

	virtual void 		FillProp				(LPCSTR pref, PropItemVec& items);

	virtual CCustomObject* CreateObject			(LPVOID data, LPCSTR name);
private:
	virtual void OnDrawUI();
};