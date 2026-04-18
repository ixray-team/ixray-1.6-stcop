//---------------------------------------------------------------------------
#ifndef SHGameMtlToolsH
#define SHGameMtlToolsH

#include "SHToolsInterface.h"
#include "../../xrEngine/GameMtlLib.h"

// refs
class PropValue;

class CSHGameMtlTools: public ISHTools
{
	bool                    m_CreatingMtl;
	xr_string				m_CreatingMtlPath;
	ISHTools*				m_GameMtlPairTools;
	void  			ItemExist			(const char* name, bool& res){res = !!FindItem(name);}
	SGameMtl*				FindItem			(const char* name);
public:
    SGameMtl*				m_Mtl;
    virtual void			AppendItem			(const char* path, const char* parent_name=0);  
	virtual void			AppendItem(const char* path, bool dynamic=false, SGameMtl* parent=0);
    virtual void   OnRemoveItem		(UIItemListForm::Node& node); 
	virtual void   OnRenameItem		(UIItemListForm::Node& node, const char* old_full_name, const char* new_full_name, EItemType type);
	void   		FillChooseMtlType	(ChooseItemVec& items, void* param);

    virtual void			FillItemList		();
public:
							CSHGameMtlTools 	(const ISHInit& init);
    virtual 				~CSHGameMtlTools	();

    virtual const char*			ToolsName			(){return "Game Materials";}

	virtual void			Reload				();
	virtual void			Load				();
	virtual bool			Save				();
    virtual void			ApplyChanges		(bool bForced=false);

    virtual bool			OnCreate			();
    virtual void			OnDestroy			();
	virtual void 			OnActivate			();
	virtual void 			OnDeactivate		();

    // misc
    virtual void			ResetCurrentItem	();
    virtual void			SetCurrentItem		(const char* name, bool bView);

	virtual void 			RealUpdateProperties();
	virtual void 			RealUpdateList		();

	virtual void 			OnFrame				();
	virtual void 			OnRender			(){;}

    virtual void			OnDeviceCreate		(){;}
    virtual void			OnDeviceDestroy		(){;}

	virtual void			OnDrawUI		();
};
//---------------------------------------------------------------------------
#endif // SHGameMtlToolsH
