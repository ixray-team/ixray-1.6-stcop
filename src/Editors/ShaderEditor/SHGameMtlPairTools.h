//---------------------------------------------------------------------------
#ifndef SHGameMtlPairToolsH
#define SHGameMtlPairToolsH

#include "SHToolsInterface.h"
#include "../../xrEngine/GameMtlLib.h"

// refs
class PropValue;

class CSHGameMtlPairTools: public ISHTools
{
	ISHTools*				m_GameMtlTools;
    u32						m_StoreFlags;
public:
    SGameMtlPair*			m_MtlPair;                                
    virtual void			AppendItem			(const char* folder_name, const char* parent_name=0){}
	virtual void 			FillItemList		();
public:
							CSHGameMtlPairTools (const ISHInit& init);
    virtual 				~CSHGameMtlPairTools();

    virtual const char*			ToolsName			(){return "Game Material Pairs";}

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
    virtual void			OnRender			(){;}

    virtual void			OnDeviceCreate		(){;}
    virtual void			OnDeviceDestroy		(){;}
    virtual void            OnDrawUI       ();
};
//---------------------------------------------------------------------------
#endif // SHGameMaterialToolsH
