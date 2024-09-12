#pragma once
#include "../xrEngine/string_table.h"

class ESceneWayTool: public ESceneCustomOTool
{
	typedef ESceneCustomOTool inherited;
protected:
    // controls
    virtual void 		CreateControls			();
	virtual void 		RemoveControls			();
public:
						ESceneWayTool			():ESceneCustomOTool(OBJCLASS_WAY){;}
	// definition
    IC LPCSTR			ClassName				(){return "way";}
    IC LPCSTR			ClassDesc				(){return g_pStringTable->translate("ed_st_way_points").c_str();}
    IC int				RenderPriority			(){return 1;}

    virtual void		Clear					(bool bSpecific=false){inherited::Clear(bSpecific);}
    // IO
    virtual bool   		IsNeedSave				(){return inherited::IsNeedSave();}
    virtual bool   		LoadStream            		(IReader&);
    virtual bool   		LoadLTX            		(CInifile&);
    virtual void   		SaveStream            	(IWriter&);
    virtual void   		SaveLTX            		(CInifile&, int id);
    virtual bool		LoadSelection      		(IReader&);
    virtual void		SaveSelection      		(IWriter&);

    virtual void    	OnActivate  			();

    virtual CCustomObject* CreateObject			(LPVOID data, LPCSTR name);
};