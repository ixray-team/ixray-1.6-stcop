//---------------------------------------------------------------------------
#ifndef SHCompilerToolsH
#define SHCompilerToolsH

#include "../../xrEngine/Shader_xrLC.h"
#include "SHToolsInterface.h"

// refs
class PropValue;                               

class CSHCompilerTools: public ISHTools
{
	void  			ItemExist			(const char* name, bool& res){res = !!FindItem(name);}
	Shader_xrLC*			FindItem			(const char* name);
    Shader_xrLC_LIB			m_Library;

    ListItem*				m_Selected;
public:
    Shader_xrLC*			m_Shader;
    virtual void            OnRemoveItem		(UIItemListForm::Node& node); 
	virtual void            OnRenameItem		(UIItemListForm::Node& node, const char* old_full_name, const char* new_full_name, EItemType type);
    virtual void			AppendItem			(const char* path, const char* parent=0);
	virtual void			FillItemList		();
public:
							CSHCompilerTools 	(const ISHInit& init);
    virtual 				~CSHCompilerTools	();

    virtual const char*			ToolsName			(){return "Compiler Shader";}

	virtual void			Reload				();
	virtual void			Load				();
	virtual bool			Save				();

    virtual bool			OnCreate			();
    virtual void			OnDestroy			();
	virtual void 			OnActivate			();
	virtual void 			OnDeactivate		();


    // misc
    virtual void			ResetCurrentItem	();
    virtual void			SetCurrentItem		(const char* name, bool bView);
    virtual void			ApplyChanges		(bool bForced=false);

	virtual void 			RealUpdateProperties();
	virtual void 			RealUpdateList		();

	virtual void 			OnFrame				();
	virtual void 			OnRender			(){;}

    virtual void			OnDeviceCreate		(){;}
    virtual void			OnDeviceDestroy		(){;}
};
//---------------------------------------------------------------------------
#endif
