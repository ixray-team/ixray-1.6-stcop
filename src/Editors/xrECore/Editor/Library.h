//----------------------------------------------------
// file: Library.h
//----------------------------------------------------

#ifndef LibraryH
#define LibraryH

#include "../../xrEngine/pure.h"     
//----------------------------------------------------
class CEditableObject;

using EditObjMap = xr_map<AnsiString, CEditableObject*, astr_pred>;
using EditObjPairIt = EditObjMap::iterator;
//----------------------------------------------------
class ECORE_API ELibrary//:	public pureDeviceCreate, public pureDeviceDestroy
{
	bool				m_bReady;
	friend class TfrmChoseObject;
	EditObjMap			m_EditObjects;

    CEditableObject*	LoadEditObject		(const char* full_name);
    void				UnloadEditObject	(const char* full_name);
public:
						ELibrary			();
	virtual 			~ELibrary			();
                    
    void  		RemoveObject		(const char* fname, EItemType type, bool& res);
    void  		RenameObject		(const char* fn0, const char* fn1, EItemType type);

	void 				OnCreate			();
	void 				OnDestroy			();
	void 				Save				(FS_FileSet* modif_map=nullptr);

    void 				ReloadObjects		();
    void 				CleanLibrary		();
    void 				ReloadObject		(const char* name);

    CEditableObject*	CreateEditObject	(const char* name);
    void				RemoveEditObject	(CEditableObject*& object);

    int					GetObjects			(FS_FileSet& files);
    int					ObjectCount			(){return m_EditObjects.size();}

    void				EvictObjects		();

	virtual		void	OnDeviceCreate		();
	virtual		void	OnDeviceDestroy		();
};

extern ECORE_API ELibrary Lib;
//----------------------------------------------------
#endif /*_INCDEF_Library_H_*/

