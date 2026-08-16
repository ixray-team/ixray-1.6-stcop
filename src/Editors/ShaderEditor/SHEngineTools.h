//---------------------------------------------------------------------------
#ifndef SHEngineToolsH    
#define SHEngineToolsH

#include "SHToolsInterface.h"
#include "SHPreviewObject.h"

using TemplateVec = xr_vector<IBlender*>;
using TemplateIt = TemplateVec::iterator;

using ConstantMap = xr_map<LPSTR, CConstant*, str_pred>;
using ConstantPairIt = ConstantMap::iterator;

using MatrixMap = xr_map<LPSTR, CMatrix*, str_pred>;
using MatrixPairIt = MatrixMap::iterator;

using BlenderMap = xr_map<LPSTR, IBlender*, str_pred>;
using BlenderPairIt = BlenderMap::iterator;

// refs
class CSHEngineTools;
class CEditableObject;

class CParseBlender{
public:
	virtual void Parse(CSHEngineTools* owner, DWORD type, const char* key, LPVOID data)=0;
};

enum EPreviewObj{
	pvoNone,
	pvoPlane,
    pvoBox,
    pvoSphere,
    pvoTeapot,
    pvoCustom,
    pvo_force_dword = u32(-1)
};

class CSHEngineTools: public ISHTools
{
	SStringVec				MCString;

	u32						m_PreviewObjectType;
	CEditableObject*		m_PreviewObject;
    bool					m_bCustomEditObject;

	bool					m_bFreezeUpdate;
    bool					m_bNeedResetShaders;
    bool					m_RemoteRenBlender;
    bool                    m_CreatingBlender;
    xr_string               m_CreatingBlenderPath;
    bool                    m_SetCustomObject;

    xr_string				m_RenBlenderOldName;
    xr_string				m_RenBlenderNewName;

    CPreviewObject			m_Preview;

	TemplateVec				m_TemplatePalette;

	ConstantMap				m_OptConstants;
	MatrixMap				m_OptMatrices;
	ConstantMap				m_Constants;
	MatrixMap				m_Matrices;
	BlenderMap				m_Blenders;

	void  			ItemExist			(const char* name, bool& res){res = !!FindItem(name);}
	IBlender*				FindItem			(const char* name);

	void 					AddMatrixRef		(LPSTR name);
	CMatrix*				FindMatrix			(const char* name);
	CMatrix*				AppendMatrix		(LPSTR name);
    const char*					GenerateMatrixName	(LPSTR name);
    const char*					AppendMatrix		(CMatrix* src=0, CMatrix** dest=0);
    void					RemoveMatrix		(const char* name);   

	void 					AddConstantRef		(LPSTR name);
	CConstant*				FindConstant		(const char* name);
	CConstant*				AppendConstant		(LPSTR name);
    const char*					GenerateConstantName(LPSTR name);
    const char*					AppendConstant		(CConstant* src=0, CConstant** dest=0);
    void					RemoveConstant		(const char* name);

friend class CCollapseBlender;
friend class CRefsBlender;
friend class CRemoveBlender;
friend class TfrmShaderProperties;
    void					CollapseMatrix		(LPSTR name);
    void					CollapseConstant	(LPSTR name);
    void					CollapseReferences	();
    void					UpdateMatrixRefs	(LPSTR name);
    void					UpdateConstantRefs	(LPSTR name);
    void					UpdateRefCounters	();

    void 					ParseBlender		(IBlender* B, CParseBlender& P);

	CMemoryWriter			m_BlenderStream;	// ������������ ��������� ���������� ������ ��� �������������
    bool 					m_bUpdateCurrent;	// ���� ������� ������ ���������������  Update____From___()
    bool					m_bCurBlenderChanged;

    void 					Save				(CMemoryWriter& F);
    void 					PrepareRender		();

    // template
	void   		FillChooseTemplate	(ChooseItemVec& items, void* param);
    // matrix props                                                
	bool   		MatrixOnAfterEdit	(PropValue* sender, xr_string& edit_val);
	void   		FillMatrixProps		(PropItemVec& items, const char* pref, LPSTR name);
	void   		MCOnDraw			(PropValue* sender, xr_string& draw_val);
    // constant props
	bool   		ConstOnAfterEdit	(PropValue* sender, xr_string& edit_val);
	void   		FillConstProps		(PropItemVec& items, const char* pref, LPSTR name);
    // name                                 
	bool   		NameOnAfterEdit		(PropValue* sender, xr_string& edit_val);

    void 					RealResetShaders	();

    ID3DBlob*				GetCurrentVSSignature();
    void					UpdatePreviewShader	();


	void   		FillMatrix			(PropItemVec& values, const char* pref, CMatrix* m);
	void   		FillConst			(PropItemVec& values, const char* pref, CConstant* c);

    void					ResetShaders		(bool bForced=false){m_bNeedResetShaders=true; if (bForced) RealResetShaders(); }
    void					UpdateObjectShader	();

    bool  			OnPreviewObjectRefChange(PropValue* sender, u32& edit_val); 
    void  			OnPreviewObjectRefChange(const char*name);
public:
	CMemoryWriter			m_RenderShaders;

    IBlender*				m_CurrentBlender;
	void					RemoteRenameBlender	(const char* old_full_name, const char* new_full_name){m_RemoteRenBlender=true;m_RenBlenderOldName=old_full_name;m_RenBlenderNewName=new_full_name;}

    Shader_xrLC*			m_Shader;

    virtual void			AppendItem			(const char* path, const char* parent=0);        
	virtual void			RealRenameItem		(const char* old_full_name, const char* new_full_name);
    virtual void            OnRemoveItem		(UIItemListForm::Node& node); 
	virtual void            OnRenameItem		(UIItemListForm::Node& node, const char* old_full_name, const char* new_full_name, EItemType type);
	virtual void			FillItemList		();

    void					UpdateStreamFromObject();
    void					UpdateObjectFromStream();

    void 					ClearData			();
public:
							CSHEngineTools		(const ISHInit& init);
    virtual 				~CSHEngineTools		();

    virtual const char*			ToolsName			(){return "Engine Shader";}

	virtual void			Reload				();
	virtual void			Load				();
	virtual bool			Save				();

    virtual bool			OnCreate			();
    virtual void			OnDestroy			();
    virtual void			OnActivate			();
    virtual void			OnDeactivate		();

    // misc
    virtual void			ResetCurrentItem	();
    virtual void			SetCurrentItem		(const char* name, bool bView);
    virtual void			ApplyChanges		(bool bForced=false);

	virtual void 			RealUpdateProperties();
	virtual void 			RealUpdateList		();

    virtual void			OnFrame				();
	virtual void 			OnRender			();

    virtual void			OnDeviceCreate		();
    virtual void			OnDeviceDestroy		(){;}

    virtual void			ZoomObject			(bool bOnlySel);
	virtual void 			OnShowHint			(AStringVec& ss);
    virtual void            OnDrawUI       ();
private:
    void                    AppendItem(const char* path, CLASS_ID cls, IBlender* parent=nullptr);
};
//---------------------------------------------------------------------------
#endif
