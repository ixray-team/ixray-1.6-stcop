#pragma once

#include "../../xrEngine/Bone.h"
#include "../../xrEngine/Motion.h"
#if 1
#	include "../../../Editors/Public/PropertiesListTypes.h"
//	#include "PropertiesListHelper.h"
#	include "..\Engine\XrGameMaterialLibraryEditors.h"
#	include "pick_defs.h"
#endif
#	include "..\..\..\Include\xrRender\Kinematics.h"

#include "PhysicsShellHolderEditorBase.h"
#include "Engine/GameMtlLib.h"

//----------------------------------------------------
struct 	SRayPickInfo;
class 	CEditableMesh;
class 	CFrustum;
class 	CCustomMotion;
class	CBone;
class	Shader;
class	Mtl;
class	CExporter;
class	CMayaTranslator;
struct	st_ObjectDB;
struct	SXRShaderData;
struct  ogf_desc;
class	CCustomObject;

#if 0
	class PropValue;
	#define ref_shader LPVOID
#endif

#define LOD_SHADER_NAME 		"details\\lod"
#define LOD_SAMPLE_COUNT 		8
#define LOD_IMAGE_SIZE 			64
#define RENDER_SKELETON_LINKS	4

// refs
class XRayMtl;
class SSimpleImage;

class ECORE_API CSurface
{
	u32				m_GameMtlID;
	ref_shader		m_Shader;
	enum ERTFlags{
		rtValidShader	= (1<<0),
	};
public:
	enum EFlags{
		sf2Sided		= (1<<0),
	};
	shared_str			m_Name;
	shared_str			m_Texture;	//
	shared_str			m_VMap;		//
	shared_str			m_ShaderName;
	shared_str			m_ShaderXRLCName;
	shared_str			m_GameMtlName;
	Flags32			m_Flags;
	u32				m_dwFVF;

	Flags32			m_RTFlags;
	u32				tag;
	SSimpleImage*	m_ImageData;
	u16				m_id = 0;

public:
	CSurface		()
	{
		m_GameMtlName="default";
		m_ImageData	= nullptr;
		m_Shader	= nullptr;
		m_RTFlags.zero	();
		m_Flags.zero	();
		m_dwFVF		= 0;
		tag			= 0;
	}
	IC bool			Validate		()
	{
		return (0!=xr_strlen(m_Texture))&&(0!=xr_strlen(m_ShaderName));
	}
#if 1
					~CSurface		(){R_ASSERT(!m_Shader);xr_delete(m_ImageData);}
	IC void			CopyFrom		(CSurface* surf){*this = *surf; m_Shader=nullptr; m_RTFlags.set(rtValidShader, false);}
	IC int			_Priority		()	{return (_Shader() && _Shader()->E[0]) ?_Shader()->E[0]->flags.iPriority:1;}
	IC bool			_StrictB2F		()	{return (_Shader() && _Shader()->E[0]) ?_Shader()->E[0]->flags.bStrictB2F:false;}
	IC ref_shader	_Shader			()	{if (!m_RTFlags.is(rtValidShader)) OnDeviceCreate(); return m_Shader;}
#endif
	IC const char*		_Name			()const {return *m_Name;}
	IC const char*		_ShaderName		()const {return *m_ShaderName;}
	IC const char*		_GameMtlName	()const {return *m_GameMtlName;}
	IC const char*		_ShaderXRLCName	()const {return *m_ShaderXRLCName;}
	IC const char*		_Texture		()const {return *m_Texture;}
	IC const char*		_VMap			()const {return *m_VMap;}
	IC u32			_FVF			()const {return m_dwFVF;}
	IC void			SetName			(const char* name){m_Name=name;}
	IC void			SetShader		(const char* name)
	{
		R_ASSERT2(name&&name[0],"Empty shader name."); 
		m_ShaderName=name; 
#if 1
		OnDeviceDestroy(); 
#endif
	}
	IC void 		SetShaderXRLC	(const char* name){m_ShaderXRLCName=name;}
	IC void			SetGameMtl		(const char* name){m_GameMtlName=name;}
	IC void			SetFVF			(u32 fvf){m_dwFVF=fvf;}
	IC void			SetTexture		(const char* name){string512 buf; xr_strcpy(buf, sizeof(buf), name); if(strext(buf)) *strext(buf)=0; m_Texture=buf;}
	IC void			SetVMap			(const char* name){m_VMap=name;}
#if 1
	IC u32			_GameMtl		()const	{return PGMLib->GetMaterialID	(*m_GameMtlName);}
	IC void			OnDeviceCreate	()
	{ 
		R_ASSERT(!m_RTFlags.is(rtValidShader));
		if (m_ShaderName.size()&&m_Texture.size())	m_Shader.create(*m_ShaderName,*m_Texture); 
		else                                       	m_Shader.create("editor\\wire");
		m_RTFlags.set(rtValidShader,true);
	}
	IC void			OnDeviceDestroy	()
	{
		m_Shader.destroy();
		m_RTFlags.set(rtValidShader,false);
	}
	void			CreateImageData	();
	void			RemoveImageData	();
	IC bool IsVoid()const
	{
		return !m_RTFlags.is(rtValidShader);
	}

#endif
};

using SurfaceVec = xr_vector<CSurface*>;
using SurfaceIt = SurfaceVec::iterator;

using EditMeshVec = xr_vector<CEditableMesh*>;
using EditMeshIt = EditMeshVec::iterator;

using OMotionVec = xr_vector<COMotion*>;
using OMotionIt = OMotionVec::iterator;

using SMotionVec = xr_vector<CSMotion*>;
using SMotionIt = SMotionVec::iterator;

struct ECORE_API SBonePart{
	shared_str 		alias;
	RStringVec 		bones;
};

using BPVec = xr_vector<SBonePart>;
using BPIt = BPVec::iterator;

class ECORE_API CEditableObject:
public IKinematics,
public CPhysicsShellHolderEditorBase
{
	friend class CSceneObject;
	friend class CEditableMesh;
	friend class TfrmPropertiesEObject;
	friend class CSector;
	friend class TUI_ControlSectorAdd;
	friend class ELibrary;
	friend class TfrmEditLibrary;
	friend class MeshExpUtility;

#if 1
	ref_geom 		vs_SkeletonGeom;
#endif
// desc
	shared_str 		m_CreateName;
	__time32_t			m_CreateTime;
	shared_str 		m_ModifName;
	__time32_t			m_ModifTime;
	
// general
	xr_string		m_ClassScript;

	EditMeshVec		m_Meshes;

	ref_shader		m_LODShader;

	// skeleton
	BoneVec			m_Bones;
	SMotionVec		m_SMotions;
	BPVec			m_BoneParts;
	CSMotion*		m_ActiveSMotion;
	CPhysicsShell*	m_physics_shell;
	Fmatrix*		m_object_xform;
public:

	SurfaceVec		m_Surfaces;
	SAnimParams				m_SMParam;
	xr_vector<shared_str>	m_SMotionRefs;
	shared_str				m_LODs;
public:
	// options
	Flags32			m_objectFlags;
	enum{
		eoDynamic 	 	= (1<<0),			
		eoProgressive 	= (1<<1),			
		eoUsingLOD		= (1<<2),			
		eoHOM			= (1<<3),			
		eoMultipleUsage	= (1<<4),			
		eoSoundOccluder	= (1<<5),
		eoHQExport      = (1<<6),           
		eoSkipOpt       = (1<<7),           
		eoFORCE32		= u32(-1)           
	};
	IC bool			IsDynamic				(){return m_objectFlags.is(eoDynamic);}
	IC bool			IsStatic				(){return !m_objectFlags.is(eoSoundOccluder)&&!m_objectFlags.is(eoDynamic)&&!m_objectFlags.is(eoHOM)&&!m_objectFlags.is(eoMultipleUsage);}
	IC bool			IsMUStatic				(){return !m_objectFlags.is(eoSoundOccluder)&&!m_objectFlags.is(eoDynamic)&&!m_objectFlags.is(eoHOM)&&m_objectFlags.is(eoMultipleUsage);}
private:
	// bounding volume
	Fbox 			m_BBox;
public:
	// temp variable for actor
	Fvector 		a_vPosition;
	Fvector			a_vRotate;

	// temp variables for transformation
	Fvector 		t_vPosition;
	Fvector			t_vScale;
	Fvector			t_vRotate;
   
	bool			bOnModified;
	IC bool			IsModified				(){return bOnModified;}
	IC void 		Modified				(){bOnModified=true;}

	xr_string		m_LoadName;
	int				m_RefCount;
protected:
	shared_str		AssignBoneName;

	time_t			m_ObjectVersion;

	void 			ClearGeometry			();

	void 			PrepareBones			();
	void			DefferedLoadRP			();
	void			DefferedUnloadRP		();

	void   OnChangeTransform		(PropValue* prop);
	void  	OnChangeShader			(PropValue* prop);
public:
	enum{
		LS_RBUFFERS	= (1<<0),
	};
	Flags32			m_LoadState;

	xr_string		m_LibName;
public:
	// constructor/destructor methods
					CEditableObject			(const char* name);
	virtual 		~CEditableObject		();

	const char*			GetName					(){ return m_LibName.c_str();}

	void			SetVersionToCurrent		(bool bCreate, bool bModif);

	void			Optimize				();

	IC EditMeshIt	FirstMesh				()	{return m_Meshes.begin();}
	IC EditMeshIt	LastMesh				()	{return m_Meshes.end();}
	IC EditMeshVec& Meshes					()	{return m_Meshes; }
	IC int			MeshCount				()	{return m_Meshes.size();}
	IC void			AppendMesh				(CEditableMesh* M){m_Meshes.push_back(M);}
	IC SurfaceVec&	Surfaces				()	{return m_Surfaces;}
	IC SurfaceIt	FirstSurface			()	{return m_Surfaces.begin();}
	IC SurfaceIt	LastSurface				()	{return m_Surfaces.end();}
	IC int			SurfaceCount			()	{return m_Surfaces.size();}
	IC time_t		Version 				() 	{return m_ObjectVersion;}

	// LOD
	xr_string		GetLODTextureName		();
	const char*			GetLODShaderName		(){return LOD_SHADER_NAME;}
	void			GetLODFrame				(int frame, Fvector p[4], Fvector2 t[4], const Fmatrix* parent=nullptr);

	// skeleton
	IC BPIt			FirstBonePart			()	{return m_BoneParts.begin();}
	IC BPIt			LastBonePart			()	{return m_BoneParts.end();}
	IC BPVec&		BoneParts				()	{return m_BoneParts;}
	IC int			BonePartCount			()	{return m_BoneParts.size();}
	IC BPIt			BonePart				(CBone* B);

	IC BoneIt		FirstBone				()	{return m_Bones.begin();}
	IC BoneIt		LastBone				()	{return m_Bones.end();}
	IC BoneVec&		Bones					()	{return m_Bones;}
	IC int			BoneCount				()const	{return m_Bones.size();}
	shared_str		BoneNameByID			(int id);
	int				GetRootBoneID			();
	int				PartIDByName			(const char* name);
	IC CBone*		GetBone					(u32 idx){VERIFY(idx<m_Bones.size()); return m_Bones[idx];}
	IC const CBone*	GetBone					(u32 idx)const{VERIFY(idx<m_Bones.size()); return m_Bones[idx];}
	void			GetBoneWorldTransform	(u32 bone_idx, float t, CSMotion* motion, Fmatrix& matrix);
	IC SMotionIt	FirstSMotion			()	{return m_SMotions.begin();}
	IC SMotionIt	LastSMotion				()	{return m_SMotions.end();}
	SMotionVec&		SMotions				()	{return m_SMotions;}
	IC int			SMotionCount 			()	{return m_SMotions.size();}
	IC bool			IsAnimated	 			()	{return SMotionCount() || m_SMotionRefs.size();}
	IC void			SkeletonPlay 			()	{m_SMParam.Play();}
	IC void			SkeletonStop 			()	{m_SMParam.Stop();}
	IC void			SkeletonPause 			(bool val)	{m_SMParam.Pause(val);}

	// get object properties methods

	IC xr_string&	GetClassScript			()	{return m_ClassScript;}

	IC const Fbox&	_BCL GetBox				() const 	{return m_BBox;}

	IC const char*		GetLODs					()	{return m_LODs.c_str();}

	// animation
	IC bool			IsSkeleton				()	{return !!m_Bones.size();}
	IC bool			IsSMotionActive			()	{return IsSkeleton()&&m_ActiveSMotion; }
	CSMotion*		GetActiveSMotion		()	{return m_ActiveSMotion; }
	void			SetActiveSMotion		(CSMotion* mot);
	bool 			CheckBoneCompliance		(CSMotion* M);
	bool			VerifyBoneParts			();
	void			OptimizeSMotions		();

	u16				BoneIDByName			(shared_str name) const;

	bool 			LoadBoneData			(IReader& F);
	void 			SaveBoneData			(IWriter& F);
	void			ResetBones				();
	CSMotion*		ResetSAnimation			(bool bGotoBindPose=true);
	void			CalculateAnimation		(CSMotion* motion);
	void			CalculateBindPose		();
	void			GotoBindPose			();
	void			OnBindTransformChange	();

	// statistics methods
	void 			GetFaceWorld			(const Fmatrix& parent, CEditableMesh* M, int idx, Fvector* verts);
	int 			GetFaceCount			(bool bMatch2Sided=true, bool bIgnoreOCC=true);
	int 			GetVertexCount			();
	int 			GetSurfFaceCount		(const char* surf_name);

	// render methods
	void 			Render					(CCustomObject*, const Fmatrix& parent, int priority, bool strictB2F,SurfaceVec * surfaces=nullptr);
	
	void 			RenderSelection			(CCustomObject*, CEditableMesh* m=0, u32 c=0x40E64646);
	void 			RenderEdge				(CCustomObject*, CEditableMesh* m=0, u32 c=0xFFC0C0C0);

	void 			RenderBones				(const Fmatrix& parent);
	void 			RenderAnimation			(const Fmatrix& parent);
	void 			RenderSingle			(CCustomObject*, const Fmatrix& parent);
	void 			RenderSkeletonSingle	(CCustomObject*, const Fmatrix& parent);
	void 			RenderLOD				(const Fmatrix& parent);

	// update methods
	void 			OnFrame					();
	void 			UpdateBox				();
	void		    EvictObject				();

	// pick methods
	bool 			RayPick					(float& dist, const Fvector& S, const Fvector& D, const Fmatrix& inv_parent, SRayPickInfo* pinf=nullptr);

	void			CreateBone				(shared_str Name);
	void			AddBone					(CBone* parent_bone);
	void			DeleteBone				(CBone* bone);
	void			RenameBone				(CBone* bone, const char* new_name);

	void 			RayQuery				(SPickQuery& pinf);
	void 			RayQuery				(const Fmatrix& parent, const Fmatrix& inv_parent, SPickQuery& pinf);
	void 			BoxQuery				(const Fmatrix& parent, const Fmatrix& inv_parent, SPickQuery& pinf);
	bool 			BoxPick					(CCustomObject* obj, const Fbox& box, const Fmatrix& inv_parent, SBoxPickInfoVec& pinf);
	bool 			FrustumPick				(const CFrustum& frustum, const Fmatrix& parent);
	bool 			SpherePick				(const Fvector& center, float radius, const Fmatrix& parent);

	// bone
	CBone* 			PickBone				(const Fvector& S, const Fvector& D, const Fmatrix& parent);
	void 			SelectBones				(bool bVal);
	void 			SelectBone				(CBone* b, bool bVal);
	void			ClampByLimits			(bool bSelOnly);

	// change position/orientation methods
	void 			TranslateToWorld		(const Fmatrix& parent);

	// clone/copy methods
	void			RemoveMesh				(CEditableMesh* mesh);

	bool			RemoveSMotion			(const char* name);
	bool			RenameSMotion			(const char* old_name, const char* new_name);
	bool			AppendSMotion			(const char* fname, SMotionVec* inserted=nullptr);
	void			ClearSMotions			();
	bool			SaveSMotions			(const char* fname);

	// load/save methods
	bool 			Reload					();
	bool 			Load					(const char* fname);
	bool 			Save					(const char* fname);
	bool 			Load					(IReader&);
	void 			Save					(IWriter&);
#if 1
	void 			FillMotionList			(const char* pref, ListItemsVec& items, int modeID);
	void 			FillBoneList			(const char* pref, ListItemsVec& items, int modeID);
	void			FillSurfaceList			(const char* pref, ListItemsVec& items, int modeID);
	void			FillSurfaceProps		(CSurface* surf, const char* pref, PropItemVec& items);
	void 			FillBasicProps			(const char* pref, PropItemVec& items);
	void 			FillSummaryProps		(const char* pref, PropItemVec& items);
	bool			CheckShaderCompatible	();
#endif

	// contains methods
	CEditableMesh* 	FindMeshByName			(const char* name, CEditableMesh* Ignore=nullptr);
	void			VerifyMeshNames			();
	bool 			ContainsMesh			(const CEditableMesh* m);
	CSurface*		FindSurfaceByName		(const char* surf_name, int* s_id=nullptr);
	int				FindBoneByNameIdx		(const char* name);
	BoneIt			FindBoneByNameIt		(const char* name);
	CBone*			FindBoneByName			(const char* name);
	int				GetSelectedBones		(BoneVec& sel_bones);
	u16				GetBoneIndexByWMap		(const char* wm_name);
	CSMotion* 		FindSMotionByName		(const char* name, const CSMotion* Ignore=nullptr);
	void			GenerateSMotionName		(char* buffer, const char* start_name, const CSMotion* M);
	bool			GenerateBoneShape		(bool bSelOnly);

	// device dependent routine
	void 			OnDeviceCreate 			();
	void 			OnDeviceDestroy			();

	// utils
	void			PrepareOGFDesc			(ogf_desc& desc);
	// skeleton
	bool			PrepareSVGeometry		(IWriter& F, u8 infl);
	bool			PrepareSVKeys			(IWriter& F);
	bool			PrepareSVDefs			(IWriter& F);
	bool			PrepareSkeletonOGF		(IWriter& F, u8 infl);
	// rigid
	bool			PrepareRigidOGF			(IWriter& F, bool gen_tb, CEditableMesh* mesh);
	// ogf
	bool			PrepareOGF				(IWriter& F, u8 infl, bool gen_tb, CEditableMesh* mesh);
	bool			ExportOGF				(const char* fname, u8 skl_infl);
	// omf
	bool			PrepareOMF				(IWriter& F);
	bool			ExportOMF				(const char* fname);
	// obj
	bool			ExportOBJ				(const char* name);

	const char*			GenerateSurfaceName		(const char* base_name);
#ifdef _MAYA_EXPORT
	bool			ParseMAMaterial			(CSurface* dest, SXRShaderData& d);
	CSurface*		CreateSurface			(const char* m_name, SXRShaderData& d);
	CSurface*		CreateSurface			(MObject shader);
#endif
	bool			ExportLWO				(const char* fname);
	bool			Validate				();
private:
	float 			GetBonesBottom			();
public:
	void 			CalculateRootObjectAnimation(const Fmatrix &anchor);
	void 			GetAnchorForRootObjectAnimation( Fmatrix &anchor );
	bool 			AnimateRootObject(CSMotion* motion);
private:
	virtual		void			Bone_Calculate		(CBoneData* bd, Fmatrix* parent) 									{ VERIFY(false); }
	virtual		void			Bone_GetAnimPos(Fmatrix& pos,u16 id, u8 channel_mask, bool ignore_callbacks) 			{ VERIFY(false); }

	virtual		bool			PickBone			(const Fmatrix &parent_xform, pick_result &r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id) { VERIFY(false); return false;}
	virtual		void			EnumBoneVertices	(SEnumVerticesCallback &C, u16 bone_id) 							{ VERIFY(false); }

	// Low level interface
	virtual u16			_BCL	LL_BoneID(const char*  B)																   	{ int id = FindBoneByNameIdx( B ); VERIFY(id<u16(-1)); return (u16)id; }
	virtual u16			_BCL	LL_BoneID(const shared_str& B)                                                          { return LL_BoneID( B.c_str() ); }
	virtual const char*		_BCL	LL_BoneName_dbg(u16 ID) 																;

	virtual CInifile*	_BCL	LL_UserData() 																			{ return nullptr; }
	virtual accel*				LL_Bones() 																				{ VERIFY(false); return nullptr; }

	virtual  CBoneInstance&	_BCL LL_GetBoneInstance(u16 bone_id);

	virtual CBoneData&	_BCL	LL_GetData(u16 bone_id);

virtual	const IBoneData&_BCL	GetBoneData(u16 bone_id) const 															{ return *GetBone( bone_id ); }

	virtual u16			_BCL	LL_BoneCount()const 																	{ return (u16)BoneCount(); }
	virtual u16					LL_VisibleBoneCount() 																	{ VERIFY(false); return 0; }
	virtual ICF Fmatrix& _BCL	LL_GetTransform(u16 bone_id) 															{ return GetBone( bone_id )->_LTransform(); }
	virtual ICF const Fmatrix& _BCL	LL_GetTransform(u16 bone_id) const 													{ return GetBone( bone_id )->_LTransform(); }
	virtual ICF Fmatrix&		LL_GetTransform_R(u16 bone_id);
	virtual Fobb&				LL_GetBox(u16 bone_id);
	virtual void				LL_GetBindTransform(xr_vector<Fmatrix>& matrices) 										{ VERIFY(false); }
	virtual int 				LL_GetBoneGroups(xr_vector<xr_vector<u16> >& groups) 									{ VERIFY(false); return 0; }

	virtual u16			_BCL	LL_GetBoneRoot() 																		{ u16 root_id = (u16)GetRootBoneID(); VERIFY( root_id < u16(-1) ); return root_id; }
	virtual void				LL_SetBoneRoot(u16 bone_id) 															{ VERIFY(false); }

	virtual bool		_BCL	LL_GetBoneVisible(u16 bone_id) 															{ return true; }
	virtual void				LL_SetBoneVisible(u16 bone_id, bool val, bool bRecursive) 								{ VERIFY(false); }

	virtual VisMask _BCL LL_GetBonesVisible() {
		VisMask x; x.set_all(); return x;
	}

	virtual void				LL_SetBonesVisibleAll() {};

	virtual void				LL_SetBonesVisible(VisMask mask) 														{ VERIFY(false); }
	virtual void				BonesHistory_Invalidate() override {}

	// Main functionality
	virtual void				CalculateBones(bool bForceExact	= false) 												{ } // Recalculate skeleton
	virtual void				CalculateBones_Invalidate()																{ }
	virtual void				Callback(UpdateCallback C, void* Param) 												{ VERIFY(false); }

	//	Callback: data manipulation
	virtual void				SetUpdateCallback(UpdateCallback pCallback) 											{ VERIFY(false); }
	virtual void				SetUpdateCallbackParam(void* pCallbackParam)											{ VERIFY(false); }

	virtual UpdateCallback		GetUpdateCallback() 																	{ VERIFY(false); return nullptr; }
	virtual void*				GetUpdateCallbackParam() 																{ VERIFY(false); return nullptr; }
	//UpdateCallback				Update_Callback;
	//void*						Update_Callback_Param;
	virtual IRenderVisual* _BCL dcast_RenderVisual()																	{ 	return nullptr; }
	virtual IKinematicsAnimated* dcast_PKinematicsAnimated() 															{ VERIFY(false); return nullptr; }

	// debug
#ifdef DEBUG_DRAW
	virtual void						DebugRender			(Fmatrix& XFORM) 											{VERIFY(false);}
#endif
	virtual shared_str					getDebugName		() 															{return m_ModifName;}

private:
	virtual	IKinematics*	 	_BCL	ObjectKinematics		()				 										{ return this;}
	int m_FaceCount;
	int m_VertexCount;

};
//----------------------------------------------------
#define EOBJ_CURRENT_VERSION		0x0010
//----------------------------------------------------
#define EOBJ_CHUNK_OBJECT_BODY		0x7777
#define EOBJ_CHUNK_VERSION		  	0x0900
#define EOBJ_CHUNK_REFERENCE     	0x0902
#define EOBJ_CHUNK_FLAGS           	0x0903
#define EOBJ_CHUNK_SURFACES			0x0905
#define EOBJ_CHUNK_SURFACES2		0x0906
#define EOBJ_CHUNK_SURFACES3		0x0907
#define EOBJ_CHUNK_EDITMESHES      	0x0910
#define EOBJ_CHUNK_CLASSSCRIPT     	0x0912
#define EOBJ_CHUNK_BONES			0x0913
#define EOBJ_CHUNK_SMOTIONS			0x0916
#define EOBJ_CHUNK_SURFACES_XRLC	0x0918
#define EOBJ_CHUNK_BONEPARTS		0x0919
#define EOBJ_CHUNK_ACTORTRANSFORM	0x0920
#define EOBJ_CHUNK_BONES2			0x0921
#define EOBJ_CHUNK_DESC				0x0922
#define EOBJ_CHUNK_BONEPARTS2		0x0923
#define EOBJ_CHUNK_SMOTIONS2		0x0924
#define EOBJ_CHUNK_LODS				0x0925
#define EOBJ_CHUNK_SMOTIONS3		0x0926
//----------------------------------------------------












