// Build.h: interface for the CBuild class.
//
//////////////////////////////////////////////////////////////////////
#pragma once
#include "../xrDXT/xrDXT.h"
#include "../../xrCore/FS.h"
#include "../../xrCore/Collision/xrCDB.h"
#include "../Shader_xrLC.h"

#include "../xrLC_Light/b_build_texture.h"
#include "../xrLC_Light/xrFace.h"

class xrLC_GlobalData;
class xrMU_Model;
class xrMU_Reference;
class CSector;

extern "C" XRLC_LIGHT_API xrLC_GlobalData*	lc_global_data();
//////////////////////////////////////////////////////////////////////////
// tesselator callbacks



typedef	int		tesscb_estimator	( const Face* F );	// -1 = none, 0,1,2 = edge-number
typedef void	tesscb_face			(Face*		F);	// new face
typedef void	tesscb_vertex		(Vertex*	V);	// new vertex

class  base_lighting;
extern size_t GetHeapMemory();
extern size_t GetHeapMemoryIXray();

//////////////////////////////////////////////////////////////////////////
class CBuild  
{
public:
	CMemoryWriter					&err_invalid();
	CMemoryWriter					&err_tjunction();
	CMemoryWriter					&err_multiedge();
	void							err_save	();

	Fbox							scene_bb;
	xr_vector<b_shader>				shader_render;
	xr_vector<b_shader>				shader_compile;
    xr_vector<b_light_dynamic>		L_dynamic;
	xr_vector<b_glow>				glows;
	xr_vector<b_portal>				portals;
	xr_vector<b_lod>				lods;
	string_path						path;
	xr_vector<const char*>				g_Shaders;
	CDB::CollectorPacked			CL;

	static u16 GetMaterialSector(const Face& F);
	static u16 GetMaterialReserved(const Face& F);
	static u32 GetMaterialInternalMaxArea(const Face& F);
	static u32& GetMutableMaterialInternalMaxArea(const Face& F);
	static b_BuildTexture& GetTexture(const Face& F);
	static u16 GetMaterialSector(u16 index, bool shared);
	static u16 GetMaterialReserved(u16 index, bool shared);
	static u32 GetMaterialInternalMaxArea(u16 index, bool shared);
	static b_BuildTexture& GetTexture(u16 index, bool shared);
	
	static Shader_xrLC& GetShaderXRLC(const Face& F);
	static Shader_xrLC& GetShaderXRLC(u32 ID, bool Shared);

	LPCSTR GetMaterialShaderName(const Face& F) const;
	LPCSTR GetMaterialShaderName(u16 index, bool shared) const;
	LPCSTR GetMaterialShaderXRLCName(const Face& F) const;
	LPCSTR GetMaterialShaderXRLCName(u16 index, bool shared) const;

	static xr_vector<b_material>& materials();
	static xr_vector<b_material_shared>& materials_shared();
	static xr_vector<b_BuildTexture>& textures();
	static xr_hash_map<b_material_shared*, b_BuildTexture>& textures_shared();
	static base_lighting& L_static();
	static xr_vector<xrMU_Model*>& mu_models();
	static xr_vector<xrMU_Reference*>& mu_refs();

	static Shader_xrLC_LIB& shaders();

public:
	void	Load					(const b_params& P, const IReader&  fs);
  	void	Run						(const char* path);
 
	void	RunAfterLight			( IWriter* fs	);
	void	PreOptimize				();
	void	CorrectTJunctions		();

	void	xrPhase_AdaptiveHT_tessalte		();
	void	xrPhase_AdaptiveHT_calculate	();


	void	u_Tesselate				(tesscb_estimator* E, tesscb_face* F, tesscb_vertex* V);
	void	u_SmoothVertColors		(int count);

	void	CalcNormals				();
	void	MU_ModelsCalculateNormals();
	void	xrPhase_TangentBasis	();

	void	BuildCForm				();
 	void	BuildPortals			(IWriter &fs);
	 
		
	void	IsolateVertices			(bool bProgress);
	void	xrPhase_ResolveMaterials();
	void	xrPhase_UVmap			();
	void	xrPhase_Subdivide		();
	void	ImplicitLighting		();

	// Lighting Functions
 	void	Light_prepare			();
 	void	Light					();
 

	// Lmaps Processing 
 	void	LMaps					();
  	void	LightVertex				();
	void	xrPhase_MergeLM			();
 
	void	xrPhase_MergeGeometry	();

	// Converting OGF
	void	Flex2OGF				();
	void	BuildSectors			();
	
	// Saving
	void	SaveLights				(IWriter &fs);
	void	SaveTREE				(IWriter &fs);
	void	SaveSectors				(IWriter &fs);

	void	validate_splits			();
	bool	IsOGFContainersEmpty	();
	void	CheckBeforeSave			( u32 stage );
	void	TempSave				( u32 stage );
 
	xr_vector<CSector*>	g_sectors;

	CBuild	();
	~CBuild	();

	int lmapNameID = 0;
};

extern CBuild*		pBuild;			;
extern vec2Face		g_XSplit		;
