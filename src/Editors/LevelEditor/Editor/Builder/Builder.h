#pragma once


// refs
class CSceneObject;
class CEditableMesh;
class EScene;
class CLight;
class CGlow;
class CPortal;
struct st_SPData;
class CSurface;
struct st_DPSurface;
class CSceneStat;

// some types
typedef Fvector b_vnormal;

struct sb_light_control						// controller or "layer", 30fps
{
	string64			name;				// empty for base layer
    U32Vec				data;
};

struct e_b_lod{        
	b_lod				lod;
    U32Vec				data;
    U32Vec				ndata;
    shared_str		   	lod_name;
};

class SceneBuilder{
public:
    struct SBuildLight{
        Flight	light;
        float	energy;
    };
    using BLVec = xr_vector<SBuildLight>;
    using BLIt = BLVec::iterator;
protected:
    BLVec						simple_hemi;
public:
	CEditableObject*			object_for_render;

    int							l_vert_cnt, l_vert_it;
	int							l_face_cnt, l_face_it;
    b_vertex*		        	l_verts;
    b_face*  		        	l_faces;
    u32*						l_smgroups;

    xr_vector<b_mu_model>		l_mu_models;
    xr_vector<b_mu_reference>	l_mu_refs;
    xr_vector<e_b_lod>			l_lods;
    xr_vector<sb_light_control>	l_light_control;
    xr_vector<b_light_static>	l_light_static;
    xr_vector<b_light_dynamic>	l_light_dynamic;
    xr_vector<b_texture_real>       	l_textures;
    xr_vector<b_shader>        	l_shaders;
    xr_vector<b_shader>        	l_shaders_xrlc;
    xr_vector<b_material>      	l_materials;
    xr_vector<b_vnormal>       	l_vnormals;
    xr_vector<b_glow>          	l_glows;
    xr_vector<b_portal>        	l_portals;
    xr_vector<Flight>          	l_light_keys;

    CSceneStat*					l_scene_stat;

    void    GetBBox         (u32 st_fid, u32 cnt, Fbox& box);

    bool    BuildGlow       (CGlow* e);
    void    BuildPortal   	(b_portal* b, CPortal* e);
    bool    BuildMesh       (const Fmatrix& parent, CEditableObject* object, CEditableMesh* mesh, int sector_num,
    						b_vertex* verts, int& vert_cnt, int& vert_it,
                            b_face* faces, int& face_cnt, int& face_it, u32* smooth_groups, const Fmatrix& real_transform, CSceneObject* obj);
    bool    BuildObject     (CSceneObject* obj);
    bool    BuildEditableObject(CEditableObject* obj, Fmatrix T, CSceneObject* Owner);
    bool    BuildMUObject   (CSceneObject* obj);

    void    Clear 			();

    int		BuildLightControl(const char* name);
    void 	BuildHemiLights	(u8 quality, const char* lcontrol);
	void	AppendLight		();
    bool 	BuildSun		(u8 quality, Fvector2 dir);
    bool 	BuildPointLight	(b_light* b, const Flags32& usage, FixedVector<WORD,16>* sectors, FvectorVec* soft_points, const Fmatrix* soft_transform=0);
    bool    BuildLight		(CLight* e);

    int     FindInLODs   	(b_lod* s);
    int		BuildObjectLOD  (const Fmatrix& parent, CEditableObject* e, int sector_num);

    int     FindInShaders   (b_shader* s);
    int     BuildShader     (const char* s);

	int 	FindInShadersXRLC(b_shader* s);
	int 	BuildShaderXRLC	(const char * s);

	int 	FindInTextures	(const char* name);
    int     BuildTexture    (const char* name);

    int     FindInMaterials (b_material* m);
	int 	BuildMaterial	(CSurface* surf, int sector_num, bool allow_draft);
	int 	BuildMaterial	(const char* esh_name, const char* csh_name, const char* tx_name, u32 tx_cnt, int sector_num, bool allow_draft);

    bool	ParseStaticObjects	(ObjectList& lst, const char* prefix, bool b_selected_only);

	int 	CalculateSector		(const Fvector& P, float R);

    void 	SaveBuild			();
    void 	SaveBuildAsObject	();
protected:
	friend void SaveBuild	();
    friend class TfrmBuildProgress;

	Fbox 	m_LevelBox;
public:
	bool		m_save_as_object;
	string_path	m_LevelPath;
    xr_string	MakeLevelPath		(const char* nm){return xr_string(m_LevelPath)+xr_string(nm);}
    bool 	PreparePath				();
protected:
	bool 	EvictResource			();
	bool 	PrepareFolders          ();

	bool 	GetBounding            	();

    bool	ParseLTX				(CInifile* pIni, ObjectList& lst, const char* prefix=0);
	bool 	BuildLTX                ();
    bool	ParseGAME				(IWriter& game, IWriter& spawn, ObjectList& lst, const char* prefix=0);
    bool 	BuildGame				();

    bool	BuildSceneStat			();
    bool 	BuildHOMModel			();
    bool 	BuildSOMModel			();
    bool	BuildAIMap				(bool Legacy);
    bool	BuildWallmarks			();
    bool 	CompileStatic		   	(bool b_selected_only);

	int 	m_iDefaultSectorNum;
	bool 	RenumerateSectors		();
public:
			SceneBuilder            ();
	virtual ~SceneBuilder           ();

	bool	Compile            		(bool b_selected_only,bool show_message=true);
	bool 	MakeGame				();
	bool 	MakePuddles				();
    bool 	MakeDetails				();
    bool 	MakeHOM					();
	bool 	MakeSOM					();
    bool	MakeAIMap				(bool Legacy);

    void	OnRender				();
};

extern SceneBuilder Builder;
