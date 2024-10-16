//---------------------------------------------------------------------------
#pragma once
#include "../../xrEngine/GameMtlLib.h"

class ECORE_API SGameMtlEditor:public SGameMtl
{
public:
	SGameMtlEditor();
    virtual ~SGameMtlEditor();
    virtual void 				Save(IWriter& fs);
    IC int				GetID() { return ID; }
    void 				FillProp(PropItemVec& values, ListItem* owner);
};

class ECORE_API SGameMtlPairEditor:public SGameMtlPair
{
public:
    bool m_EditParent;
    bool m_EditCommand;
    void OnFlagChange(PropValue* sender);
    void OnParentClick(ButtonValue* sender, bool& bModif, bool& bSafe);
    void OnCommandClick(ButtonValue* sender, bool& bModif, bool& bSafe);
    void FillChooseMtl(ChooseItemVec& items, void* param);
    void CopyFrom(SGameMtlPairEditor* parent);
	SGameMtlPairEditor(CGameMtlLibrary* owner);
    void 				FillProp(PropItemVec& values);
    void				TransferFromParent(SGameMtlPairEditor* parent);
    void                OnDrawUI();
    virtual ~SGameMtlPairEditor();
	BOOL				SetParent(int parent);
    virtual void		Load(IReader& fs);
	virtual void		Save(IWriter& fs);
};

class ECORE_API XrGameMaterialLibraryEditors:public CGameMtlLibrary
{
public:
	XrGameMaterialLibraryEditors();
	~XrGameMaterialLibraryEditors();
	virtual void				Unload()
	{
		for (GameMtlIt m_it = materials.begin(); materials.end() != m_it; ++m_it)
			xr_delete(*m_it);
		materials.clear();
		for (GameMtlPairIt p_it = material_pairs.begin(); material_pairs.end() != p_it; ++p_it)
			xr_delete(*p_it);
		material_pairs.clear();
	}
	// material routine
	virtual GameMtlIt 		GetMaterialIt(LPCSTR name)
	{
		for (GameMtlIt it = materials.begin(); materials.end() != it; ++it)
			if (0 == strcmpi(*(*it)->m_Name, name)) return it;
		return materials.end();
	}
	virtual GameMtlIt 		GetMaterialIt(shared_str& name)
	{
		for (GameMtlIt it = materials.begin(); materials.end() != it; ++it)
			if (name.equal((*it)->m_Name)) return it;
		return materials.end();
	}
	virtual GameMtlIt 		GetMaterialItByID(int id)
	{
		for (GameMtlIt it = materials.begin(); materials.end() != it; ++it)
			if ((*it)->ID == id) return it;
		return materials.end();
	}
	virtual u32				GetMaterialID(LPCSTR name)
	{
		GameMtlIt it = GetMaterialIt(name);
		return (it == materials.end()) ? GAMEMTL_NONE_ID : (*it)->ID;
	}
	// editor
	SGameMtl* AppendMaterial(SGameMtl* parent);
	void				RemoveMaterial(LPCSTR name);
	virtual SGameMtl* GetMaterialByID(int ID)
	{
		GameMtlIt it = GetMaterialItByID(ID);
		return materials.end() != it ? *it : 0;
	}
	virtual SGameMtl* GetMaterial(LPCSTR name)
	{
		GameMtlIt it = GetMaterialIt(name);
		return materials.end() != it ? *it : 0;
	}
	virtual u16				GetMaterialIdx(int ID) { GameMtlIt it = GetMaterialItByID(ID); VERIFY(materials.end() != it); return (u16)(it - materials.begin()); }
	virtual u16				GetMaterialIdx(LPCSTR name) { GameMtlIt it = GetMaterialIt(name); VERIFY(materials.end() != it); return (u16)(it - materials.begin()); }
	virtual SGameMtl* GetMaterialByIdx(u16 idx) { VERIFY(idx < (u16)materials.size()); return materials[idx]; }


	virtual GameMtlIt		FirstMaterial() { return materials.begin(); }
	virtual GameMtlIt		LastMaterial() { return materials.end(); }
	virtual u32				CountMaterial() { return materials.size(); }

	void 				CopyMtlPairs(SGameMtl* from, SGameMtl* to);
	BOOL				UpdateMtlPairs(SGameMtl* src);
	BOOL				UpdateMtlPairs();
	LPCSTR				MtlPairToName(int mtl0, int mtl1);
	void				NameToMtlPair(LPCSTR name, int& mtl0, int& mtl1);
	void				MtlNameToMtlPair(LPCSTR name, int& mtl0, int& mtl1);
	SGameMtlPair* CreateMaterialPair(int m0, int m1, SGameMtlPair* parent = 0);
	SGameMtlPair* AppendMaterialPair(int m0, int m1, SGameMtlPair* parent = 0);
	void				RemoveMaterialPair(LPCSTR name);
	void				RemoveMaterialPair(GameMtlPairIt rem_it);
	void				RemoveMaterialPair(int mtl);
	void				RemoveMaterialPair(int mtl0, int mtl1);
	GameMtlPairIt		GetMaterialPairIt(int id);
	SGameMtlPairEditor* GetMaterialPair(int id);
	GameMtlPairIt		GetMaterialPairIt(u16 mtl0, u16 mtl1);
	virtual SGameMtlPair* GetMaterialPair(u16 mtl0, u16 mtl1);
	SGameMtlPairEditor* GetMaterialPair(LPCSTR name);
	virtual GameMtlPairIt	FirstMaterialPair() { return material_pairs.begin(); }
	virtual GameMtlPairIt	LastMaterialPair() { return material_pairs.end(); }

	// IO routines
	virtual void				Load();
	virtual bool				Save();
};

#define GET_RANDOM(a_vector)			(a_vector[Random.randI(a_vector.size())])

#define CLONE_MTL_SOUND(_res_, _mtl_pair_, _a_vector_)\
	{ VERIFY2(!_mtl_pair_##->_a_vector_.empty(),_mtl_pair_->dbg_Name());\
	_res_.clone(GET_RANDOM(_mtl_pair_##->_a_vector_),st_Effect,sg_SourceType);\
	}

extern ECORE_API XrGameMaterialLibraryEditors* GameMaterialLibraryEditors;