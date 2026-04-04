#include "stdafx.h"


#include "EditObject.h"
#include "EditMesh.h"
#include "ui_main.h"
#include "../Public/PropertiesListHelper.h"
#include "../xrEngine/motion.h"
#include "../xrEngine/bone.h"

void CEditableObject::OnChangeSharedMode(PropValue* sender)
{
	for (CSurface* i : m_Surfaces)
	{
		if(i->UseShared != i->UseSharedPrev)
		{
			if(i->UseSharedPrev)
			{
				auto TempShared = i->m_pData.second;
				i->m_pData.second = new SSurfaceData(*TempShared);
			} else
			{
				auto Shared = CSharedMaterialLibrary::Instance().GetData(i->m_pData.first);
				if(!Shared)
				{
					CSharedMaterialLibrary::Instance().MakeSharedCopy(i->m_pData.second);
					Shared = CSharedMaterialLibrary::Instance().GetData(i->m_pData.first);
				}
				i->m_pDataOld = i->m_pData.second;
				i->m_pData.second = Shared;
			}
			i->UseSharedPrev = i->UseShared;
		}	
	}
	OnChangeShader(sender);
	//Tools->UpdateProperties();
}

void CEditableObject::OnChangeSharedMaterial(PropValue* sender)
{
	for (CSurface* i : m_Surfaces)
	{
		if(i->m_pData.first != i->m_pData.second->m_Name)
		{
			i->m_pData.second = CSharedMaterialLibrary::Instance().GetData(i->m_pData.first);
		}
	}
	OnChangeShader(sender);
	//Tools->UpdateProperties();
}

void CEditableObject::OnBatchProcessMaterial(ButtonValue* value, bool& bModif, bool& bSafe)
{
	switch(value->btn_num)
	{
	case 0: // unique
		{
			for (auto elem : m_Surfaces)
			{
				elem->UseShared = false;
			}
			break;
		}
	case 1: // shared
		{
			for (auto elem : m_Surfaces)
			{
				elem->UseShared = true;
			}
			break;
		}
	}
	bModif = true;
	OnChangeSharedMode(value);
}

void CEditableObject::OnChangeShader(PropValue*)
{
    OnDeviceDestroy	();
    UI->RedrawScene	();
}

void CEditableObject::FillSurfaceProps(CSurface* SURF, const char* pref, PropItemVec& items)
{
	
	auto B = PHelper().CreateBool(items, PrepareKey(pref, "Use shared material"), &SURF->UseShared);
	B->OnChangeEvent.bind(this, &CEditableObject::OnChangeSharedMode);

	if(SURF->UseShared)
	{
		auto SMC = PHelper().CreateChoose(items, PrepareKey(pref, "Shared Material Name"), &SURF->m_pData.first, smSharedMaterial);
		SMC->OnChangeEvent.bind(this, &CEditableObject::OnChangeSharedMaterial);
	}
	
	MultiChooseValue* MultiValue = PHelper().CreateChooseTexture(items, PrepareKey(pref, "TextureView"));
	PropValue* V = nullptr;
	
	if(SURF->UseShared)
	{
		V = MultiValue->CreateValue(PrepareKey(pref, "Texture"), &SURF->m_pData.second->m_Texture, smDisabled);
		V= MultiValue->CreateValue(PrepareKey(pref, "Shader"), &SURF->m_pData.second->m_ShaderName, smDisabled);
		V = MultiValue->CreateValue(PrepareKey(pref, "Compile"), &SURF->m_pData.second->m_ShaderXRLCName, smDisabled);
		V = MultiValue->CreateValue(PrepareKey(pref, "Game Mtl"), &SURF->m_pData.second->m_GameMtlName, smDisabled);
		PHelper().CreateCaption(items, PrepareKey(pref, "2 Sided"), SURF->m_pData.second->m_Flags.test(SSurfaceData::sf2Sided) ? "true" : "false");
	} else
	{
		V = MultiValue->CreateValue(PrepareKey(pref, "Texture"), &SURF->m_pData.second->m_Texture, smTexture);
		V->OnChangeEvent.bind(this, &CEditableObject::OnChangeShader);

		V = MultiValue->CreateValue(PrepareKey(pref, "Shader"), &SURF->m_pData.second->m_ShaderName, smEShader);
		V->OnChangeEvent.bind(this, &CEditableObject::OnChangeShader);

		V = MultiValue->CreateValue(PrepareKey(pref, "Compile"), &SURF->m_pData.second->m_ShaderXRLCName, smCShader);
		V = MultiValue->CreateValue(PrepareKey(pref, "Game Mtl"), &SURF->m_pData.second->m_GameMtlName, smGameMaterial);

		V = PHelper().CreateFlag32(items, PrepareKey(pref, "2 Sided"), &SURF->m_pData.second->m_Flags, SSurfaceData::sf2Sided);
		V->OnChangeEvent.bind(this, &CEditableObject::OnChangeShader);
	}
		
    PHelper().CreateCaption(items, PrepareKey(pref, "Face Count"), shared_str().printf("%d", GetSurfFaceCount(SURF->_Name())));
}

xr_token ECORE_API eo_type_token[]={
	{ "Static",					0},
	{ "Dynamic",				CEditableObject::eoDynamic},
	{ "Progressive Dynamic",	CEditableObject::eoDynamic|CEditableObject::eoProgressive},
	{ "Disable Opt Dynamic",	CEditableObject::eoDynamic|CEditableObject::eoSkipOpt},
	{ "HOM",					CEditableObject::eoHOM},
	{ "Multiple Usage",			CEditableObject::eoMultipleUsage|CEditableObject::eoUsingLOD},
	{ "Sound Occluder",			CEditableObject::eoSoundOccluder},
	{ nullptr,						0}
};

void CEditableObject::FillBasicProps(const char* pref, PropItemVec& items)
{
    xr_string ct(_ctime32(&m_CreateTime));
    _Trim(ct);
    xr_string mt(_ctime32(&m_ModifTime));
    _Trim(mt);
	PropValue* V=nullptr;
	PHelper().CreateCaption		(items, PrepareKey(pref,"Reference Name"),		m_LibName.c_str());
    PHelper().CreateToken32		(items, PrepareKey(pref,"Object Type"),   		&m_objectFlags.flags,		eo_type_token);
	PHelper().CreateCaption		(items, PrepareKey(pref,"Version\\Owner Name"),	m_CreateName.c_str());
	PHelper().CreateCaption		(items, PrepareKey(pref,"Version\\Modif Name"),	m_ModifName.c_str());
	PHelper().CreateCaption		(items, PrepareKey(pref,"Version\\Creation Time"), ct.c_str());
	PHelper().CreateCaption		(items, PrepareKey(pref,"Version\\Modified Time"), mt.c_str());
    V=PHelper().CreateVector   	(items, PrepareKey(pref,"Transform\\Position"),	&t_vPosition,	-100000,	100000,0.01,2); 		V->OnChangeEvent.bind(this,&CEditableObject::OnChangeTransform);
    V=PHelper().CreateAngle3   	(items, PrepareKey(pref,"Transform\\Rotation"),	&t_vRotate, 	-10000,	10000,0.1,1);		V->OnChangeEvent.bind(this,&CEditableObject::OnChangeTransform);
    V=PHelper().CreateVector   	(items, PrepareKey(pref,"Transform\\Scale"),	&t_vScale, 		0.01,	10000,0.01,2);			V->OnChangeEvent.bind(this,&CEditableObject::OnChangeTransform);
    V=PHelper().CreateCaption  	(items, PrepareKey(pref,"Transform\\BBox Min"),	shared_str().printf("{%3.2f, %3.2f, %3.2f}",VPUSH(GetBox().min)));
    V=PHelper().CreateCaption  	(items, PrepareKey(pref,"Transform\\BBox Max"),	shared_str().printf("{%3.2f, %3.2f, %3.2f}",VPUSH(GetBox().max)));
//.    PHelper().CreateChoose	    (items, PrepareKey(pref,"LOD\\Reference"),	&m_LODs, smObject);
    PHelper().CreateChoose	    (items, PrepareKey(pref,"LOD\\Reference"),		&m_LODs, smVisual);

    FillSummaryProps			(pref,items);
}
//---------------------------------------------------------------------------

void CEditableObject::FillSummaryProps(const char* pref, PropItemVec& items)
{
    string128 t; 
    sprintf(t, "V: %d, F: %d", GetVertexCount(), GetFaceCount());

    PHelper().CreateCaption(items,PrepareKey(pref,"Geometry\\Object"),t);

    for (EditMeshIt m_it = FirstMesh(); m_it != LastMesh(); m_it++)
    {
        string128 t1;
        CEditableMesh* MESH = *m_it;
        sprintf(t1, "V: %d, F: %d", MESH->GetVertexCount(), MESH->GetFaceCount());
        PHelper().CreateCaption(items, PrepareKey(pref, xr_string(xr_string("Geometry\\Meshes\\") + MESH->Name().c_str()).c_str()), t1);
    }

    PHelper().CreateSText(items,PrepareKey(pref, "Game options\\User Data"),&m_ClassScript);
}
//---------------------------------------------------------------------------

ECORE_API xr_string MakeFullBoneName(CBone* bone)
{
	if (bone->Parent()){
    	return MakeFullBoneName(bone->Parent())+"\\"+bone->Name().c_str();
    }else{
    	return bone->Name().c_str();
    }
}

xr_string MakeFullBonePath(CBone* bone)
{
	if (bone->Parent()){
	   	return MakeFullBoneName(bone->Parent());
	}else{
    	return "";
	}
}

void CEditableObject::FillSurfaceList(const char* pref, ListItemsVec& items, int modeID)
{
    SurfaceVec& s_lst 	= Surfaces();
	if (pref) LHelper().CreateItem(items, pref, modeID, ListItem::flSorted);
    for (SurfaceIt s_it=s_lst.begin(); s_it!=s_lst.end(); s_it++)
        LHelper().CreateItem(items, PrepareKey(pref, (*s_it)->_Name()).c_str(), modeID, 0, *s_it);
}
//---------------------------------------------------------------------------

void CEditableObject::FillBoneList(const char* pref, ListItemsVec& items, int modeID)
{
    BoneVec& b_lst 		= Bones();
    if (pref) LHelper().CreateItem(items, pref, modeID, ListItem::flSorted);
    for(BoneIt b_it=b_lst.begin(); b_it!=b_lst.end(); b_it++){
    	xr_string pt	= MakeFullBonePath(*b_it);
    	xr_string path	= (!pt.size())?pref:PrepareKey(pref, pt.c_str()).c_str();
		LHelper().CreateItem(items, PrepareKey(path.c_str(), (*b_it)->Name().c_str()).c_str(), modeID, 0, *b_it);
    }
}

void CEditableObject::FillMotionList(const char* pref, ListItemsVec& items, int modeID)
{
    SMotionVec&	m_lst	= SMotions();
	if (pref) LHelper().CreateItem(items, pref,  modeID, ListItem::flSorted);
    for (SMotionIt m_it=m_lst.begin(); m_it!=m_lst.end(); m_it++)
        LHelper().CreateItem(items, PrepareKey(pref, (*m_it)->Name()).c_str(), modeID, 0, *m_it);
}