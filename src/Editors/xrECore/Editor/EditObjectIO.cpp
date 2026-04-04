//----------------------------------------------------
// file: CEditableObject.cpp
//----------------------------------------------------

#include "stdafx.h"


#include "EditObject.h"
#include "EditMesh.h"
#include "../xrEngine/bone.h"
#include "../xrEngine/motion.h"

#include "ExportSkeleton.h"
#include "ExportObjectOGF.h"

#if 1
 #include "../Layers/xrRender/Shader.h"

bool CEditableObject::Load(const char* fname)
{
	if (FS.exist(fname))
	{
        time_t age		= FS.get_file_age	(fname);		VERIFY3(age>0,"Invalid file age:",fname);
        IReader* F 		= FS.r_open			(fname); 		R_ASSERT(F);
        IReader* OBJ 	= F->open_chunk(EEditableObjectChunks::OBJECT_BODY);
        R_ASSERT2		(OBJ,"Corrupted file.");
        bool bRes 		= Load(*OBJ);
        OBJ->close();
        FS.r_close(F);
        if (bRes)
		{ 
            m_LoadName 		= fname;
            m_ObjectVersion = age; 
        }
        return bRes;
    }
    return false;
}
#endif

bool CEditableObject::Save(const char* fname)
{
	if (IsModified())
	{
        // update transform matrix
        Fmatrix	mTransform, mScale, mTranslate, mRotate;
        if (	!fsimilar(t_vRotate.magnitude(),0)||
				!fsimilar(t_vScale.magnitude(),1.73205f)||
				!fsimilar(t_vPosition.magnitude(),0))
        {
            mRotate.setHPB			(t_vRotate.y, t_vRotate.x, t_vRotate.z);
            mScale.scale			(t_vScale);
            mTranslate.translate	(t_vPosition);
            mTransform.mul			(mTranslate,mRotate);
            mTransform.mulB_43		(mScale);
	        TranslateToWorld		(mTransform);
            t_vRotate.set			(0,0,0);
            t_vPosition.set			(0,0,0);
            t_vScale.set			(1,1,1);
        }
    }

    // save object
    IWriter* F			= FS.w_open(fname);
	if (F)
	{
        F->open_chunk	(EEditableObjectChunks::OBJECT_BODY);
        Save			(*F);
        F->close_chunk	();

        FS.w_close		(F);

        m_LoadName 		= fname;
        m_ObjectVersion = FS.get_file_age(fname); 
        return			true;
    }else{
    	return 			false;
    }
}

void CEditableObject::Save(IWriter& F)
{
	F.open_chunk	(EEditableObjectChunks::VERSION);
	F.w_u16			((u16)EEditableObjectVersions::Vanilla);
	F.close_chunk	();

	F.open_chunk	(EEditableObjectChunks::CLASSSCRIPT);
	F.w_stringZ		(m_ClassScript);
	F.close_chunk	();

	F.open_chunk	(EEditableObjectChunks::LODS);
	F.w_stringZ		(m_LODs);
	F.close_chunk	();

    F.w_chunk		(EEditableObjectChunks::FLAGS,&m_objectFlags.flags,sizeof(m_objectFlags.flags));

    // meshes
    F.open_chunk	(EEditableObjectChunks::EDITMESHES);
    int count		= 0;

    for(EditMeshIt m = m_Meshes.begin(); m!=m_Meshes.end(); ++m)
	{
        F.open_chunk(count); 
		++count;
        (*m)->SaveMesh(F);
        F.close_chunk();
    }
    F.close_chunk	();

	// surfaces
    F.open_chunk(EEditableObjectChunks::SURFACES_SHARED);
    F.w_u32(m_Surfaces.size());

    for (auto& m_Surface : m_Surfaces)
    {
    	bool IsShared = m_Surface->IsSharedMode();
    	F.w_u8(IsShared);
    	if(IsShared)
    	{
    		F.w_stringZ(m_Surface->_Name());
    	}
    	else
    	{
    		F.w_stringZ(m_Surface->_Name());
    		F.w_stringZ(m_Surface->_ShaderName());
    		F.w_stringZ(m_Surface->_ShaderXRLCName());
    		F.w_stringZ(m_Surface->_GameMtlName());
    		F.w_stringZ(m_Surface->_Texture());
    	}
		F.w_stringZ(m_Surface->_VMap());
        F.w_u32(m_Surface->m_pData.second->m_Flags.get());
        F.w_u32(m_Surface->_FVF());
        F.w_u32(1);
    }
    F.close_chunk();

    // bones
    if (!m_Bones.empty()){
	    F.open_chunk	(EEditableObjectChunks::BONES2);
	    for (BoneIt b_it=m_Bones.begin(); b_it!=m_Bones.end(); ++b_it)
		{
        	F.open_chunk	(b_it-m_Bones.begin());
        	(*b_it)->Save	(F);
	    	F.close_chunk	();
        }
    	F.close_chunk		();
    }

    // skeleton motions
    if (!m_SMotions.empty())
    {
        F.open_chunk	(EEditableObjectChunks::SMOTIONS);
        F.w_u32			(m_SMotions.size());
        
		for (SMotionIt s_it=m_SMotions.begin(); s_it!=m_SMotions.end(); ++s_it) 
			(*s_it)->Save(F);

		F.close_chunk	();
    }
    // skeleton motions refs
    if (m_SMotionRefs.size())
    {
        F.open_chunk	(EEditableObjectChunks::SMOTIONS3);
        F.w_u32			(m_SMotionRefs.size());

        for(u32 i=0; i<m_SMotionRefs.size(); ++i)
        	F.w_stringZ	(m_SMotionRefs[i].c_str());

        F.close_chunk	();
    }

    // bone parts
    if (!m_BoneParts.empty())
	{
        F.open_chunk	(EEditableObjectChunks::BONEPARTS2);
        F.w_u32			(m_BoneParts.size());
        for (BPIt bp_it=m_BoneParts.begin(); bp_it!=m_BoneParts.end(); ++bp_it)
		{
            F.w_stringZ	(bp_it->alias);
            F.w_u32		(bp_it->bones.size());

            for (RStringVecIt s_it=bp_it->bones.begin(); s_it!=bp_it->bones.end(); ++s_it)
	            F.w_stringZ(s_it->c_str());

        }
        F.close_chunk	();
    }

    if (IsDynamic())
	{
		F.open_chunk	(EEditableObjectChunks::ACTORTRANSFORM);
        F.w_fvector3	(a_vPosition);
        F.w_fvector3	(a_vRotate);
		F.close_chunk	();
    }

    F.open_chunk		(EEditableObjectChunks::DESC);
    F.w_stringZ			(m_CreateName.c_str());
    F.w					(&m_CreateTime,sizeof(m_CreateTime));
    F.w_stringZ			(m_ModifName.c_str());
    F.w					(&m_ModifTime,sizeof(m_ModifTime));
    F.close_chunk		();

    // set modif desc
	SetVersionToCurrent	(false, true);

	bOnModified			= false;
}

bool CEditableObject::Load(IReader& F)
{
	bool bRes = true;

	u32 version = 0;
    shared_str buf;
	shared_str sh_name;
	R_ASSERT(F.r_chunk(EEditableObjectChunks::VERSION,&version));
	if (version!=(u32)EEditableObjectVersions::Vanilla)
	{
		ELog.DlgMsg( mtError, "CEditableObject: unsupported file version. Object can't load.");
		bRes = false;
		return bRes;
	}

	R_ASSERT(F.r_chunk(EEditableObjectChunks::FLAGS, &m_objectFlags.flags));

	if (F.find_chunk	(EEditableObjectChunks::CLASSSCRIPT))
	{
		F.r_stringZ		(m_ClassScript);
	}

	if (F.find_chunk	(EEditableObjectChunks::LODS))
	{
		F.r_stringZ		(m_LODs);
	}

	// surfaces
	if (F.find_chunk(EEditableObjectChunks::SURFACES_SHARED))
	{
		u32 cnt = F.r_u32();
		m_Surfaces.resize(cnt);
		for (auto& m_Surface : m_Surfaces)
		{
			m_Surface = new CSurface();
			bool IsShared = F.r_u8();
			if (IsShared)
			{
				F.r_stringZ(buf);
				auto Data = CSharedMaterialLibrary::Instance().GetData(buf);
				if(!IVERIFY(Data))
				{
					Msg("Object [%s]: unable to find shared surface data [%s], set default...", m_LibName.c_str(), buf.c_str());
				}
				m_Surface->SetSharedData(Data);
			} else
			{
				m_Surface->SetSharedData(nullptr);
				F.r_stringZ	(buf);
				m_Surface->SetName(buf.c_str());
				F.r_stringZ	(buf);
				m_Surface->SetShader(buf.c_str());
				F.r_stringZ	(buf);
				m_Surface->SetShaderXRLC(buf.c_str());
				F.r_stringZ	(buf);
				m_Surface->SetGameMtl(buf.c_str());
				F.r_stringZ	(buf);
				m_Surface->SetTexture(buf.c_str());
			}
			F.r_stringZ(buf);
			m_Surface->SetVMap(buf.c_str());
			m_Surface->m_pData.second->m_Flags.assign(F.r_u32());
			m_Surface->SetFVF(F.r_u32());
			cnt = F.r_u32();
			if (!I_ASSERT(cnt==1))
			{
				ELog.DlgMsg(mtError,"Object surface '%s' has more than one TC's.",buf.c_str());
			}
		}
	}
	else if (F.find_chunk(EEditableObjectChunks::SURFACES3))
	{
		u32 cnt = F.r_u32();
		m_Surfaces.resize(cnt);
		for (auto& m_Surface : m_Surfaces)
		{
			m_Surface = new CSurface();
			m_Surface->SetSharedData(nullptr);
			F.r_stringZ	(buf);
			m_Surface->SetName(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetShader(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetShaderXRLC(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetGameMtl(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetTexture(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetVMap(buf.c_str());
			m_Surface->m_pData.second->m_Flags.assign(F.r_u32());
			m_Surface->SetFVF(F.r_u32());
			cnt = F.r_u32();
			if (!I_ASSERT(cnt == 1))
			{
				ELog.DlgMsg(mtError,"Object surface '%s' has more than one TC's.",buf.c_str());
			}
		}
	}else if (F.find_chunk(EEditableObjectChunks::SURFACES2)){
		u32 cnt = F.r_u32();
		m_Surfaces.resize(cnt);
		for (auto& m_Surface : m_Surfaces)
		{
			m_Surface = new CSurface();
			m_Surface->SetSharedData(nullptr);
			F.r_stringZ	(buf);
			m_Surface->SetName(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetShader(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetShaderXRLC(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetTexture(buf.c_str());
			F.r_stringZ	(buf);
			m_Surface->SetVMap(buf.c_str());
			m_Surface->m_pData.second->m_Flags.assign(F.r_u32());
			m_Surface->SetFVF(F.r_u32());
			cnt = F.r_u32();
			if (!I_ASSERT(cnt == 1))
			{
				ELog.DlgMsg(mtError,"Object surface '%s' has more than one TC's.",buf.c_str());
			}
		}
	}else{
		R_ASSERT(F.find_chunk(EEditableObjectChunks::SURFACES));
		u32 cnt = F.r_u32();
		m_Surfaces.resize(cnt);
		for (auto& m_Surface : m_Surfaces)
		{
			m_Surface = new CSurface();
			m_Surface->SetSharedData(nullptr);
			F.r_stringZ(buf);
			m_Surface->SetName(buf.c_str());
			F.r_stringZ(sh_name);
			m_Surface->m_pData.second->m_Flags.set(SSurfaceData::sf2Sided,!!F.r_u8());
			m_Surface->SetFVF(F.r_u32());
			cnt = F.r_u32();
			if (!I_ASSERT(cnt==1))
			{
				ELog.DlgMsg(mtError,"Object surface '%s' has more than one TC's.",buf.c_str());
			}
			F.r_stringZ(buf);
			m_Surface->SetTexture(buf.c_str());
			F.r_stringZ(buf);
			m_Surface->SetVMap(buf.c_str());
			m_Surface->SetShader(sh_name.c_str());
			m_Surface->SetShaderXRLC("default");
		}

		// surfaces xrlc part
		if(F.find_chunk(EEditableObjectChunks::SURFACES_XRLC))
		{
			for (auto& m_Surface : m_Surfaces)
			{
				F.r_stringZ(buf);
				m_Surface->SetShaderXRLC(buf.c_str());
			}
		}
	}

	// Load meshes
	IReader* OBJ = F.open_chunk(EEditableObjectChunks::EDITMESHES);
	if(OBJ)
	{
		IReader* M   = OBJ->open_chunk(0);
		for (int count=1; M; count++) 
		{
			CEditableMesh* mesh=new CEditableMesh(this);
			if (mesh->LoadMesh(*M))
				m_Meshes.push_back(mesh);
			else{
				ELog.DlgMsg( mtError, "CEditableObject: Can't load mesh '%s'!", *mesh->m_Name );
				xr_delete(mesh);
				bRes = false;
			}
			M->close();
			if (!bRes)	break;
			M = OBJ->open_chunk(count);
		}
		OBJ->close();
	}

	// bones
    if (bRes){
        IReader* B_CHUNK = F.open_chunk(EEditableObjectChunks::BONES2);
        if (B_CHUNK)
		{
            int chunk = 0;
            IReader* O;
            while (nullptr!=(O=B_CHUNK->open_chunk(chunk++)))
			{
                m_Bones.push_back(new CBone());
                m_Bones.back()->Load_1(*O);
                O->close();
            }
            B_CHUNK->close();
            PrepareBones();
        }else if (F.find_chunk(EEditableObjectChunks::BONES)){
            m_Bones.resize(F.r_u32());
            for (BoneIt b_it=m_Bones.begin(); b_it!=m_Bones.end(); b_it++){
                *b_it = new CBone();
                (*b_it)->Load_0(F);
            }
            PrepareBones();
        }

  		// skeleton motions
        if (F.find_chunk(EEditableObjectChunks::SMOTIONS))
		{
            m_SMotions.resize(F.r_u32());
            for (SMotionIt s_it=m_SMotions.begin(); s_it!=m_SMotions.end(); s_it++)
			{
                *s_it = new CSMotion();
                if (!(*s_it)->Load(F))
				{
                    Log		("!Motions has different version. Load failed.");
                    xr_delete(*s_it);
                    m_SMotions.clear();
                    break;                 
                }
                // resort bone_mots
	            (*s_it)->SortBonesBySkeleton(m_Bones);
            }
        }
        if (F.find_chunk(EEditableObjectChunks::SMOTIONS2))
        {
        	shared_str 		tmp;
            F.r_stringZ		(tmp);
            u32 set_cnt		= _GetItemCount(tmp.c_str());

            string_path		nm;
            for (u32 k=0; k<set_cnt; ++k)
            {
                _GetItem			(tmp.c_str(),k,nm);
            	m_SMotionRefs.push_back	(nm);
        	}
        }else
        if (F.find_chunk(EEditableObjectChunks::SMOTIONS3))
        {
            u32 set_cnt		= F.r_u32();

        	shared_str 		nm;
            for (u32 k=0; k<set_cnt; ++k)
            {
            	F.r_stringZ				(nm);
            	m_SMotionRefs.push_back	(nm);
        	}
        }
    }

	// bone parts
    if (bRes){
        if (F.find_chunk(EEditableObjectChunks::BONEPARTS)){
            m_BoneParts.resize(F.r_u32());
            bool bBPok = true;
            for (BPIt bp_it=m_BoneParts.begin(); bp_it!=m_BoneParts.end(); bp_it++){
                F.r_stringZ	(buf); bp_it->alias=buf;
                bp_it->bones.resize(F.r_u32());
                for (RStringVecIt s_it=bp_it->bones.begin(); s_it!=bp_it->bones.end(); s_it++){
                    int idx		= F.r_u32();
                    if ((idx>=0)&&(idx<(int)m_Bones.size())){
                        *s_it	= m_Bones[idx]->Name();
                    }else{
	                    Log		("!Invalid bone parts.",GetName());
                        bBPok = false;
                        break;
                    }
                }
                if (!bBPok) break;
            }
			if (!bBPok)	m_BoneParts.clear();
            if (!m_BoneParts.empty()&&!VerifyBoneParts())
                Log		("!Invalid bone parts. Found duplicate bones in object '%s'.",GetName());
        }else if (F.find_chunk(EEditableObjectChunks::BONEPARTS2)){
            m_BoneParts.resize(F.r_u32());
            for (BPIt bp_it=m_BoneParts.begin(); bp_it!=m_BoneParts.end(); bp_it++){
                F.r_stringZ	(buf); bp_it->alias=buf;
                bp_it->bones.resize(F.r_u32());
                for (RStringVecIt s_it=bp_it->bones.begin(); s_it!=bp_it->bones.end(); s_it++)
                    F.r_stringZ(*s_it);
            }
            if (!m_BoneParts.empty()&&!VerifyBoneParts())
                Log			("!Invalid bone parts. Found duplicate bones in object '%s'.",GetName());
        }
    }

    if (bRes)
    {
        if (F.find_chunk	(EEditableObjectChunks::ACTORTRANSFORM))
        {
            F.r_fvector3	(a_vPosition);
            F.r_fvector3	(a_vRotate);
        }

        if (F.find_chunk	(EEditableObjectChunks::DESC))
        {
            F.r_stringZ		(m_CreateName);
            F.r				(&m_CreateTime,sizeof(m_CreateTime));
            F.r_stringZ		(m_ModifName);
            F.r				(&m_ModifTime,sizeof(m_ModifTime));
        }
	
        ResetSAnimation();
    }

	if (!bRes)
	{
		return bRes;
	}
	UpdateBox		();
	VerifyMeshNames	();

	return bRes;
}

bool CEditableObject::ExportOGF(const char* fn, u8 infl)
{
	UpdateBox		();
	CMemoryWriter	F;

    if (PrepareOGF(F,infl,true, nullptr))
	{
    	return F.save_to(fn);
    }
    return false;
}
//------------------------------------------------------------------------------
bool CEditableObject::ExportOMF(const char* fn)
{
	UpdateBox		();
	CMemoryWriter	F;
    if (PrepareOMF(F))
	{
    	return F.save_to(fn);
    }
    return false;
}
//------------------------------------------------------------------------------
bool CEditableObject::ExportOBJ(const char* fn)
{
	UpdateBox			();
    CExportObjectOGF E(this);
	CMemoryWriter F;
    if (E.ExportAsWavefrontOBJ(F,fn))
	{
    	return F.save_to(fn);
    }
    return false;
}
//------------------------------------------------------------------------------


