#pragma once
#include "../../Public/VIMP_Processor.h"
#include "EditMesh.h"
#include "Export/MeshVertPack.h"

//---------------------------------------------------------------------------
const int clpSMX = 28, clpSMY=16, clpSMZ=28;
//---------------------------------------------------------------------------
// refs                                           
class CEditableObject;
class CSurface;
class CInifile;
extern ECORE_API bool g_force16BitTransformQuant;
extern ECORE_API float g_EpsSkelPositionDelta;

struct ECORE_API SSkelVert: public st_SVert{
    Fvector		tang;
    Fvector		binorm;
	SSkelVert(){
        uv.set	(0.f,0.f);
        offs.set	(0,0,0);
		norm.set	(0,1,0);
        tang.set	(1,0,0);
		binorm.set	(0,0,1);
	}
    void set(const Fvector& _o, const Fvector& _n, const Fvector2& _uv, u8 _w_cnt, const st_SVert::bone* b)
    {
        offs.set 	(_o);
        norm.set	(_n);
        uv.set		(_uv);
        VERIFY		(_w_cnt>0 && _w_cnt<=4);
        bones.resize(_w_cnt);

        for (u8 k=0; k<_w_cnt; k++)
        {
        	bones[k]=b[k];
        }
        sort_by_bone(); // need to similar 
    }

    bool	similar_pos(SSkelVert& V)
    {
        return offs.similar(V.offs, g_EpsSkelPositionDelta);
    }
    bool	similar(SSkelVert& V)
    {
        if (bones.size() != V.bones.size())	return false;
        for (u8 k = 0; k < (u8)bones.size(); k++)
        {
            if (!bones[k].similar(V.bones[k]))
                return false;
        }
        if (!uv.similar(V.uv, EPS_S))
            return false;

        if (!offs.similar(V.offs, g_EpsSkelPositionDelta))
            return false;

        if (!norm.similar(V.norm, g_EpsSkelPositionDelta))
            return false;

        return true;
    }
};

struct ECORE_API SSkelFace{
	WORD		v[3];
};

using SkelVertVec = xr_vector<SSkelVert>;
using SkelFaceVec = xr_vector<SSkelFace>;


class ECORE_API CSkeletonCollectorPacked
{
public:
    SkelVertVec		m_Verts;
    SkelFaceVec		m_Faces;
    
    MeshVertPackGrid<clpSMX, clpSMY, clpSMZ> m_VertGrid;

	u16 VPack(SSkelVert& V)
	{
		return m_VertGrid.Pack(m_Verts, V, [](SSkelVert& Vertex) -> Fvector& { return Vertex.offs; }, false);
	}
    u32 			invalid_faces;
public:
    CSkeletonCollectorPacked	(const Fbox &bb, int apx_vertices=5000, int apx_faces=5000);
    bool 			check      	(SSkelFace& F){
		if ((F.v[0]==F.v[1]) || (F.v[0]==F.v[2]) || (F.v[1]==F.v[2])) return false;
        for (SSkelFace const& Face : m_Faces)
        {
            if ((Face.v[0]==F.v[0]) && (Face.v[1]==F.v[1]) && (Face.v[2]==F.v[2])) return false;
            if ((Face.v[0]==F.v[0]) && (Face.v[2]==F.v[1]) && (Face.v[1]==F.v[2])) return false;
            if ((Face.v[2]==F.v[0]) && (Face.v[0]==F.v[1]) && (Face.v[1]==F.v[2])) return false;
            if ((Face.v[2]==F.v[0]) && (Face.v[1]==F.v[1]) && (Face.v[0]==F.v[2])) return false;
            if ((Face.v[1]==F.v[0]) && (Face.v[0]==F.v[1]) && (Face.v[2]==F.v[2])) return false;
            if ((Face.v[1]==F.v[0]) && (Face.v[2]==F.v[1]) && (Face.v[0]==F.v[2])) return false;
        }
        return true;
    }
	bool add_face	(SSkelVert& v0, SSkelVert& v1, SSkelVert& v2)
    {
		if (v0.offs.similar(v1.offs,EPS) || v0.offs.similar(v2.offs,EPS) || v1.offs.similar(v2.offs,EPS)){
			ELog.Msg(mtError,"Degenerate face found. Removed.");
            invalid_faces++;
            return false;
        }
        SSkelFace F;
        F.v[0]	= VPack(v0);
        F.v[1]	= VPack(v1);
        F.v[2]	= VPack(v2);
        if (check(F)){ 
        	m_Faces.push_back	(F);
	        return 				true;
        }else{	
        	ELog.Msg(mtError,"Duplicate face found. Removed.");
            invalid_faces++;
            return false;
        }
    }
};
//----------------------------------------------------

class ECORE_API CExportSkeletonCustom
{
protected:
    struct ECORE_API SSplit: 
        public CSkeletonCollectorPacked
    {
    	shared_str		m_Shader;
        shared_str		m_Texture;
        u16 			m_PartID;
        Fbox			m_Box;
        U16Vec			m_UsedBones;
        u16             m_id;

        // Progressive
		ArbitraryList<VIPM_SWR>	m_SWR;// The records of the collapses.
	    u32				m_SkeletonLinkType;
    public:
        SSplit (CSurface* surf, const Fbox& bb, u16 part);

        bool valid()
        {
        	if (m_Verts.empty()) return false;
        	if (m_Faces.empty()) return false;
            return true;
        }
		void 			MakeProgressive				();
        void			MakeStripify				();
		void 			CalculateTB					();
		void 			OptimizeTextureCoordinates	();

        void 			Save			(IWriter& F);

        void			ComputeBounding	()
        {
            m_Box.invalidate();
            for (SSkelVert& Vert : m_Verts)
                m_Box.modify(Vert.offs);
        }
    };
    using SplitVec = xr_vector<SSplit>;
	SplitVec			m_Splits;
    Fbox 				m_Box;
//----------------------------------------------------    
    int  FindSplit(shared_str shader, shared_str texture, u16 part_id, u16 surf_id);
    void ComputeBounding()
    {
        m_Box.invalidate();
        for (SSplit& Split : m_Splits)
        {
            Split.ComputeBounding();
            m_Box.merge(Split.m_Box);
        }
    }
public:
    virtual ~CExportSkeletonCustom() = default;

    virtual bool    	Export				(IWriter& F, u8 infl)=0;
};


class ECORE_API CExportSkeleton: public CExportSkeletonCustom{
	CEditableObject*	m_Source;
    bool				PrepareGeometry		(u8 influence);
public:
						CExportSkeleton		(CEditableObject* object);
    virtual bool    	Export				(IWriter& F, u8 infl);
    virtual bool    	ExportGeometry		(IWriter& F, u8 infl);
    virtual bool    	ExportMotions		(IWriter& F);

    virtual bool    	ExportMotionKeys	(IWriter& F);
    virtual bool    	ExportMotionDefs	(IWriter& F);
    bool                ExportAsSimple		(IWriter& F);
};

void ECORE_API 			ComputeOBB_RAPID	(Fobb &B, FvectorVec& V, u32 t_cnt);
void ECORE_API 			ComputeOBB_WML		(Fobb &B, FvectorVec& V);