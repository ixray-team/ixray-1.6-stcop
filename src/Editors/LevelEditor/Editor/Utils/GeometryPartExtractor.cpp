#include "stdafx.h"
#include "GeometryPartExtractor.h"

void SBPart::append_face			(SBFace* F)
{
    m_Faces.push_back				(F);
}

void SBPart::use_face				(SBFace* F, u32& cnt, u32 bone_id, float& area)
{
    VERIFY							(F->bone_id==-1);
    F->marked						= true;
    F->bone_id						= bone_id;
    area							+= F->CalcArea();
    cnt++;
}

void SBPart::recurse_fragment		(SBFace* F, u32& cnt, u32 bone_id, u32 max_faces, float& area)
{
    if (F){
        if (!F->marked)	use_face	(F,cnt,bone_id,area);
        // fill nearest
        SBFaceVec r_vec;
        for (auto& adj : F->adjs)
        {
            if (cnt>=max_faces)		break;
            if (adj->marked)	continue;
            use_face				(adj,cnt,bone_id,area);
            r_vec.push_back			(adj);
        }     
        // recurse adjs   	
        for (auto& a_it : r_vec)
        {
            if (cnt>=max_faces)					break;
            if (a_it->bone_id!=(int)bone_id)	continue;
            recurse_fragment					(a_it,cnt,bone_id,max_faces,area);
        } 
    }
}

bool SBPart::prepare				(SBAdjVec& adjs, u32 bone_face_min)
{
    m_bValid	= true;
    // compute OBB
    FvectorVec pts; pts.reserve		(m_Faces.size()*3);
    for (auto& m_Face : m_Faces)
    {
	    m_Face->marked				= false;
        for (int k=0; k<3; k++)		pts.push_back(m_Face->o[k]);
    }
    ComputeOBB_RAPID				(m_OBB,pts,pts.size()/3);
    // fill adjacent
    for (auto A : m_Faces)
    {
	    for (int k : A->vert_id)
        {
            SBFaceVec& b_vec		= adjs[k];
            for (auto B : b_vec)
            {
	            if (A!=B){
                    int cnt			= 0;
                    for (int a : A->vert_id)
	                    for (int b=0; b<3; b++) if (a ==B->vert_id[b]) cnt++;
                    if (cnt>=2){
                        if (std::ranges::find(A->adjs,B)==A->adjs.end()) A->adjs.push_back(B);
                        if (std::ranges::find(B->adjs,A)==B->adjs.end()) B->adjs.push_back(A);
                    }
                }
            }        
        }
    }
    // prepare transform matrix
    m_BBox.invalidate				();
    Fmatrix M; M.set				(m_OBB.m_rotate.i,m_OBB.m_rotate.j,m_OBB.m_rotate.k,m_OBB.m_translate);
    m_RefOffset.set					(m_OBB.m_translate);
    M.getXYZ						(m_RefRotate); // не i потому что в движке так
    M.invert						();

    // transform vertices & calculate bounding box
    for (auto F : m_Faces)
    {
	    if (F->adjs.empty()){	
            ELog.Msg(mtError,"Error face found at pos: [%3.2f,%3.2f,%3.2f]",VPUSH(F->o[0])); 
            Tools->m_DebugDraw.AppendWireFace(F->o[0],F->o[1],F->o[2]);
            m_bValid				= false;
        }
        for (int k=0; k<3; k++){ 
            M.transform_dir			(F->n[k]);
            M.transform_tiny		(F->o[k]);
            m_BBox.modify			(F->o[k]);
        }
    }   
    if (m_bValid){
        // calculate bone params
        int bone_cnt_calc				= iFloor(float(m_Faces.size())/bone_face_min);
        int bone_cnt_max				= (bone_cnt_calc<62)?(bone_cnt_calc<=0?1:bone_cnt_calc):62;
        int bone_face_max				= iFloor(float(m_Faces.size())/bone_cnt_max+0.5f); bone_face_max *= 4.f;
        int bone_idx					= 0;
        // create big fragment
        u32 face_accum_total			= 0;
        xr_string parent_bone			= "";
        do{
            SBFace* F					= 0;
            // find unused face
            for (auto& m_Face : m_Faces)
            {
                if (!m_Face->marked){
                    F					= m_Face;
                    int cnt 			= 0;
                    for (auto& adj : F->adjs)
                    {
	                    cnt+= adj->marked?0:1;
                    }
                    if ((cnt==0)||(cnt>=2))	break;
                }
            }
            if (!F)						break;
            float area					= 0;
            u32 face_accum				= 0;
            u32 face_max_count 			= Random.randI(bone_face_min,bone_face_max+1);
            // fill faces
            recurse_fragment			(F,face_accum,bone_idx,face_max_count,area);
            if (face_accum==1){
//            	F->marked				= false;
                F->bone_id				= -1;
            }
        	else
            {
                string1024 Name;
				xr_sprintf(Name,"%d",bone_idx);

                m_Bones.emplace_back(Name,parent_bone,F->surf->m_GameMtlName.c_str(),face_accum,area);
                parent_bone				= "0";
                bone_idx				++;
                face_accum_total		+= face_accum;
            }
            // create bone
        }while(bone_idx<bone_cnt_max);
        
        // attach small single face to big fragment
        u32 face_accum_total_saved 		= face_accum_total;
        while (face_accum_total<m_Faces.size()){
        	if (face_accum_total_saved!=face_accum_total){
				face_accum_total_saved 	= face_accum_total;
            }else{
            	// если проход оказалс€ безрезультатным (т.е. не смогли добавить хоть один фейс) - добавл€ем 
                //  все неприаттаченные фейсы к 0-кости (иначе зацикливаетс€ (mike - L03_agroprom))
                for (auto F : m_Faces)
                {
	                if (-1==F->bone_id){
                        F->marked		= true;
                        F->bone_id		= 0;
                        face_accum_total++;
                    }
                } 
            }
            for (auto F : m_Faces)
            {
	            if (-1==F->bone_id){
                    SBFace* P			= 0;
                    if (F->adjs.empty()){
                        F->marked		= true;
                        F->bone_id		= 0;
                        face_accum_total++;
                    }else{
                        for (auto& adj : F->adjs)
                        { 
                            if (-1!= adj->bone_id){	
	                            P		= adj;
                            	break;
                            }
                        }
                    }
                    if (P){
                    	VERIFY(-1!=P->bone_id);
                        F->marked		= true;
                        F->bone_id		= P->bone_id;
                        face_accum_total++;
                    }
                }
            } 
        }
        
        // calculate bone offset
        for (auto F : m_Faces)
        {
	        VERIFY						(F->bone_id!=-1);
            SBBone& B					= m_Bones[F->bone_id];
            for (auto k : F->o)
            {
	            B.offset.add(k);
            }
        }
        for (auto& B : m_Bones)
        {
	        VERIFY						(0!=B.f_cnt);
            B.offset.div				(B.f_cnt*3);
        }
        Fvector& offs					= m_Bones.front().offset;
        for (auto& m_Bone : m_Bones)
        {
	        m_Bone.offset.sub			(offs);
        }
    }
    return m_bValid;
}
bool SBPart::Export	(IWriter& F, u8 infl)
{
	VERIFY			(!m_Bones.empty());
    if (m_Bones.size()>63){
    	ELog.Msg(mtError,"Breakable object cannot handle more than 63 parts.");
     	return false;
    }

    bool bRes = true;

    u32 bone_count			= m_Bones.size();
                    
    xr_vector<FvectorVec>	bone_points;
	bone_points.resize		(bone_count);

    u32 mtl_cnt				= 0;
                                  
    for (auto face : m_Faces)
    {
	    int mtl_idx = FindSplit(face->surf->m_ShaderName.c_str(), face->surf->m_Texture.c_str(), 0, face->surf->m_id);

        if (mtl_idx<0)
        {
            m_Splits.emplace_back(face->surf,m_BBox,0);
            mtl_idx	= mtl_cnt++;
        }
        SSplit& split=m_Splits[mtl_idx];
        SSkelVert v[3];
        for (int k=0; k<3; k++){
            st_SVert::bone b[1];
            b[0].id		= face->bone_id;
            b[0].w		= 1.f;
            v[k].set	(face->o[k],face->n[k],face->uv[k],1,b);
        }
        split.add_face		(v[0], v[1], v[2]);

        if (face->surf->_flags().is(SSurfaceData::sf2Sided)){
            v[0].norm.invert(); v[1].norm.invert(); v[2].norm.invert();
            if (!split.add_face(v[0], v[2], v[1]))
            {
	            split.invalid_faces++;
            }
        }
    }

    // fill per bone vertices
    for (SSplit& Split : m_Splits)
    {
        if (!Split.valid()){
            ELog.Msg(mtError,"Degenerate part found (Texture '%s').",*Split.m_Texture);
            bRes = false;
            break;
        }
        if (0!=Split.invalid_faces){
	        ELog.Msg(mtError,"Part [texture '%s'] have %d duplicate(degenerate) face(s).",*Split.m_Texture,Split.invalid_faces);
        }
		Split.CalculateTB();
	    for (SSkelVert& Vert : Split.m_Verts){
		    bone_points[Vert.bones[0].id].push_back(Vert.offs);
            bone_points[Vert.bones[0].id].back().sub(m_Bones[Vert.bones[0].id].offset);
        }
    }

    if (!bRes) return false;

    // compute bounding
    ComputeBounding	();

	// create OGF
    // Header
    ogf_header 		H;
    H.format_version= xrOGF_FormatVersion;
    H.type			= MT_SKELETON_RIGID;
    H.shader_id		= 0;
    H.bb.min		= m_Box.min;
    H.bb.max		= m_Box.max;
    m_Box.getsphere	(H.bs.c,H.bs.r);
    F.w_chunk		(OGF_HEADER,&H,sizeof(H));

    // Desc
    ogf_desc		desc;
    F.open_chunk	(OGF_S_DESC);
    desc.Save		(F);
    F.close_chunk	();
	
    // OGF_CHILDREN
    F.open_chunk	(OGF_CHILDREN);
    int children=0;
    for (auto& m_Split : m_Splits)
    {
	    F.open_chunk(children++);
	    m_Split.Save(F);
	    F.close_chunk();
    }
    F.close_chunk();

    // BoneNames                   
    F.open_chunk(OGF_S_BONE_NAMES); 
    F.w_u32		(bone_count);
    // write other bones
    for (u32 bone_idx=0; bone_idx<bone_count; bone_idx++){
    	SBBone& bone=m_Bones[bone_idx];
        F.w_stringZ	(bone.name.c_str());
        F.w_stringZ	(bone.parent.c_str());
        Fobb		obb;
        ComputeOBB_WML	(obb,bone_points[bone_idx]);
        F.w				(&obb,sizeof(Fobb));
    }
    F.close_chunk();

    F.open_chunk(OGF_S_IKDATA);
    for (u32 bone_idx=0; bone_idx<bone_count; bone_idx++){
    	SBBone& bone=m_Bones[bone_idx];

        F.w_u32		(0x0001); VERIFY(0x0001==OGF_IKDATA_VERSION);
        // material
        F.w_stringZ (bone.mtl.c_str());
        // shape
        SBoneShape	shape;
        shape.type	= SBoneShape::stBox;
        shape.flags.assign(SBoneShape::sfRemoveAfterBreak);
        ComputeOBB_WML	(shape.box,bone_points[bone_idx]);
	    F.w				(&shape,sizeof(SBoneShape));
        // IK data
        SJointIKData 	ik_data;
        ik_data.Reset	();
        ik_data.type	= jtNone;
        ik_data.ik_flags.set(SJointIKData::flBreakable,true);
        ik_data.Export	(F);

        Fvector rot={0,0,0};
        F.w_fvector3(rot);
        F.w_fvector3(bone.offset);
        F.w_float   (bone.area);	// mass (дл€  ости посчитал площадь)
        F.w_fvector3(shape.box.m_translate);	// center of mass        
    }
    F.close_chunk();

    return bRes;
}


IC void recurse_tri(SBPart* P, SBFaceVec& faces, SBAdjVec& adjs, SBFace* F)
{
	if (F->marked) 		return;

	P->append_face		(F);
    F->marked			= true;

    // recurse
    for (int k : F->vert_id)
    {
	    SBFaceVec& PL 	= adjs[k];
        for (auto& pl_it : PL)
        {
	        recurse_tri	(P,faces,adjs, pl_it);
        }
    }
}

void CGeomPartExtractor::AppendFace(CSurface* surf, const Fvector* v, const Fvector* n, const Fvector2* uvs[3])
{
	SBFace* F			= new SBFace(surf,uvs);
    // insert verts
    for (int k=0; k<3; k++){
        F->vert_id[k] 	= m_Verts->AddVert(v[k]);
	    F->o[k].set		(v[k]);
	    F->n[k].set		(n[k]);
    }
    m_Faces.push_back	(F);
}
CGeomPartExtractor::CGeomPartExtractor	()
{
	m_Verts				= nullptr;
}
void CGeomPartExtractor::Initialize	(const Fbox& bb, float eps, u32 per_bone_face_count_min)
{
	VERIFY				(0==m_Verts);
	m_Verts 			= new VCPacked(bb,eps);
    m_PerBoneFaceCountMin=per_bone_face_count_min; VERIFY(m_PerBoneFaceCountMin>0);
}
void CGeomPartExtractor::Clear		()
{
    xr_delete		(m_Verts);
    for (auto& m_Face : m_Faces)
    {
	    xr_delete	(m_Face);
    }
    m_Faces.clear	();
    for (auto& m_Part : m_Parts)
    {
	    xr_delete	(m_Part);
    }
    m_Parts.clear	();
    m_Adjs.clear	();
}
bool CGeomPartExtractor::Process()
{
    // make adjacement
    {
        m_Adjs.resize	(m_Verts->Vertices().size());
        for (auto& m_Face : m_Faces)
        {
	        for (int k=0; k<3; k++) m_Adjs[m_Face->vert_id[k]].push_back(m_Face);
        }
    }
    // extract parts
    {
		SPBItem* pb = EContext.UI->ProgressStart(m_Faces.size(), "Extract Parts...");
        for (auto& m_Face : m_Faces)
        {
	        pb->Inc();
            SBFace* F	= m_Face;
            if (!F->marked){
                SBPart* P 		= new SBPart();
                recurse_tri		(P,m_Faces,m_Adjs, m_Face);
                m_Parts.push_back	(P);
            }
        }
		EContext.UI->ProgressEnd(pb);
    }
    // simplify parts
    {
		SPBItem* pb = EContext.UI->ProgressStart(m_Parts.size(), "Simplify Parts...");
        for (auto& m_Part : m_Parts)
        {	
	        pb->Inc();
	        m_Part->prepare	(m_Adjs,m_PerBoneFaceCountMin);
        }
		EContext.UI->ProgressEnd(pb);
    }
    return true;
}

