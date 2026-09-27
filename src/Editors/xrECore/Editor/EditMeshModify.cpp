//----------------------------------------------------
// file: EditMeshModify.cpp
//----------------------------------------------------

#include "stdafx.h"


#include "EditMesh.h"
#include "EditObject.h"

//----------------------------------------------------
void CEditableMesh::Transform(const Fmatrix& parent)
{
	// transform position
	for(u32 k=0; k<m_Vertices.size(); ++k)
		parent.transform_tiny(m_Vertices[k]);

    // RecomputeBBox
	RecomputeBBox	();
    // update normals & cform
#if 1
	UnloadRenderBuffers	();
	UnloadCForm		();
#endif
    UnloadFNormals	(true);
    UnloadVNormals	(true);
    UnloadSVertices	(true);
}
//----------------------------------------------------

int CEditableMesh::FindSimilarUV(st_VMap* vmap, Fvector2& _uv)
{
	int sz			= vmap->size();
	for (int k=0; k<sz; ++k)
	{
		const Fvector2& uv = vmap->getUV(k);
		if (uv.similar(_uv)) 
			return k;
	}
	return -1;
}

int CEditableMesh::FindSimilarWeight(st_VMap* vmap, float _w)
{
	int sz			= vmap->size();
	for (int k=0; k<sz; ++k)
	{
		float w		= vmap->getW(k);
		if (fsimilar(w,_w)) return k;
	}
	return -1;
}

void CEditableMesh::RebuildVMaps()
{
	IntVec m_VertVMap;
	m_VertVMap.resize(m_Vertices.size(), -1);
	VMapVec nVMaps;
	VMRefsVec NewVMRefs = m_VMRefs;

	for (u32 f_id = 0; f_id < m_Faces.size(); f_id++)
	{
		st_Face& F = m_Faces[f_id];
		for (int k = 0; k < 3; k++)
		{
			u32 pts_cnt = m_VMRefs[F.pv[k].vmref].size();
			for (u32 pt_id = 0; pt_id < pts_cnt; pt_id++)
			{
				st_VMapPt* n_pt_it = &NewVMRefs[F.pv[k].vmref][pt_id];
				st_VMapPt* o_pt_it = &m_VMRefs[F.pv[k].vmref][pt_id];
				st_VMap* vmap = m_VMaps[o_pt_it->vmap_index].get();
				switch (vmap->type)
				{
					case vmtUV:
					{
						int& pm = m_VertVMap[F.pv[k].pindex];
						if (-1 == pm)
						{ // point map
							pm = F.pv[k].vmref;
							int vm_idx = FindVMapByName(nVMaps, vmap->name.c_str(), vmap->type, false);
							if (-1 == vm_idx)
							{
								nVMaps.push_back(xr_make_unique<st_VMap>(vmap->name.c_str(), static_cast<u8>(vmap->type), false));
								vm_idx = nVMaps.size() - 1;
							}
							st_VMap* nVMap = nVMaps[vm_idx].get();

							nVMap->appendUV(vmap->getUV(o_pt_it->index));
							nVMap->appendVI(F.pv[k].pindex);
							n_pt_it->index = nVMap->size() - 1;
							n_pt_it->vmap_index = vm_idx;
						}
						else
						{ // poly map
							int vm_idx = FindVMapByName(nVMaps, vmap->name.c_str(), vmap->type, true);
							if (-1 == vm_idx)
							{
								nVMaps.push_back(xr_make_unique<st_VMap>(vmap->name.c_str(), static_cast<u8>(vmap->type), true));
								vm_idx = nVMaps.size() - 1;
							}
							st_VMap* nVMapPM = nVMaps[vm_idx].get();

							nVMapPM->appendUV(vmap->getUV(o_pt_it->index));
							nVMapPM->appendVI(F.pv[k].pindex);
							nVMapPM->appendPI(f_id);
							n_pt_it->index = nVMapPM->size() - 1;
							n_pt_it->vmap_index = vm_idx;
						}
					}
					break;
					case vmtWeight:
					{
						int vm_idx = FindVMapByName(nVMaps, vmap->name.c_str(), vmap->type, false);
						if (-1 == vm_idx)
						{
							nVMaps.push_back(xr_make_unique<st_VMap>(vmap->name.c_str(), static_cast<u8>(vmap->type), false));
							vm_idx = nVMaps.size() - 1;
						}
						st_VMap* nWMap = nVMaps[vm_idx].get();
						nWMap->appendW(vmap->getW(o_pt_it->index));
						nWMap->appendVI(F.pv[k].pindex);
						n_pt_it->index = nWMap->size() - 1;
						n_pt_it->vmap_index = vm_idx;
					}
					break;
				}
			}
		}
	}

	m_VMaps.clear();
	m_VMRefs.clear();

	m_VMaps = std::move(nVMaps);
	m_VMRefs = std::move(NewVMRefs);
}

#define MX 25
#define MY 15
#define MZ 25
static Fvector		VMmin, VMscale;
static U32Vec		VM[MX+1][MY+1][MZ+1];
static Fvector		VMeps;

static FvectorVec	m_NewPoints;
bool CEditableMesh::OptimizeFace(st_Face& face)
{
	Fvector points[3];
	int mface[3];
	int k;

	for (k = 0; k < 3; k++)
	{
		points[k].set(m_Vertices[face.pv[k].pindex]);
		mface[k] = -1;
	}

	// get similar vert idx list
	for (k = 0; k < 3; k++)
	{
		U32Vec* vl;
		int ix, iy, iz;
		ix = iFloor(float(points[k].x - VMmin.x) / VMscale.x * MX);
		iy = iFloor(float(points[k].y - VMmin.y) / VMscale.y * MY);
		iz = iFloor(float(points[k].z - VMmin.z) / VMscale.z * MZ);
		vl = &(VM[ix][iy][iz]);
		for (U32It it = vl->begin(); it != vl->end(); it++)
		{
			FvectorIt v = m_NewPoints.begin() + (*it);
			if (v->similar(points[k], EPS))
			{
				mface[k] = *it;
			}
		}
	}
	for (k = 0; k < 3; k++)
	{
		if (mface[k] == -1)
		{
			mface[k] = m_NewPoints.size();
			m_NewPoints.push_back(points[k]);
			int ix, iy, iz;
			ix = iFloor(float(points[k].x - VMmin.x) / VMscale.x * MX);
			iy = iFloor(float(points[k].y - VMmin.y) / VMscale.y * MY);
			iz = iFloor(float(points[k].z - VMmin.z) / VMscale.z * MZ);
			VM[ix][iy][iz].push_back(mface[k]);
			int ixE, iyE, izE;
			ixE = iFloor(float(points[k].x + VMeps.x - VMmin.x) / VMscale.x * MX);
			iyE = iFloor(float(points[k].y + VMeps.y - VMmin.y) / VMscale.y * MY);
			izE = iFloor(float(points[k].z + VMeps.z - VMmin.z) / VMscale.z * MZ);
			if (ixE != ix)
			{
				VM[ixE][iy][iz].push_back(mface[k]);
			}
			if (iyE != iy)
			{
				VM[ix][iyE][iz].push_back(mface[k]);
			}
			if (izE != iz)
			{
				VM[ix][iy][izE].push_back(mface[k]);
			}
			if ((ixE != ix) && (iyE != iy))
			{
				VM[ixE][iyE][iz].push_back(mface[k]);
			}
			if ((ixE != ix) && (izE != iz))
			{
				VM[ixE][iy][izE].push_back(mface[k]);
			}
			if ((iyE != iy) && (izE != iz))
			{
				VM[ix][iyE][izE].push_back(mface[k]);
			}
			if ((ixE != ix) && (iyE != iy) && (izE != iz))
			{
				VM[ixE][iyE][izE].push_back(mface[k]);
			}
		}
	}

	if ((mface[0] == mface[1]) || (mface[1] == mface[2]) || (mface[0] == mface[2]))
	{
		Msg("! Optimize: Invalid face found in %s at [%.3f, %.3f, %.3f]. Removed", *m_Name, mface[0], mface[1], mface[2]);
		return false;
	}
	else
	{
		face.pv[0].pindex = mface[0];
		face.pv[1].pindex = mface[1];
		face.pv[2].pindex = mface[2];
		return true;
	}
}

void CEditableMesh::OptimizeMesh(bool NoOpt)
{
	if (!NoOpt){
#if 1
    	UnloadRenderBuffers	();
		UnloadCForm     	();
#endif
        UnloadFNormals   	(true);
        UnloadVNormals   	(true);
       	UnloadSVertices  	(true);
       	UnloadAdjacency		(true);
    	
		// clear static data
		for (int x=0; x<MX+1; x++)
			for (int y=0; y<MY+1; y++)
    			for (int z=0; z<MZ+1; z++)
            		VM[x][y][z].clear();
		VMscale.set(m_Box.max.x-m_Box.min.x+EPS_S, m_Box.max.y-m_Box.min.y+EPS_S, m_Box.max.z-m_Box.min.z+EPS_S);
		VMmin.set(m_Box.min.x, m_Box.min.y, m_Box.min.z);

		VMeps.set(VMscale.x/MX/2,VMscale.y/MY/2,VMscale.z/MZ/2);
		VMeps.x = (VMeps.x<EPS_L)?VMeps.x:EPS_L;
		VMeps.y = (VMeps.y<EPS_L)?VMeps.y:EPS_L;
		VMeps.z = (VMeps.z<EPS_L)?VMeps.z:EPS_L;

		m_NewPoints.clear();
		m_NewPoints.reserve(m_Vertices.size());
                                                
		boolVec 	faces_mark;
		faces_mark.resize(m_Faces.size(),false);
        int			i_del_face 		= 0;
		for (u32 k=0; k<m_Faces.size(); k++){
    		if (!OptimizeFace(m_Faces[k])){
				faces_mark[k]		= true;
                i_del_face			++;
            }
		}

        m_Vertices = m_NewPoints;

		if (i_del_face){
	        xr_vector<st_Face> old_faces;
	        xr_vector<u32> old_sg;
			old_faces.swap(m_Faces);
			old_sg.swap(m_SmoothGroups);

            m_Faces.resize(old_faces.size()-i_del_face);
            m_SmoothGroups.resize(m_Faces.size());
            
            u32 new_dk	= 0;
            for (u32 dk=0; dk<old_faces.size(); ++dk)
			{
            	if (faces_mark[dk])
				{
                    for (SurfFacesPairIt plp_it=m_SurfFaces.begin(); plp_it!=m_SurfFaces.end(); ++plp_it)
					{
                        IntVec& 	pol_lst = plp_it->second;
                        for (int k=0; k<int(pol_lst.size()); ++k)
						{
                            int& f = pol_lst[k];
                            if (f>(int)dk)
							{ 
								--f;
                            }else if (f==(int)dk)
							{
                                pol_lst.erase(pol_lst.begin()+k);
                                --k;
                            }
                        }
                    }
                	continue;
                } 

            	m_Faces[new_dk]				= old_faces[dk];
            	m_SmoothGroups[new_dk]		= old_sg[dk];
				++new_dk;
            }
		}
	}
}
