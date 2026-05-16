#include "stdafx.h"
#include "xrMU_Model.h"
#include "../Shader_xrLC.h"

bool	cmp_face_material		(_face* f1, _face* f2)
{
	if (f1->dwMaterial != f2->dwMaterial)
	    return f1->dwMaterial < f2->dwMaterial;

    //Seakad: Grouping of polygons to cut more evenly
    Fvector ca, cb;
    ca.add(f1->v[0]->P, f1->v[1]->P); 
    ca.add(f1->v[2]->P);
    ca.div(3.f);

    cb.add(f2->v[0]->P, f2->v[1]->P); 
    cb.add(f2->v[2]->P);
    cb.div(3.f);

    if (ca.x != cb.x)
        return ca.x < cb.x;

    return ca.z < cb.z;
}
//static xrMU_Model::v_faces temp_vector;
void xrMU_Model::calc_materials	()
{
 	xrMU_Model::v_faces &temp_vector			= m_faces;

	std::sort			(temp_vector.begin(),temp_vector.end(),cmp_face_material);

	static const u32 MAX_SUBDIV_VERTS = 30000;
    //Seakad: Overall, without Stripify and MakeProgressive i would bet 40k, otherwise 30k as a reserve.
    m_subdivs.clear();

	if (temp_vector.empty())
        return;

	_subdiv				current;
	current.material	= temp_vector[0]->dwMaterial;
	current.start		= 0;
	current.count		= 1;

	xr_set<_vertex*> unique_verts;

    auto face_new_vert_count = [&](const _face* F) -> u32
    {
        u32 count = (u32)unique_verts.size();
        if (unique_verts.find(F->v[0]) == unique_verts.end())
            ++count;

        if (unique_verts.find(F->v[1]) == unique_verts.end())
            ++count;

        if (unique_verts.find(F->v[2]) == unique_verts.end())
            ++count;

        return count;
    };

    auto add_face_verts = [&](const _face* F)
    {
        unique_verts.insert(F->v[0]);
        unique_verts.insert(F->v[1]);
        unique_verts.insert(F->v[2]);
    };

    add_face_verts(temp_vector[0]);
    current.count = 1;

	for (u32 it=1; it<temp_vector.size(); it++)
	{
		_face* F = temp_vector[it];
		if ((current.material != F->dwMaterial) || (face_new_vert_count(F) > MAX_SUBDIV_VERTS))
		{
			// end of strip 
			m_subdivs.push_back	(current);
			current.material	= F->dwMaterial;
			current.start		= it;
			current.count		= 0;

			unique_verts.clear();
		} 
        add_face_verts(F);
        current.count++;
	}
	m_subdivs.push_back	(current);

	// remove non-visible materials
	for (s32 it=0; it<s32(m_subdivs.size()); it++)
	{
		_face*		first	= temp_vector[m_subdivs[it].start];
		if (first->Shader().flags.bRendering)	continue;

		m_subdivs.erase	(m_subdivs.begin()+it);
		it--;
	}

//	clMsg	("model '%s' - %d subdivisions",*m_name,m_subdivs.size());
}
