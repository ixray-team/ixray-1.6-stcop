#include "stdafx.h"
//#include "build.h"
#include "calculate_normals.h"
#include "xrMU_Model.h"

void calc_normals( xrMU_Model &model )
{
	calculate_normals::calc_normals( model.m_vertices, model.m_faces, true );
}
