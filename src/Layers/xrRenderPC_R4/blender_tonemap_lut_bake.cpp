#include "stdafx.h"
#include "blender_tonemap_lut_bake.h"

CBlender_tonemap_lut_bake::CBlender_tonemap_lut_bake()
{
    description.CLS = 0;
}

CBlender_tonemap_lut_bake::~CBlender_tonemap_lut_bake()
{
}

void CBlender_tonemap_lut_bake::Compile(CBlender_Compile& C)
{
    IBlender::Compile(C);

    switch (C.iElement)
    {
    case 0:
        C.r_ComputePass("tonemap_lut_bake");
        C.r_dx10Texture("s_adapt", r2_RT_lumD);

        C.r_End();
        break;
    }
}