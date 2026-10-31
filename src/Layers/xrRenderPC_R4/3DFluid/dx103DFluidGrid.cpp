#include "stdafx.h"
#include "dx103DFluidGrid.h"

void dx103DFluidGrid::Initialize(int gridDepth)
{
	m_iNumVerticesSlices = 6 * (gridDepth - 2);
}

void dx103DFluidGrid::DrawSlices(GfxCmdList& Cmd)
{
	Cmd.DrawNoInput(m_iNumVerticesSlices);
}
