#ifndef	dx103DFluidGrid_included
#define	dx103DFluidGrid_included
#pragma once

class dx103DFluidGrid
{
public:
	void	Initialize(int gridDepth);

	void	DrawSlices(GfxCmdList& Cmd);

private:
	u32		m_iNumVerticesSlices;
};

#endif	//	dx103DFluidGrid_included
