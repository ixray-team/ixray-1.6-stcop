//---------------------------------------------------------------------------

#ifndef du_boxH
#define du_boxH
#include "RenderAPI.h"
//---------------------------------------------------------------------------
#define DU_BOX_NUMVERTEX	8
#define DU_BOX_NUMFACES		12
#define DU_BOX_NUMLINES		12
#define DU_BOX_NUMVERTEX2	36

extern RENDER_API Fvector du_box_vertices[];
extern RENDER_API WORD du_box_faces[];
extern RENDER_API WORD du_box_lines[];

extern RENDER_API Fvector du_box_vertices2[];
#endif
