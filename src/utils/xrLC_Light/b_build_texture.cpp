#include "stdafx.h"
#include "b_build_texture.h"
 
/*
struct b_texture
{
	string128			name;
	u32					dwWidth;
	u32					dwHeight;
	BOOL				bHasAlpha;
	u32*				pSurface;
};
STextureParams	THM;
*/

void clear( b_BuildTexture &texture )
{
	xr_free(texture.pSurface);
}
 