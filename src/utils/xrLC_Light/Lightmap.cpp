// Lightmap.cpp: implementation of the CLightmap class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "Lightmap.h"
#include "xrDeflector.h"
#include "xrFace.h"
#include <xrLC_GlobalData.h>
#include <../xrLC/Build.h>
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLightmap::CLightmap()
{
}

CLightmap::~CLightmap()
{
}
 
void CLightmap::Capture		(CDeflector *D, int b_u, int b_v, int s_u, int s_v, bool bRotated)
{
	// Allocate texture if needed
	if (lm.surface.empty())
		lm.create(gCompilerMode.LC_sizeLmaps, gCompilerMode.LC_sizeLmaps);
	
	// Addressing
	xr_vector<UVtri>	tris;
	D->RemapUV			(tris, b_u+BORDER, b_v+BORDER, s_u-2*BORDER, s_v-2*BORDER, gCompilerMode.LC_sizeLmaps, gCompilerMode.LC_sizeLmaps, bRotated);
	
	// Capture faces and setup their coords
	for (UVIt T=tris.begin(); T!=tris.end(); T++)
	{
		UVtri&	P			= *T;
		Face	*F			= P.owner;
		F->lmap_layer		= this;
		F->AddChannel		(P.uv[0], P.uv[1], P.uv[2]);
	}
	tris.clear(); tris.shrink_to_fit();
	
	// Perform BLIT
	lm_layer&	L		=	D->layer;
	u32 real_H			=   (L.height + 2 * BORDER);
	u32 real_W			=   (L.width + 2 * BORDER);

	if (!bRotated) 
		blit	(lm, gCompilerMode.LC_sizeLmaps, gCompilerMode.LC_sizeLmaps,L,real_W,real_H,b_u,b_v, 254-BORDER);
	else 
		blit_r	(lm, gCompilerMode.LC_sizeLmaps, gCompilerMode.LC_sizeLmaps,L,real_W,real_H,b_u,b_v, 254-BORDER);
}

//////////////////////////////////////////////////////////////////////
IC u32 convert(float a)
{
	if (a<=0)		return 0;
	else if (a>=1)	return 255;
	else			return iFloor(a*255.f);
}
IC void pixel	(int x, int y,  b_texture* T, u32 C=color_rgba(0,255,0,0))
{
	if (x<0) return; else if (x>=(int)T->dwWidth)	return;
	if (y<0) return; else if (y>=(int)T->dwHeight)	return;

	u32* raw = static_cast<u32*>(*T->pSurface);
	raw[y * T->dwWidth + x] = C;
}

IC void line	( int x1, int y1, int x2, int y2, b_texture* T )
{
    int dx = std::abs(x2 - x1);
    int dy = std::abs(y2 - y1);
    int sx = x2 >= x1 ? 1 : -1;
    int sy = y2 >= y1 ? 1 : -1;

    if ( dy <= dx ){
        int d  = ( dy << 1 ) - dx;
        int d1 = dy << 1;
        int d2 = ( dy - dx ) << 1;

		pixel(x1,y1,T);

        for  (int x = x1 + sx, y = y1, i = 1; i <= dx; i++, x += sx){
            if ( d > 0){
                d += d2; y += sy;
            }else
                d += d1;
			pixel(x,y,T);
        }
    }else{
        int d  = ( dx << 1 ) - dy;
        int d1 = dx << 1;
        int d2 = ( dx - dy ) << 1;

		pixel(x1,y1,T);
        for  (int x = x1, y = y1 + sy, i = 1; i <= dy; i++, y += sy ){
            if ( d > 0){
                d += d2; x += sx;
            }else
                d += d1;
			pixel(x,y,T);
        }
    }
}

size_t GetMemory();

void CLightmap::Save(const char* path)
{
	size_t StartMemory = GetMemory();

	static int		lmapNameID = 0;
	++lmapNameID;

	CTimer t;
	t.Start();

	// Borders correction
	for (u32 _y = 0; _y < gCompilerMode.LC_sizeLmaps; _y++)
	{
		for (u32 _x = 0; _x < gCompilerMode.LC_sizeLmaps; _x++)
		{
			u32	offset = _y * gCompilerMode.LC_sizeLmaps + _x;
			if (lm.marker[offset] >= (254 - BORDER))
				lm.marker[offset] = 255;
			else
				lm.marker[offset] = 0;
		}
	}
  	u32 correct = t.GetElapsed_ms();  t.Start();

	for (u32 ref = 254; ref > (254 - 16); ref--)
	{
		ApplyBorders(lm, ref);
		Progress(1.f - float(ref) / float(254 - 16));
	}
 	u32 ApplyBorders = t.GetElapsed_ms();

	Progress(1.f);

	xr_vector<u32>			lm_packed;
	lm.Pack(lm_packed);

	xr_vector<u32>			hemi_packed;
	lm.Pack_hemi(hemi_packed);

	lm_texture.bHasAlpha = TRUE;
	lm_texture.dwWidth = lm.width;
	lm_texture.dwHeight = lm.height;
	lm_texture.pSurface.Clear();
 	lm.clear_memory();


	clMsg("$ [Lightmap] Saving DDS ...");
 	t.Start();
	if (true)
	{
		// Status("Compression base...");

		string_path				FN;
		xr_sprintf(lm_texture.name, "lmap#%d", lmapNameID);
		xr_sprintf(FN, "%s%s_1.dds", path, lm_texture.name);
		
		BYTE* raw_data = LPBYTE(&*lm_packed.begin());
		u32	w = lm_texture.dwWidth;//lm.width;
		u32	h = lm_texture.dwHeight;//lm.height;
		u32	pitch = w * 4;

		STextureParams fmt;
		switch (gCompilerMode.LmapsFormat)
		{
			case LCLightmapFormat::FORMAT_RGBA: fmt.fmt = STextureParams::tfRGBA; break;
			case LCLightmapFormat::FORMAT_BC7:  fmt.fmt = STextureParams::tfBC7; break;
			case LCLightmapFormat::FORMAT_BC5:  fmt.fmt = STextureParams::tfDXT5; break;
		}

		fmt.flags.set(STextureParams::flDitherColor, FALSE);
		fmt.flags.set(STextureParams::flGenerateMipMaps, FALSE);
		fmt.flags.set(STextureParams::flBinaryAlpha, FALSE);

 		DXTUtils::Compress(FN, raw_data, 0, w, h, pitch, &fmt, 4);
 	}

	u32 saving_base = t.GetElapsed_ms(); t.Start();

	if (true)
	{
		// Status("Compression hemi...");

 		string_path				FN;
		xr_sprintf(lm_texture.name, "lmap#%d", lmapNameID);
		xr_sprintf(FN, "%s%s_2.dds", path, lm_texture.name);

		u32 w = lm_texture.dwWidth;		//lm.width;
		u32 h = lm_texture.dwHeight;	//lm.height;
		u32	pitch = w * 4;

		u8* raw_data = LPBYTE(&*hemi_packed.begin());

		STextureParams fmt;
		switch (gCompilerMode.LmapsFormat)
		{
		case LCLightmapFormat::FORMAT_RGBA: fmt.fmt = STextureParams::tfRGBA; break;
		case LCLightmapFormat::FORMAT_BC7:  fmt.fmt = STextureParams::tfBC7; break;
		case LCLightmapFormat::FORMAT_BC5:  fmt.fmt = STextureParams::tfDXT5; break;
		}

		fmt.flags.set(STextureParams::flDitherColor, FALSE);
		fmt.flags.set(STextureParams::flGenerateMipMaps, FALSE);
		fmt.flags.set(STextureParams::flBinaryAlpha, FALSE);

 		DXTUtils::Compress(FN, raw_data, 0, w, h, pitch, &fmt, 4);
 	}

	lm_packed.clear();
	hemi_packed.clear();

	lm_packed.shrink_to_fit();
	hemi_packed.shrink_to_fit();

 	s32 UsedMemory = StartMemory > GetMemory() ? - s32( ( StartMemory - GetMemory() ) / 1024 / 1024) : ( ( GetMemory() - StartMemory) / 1024 / 1024 );


	// Чтобы лучше выглядело в логе
	clMsg("* [Lightmap] Corection Borders: %u ms, Apply Borders: %u ms", correct, ApplyBorders);
	clMsg("* [Lightmap] Save Base: %u ms, Hemi: %u ms, Memory: %d mb",   saving_base, t.GetElapsed_ms(), u32(UsedMemory) );
}