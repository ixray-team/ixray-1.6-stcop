#include "stdafx.h"
#include "xrLight_ImplicitDeflector.h"
#include "b_build_texture.h"
#include "xrFace.h"
#include "xrLC_GlobalData.h"
#include "../xrDXT/xrDXT.h"

u32	ImplicitDeflector::Width	()						
{
	return texture->dwWidth; 
}
u32	ImplicitDeflector::Height	()						
{
	return texture->dwHeight; 
}
	
u32& ImplicitDeflector::Texel(u32 x, u32 y)
{
	u32* raw = static_cast<u32*>(*texture->pSurface);
	return raw[y * Width() + x];
}

void	ImplicitDeflector::Bounds	(u32 ID, Fbox2& dest)
{
	Face* F		= faces[ID];
	_TCF& TC	= F->tc[0];
	dest.min.set	(TC.uv[0]);
	dest.max.set	(TC.uv[0]);
	dest.modify		(TC.uv[1]);
	dest.modify		(TC.uv[2]);
}

void	ImplicitDeflector::Bounds_Summary (Fbox2& bounds)
{
	bounds.invalidate();
	for (u32 I=0; I<faces.size(); I++)
	{
		Fbox2	B;
		Bounds	(I,B);
		bounds.merge(B);
	}
}
void ImplicitDeflector::SaveTextures()
{
	// base (HEMI)
	{
		Status("Processing lightmap...");
		lmap.ApplyBordersFast(0);

		Status("Mixing lighting with texture...");
		{
			b_BuildTexture& TEX = *texture;
			VERIFY(!TEX.pSurface.Empty());

			u32* color = (u32*)*TEX.pSurface;
			for (u32 V = 0; V < Height(); V++)
			{
				for (u32 U = 0; U < Width(); U++)
				{
					// Retreive Texel
					float	hemi = Lumel(U, V).h._r();
					u32& C = color[V * Width() + U];
					C = subst_alpha(C, u8_clr(hemi));
				}
			}

		}

		Status("Saving base...");
		string128 name;
		string_path out_name;
		xr_strcpy(name, gCompilerMode.get_lname());

		R_ASSERT(name[0] && texture);

		b_BuildTexture& TEX = *texture;
		xr_strconcat(out_name, name, "\\", TEX.name, ".dds");
		FS.update_path(out_name, "$game_levels$", out_name);
		clMsg("Saving texture '%s'...", out_name);
		VerifyPath(out_name);

		STextureParams fmt = TEX.THM;
		switch (gCompilerMode.LmapsFormat)
		{
			case LCLightmapFormat::FORMAT_RGBA: fmt.fmt = STextureParams::tfRGBA; break;
			case LCLightmapFormat::FORMAT_BC7:  fmt.fmt = STextureParams::tfBC7; break;
			case LCLightmapFormat::FORMAT_BC5:  fmt.fmt = STextureParams::tfDXT5; break;
		}

		fmt.flags.set(STextureParams::flDitherColor, false);
		fmt.flags.set(STextureParams::flGenerateMipMaps, false);
		fmt.flags.set(STextureParams::flBinaryAlpha, false);

		BYTE* raw_data = LPBYTE(*TEX.pSurface);
		u32	w = TEX.dwWidth;
		u32	h = TEX.dwHeight;
		u32	pitch = w * 4;
		DXTUtils::Compress(out_name, raw_data, 0, w, h, pitch, &fmt, 4);
	}

	// lmap (RGB + SUN)
	
 	{
		Status("Saving lmap...");
		string128 name;
		string_path out_name;
		xr_strcpy(name, gCompilerMode.get_lname());

		b_BuildTexture& TEX = *texture;
		xr_strconcat(out_name, name, "\\", TEX.name, "_lm.dds");
		FS.update_path(out_name, "$game_levels$", out_name);
		clMsg("Saving texture '%s'...", out_name);
		VerifyPath(out_name);

		STextureParams			fmt;
		switch (gCompilerMode.LmapsFormat)
		{
			case LCLightmapFormat::FORMAT_RGBA: fmt.fmt = STextureParams::tfRGBA; break;
			case LCLightmapFormat::FORMAT_BC7:  fmt.fmt = STextureParams::tfBC7; break;
			case LCLightmapFormat::FORMAT_BC5:  fmt.fmt = STextureParams::tfDXT5; break;
		}

		fmt.flags.set(STextureParams::flDitherColor, false);
		fmt.flags.set(STextureParams::flGenerateMipMaps, false);
		fmt.flags.set(STextureParams::flBinaryAlpha, false);

		
		if (!gCompilerMode.LC_SkipStaticMap)
		{
			xr_vector<u32> packed;
			lmap.Pack(packed);

			BYTE* raw_data = LPBYTE(&*packed.begin());
			u32	w = TEX.dwWidth;
			u32	h = TEX.dwHeight;
			u32	pitch = w * 4;
			DXTUtils::Compress(out_name, raw_data, 0, w, h, pitch, &fmt, 4);
		}
		else
		{
			xr_vector<u32> packed(4 * 4);
			for (auto& C : packed)
				C = color_rgba(0, 0, 0, 255);
 			BYTE* raw_data = LPBYTE(&*packed.begin());
			u32 w = 4, h = 4, pitch = w * 4;
 			DXTUtils::Compress(out_name, raw_data, 0, w, h, pitch, &fmt, 4);
		}

	}
	
	// Dealocate
	Deallocate();
	b_BuildTexture& TEX = *texture;
	TEX.pSurface.Clear();
}


// Client Global
vecFace& ImplicitCalcGlobs::query(float px, float py)
{
	return hash2dImpl.query(px, py);
}

void	ImplicitCalcGlobs::Initialize(ImplicitDeflector& d)
{
	d.Allocate();

	defl = &d;
	R_ASSERT(defl);
	Fbox2 bounds;
	defl->Bounds_Summary(bounds);

	hash2dImpl.initialize(bounds, (u32)defl->faces.size());
	for (u32 fid = 0; fid < defl->faces.size(); fid++)
	{
		Face* F = defl->faces[fid];
		F->AddChannel(F->tc[0].uv[0], F->tc[0].uv[1], F->tc[0].uv[2]); // make compatible format with LMAPs
		defl->Bounds(fid, bounds);
		hash2dImpl.add(bounds, F);
	}
}