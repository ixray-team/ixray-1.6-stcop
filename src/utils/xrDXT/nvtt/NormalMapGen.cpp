#include "stdafx.h"

#include "NVI_Convolution.h"
#include "NVI_Image.h"
#include "../API/tga.h"

using namespace xray_nvi;

void MakeKernelElems(const float* pInWeightArray, int num_x, int num_y, ConvolutionKernelElement* pOutArray);
void RotateArrayCCW(const float* pInArray, int num_x, int num_y, float* pOutArray);

u8 fpack(float v)
{
    s32 _v = iFloor(((v + 1.f)*.5f)*255.f + .5f);
    clamp(_v, 0, 255);
    return u8(_v);
}

Fvector vunpack(s32 x, s32 y, s32 z)
{
    Fvector pck;
    pck.x = (float(x) / 255.f - .5f)*2.f;
    pck.y = (float(y) / 255.f - .5f)*2.f;
    pck.z = (float(z) / 255.f - .5f)*2.f;
    return pck;
}

Fvector vunpack(Ivector src)
{
    return vunpack(src.x, src.y, src.z);
}

Ivector	vpack(Fvector src)
{
	src.normalize();
	Fvector _v;
	int bx = fpack(src.x);
	int by = fpack(src.y);
	int bz = fpack(src.z);
	// dumb test
	float e_best = flt_max;
	int r = bx, g = by, b = bz;
	int d = 2;
	for (int x = std::max(bx - d, 0); x <= std::max(bx + d, 255); x++)
		for (int y = std::max(by - d, 0); y <= std::min(by + d, 255); y++)
			for (int z = std::max(bz - d, 0); z <= std::min(bz + d, 255); z++)
			{
				_v = vunpack(x, y, z);
				float m = _v.magnitude();
				float me = _abs(m - 1.f);
				if (me > 0.03f)
					continue;

				_v.div(m);
				float e = _abs(src.dotproduct(_v) - 1.f);
				if (e < e_best)
				{
					e_best = e;
					r = x, g = y, b = z;
				}
			}

	Ivector ipck;
	ipck.set(r, g, b);
	return ipck;
}

void CalculateNormalMap(NVI_Image* pSrc, ConvolutionKernel* pKernels, int num_kernels, float scale, bool wrap)
{
    // pKernels must be an array of at least two kernels
    // The first kernel extracts dh/dx  (change in height with respect to x )
    // the second extracts dh/dy  (change in height with respect to y )
    VERIFY(pKernels);
    VERIFY(num_kernels == 2);
    float results[2];
    // Set up the convolver & prepare image data
    Convolver conv;
    conv.Initialize(&pSrc, pKernels, num_kernels, wrap);
    int size_x = (int)pSrc->GetWidth();
    int size_y = (int)pSrc->GetHeight();
    DWORD* pArray = (DWORD*)pSrc->GetImageDataPointer();
    assert(pArray != NULL);
    // Now run the kernel over the source image area and write out the values.
    // coordinates of source image (not padded)
    for (int j = 0; j < size_y; j++)
    {
        for (int i = 0; i < size_x; i++)
        {
            // apply kernels
            conv.Convolve_Alpha_At(i, j, results, 2);
            float du = results[0] * scale;
            float dv = results[1] * scale;
            // det  | x  y  z |
            //      | 1  0 du |
            //      | 0  1 dv |
            //
            // cross product gives (-du, -dv, 1.0) as normal 
            float mag = du*du + dv*dv + 1.0f;
            mag = (float)_sqrt(mag);
            // Get alpha as height
            char height = (char)(pArray[j * size_x + i]) >> 24;
            Fvector src = { -du / mag, -dv / mag, 1.0f / mag };
            Ivector dst = vpack(src);
            unsigned long nmap_color = color_rgba(dst.x, dst.y, dst.z, 0);
            //. AlphaAndVectorToARGB(height, -du/mag, -dv/mag, 1.0f / mag, nmap_color);
            pArray[j * size_x + i] = nmap_color;
        }
    }
}

void ConvertAlphaToNormalMap_4x(NVI_Image* pSrc, float scale, bool wrap)
{
    // Do the conversion using a 4 sample nearest neighbor pattern
    //
    // d height / du kernel:
    //  0		0		0
    //	-1/2	0		1/2
    //	0		0		0
    //
    // d height / dv kernel:
    //	0	1/2		0
    //	0	0		0
    //	0	-1/2	0
    int numelem = 2; // num elements in each kernel
    ConvolutionKernelElement du_elem[] =
    {
        { -1, 0, -1.0f / 2.0f },
        { 1, 0, 1.0f / 2.0f }
    };
    ConvolutionKernelElement dv_elem[] =
    {
        { 0, 1, 1.0f / 2.0f },
        { 0, -1, -1.0f / 2.0f }
    };
    int num_kernels = 2;
    ConvolutionKernel kernels[2];
    kernels[0].SetElements(numelem, du_elem);
    kernels[1].SetElements(numelem, dv_elem);
    // Calc ARGB normal map & write to the "in." file
    CalculateNormalMap(pSrc, kernels, num_kernels, scale, wrap);
}

static float gloss_power = 0.f;

IC u32 it_gloss_rev(u32 d, u32 s)
{
    gloss_power += float(color_get_A(s)) / 255.f;
    return color_rgba(
        //.	color_get_A(s)+1, // gloss
        clampr(color_get_A(s) + 1, u32(0), u32(255)),
        color_get_B(d),
        color_get_G(d),
        color_get_R(d));
}

IC u32 it_difference(u32 d, u32 orig, u32 ucomp)
{
    return color_rgba(
        128 + 2 * (int(color_get_R(orig)) - int(color_get_R(ucomp))) / 3,   // R-error
        128 + 2 * (int(color_get_G(orig)) - int(color_get_G(ucomp))) / 3,   // G-error
        128 + 2 * (int(color_get_B(orig)) - int(color_get_B(ucomp))) / 3,   // B-error
        128 + 2 * (int(color_get_A(orig)) - int(color_get_A(ucomp))) / 3);	// A-error	
}

IC u32 it_height_rev(u32 d, u32 s)
{
    return color_rgba(
        color_get_A(d),  // diff x
        color_get_B(d),  // diff y
        color_get_G(d),  // diff z
        color_get_R(s)); // height
}

template<class _It>
IC void TW_Iterate_1OP(u32 width, u32 height, u32 pitch, u8* dst, u8* src, const _It pred)
{
    for (u32 y = 0; y < height; y++)
    {
        for (u32 x = 0; x < width; x++)
        {
            u32& pSrc = *((u32*)(src + y*pitch) + x);
            u32& pDst = *((u32*)(dst + y*pitch) + x);
            pDst = pred(pDst, pSrc);
        }
    }
}

template<class _It>
IC void TW_Iterate_2OP(u32 width, u32 height, u32 pitch, u8* dst, u8* src0, u8* src1, const _It pred)
{
    for (u32 y = 0; y < height; y++)
    {
        for (u32 x = 0; x < width; x++)
        {
            u32& pSrc0 = *((u32*)(src0 + y*pitch) + x);
            u32& pSrc1 = *((u32*)(src1 + y*pitch) + x);
            u32& pDst = *((u32*)(dst + y*pitch) + x);
            pDst = pred(pDst, pSrc0, pSrc1);
        }
    }
}

u32	hsample(s32 w, s32 h, s32 p, s32 x, s32 y, u8* src)
{
    if (x < 0)
    {
        x += w;
    }
    x %= w;
    if (y < 0)
    {
        y += h;
    }
    y %= h;
    return color_get_R(*((u32*)(src + y*p) + x));
}

#include "../../Layers/xrRender/ETextureParams.h"
#include "Image_DXTC.h"

extern int DXTCompressImageNVTT(LPCSTR out_name, u8* raw_data,
    u32 w, u32 h, u32 pitch, STextureParams* fmt, u32 depth);

int DXTCompressBump(LPCSTR out_name, u8* T_height_gloss, u8* T_normal_map,
	u32 w, u32 h, u32 pitch, STextureParams* fmt, u32 depth)
{
	VERIFY(4 == depth);
	NVI_Image* pSrc = new NVI_Image();
	pSrc->Initialize(w, h, NVI_A8_R8_G8_B8, T_height_gloss);
	pSrc->AverageRGBToAlpha();
	// stage 0
#ifndef PVS_STUDIO
	pitch = w * 4;
#endif

	if (T_normal_map)
	{
		u8* ext_nm = pSrc->GetImageDataPointer();
		memcpy(ext_nm, T_normal_map, w * h * sizeof(u32));
	}
	else
	{
		ConvertAlphaToNormalMap_4x(pSrc, fmt->bump_virtual_height * 200.f, true);
	}
	u8* T_normal_1 = pSrc->GetImageDataPointer();

	gloss_power = 0.0f;
	// T_height_gloss.a (gloss) -> T_normal_1 + reverse of channels
	TW_Iterate_1OP(w, h, pitch, T_normal_1, T_height_gloss, it_gloss_rev);
	gloss_power /= float(w * h);

	STextureParams fmt0;
	fmt0.flags.assign(STextureParams::flGenerateMipMaps);
	fmt0.type = STextureParams::ttImage;
	fmt0.fmt = STextureParams::tfDXT5;
	int res = DXTCompressImageNVTT(out_name, T_normal_1, w, h, pitch, &fmt0, depth);

	// stage 1
	if (res == 1)
	{
		// Decompress (back)
		Image_DXTC* img = new Image_DXTC();
		if (img->LoadFromFile(out_name))
		{
			VERIFY(w == img->Width() && h == img->Height());
			img->Decompress();
			u8* T_normal_1U = img->GetDecompDataPointer();

			// Calculate difference
			u8* T_normal_1D = (u8*)calloc(w * h, sizeof(u32));
			TW_Iterate_2OP(w, h, pitch, T_normal_1D, T_normal_1, T_normal_1U, it_difference);

			// Rescale by virtual height
			float h_scale = powf(fmt->bump_virtual_height / 0.05f, 0.75f); // move towards 1.0f
			if (h_scale > 1.f)
				h_scale = _sqrt(h_scale);

			for (u32 y = 0; y < h; y++)
			{
				for (u32 x = 0; x < w; x++)
				{
					u32& sh = *((u32*)(T_height_gloss + y * pitch) + x);
					u32 h = color_get_R(sh); // height -> R-channel
					h = iFloor(float(h) * h_scale + EPS_S);
					sh = color_rgba(h, color_get_G(sh), color_get_B(sh), color_get_A(sh));
				}
			}
			// Calculate bounds for centering
			u32 h_average = 0, h_min = 255, h_max = 0;
			{
				for (u32 y = 0; y < h; y++)
				{
					for (u32 x = 0; x < w; x++)
					{
						u32 sh = *((u32*)(T_height_gloss + y * pitch) + x);
						u32 h = color_get_R(sh); // height -> R-channel
						h_average += h;
						
						h_min = std::min(h, h_min);
						h_max = std::min(h, h_max);
					}
				}
			}
			// final median, which will be used for centering
			u32 h_median = 9 * (h_average / (w * h)) + 1 * ((h_max - h_min) / 2 + h_min);
			h_median /= 10;
			s32 h_correction = s32(127) - s32(h_median);
			// Calculate filtered and corrected height
			u8* T_height_pf = (u8*)calloc(w * h, sizeof(u32)); // filtered for parallax
			for (s32 y = 0; y < s32(h); y++)
			{
				u32 p = pitch;
				u8* T = T_height_gloss;
				for (s32 x = 0; x < s32(w); x++)
				{
					u32& dst = *((u32*)(T_height_pf + y * pitch) + x);
					u32 val;
					if (strstr(Core.Params, "-bump_filtering"))
					{
						val = hsample(w, h, p, x - 1, y - 1, T) + hsample(w, h, p, x + 0, y - 1, T) + hsample(w, h, p, x + 1, y - 1, T) +
							  hsample(w, h, p, x - 1, y + 0, T) + hsample(w, h, p, x + 0, y + 0, T) + hsample(w, h, p, x + 1, y + 0, T) +
							  hsample(w, h, p, x - 1, y + 1, T) + hsample(w, h, p, x + 0, y + 1, T) + hsample(w, h, p, x + 1, y + 1, T);
						val /= 9;
					}
					else
						val = hsample(w, h, p, x, y, T);

					s32	r = clampr(s32(val) + h_correction, 0, 255);
					dst = color_rgba(r, r, r, r);
				}
			}
			// Reverse channels back + transfer heightmap
			TW_Iterate_1OP(w, h, pitch, T_normal_1D, T_height_pf, it_height_rev);

			// Compress
			STextureParams fmt0;
			fmt0.flags.assign(STextureParams::flGenerateMipMaps);
			fmt0.type = STextureParams::ttImage;
			fmt0.fmt = STextureParams::tfDXT5;

			string256 out_name1;
			strcpy(out_name1, out_name);
			if (strext(out_name1))
				*strext(out_name1) = 0;

			strcat(out_name1, "#.dds");
			res |= DXTCompressImageNVTT(out_name1, T_normal_1D, w, h, pitch, &fmt0, depth);
			free(T_height_pf);
			free(T_normal_1D);
		}
		else res = 0;

		delete img;
	}

	delete pSrc;

	if (gloss_power < 0.1f)
		res = -1000;

	return res;
}
