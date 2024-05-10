// Image.h: interface for the CImage class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

class ECORE_API CXImage
{
public:
	u32   dwWidth;
	u32   dwHeight;
	BOOL	bAlpha;
	u32*	pData;

	CXImage()			{ ZeroMemory(this,sizeof(*this)); }
	~CXImage()			{ xr_free(pData); }

	void	Create		(u32 w, u32 h);
	void	Create		(u32 w, u32 h, u32* data);
	void	Load		(LPCSTR name);
	bool	LoadTGA		(LPCSTR name);
	void	SaveTGA		(LPCSTR name, BOOL b24=FALSE);

	void	Vflip		(void);
	void	Hflip		(void);
	void	Contrast	(float Q);
	void	Grayscale	();

	__forceinline u32	GetPixel(int x, int y)			{ return pData[y*dwWidth+x];}
	__forceinline void	PutPixel(int x, int y, u32 p)	{ pData[y*dwWidth+x] = p;	}
};
