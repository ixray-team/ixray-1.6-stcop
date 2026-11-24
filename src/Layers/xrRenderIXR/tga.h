// file: targasaver.h

#ifndef _INCDEF_TARGASAVER_H_
#define _INCDEF_TARGASAVER_H_

#pragma pack(push,1)
struct tgaImgSpecHeader{
	u16		tgaXOrigin;
	u16		tgaYOrigin;
	u16		tgaXSize;
	u16		tgaYSize;
	u8 tgaDepth;
	u8 tgaImgDesc;
};
struct tgaHeader{
	u8 tgaIDL;
	u8 tgaMapType;
	u8 tgaImgType;
	u8 tgaClrMapSpec[5];
	tgaImgSpecHeader tgaImgSpec;
};
#pragma pack(pop)


#define IMG_24B 0
#define IMG_32B 1

class TGAdesc
{
public:
	int format;
	int scanlenght;
	int width,height;
	void *data;
public:
	TGAdesc()	{ data = nullptr; };
	~TGAdesc()	{};

	void maketga( IWriter &fs );
//	void maketga( int hf );
};

void	tga_save	(const char* name, u32 w, u32 h, void* data, bool alpha );

#endif /*_INCDEF_TARGASAVER_H_*/
