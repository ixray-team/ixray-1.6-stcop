#ifndef tntQAVIH
#define tntQAVIH

#include <math.h>

#ifdef IXR_WINDOWS
#include "Vfw.h"
#include "mmsystem.h"
#else
using FOURCC = u32;

struct AVIINDEXENTRY
{
    u32 ckid;
    u32 dwFlags;
    u32 dwChunkOffset;
    u32 dwChunkLength;
};

    #define BI_RGB        0L
    #define BI_RLE8       1L
    #define BI_RLE4       2L
    #define BI_BITFIELDS  3L
    #define BI_JPEG       4L
    #define BI_PNG        5L
    
    #pragma pack(push, 1)
    struct BITMAPINFOHEADER
	{
        u32 biSize;			   // Размер структуры (40 байт)
		s32 biWidth;		   // Ширина в пикселях
		s32 biHeight;		   // Высота в пикселях
        u16  biPlanes;         // Всегда 1
        u16  biBitCount;       // Бит на пиксель (1, 4, 8, 16, 24, 32)
        u32 biCompression;	   // Тип компрессии
		u32 biSizeImage;	   // Размер изображения в байтах (0 если BI_RGB)
		s32 biXPelsPerMeter;   // Горизонтальное разрешение (пикселей на метр)
		s32 biYPelsPerMeter;   // Вертикальное разрешение
		u32 biClrUsed;		   // Количество используемых цветов (0 = все)
		u32 biClrImportant;	   // Количество важных цветов (0 = все)
    };
    #pragma pack(pop)
    static_assert(sizeof(BITMAPINFOHEADER) == 40);
    
    // RGBQUAD для палитры
    struct RGBQUAD
	 {
        u8 rgbBlue;
        u8 rgbGreen;
        u8 rgbRed;
        u8 rgbReserved;  // Обычно 0
    };

    // Полная структура BITMAPINFO (BITMAPINFOHEADER + палитра)
    struct BITMAPINFO
	{
        BITMAPINFOHEADER bmiHeader;
        RGBQUAD          bmiColors[1];  // Палитра (для <= 8 бит)
    };
	using HIC = void*;
#endif

// replaced with standard AVIIF_KEYFRAME
//rr #define	AVIINDEX_ISKEYFRAME		0x10	// ключевой кадр

// reverse enginered AVI index v.1 format
/*struct AviIndex {

	DWORD	dwChunkType;	// chunk type, i.e. '##dc' - DIB compressed
	DWORD	dwFlags;		// key-frame etc.
	DWORD	dwOffset;		// sub-chunk offset from the begining of the LIST chunk
	DWORD	dwLenght;		// chunk lenght

};

typedef struct {
	FOURCC fccType;
	FOURCC fccHandler;
	DWORD  dwFlags;
	DWORD  dwPriority;
	DWORD  dwInitialFrames;
	DWORD  dwScale;
	DWORD  dwRate;
	DWORD  dwStart;
	DWORD  dwLength;
	DWORD  dwSuggestedBufferSize;
	DWORD  dwQuality;
	DWORD  dwSampleSize;
	RECT   rcFrame;
} AVIStreamHeader;
*/
typedef struct {
	FOURCC fccType;
	FOURCC fccHandler;
	u32 dwFlags;
	u32 dwPriority;
	u32 dwInitialFrames;
	u32 dwScale;
	u32 dwRate;
	u32 dwStart;
	u32 dwLength;
	u32 dwSuggestedBufferSize;
	u32 dwQuality;
	u32 dwSampleSize;
	struct
	{
		u16	left;
		u16	top;
		u16	right;
		u16	bottom;
	};
//	RECT   rcFrame;		- лажа в MSDN
} AVIStreamHeaderCustom;

class ENGINE_API CAviPlayerCustom
{
protected:
	CAviPlayerCustom	*alpha;
protected:
	AVIINDEXENTRY		*m_pMovieIndex;
	u8* m_pMovieData;
	HIC					m_aviIC;
	u8* m_pDecompressedBuf;

	BITMAPINFOHEADER	m_biOutFormat;
	BITMAPINFOHEADER	m_biInFormat;

	float				m_fRate;		// стандартная скорость, fps
	float				m_fCurrentRate;	// текущая скорость, fps

	u32 m_dwFrameTotal;
	u32 m_dwFrameCurrent;
	u32					m_dwFirstFrameOffset;


	u32 CalcFrame();

	bool DecompressFrame(u32 dwFrameNum);
	void PreRoll(u32 dwFrameNum);

public:
						CAviPlayerCustom		( );
						~CAviPlayerCustom		( );

	u32 m_dwWidth, m_dwHeight;

	void GetSize(u32* dwWidth, u32* dwHeight);
	
	bool				Load				( char *fname  );
	bool GetFrame(u8** pDest);

	bool				NeedUpdate			( ) { return CalcFrame( ) != m_dwFrameCurrent; }
	int					SetSpeed			( int nPercent );
};
#endif
