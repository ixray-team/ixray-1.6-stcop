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
        DWORD biSize;           // Размер структуры (40 байт)
        long  biWidth;          // Ширина в пикселях
        long  biHeight;         // Высота в пикселях
        u16  biPlanes;         // Всегда 1
        u16  biBitCount;       // Бит на пиксель (1, 4, 8, 16, 24, 32)
        DWORD biCompression;    // Тип компрессии
        DWORD biSizeImage;      // Размер изображения в байтах (0 если BI_RGB)
        long  biXPelsPerMeter;  // Горизонтальное разрешение (пикселей на метр)
        long  biYPelsPerMeter;  // Вертикальное разрешение
        DWORD biClrUsed;        // Количество используемых цветов (0 = все)
        DWORD biClrImportant;   // Количество важных цветов (0 = все)
    };
    #pragma pack(pop)
    
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
	BYTE				*m_pMovieData;
	HIC					m_aviIC;
	BYTE				*m_pDecompressedBuf;

	BITMAPINFOHEADER	m_biOutFormat;
	BITMAPINFOHEADER	m_biInFormat;

	float				m_fRate;		// стандартная скорость, fps
	float				m_fCurrentRate;	// текущая скорость, fps

	DWORD				m_dwFrameTotal;
	DWORD				m_dwFrameCurrent;
	u32					m_dwFirstFrameOffset;


	DWORD				CalcFrame			();

	bool				DecompressFrame		( DWORD	dwFrameNum );
	void				PreRoll				( DWORD dwFrameNum );

public:
						CAviPlayerCustom		( );
						~CAviPlayerCustom		( );

	DWORD				m_dwWidth, m_dwHeight;

	void				GetSize				( DWORD *dwWidth, DWORD *dwHeight );
	
	bool				Load				( char *fname  );
	bool				GetFrame			( BYTE **pDest );

	bool				NeedUpdate			( ) { return CalcFrame( ) != m_dwFrameCurrent; }
	int					SetSpeed			( int nPercent );
};
#endif
