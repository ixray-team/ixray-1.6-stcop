#pragma once

#define _FACDD  0x876
#define MAKE_DDHRESULT( code )  MAKE_HRESULT( 1, _FACDD, code )

enum _D3DXERR {
    D3DXERR_CANNOTMODIFYINDEXBUFFER = MAKE_DDHRESULT(2900),
    D3DXERR_INVALIDMESH = MAKE_DDHRESULT(2901),
    D3DXERR_CANNOTATTRSORT = MAKE_DDHRESULT(2902),
    D3DXERR_SKINNINGNOTSUPPORTED = MAKE_DDHRESULT(2903),
    D3DXERR_TOOMANYINFLUENCES = MAKE_DDHRESULT(2904),
    D3DXERR_INVALIDDATA = MAKE_DDHRESULT(2905),
    D3DXERR_LOADEDMESHASNODATA = MAKE_DDHRESULT(2906),
    D3DXERR_DUPLICATENAMEDFRAGMENT = MAKE_DDHRESULT(2907),
    D3DXERR_CANNOTREMOVELASTITEM = MAKE_DDHRESULT(2908),
};

typedef struct _D3DXSHADER_CONSTANTTABLE
{
    DWORD Size;             // sizeof(D3DXSHADER_CONSTANTTABLE)
    DWORD Creator;          // LPCSTR offset
    DWORD Version;          // shader version
    DWORD Constants;        // number of constants
    DWORD ConstantInfo;     // D3DXSHADER_CONSTANTINFO[Constants] offset
    DWORD Flags;            // flags shader was compiled with
    DWORD Target;           // LPCSTR offset 

} D3DXSHADER_CONSTANTTABLE, * LPD3DXSHADER_CONSTANTTABLE;


typedef struct _D3DXSHADER_CONSTANTINFO
{
    DWORD Name;             // LPCSTR offset
    WORD  RegisterSet;      // D3DXREGISTER_SET
    WORD  RegisterIndex;    // register number
    WORD  RegisterCount;    // number of registers
    WORD  Reserved;         // reserved
    DWORD TypeInfo;         // D3DXSHADER_TYPEINFO offset
    DWORD DefaultValue;     // offset of default value

} D3DXSHADER_CONSTANTINFO, * LPD3DXSHADER_CONSTANTINFO;

//----------------------------------------------------------------------------
// D3DXREGISTER_SET:
//----------------------------------------------------------------------------

typedef enum _D3DXREGISTER_SET
{
    D3DXRS_BOOL,
    D3DXRS_INT4,
    D3DXRS_FLOAT4,
    D3DXRS_SAMPLER,

    // force 32-bit size enum
    D3DXRS_FORCE_DWORD = 0x7fffffff

} D3DXREGISTER_SET, * LPD3DXREGISTER_SET;

//----------------------------------------------------------------------------
// D3DXPARAMETER_CLASS:
//----------------------------------------------------------------------------

typedef enum _D3DXPARAMETER_CLASS
{
    D3DXPC_SCALAR,
    D3DXPC_VECTOR,
    D3DXPC_MATRIX_ROWS,
    D3DXPC_MATRIX_COLUMNS,
    D3DXPC_OBJECT,
    D3DXPC_STRUCT,

    // force 32-bit size enum
    D3DXPC_FORCE_DWORD = 0x7fffffff

} D3DXPARAMETER_CLASS, * LPD3DXPARAMETER_CLASS;

typedef struct _D3DXSHADER_TYPEINFO
{
    WORD  Class;            // D3DXPARAMETER_CLASS
    WORD  Type;             // D3DXPARAMETER_TYPE
    WORD  Rows;             // number of rows (matrices)
    WORD  Columns;          // number of columns (vectors and matrices)
    WORD  Elements;         // array dimension
    WORD  StructMembers;    // number of struct members
    DWORD StructMemberInfo; // D3DXSHADER_STRUCTMEMBERINFO[Members] offset

} D3DXSHADER_TYPEINFO, * LPD3DXSHADER_TYPEINFO;

//----------------------------------------------------------------------------
// D3DXPARAMETER_TYPE:
//----------------------------------------------------------------------------

typedef enum _D3DXPARAMETER_TYPE
{
    D3DXPT_VOID,
    D3DXPT_BOOL,
    D3DXPT_INT,
    D3DXPT_FLOAT,
    D3DXPT_STRING,
    D3DXPT_TEXTURE,
    D3DXPT_TEXTURE1D,
    D3DXPT_TEXTURE2D,
    D3DXPT_TEXTURE3D,
    D3DXPT_TEXTURECUBE,
    D3DXPT_SAMPLER,
    D3DXPT_SAMPLER1D,
    D3DXPT_SAMPLER2D,
    D3DXPT_SAMPLER3D,
    D3DXPT_SAMPLERCUBE,
    D3DXPT_PIXELSHADER,
    D3DXPT_VERTEXSHADER,
    D3DXPT_PIXELFRAGMENT,
    D3DXPT_VERTEXFRAGMENT,
    D3DXPT_UNSUPPORTED,

    // force 32-bit size enum
    D3DXPT_FORCE_DWORD = 0x7fffffff

} D3DXPARAMETER_TYPE, * LPD3DXPARAMETER_TYPE;


inline HRESULT D3D9FindShaderComment(const DWORD* byte_code, DWORD fourcc, const void** data, UINT* size)
{
    const DWORD* ptr = byte_code;
    DWORD version = {};

    if (data) {
        *data = nullptr;
    }
    if (size) {
        *size = 0;
    }

    if (!byte_code) {
        return D3DERR_INVALIDCALL;
    }

    version = *ptr >> 16;

    BOOL isInvalidData = version != 0x4658      /* FX */
        && version != 0x5458                    /* TX */
        && version != 0x7ffe
        && version != 0x7fff
        && version != 0xfffe                    /* VS */
        && version != 0xffff;                   /* PS */
    if (isInvalidData) {
        return D3DXERR_INVALIDDATA;
    }

    while (*++ptr != D3DSIO_END) {
        if ((*ptr & D3DSI_OPCODE_MASK) == D3DSIO_COMMENT) {
            DWORD comment_size = (*ptr & D3DSI_COMMENTSIZE_MASK) >> D3DSI_COMMENTSIZE_SHIFT;

            if (*(ptr + 1) == fourcc) {
                UINT ctab_size = (comment_size - 1) * sizeof(DWORD);
                const void* ctab_data = ptr + 2;
                if (size) {
                    *size = ctab_size;
                }
                if (data) {
                    *data = ctab_data;
                }
                return D3D_OK;
            }
            ptr += comment_size;
        }
    }

    return S_FALSE;
}