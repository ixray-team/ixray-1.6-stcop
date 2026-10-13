//--------------------------------------------------------------------------------------
// Helpers for legacy Direct3D 9 era FVF codes and vertex declarations.
//
// Derived from DirectXMesh FlexibleVertexFormat.h
// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
// http://go.microsoft.com/fwlink/?LinkID=324981
//
// Only the functions used by the engine are kept; the DX9 identifiers come from D3DLegacy.h.
//--------------------------------------------------------------------------------------

#pragma once

#include "D3DLegacy.h"

#include <cstdint>
#include <iterator>
#include <vector>

namespace FVF
{
    constexpr uint8_t g_declTypeSizes[] =
    {
        4,  // D3DDECLTYPE_FLOAT1
        8,  // D3DDECLTYPE_FLOAT2
        12, // D3DDECLTYPE_FLOAT3
        16, // D3DDECLTYPE_FLOAT4
        4,  // D3DDECLTYPE_D3DCOLOR
        4,  // D3DDECLTYPE_UBYTE4
        4,  // D3DDECLTYPE_SHORT2
        8,  // D3DDECLTYPE_SHORT4
        4,  // D3DDECLTYPE_UBYTE4N
        4,  // D3DDECLTYPE_SHORT2N
        8,  // D3DDECLTYPE_SHORT4N
        4,  // D3DDECLTYPE_USHORT2N
        8,  // D3DDECLTYPE_USHORT4N
        4,  // D3DDECLTYPE_UDEC3
        4,  // D3DDECLTYPE_DEC3N
        4,  // D3DDECLTYPE_FLOAT16_2
        8,  // D3DDECLTYPE_FLOAT16_4
    };

    static_assert(std::size(g_declTypeSizes) == D3DDECLTYPE_UNUSED, "Mismatch of array size");

    inline size_t ComputeVertexSize(uint32_t fvfCode)
    {
        if ((fvfCode & ((D3DFVF_RESERVED0 | D3DFVF_RESERVED2) & ~D3DFVF_POSITION_MASK)) != 0)
            return 0;

        const size_t numCoords = (fvfCode & D3DFVF_TEXCOUNT_MASK) >> D3DFVF_TEXCOUNT_SHIFT;
        if (numCoords > 8)
            return 0;

        size_t vertexSize = 0;

        switch (fvfCode & D3DFVF_POSITION_MASK)
        {
        case 0: break;
        case D3DFVF_XYZ:    vertexSize = 3 * sizeof(float); break;

        case D3DFVF_XYZRHW:
        case D3DFVF_XYZB1:
        case D3DFVF_XYZW:
            vertexSize = 4 * sizeof(float);
            break;

        case D3DFVF_XYZB2:  vertexSize = 5 * sizeof(float); break;
        case D3DFVF_XYZB3:  vertexSize = 6 * sizeof(float); break;
        case D3DFVF_XYZB4:  vertexSize = 7 * sizeof(float); break;
        case D3DFVF_XYZB5:  vertexSize = 8 * sizeof(float); break;
        default: return 0;
        }

        if (fvfCode & D3DFVF_NORMAL)
            vertexSize += 3 * sizeof(float);

        if (fvfCode & D3DFVF_PSIZE)
            vertexSize += sizeof(uint32_t);

        if (fvfCode & D3DFVF_DIFFUSE)
            vertexSize += sizeof(uint32_t);

        if (fvfCode & D3DFVF_SPECULAR)
            vertexSize += sizeof(uint32_t);

        // Texture coordinates
        uint32_t textureFormats = fvfCode >> 16u;

        if (textureFormats)
        {
            for (size_t i = 0; i < numCoords; i++)
            {
                switch (textureFormats & 3)
                {
                case 0: vertexSize += 2 * sizeof(float); break;
                case 1: vertexSize += 3 * sizeof(float); break;
                case 2: vertexSize += 4 * sizeof(float); break;
                case 3: vertexSize += 1 * sizeof(float); break;
                }

                textureFormats >>= 2;
            }
        }
        else
        {
            vertexSize += numCoords * (2 * sizeof(float));
        }

        return vertexSize;
    }

    inline size_t ComputeVertexSize(const D3DVERTEXELEMENT9* pDecl, uint32_t stream)
    {
        if (!pDecl || stream >= 16u /*D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT*/)
            return 0;

        size_t currentSize = 0;
        size_t count = 0;

        //search for the max offset in the stream,
        //(min)vertex size = max offset + type size
        while (pDecl->Stream != 0xFF)
        {
            ++count;
            if (count > MAXD3DDECLLENGTH)
                return 0;

            // only look at items of this stream and vertex elements actually in the data stream (not generated)
            // UV is phantom data.
            if ((pDecl->Stream == stream) && (pDecl->Method != D3DDECLMETHOD_UV))
            {
                if (pDecl->Type >= std::size(g_declTypeSizes))
                    return 0;

                const size_t slotSize = g_declTypeSizes[pDecl->Type];
                if (currentSize < slotSize + pDecl->Offset)
                    currentSize = slotSize + pDecl->Offset;
            }

            ++pDecl;
        }

        return currentSize;
    }

    // More secure version
    inline size_t ComputeVertexSize(
        _In_reads_(maxDeclLength) const D3DVERTEXELEMENT9* pDecl, size_t maxDeclLength, uint32_t stream)
    {
        if (!pDecl || stream >= 16u /*D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT*/)
            return 0;

        if (maxDeclLength > MAXD3DDECLLENGTH + 1)
            return 0;

        size_t currentSize = 0;
        size_t count = 0;

        //search for the max offset in the stream,
        //(min)vertex size = max offset + type size
        while (pDecl->Stream != 0xFF)
        {
            ++count;
            if (count > maxDeclLength)
                return 0;

            // only look at items of this stream and vertex elements actually in the data stream (not generated)
            // UV is phantom data.
            if ((pDecl->Stream == stream) && (pDecl->Method != D3DDECLMETHOD_UV))
            {
                if (pDecl->Type >= std::size(g_declTypeSizes))
                    return 0;

                const size_t slotSize = g_declTypeSizes[pDecl->Type];
                if (currentSize < slotSize + pDecl->Offset)
                    currentSize = slotSize + pDecl->Offset;
            }

            ++pDecl;
        }

        return currentSize;
    }

    inline size_t GetDeclLength(const D3DVERTEXELEMENT9* pDecl)
    {
        if (!pDecl)
            return 0;

        size_t length = 0;
        while (pDecl->Stream != 0xFF)
        {
            if (length >= MAXD3DDECLLENGTH)
                return 0;

            ++pDecl;
            ++length;
        }
        return length;
    }

    _Success_(return)
    inline bool CreateDeclFromFVF(uint32_t fvfCode, std::vector<D3DVERTEXELEMENT9>& decl)
    {
        static constexpr size_t s_texCoordSizes[] =
        {
            2 * sizeof(float),
            3 * sizeof(float),
            4 * sizeof(float),
            sizeof(float)
        };

        decl.clear();

        if ((fvfCode & ((D3DFVF_RESERVED0 | D3DFVF_RESERVED2) & ~D3DFVF_POSITION_MASK)) != 0)
            return false;

        const uint32_t nTexCoords = (fvfCode & D3DFVF_TEXCOUNT_MASK) >> D3DFVF_TEXCOUNT_SHIFT;
        if (nTexCoords > 8)
            return false;

        size_t offset = 0;

        switch (fvfCode & D3DFVF_POSITION_MASK)
        {
        case 0:
            break;

        case D3DFVF_XYZRHW:
            decl.emplace_back(
                D3DVERTEXELEMENT9{ 0, 0, D3DDECLTYPE_FLOAT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITIONT, 0 }
            );
            offset = sizeof(float) * 4;
            break;

        case D3DFVF_XYZW:
            decl.emplace_back(
                D3DVERTEXELEMENT9{ 0, 0, D3DDECLTYPE_FLOAT4, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0 }
            );
            offset = sizeof(float) * 4;
            break;

        default:
            decl.emplace_back(
                D3DVERTEXELEMENT9{ 0, 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0 }
            );
            offset = sizeof(float) * 3;
            break;
        }

        size_t weights = 0;
        switch (fvfCode & D3DFVF_POSITION_MASK)
        {
        case D3DFVF_XYZB1: weights = 1; break;
        case D3DFVF_XYZB2: weights = 2; break;
        case D3DFVF_XYZB3: weights = 3; break;
        case D3DFVF_XYZB4: weights = 4; break;
        case D3DFVF_XYZB5: weights = 5; break;
        }

        if (weights > 0)
        {
            if (fvfCode & (D3DFVF_LASTBETA_UBYTE4 | D3DFVF_LASTBETA_D3DCOLOR))
            {
                // subtract one to convert to D3DDECLTYPE_FLOAT* and another for where the indices were
                if (weights > 1)
                {
                    decl.emplace_back(
                        D3DVERTEXELEMENT9{ 0, static_cast<uint16_t>(offset), static_cast<uint8_t>(weights - 2),
                            D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDWEIGHT, 0 }
                    );
                    offset += sizeof(float) * (weights - 1);
                }

                decl.emplace_back(
                    D3DVERTEXELEMENT9{ 0, static_cast<uint16_t>(offset),
                        static_cast<uint8_t>((fvfCode & D3DFVF_LASTBETA_UBYTE4) ? D3DDECLTYPE_UBYTE4 : D3DDECLTYPE_D3DCOLOR),
                        D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDINDICES, 0 }
                );
                offset += sizeof(uint32_t);
            }
            else if (weights == 5)
            {
                // D3DFVF_XYZB5 is only supported when the 5th beta is D3DFVF_LASTBETA_UBYTE4/D3DCOLOR
                decl.clear();
                return false;
            }
            else
            {
                // subtract one to convert to D3DDECLTYPE_FLOAT*
                decl.emplace_back(
                    D3DVERTEXELEMENT9{ 0, static_cast<uint16_t>(offset), static_cast<uint8_t>(weights - 1),
                        D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_BLENDWEIGHT, 0 }
                );
                offset += sizeof(float) * (weights - 1);
            }
        }

        if (fvfCode & D3DFVF_NORMAL)
        {
            decl.emplace_back(
                D3DVERTEXELEMENT9{ 0, static_cast<uint16_t>(offset), D3DDECLTYPE_FLOAT3,
                    D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_NORMAL, 0 }
            );
            offset += sizeof(float) * 3;
        }

        if (fvfCode & D3DFVF_PSIZE)
        {
            decl.emplace_back(
                D3DVERTEXELEMENT9{ 0, static_cast<uint16_t>(offset), D3DDECLTYPE_FLOAT1,
                    D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_PSIZE, 0 }
            );
            offset += sizeof(float);
        }

        if (fvfCode & D3DFVF_DIFFUSE)
        {
            decl.emplace_back(
                D3DVERTEXELEMENT9{ 0, static_cast<uint16_t>(offset), D3DDECLTYPE_D3DCOLOR,
                    D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0 }
            );
            offset += sizeof(uint32_t);
        }

        if (fvfCode & D3DFVF_SPECULAR)
        {
            decl.emplace_back(
                D3DVERTEXELEMENT9{ 0, static_cast<uint16_t>(offset), D3DDECLTYPE_D3DCOLOR,
                    D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 1 }
            );
            offset += sizeof(uint32_t);
        }

        if (nTexCoords > 0)
        {
            for (uint32_t t = 0; t < nTexCoords; ++t)
            {
                const size_t texCoordSize = s_texCoordSizes[(fvfCode >> (16 + t * 2)) & 0x3];

                // D3DDECLTYPE_FLOAT1 = 0, D3DDECLTYPE_FLOAT4 = 3
                decl.emplace_back(
                    D3DVERTEXELEMENT9{ 0, static_cast<uint16_t>(offset),
                        static_cast<uint8_t>(texCoordSize / sizeof(float) - 1),
                        D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, static_cast<uint8_t>(t) }
                );
                offset += texCoordSize;
            }
        }

        decl.emplace_back(D3DVERTEXELEMENT9{ 0xFF, 0, D3DDECLTYPE_UNUSED, 0, 0, 0 });

        return true;
    }

}
