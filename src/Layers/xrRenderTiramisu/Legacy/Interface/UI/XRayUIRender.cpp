#include "stdafx.h"

CDS0_UIRender GUIRender;

u32 UIShaderCounter = 0;
xr_set<CDS0_UIShader*> dbg_uishaders;
CDS0_UIShader::CDS0_UIShader()
{
	UIShaderCounter++;
	dbg_uishaders.insert(this);
}

CDS0_UIShader::~CDS0_UIShader()
{
	destroy();
	UIShaderCounter--;

	dbg_uishaders.erase(this);
}

void CDS0_UIShader::Copy(IUIShader& _in)
{
	destroy();
	Texture = static_cast<CDS0_UIShader&>(_in).Texture;
	if (Texture)
	{
		GRenderResourcesManager->TexturesManager->Copy(Texture);
	}
}

void CDS0_UIShader::create(LPCSTR sh, LPCSTR tex)
{
	destroy();
	if (tex == nullptr)
	{
		Texture = GRenderResourcesManager->WhiteTexture;
		return;
	}
	Texture = GRenderResourcesManager->TexturesManager->GetTexture(tex);
}

bool CDS0_UIShader::inited()
{
	return Texture;
}

void CDS0_UIShader::destroy()
{
	if (Texture)
	{
		GRenderResourcesManager->TexturesManager->Free(Texture);
		Texture = nullptr;
	}
}

CDS0_UIRender::CDS0_UIRender()
{
}

CDS0_UIRender::~CDS0_UIRender()
{
}

void CDS0_UIRender::CreateUIGeom()
{
}

void CDS0_UIRender::DestroyUIGeom()
{
}

void CDS0_UIRender::SetShader(IUIShader& shader)
{
	CurrentShader = &shader;
}

void CDS0_UIRender::SetAlphaRef(int aref)
{
}

void CDS0_UIRender::SetScissor(Irect* Rect)
{
	if (Rect == nullptr)
	{
		CurrentScissor = {};
		return;
	}

	CurrentScissor = *Rect;
}

void CDS0_UIRender::GetActiveTextureResolution(Fvector2& res)
{
	res = {1,1};
	if (CurrentShader)
	{
		if (static_cast<CDS0_UIShader*>(CurrentShader)->Texture)
		{
			res.x = static_cast<CDS0_UIShader*>(CurrentShader)->Texture->TextureDescription.width;
			res.y = static_cast<CDS0_UIShader*>(CurrentShader)->Texture->TextureDescription.height;
		}
	}
}

void CDS0_UIRender::PushPoint(float x, float y, float z, u32 C, float u, float v)
{
	FXRayUIPrimitive& Primitive = Primitivs.back();
	FUIVertex& Vertex = Primitive.VertexesCache.emplace_back();
	Vertex.position[0] = x;
	Vertex.position[1] = y;
	Vertex.position[2] = z;
	Vertex.color = C;
	Vertex.uv[0] = u;
	Vertex.uv[1] = v;
}

void** CDS0_UIRender::StartPrimitive(u32 iMaxVerts, ePrimitiveType primType, ePointType pointType)
{
	if (!Primitivs.empty())
	{
		VERIFY(Primitivs.back().VertexOffset != INDEX_NONE);
	}

	FXRayUIPrimitive& Primitive = Primitivs.emplace_back();
	Primitive.PrimitiveType = primType;
	Primitive.PointType = pointType;
	Primitive.VertexesCache.reserve(iMaxVerts);
	Primitive.VertexOffset = INDEX_NONE;
	Primitive.ScissorRect = CurrentScissor;

	return nullptr;
}

void CDS0_UIRender::FlushPrimitive()
{
	VERIFY(Primitivs.back().VertexOffset == INDEX_NONE);
	
	FXRayUIPrimitive& Primitive = Primitivs.back();
	Primitive.VertexOffset = Vertexes.size();

	if (CurrentShader)
	{
		Primitive.Texture = static_cast<CDS0_UIShader*>(CurrentShader)->Texture;
		if (Primitive.Texture)
		{
			GRenderResourcesManager->TexturesManager->Copy(Primitive.Texture);
		}
		Primitive.TextureResourceProxy = Primitive.Texture->ResourceProxy;
	}

	switch (Primitive.PointType)
	{
		case ePointType::pttL:
		{
			for (FUIVertex& Vertex : Primitive.VertexesCache)
			{
				Vertex.uv[0] = 0;
				Vertex.uv[1] = 0;
			}
			return;
		}
		case ePointType::pttLIT:
		case ePointType::pttTL:
			default: ;
	}
	
	switch (Primitive.PrimitiveType)
	{
	case ptTriList:
	{
		const size_t VertexCount = Primitive.VertexesCache.size();
		Vertexes.insert(Vertexes.end(), Primitive.VertexesCache.begin(), Primitive.VertexesCache.end());
		break;
	}
	case ptTriStrip:
		for (size_t VertexID = 2; VertexID < Primitive.VertexesCache.size(); ++VertexID)
		{
			if (VertexID & 1)
			{
				Vertexes.push_back(Primitive.VertexesCache[VertexID - 1]);
				Vertexes.push_back(Primitive.VertexesCache[VertexID - 2]);
				Vertexes.push_back(Primitive.VertexesCache[VertexID]);
			}
			else
			{
				Vertexes.push_back(Primitive.VertexesCache[VertexID - 2]);
				Vertexes.push_back(Primitive.VertexesCache[VertexID - 1]);
				Vertexes.push_back(Primitive.VertexesCache[VertexID]);
			}
		}
		break;
	case ptLineStrip:
	case ptLineList:
		break;
	default:
		NODEFAULT;
	}
	
	Primitive.VertexCount = Vertexes.size() - Primitive.VertexOffset;
}

void CDS0_UIRender::Flush()
{
	CurrentShader = nullptr;
	if (!Primitivs.empty())
	{
		VERIFY(Primitivs.back().VertexOffset != INDEX_NONE);
	}
	for (auto&Primitive:Primitivs)
	{
		if (Primitive.Texture)
		{
			GRenderResourcesManager->TexturesManager->Free(Primitive.Texture);
		}
	}
	Primitivs.clear();
	Vertexes.clear();
}

LPCSTR CDS0_UIRender::UpdateShaderName(LPCSTR tex_name, LPCSTR sh_name)
{
	string_path buff;
	return FS.exist(buff, "$game_textures$", tex_name, ".ogm") ? "hud\\movie" : sh_name;
}

void CDS0_UIRender::CacheSetXformWorld(const Fmatrix& M)
{
}

void CDS0_UIRender::CacheSetCullMode(ERHI_CULLMODE)
{
}
