#include "StdAfx.h"
#include "compiler.h"
#include "xrCore/FormatParsers/LevelCForm/CFormIO.h"
#include "AIMapExport.h"

size_t BuildAIMapVersion = 0;
IC	const Fvector vertex_position(const NodePosition& Psrc, const Fbox& bb, const SAIParams& params)
{
	Fvector Pdest;
	int	x, z, row_length;
	row_length = iFloor((bb.max.z - bb.min.z) / params.fPatchSize + EPS_L + 1.5f);
	x = Psrc.xz() / row_length;
	z = Psrc.xz() % row_length;
	Pdest.x = float(x) * params.fPatchSize + bb.min.x;
	Pdest.y = (float(Psrc.y()) / 65535) * (bb.max.y - bb.min.y) + bb.min.y;
	Pdest.z = float(z) * params.fPatchSize + bb.min.z;
	return				(Pdest);
}

IC void CNodePositionConverter(const SNodePositionOld& Psrc, hdrNODES& m_header, NodePosition& np)
{
	Fvector		Pdest;
	Pdest.x = float(Psrc.x) * m_header.size;
	Pdest.y = (float(Psrc.y) / 65535) * m_header.size_y + m_header.aabb.min.y;
	Pdest.z = float(Psrc.z) * m_header.size;
	CNodePositionCompressor(np, Pdest, m_header);
	np.y(Psrc.y);
}

//-----------------------------------------------------------------

void xrLoad(const char* name, bool draft_mode, bool skipThm)
{
	FS.get_path("$level$")->_set((LPSTR)name);

	comp_data.xrLoadData(name, draft_mode, skipThm);

	// Load initial map from the Level Editor
	{
		string_path file_name;
		xr_strconcat(file_name, name, "build.aimap");
		IReader* F = FS.r_open(file_name);
		R_ASSERT2(F, file_name);

		R_ASSERT(F->open_chunk(E_AIMAP_CHUNK_VERSION));
		u16 version = F->r_u16();
		R_ASSERT(version <= E_AIMAP_VERSION);

		Fbox LevelBB;
		R_ASSERT(F->open_chunk(E_AIMAP_CHUNK_BOX));
		F->r(&LevelBB, sizeof(LevelBB));

		R_ASSERT(F->open_chunk(E_AIMAP_CHUNK_PARAMS));
		F->r(&g_params, sizeof(g_params));

		R_ASSERT(F->open_chunk(E_AIMAP_CHUNK_NODES));
		u32 N_ = F->r_u32();
		R_ASSERT2(N_ < MAX_AI_NODES - 1, "Too many nodes!");
		g_nodes.resize(N_);

		BuildAIMapVersion = version;

		hdrNODES H;
		H.version = XRAI_CURRENT_VERSION - (E_AIMAP_VERSION - version);
		H.count = N_ + 1;
		H.size = g_params.fPatchSize;
		H.size_y = 1.f;
		H.aabb = LevelBB;

		typedef u32 NodeLink;

		for (u32 i = 0; i < N_; i++)
		{
			NodeLink id{};

			if (version == 1)
			{
				for (int j = 0; j < 4; ++j)
				{
					F->r(&id, 3);
					id = id & 0x00ffffff;
					if (id == InvalidNode_v1)
						id = InvalidNode_v1;
					g_nodes[i].n[j] = id;
				}
			}
			else
			{
				for (int j = 0; j < 4; ++j)
				{
					F->r(&id, sizeof(NodeLink));
					g_nodes[i].n[j] = id;
				}
			}

			u16 pl = F->r_u16();
			NodePosition np{};
			SNodePositionOld _np;
			pvDecompress(g_nodes[i].Plane.n, pl);
			F->r(&_np, sizeof(_np));
			CNodePositionConverter(_np, H, np);
			g_nodes[i].Pos = vertex_position(np, LevelBB, g_params);
			g_nodes[i].Plane.build(g_nodes[i].Pos, g_nodes[i].Plane.n);
		}

		F->close();

		if (strstr(Core.Params, "-clear_temp_files"))
			DeleteFileA(file_name);
	}
}
