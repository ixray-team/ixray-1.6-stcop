#include "stdafx.h"
#include "level_graph_editor.h"


const u32 UnkonnectedNode = 0xfffffff0;
const WORD	InvalidSector = 0xff;
const float	cover_distance = 30.f;
const float high_cover_height = 1.5f;
const float low_cover_height = 0.6f;
const float cover_sqr_dist = cover_distance * cover_distance;
static SAIParams g_params;



IC void CNodePositionCompressor(LevelGraph::CPosition& Pdest, Fvector& Psrc, hdrNODES& H)
{
	float sp = 1 / g_params.fPatchSize;
	int row_length = iFloor((H.aabb.max.z - H.aabb.min.z) / H.size + EPS_L + 1.5f);
	int pxz = iFloor((Psrc.x - H.aabb.min.x) * sp + EPS_L + .5f) * row_length + iFloor((Psrc.z - H.aabb.min.z) * sp + EPS_L + .5f);
	int py = iFloor(65535.f * (Psrc.y - H.aabb.min.y) / (H.size_y) + EPS_L);
	VERIFY(pxz < MAX_AI_NODES);
	Pdest.xz(pxz);
	clamp(py, 0, 65535);	Pdest.y(u16(py));
}
IC void	compress_node(LevelGraph::CVertex& Dest, SAINode* Src)
{
	for (u8 L = 0; L < 4; ++L)
		Dest.link(L, Src->n[L] ? Src->n[L]->idx : InvalidNode);
}
IC BYTE	compress(float c, int max_value)
{
	int	cover = iFloor(c * float(max_value) + .5f);
	clamp(cover, 0, max_value);
	return BYTE(cover);
}

void Compress(LevelGraph::CVertex& Dest, SAINode* Src, hdrNODES& H)
{
	// Compress plane (normal)
	Dest.UncompressedNode.plane = pvCompress(Src->Plane.n);

	// Compress position
	CNodePositionCompressor(Dest.UncompressedNode.p, Src->Pos, H);

	compress_node(Dest, Src);
	Dest.UncompressedNode.high.cover0 = compress(high_cover_height, 15);
	Dest.UncompressedNode.high.cover1 = compress(high_cover_height, 15);
	Dest.UncompressedNode.high.cover2 = compress(high_cover_height, 15);
	Dest.UncompressedNode.high.cover3 = compress(high_cover_height, 15);
	Dest.UncompressedNode.low.cover0 = compress(low_cover_height, 15);
	Dest.UncompressedNode.low.cover1 = compress(low_cover_height, 15);
	Dest.UncompressedNode.low.cover2 = compress(low_cover_height, 15);
	Dest.UncompressedNode.low.cover3 = compress(low_cover_height, 15);
}

class CNodeRenumberer
{


public:
	CNodeRenumberer(
		xr_vector<CLevelGraphEditor::CVertex>& nodes,
		xr_vector<u32>& sorted,
		xr_vector<u32>& renumbering
	)
	{
		u32					N = (u32)nodes.size();
		sorted.resize(N);
		renumbering.resize(N);

		for (u32 i = 0; i < N; ++i)
			sorted[i] = i;

		std::stable_sort(sorted.begin(), sorted.end(), [&nodes](u32 vertex_id0, u32 vertex_id1)
		{
			return (nodes[vertex_id0].UncompressedNode.p.xz() < nodes[vertex_id1].UncompressedNode.p.xz());
		});

		for (u32 i = 0; i < N; ++i)
			renumbering[sorted[i]] = i;

		for (u32 i = 0; i < N; ++i) {
			for (u32 j = 0; j < 4; ++j) {
				u32			vertex_id = nodes[i].link(u8(j));
				if (vertex_id >= N)
					continue;
				nodes[i].link(u8(j), renumbering[vertex_id]);
			}
		}

		std::stable_sort(nodes.begin(), nodes.end(), [](const LevelGraph::CVertex& vertex0, const LevelGraph::CVertex& vertex1)
			{
				return		(vertex0.UncompressedNode.p.xz() < vertex1.UncompressedNode.p.xz());
			});
	}
};

CLevelGraphEditor::CLevelGraphEditor()
{
	m_header = &m_RealHeader;
}

CLevelGraphEditor::~CLevelGraphEditor()
{
}

bool CLevelGraphEditor::build()
{
	ESceneAIMapTool* AIMapTool = smart_cast<ESceneAIMapTool*>(Scene->GetTool(OBJCLASS_AIMAP));
	g_params = AIMapTool->AIParams();
	if (AIMapTool->Nodes().size() == 0)
	{
		Msg("! AI-Map is empty!");
		return false;
	}

	AIMapTool->EnumerateNodes();

	size_t Index = 0;
	auto CalculateHeight = [&AIMapTool](Fbox& BB)->float
	{
		// All nodes
		BB.invalidate();

		for (u32 i = 0; i < AIMapTool->Nodes().size(); i++)
		{
			BB.modify(AIMapTool->Nodes()[i]->Pos);
		}
		return BB.max.y - BB.min.y + EPS_L;
	};
	hdrNODES*RealHeader = (hdrNODES*)&m_RealHeader;
	AIMapTool->CalculateNodesBBox(RealHeader->aabb);
	RealHeader->version = XRAI_CURRENT_VERSION;

	RealHeader->count = AIMapTool->Nodes().size();
	RealHeader->size = g_params.fPatchSize;
	RealHeader->size_y = CalculateHeight(RealHeader->aabb);
	RealHeader->guid = generate_guid();
	m_RealNodes.erase(m_RealNodes.begin(), m_RealNodes.end());
	m_RealNodes.reserve(AIMapTool->Nodes().size());
	for (SAINode* Node : AIMapTool->Nodes())
	{
		CVertex	NC;
		Compress(NC, Node, *RealHeader);
		m_RealNodes.push_back(NC);
	}
	xr_vector<u32>	sorted;
	xr_vector<u32>	renumbering;
	CNodeRenumberer	A(m_RealNodes, sorted, renumbering);
	m_nodes = m_RealNodes.data();

	m_row_length = iFloor((header().box().max.z - header().box().min.z) / header().cell_size() + EPS_L + 1.5f);
	m_column_length = iFloor((header().box().max.x - header().box().min.x) / header().cell_size() + EPS_L + 1.5f);
	m_access_mask.assign(header().vertex_count(), true);
	unpack_xz(vertex_position(header().box().max), m_max_x, m_max_z);
	m_level_id = -1;
	return true;
}

bool CLevelGraphEditor::empty() const
{
	return m_RealNodes.empty();
}

void CLevelGraphEditor::clear()
{
	m_RealNodes.clear();
}

bool CLevelGraphEditor::save_temp()
{

	string_path FileName;
	xr_strcpy(FileName, Scene->m_LevelOp.m_FNLevelPath.c_str());
	xr_strcat(FileName, "\\level.ai.temp");
	FS.update_path(FileName, _game_levels_, FileName);
	IWriter* fs = FS.w_open(FileName);
	if (!fs)
		return false;

	fs->w(&m_RealHeader, sizeof(m_RealHeader));
	for (size_t i = 0; i < m_RealNodes.size(); i++)
	{
		fs->w(&m_RealNodes[i], sizeof(NodeCompressed));
	}
	FS.w_close(fs);
	return true;
}
