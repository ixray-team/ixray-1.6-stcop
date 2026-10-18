#include "StdAfx.h"
#include "compiler.h"

xr_vector<NodeCompressed> compressed_nodes;
xr_vector<NodeCompressed10> compressed_legacy_nodes;

IC u8 compress(float c, int max_value)
{
	int	cover = iFloor(c*float(max_value)+.5f);
	clamp(cover,0,max_value);
	return u8(cover);
}

template <class NodeClass>
IC void	compress_node(NodeClass& Dest, vertex& Src)
{
	for (u8 L = 0; L < 4; ++L)
		Dest.link(L, Src.n[L]);
}

template <class NodeClass>
void Compress(NodeClass& Dest, vertex& Src, hdrNODES& H)
{
	// Compress plane (normal)
	Dest.plane = pvCompress(Src.Plane.n);

	// Compress position
	CNodePositionCompressor(Dest.p, Src.Pos, H);

	// Light & Cover
	compress_node(Dest, Src);

	Dest.high.cover0 = compress(Src.high_cover[0], 15);
	Dest.high.cover1 = compress(Src.high_cover[1], 15);
	Dest.high.cover2 = compress(Src.high_cover[2], 15);
	Dest.high.cover3 = compress(Src.high_cover[3], 15);
	Dest.low.cover0  = compress(Src.low_cover[0], 15);
	Dest.low.cover1  = compress(Src.low_cover[1], 15);
	Dest.low.cover2  = compress(Src.low_cover[2], 15);
	Dest.low.cover3  = compress(Src.low_cover[3], 15);
}

float CalculateHeight(Fbox& BB)
{
	// All nodes
	BB.invalidate();

	for (u32 i=0; i<g_nodes.size(); i++)
	{
		vertex&	N	= g_nodes[i];
		BB.modify	(N.Pos);
	}
	return BB.max.y-BB.min.y+EPS_L;
}

template <class NodeClass>
class CNodeRenumberer
{
	xr_vector<NodeClass> &m_nodes;
	xr_vector<u32> &m_sorted;
	xr_vector<u32> &m_renumbering;

public:
	CNodeRenumberer(xr_vector<NodeClass>& nodes, xr_vector<u32>& sorted, xr_vector<u32>& renumbering) :
		m_nodes(nodes),
		m_sorted(sorted),
		m_renumbering(renumbering)
	{
		u32 N = (u32)m_nodes.size();
		m_sorted.resize(N);
		m_renumbering.resize(N);

		for (u32 i = 0; i < N; ++i)
		{
			m_sorted[i] = i;
		}

		std::stable_sort(m_sorted.begin(), m_sorted.end(), [this](u32 vertex_id0, u32 vertex_id1)
		{
			return (m_nodes[vertex_id0].p.xz() < m_nodes[vertex_id1].p.xz());
		});

		for (u32 i = 0; i < N; ++i)
		{
			m_renumbering[m_sorted[i]] = i;
		}

		for (u32 i = 0; i < N; ++i)
		{
			for (u32 j = 0; j < 4; ++j)
			{
				u32 vertex_id = m_nodes[i].link(u8(j));
				if (vertex_id >= N)
					continue;

				m_nodes[i].link(u8(j), m_renumbering[vertex_id]);
			}
		}

		std::stable_sort(m_nodes.begin(), m_nodes.end(), [](const NodeClass& vertex0, const NodeClass& vertex1)
		{
			return (vertex0.p.xz() < vertex1.p.xz());
		});
	}
};

template <class NodeClass>
void SaveNodesByVersion(hdrNODES& H, IWriter*& fs, xr_vector<NodeClass> OutNodes)
{
	Status("Saving nodes...");
	OutNodes.reserve(g_nodes.size());

	for (u32 i = 0; i < g_nodes.size(); ++i)
	{
		vertex& N_ = g_nodes[i];

		NodeClass NC;
		Compress(NC, N_, H);
		OutNodes.push_back(NC);
	}

	xr_vector<u32> sorted;
	xr_vector<u32> renumbering;
	CNodeRenumberer	A(OutNodes, sorted, renumbering);

	for (u32 i = 0; i < g_nodes.size(); ++i)
	{
		fs->w(&OutNodes[i], sizeof(NodeClass));
		Progress(float(i) / float(g_nodes.size()));
	}

	// Stats
	u32	SizeTotal = fs->tell();
	Msg("%dK saved", SizeTotal / 1024);

	FS.w_close(fs);

	OutNodes.clear();
}

void xrSaveNodes(const char* N, const char* out_name)
{
	Msg("Renumbering nodes...");

	string_path fName;
	xr_strconcat(fName, N, out_name);

	IWriter* fs = FS.w_open(fName);

	// Header
	Status("Saving header...");
	hdrNODES H;
	H.version = XRAI_CURRENT_VERSION - (E_AIMAP_VERSION - BuildAIMapVersion);
	H.count = g_nodes.size();
	H.size = g_params.fPatchSize;
	H.size_y = CalculateHeight(H.aabb);
	H.guid = generate_guid();
	fs->w(&H, sizeof(H));

	// All nodes
	if (BuildAIMapVersion == E_AIMAP_VERSION)
	{
		SaveNodesByVersion(H, fs, compressed_nodes);
	}
	else
	{
		SaveNodesByVersion(H, fs, compressed_legacy_nodes);
	}
}