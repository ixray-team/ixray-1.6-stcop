#include "stdafx.h"
#include "ESceneAIMapTools.h"

static inline bool VecIsNan(const Fvector& v)
{
	return _isnan(v.x) || _isnan(v.y) || _isnan(v.z);
}

static inline bool PlaneIsInvalid(const Fplane& p)
{
	return _isnan(p.n.x) || _isnan(p.n.y) || _isnan(p.n.z) || _isnan(p.d);
}

void ESceneAIMapTool::RemoveNodeImmediate(SAINode* Node)
{
	if (!Node)
	{
		return;
	}

	for (int Side = 0; Side < 4; ++Side)
	{
		SAINode* Neighbor = Node->n[Side];
		if (Neighbor)
		{
			for (int S = 0; S < 4; ++S)
			{
				if (Neighbor->n[S] == Node)
				{
					Neighbor->n[S] = nullptr;
				}
			}
		}
	}

	AINodeVec* Bucket = HashMap(Node->Pos);
	if (Bucket)
	{
		auto It = std::find(Bucket->begin(), Bucket->end(), Node);
		if (It != Bucket->end())
		{
			Bucket->erase(It);
		}
	}

	xr_delete(Node);
}

bool ESceneAIMapTool::IsNodeValidForCleanup(SAINode* Node) const
{
	if (!Node)
	{
		return false;
	}

	if (VecIsNan(Node->Pos))
	{
		return false;
	}

	if (PlaneIsInvalid(Node->Plane))
	{
		return false;
	}

	if (m_AIBBox.min.x <= m_AIBBox.max.x && m_AIBBox.min.y <= m_AIBBox.max.y && m_AIBBox.min.z <= m_AIBBox.max.z)
	{
		const float Eps = 0.01f;

		Fvector Min = m_AIBBox.min;
		Min.sub(Eps);

		Fvector Max = m_AIBBox.max;
		Max.add(Eps);

		const Fvector& P = Node->Pos;
		if (P.x < Min.x || P.y < Min.y || P.z < Min.z || P.x > Max.x || P.y > Max.y || P.z > Max.z)
		{
			return false;
		}
	}

	const float MaxCoord = 100000.f;
	if (fabsf(Node->Pos.x) > MaxCoord || fabsf(Node->Pos.y) > MaxCoord || fabsf(Node->Pos.z) > MaxCoord)
	{
		return false;
	}

	if (Node->Links() == 0)
	{
		return false;
	}

	if (!VecIsNan(Node->SavePos))
	{
		Fvector Delta;
		Delta.sub(Node->Pos, Node->SavePos);

		if (Delta.square_magnitude() > (100.f * 100.f))
		{
			return false;
		}
	}

	return true;
}

int ESceneAIMapTool::CleanupInvalidNodes()
{
	if (m_Nodes.empty())
	{
		return 0;
	}

	xr_vector<SAINode*> ToDelete;
	ToDelete.reserve(64);

	const float DupEpsSqr = (0.001f * 0.001f);

	for (size_t I = 0; I < m_Nodes.size(); ++I)
	{
		SAINode* N = m_Nodes[I];
		if (!IsNodeValidForCleanup(N))
		{
			ToDelete.push_back(N);
			continue;
		}

		for (size_t J = I + 1; J < m_Nodes.size(); ++J)
		{
			SAINode* M = m_Nodes[J];
			if (!M)
			{
				continue;
			}

			Fvector Diff;
			Diff.sub(N->Pos, M->Pos);

			if (Diff.square_magnitude() <= DupEpsSqr)
			{
				ToDelete.push_back(M);
			}
		}
	}

	if (ToDelete.empty())
	{
		return 0;
	}

	xr_vector<SAINode*> NewNodes;
	NewNodes.reserve(m_Nodes.size() - ToDelete.size());

	xr_hash_set<SAINode*> DelSet;
	DelSet.reserve(ToDelete.size() * 2);

	for (SAINode* N : ToDelete)
	{
		DelSet.insert(N);
	}

	for (SAINode* N : ToDelete)
	{
		if (!N)
		{
			continue;
		}

		for (int Side = 0; Side < 4; ++Side)
		{
			SAINode* Neighbor = N->n[Side];
			if (Neighbor)
			{
				for (int S = 0; S < 4; ++S)
				{
					if (Neighbor->n[S] == N)
					{
						Neighbor->n[S] = nullptr;
					}
				}
			}
		}
	}

	for (SAINode* N : m_Nodes)
	{
		if (DelSet.find(N) == DelSet.end())
		{
			NewNodes.push_back(N);
		}
		else
		{
			AINodeVec* Bucket = HashMap(N->Pos);
			if (Bucket)
			{
				auto It = std::find(Bucket->begin(), Bucket->end(), N);
				if (It != Bucket->end())
				{
					Bucket->erase(It);
				}
			}

			xr_delete(N);
		}
	}

	m_Nodes.swap(NewNodes);

	EnumerateNodes();
	hash_Clear();
	hash_FillFromNodes();

	for (SAINode* N : m_Nodes)
	{
		UpdateLinks(N, false);
	}

	Msg("AI: CleanupInvalidNodes removed %d nodes, left %d", (int)ToDelete.size(), (int)m_Nodes.size());

	return (int)ToDelete.size();
}
