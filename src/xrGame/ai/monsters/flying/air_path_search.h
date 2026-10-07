#pragma once
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <map>
#include <queue>
#include <tuple>
#include <vector>

// CPU-only aerial search. No terrain graph, renderer, object or species dependencies.
namespace FlyingPath
{
struct Point
{
    float x, y, z;
    float distance(const Point& other) const
    {
        const float dx = x - other.x, dy = y - other.y, dz = z - other.z;
        return std::sqrt(dx * dx + dy * dy + dz * dz);
    }
};
struct Bounds
{
    Point min, max;
    bool contains(const Point& p) const
    {
        return p.x >= min.x && p.x <= max.x && p.y >= min.y && p.y <= max.y &&
            p.z >= min.z && p.z <= max.z;
    }
};

class Search
{
public:
    enum class Status { Searching, Found, Failed };
    void begin(Point from, Point goal, Bounds bounds, float cell, unsigned limit)
    {
        m_nodes.clear();
        m_indices.clear();
        m_open = {};
        m_path.clear();
        m_origin = from;
        m_goal = goal;
        m_bounds = bounds;
        m_cell = cell;
        m_limit = limit;
        m_expanded = 0;
        m_status = Status::Searching;
        if (!(cell > 0.f) || !std::isfinite(cell) || !bounds.contains(from) || !bounds.contains(goal) || !limit)
        {
            m_status = Status::Failed;
            return;
        }
        m_indices.emplace(Key{0,0,0}, 0);
        m_nodes.push_back({{0,0,0}, 0.f, no_parent});
        m_open.push({from.distance(goal), 0.f, 0});
    }

    // Clearance verifies the whole body, including each graph edge and final connection.
	// At most seven clearance calls per budgeted expansion, including completion.
    template<class Clearance>
    Status step(unsigned budget, Clearance&& clear)
    {
        if (m_status != Status::Searching)
            return m_status;
        for (unsigned iteration = 0; iteration < budget; ++iteration)
        {
            if (m_open.empty() || m_expanded >= m_limit)
                return m_status = Status::Failed;
            const Entry entry = m_open.top();
            m_open.pop();
            if (m_nodes[entry.node].closed || entry.cost > m_nodes[entry.node].cost + .0001f)
                continue;
            m_nodes[entry.node].closed = true;
            ++m_expanded;
            const Key key = m_nodes[entry.node].key;
            const float cost = m_nodes[entry.node].cost;
            const Point from = point(key);
            if (clear(from, m_goal))
            {
				BuildCheckedPath(entry.node);
                return m_status = Status::Found;
            }
            static constexpr int offsets[][3] = {{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1},{0,0,-1}};
            for (const auto& offset : offsets)
            {
                const Key neighbour{std::get<0>(key) + offset[0],
                    std::get<1>(key) + offset[1], std::get<2>(key) + offset[2]};
                const Point to = point(neighbour);
                if (!m_bounds.contains(to))
                    continue;
                auto found = m_indices.find(neighbour);
                if (found != m_indices.end() && m_nodes[found->second].closed)
                    continue;
                const float next_cost = cost + m_cell;
                if (found != m_indices.end() && next_cost >= m_nodes[found->second].cost)
                    continue;
                if (!clear(from, to))
                    continue;
                std::size_t index;
                if (found == m_indices.end())
                {
                    if (m_nodes.size() >= std::size_t(m_limit) * 4u)
                        continue;
                    index = m_nodes.size();
                    m_indices.emplace(neighbour, index);
                    m_nodes.push_back({neighbour, next_cost, entry.node});
                }
                else
                {
                    index = found->second;
                    m_nodes[index].cost = next_cost;
                    m_nodes[index].parent = entry.node;
                }
                m_open.push({next_cost + to.distance(m_goal), next_cost, index});
            }
        }
        return m_status;
    }
    const std::vector<Point>& path() const { return m_path; }
    unsigned expanded() const { return m_expanded; }

private:
    using Key = std::tuple<int,int,int>;
    static constexpr std::size_t no_parent = std::numeric_limits<std::size_t>::max();
    struct Node { Key key; float cost; std::size_t parent; bool closed = false; };
    struct Entry
    {
        float score, cost;
        std::size_t node;
        bool operator<(const Entry& other) const { return score > other.score; }
    };
	void BuildCheckedPath(std::size_t NodeIndex)
	{
		m_path.clear();
		Key PreviousDirection{};
		bool HasDirection = false;
		while (m_nodes[NodeIndex].parent != no_parent)
		{
			const auto& Current = m_nodes[NodeIndex];
			const auto& Parent = m_nodes[Current.parent];
			const Key Direction{std::get<0>(Current.key) - std::get<0>(Parent.key),
				std::get<1>(Current.key) - std::get<1>(Parent.key), std::get<2>(Current.key) - std::get<2>(Parent.key)};
			// Consecutive steps along the same grid axis form a union of already
			// checked edges. Preserve every corner; no clearance rescan is needed.
			if (!HasDirection || Direction != PreviousDirection)
			{
				m_path.push_back(point(Current.key));
			}
			PreviousDirection = Direction;
			HasDirection = true;
			NodeIndex = Current.parent;
		}
		std::reverse(m_path.begin(), m_path.end());
		if (m_path.empty() || m_path.back().x != m_goal.x || m_path.back().y != m_goal.y || m_path.back().z != m_goal.z)
		{
			// The final connection was checked by step() before reconstruction.
			m_path.push_back(m_goal);
		}
	}
    Point point(const Key& key) const
    {
        return {m_origin.x + std::get<0>(key) * m_cell,
            m_origin.y + std::get<1>(key) * m_cell, m_origin.z + std::get<2>(key) * m_cell};
    }
    Point m_origin{}, m_goal{};
    Bounds m_bounds{};
    float m_cell = 2.f;
    unsigned m_limit = 0, m_expanded = 0;
    Status m_status = Status::Failed;
    std::map<Key, std::size_t> m_indices;
    std::vector<Node> m_nodes;
    std::priority_queue<Entry> m_open;
    std::vector<Point> m_path;
};
}
