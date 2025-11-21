#pragma once

/*
*
* Author: wh1t3lord
* Description: 2d r-tree spatial contianer with pmr based allocation policies
*
*/

#ifndef RTREE2D_PMR_NOREC_HPP
#define RTREE2D_PMR_NOREC_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <limits>
#include <memory_resource>
#include <queue>
#include <vector>
#include <bit>

namespace rtree2d {

	inline constexpr size_t calculate_reserve_count(size_t bytes, size_t amount)
	{
#ifndef DEBUG
		return std::bit_ceil(bytes * amount);
#else
		return std::bit_ceil(bytes * amount) * 2;
#endif
	}

	//----------------------------------------------------------------------------
	// 1) Basic geometric types: 2D point and axis-aligned rectangle (Rect).
	//----------------------------------------------------------------------------

	struct Point {
		float x, y;
	};

	struct Rect {
		float minx, miny;
		float maxx, maxy;

		// Create a degenerate rectangle around a single point:
		static Rect from_point(const Point& p) noexcept {
			return { p.x, p.y, p.x, p.y };
		}

		// Create an “empty” rectangle that can expand to include any point:
		static Rect infinite_negative() noexcept {
			float inf = std::numeric_limits<float>::infinity();
			return { +inf, +inf, -inf, -inf };
		}

		// Compute this rectangle’s area (zero if invalid):
		float area() const noexcept {
			float w = (maxx > minx) ? (maxx - minx) : 0.0f;
			float h = (maxy > miny) ? (maxy - miny) : 0.0f;
			return w * h;
		}

		// Return the bounding rectangle that encloses both `a` and `b`.
		static Rect unite(const Rect& a, const Rect& b) noexcept {
			return {
				std::min(a.minx, b.minx),
				std::min(a.miny, b.miny),
				std::max(a.maxx, b.maxx),
				std::max(a.maxy, b.maxy)
			};
		}

		// How much area enlargement is needed to include `r`?
		float enlargement_needed(const Rect& r) const noexcept {
			Rect u = unite(*this, r);
			return u.area() - area();
		}

		// Squared distance from point p to this rectangle (0 if p is inside).
		float distance2(const Point& p) const noexcept {
			float dx = 0.0f;
			if (p.x < minx) dx = minx - p.x;
			else if (p.x > maxx) dx = p.x - maxx;
			float dy = 0.0f;
			if (p.y < miny) dy = miny - p.y;
			else if (p.y > maxy) dy = p.y - maxy;
			return dx * dx + dy * dy;
		}
	};

	//----------------------------------------------------------------------------
	// 2) R-Tree with PMR-backed, preallocated nodes, and iterative algorithms.
	//----------------------------------------------------------------------------

	template <typename Value, std::size_t MaxEntries = 4, std::size_t PreallocNodes = 32>
	class RTree {
		struct Node;
		struct Entry {
			Point   pt;     // valid if leaf-entry
			Node* child;  // valid if internal-entry, nullptr if leaf
			Rect    box;
			Value   value;  // valid if leaf-entry


			// Leaf-entry constructor:
			Entry(const Point& p, const Value& v)
				: pt(p), child(nullptr), box(Rect::from_point(p)), value(v) {
			}

			// Internal-entry constructor from a child Node*:
			explicit Entry(Node* c)
				: pt(Point{ 0,0 }), child(c), box(c->mbr), value() {
			}
		};

		struct Node {
			bool                is_leaf;
			Node* parent;
			Rect                mbr;      // MBR of all entries


			std::pmr::monotonic_buffer_resource pool_;
			std::pmr::polymorphic_allocator<Entry> allocator_;
			std::pmr::vector<Entry>  entries;  // up to MaxEntries+1 while splitting
			unsigned char buffer_[calculate_reserve_count(sizeof(Entry), MaxEntries)];

			Node(bool leaf, Node* parent_)
				: is_leaf(leaf), parent(parent_), mbr(Rect::infinite_negative()), pool_{ &buffer_, sizeof(buffer_), std::pmr::null_memory_resource() }, allocator_{ &pool_ }, entries{ allocator_ } {
				entries.reserve(MaxEntries);
			}

			Node(const Node&) = delete; // No copying of nodes
			Node& operator=(const Node&) = delete; // No assignment of nodes

			Node(Node&& other) noexcept
				: is_leaf(other.is_leaf), parent(other.parent), mbr(other.mbr), pool_{ &buffer_, sizeof(buffer_), std::pmr::null_memory_resource() }, allocator_{ &pool_ }, entries{ allocator_ } {
				entries.reserve(MaxEntries);

				for (auto& entry : other.entries) {
					entries.push_back(std::move(entry));
				}

				other.entries.clear();
				other.parent = nullptr; // Prevent double deletion
			}

			Node& operator=(Node&& other) noexcept {
				if (this != &other) {
					is_leaf = other.is_leaf;
					parent = other.parent;
					mbr = other.mbr;
					entries.clear();
					for (auto& entry : other.entries) {
						entries.push_back(std::move(entry));
					}
					other.entries.clear();
					other.parent = nullptr; // Prevent double deletion
				}
				return *this;
			}
		};
	public:
		static_assert(MaxEntries >= 2, "MaxEntries must be ≥ 2");
		static_assert(PreallocNodes >= 2, "PreallocNodes must be ≥ 2");

		// Public interface:

		RTree() : pool_{ &buffer_, sizeof(buffer_), std::pmr::get_default_resource() }, alloc_{ &pool_ }, nodes_{ alloc_ } {
			nodes_.reserve(PreallocNodes);

			// Allocate the root node (as a leaf):
			root_ = allocate_node(/*is_leaf=*/true, /*parent=*/nullptr);
		}
		
		RTree(const RTree& other) : pool_{ &buffer_, sizeof(buffer_), std::pmr::get_default_resource() }, alloc_{ &pool_ }, nodes_{alloc_}
		{
			nodes_.reserve(PreallocNodes);
			root_ = allocate_node(true, nullptr);
			// Copy entries from other tree
			for (const auto& node : other.nodes_)
			{
				for (const auto& entry : node.entries)
				{
					insert(entry.pt, entry.value);
				}
			}
		}
		RTree& operator=(const RTree& other)
		{
			if (this != &other ) {
				// Clear current tree
				nodes_.clear();
				root_ = allocate_node(true, nullptr);
				// Copy entries from other tree
				for (const auto& node : other.nodes_)
				{
					for (const auto& entry : node.entries)
					{
						insert(entry.pt, entry.value);
					}
				}
			}
			return *this;
		}
		RTree& operator=(RTree&& other) noexcept 
		{
			if (this != &other) {
				root_ = allocate_node(true, nullptr);

				for (auto& node : other.nodes_)
				{
					for (auto& entry : node.entries)
					{
						insert(entry.pt, entry.value);
					}
					node.entries.clear();
				}

				other.nodes_.clear();

				other.root_ = nullptr;
			}

			return *this;
		}

		RTree(RTree&& other) noexcept : pool_{ &buffer_, sizeof(buffer_), std::pmr::get_default_resource() }, alloc_{ &pool_ }, nodes_{ alloc_ } {
			nodes_.reserve(PreallocNodes);

			root_ = allocate_node(true, nullptr);

			for (auto& node : other.nodes_)
			{
				for (auto& entry : node.entries)
				{
					insert(entry.pt, entry.value);
				}
				node.entries.clear();
			}

			other.nodes_.clear();
			other.root_ = nullptr;
		}

		~RTree() = default; // nodes_ will be destroyed automatically

		const std::pmr::vector<Node>& get_nodes(void) const { return this->nodes_; }

		// Insert (point → value) into the R-Tree (O(log N) amortized).
		void insert(const Point& p, const Value& value) {
			Entry new_entry(p, value);

			// Choose a leaf:
			Node* leaf = choose_leaf(root_, new_entry);

			// Insert into leaf:
			leaf->entries.push_back(std::move(new_entry));
			if (leaf->entries.size() == 1) {
				leaf->mbr = leaf->entries[0].box;
			}
			else {
				leaf->mbr = Rect::unite(leaf->mbr, leaf->entries.back().box);
			}

			// If overflow, split and adjust upward:
			if (leaf->entries.size() > MaxEntries) {
				Node* sibling = split_node(leaf);
				adjust_tree_iterative(leaf, sibling);
			}
			else {
				// Expand ancestors:
				adjust_mbr_upward_iterative(leaf->parent, leaf->mbr);
			}
		}

		// Find the nearest neighbor's value; returns nullptr if empty.
		Value* nearest(const Point& q) const {
			if (!root_ || root_->entries.empty()) return nullptr;

			// Use a min-heap to explore the tree in best-first order:
			struct Candidate {
				Node* node;
				float   dist2;
			};
			struct Cmp {
				bool operator()(Candidate const& a, Candidate const& b) const {
					return a.dist2 > b.dist2; // min-heap
				}
			};

			unsigned char temp[calculate_reserve_count(sizeof(Candidate), PreallocNodes)];
			std::pmr::monotonic_buffer_resource resource{ &temp, sizeof(temp), std::pmr::get_default_resource() };
			std::pmr::polymorphic_allocator<Candidate> allocator_{ &resource };
			std::pmr::vector<Candidate> _buffer{ allocator_ };
			_buffer.reserve(PreallocNodes);

			std::priority_queue<Candidate, std::pmr::vector<Candidate>, Cmp> pq{Cmp(), std::move(_buffer)};
			// Start from root: distance from q to root MBR is 0 if q inside, else boundary dist.
			pq.push({ root_, root_->mbr.distance2(q) });

			float best_d2 = std::numeric_limits<float>::infinity();
			Value* best_val = nullptr;

			while (!pq.empty()) {
				auto [node, node_d2] = pq.top(); pq.pop();
				if (node_d2 >= best_d2) {
					// Any further candidates cannot improve
					break;
				}
				if (node->is_leaf) {
					for (auto const& e : node->entries) {
						float dx = q.x - e.pt.x;
						float dy = q.y - e.pt.y;
						float d2 = dx * dx + dy * dy;
						if (d2 < best_d2) {
							best_d2 = d2;
							best_val = const_cast<Value*>(&e.value);
						}
					}
				}
				else {
					for (auto const& e : node->entries) {
						float child_d2 = e.box.distance2(q);
						if (child_d2 < best_d2) {
							pq.push({ e.child, child_d2 });
						}
					}
				}
			}
			return best_val;
		}

		// Range-search: return pointers to all values whose points lie within query_rect.
		std::vector<Value*> range_search(const Rect& query_rect) const {
			std::vector<Value*> results;
			// Use a stack to avoid recursion:
			std::vector<Node*> stack;
			stack.reserve(64);
			stack.push_back(root_);

			while (!stack.empty()) {
				Node* node = stack.back();
				stack.pop_back();
				if (!node) continue;
				if (!rects_intersect(node->mbr, query_rect)) continue;

				if (node->is_leaf) {
					for (auto const& e : node->entries) {
						if (point_in_rect(e.pt, query_rect)) {
							results.push_back(const_cast<Value*>(&e.value));
						}
					}
				}
				else {
					for (auto const& e : node->entries) {
						if (rects_intersect(e.box, query_rect)) {
							stack.push_back(e.child);
						}
					}
				}
			}
			return results;
		}

	private:

		//-------------------------------------------------------------------------------
		// 2.2) PMR and preallocation
		//-------------------------------------------------------------------------------

		alignas(alignof(Node)) unsigned char buffer_[calculate_reserve_count(sizeof(Node), PreallocNodes)];

		std::pmr::monotonic_buffer_resource pool_;
		std::pmr::polymorphic_allocator<Node> alloc_;
		std::pmr::vector<Node> nodes_;
		Node* root_ = nullptr;

		Node* allocate_node(bool is_leaf, Node* parent) {
			nodes_.emplace_back(is_leaf, parent);
			return &nodes_.back();
		}

		//-------------------------------------------------------------------------------
		// 3) Insertion helpers: choose_leaf, split_node, adjust_tree_iterative, adjust_mbr_upward_iterative
		//-------------------------------------------------------------------------------

		Node* choose_leaf(Node* current, const Entry& e) {
			while (!current->is_leaf) {
				float best_inc = std::numeric_limits<float>::infinity();
				Node* best_child = nullptr;
				for (auto& child_entry : current->entries) {
					float inc = child_entry.box.enlargement_needed(e.box);
					if (inc < best_inc) {
						best_inc = inc;
						best_child = child_entry.child;
					}
					else if (inc == best_inc) {
						float a1 = child_entry.box.area();
						float a2 = child_entry.child->mbr.area();
						if (a2 < a1) {
							best_child = child_entry.child;
						}
					}
				}
				assert(best_child);
				current = best_child;
			}
			return current;
		}

		Node* split_node(Node* node) {
			// Gather all entries:
			unsigned char _buffer_entries[calculate_reserve_count(sizeof(Entry), MaxEntries)];
			std::pmr::monotonic_buffer_resource _resource{ &_buffer_entries, sizeof(_buffer_entries), std::pmr::null_memory_resource()};
			std::pmr::polymorphic_allocator<Entry> _allocator_entries{ &_resource };

			std::pmr::vector<Entry> all{ _allocator_entries };
			all.reserve(node->entries.size());
			for (auto& e : node->entries) {
				all.push_back(std::move(e));
			}
			node->entries.clear();

			std::size_t N = all.size(); // = MaxEntries + 1
			std::size_t seed1 = 0, seed2 = 1;

			{   // Choose seeds
				struct Center { float cx, cy; };

				unsigned char _buffer_centers[calculate_reserve_count(sizeof(Center), MaxEntries)];
				std::pmr::monotonic_buffer_resource _resource_centers{ &_buffer_centers, sizeof(_buffer_centers), std::pmr::null_memory_resource()};
				std::pmr::polymorphic_allocator<Center> _allocator_centers{ &_resource_centers };

				std::pmr::vector<Center> centers{_allocator_centers};
				centers.reserve(N);
				Rect overall = Rect::infinite_negative();
				for (std::size_t i = 0; i < N; ++i) {
					centers[i].cx = (all[i].box.minx + all[i].box.maxx) * 0.5f;
					centers[i].cy = (all[i].box.miny + all[i].box.maxy) * 0.5f;
					overall = Rect::unite(overall, all[i].box);
				}
				float width_x = overall.maxx - overall.minx;
				float width_y = overall.maxy - overall.miny;

				float min_cx = centers[0].cx, max_cx = centers[0].cx;
				float min_cy = centers[0].cy, max_cy = centers[0].cy;
				std::size_t ix_min = 0, ix_max = 0, iy_min = 0, iy_max = 0;

				for (std::size_t i = 1; i < N; ++i) {
					if (centers[i].cx < min_cx) { min_cx = centers[i].cx; ix_min = i; }
					if (centers[i].cx > max_cx) { max_cx = centers[i].cx; ix_max = i; }
					if (centers[i].cy < min_cy) { min_cy = centers[i].cy; iy_min = i; }
					if (centers[i].cy > max_cy) { max_cy = centers[i].cy; iy_max = i; }
				}

				float sep_x = (width_x > 0.0f) ? ((max_cx - min_cx) / width_x) : 0.0f;
				float sep_y = (width_y > 0.0f) ? ((max_cy - min_cy) / width_y) : 0.0f;
				if (sep_x >= sep_y) {
					seed1 = ix_min; seed2 = ix_max;
				}
				else {
					seed1 = iy_min; seed2 = iy_max;
				}
				if (seed1 == seed2) {
					seed2 = (seed1 + 1) % N;
				}
			}

			Node* sibling = allocate_node(node->is_leaf, node->parent);

			if (seed1 < seed2) std::swap(seed1, seed2);
			node->entries.push_back(std::move(all[seed1]));
			sibling->entries.push_back(std::move(all[seed2]));
			all.erase(all.begin() + seed1);
			all.erase(all.begin() + seed2);

			node->mbr = node->entries.front().box;
			sibling->mbr = sibling->entries.front().box;

			std::size_t min_fill = (MaxEntries + 1) / 2;
			while (!all.empty()) {
				std::size_t remain = all.size();
				std::size_t c1 = node->entries.size();
				std::size_t c2 = sibling->entries.size();

				if (c1 + remain == min_fill) {
					for (auto& e : all) {
						node->entries.push_back(std::move(e));
						node->mbr = Rect::unite(node->mbr, node->entries.back().box);
					}
					break;
				}
				if (c2 + remain == min_fill) {
					for (auto& e : all) {
						sibling->entries.push_back(std::move(e));
						sibling->mbr = Rect::unite(sibling->mbr, sibling->entries.back().box);
					}
					break;
				}

				float best_diff = -std::numeric_limits<float>::infinity();
				std::size_t idx_best = 0;
				bool assign_to_node = true;
				for (std::size_t i = 0; i < remain; ++i) {
					Rect const& r = all[i].box;
					float inc1 = node->mbr.enlargement_needed(r);
					float inc2 = sibling->mbr.enlargement_needed(r);
					float diff = std::fabs(inc1 - inc2);
					if (diff > best_diff) {
						best_diff = diff;
						idx_best = i;
						assign_to_node = (inc1 < inc2);
					}
				}

				Entry e = std::move(all[idx_best]);
				all.erase(all.begin() + idx_best);
				if (assign_to_node) {
					node->entries.push_back(std::move(e));
					node->mbr = Rect::unite(node->mbr, node->entries.back().box);
				}
				else {
					sibling->entries.push_back(std::move(e));
					sibling->mbr = Rect::unite(sibling->mbr, sibling->entries.back().box);
				}
			}

			if (!node->is_leaf) {
				for (auto& e : node->entries) {
					e.child->parent = node;
				}
				for (auto& e : sibling->entries) {
					e.child->parent = sibling;
				}
			}

			return sibling;
		}

		void adjust_tree_iterative(Node* node, Node* sibling) {
			Node* n = node;
			Node* s = sibling;
			while (true) {
				if (n == root_) {
					// Create new root
					Node* new_root = allocate_node(/*is_leaf=*/false, /*parent=*/nullptr);
					n->parent = new_root;
					s->parent = new_root;
					new_root->entries.emplace_back(n);
					new_root->entries.emplace_back(s);
					new_root->mbr = Rect::unite(n->mbr, s->mbr);
					root_ = new_root;
					return;
				}
				Node* parent = n->parent;
				// Update parent's entry for n:
				for (auto& e : parent->entries) {
					if (e.child == n) {
						e.box = n->mbr;
						break;
					}
				}
				// Insert new entry for s:
				parent->entries.emplace_back(s);
				s->parent = parent;
				parent->mbr = Rect::unite(parent->mbr, s->mbr);

				if (parent->entries.size() > MaxEntries) {
					Node* parent_sib = split_node(parent);
					n = parent;
					s = parent_sib;
					continue; // loop upward
				}
				else {
					adjust_mbr_upward_iterative(parent->parent, parent->mbr);
					return;
				}
			}
		}

		void adjust_mbr_upward_iterative(Node* node, const Rect& child_mbr) {
			Node* n = node;
			Rect cm = child_mbr;
			while (n) {
				Rect unified = Rect::unite(n->mbr, cm);
				if (unified.minx == n->mbr.minx && unified.miny == n->mbr.miny
					&& unified.maxx == n->mbr.maxx && unified.maxy == n->mbr.maxy) {
					break;
				}
				n->mbr = unified;
				n = n->parent;
			}
		}

		//-------------------------------------------------------------------------------
		// 4) Helpers for range and nearest
		//-------------------------------------------------------------------------------

		static bool rects_intersect(const Rect& a, const Rect& b) noexcept {
			return !(a.maxx < b.minx || a.minx > b.maxx
				|| a.maxy < b.miny || a.miny > b.maxy);
		}

		static bool point_in_rect(const Point& p, const Rect& r) noexcept {
			return (p.x >= r.minx && p.x <= r.maxx
				&& p.y >= r.miny && p.y <= r.maxy);
		}
	};

} // namespace rtree2d

template<typename Type, std::size_t MaxEntries, std::size_t PreallocatedNodeAmount>
using xr_rtree2d = rtree2d::RTree<Type, MaxEntries, PreallocatedNodeAmount>;

#endif // RTREE2D_PMR_NOREC_HPP
