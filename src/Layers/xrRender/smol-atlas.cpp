// SPDX-License-Identifier: MIT OR Unlicense
// smol-atlas: https://github.com/aras-p/smol-atlas

#include "stdafx.h"
#include "smol-atlas.h"

#include <assert.h>
#include <stddef.h>
#include <vector>
#include <type_traits>

// "memory pool" that allocates chunks of same size items,
// and maintains a freelist of items for O(1) alloc and free.
template <typename T>
struct smol_pool_t
{
    static_assert(std::is_trivially_destructible_v<T>, "smol_pool_t type must be trivially destructible");

    union item_t
    {
        item_t* next;
        alignas(alignof(T)) char data[sizeof(T)];
    };

    struct chunk_t
    {
        item_t* storage = nullptr;
        chunk_t* next = nullptr;
        size_t m_size_in_items;
        chunk_t(size_t size_in_items) : storage(new item_t[size_in_items]), m_size_in_items(size_in_items)
        {
            link_all_items(nullptr);
        }
        ~chunk_t() { delete[] storage; }

        void link_all_items(item_t* last_item_next_ptr)
        {
            for (size_t i = 1; i < m_size_in_items; ++i)
                storage[i - 1].next = &storage[i];
            storage[m_size_in_items - 1].next = last_item_next_ptr;
        }
    };

    size_t m_chunk_size_in_items;
    chunk_t* m_cur_chunk = nullptr;
    item_t* m_free_list = nullptr;

    smol_pool_t(size_t size_in_items)
        : m_chunk_size_in_items(size_in_items)
        , m_cur_chunk(new chunk_t(size_in_items))
        , m_free_list(m_cur_chunk->storage)
    {
    }
    ~smol_pool_t()
    {
        chunk_t* chunk = m_cur_chunk;
        while (chunk) {
            chunk_t* next = chunk->next;
            delete chunk;
            chunk = next;
        }
    }

    // makes all items "unused", but keeps the allocated space
    void clear()
    {
        chunk_t* chunk = m_cur_chunk;
        while (chunk) {
            chunk_t* next = chunk->next;
            chunk->link_all_items(next ? next->storage : nullptr);
            chunk = next;
        }
        m_free_list = m_cur_chunk ? m_cur_chunk->storage : nullptr;
    }

    template <typename... Args> T* alloc(Args &&... args)
    {
        // create a new chunk if current one is full
        if (m_free_list == nullptr) {
            chunk_t* new_chunk = new chunk_t(m_chunk_size_in_items);
            new_chunk->next = m_cur_chunk;
            m_cur_chunk = new_chunk;
            m_free_list = m_cur_chunk->storage;
        }

        // grab item from free list
        item_t* item = m_free_list;
        m_free_list = item->next;

        // construct the object
        T* res = reinterpret_cast<T*>(item->data);
        new (res) T(std::forward<Args>(args)...);
        return res;
    }

    void free(T* ptr)
    {
        // add item to the free list
        item_t* item = reinterpret_cast<item_t*>(ptr);
        item->next = m_free_list;
        m_free_list = item;
    }
};

struct smol_atlas_item_t
{
    explicit smol_atlas_item_t(int x_, int y_, int w_, int h_, int shelf_)
    : x(x_), y(y_), width(w_), height(h_), shelf_index(shelf_)
    {
    }

    int x;
    int y;
    int width;
    int height;
    int shelf_index;
};

struct smol_free_span_t
{
    explicit smol_free_span_t(int x_, int w_) : x(x_), width(w_), next(nullptr) {}

    int x;
    int width;
    smol_free_span_t* next;
};

template<typename T>
struct smol_single_list_t
{
    smol_single_list_t(T* h) : m_head(h) { }
    
    void insert(T* prev, T* node)
    {
        if (prev == nullptr) { // this is first node
            node->next = m_head;
            m_head = node;
        }
        else {
            node->next = prev->next;
            prev->next = node;
        }
    }

    void remove(T* prev, T* node)
    {
        if (prev)
            prev->next = node->next;
        else
            m_head = node->next;
    }

    T* m_head = nullptr;
};

struct smol_shelf_t
{
    explicit smol_shelf_t(int y, int width, int height, int index, smol_pool_t<smol_free_span_t>& span_pool)
        : m_y(y), m_height(height), m_index(index), m_free_spans(span_pool.alloc(0, width))
    {
    }

    bool has_space_for(int width) const
    {
        smol_free_span_t* it = m_free_spans.m_head;
        while (it != nullptr) {
            if (width <= it->width)
                return true;
            it = it->next;
        }
        return false;
    }

    smol_atlas_item_t* alloc_item(int w, int h, smol_pool_t<smol_atlas_item_t>& item_pool, smol_pool_t<smol_free_span_t>& span_pool)
    {
        if (h > m_height)
            return nullptr;

        // find a suitable free span
        smol_free_span_t* it = m_free_spans.m_head;
        smol_free_span_t* prev = nullptr;
        while (it != nullptr) {
            if (it->width >= w) {
                break;
            }
            prev = it;
            it = it->next;
        }

        // no space in this shelf
        if (it == nullptr)
            return nullptr;

        const int x = it->x;
        const int rest = it->width - w;
        if (rest > 0) {
            // there will be still space left in this span, adjust
            it->x += w;
            it->width -= w;
        }
        else {
            // whole span is taken, remove it
            m_free_spans.remove(prev, it);
            span_pool.free(it);
        }

        return item_pool.alloc(x, m_y, w, h, m_index);
    }

    void add_free_span(int x, int width, smol_pool_t<smol_free_span_t>& span_pool)
    {
        // insert into free spans list at the right position
        smol_free_span_t* free_e = span_pool.alloc(x, width);
        smol_free_span_t* it = m_free_spans.m_head;
        smol_free_span_t* prev = nullptr;
        bool added = false;

        while (it != nullptr)
        {
            if (free_e->x < it->x)
            {
                // found right place, insert into the list
                m_free_spans.insert(prev, free_e);
                added = true;
                break;
            }

            prev = it;
            it = it->next;
        }

        if (!added)
        {
            m_free_spans.insert(prev, free_e);
        }

        merge_free_spans(prev, free_e, span_pool);
    }

    void free_item(smol_atlas_item_t* e, smol_pool_t<smol_atlas_item_t>& item_pool, smol_pool_t<smol_free_span_t>& span_pool)
    {
        assert(e);
        assert(e->shelf_index == m_index);
        assert(e->y == m_y);
        add_free_span(e->x, e->width, span_pool);
        item_pool.free(e);
    }

    void merge_free_spans(smol_free_span_t* prev, smol_free_span_t* span, smol_pool_t<smol_free_span_t>& span_pool)
    {
        smol_free_span_t* next = span->next;
        if (next != nullptr && span->x + span->width == next->x) {
            // merge with next
            span->width += next->width;
            m_free_spans.remove(span, next);
            span_pool.free(next);
        }
        if (prev != nullptr && prev->x + prev->width == span->x) {
            // merge with prev
            prev->width += span->width;
            m_free_spans.remove(prev, span);
            span_pool.free(span);
        }
    }

    smol_single_list_t<smol_free_span_t> m_free_spans;
    const int m_y;
    const int m_height;
    const int m_index;
};

struct smol_atlas_t
{
    explicit smol_atlas_t(int w, int h)
        : m_item_pool(1024), m_span_pool(1024)
    {
        m_shelves.reserve(8);
        m_width = w > 0 ? w : 64;
        m_height = h > 0 ? h : 64;
    }
    
    ~smol_atlas_t()
    {
        clear();
    }

    smol_atlas_item_t* pack(int w, int h)
    {
        // find best shelf
        smol_shelf_t* best_shelf = nullptr;
        int best_score = 0x7fffffff;

        int top_y = 0;
        for (auto& shelf : m_shelves)
        {
            const int shelf_h = shelf.m_height;
            top_y = std::max(top_y, shelf.m_y + shelf_h);
            
            if (shelf_h < h)
                continue; // too short
            
            if (shelf_h == h)
            { // exact height fit, try to use it
                smol_atlas_item_t* res = shelf.alloc_item(w, h, m_item_pool, m_span_pool);
                if (res != nullptr)
                    return res;
            }

            // otherwise the shelf is too tall, track best one
            int score = shelf_h - h;
            if (score < best_score && shelf.has_space_for(w))
            {
                best_score = score;
                best_shelf = &shelf;
            }
        }

        if (best_shelf) 
        {
            smol_atlas_item_t* res = best_shelf->alloc_item(w, h, m_item_pool, m_span_pool);
            if (res != nullptr)
                return res;
        }

        // no shelf with enough space: add a new shelf
        if (w <= m_width && h <= m_height - top_y) {
            int shelf_index = int(m_shelves.size());
            m_shelves.emplace_back(top_y, m_width, h, shelf_index, m_span_pool);
            return m_shelves.back().alloc_item(w, h, m_item_pool, m_span_pool);
        }

        // out of space
        return nullptr;
    }

    void free_item(smol_atlas_item_t* item)
    {
        if (item == nullptr)
            return;
        assert(item->shelf_index >= 0 && item->shelf_index < m_shelves.size());
        m_shelves[item->shelf_index].free_item(item, m_item_pool, m_span_pool);
    }

    void clear()
    {
        m_item_pool.clear();
        m_span_pool.clear();
        m_shelves.clear();
    }

    smol_pool_t<smol_atlas_item_t> m_item_pool;
    smol_pool_t<smol_free_span_t> m_span_pool;
    xr_vector<smol_shelf_t> m_shelves;

    int m_width;
    int m_height;
};

smol_atlas_t* sma_atlas_create(int width, int height)
{
    return new smol_atlas_t(width, height);
}

void sma_atlas_destroy(smol_atlas_t* atlas)
{
    delete atlas;
}

int sma_atlas_width(const smol_atlas_t* atlas)
{
    return atlas->m_width;
}

int sma_atlas_height(const smol_atlas_t* atlas)
{
    return atlas->m_height;
}

smol_atlas_item_t* sma_item_add(smol_atlas_t* atlas, int width, int height)
{
    return atlas->pack(width, height);
}

void sma_item_remove(smol_atlas_t* atlas, smol_atlas_item_t* item)
{
    atlas->free_item(item);
}

void sma_atlas_clear(smol_atlas_t* atlas, int new_width, int new_height)
{
    atlas->clear();
    if (new_width > 0) atlas->m_width = new_width;
    if (new_height > 0) atlas->m_height = new_height;
}

int sma_item_x(const smol_atlas_item_t* item)
{
    return item->x;
}

int sma_item_y(const smol_atlas_item_t* item)
{
    return item->y;
}

int sma_item_width(const smol_atlas_item_t* item)
{
    return item->width;
}

int sma_item_height(const smol_atlas_item_t* item)
{
    return item->height;
}