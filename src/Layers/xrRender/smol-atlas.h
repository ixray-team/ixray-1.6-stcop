// SPDX-License-Identifier: MIT OR Unlicense
// smol-atlas: https://github.com/aras-p/smol-atlas

#pragma once

// 2D rectangular bin packing utility that uses the Shelf Best Height Fit
// heuristic, and supports item removal. You could also call it a
// dynamic texture atlas allocator.
//
// The shelf packing algorithm works well when there is a high number of items
// with similar sizes, for example dynamic glyph or thumbnail atlases.
//
// - Incoming items are placed into horizontal "shelves" based on item heights.
// - Within each shelf, there is a sorted list of free spans.
// - When an item is added, needed portion of the first suitable free span
//   is used.
// - When an item is removed, resulting free span is joined with any
//   neighboring spans.
// - Shelves, once created, stay at their height and location. Even if they
//   become empty, they are not removed nor joined with nearby shelves.
//
// Implementation uses STL <vector>, and some manual memory allocation
// with just regular `new` and `delete`. Custom allocators might be nice to
// do someday.
//
// At least C++11 is required.

struct smol_atlas_t;
struct smol_atlas_item_t;

/// Create atlas of given size.
smol_atlas_t* sma_atlas_create(int width, int height);

/// Destroy the atlas.
void sma_atlas_destroy(smol_atlas_t* atlas);

/// Get atlas width.
int sma_atlas_width(const smol_atlas_t* atlas);

/// Get atlas height.
int sma_atlas_height(const smol_atlas_t* atlas);

/// Add an item of (width x height) size into the atlas.
/// Use `sma_item_x` and `sma_item_y` to query the resulting item location.
/// Item can later be removed with `sma_item_remove`.
/// Returned pointer is valid until atlas is cleared or destroyed, or the item is removed.
/// Returns NULL if there is no more space left.
smol_atlas_item_t* sma_item_add(smol_atlas_t* atlas, int width, int height);

/// Remove a previously added item from the atlas.
/// The item pointer becomes invalid and can no longer be used.
void sma_item_remove(smol_atlas_t* atlas, smol_atlas_item_t* item);

/// Clear the atlas. This invalidates any previously returned item pointers.
/// If passed width and height are positive, the atlas size is also set
/// to the new values.
void sma_atlas_clear(smol_atlas_t* atlas, int new_width = 0, int new_height = 0);

/// Get item X coordinate.
int sma_item_x(const smol_atlas_item_t* item);
/// Get item Y coordinate.
int sma_item_y(const smol_atlas_item_t* item);
/// Get item width.
int sma_item_width(const smol_atlas_item_t* item);
/// Get item height.
int sma_item_height(const smol_atlas_item_t* item);
