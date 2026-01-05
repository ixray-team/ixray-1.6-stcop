#pragma once

template <typename P>
concept IterateAdjacentsParams = requires(P p)
{
	typename P::type_vertex;
	typename P::type_face;

	{ p.current_adjacents_size() } -> std::convertible_to<std::size_t>;
	{ p.add_adjacents(std::declval<u32>(), std::declval<u32>()) } -> std::same_as<bool>;
};

template <IterateAdjacentsParams Params>
class itterate_adjacents
{
public:
	using recurse_tri_params = Params;

	static void RecurseTri(u32 start_face_idx, recurse_tri_params& p)
	{
		for (u32 test_face_idx = 0; test_face_idx < p.current_adjacents_size(); ++test_face_idx)
		{
			if (p.add_adjacents(start_face_idx, test_face_idx))
				RecurseTri(test_face_idx, p);
		}
	}
};