#pragma once

template <typename TP>
std::time_t xr_chrono_to_time_t(TP tp)
{
	using namespace std::chrono;
	auto sctp = time_point_cast<system_clock::duration>(tp - TP::clock::now() + system_clock::now());

	return system_clock::to_time_t(sctp);
}