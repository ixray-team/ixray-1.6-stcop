// separated to avoid unnecessary inclusion of scrambler code.
#include "xr_reader.h"
#include "xr_lzhuf.h"
#include "xr_scrambler.h"

using namespace xray_re;

xr_reader* xr_reader::open_chunk(uint32_t id, const xr_scrambler& scrambler)
{
	bool compressed;
	size_t size = find_chunk(id, &compressed);
	if (size == 0)
		return 0;
	if (compressed) {
		uint8_t* temp = new uint8_t[size];
		scrambler.decrypt(temp, m_p, size);
		uint8_t* data;
		size_t real_size;
		xr_lzhuf::decompress(data, real_size, temp, size);
		delete[] temp;
		return new xr_temp_reader(data, real_size);
	} else {
		return new xr_reader(m_p, size);
	}
}
