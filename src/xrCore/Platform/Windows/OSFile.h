#pragma once

class FS_Path;

namespace Platform
{
	bool OpenFileWnd(char* buffer, size_t sz_buf, FS_Path* P, int start_flt_ext, char flt[1024], LPCSTR offset, bool bMulti);
}