#pragma once

class xrDecompressor
{
    static const char s_Separators[4];
    const char *m_OutDir;

    tbb::concurrent_unordered_map<xr_string, xr_string> m_PathCache;
    virtual int ExtractFile(const char *filename);
    virtual const char * CreatePath(const char *path);
    virtual int CreateDir(const char *base, const char *path);
public:
    xrDecompressor(const char *);
    virtual ~xrDecompressor() = default;
    virtual void Decompress();
};