#pragma once

struct SLFSPattern
{
    xr_string Pattern;
    bool Enabled = true;
    xr_string Description;
};

class CGitLFSConfig final
{
public:
    static CGitLFSConfig& Instance();

    void Load();
    void Save();

    void AddPattern(const xr_string& Pattern, const xr_string& Description = "");
    void RemovePattern(const xr_string& Pattern);
    void SetPatternEnabled(const xr_string& Pattern, bool Enabled);

    bool ShouldTrackWithLFS(const xr_string& FilePath) const;

    void LoadDefaultPatterns();
    bool MatchesPattern(const xr_string& FilePath, const xr_string& Pattern) const;

    xr_vector<SLFSPattern> Patterns;
    bool AutoTrackEnabled = true;
};
