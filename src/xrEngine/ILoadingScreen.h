#pragma once

class ILoadingScreen {
public:
    virtual ~ILoadingScreen() = default;

    virtual void Initialize() = 0;

    virtual void Update(int stagesCompleted, int stagesTotal) = 0;
    virtual void ForceFinish() = 0;

    virtual void SetLevelLogo(const char* name) const = 0;
    virtual void SetStageTitle(const char* title) const = 0;
    virtual void SetStageTip(const char* header, const char* tipNumber, const char* tip) const = 0;
};