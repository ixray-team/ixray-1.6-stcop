#pragma once
#include "object_interfaces.h"
#include "../xrCore/_stl_extensions.h"

enum EBinderParamType {
    eBinderParamString = 0,
    eBinderParamBool,
    eBinderParamDouble,
    eBinderParamInvalid
};

class CBinderParam : public IPureSerializeObject<IReader, IWriter> {
    EBinderParamType type = eBinderParamInvalid;
    using types = xr_variant<xr_string, bool, double>;
    types value;

    void DestroyValue();

public:
    CBinderParam(){}
    CBinderParam(const CBinderParam& other);
    CBinderParam(LPCSTR TypeString);
    CBinderParam(bool TypeBool);
    CBinderParam(double TypeDouble);

    ~CBinderParam(){
        DestroyValue();
    }

    CBinderParam& operator=(const CBinderParam& other);

    EBinderParamType GetType() const;
    void SetString(LPCSTR value);
    void SetBool(bool value);
    void SetDouble(double value);
    LPCSTR GetString() const;
    bool GetBool() const;
    double GetDouble() const;

    void save(IWriter& output_packet) override;
    void load(IReader& input_packet) override;
    virtual void Serialize(ISaveObject& Object);
};

ISaveObject& operator<<(ISaveObject& Object, CBinderParam& Value);

class CBinderParams : public IPureSerializeObject<IReader, IWriter> {
    xr_vector<CBinderParam> params = {};

public:
    CBinderParams();
    CBinderParams(const CBinderParams& other);
    CBinderParams(CBinderParams&& other) noexcept;

    CBinderParams& operator=(const CBinderParams& other);

    void Add(const CBinderParam& other);
    void Insert(int Index, const CBinderParam& other);
    void Remove(int Index);
    const CBinderParam& Get(int Index);
    int Size();

    void save(IWriter& output_packet) override;
    void load(IReader& input_packet) override;
    virtual void Serialize(ISaveObject& Object);
};

ISaveObject& operator<<(ISaveObject& Object, CBinderParams& Value);

class CBinderHandler : public IPureSerializeObject<IReader, IWriter>
{
    int m_timer_id;

public:
    CBinderHandler() = default;
    CBinderHandler(int id) : m_timer_id(id){}

    int GetID() const {return m_timer_id;}
    
    void save(IWriter& output_packet) override;
    void load(IReader& input_packet) override;
    void Serialize(ISaveObject& Object);
};

class CBinder : public IPureSerializeObject<IReader, IWriter>
{
    int m_id;
    int m_iTimerStartValue = 0;
    int m_iTimerCurValue = 0;
    u32 m_iStartTime = 0;
    struct Func
    {
        shared_str m_sFuncName = "";
        CBinderParams m_params;
    };
    xr_variant<Func, luabind::object> m_value;
    bool m_expired = false;
    bool m_bIsActive = true;
    bool m_looped = false;

protected:
    void OnTimerEnd();

public:
    CBinder() {}
    CBinder(int id, shared_str name, const CBinderParams& params, int value, bool looped) : m_id(id), m_iTimerStartValue(value), m_looped(looped)
    {
        m_value = Func{name, params};
    }
    CBinder(int id, luabind::object func, int value, bool looped) : m_id(id), m_iTimerStartValue(value), m_looped(looped)
    {
        m_value = func;
    }

    bool IsSaveable();
    
    void Pause() {m_bIsActive = false;}
    void Resume() {m_bIsActive = true;}
    void Stop() {m_bIsActive = false; m_expired = true; m_looped = false;}

    void save(IWriter& output_packet) override;
    void load(IReader& input_packet) override;
    void Serialize(ISaveObject& Object);

    bool getExpired() const { return m_expired; }
    bool getLooped() const { return m_looped; }
    int getId() const { return m_id; }

    void ResetTimer() {m_iTimerCurValue = 0; m_expired = false;}

    void Update();
};

ISaveObject& operator<<(ISaveObject& Object, CBinder& Value);

class CBinderManager
{
    xr_vector<xr_unique_ptr<CBinder>> Binders;
    xr_atomic_s32 m_id_gen = 0;
    bool LoadFinished = false;

    CBinderManager(){}

public:
    CBinderManager(const CBinderManager& other) = delete;
    CBinderManager(CBinderManager&& other) = delete;
    CBinderManager& operator=(const CBinderManager& other) = delete;
    CBinderManager& operator=(CBinderManager&& other) = delete;

    static CBinderManager& GetInstance();

    CBinderHandler CreateBinder(shared_str name, const CBinderParams& params, int value, bool looped = false);
    CBinderHandler CreateBinder(luabind::object func, int value, bool looped = false);

    bool IsTimerValid(CBinderHandler handler);
    void Pause(CBinderHandler handler);
    void Resume(CBinderHandler handler);
    void Stop(CBinderHandler handler);

    void save(IWriter& output_packet);
    void load(IReader& input_packet);
    virtual void Serialize(ISaveObject& Object);

    void Update();
    IC void NotifyLoadFinished() {LoadFinished = true;}
};