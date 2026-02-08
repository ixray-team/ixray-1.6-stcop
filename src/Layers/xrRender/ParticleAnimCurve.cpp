#include "stdafx.h"
#include "ParticleAnimCurve.h"

#ifdef _EDITOR
#include "Editor/UI_ToolsCustom.h"
#endif

PS::CPACDef::~CPACDef()
{
    for (auto elem : m_Keys)
    {
        xr_delete(elem);
    }
    m_Keys.clear();
}

void PS::CPACDef::Save(IWriter& F)
{
    F.open_chunk(PS::PAC::Chunks::VERSION);
    F.w_enum(PS::PAC::Version::Latest);
    F.close_chunk();

    F.open_chunk(PS::PAC::Chunks::NAME);
    F.w_stringZ(m_Name);
    F.close_chunk();

    F.open_chunk(PS::PAC::Chunks::TIME_MAX);
    F.w_float(m_MaxTime);
    F.close_chunk();

    auto Size = m_Keys.size();
    F.open_chunk(PS::PAC::Chunks::KEYS_NUM);
    F.w_u32(Size);
    F.close_chunk();

    F.open_chunk(PS::PAC::Chunks::KEYS);
    for (int i = 0; i < Size; i++)
    {
        auto Key = m_Keys[i];
        F.open_chunk(i);
        F.w_float(Key->time);
        F.w_fvector4(Key->value);
        F.close_chunk();
    }
    F.close_chunk();
}

bool PS::CPACDef::Load(IReader& F)
{
    R_ASSERT(m_Keys.empty());
    
    bool FoundedChunk = F.find_chunk(PS::PAC::Chunks::VERSION);
    R_ASSERT2(FoundedChunk, "Not found chunk PED_CHUNK_VERSION");

    auto version = F.r_enum<PS::PAC::Version>();
    switch (version)
    {
    case PS::PAC::Version::Original:
        {
            FoundedChunk = F.find_chunk(PS::PAC::Chunks::NAME);
            R_ASSERT2(FoundedChunk, "Not found chunk PED_CHUNK_NAME");
            F.r_stringZ(m_Name);

            FoundedChunk = F.find_chunk(PS::PAC::Chunks::TIME_MAX);
            R_ASSERT2(FoundedChunk, "Not found chunk PED_CHUNK_TIME_MAX");
            m_MaxTime = F.r_float();

            FoundedChunk = F.find_chunk(PS::PAC::Chunks::KEYS_NUM);
            R_ASSERT2(FoundedChunk, "Not found chunk PED_CHUNK_KEYS_NUM");
            auto Size = F.r_u32();

            auto KeysChunk = F.open_chunk(PS::PAC::Chunks::KEYS);
            R_ASSERT2(KeysChunk, "Not found chunk PED_CHUNK_KEYS");
            for (int i = 0; i < Size; i++)
            {
                auto KeyChunk = KeysChunk->open_chunk(i);
                R_ASSERT3(KeyChunk, "Not found key chunk in chunk PED_CHUNK_KEYS", std::to_string(i).c_str());
                m_Keys.push_back(new st_PACKey);
                auto Key = m_Keys.back();
                Key->time = KeyChunk->r_float();
                KeyChunk->r_fvector4(Key->value);
                m_MinValue = std::min(
                    m_MinValue,
                    std::min(
                        Key->value.x,
                        std::min(
                            Key->value.y,
                            std::min(
                                Key->value.z,
                                Key->value.w))));
                m_MaxValue = std::max(
                    m_MaxValue,
                    std::max(
                        Key->value.x,
                        std::max(
                            Key->value.y,
                            std::max(
                                Key->value.z,
                                Key->value.w))));
                KeyChunk->close();
            }
            KeysChunk->close();
            
            break;
        }
    default:
        {
            return false;
        }
    }
    return true;
}

void PS::CPACDef::Save2(CInifile& ini)
{
    ini.w_enum("_anim_curve", "version", PS::PAC::Version::Original);
    ini.w_float("_anim_curve", "time_max", m_MaxTime);
    ini.w_string("_anim_curve", "name", m_Name.c_str());
    auto Size = m_Keys.size();
    ini.w_u64("_anim_curve", "keys_num", Size);
    for (int i = 0; i < Size; i++)
    {
        auto key = m_Keys[i];
        string16		sect;
        xr_sprintf		(sect, sizeof(sect), "key_%04d", i);
        ini.w_float(sect, "time", key->time);
        ini.w_fvector4(sect, "keys", key->value);
    }
}

bool PS::CPACDef::Load2(CInifile& ini)
{
    auto version = ini.r_enum<PS::PAC::Version>("_anim_curve", "version");
    switch (version)
    {
    case PAC::Version::Original:
        {
            m_MaxTime = ini.r_float("_anim_curve", "time_max");
            m_Name = ini.r_string("_anim_curve", "name");
            auto Size = ini.r_u64("_anim_curve", "keys_num");
            R_ASSERT(m_Keys.empty());
            for (int i = 0; i < Size; i++)
            {
                m_Keys.push_back(new st_PACKey);
                auto key = m_Keys.back();
                string16		sect;
                xr_sprintf		(sect, sizeof(sect), "key_%04d", i);
                key->time = ini.r_float(sect, "time");
                key->value = ini.r_fvector4(sect, "keys");
                m_MinValue = std::min(
                    m_MinValue,
                    std::min(
                        key->value.x,
                        std::min(
                            key->value.y,
                            std::min(
                                key->value.z,
                                key->value.w))));
                m_MaxValue = std::max(
                    m_MaxValue,
                    std::max(
                        key->value.x,
                        std::max(
                            key->value.y,
                            std::max(
                                key->value.z,
                                key->value.w))));
            }
            break;
        }
    default:
        {
            return false;
        }
    }
    return true;
}

Fvector4 PS::CPACDef::GetValueOnIndex(int index)
{
    R_ASSERT(index >= 0 && index < m_Keys.size());
    return m_Keys[index]->value;
}

Fvector4 PS::CPACDef::GetValueOnTime(float time)
{
    R_ASSERT(time >= 0 && time <= m_MaxTime);
    auto cmp = [&](float key_a, float key_b)
    {
        return key_a < key_b;
    };
    auto proj = [&](st_PACKey* key)
    {
        return key->time;
    };
    R_ASSERT(!m_Keys.empty());
    if (time <= m_Keys[0]->time || time >= m_Keys[m_Keys.size() - 1]->time)
    {
        return CalculateIntermedialeValue(m_Keys[m_Keys.size() - 1], m_Keys[0], time);
    }
#ifdef _EDITOR
    for (int i = 1; i < m_Keys.size(); i++)
    {
        auto key1 = m_Keys[i-1];
        auto key2 = m_Keys[i];
        I_ASSERT_M(key1->time < key2->time, "Particle Anim Curve %s contains invalid sorted by time keys!", m_Name.c_str());
    }
#endif
    // if we here, we already know this PAC has at least 2 keys, and we are somewhere between start and end
    auto FoundIt = std::ranges::lower_bound(m_Keys, time, cmp, proj);
    VERIFY(FoundIt != m_Keys.begin() || FoundIt == m_Keys.end());
    return CalculateIntermedialeValue(*std::prev(FoundIt), *FoundIt, time);
}

Fvector4 PS::CPACDef::CalculateIntermedialeValue(st_PACKey* A, st_PACKey* B, float time)
{
    auto Atime = A->time;
    auto Btime = B->time;
    if (Atime > Btime)
    {
        Btime = m_MaxTime;
    }
    if (time < Atime)
    {
        time = m_MaxTime - time;
    }
    auto Alpha = (time - Atime)/(Btime - Atime);
    VERIFY(Alpha >= 0.0f && Alpha <= 1.0f);
    Fvector4 InterValue;
    auto ProcessFunc = [&](int index)
    {
        return A->value[index] + (B->value[index] - A->value[index])*Alpha;
    };
    for (int i = 0; i < 4; ++i)
    {
        InterValue[i] = ProcessFunc(i);
    }
    return InterValue;
}

Fvector4 PS::CPACDef::FastUpdateValue(size_t& CurrentIndex, float& CurrentTime, float dt, bool Loop, bool Reverse)
{
    R_ASSERT(m_Keys.size() > 1);
    st_PACKey* LowerKey = nullptr;
    st_PACKey* UpperKey = nullptr;
    if (Reverse)
    {
        CurrentTime -= dt;
        if (Loop)
        {
            while (CurrentTime < 0)
            {
                CurrentTime += m_MaxTime;
                CurrentIndex = m_Keys.size() - 1;
            }
        } else
        {
            if (CurrentTime < 0)
            {
                CurrentTime = 0;
                CurrentIndex = 0;
                return m_Keys[0]->value;
            }
        }
        auto NextIndex = CurrentIndex - 1;
        while (m_Keys[NextIndex]->time > CurrentTime)
        {
            --CurrentIndex;
            --NextIndex;
        }
        UpperKey = m_Keys[CurrentIndex];
        LowerKey = m_Keys[NextIndex];
    } else
    {
        CurrentTime += dt;
        if (Loop)
        {
            while (CurrentTime > m_MaxTime)
            {
                CurrentTime -= m_MaxTime;
                CurrentIndex = 0;
            }
        } else
        {
            if (CurrentTime > m_MaxTime)
            {
                CurrentTime = m_MaxTime;
                CurrentIndex = m_Keys.size() - 1;
                return m_Keys[CurrentIndex]->value;
            }
        }
        auto NextIndex = CurrentIndex + 1;
        while (m_Keys[NextIndex]->time < CurrentTime)
        {
            ++CurrentIndex;
            ++NextIndex;
        }
        LowerKey = m_Keys[CurrentIndex];
        UpperKey = m_Keys[NextIndex];
    }
    return CalculateIntermedialeValue(LowerKey, UpperKey, CurrentTime);
}

void PS::CPACDef::setName(LPCSTR name)
{
    m_Name = name;
}

#ifdef _EDITOR
void PS::CPACDef::Clone(PS::CPACDef* source)
{
    m_Name = "<invalid_name>";
    m_MaxTime = source->m_MaxTime;

    m_Keys.resize(source->m_Keys.size(), nullptr);
    for (auto d_it=m_Keys.begin(),s_it=source->m_Keys.begin(); s_it!=source->m_Keys.end(); s_it++,d_it++)
    {
        *d_it = new st_PACKey(**s_it);
    }
}

void PS::CPACDef::FillProp(LPCSTR pref, PropItemVec& items, void* owner)
{
    PHelper().CreateName(items,PrepareKey(pref,"Name"),&m_Name,(::ListItem*)owner);
    // TODO: Add disabled
    PHelper().CreateFloat(items, PrepareKey(pref,"Time Max"), &m_MaxTime, 0, 1000000);
    auto B = PHelper().CreateButton(items, PrepareKey(pref, "Edit"), "Edit",ButtonValue::flFirstOnly);
    B->OnBtnClickEvent.bind(this, &CPACDef::OnEditClicked);
}

void PS::CPACDef::OnEditClicked(ButtonValue* B, bool& bModif, bool& bSafe)
{
    Tools->EditPAC(this);
}

bool PS::CPACDef::Validate(bool bMsg)
{
    bool failed = false;
    if (m_Keys.size() < 2 && bMsg)
    {
        Msg("Validation FAILED (invalid keys num) anim curve [%s]", m_Name.c_str());
        failed = true;
    }
    return !failed;
}

void PS::CPACDef::SplitKeysForPlot(
            xr_vector<float>& R_keys_y,
            xr_vector<float>& G_keys_y,
            xr_vector<float>& B_keys_y,
            xr_vector<float>& A_keys_y,
            xr_vector<float>& keys_x
        )
{
    R_keys_y.resize(m_Keys.size(), 0);
    G_keys_y.resize(m_Keys.size(), 0);
    B_keys_y.resize(m_Keys.size(), 0);
    A_keys_y.resize(m_Keys.size(), 0);
    keys_x.resize(m_Keys.size(), 0);

    for (size_t i = 0; i < m_Keys.size(); i++)
    {
        keys_x[i] = m_Keys[i]->time;
        R_keys_y[i] = m_Keys[i]->value.x;
        G_keys_y[i] = m_Keys[i]->value.y;
        B_keys_y[i] = m_Keys[i]->value.z;
        A_keys_y[i] = m_Keys[i]->value.w;
    }
}

void PS::CPACDef::UpdateCurveFromKeys(const xr_vector<float>& R_keys_y, const xr_vector<float>& G_keys_y,
    const xr_vector<float>& B_keys_y, const xr_vector<float>& A_keys_y, const xr_vector<float>& keys_x)
{
    R_ASSERT(
        R_keys_y.size() == keys_x.size() &&
        G_keys_y.size() == keys_x.size() &&
        B_keys_y.size() == keys_x.size() &&
        A_keys_y.size() == keys_x.size()
        );
    for (auto elem : m_Keys)
    {
        xr_delete(elem);
    }
    auto StartTime = keys_x[0];
    m_Keys.resize(keys_x.size(), nullptr);
    for (size_t i = 0; i < keys_x.size(); i++)
    {
        m_Keys[i] = new st_PACKey;
        m_Keys[i]->time = keys_x[i]-StartTime;
        m_Keys[i]->value.x = R_keys_y[i];
        m_Keys[i]->value.y = G_keys_y[i];
        m_Keys[i]->value.z = B_keys_y[i];
        m_Keys[i]->value.w = A_keys_y[i];
    }
    m_MaxTime = m_Keys.back()->time;
}
#endif
