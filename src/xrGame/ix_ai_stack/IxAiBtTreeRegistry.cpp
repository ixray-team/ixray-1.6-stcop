#include "StdAfx.h"

#include "../../xrCore/Xr_ini.h"
#include "../../xrCore/_std_extensions.h"
#include "../../xrCore/xrSyncronize.h"
#include "IxAiBtNodes.h"
#include "IxAiBtTreeRegistry.h"

bool IxAiBtValidatePreorderSpec(const xr_vector<IxAiBtSpecNode>& specs);

static xrCriticalSection g_ixAiBtTreeRegistryCs{};
static bool g_ixAiBtTreeRegistryInitialized{};
static xr_vector<IxAiBtSpecNode> g_specsGuardBasic{};
static xr_vector<IxAiBtSpecNode> g_specsFlankerLite{};

static constexpr LPCSTR kSectionGuardBasic = "ix_ai_bt_tree_guard_basic";
static constexpr LPCSTR kSectionFlankerLite = "ix_ai_bt_tree_flanker_lite";

namespace IxAiBtTreeDetail
{
const char* SkipSpaces(const char* p)
{
    while (*p == ' ' || *p == '\t' || *p == '\r')
    {
        ++p;
    }

    return p;
}

void CopyToken(const char*& p, char* buf, size_t bufSize)
{
    p = SkipSpaces(p);
    size_t i = 0;

    while (*p && *p != ',' && i + 1 < bufSize)
    {
        buf[i++] = *p++;
    }

    buf[i] = 0;

    if (*p == ',')
    {
        ++p;
    }
}

bool ParseU32Token(const char* token, u32& outValue)
{
    if (token[0] == 0)
    {
        return false;
    }

    u32 v = 0;

    for (const char* q = token; *q; ++q)
    {
        if (*q < '0' || *q > '9')
        {
            return false;
        }

        v = v * 10u + (u32)(*q - '0');

        if (v > 255u)
        {
            return false;
        }
    }

    outValue = v;
    return true;
}

bool ParseAlertLevelToken(const char* token, IxAiAlertLevel& outLevel)
{
    if (_stricmp(token, "vigilant") == 0)
    {
        outLevel = IxAiAlertLevel::Vigilant;
        return true;
    }

    if (_stricmp(token, "suspicious") == 0)
    {
        outLevel = IxAiAlertLevel::Suspicious;
        return true;
    }

    if (_stricmp(token, "search") == 0)
    {
        outLevel = IxAiAlertLevel::Search;
        return true;
    }

    if (_stricmp(token, "combat") == 0)
    {
        outLevel = IxAiAlertLevel::Combat;
        return true;
    }

    return false;
}

bool ParseBehaviourTreeSpecLine(const char* line, IxAiBtSpecNode& outNode)
{
    char tokenKind[64];
    char tokenArg[64];
    const char* p = line;

    CopyToken(p, tokenKind, sizeof(tokenKind));

    if (tokenKind[0] == 0)
    {
        return false;
    }

    if (_stricmp(tokenKind, "selector") == 0)
    {
        CopyToken(p, tokenArg, sizeof(tokenArg));
        u32 arity = 0;

        if (!ParseU32Token(tokenArg, arity) || arity == 0u || arity > 32u)
        {
            return false;
        }

        outNode._kind = IxAiBtSpecNodeKind::Selector;
        outNode._childArity = (u8)arity;
        return SkipSpaces(p)[0] == 0;
    }

    if (_stricmp(tokenKind, "sequence") == 0)
    {
        CopyToken(p, tokenArg, sizeof(tokenArg));
        u32 arity = 0;

        if (!ParseU32Token(tokenArg, arity) || arity == 0u || arity > 32u)
        {
            return false;
        }

        outNode._kind = IxAiBtSpecNodeKind::Sequence;
        outNode._childArity = (u8)arity;
        return SkipSpaces(p)[0] == 0;
    }

    if (_stricmp(tokenKind, "condition_alert_gte") == 0)
    {
        CopyToken(p, tokenArg, sizeof(tokenArg));
        IxAiAlertLevel lvl{};

        if (!ParseAlertLevelToken(tokenArg, lvl))
        {
            return false;
        }

        outNode._kind = IxAiBtSpecNodeKind::ConditionAlertGte;
        outNode._childArity = 0;
        outNode._threshold = lvl;
        return SkipSpaces(p)[0] == 0;
    }

    if (_stricmp(tokenKind, "condition_alert_lt") == 0)
    {
        CopyToken(p, tokenArg, sizeof(tokenArg));
        IxAiAlertLevel lvl{};

        if (!ParseAlertLevelToken(tokenArg, lvl))
        {
            return false;
        }

        outNode._kind = IxAiBtSpecNodeKind::ConditionAlertLt;
        outNode._childArity = 0;
        outNode._threshold = lvl;
        return SkipSpaces(p)[0] == 0;
    }

    if (_stricmp(tokenKind, "action_noop") == 0)
    {
        outNode._kind = IxAiBtSpecNodeKind::ActionNoop;
        outNode._childArity = 0;
        return SkipSpaces(p)[0] == 0;
    }

    if (_stricmp(tokenKind, "action_clear_tactical_hint") == 0)
    {
        outNode._kind = IxAiBtSpecNodeKind::ActionClearTacticalHint;
        outNode._childArity = 0;
        return SkipSpaces(p)[0] == 0;
    }

    if (_stricmp(tokenKind, "action_apply_strongest_memory_hint") == 0)
    {
        outNode._kind = IxAiBtSpecNodeKind::ActionApplyStrongestMemoryHint;
        outNode._childArity = 0;
        return SkipSpaces(p)[0] == 0;
    }

    if (_stricmp(tokenKind, "condition_has_investigate_memory") == 0)
    {
        outNode._kind = IxAiBtSpecNodeKind::ConditionHasInvestigateMemory;
        outNode._childArity = 0;
        return SkipSpaces(p)[0] == 0;
    }

    if (_stricmp(tokenKind, "action_apply_investigate_memory_hint") == 0)
    {
        outNode._kind = IxAiBtSpecNodeKind::ActionApplyInvestigateMemoryHint;
        outNode._childArity = 0;
        return SkipSpaces(p)[0] == 0;
    }

    return false;
}

bool ValidatePreorderWalk(const xr_vector<IxAiBtSpecNode>& specs, size_t& index)
{
    if (index >= specs.size())
    {
        return false;
    }

    const IxAiBtSpecNode& node = specs[index++];

    switch (node._kind)
    {
        case IxAiBtSpecNodeKind::Selector:
        case IxAiBtSpecNodeKind::Sequence:
        {
            for (u32 i = 0; i < node._childArity; ++i)
            {
                if (!ValidatePreorderWalk(specs, index))
                {
                    return false;
                }
            }

            return true;
        }

        default:
            return true;
    }
}

void FillDefaultTreeSpec(xr_vector<IxAiBtSpecNode>& out)
{
    out.clear();
    out.reserve(16);

    out.push_back({IxAiBtSpecNodeKind::Selector, 4, IxAiAlertLevel::Vigilant});
    out.push_back({IxAiBtSpecNodeKind::Sequence, 2, IxAiAlertLevel::Vigilant});
    out.push_back({IxAiBtSpecNodeKind::ConditionAlertGte, 0, IxAiAlertLevel::Combat});
    out.push_back({IxAiBtSpecNodeKind::ActionClearTacticalHint, 0, IxAiAlertLevel::Vigilant});
    out.push_back({IxAiBtSpecNodeKind::Sequence, 3, IxAiAlertLevel::Vigilant});
    out.push_back({IxAiBtSpecNodeKind::ConditionAlertGte, 0, IxAiAlertLevel::Search});
    out.push_back({IxAiBtSpecNodeKind::ConditionAlertLt, 0, IxAiAlertLevel::Combat});
    out.push_back({IxAiBtSpecNodeKind::ActionApplyStrongestMemoryHint, 0, IxAiAlertLevel::Vigilant});
    out.push_back({IxAiBtSpecNodeKind::Sequence, 3, IxAiAlertLevel::Vigilant});
    out.push_back({IxAiBtSpecNodeKind::ConditionAlertLt, 0, IxAiAlertLevel::Search});
    out.push_back({IxAiBtSpecNodeKind::ConditionHasInvestigateMemory, 0, IxAiAlertLevel::Vigilant});
    out.push_back({IxAiBtSpecNodeKind::ActionApplyInvestigateMemoryHint, 0, IxAiAlertLevel::Vigilant});
    out.push_back({IxAiBtSpecNodeKind::ActionNoop, 0, IxAiAlertLevel::Vigilant});
}

IxAiBtNode* BuildSubtreeFromSpec(const xr_vector<IxAiBtSpecNode>& specs, size_t& index, IxAiBtNodePool* pool)
{
    if (index >= specs.size())
    {
        Msg("! [IX AI]: BT build underrun (missing spec nodes)");
        return nullptr;
    }

    const IxAiBtSpecNode& spec = specs[index++];

    switch (spec._kind)
    {
        case IxAiBtSpecNodeKind::Selector:
        {
            IxAiBtSelector* node = pool != nullptr ? pool->AcquireSelector() : new IxAiBtSelector();
            node->ReserveChildren(spec._childArity);

            for (u32 i = 0; i < spec._childArity; ++i)
            {
                IxAiBtNode* child = BuildSubtreeFromSpec(specs, index, pool);

                if (child == nullptr)
                {
                    if (pool == nullptr)
                    {
                        xr_delete(node);
                    }

                    return nullptr;
                }

                node->AddChild(child);
            }

            return node;
        }

        case IxAiBtSpecNodeKind::Sequence:
        {
            IxAiBtSequence* node = pool != nullptr ? pool->AcquireSequence() : new IxAiBtSequence();
            node->ReserveChildren(spec._childArity);

            for (u32 i = 0; i < spec._childArity; ++i)
            {
                IxAiBtNode* child = BuildSubtreeFromSpec(specs, index, pool);

                if (child == nullptr)
                {
                    if (pool == nullptr)
                    {
                        xr_delete(node);
                    }

                    return nullptr;
                }

                node->AddChild(child);
            }

            return node;
        }

        case IxAiBtSpecNodeKind::ConditionAlertGte:
        {
            return pool != nullptr ? pool->AcquireConditionAlertGte(spec._threshold)
                                   : new IxAiBtConditionAlertGte(spec._threshold);
        }

        case IxAiBtSpecNodeKind::ConditionAlertLt:
        {
            return pool != nullptr ? pool->AcquireConditionAlertLt(spec._threshold)
                                   : new IxAiBtConditionAlertLt(spec._threshold);
        }

        case IxAiBtSpecNodeKind::ActionNoop:
        {
            return pool != nullptr ? pool->AcquireActionNoop() : new IxAiBtActionNoop();
        }

        case IxAiBtSpecNodeKind::ActionClearTacticalHint:
        {
            return pool != nullptr ? pool->AcquireActionClearTacticalHint() : new IxAiBtActionClearTacticalHint();
        }

        case IxAiBtSpecNodeKind::ActionApplyStrongestMemoryHint:
        {
            return pool != nullptr ? pool->AcquireActionApplyStrongestMemoryHint()
                                   : new IxAiBtActionApplyStrongestMemoryHint();
        }

        case IxAiBtSpecNodeKind::ConditionHasInvestigateMemory:
        {
            return pool != nullptr ? pool->AcquireConditionHasInvestigateMemory()
                                   : new IxAiBtConditionHasInvestigateMemory();
        }

        case IxAiBtSpecNodeKind::ActionApplyInvestigateMemoryHint:
        {
            return pool != nullptr ? pool->AcquireActionApplyInvestigateMemoryHint()
                                   : new IxAiBtActionApplyInvestigateMemoryHint();
        }

        default:
            return nullptr;
    }
}

IxAiBtNode* BuildTreeFromSpec(const xr_vector<IxAiBtSpecNode>& specs, IxAiBtNodePool* pool)
{
    size_t index = 0;
    IxAiBtNode* root = BuildSubtreeFromSpec(specs, index, pool);

    if (root == nullptr)
    {
        return nullptr;
    }

    if (index != specs.size())
    {
        Msg("! [IX AI]: BT build leftover spec nodes (expected preorder to consume all)");

        if (pool == nullptr)
        {
            xr_delete(root);
        }

        return nullptr;
    }

    return root;
}

bool TryLoadBehaviourTreeSection(CInifile& ini, LPCSTR section, xr_vector<IxAiBtSpecNode>& dest)
{
    if (!ini.section_exist(section))
    {
        return false;
    }

    xr_vector<IxAiBtSpecNode> parsed{};
    parsed.reserve(kIxAiBtMaxSpecNodes);

    for (u32 i = 0; i < kIxAiBtMaxSpecNodes; ++i)
    {
        char key[32];
        xr_sprintf(key, "node_%u", i);

        if (!ini.line_exist(section, key))
        {
            break;
        }

        shared_str line = ini.r_string(section, key);
        const char* linePtr = line.c_str();
        linePtr = SkipSpaces(linePtr);

        if (*linePtr == ';' || *linePtr == 0)
        {
            continue;
        }

        IxAiBtSpecNode node{};

        if (!ParseBehaviourTreeSpecLine(linePtr, node))
        {
            Msg("! [IX AI]: Invalid BT spec line %s in [%s]", key, section);
            return false;
        }

        parsed.push_back(node);
    }

    if (parsed.empty())
    {
        Msg("! [IX AI]: [%s] has no node_* entries", section);
        return false;
    }

    if (!IxAiBtValidatePreorderSpec(parsed))
    {
        Msg("! [IX AI]: BT preorder validation failed for [%s]", section);
        return false;
    }

    dest.swap(parsed);
    return true;
}
} // namespace IxAiBtTreeDetail

bool IxAiBtValidatePreorderSpec(const xr_vector<IxAiBtSpecNode>& specs)
{
    size_t index = 0;

    if (!IxAiBtTreeDetail::ValidatePreorderWalk(specs, index))
    {
        return false;
    }

    return index == specs.size();
}

void IxAiBtTreeRegistryResetToCodeDefaults()
{
    xrCriticalSectionGuard guard(g_ixAiBtTreeRegistryCs);
    IxAiBtTreeDetail::FillDefaultTreeSpec(g_specsGuardBasic);
    IxAiBtTreeDetail::FillDefaultTreeSpec(g_specsFlankerLite);
    g_ixAiBtTreeRegistryInitialized = true;
}

void IxAiBtTreeRegistryTryLoadFromIni(CInifile& ini)
{
    xr_vector<IxAiBtSpecNode> parsedGuard{};
    xr_vector<IxAiBtSpecNode> parsedFlanker{};

    const bool hasGuard =
        IxAiBtTreeDetail::TryLoadBehaviourTreeSection(ini, kSectionGuardBasic, parsedGuard);
    const bool hasFlanker =
        IxAiBtTreeDetail::TryLoadBehaviourTreeSection(ini, kSectionFlankerLite, parsedFlanker);

    if (!hasGuard && !hasFlanker)
    {
        return;
    }

    xrCriticalSectionGuard guard(g_ixAiBtTreeRegistryCs);

    if (hasGuard)
    {
        g_specsGuardBasic.swap(parsedGuard);
        Msg("* [IX AI]: Loaded BT spec from [%s]", kSectionGuardBasic);
    }

    if (hasFlanker)
    {
        g_specsFlankerLite.swap(parsedFlanker);
        Msg("* [IX AI]: Loaded BT spec from [%s]", kSectionFlankerLite);
    }

    g_ixAiBtTreeRegistryInitialized = true;
}

void IxAiBtTreeRegistryEnsureInitialized()
{
    xrCriticalSectionGuard guard(g_ixAiBtTreeRegistryCs);

    if (g_ixAiBtTreeRegistryInitialized)
    {
        return;
    }

    IxAiBtTreeDetail::FillDefaultTreeSpec(g_specsGuardBasic);
    IxAiBtTreeDetail::FillDefaultTreeSpec(g_specsFlankerLite);
    g_ixAiBtTreeRegistryInitialized = true;
}

const xr_vector<IxAiBtSpecNode>& IxAiBtTreeRegistryGetSpec(IxAiBehaviourKind kind)
{
    xrCriticalSectionGuard guard(g_ixAiBtTreeRegistryCs);

    if (!g_ixAiBtTreeRegistryInitialized)
    {
        IxAiBtTreeDetail::FillDefaultTreeSpec(g_specsGuardBasic);
        IxAiBtTreeDetail::FillDefaultTreeSpec(g_specsFlankerLite);
        g_ixAiBtTreeRegistryInitialized = true;
    }

    switch (kind)
    {
        case IxAiBehaviourKind::FlankerLite:
            return g_specsFlankerLite;

        case IxAiBehaviourKind::GuardBasic:
        default:
            return g_specsGuardBasic;
    }
}

IxAiBtNode* IxAiBtBuildBehaviourTreeRoot(IxAiBehaviourKind kind, IxAiBtNodePool* pool)
{
    xr_vector<IxAiBtSpecNode> specCopy{};

    {
        xrCriticalSectionGuard guard(g_ixAiBtTreeRegistryCs);

        if (!g_ixAiBtTreeRegistryInitialized)
        {
            IxAiBtTreeDetail::FillDefaultTreeSpec(g_specsGuardBasic);
            IxAiBtTreeDetail::FillDefaultTreeSpec(g_specsFlankerLite);
            g_ixAiBtTreeRegistryInitialized = true;
        }

        specCopy = (kind == IxAiBehaviourKind::FlankerLite) ? g_specsFlankerLite : g_specsGuardBasic;
    }

    return IxAiBtTreeDetail::BuildTreeFromSpec(specCopy, pool);
}
