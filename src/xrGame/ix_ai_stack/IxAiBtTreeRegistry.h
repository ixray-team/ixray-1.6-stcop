#pragma once

#include "IxAiBtTreeSpec.h"

class CInifile;
class IxAiBtNode;
class IxAiBtNodePool;

void IxAiBtTreeRegistryResetToCodeDefaults();
void IxAiBtTreeRegistryTryLoadFromIni(CInifile& ini);
void IxAiBtTreeRegistryEnsureInitialized();

const xr_vector<IxAiBtSpecNode>& IxAiBtTreeRegistryGetSpec(IxAiBehaviourKind kind);

IxAiBtNode* IxAiBtBuildBehaviourTreeRoot(IxAiBehaviourKind kind, IxAiBtNodePool* pool);
