#pragma once

enum EChooseMode
{
    smCustom = 0,
    smSoundSource,
    smSoundEnv,
    smObject,
    smGroup,
    smEShader,
    smCShader,
    smPE,
    smParticles,
    smTexture,
    smTextureRaw,
    smEntityType,
    smSpawnItem,
    smLAnim,
    smVisual,
    smSkeletonAnims,
    smSkeletonBones,
    smSkeletonBonesInObject,
    smGameMaterial,
    smGameAnim,
    smGameSMotions,
};

enum EChooseFlags
{
    cfMultiSelect = (1 << 0),
    cfAllowNone = (1 << 1),
    cfFullExpand = (1 << 2),
};
