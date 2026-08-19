# R4 feature matrix

> Baseline на 23 июля 2026 года. Матрица фиксирует возможности текущего R4 и не означает, что Tiramisu уже достиг паритета.

## Назначение

Эта страница — полный проверяемый список функциональности, которую необходимо учесть при замене игрового R4. Короткая таблица в [roadmap](./roadmap.md#матрица-паритета) показывает только состояние крупных областей, а эта матрица является источником отдельных parity-задач и acceptance-сценариев.

При инвентаризации использовались:

- orchestration R4 в `src/Layers/xrRenderPC_R4/r4_R_render.cpp`, `r4_rendertarget.h` и `r4_rendertarget_phase_*.cpp`;
- общая scene/visual база в `src/Layers/xrRender/`;
- D3D10/11 implementation в `src/Layers/xrRenderDX10/`;
- R4 shader assets в `gamedata/shaders/d3d11/` и общие includes в `gamedata/shaders/shared/`;
- фактическое состояние `src/Layers/xrRenderTiramisu/`, `src/xrTiramisuMaterialCore/` и Tiramisu backend редактора.

Наличие исходника или pass в R4 считается частью baseline даже тогда, когда возможность зависит от console option, качества, стороннего SDK или конкретного контента. Поддержка Tiramisu засчитывается только после одинаковой проверки Vulkan и D3D12 с обязательным `-rdbg`.

## Обозначения

| Статус Tiramisu | Значение |
| --- | --- |
| ✅ | Реализовано в целевом path и подтверждено тестом на обоих backend |
| 🟡 | Есть foundation, editor-only path или неполная реализация |
| 🧪 | Есть HLSL/CPU contract, но нет production GPU pass |
| ⬜ | Реализации ещё нет |
| N/A | Возможность R4 заменяется другой архитектурой, но совместимый результат всё равно проверяется |

## Backend, frame и lifecycle

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| D3D11 device и renderer selection | `xrRenderPC_R4`, `dxRenderDeviceRender` | 🟡 Равноправные NRI Vulkan/D3D12 device paths; игровой path остаётся opt-in | Одинаковый feature set Vulkan/D3D12 без validation errors |
| Adapter selection | `dxRenderDeviceRender` | ✅ Полная NRI enumeration, graphics/API filtering и стабильный priority | Unit test policy и запуск на доступных adapter classes |
| Swapchain и present | `dxRenderDeviceRender`, `r4_rendertarget_phase_combine.cpp` | 🟡 Game/editor present работает, lifecycle ещё прототипный | Resize, minimize/restore, fullscreen, VSync и повторное создание |
| Несколько кадров в полёте | R4 backend frame resources | 🟡 Три command contexts есть; uploads, descriptors и queries разделены не полностью | Три независимых frame contexts без преждевременного reuse |
| Device reset/loss | `dxRenderDeviceRender` | ⬜ | Automated device restart/recovery без утечки и stale handles |
| Render thread | engine render-device path | 🟡 Очередь команд и часть affinity checks готовы | Полный аудит всех NRI create/destroy/update и shutdown |
| Deferred resource deletion | resource manager/backend | 🟡 Fence-aware очередь подключена только к части ресурсов | Все GPU resources уничтожаются после нужного fence |
| GPU annotations и debug names | `GPU_EVENT`, R4 resource names | 🟡 Основные Tiramisu passes и render graph отмечены; покрытие неполное | Каждый production pass и долгоживущий resource видим в RenderDoc |
| RenderDoc capture | R4 include/API integration | ✅ Общий bootstrap `-renderdoc`, F12 и logs path проверены | Normal/ASan Vulkan/D3D12 smoke с `-rdbg` |
| Screenshot и async screenshot | `RenderScreenshot.cpp`, `DoAsyncScreenshot` | ⬜ | LDR/HDR screenshot и async readback |
| Frame/resource statistics | `dxStatsRender`, renderer statistics | 🟡 Базовый game/editor snapshot: CPU frame time, passes, draws, triangles/lines, uploads и tracked buffers/textures/pipelines/descriptors/bytes; GPU time и driver VRAM не выдаются за измеренные | Стабильные GPU timestamp queries и API budget/residency counters |

## Scene, geometry и visibility

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| Старая `.level`/OGF scene | `r4_loader.cpp`, `ModelPool.cpp` | 🟡 `TiramisuLegacyScene` и editor conversion читают static slice | Representative legacy levels без пропущенных visuals |
| Новый scene format | отсутствует как отдельная архитектура R4 | 🟡 `xrTiramisuSceneCore` StaticMesh/RenderScene v2 | Native scene полностью загружается без `EScene`/R4 renderer |
| Static level geometry | `FBasicVisual`, `r__dsgraph_*` | 🟡 `FMeshBatch`, sections/slots и viewport draw работают | Game path, correct transforms/materials и image reference |
| Hierarchy visuals | `FHierrarhyVisual` | 🟡 Legacy import flattening/instances покрывает только текущий slice | Иерархия, visibility и instance transforms сохраняются |
| Progressive meshes и SWI | `FProgressive`, `R_Backend_LOD` | ⬜ | Все SWI ranges, transitions и bounds |
| FLOD/LOD visuals | `FLOD`, `r__dsgraph_render_lods.cpp` | 🟡 StaticMesh LOD data model есть, selection policy не подключена | Deterministic screen-size LOD и отсутствие popping regressions |
| Skeletal rigid/animated | `SkeletonRigid`, `SkeletonAnimated`, `SkeletonX` | 🧪 LevelEditor OGF/OMF actor path: 1–4 weights, current/previous GPU palette и `skeletal` material vertex factory | Игровые Actors/NPC/HUD skinned scenes, LOD/SWI, velocity acceptance и shadows |
| Trees и wind deformation | `FTreeVisual`, `R_Backend_tree` | ⬜ | Tree vertex factory, wind, lighting и masked shadows |
| Details/grass | `DetailManager*`, `DetailModel` | 🟡 | LevelEditor: CPU slot placement → batched neutral static mesh → `xrRenderTiramisu`; отсутствуют game streaming, wind, fade, shadows, density runtime и native GPU instancing |
| Particles и particle groups | `ParticleEffect`, `ParticleGroup` | 🟡 Editor billboard bridge есть, игровой vertex factory отсутствует | Game particle library, sorting, soft/distort/additive variants |
| Glows | glow renderer/shared render interface | 🟡 Editor billboard bridge есть | Game glows, occlusion/fade и blend parity |
| HUD models и оружие | HUD render phases и model pool | ⬜ | First-person weapon/HUD depth/FOV, attachments и effects |
| Dynamic objects | dynamic dsgraph paths | 🟡 Editor transforms работают только для native/static content | Moving rigid objects с previous transform и velocity |
| Frustum culling | `R_calculate.cpp`, dsgraph build | 🟡 Editor CPU bounds checks, production visibility list отсутствует | CPU/GPU visible set совпадает с reference |
| Sectors и portals | `r__sector*`, portal traversal | ⬜ | Legacy indoor/outdoor traversal и camera crossing |
| HOM occlusion | `HOM.cpp` | ⬜ Целевая замена — async compute OCC | Representative scenes без false-negative visibility |
| Hardware occlusion queries | `r__occlusion.*`, `phase_occq` | ⬜ | Только если нужны как fallback для async compute OCC |
| Async compute occlusion culling | отсутствует в R4 | ⬜ Целевая Tiramisu architecture | Indirect visible list, overlap с graphics и deterministic fallback |
| Draw sorting/state batching | `r__dsgraph_structure.h`, `r__dsgraph_render.cpp` | 🟡 Базовые material/pipeline keys есть | Opaque front-to-back, transparent back-to-front, stable keys |
| Object ID и picking | legacy editor/game selection paths | 🟡 Native/editor CPU picking и object ID работают | GPU ID/debug view и корректность для всех vertex factories |
| Motion vectors | R4 TAA/motion-blur inputs | 🧪 G-buffer ABI содержит Velocity; pass не подключён | Static/dynamic/skinned/vegetation velocity reference scenes |

## Materials и surface passes

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| `shaders.xr`/Lua shader selection | R4 resource manager и blenders | 🟡 `legacy-map.json`, pre-authored instances и fallback chain | Ноль unmapped штатных материалов в cooker report |
| Opaque deferred materials | `blender_deffer_flat/model` | 🟡 Forward editor path; G-buffer только shader foundation | Material pass proxy создаёт production G-buffer pipeline |
| Alpha-masked materials | `blender_deffer_aref` | 🧪 Pass/template и static switch foundation | Depth, shadow и G-buffer alpha test совпадают |
| Two-sided materials | R4 cull-state blenders | 🟡 Metadata и editor pipeline state поддерживаются | Correct normals/culling во всех passes |
| Translucent materials | forward/distort dsgraph paths | 🟡 Editor Forward translucent draw работает | Game clustered lighting, sorting, depth/refraction policy |
| Additive/modulate materials | blend definitions и forward paths | 🟡 Editor additive draw работает; modulate не закрыт | Legacy blend matrix и representative effects |
| Emissive | deferred/forward shader variants | 🟡 Legacy masters и `Emissive` input есть | HDR emissive, bloom contribution и masked variants |
| Normal maps | D3D11 deferred shaders | 🧪 `MaterialInputs.Normal`/G-buffer contract есть | Tangent-space basis и compressed normal reference |
| Roughness/metallic/AO PBR | R4 shader implementation | 🧪 Parameter/input и BRDF foundation | G-buffer round-trip и IBL/direct-light reference |
| Parallax | `binder_parallax`, D3D11 material shaders | ⬜ | Legacy quality switches и UV/depth behavior |
| Tessellation/displacement | D3D11 shader/options path | ⬜ | Решение сохранить или явно снять с parity с content audit |
| Detail textures/bump detail | D3D11 material shaders | ⬜ Игровой legacy bridge передаёт только первую texture | Полный legacy texture/parameter block через material ABI |
| Lightmaps/hemi data | legacy vertex formats и binders | ⬜ | Level vertex factories и baked lighting parity |
| World Position Offset | material contract/canonical vertex shader | 🧪 WPO contract компилируется | Graph/HLSL WPO на static/skinned/foliage passes |
| Master material HLSL | у R4 отдельные shader/blender combinations | 🟡 Общий template/contract/compiler готовы | Production pass set и hot reload |
| Material graph | отсутствует в R4 | 🟡 Typed graph, HLSL generator и ImNodes editor работают | Полный authoring acceptance и production permutation stats |
| Material instances | отсутствуют как единый asset model R4 | 🟡 Master/instance/dynamic hierarchy и GPU ABI готовы | Cooked flattened records, parameter lifetime и game binding |
| Bindless textures/samplers | отсутствуют как общий R4 contract | 🟡 Descriptor Heap Indexing работает в shader/runtime slice | Все material textures используют versioned descriptor ABI |
| Hot reload | shader/resource reload paths | 🟡 Editor focused reload с last-good fallback | Renderer-wide production pass/pipeline set |

## Deferred, lighting и shadows

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| G-buffer geometry pass | `phase_scene_begin/end`, deferred blenders | 🧪 Pack/unpack HLSL есть; `TiramisuRenderDeferredPass` — временный single-target pass | MRT BaseColor/AO, Normal/R/M, Emissive/flags, Velocity, Depth |
| Depth/stencil prepass | scene and shadow phases | 🧪 Templates есть, production resource/pass нет | Masked/two-sided depth, stable stencil classification |
| Deferred light accumulator | `phase_accumulator`, `r4_rendertarget_accum_*` | ⬜ | Render-graph light accumulation/resolve |
| Directional sun | `accum_direct_cascade`, `R_sun.cpp` | 🟡 Editor Forward directional light | Game upload и deferred/clustered evaluation |
| Point lights | `accum_point` | 🟡 Editor Forward point light | Volume/cluster visibility, attenuation и shadows |
| Spot lights | `accum_spot` | 🟡 Editor Forward spot light | Cone attenuation, cookie/projector и shadows |
| Reflected lights | `accum_reflected` | ⬜ | Content audit определяет отдельный path или clustered representation |
| GGX/Smith/Schlick BRDF | R4 material/light shaders | 🧪 Общий include компилируется и используется editor Forward | Deterministic lighting images Vulkan/D3D12 |
| IBL/environment reflections | environment/cubemap shader paths | 🟡 Preview environment lighting без production prefilter | Irradiance, prefiltered specular и BRDF LUT |
| Clustered light lists | отсутствуют в R4 | ⬜ Целевой Tiramisu path | Async/compute build, overflow diagnostics и stress scene |
| Cascaded sun shadows | `render_sun_cascades`, `phase_smap_direct*` | ⬜ | Cascades, stabilization, bias и masked casters |
| Local-light shadows | `phase_smap_spot*`, light visibility | ⬜ | Point/spot allocation, cache/lifetime и masked casters |
| Translucent shadows | `*_tsh` shadow phases | ⬜ | Решение parity по content audit и reference |
| Volumetric lights/sunshafts | `accum_direct_volumetric`, `phase_combine_volumetric` | ⬜ | Sun/local volumetrics и composition |
| Decals/wallmarks | `WallmarksEngine`, `phase_wallmarks` | 🟡 Editor projective Decal pass + legacy adapter | Production DBuffer/G-buffer composition, angle fade, lifetime/culling, game dynamic decals и persistent migration dump |
| Fog Volume authoring | `EFogVolume`, emitter/occlusion shapes | 🟡 LevelEditor renderer-neutral shape packet | Volumetric fog simulation/composition остаётся отдельным game-renderer pass |

## World и environment

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| Sky | environment renderer/combine phase | ⬜ | Sky geometry/cubemap, exposure и depth policy |
| Clouds | `Environment().RenderClouds()` | ⬜ | Weather-driven cloud layers и blending |
| Weather transitions | environment manager/render bridge | ⬜ | Automated deterministic transition sequence |
| Rain | `r4_R_rain.cpp`, `phase_rain`, `draw_rain` | ⬜ | Drops, splash/volume, light interaction и performance |
| Wet surfaces и puddles | `phase_puddles` и rain shaders | ⬜ | Accumulation/fade, normals/reflections и material response |
| Water | water shader/forward paths | ⬜ | Waves, reflection/refraction, depth/fog и rain interaction |
| Lens flares | `dxLensFlareRender`, `RenderFlares` | ⬜ | Occlusion, weather descriptors и HDR composition |
| Thunderbolts | `dxThunderbolt*Render` | ⬜ | Geometry/light/flash timing in weather transition |
| Foliage | tree/details paths | ⬜ | Foliage shading model, subsurface approximation, wind и shadows |
| Ambient wind/time data | shader constants/environment | 🧪 Material graph exposes time/world/camera data | Stable frame data ABI and deterministic time override |

## Postprocessing

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| HDR scene pipeline | combine/luminance render targets | ⬜ Tone-map HLSL alone не образует HDR pipeline | HDR target formats, luminance range и transparent composition |
| Luminance/exposure/adaptation | `phase_luminance`, `phase_new_luminance` | ⬜ | Deterministic fixed exposure и production auto exposure |
| Tone mapping/gamma | combine/gamma blenders | 🧪 Bindless tone-map HLSL компилируется | Runtime pass, color-space and screenshot validation |
| Bloom | `phase_bloom`, downsample/upsample | ⬜ | Threshold/downsample/upsample/combine и emissive reference |
| SSAO | `phase_ssao` | ⬜ | R4 quality baseline или утверждённая GTAO replacement policy |
| GTAO | `phase_gtao` | ⬜ | Quality modes, denoise и temporal stability |
| SSLR/SSR | `phase_sslr` | ⬜ | Roughness/depth hierarchy, miss fallback и temporal behavior |
| FXAA | `phase_fxaa` | ⬜ | Quality option и UI composition |
| SMAA | `phase_smaa` | ⬜ | Edge/weight/neighborhood passes |
| TAA | `phase_taa` | ⬜ | Jitter, velocity, history rejection и ghosting suite |
| Motion blur | `phase_mblur` | ⬜ | Camera/object velocity, HUD exclusion и quality switches |
| DOF | `phase_new_dof` | ⬜ | Near/far blur, weapon policy и gameplay parameters |
| CAS/sharpen | `phase_cas` | ⬜ | Standalone и upscale ordering |
| Resolution scale/upscale | `phase_scale`, `phase_depth_upscale` | ⬜ | Dynamic/static scale, depth/UI mapping и resize |
| FSR | `phase_fsr` | ⬜ | Supported SDK path, jitter/reactive mask и fallback |
| DLSS | `phase_dlss` | ⬜ | Supported SDK path, feature lifecycle и fallback |
| XeSS | `phase_xess` | ⬜ | Подтверждение доступной SDK integration или явное исключение |
| NVG | `phase_nvg` | ⬜ | Gameplay activation, noise/light response и UI ordering |
| Gas mask/screen PP | `BlenderGasMask`, `RenderEffect`, `phase_pp` | ⬜ | Mask overlays, droplets/custom effects и ordering |
| Distortion/heat haze | distort dsgraph/combine path | ⬜ | Distortion buffer, particles и UI exclusion |
| Sunshafts | `need_to_render_sunshafts`, volumetric direct path | ⬜ | Weather/light-driven shafts and quality settings |
| PP UI/video UI | `OnRenderPPUI_*`, `phase_ui_postprocess` | ⬜ | Menu/HUD/video composition before/after tone mapping |

## UI, debug и tooling

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| Game UI primitives | `dxUIRender`, `RenderUI` | 🟡 Прототипный UI texture-index path | Material/pass proxy UI domain, clipping, fonts и batching |
| Fonts | `dxFontRender` | ⬜ В NRI game renderer | Unicode/font atlas, scaling и HUD reference |
| Debug draw | `dxDebugRender`, backend DBG | 🟡 Editor world/overlay primitives и labels работают | Game debug geometry/text and RenderDoc names |
| Renderer statistics overlay | `dxStatsRender`, `dxStatGraphRender` | ⬜ | Frame/resource counters and history graph |
| Material cooker | отсутствует как общий R4 material bundle | 🟡 Compiler/cooker и deterministic shader blobs готовы | Cooked runtime не читает JSON и не компилирует HLSL |
| Material Editor | legacy ShaderEditor workflows | 🟡 Master/instance/node authoring и GPU preview работают | Production IBL, permutation coverage and full scene workflow |
| LevelEditor viewport | legacy editor renderer | 🟡 Tiramisu static/light authoring и legacy auto-conversion | Tiramisu-only composition root and all required editor tools |
| Validation runner | D3D debug layer/manual workflow | 🟡 `-rdbg` policy и smoke tests есть | Отдельный Vulkan/D3D12 runner, zero API/NRI errors |
| Deterministic GPU mode | отсутствует как единый R4 mode | 🟡 Общая game/editor policy фиксирует seed, timestep, shader/weather time и exposure contract; GPU smoke проверен на двух API | Deterministic cameras, content flythrough и stable image captures |
| Performance comparison | R4 statistics/manual captures | ⬜ | Одинаковые settings/scenes: P95 ≤ +10%, VRAM ≤ +15% |

## Правило закрытия строк

Статус отдельной строки меняется на ✅ только если:

1. production path использует целевую Tiramisu architecture, а не editor-only или validation pipeline;
2. CPU/compiler tests выполняются с `-rdbg`;
3. GPU acceptance выполнен на Vulkan и D3D12 с `-rdbg`;
4. в NRI/API validation нет ошибок;
5. для визуальной функции есть deterministic reference или утверждённый image-diff допуск;
6. документация указывает фактические ограничения и не называет foundation готовой функцией.

Создание этой матрицы закрывает baseline-инвентаризацию этапа 1. Строка этапа 9 «закрыть feature matrix» останется незавершённой до тех пор, пока все обязательные строки не получат ✅ либо документированное решение исключить их после аудита контента.
