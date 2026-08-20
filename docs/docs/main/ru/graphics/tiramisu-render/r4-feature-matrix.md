# R4 feature matrix

> Baseline R4 зафиксирован 23 июля 2026 года. Состояние Tiramisu актуализировано 20 августа 2026 года и не означает, что новый renderer уже достиг паритета.

## Назначение

Эта страница — полный проверяемый список функциональности, которую необходимо учесть при замене игрового R4. Короткая таблица в [roadmap](./roadmap.md#матрица-паритета) показывает только состояние крупных областей, а эта матрица является источником отдельных parity-задач и acceptance-сценариев.

При инвентаризации использовались:

- orchestration R4 в `src/Layers/xrRenderPC_R4/r4_R_render.cpp`, `r4_rendertarget.h` и `r4_rendertarget_phase_*.cpp`;
- общая scene/visual база в `src/Layers/xrRender/`;
- D3D10/11 implementation в `src/Layers/xrRenderDX10/`;
- R4 shader assets в `gamedata/shaders/d3d11/` и общие includes в `gamedata/shaders/shared/`;
- фактическое состояние `src/Layers/xrRenderTiramisu/`, `src/xrTiramisuMaterialCore/` и Tiramisu backend редактора.

Наличие исходника или pass в R4 считается частью baseline даже тогда, когда возможность зависит от console option, качества, стороннего SDK или конкретного контента. Поддержка Tiramisu засчитывается только после одинаковой проверки Vulkan и D3D12 с обязательным `-rdbg`. Editor-only реализация остаётся 🟡 даже при успешных GPU smoke, пока тот же contract не подключён к игровому production path.

Последняя актуализация учитывает normal/ASan × Vulkan/D3D12 проверки Tiramisu LevelEditor, Material Preview, legacy conversion и RenderDoc 1.45. Deterministic skeletal capture теперь фиксирует current/previous animation sample независимо от длительности фоновой компиляции, а scene layout сортируется по backend-neutral `PipelineSortKey`, Material Slot, Mesh/Section и Object ID вместо адреса `nri::Pipeline*`. RenderDoc MCP подтвердил одинаковый порядок обоих actor draw, одинаковые `BaseVertex`/`BaseInstance` и побайтное совпадение target Vulkan/D3D12 (`0` отличающихся пикселей, `max delta = 0`) в normal и ASan.

Для игрового `-r5` path отдельно проверена deterministic загрузка Zaton в 1600×1024. На актуальных normal и ASan Vulkan/D3D12 capture все четыре G-buffer MRT после 100 geometry draws и результат fullscreen directional resolve совпали побайтно. Normal capture содержат по 131 draw, 0 dispatch и 0 RenderDoc debug messages; ASan capture также имеют 0 debug messages и не содержат sanitizer/NRI/API/device-lost ошибок в engine logs. Для capture-safe GPU ABI draw table копируется из frame-local CPU upload regions, а dirty ranges material-instance/parameter tables — из отдельных CPU upload buffers в device-local bindless buffers.

## Обозначения

| Статус Tiramisu | Значение |
| --- | --- |
| ✅ | Реализовано в целевом path и подтверждено тестом на обоих backend |
| 🟡 | Есть foundation, editor-only path или неполная реализация |
| 🧪 | Есть HLSL/CPU contract, но нет production GPU pass |
| ⬜ | Реализации ещё нет |
| N/A | Возможность R4 заменяется другой архитектурой, но совместимый результат всё равно проверяется |

## Сводка актуального состояния

На 20 августа 2026 года матрица содержит 111 строк: 2 подтверждены как ✅, 52 имеют 🟡 рабочий foundation, неполный production path или editor-only path, 9 остаются 🧪 CPU/HLSL contract без законченной функции, 45 ещё не реализованы, а 3 legacy-механизма помечены N/A и заменяются async compute occlusion culling. Это не процент готовности: строки различаются по объёму, а renderer по умолчанию не переключается до выполнения всех acceptance gates.

Ближайший приоритет: расширить проверенный G-buffer/directional resolve до alpha-masked geometry, game lights, clustered lists и shadows; затем реализовать GPU visibility с async compute OCC. Одновременно нужно превратить проверенный RenderDoc MCP export в постоянный межAPI image-diff suite и добавить representative flythrough.

## Backend, frame и lifecycle

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| D3D11 device и renderer selection | `xrRenderPC_R4`, `dxRenderDeviceRender` | 🟡 Равноправные NRI Vulkan/D3D12 device paths; игровой path остаётся opt-in | Одинаковый feature set Vulkan/D3D12 без validation errors |
| Adapter selection | `dxRenderDeviceRender` | ✅ Полная NRI enumeration, graphics/API filtering и стабильный priority | Unit test policy и запуск на доступных adapter classes |
| Swapchain и present | `dxRenderDeviceRender`, `r4_rendertarget_phase_combine.cpp` | 🟡 Game/editor present и editor resize/recreate работают на двух API; fullscreen, VSync и device-loss lifecycle ещё прототипные | Resize, minimize/restore, fullscreen, VSync и повторное создание |
| Несколько кадров в полёте | R4 backend frame resources | 🟡 Editor использует три command/frame contexts и отдельные GPU ranges; game path использует три command contexts и frame-local draw upload regions, но global constants/query lifecycle ещё не полностью разделены | Три независимых frame contexts без преждевременного reuse |
| Device reset/loss | `dxRenderDeviceRender` | ⬜ | Automated device restart/recovery без утечки и stale handles |
| Render thread | engine render-device path | 🟡 Общий Tiramisu render thread выполняет editor resource creation, command recording, resize, present и deferred deletion; полный game/runtime affinity audit не закрыт | Полный аудит всех NRI create/destroy/update и shutdown |
| Deferred resource deletion | resource manager/backend | 🟡 Fence-aware очередь подключена только к части ресурсов | Все GPU resources уничтожаются после нужного fence |
| GPU annotations и debug names | `GPU_EVENT`, R4 resource names | 🟡 Основные game/editor/material/decal/particle passes отмечены и видны в capture; production покрытие ресурсов неполное | Каждый production pass и долгоживущий resource видим в RenderDoc |
| RenderDoc capture | R4 include/API integration | ✅ RenderDoc 1.45/API 1.7: общий bootstrap `-renderdoc`, F12, explicit capture и `logs/renderdoc` проверены normal/ASan × Vulkan/D3D12 с `-rdbg` | Сохранить capture workflow при расширении production passes |
| Screenshot и async screenshot | `RenderScreenshot.cpp`, `DoAsyncScreenshot` | ⬜ | LDR/HDR screenshot и async readback |
| Frame/resource statistics | `dxStatsRender`, renderer statistics | 🟡 Базовый game/editor snapshot: CPU frame time, passes, draws, triangles/lines, uploads и tracked buffers/textures/pipelines/descriptors/bytes; GPU time и driver VRAM не выдаются за измеренные | Стабильные GPU timestamp queries и API budget/residency counters |

## Scene, geometry и visibility

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| Старая `.level`/OGF scene | `r4_loader.cpp`, `ModelPool.cpp` | 🟡 `TiramisuLegacyScene` остаётся runtime adapter, а editor importer конвертирует полный Zaton с sibling `.part`, material/light/decal mappings и audit dump | Representative legacy levels без пропущенных visuals |
| Новый scene format | отсутствует как отдельная архитектура R4 | 🟡 `xrTiramisuSceneCore`: StaticMesh v2 JSON+BIN и RenderScene v3 StaticMesh/Light/Decal components | Native scene полностью загружается без `EScene`/R4 renderer |
| Static level geometry | `FBasicVisual`, `r__dsgraph_*` | 🟡 `FMeshBatch`, sections/slots, transforms, material overrides, shared arenas и indexed indirect viewport draw работают; Zaton: 426 meshes и 5536 components | Game path, correct transforms/materials и image reference |
| Hierarchy visuals | `FHierrarhyVisual` | 🟡 Standalone OGF loader читает hierarchy и editor scene публикует renderer-owned model packets; game integration неполная | Иерархия, visibility и instance transforms сохраняются |
| Progressive meshes и SWI | `FProgressive`, `R_Backend_LOD` | 🟡 OGF loader валидирует progressive payload, но SWI selection/render policy ещё не подключена | Все SWI ranges, transitions и bounds |
| FLOD/LOD visuals | `FLOD`, `r__dsgraph_render_lods.cpp` | 🟡 StaticMesh LOD data model есть, selection policy не подключена | Deterministic screen-size LOD и отсутствие popping regressions |
| Skeletal rigid/animated | `SkeletonRigid`, `SkeletonAnimated`, `SkeletonX` | 🟡 LevelEditor OGF/OMF GPU path: hierarchy, 1–4 weights, motions, current/previous palette и `skeletal` vertex factory; RenderDoc MCP подтвердил одинаковый skeletal draw Vulkan/D3D12 | Игровые Actors/NPC/HUD, LOD/SWI, velocity, shadows и постоянный image-diff gate |
| Trees и wind deformation | `FTreeVisual`, `R_Backend_tree` | ⬜ | Tree vertex factory, wind, lighting и masked shadows |
| Details/grass | `DetailManager*`, `DetailModel` | 🟡 LevelEditor CPU slot placement публикуется как batched renderer-neutral static mesh | Game streaming, wind, fade, shadows, density runtime и native GPU instancing |
| Particles и particle groups | `ParticleEffect`, `ParticleGroup` | 🟡 Renderer-owned original/extended catalog, PAPI simulation, group scheduling/callbacks, frame animation, alignment, bindless textured billboards и отдельный editor preview работают | Game scene wiring, culling, collision, sorting, soft/distort variants |
| Glows | glow renderer/shared render interface | 🟡 Legacy `CGlow` публикуется как renderer-owned bindless sprite и проверяется editor smoke | Game glows, occlusion/fade и blend parity |
| HUD models и оружие | HUD render phases и model pool | ⬜ | First-person weapon/HUD depth/FOV, attachments и effects |
| Dynamic objects | dynamic dsgraph paths | 🟡 Native editor components и transient packets передают current/previous transforms; игровой dynamic model pool не подключён | Moving rigid objects с previous transform и velocity |
| Frustum culling | `R_calculate.cpp`, dsgraph build | 🟡 Editor CPU culling строит compact visibility для mesh sections и projective decals; GPU/portal visibility отсутствует | CPU/GPU visible set совпадает с reference |
| Sectors и portals | `r__sector*`, portal traversal | N/A Не переносятся; visibility заменяется async compute occlusion culling | Закрывается acceptance-критериями строки Async compute occlusion culling |
| HOM occlusion | `HOM.cpp` | N/A Не переносится; заменяется async compute occlusion culling | Закрывается acceptance-критериями строки Async compute occlusion culling |
| Hardware occlusion queries | `r__occlusion.*`, `phase_occq` | N/A Не переносятся и не используются как fallback | Закрывается acceptance-критериями строки Async compute occlusion culling |
| Async compute occlusion culling | отсутствует в R4 | ⬜ Единый целевой visibility/occlusion path Tiramisu; заменяет sectors/portals, HOM и hardware occlusion queries | GPU-generated indirect visible list, conservative occlusion без false-negative visibility, overlap с graphics и deterministic CPU fallback |
| Draw sorting/state batching | `r__dsgraph_structure.h`, `r__dsgraph_render.cpp` | 🟡 Backend-neutral material permutation sort, accepted-revision cache invalidation, shared geometry arenas и indexed indirect grouping работают в editor; Zaton сводится с 3296 logical draws к 900 indirect commands | Opaque front-to-back, transparent back-to-front и game integration |
| Indexed indirect/instancing | R4 batching и обычные indexed draws | 🟡 NRI indirect ABI проверен для Vulkan/D3D12; одинаковые mesh sections объединяются в instances; D3D12 emulation передаёт `_BaseAttributes` и даёт побайтно тот же skeletal target, что direct D3D12 и Vulkan | GPU-generated visible arguments и автоматический межAPI image-diff gate |
| Object ID и picking | legacy editor/game selection paths | 🟡 Renderer-neutral CPU picker покрывает native/static и expanded OGF geometry, transforms, sections/material IDs и nearest triangle | GPU ID/debug view и корректность для всех vertex factories |
| Motion vectors | R4 TAA/motion-blur inputs | 🧪 G-buffer ABI содержит Velocity; pass не подключён | Static/dynamic/skinned/vegetation velocity reference scenes |

## Materials и surface passes

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| `shaders.xr`/Lua shader selection | R4 resource manager и blenders | 🟡 `legacy-map.json`, pre-authored instances, cached dynamic overrides, automatic per-object migration и diagnostic fallback | Ноль unmapped штатных материалов в cooker report |
| Opaque deferred materials | `blender_deffer_flat/model` | 🟡 Игровые legacy BaseWithLightColor/BaseWithLightMap proxies разрешают material pass proxy и пишут production G-buffer через bindless draw/instance/parameter tables; legacy bridge пока передаёт только первую texture и фиксированные PBR defaults | Полный legacy parameter block, masked variants и representative material images |
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
| Lightmaps/hemi data | legacy vertex formats и binders | 🧪 `TexCoord1`, `lmap` instance/static switches и parameter contract есть; временный GPU proxy всё ещё передаёт только первую texture | Полный lightmap/hemi parameter block и baked-lighting image parity |
| World Position Offset | material contract/canonical vertex shader | 🧪 WPO contract компилируется | Graph/HLSL WPO на static/skinned/foliage passes |
| Master material HLSL | у R4 отдельные shader/blender combinations | 🟡 Общий template/contract/compiler готовы | Production pass set и hot reload |
| Material graph | отсутствует в R4 | 🟡 Каталог из 24 typed node types, Constant/Make/Break/Swizzle float2–4, editable Custom HLSL signature, type checking, folding/DCE, HLSL preview и ImNodes authoring работают | Production permutation set, full-scene authoring и image acceptance |
| Material instances | отсутствуют как единый asset model R4 | 🟡 Master/instance/dynamic hierarchy, отдельные master/instance editors, parent flattening, typed overrides и GPU ABI готовы | Cooked flattened records, parameter lifetime и game binding |
| Назначение material asset объекту | legacy Surface properties | 🟡 Static Mesh Properties принимает master или instance через внутренний searchable modal `Materials`/`Instances`, сохраняет per-surface override и открывает правильный editor | Native/game asset binding без legacy Surface и полный reimport lifecycle |
| Bindless textures/samplers | отсутствуют как общий R4 contract | 🟡 Descriptor Heap Indexing и material ABI v5 работают для editor paths и игрового legacy G-buffer; draw/instance/parameter tables копируются в device-local buffers и дали побайтный Vulkan/D3D12 parity | Все игровые material textures и passes используют versioned descriptor ABI |
| Hot reload | shader/resource reload paths | 🟡 Editor focused reload с last-good fallback | Renderer-wide production pass/pipeline set |

## Deferred, lighting и shadows

| Возможность R4 | Опорный код R4 | Tiramisu сейчас | Gate паритета |
| --- | --- | --- | --- |
| G-buffer geometry pass | `phase_scene_begin/end`, deferred blenders | 🟡 `TiramisuRenderDeferredPass` пишет четыре production MRT: BaseColor/AO, Normal/R/M, Emissive/flags и Velocity плюс D24S8 Depth; deterministic Zaton Vulkan/D3D12 capture совпал побайтно по всем MRT, но pass покрывает только текущую legacy opaque статику | Masked/skinned/dynamic geometry, stencil classification, velocity и постоянный image-diff gate |
| Depth/stencil prepass | scene and shadow phases | 🧪 Templates есть, production resource/pass нет | Masked/two-sided depth, stable stencil classification |
| Deferred light accumulator | `phase_accumulator`, `r4_rendertarget_accum_*` | 🟡 Игровой fullscreen directional resolve читает bindless G-buffer/depth, восстанавливает world position и даёт побайтно одинаковый Vulkan/D3D12 target; полноценного light accumulator/clustered composition ещё нет | Render-graph accumulation для sun/local lights, transparent consumers и composition |
| Directional sun | `accum_direct_cascade`, `R_sun.cpp` | 🟡 Editor Forward directional light и игровой deferred directional resolve работают; game pass пока использует deterministic test constants вместо environment sun upload | Environment/game upload, cascaded shadows и clustered/deferred evaluation |
| Point lights | `accum_point` | 🟡 Editor Forward point light | Volume/cluster visibility, attenuation и shadows |
| Spot lights | `accum_spot` | 🟡 Editor Forward spot light | Cone attenuation, cookie/projector и shadows |
| Reflected lights | `accum_reflected` | ⬜ | Content audit определяет отдельный path или clustered representation |
| GGX/Smith/Schlick BRDF | R4 material/light shaders | 🟡 Общий PBR include используется editor scene/preview и игровым deferred directional resolve; текущий Zaton resolve побайтно совпал между Vulkan/D3D12 | Game light/environment integration, reference BRDF suite и IBL |
| IBL/environment reflections | environment/cubemap shader paths | 🟡 Preview и editor Forward scene используют bindless TextureCube environment и одинаковое приближение; production prefilter отсутствует | Irradiance, prefiltered specular и BRDF LUT |
| Clustered light lists | отсутствуют в R4 | ⬜ Целевой Tiramisu path | Async/compute build, overflow diagnostics и stress scene |
| Cascaded sun shadows | `render_sun_cascades`, `phase_smap_direct*` | ⬜ | Cascades, stabilization, bias и masked casters |
| Local-light shadows | `phase_smap_spot*`, light visibility | ⬜ | Point/spot allocation, cache/lifetime и masked casters |
| Translucent shadows | `*_tsh` shadow phases | ⬜ | Решение parity по content audit и reference |
| Volumetric lights/sunshafts | `accum_direct_volumetric`, `phase_combine_volumetric` | ⬜ | Sun/local volumetrics и composition |
| Decals/wallmarks | `WallmarksEngine`, `phase_wallmarks` | 🟡 Projective Decal domain/pass, box volume, depth reconstruction, angle fade, frustum culling, live adapter и persistent RenderScene v3 migration готовы; Zaton переносит 220 decals без пропусков | Production DBuffer/G-buffer composition, lifetime/occlusion и game dynamic decals |
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
| Debug draw | `dxDebugRender`, backend DBG | 🟡 Editor world/overlay lines/triangles, selection, gizmos, labels, spawn/glow/particle diagnostics работают через Tiramisu | Game debug geometry/text and RenderDoc names |
| Renderer statistics overlay | `dxStatsRender`, `dxStatGraphRender` | 🟡 Versioned snapshot считает CPU frame, passes/draws/triangles/lines/uploads и tracked resources; overlay history, GPU timestamps и VRAM budgets отсутствуют | GPU timing/budget counters и history graph |
| Material cooker | отсутствует как общий R4 material bundle | 🟡 Compiler/cooker и deterministic shader blobs готовы | Cooked runtime не читает JSON и не компилирует HLSL |
| Material Editor | legacy ShaderEditor workflows | 🟡 Отдельные Master Material/Material Instance окна, Content Browser routing, typed graph/Custom HLSL, undo/copy/autosave, diagnostics, hot reload и GPU preview работают | Production IBL, production permutation coverage и полный scene workflow |
| LevelEditor viewport | legacy editor renderer | 🟡 Tiramisu-only NRI composition root больше не создаёт legacy D3D11 device; native scene authoring, legacy conversion, material/light/decal/particle/debug paths работают | Перенести оставшиеся object types/tools и закрыть restart/device-loss acceptance |
| Validation runner | D3D debug layer/manual workflow | 🟡 Hidden deterministic normal/ASan editor smoke проходит Vulkan/D3D12 и RenderDoc с `-rdbg`; игровые Zaton normal/ASan capture проверены RenderDoc MCP 0.3.2/RenderDoc 1.45: 0 debug messages, все G-buffer MRT и directional resolve имеют `changedPixels = 0`, `max delta = 0`, sanitizer/NRI/API/device-lost ошибки в логах отсутствуют. Normal capture содержат по 131 draw. Отдельного headless runner ещё нет | Автоматический Vulkan→D3D12 runner, zero API/NRI errors, committed references и допуск image diff |
| Deterministic GPU mode | отсутствует как единый R4 mode | 🟡 Общая game/editor policy фиксирует seed, timestep, shader/weather time и exposure contract; GPU smoke проверен на двух API | Deterministic cameras, content flythrough и stable image captures |
| Performance comparison | R4 statistics/manual captures | 🟡 Добавлены pass-level CPU profiles, resource census и Zaton batching profile; сопоставимого R4/GPU/VRAM baseline пока нет | Одинаковые settings/scenes: P95 ≤ +10%, VRAM ≤ +15% |

## Правило закрытия строк

Статус отдельной строки меняется на ✅ только если:

1. production path использует целевую Tiramisu architecture, а не editor-only или validation pipeline;
2. CPU/compiler tests выполняются с `-rdbg`;
3. GPU acceptance выполнен на Vulkan и D3D12 с `-rdbg`;
4. в NRI/API validation нет ошибок;
5. для визуальной функции есть deterministic reference или утверждённый image-diff допуск;
6. документация указывает фактические ограничения и не называет foundation готовой функцией.

Создание этой матрицы закрывает baseline-инвентаризацию этапа 1. Актуализация статусов не закрывает строку этапа 9 «закрыть feature matrix»: она останется незавершённой, пока все обязательные строки не получат ✅ либо документированное решение исключить их после аудита контента. Sectors/portals, HOM и hardware occlusion queries являются документированным исключением: их функциональные acceptance-критерии перенесены в строку Async compute occlusion culling. Ближайшие блокирующие строки — production G-buffer/deferred lighting, GPU visibility/async OCC, игровой material binding, shadows и автоматизированный representative image-diff suite.
