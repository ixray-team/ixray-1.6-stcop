# Representative scenes и deterministic GPU mode

> Baseline suite v1 на 23 июля 2026 года. Source of truth: `gamedata/render_tests/representative-scenes.json`.

## Режим запуска

Все GPU-сравнения Tiramisu запускаются с двумя обязательными аргументами:

```text
-rdbg -render-deterministic
```

LevelEditor использует `xrRenderTiramisu` по умолчанию; `-tiramisu-editor` в командах ниже оставлен для явности и совместимости старых launch-конфигураций. Для D3D12 требуется `-dx12`. Автоматический smoke всегда получает `-editor-test-hidden`, чтобы splash и главное окно не забирали фокус; этот флаг валиден только вместе с `-render-deterministic`. Запуск `-render-deterministic` без точного `-rdbg` считается ошибкой и не начинается. Алиас `-rdebug` это требование не удовлетворяет.

Общий policy находится в `xrCore/RenderTestPolicy.h` и фиксирует:

| Параметр | Значение |
| --- | --- |
| Simulation timestep | `1 / 60 s` |
| Engine RNG seed | `0x13572468` |
| Shader time | `123.0 s` |
| Weather time | `12:00:00`, time factor `0` |
| Exposure contract | `1.0` |

Игра и редактор используют фиксированный timestep вместо wall clock. Основной engine `Random` получает одинаковый seed при старте. Tiramisu записывает фиксированное время в `CameraPositionAndTime.w`; environment каждый кадр возвращается к зафиксированному времени суток. Editor material backend в этом режиме не реагирует на фоновые filesystem events, но явная транзакция `-viewport-material-reload-smoke` остаётся доступной и проверяет last-good pipeline.

Fixed exposure уже входит в versioned contract режима, но начальный Tiramisu ещё не имеет production HDR/exposure pass. До его реализации значение не оказывает влияния на изображение и не должно ошибочно считаться готовым auto-exposure.

## Набор сцен

| ID | Source | Что фиксирует | Состояние runner |
| --- | --- | --- | --- |
| `editor-material-lighting-smoke` | built-in LevelEditor scene, 512×512 | Opaque/translucent/additive, Texture2D/Cube, Directional/Point, selection/debug/UI, hot reload | Готов |
| `legacy-zaton-import` | `rawdata/levels/!FinalSP/zaton.level` и sibling `.part` | Полная legacy-конвертация, большая static scene, material migration/dump | Готов |
| `materials-reference` | `gamedata/levels/test_materials` | Masked/emissive/PBR inputs и permutations | Capture runner ожидает production material passes |
| `lighting-reference` | `gamedata/levels/test_light` | Directional/Point/Spot, shadows, IBL, HDR | Ожидает deferred lighting/shadows |
| `foliage-reference` | `gamedata/levels/test_tree` | Trees/details, masked foliage, wind/shadows | Ожидает vertex factories/world features |
| `zaton-outdoor-flythrough` | `gamedata/levels/zaton` | Outdoor visibility/LOD, water/weather/rain/sky, performance | Ожидает game scene pipeline и flythrough runner |
| `jupiter-underground-flythrough` | `gamedata/levels/jupiter_underground` | Indoor sectors/portals/OCC, dynamic lights/decals/particles | Ожидает visibility и world passes |
| `zaton-weather-sequence` | `gamedata/levels/zaton` | Weather/exposure/rain/wetness/puddles/flares/thunderbolt | Ожидает deterministic weather runner |

Статус `runner_pending` не означает, что сцена необязательна. Он явно показывает, какая более поздняя строка [R4 feature matrix](./r4-feature-matrix.md) блокирует автоматический capture. Camera profiles получают versioned координаты только после появления соответствующего production loader: записывать непроверенные координаты в baseline запрещено.

## Текущие команды

Vulkan:

```text
LevelEditor.exe -tiramisu-editor -rdbg -render-deterministic -editor-test-hidden -material-preview-smoke -viewport-material-reload-smoke
LevelEditor.exe -tiramisu-editor -rdbg -render-deterministic -editor-test-hidden -legacy-zaton-conversion-smoke
```

D3D12:

```text
LevelEditor.exe -tiramisu-editor -dx12 -rdbg -render-deterministic -editor-test-hidden -material-preview-smoke -viewport-material-reload-smoke
LevelEditor.exe -tiramisu-editor -dx12 -rdbg -render-deterministic -editor-test-hidden -legacy-zaton-conversion-smoke
```

ASan использует те же аргументы. Ни normal, ни ASan запуск без `-rdbg -render-deterministic` не закрывает deterministic GPU acceptance.

Последний baseline smoke пройден в четырёх комбинациях normal/ASan × Vulkan/D3D12. Во всех запусках exit code равен нулю, pipeline keys совпадают между normal и ASan внутри одного backend, NRI/API validation и AddressSanitizer ошибок нет. Statistics gate подтвердил `passes=3`, `draws=177`, `triangles=6560` и ненулевой tracked resource census; GPU timestamp пока ожидаемо отмечен как `not-collected`. Full-level Zaton conversion в той же матрице создал 426 mesh assets, 5536 StaticMesh components и 753 native light components с пустым списком diagnostics.

## Критерий готовности capture

Для каждой строки со статусом `ready` или будущим `capture_ready` runner обязан сохранять:

- suite/scene/version, backend, adapter и build identity;
- точный command line и deterministic policy;
- camera profile, frame/capture index, weather preset и exposure;
- color/depth и нужные diagnostic attachments;
- frame/resource statistics;
- NRI/API validation summary;
- image hash и результат сравнения с утверждённым допуском.

Baseline image не создаётся из текущей заглушки `TiramisuRenderDeferredPass`: сначала сцена должна проходить через production pass, указанный в feature matrix.
