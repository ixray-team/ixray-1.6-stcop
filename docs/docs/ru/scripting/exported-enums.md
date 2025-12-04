# Экспортированные enum'ы
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

## EBoostParams
```lua
eBoostHpRestore -- Восстановление здоровья
eBoostPowerRestore -- Восстановление стамины
eBoostRadiationRestore -- Выведение радиации
eBoostBleedingRestore -- Свёртывание крови
eBoostMaxWeight -- Увеличение максимального переносимого веса
eBoostRadiationProtection -- Увеличение защиты от радиации
eBoostTelepaticProtection -- Увеличение защиты от пси-излучения
eBoostChemicalBurnProtection -- Увеличение защиты от химического ожога
eBoostBurnImmunity -- Уменьшение влияния ожога на прочность костюма/шлема/актора
eBoostShockImmunity -- Уменьшение влияния электричества на прочность костюма/шлема/актора
eBoostRadiationImmunity -- Уменьшение влияния радиации на прочность костюма/шлема/актора
eBoostTelepaticImmunity -- Уменьшение влияния пси-излучения на прочность костюма/шлема/актора
eBoostChemicalBurnImmunity -- Уменьшение влияния химического ожога на прочность костюма/шлема/актора
eBoostExplImmunity -- Уменьшение влияния взрыва на прочность костюма/шлема/актора
eBoostStrikeImmunity -- Уменьшение влияния удара на прочность костюма/шлема/актора
eBoostFireWoundImmunity -- Уменьшение влияния огнестрельного ранения на прочность костюма/шлема/актора
eBoostWoundImmunity -- Уменьшение влияния ранения на прочность костюма/шлема/актора
```

## EMovementStates
```lua
eOld -- Предыдущее состояние движения актора
eWishful -- Желаемое состояние движения актора
eReal -- Реальное состояние движения актора
```

## EMoveCommand
```lua
mcFwd -- Актор идёт вперед
mcBack -- Актор идёт назад
mcLStrafe -- Актор идёт влево
mcRStrafe -- Актор идёт вправо
mcCrouch -- Актор присел
mcAccel -- Актор идёт медленным шагом
mcTurn -- Актор поворачивает камеру
mcJump -- Актор прыгнул
mcFall -- Актор падает
mcLanding -- Актор приземляется
mcLanding2 -- Актор приземляется с потерей здоровья
mcClimb -- Актор забирается по лестнице
mcSprint -- Актор бежит
mcLLookout -- Актор выглядывает влево
mcRLookout -- Актор выглядывает вправо
mcAnyMove -- Актор в одном/нескольких из этих состояний: mcFwd, mcBack, mcLStrafe, mcRStrafe
mcAnyAction -- Актор в одном/нескольких из этих состояний: mcAnyMove, mcJump, mcFall, mcLanding, mcLanding2
mcAnyState -- Актор в одном/нескольких из этих состояний: mcCrouch, mcAccel, mcClimb, mcSprint
mcLookout -- Актор в одном/нескольких из этих состояний: mcLLookout, mcRLookout 
mcJumpSeq -- Актор в одном/нескольких из этих состояний: mcJump, mcFall, mcLanding, mcLanding2
```
