# XML Expression
## Обзор
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

**XML Expression** - это система простого скриптового кода для UI элементов. На текущий момент поддерживает: 
1. Обновление значений прогресс-бара (`CUIProgressBar`). 
2. Установки текста в подсказки (`UIHintWindow`).
3. Установки текста в `CUIStatic`.
3. Установки текста в `CUIButton`.

## Типы данных
* `float`
* `int`
* `u32` (unsigned int)
* `u16`(unsigned short)
* `string` (shared_str)
* `bool`

## Функции
### Математика 
* [floor](https://en.cppreference.com/w/cpp/numeric/math/floor)
* [ceil](https://en.cppreference.com/w/cpp/numeric/math/ceil)

## Константы
> flt - float; int - int; str - string
```lua
fltActorOutfitBurnProtection
fltActorOutfitShockProtection
fltActorOutfitChemicalBurnProtection	
fltActorOutfitRadiationProtection	
fltActorOutfitTelepaticProtection	
fltActorOutfitWoundProtection		
fltActorOutfitFireWoundProtection	
fltActorOutfitStrikeProtection		
fltActorOutfitExplosionProtection	
fltActorOutfitMaxFireWoundProtection
fltActorHelmetBurnProtection			
fltActorHelmetShockProtection		
fltActorHelmetChemicalBurnProtection	
fltActorHelmetRadiationProtection	
fltActorHelmetTelepaticProtection	
fltActorHelmetWoundProtection		
fltActorHelmetFireWoundProtection	
fltActorHelmetStrikeProtection		
fltActorHelmetExplosionProtection
fltActorBoostRadiationProtection		
fltActorBoostTelepaticProtection		
fltActorBoostChemicalBurnProtection
fltArtefactsBurnProtection			
fltArtefactsShockProtection			
fltArtefactsChemicalBurnProtection	
fltArtefactsRadiationProtection		
fltArtefactsTelepaticProtection		
fltArtefactsWoundProtection			
fltArtefactsFireWoundProtection		
fltArtefactsStrikeProtection			
fltArtefactsExplosionProtection
fltZoneMaxPowerBurnProtection		
fltZoneMaxPowerShockProtection		
fltZoneMaxPowerChemicalBurnProtection
fltZoneMaxPowerRadiationProtection	
fltZoneMaxPowerTelepaticProtection	
fltZoneMaxPowerWoundProtection		
fltZoneMaxPowerFireWoundProtection	
fltZoneMaxPowerStrikeProtection	
fltZoneMaxPowerExplosionProtection
fltPlayerHealth
fltPlayerRad
fltPlayerPsy
fltPlayerPower
fltPlayerSatiety
fltPlayerRestoreSpeed
fltPlayerBleedingSpeed

intPDAActiveContacts

strPlayerName
```

## Пример
Вывод текста о текущем здоровье актера 

![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/9d47613b-871b-4429-aac0-687156f53da5)

```xml
<a_health_text x="143" y="680" width="20" height="66" stretch="1" expression="floor(fltPlayerHealth * 100.0)">
	<text x="0" y="20" align="r" vert_align="t" font="graffiti32"/>
</a_health_text>
```
> expression="floor(fltPlayerHealth * 100.0)"
* `fltPlayerHealth` хранит значение от 0 до 1, поэтому умножим на 100
* Полученное значение имеет дробную часть, поэтому приведём результат к большему целому числу, вызвав `floor`
