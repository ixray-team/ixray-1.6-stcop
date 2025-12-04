# XML Expression
## Overview
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.0

**XML Expression** - is a system of simple script code for UI elements. Currently supports: 
1. Updating progress bar values (`CUIProgressBar`). 
2. Setting text in hints (`UIHintWindow`).
3. Setting text in `CUIStatic`.
3. Setting text in `CUIButton`.

## Data Types
* `float`
* `int`
* `u32` (unsigned int)
* `u16`(unsigned short)
* `string` (shared_str)
* `bool`

## Functions
### Math 
* [floor](https://en.cppreference.com/w/cpp/numeric/math/floor)
* [ceil](https://en.cppreference.com/w/cpp/numeric/math/ceil)

## Constants
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

## Example
Output text about current actor health 

![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/9d47613b-871b-4429-aac0-687156f53da5)

```xml
<a_health_text x="143" y="680" width="20" height="66" stretch="1" expression="floor(fltPlayerHealth * 100.0)">
	<text x="0" y="20" align="r" vert_align="t" font="graffiti32"/>
</a_health_text>
```
> expression="floor(fltPlayerHealth * 100.0)"
* `fltPlayerHealth` stores a value from 0 to 1, so multiply by 100
* The result has a fractional part, so we convert the result to a larger integer by calling `floor`
