# Exported enums
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.0

## EBoostParams
```lua
eBoostHpRestore -- Health restoration
eBoostPowerRestore -- Stamina restoration
eBoostRadiationRestore -- Radiation removal
eBoostBleedingRestore -- Bleeding stop
eBoostMaxWeight -- Increase max carry weight
eBoostRadiationProtection -- Increase radiation protection
eBoostTelepaticProtection -- Increase psi protection
eBoostChemicalBurnProtection -- Increase chemical burn protection
eBoostBurnImmunity -- Reduce burn damage to suit/helmet/actor durability
eBoostShockImmunity -- Reduce electric damage to suit/helmet/actor durability
eBoostRadiationImmunity -- Reduce radiation damage to suit/helmet/actor durability
eBoostTelepaticImmunity -- Reduce psi damage to suit/helmet/actor durability
eBoostChemicalBurnImmunity -- Reduce chemical burn damage to suit/helmet/actor durability
eBoostExplImmunity -- Reduce explosion damage to suit/helmet/actor durability
eBoostStrikeImmunity -- Reduce strike damage to suit/helmet/actor durability
eBoostFireWoundImmunity -- Reduce firearm damage to suit/helmet/actor durability
eBoostWoundImmunity -- Reduce wound damage to suit/helmet/actor durability
```

## EMovementStates
```lua
eOld -- Previous actor movement state
eWishful -- Desired actor movement state
eReal -- Actual actor movement state
```

## EMoveCommand
```lua
mcFwd -- Actor moves forward
mcBack -- Actor moves backward
mcLStrafe -- Actor strafes left
mcRStrafe -- Actor strafes right
mcCrouch -- Actor is crouched
mcAccel -- Actor walks slowly
mcTurn -- Actor turns the camera
mcJump -- Actor jumped
mcFall -- Actor is falling
mcLanding -- Actor lands
mcLanding2 -- Actor lands with health loss
mcClimb -- Actor climbs a ladder
mcSprint -- Actor is sprinting
mcLLookout -- Actor leans left
mcRLookout -- Actor leans right
mcAnyMove -- Actor is in any of: mcFwd, mcBack, mcLStrafe, mcRStrafe
mcAnyAction -- Actor is in any of: mcAnyMove, mcJump, mcFall, mcLanding, mcLanding2
mcAnyState -- Actor is in any of: mcCrouch, mcAccel, mcClimb, mcSprint
mcLookout -- Actor is in any of: mcLLookout, mcRLookout
mcJumpSeq -- Actor is in any of: mcJump, mcFall, mcLanding, mcLanding2
```
