# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### Main file structure:
```ini
; Custom callbacks
scripts/ixr_framework/extend_callbacks/*.script   
; Framework modules
scripts/ixr_framework/modules/ixr_module_*.script 
; Script utilities
scripts/ixr_framework/utils/*_utils.script   
; Script libraries    
scripts/ixr_framework/utils/libs/*_lib.script   

; Framework core
scripts/ixr_framework/ixr_framework.script      
; Mass callback subscription script for binders  
scripts/ixr_framework/ixr_callback_binder.script  

; Override: game version from storage module
scripts/__storage_mod_version.script              
; Override: enabled signal module event processing
scripts/__ixr_override_signals_intercepts.script  
; Override: framework submodule load queue
scripts/__ixr_override_framework_load_sub_modules.script 
; Override: autoloader filter settings
scripts/__ixr_override_autoload_system.script     
; Separate file for direct engine calls
scripts/___ixr_engine_callbacks.script            
```
