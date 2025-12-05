# Snowing
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: IX-Ray Platform 1.3

Snowing dynamically applies snow to surfaces that fall under `outdoor`.  
Enable it in **engine_external.ltx** by setting `UseDynamicSnowMask` to `true`:
```ini
UseDynamicSnowMask = true
```

## Snowing: Dynamic objects
* Create an additional texture mask with the suffix `_snowmask`. 
![image](https://github.com/user-attachments/assets/c747eab5-5e45-4f61-8201-6ab63104f7a9)

* Sample mask
![image](https://github.com/user-attachments/assets/1a4fdd66-5872-45fb-b7a0-6cdb8785a4be)

* Snow data is stored in the green channel of the texture
![image](https://github.com/user-attachments/assets/05e7212a-6c43-4800-ab7b-218990b67ffe)
![image](https://github.com/user-attachments/assets/77c878e5-6f46-4499-8cb6-b9852a6f71f3)

## Snowing: Static objects
* Statics need no extra masks beyond `textures/shaders/snowmask/snow.dds`
![image](https://github.com/user-attachments/assets/1c19bdac-94d1-4e69-8a7d-6fc0dbbc8399)
