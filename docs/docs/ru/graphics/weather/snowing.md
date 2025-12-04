# Заснеживание
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: IX-Ray Platform 1.3

Заснеживание - это динамическое нанесения снега на поверхности, которые не попадают под `outdoor`. 
Для активации перейдите в файл **engine_external.ltx** и установите свойство `UseDynamicSnowMask` в `true`:
```ini
UseDynamicSnowMask = true
```

## Заснеживание: Динамика
* Для заснеживания на динамике нужно создавать дополнительную текстурную маску с подписью `_snowmask`. 
![image](https://github.com/user-attachments/assets/c747eab5-5e45-4f61-8201-6ab63104f7a9)

* Примерный вид маски
![image](https://github.com/user-attachments/assets/1a4fdd66-5872-45fb-b7a0-6cdb8785a4be)

* Само заснеживание хранится в зелёном канале текстуры
![image](https://github.com/user-attachments/assets/05e7212a-6c43-4800-ab7b-218990b67ffe)
![image](https://github.com/user-attachments/assets/77c878e5-6f46-4499-8cb6-b9852a6f71f3)

## Заснеживание: Статика
* Статика не требует каких-либо дополнительных масок, кроме `textures/shaders/snowmask/snow.dds`
![image](https://github.com/user-attachments/assets/1c19bdac-94d1-4e69-8a7d-6fc0dbbc8399)
