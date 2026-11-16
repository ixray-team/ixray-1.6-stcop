# Физически корректный рендеринг (PBR)

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.3

## Обзор

Для расчёта освещения PBS использует свойства материала текстуры и карты окружения.

## Синтаксис

Чтобы включить PBS в R4, уберите глобальный макрос `USE_LEGACY_LIGHT`. Задайте `0` или удалите строку из `gamedata/configs/engine_external.ltx`. После правки перезапустите игру.

```ini
[shaders_options]
USE_LEGACY_LIGHT = 0
```

![image](https://github.com/user-attachments/assets/51dfacd3-f01a-48ae-be84-376d36c8f822)

Описание остальных макросов

![image](https://github.com/user-attachments/assets/ba1932ab-245f-4aec-a628-bfc75a1b041a)

Для включения PBS материала у текстуры надо при создании **_THM_** включить нужную модель освещения в **_IXR_** редакторах
* Либо в [THM Editor](https://disk.yandex.ru/d/AkZWYx3Xohdd-g) с правками **_by Mortan_**

![image](https://github.com/user-attachments/assets/bf1c7cdc-4071-4c09-b070-dd3cad1c02e6)


## Упаковка материала PBS

### **_BUMP_** - **_Texture 2D_** - геометрические данные текстуры (основа - классическая BC3N паковка)
- **_BUMP R_** - Карта высот для параллакса и тесселяции (опционально) - по умолчанию не пишем (BC3N пишет мусор)
- **_BUMP G_** - Зелёный канал карты нормалей (формат OpenGL)
- **_BUMP B_** - Синий канал (опционально) - резерв на будущее. Пока хз что тут будет - по умолчанию не пишем (BC3N пишет мусор)
- **_BUMP A_** - Красный канал оригинальной нормали 

![image](https://github.com/user-attachments/assets/05ae4ef1-98f3-402e-8c28-33a5e5002c67)

### **_BUMP#_** - **_Texture 2D_** - упаковка материала для текстуры
- **_BUMP# R_** - Карта металличности
- **_BUMP# G_** - Карта шероховатости
- **_BUMP# B_** - Карта просвечивания (SSS) для растительности - черный по умолчанию
- **_BUMP# A_** - Карта AO

![image](https://github.com/user-attachments/assets/9f003bbc-675f-404b-9c1c-e6baf943e8f3)

## IBL освещение

Для правильной работы _**IBL**_ требуется запекать мип-уровни на текстурах окружающей среды (скайбоксах)

![image](https://github.com/user-attachments/assets/cb8cbcac-f474-472c-8e7b-d83527250eeb)

Карта диффузной облученности (_small sky_) не требует высокого разрешения. В будущем возможно будет поддержка сферических гармоник для диффузной облученности.

![image](https://github.com/user-attachments/assets/66a7e7fd-66f5-449b-8186-a14a993b9796)

## См. также

- [Глобальные опции шейдеров](shader-options.md)
- [THM override](thm-override.md)
