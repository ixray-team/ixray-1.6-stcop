object StrPoolEditForm: TStrPoolEditForm
  Left = 255
  Top = 208
  Caption = 'Strings Editor'
  ClientHeight = 259
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu: TPopupMenu
    Left = 312
    Top = 8
    object AddItem: TMenuItem
      Caption = '&Add'
      OnClick = AddBtnClick
    end
    object InsertItem: TMenuItem
      Caption = '&Insert'
      Enabled = False
      OnClick = InsertBtnClick
    end
    object DeleteItem: TMenuItem
      Caption = '&Delete'
      Enabled = False
      OnClick = DeleteBtnClick
    end
  end
  object MainMenu: TMainMenu
    Left = 232
    Top = 88
    object Pool1: TMenuItem
      Caption = 'Pool'
      object Clear1: TMenuItem
        Caption = 'Clear'
      end
      object Open1: TMenuItem
        Caption = 'Open ...'
      end
      object Save1: TMenuItem
        Caption = 'Save ...'
      end
    end
    object Text1: TMenuItem
      Caption = 'Item'
      object Open2: TMenuItem
        Caption = 'Open ...'
      end
      object Save2: TMenuItem
        Caption = 'Save ...'
      end
    end
  end
end