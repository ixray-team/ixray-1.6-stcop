object SoundMapForm: TSoundMapForm
  Left = 328
  Top = 284
  Caption = 'Sound Map Editor'
  ClientHeight = 208
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu: TPopupMenu
    Left = 328
    Top = 40
    object AddItem: TMenuItem
      Caption = '&Add'
      OnClick = AddItemClick
    end
    object RemoveItem: TMenuItem
      Caption = '&Remove'
      Enabled = False
      OnClick = RemoveItemClick
    end
    object PlayItem: TMenuItem
      Caption = '&Play'
      Enabled = False
      OnClick = PlayItemClick
    end
  end
  object SoundDialog: TOpenDialog
    Filter = 'Sound files (*.wav)|*.wav'
    FilterIndex = 0
    Left = 256
    Top = 40
  end
end