object ElDesignMenu: TElDesignMenu
  Left = 403
  Top = 229
  Caption = 'ElMenu Designer'
  ClientHeight = 365
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 473
    Height = 365
    Align = alClient
    Caption = ' Menu Items '
    TabOrder = 0
    ExplicitWidth = 481
    ExplicitHeight = 370
    object Panel2: TPanel
      Left = 378
      Top = 15
      Width = 101
      Height = 353
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Bevel1: TBevel
        Left = 10
        Top = 221
        Width = 81
        Height = 5
        Shape = bsTopLine
      end
      object Bevel2: TBevel
        Left = 10
        Top = 93
        Width = 81
        Height = 5
        Shape = bsTopLine
      end
      object NewSubItemBtn: TButton
        Left = 10
        Top = 32
        Width = 81
        Height = 25
        Caption = 'New &Subitem'
        TabOrder = 0
        OnClick = NewSubItemBtnClick
      end
      object NewItemBtn: TButton
        Left = 10
        Top = 4
        Width = 81
        Height = 25
        Caption = '&New Item'
        TabOrder = 1
        OnClick = NewItemBtnClick
      end
      object DeleteItemBtn: TButton
        Left = 10
        Top = 60
        Width = 81
        Height = 25
        Caption = '&Delete'
        TabOrder = 2
        OnClick = DeleteItemBtnClick
      end
      object Load: TButton
        Left = 10
        Top = 231
        Width = 81
        Height = 25
        Caption = '&Load...'
        TabOrder = 3
        OnClick = LoadClick
      end
      object Save: TButton
        Left = 10
        Top = 259
        Width = 81
        Height = 25
        Caption = '&Save...'
        TabOrder = 4
        OnClick = SaveClick
      end
      object MoveUp: TButton
        Left = 10
        Top = 103
        Width = 81
        Height = 25
        Caption = 'Move &Up'
        TabOrder = 5
        OnClick = MoveUpClick
      end
      object MoveDown: TButton
        Left = 10
        Top = 131
        Width = 81
        Height = 25
        Caption = 'Move &Down'
        TabOrder = 6
        OnClick = MoveDownClick
      end
      object LevelUp: TButton
        Left = 10
        Top = 160
        Width = 81
        Height = 25
        Caption = '&<<'
        TabOrder = 7
        OnClick = LevelUpClick
      end
      object LevelDown: TButton
        Left = 10
        Top = 188
        Width = 81
        Height = 25
        Caption = '&>>'
        TabOrder = 8
        OnClick = LevelDownClick
      end
    end
  end
  object OpenMenuDlg: TOpenDialog
    DefaultExt = 'elm'
    Filter = 'ElMenu items file (*.elm)|*.elm'
    Left = 32
    Top = 276
  end
  object SaveMenuDlg: TSaveDialog
    DefaultExt = 'elm'
    Filter = 'ElMenu items file (*.elm)|*.elm'
    Left = 4
    Top = 276
  end
end