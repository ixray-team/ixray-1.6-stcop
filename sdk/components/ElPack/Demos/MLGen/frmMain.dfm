object MainForm: TMainForm
  Left = 410
  Top = 259
  AutoScroll = False
  Caption = 'MlGen Demo'
  ClientHeight = 456
  ClientWidth = 537
  Color = clBtnFace
  Constraints.MinHeight = 383
  Constraints.MinWidth = 543
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Width = 3
    Height = 435
    Cursor = crHSplit
  end
  object StatusLine: TStatusBar
    Left = 0
    Top = 435
    Width = 537
    Height = 21
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 435
    Align = alLeft
    TabOrder = 0
    object Label3: TLabel
      Left = 28
      Top = 96
      Width = 70
      Height = 13
      Caption = 'Lines on page:'
    end
    object Label4: TLabel
      Left = 8
      Top = 204
      Width = 62
      Height = 13
      Caption = 'Output file(s):'
    end
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 47
      Height = 13
      Caption = 'Template:'
    end
    object LinesOnPageEdit: TSpinEdit
      Left = 28
      Top = 112
      Width = 69
      Height = 22
      MaxValue = 200
      MinValue = 1
      TabOrder = 3
      Value = 10
    end
    object HeaderCheckBox: TCheckBox
      Left = 8
      Top = 152
      Width = 137
      Height = 17
      Caption = 'Header on each page'
      TabOrder = 4
    end
    object FooterCheckBox: TCheckBox
      Left = 8
      Top = 172
      Width = 137
      Height = 17
      Caption = 'Footer on each page'
      TabOrder = 5
    end
    object GenerateButton: TButton
      Left = 56
      Top = 284
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'Generate'
      TabOrder = 8
      OnClick = GenerateButtonClick
    end
    object MultipageCheckBox: TCheckBox
      Left = 8
      Top = 76
      Width = 137
      Height = 17
      Caption = 'Multipage'
      TabOrder = 2
      OnClick = MultipageCheckBoxClick
    end
    object OutputFilesEdit: TEdit
      Left = 8
      Top = 220
      Width = 173
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      OnChange = OutputFilesEditChange
    end
    object BrowseOutputButton: TButton
      Left = 104
      Top = 248
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Browse'
      TabOrder = 7
      OnClick = BrowseOutputButtonClick
    end
    object TemplateEdit: TEdit
      Left = 8
      Top = 24
      Width = 173
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = TemplateEditChange
    end
    object BrowseTemplateBtn: TButton
      Left = 104
      Top = 48
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Browse'
      TabOrder = 1
      OnClick = BrowseTemplateBtnClick
    end
    object OpenResultBtn: TButton
      Left = 56
      Top = 332
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'Open result'
      TabOrder = 9
      OnClick = OpenResultBtnClick
    end
  end
  object PageControl1: TPageControl
    Left = 188
    Top = 0
    Width = 349
    Height = 435
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Template'
      object Label5: TLabel
        Left = 4
        Top = 8
        Width = 102
        Height = 13
        Caption = 'Template parametres:'
      end
      object Label6: TLabel
        Left = 4
        Top = 164
        Width = 86
        Height = 13
        Caption = 'Translation tables:'
      end
      object TemplateParamsListBox: TListBox
        Left = 4
        Top = 24
        Width = 332
        Height = 128
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
      end
      object TranslationTableNamesListBox: TListBox
        Left = 4
        Top = 180
        Width = 89
        Height = 221
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 1
        OnClick = TranslationTableNamesListBoxClick
      end
      object TranslationTableListBox: TListBox
        Left = 96
        Top = 180
        Width = 238
        Height = 221
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Sample data'
      ImageIndex = 1
      object Label1: TLabel
        Left = 4
        Top = 4
        Width = 62
        Height = 13
        Caption = 'Sample data:'
      end
      object StringGrid: TStringGrid
        Left = 4
        Top = 20
        Width = 332
        Height = 353
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 4
        DefaultColWidth = 70
        DefaultRowHeight = 17
        RowCount = 10
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
        TabOrder = 0
        RowHeights = (
          17
          17
          16
          17
          17
          17
          17
          17
          17
          17)
      end
      object AddRowBtn: TButton
        Left = 8
        Top = 380
        Width = 75
        Height = 21
        Hint = 'Add row'
        Anchors = [akLeft, akBottom]
        Caption = 'Add row'
        TabOrder = 1
        OnClick = AddRowBtnClick
      end
      object DeleteRowBtn: TButton
        Left = 92
        Top = 380
        Width = 75
        Height = 21
        Hint = 'Delete row'
        Anchors = [akLeft, akBottom]
        Caption = 'Delete row'
        TabOrder = 2
        OnClick = DeleteRowBtnClick
      end
      object AddColBtn: TButton
        Left = 176
        Top = 380
        Width = 75
        Height = 21
        Hint = 'Add col'
        Anchors = [akLeft, akBottom]
        Caption = 'Add col'
        TabOrder = 3
        OnClick = AddColBtnClick
      end
      object DeleteColBtn: TButton
        Left = 260
        Top = 380
        Width = 75
        Height = 21
        Hint = 'Delete col'
        Anchors = [akLeft, akBottom]
        Caption = 'Delete col'
        TabOrder = 4
        OnClick = DeleteColBtnClick
      end
    end
  end
  object OpenTemplateDialog: TOpenDialog
    DefaultExt = '.templ'
    Filter = 'Template files|*.templ'
    Title = 'Please select template'
    Left = 124
    Top = 148
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.txt'
    Filter = 
      'Text files (*.txt)|*.txt|HTML files (*.html)|*.html|All files (*' +
      '.*)|*.*'
    Title = 'Output file(s)'
    Left = 160
    Top = 156
  end
end
