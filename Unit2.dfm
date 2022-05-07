object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Adapters'
  ClientHeight = 273
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  DesignSize = (
    625
    273)
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 529
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Double-click or press the [Enter] key to select the item and clo' +
      'se the window.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object ListView1: TListView
    Left = 9
    Top = 40
    Width = 608
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Unicast'
        Width = 290
      end
      item
        Caption = 'Gateway'
        Width = 290
      end>
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    GridLines = True
    GroupView = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListView1Change
    OnDblClick = ListView1DblClick
    OnKeyDown = ListView1KeyDown
  end
  object Button1: TButton
    Left = 543
    Top = 9
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Select'
    Enabled = False
    TabOrder = 1
    OnClick = Button1Click
  end
end
