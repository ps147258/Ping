object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Ping'
  ClientHeight = 441
  ClientWidth = 697
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 713
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  DesignSize = (
    697
    441)
  TextHeight = 15
  object Label1: TLabel
    Left = 144
    Top = 7
    Width = 37
    Height = 21
    Caption = 'From'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 434
    Top = 7
    Width = 14
    Height = 21
    Caption = 'to'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 40
    Width = 57
    Height = 21
    Caption = 'Timeout'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 152
    Top = 40
    Width = 52
    Height = 21
    Caption = 'Interval'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 55
    Height = 21
    Caption = 'Adapter'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 448
    Top = 40
    Width = 102
    Height = 21
    Caption = 'History Length'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 288
    Top = 40
    Width = 86
    Height = 21
    Caption = 'Request size'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Button1: TButton
    Left = 616
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 69
    Width = 681
    Height = 218
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = #32048#26126#39636
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Chart1: TChart
    Left = 8
    Top = 295
    Width = 681
    Height = 121
    AllowPanning = pmNone
    BackWall.Visible = False
    BackWall.Emboss.Color = 8487297
    BackWall.Shadow.Color = 8487297
    BottomWall.Visible = False
    LeftWall.Visible = False
    Legend.Visible = False
    MarginBottom = 2
    MarginLeft = 2
    MarginRight = 4
    MarginUnits = muPixels
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Axis.Color = clScrollBar
    BottomAxis.Inverted = True
    BottomAxis.MinorTicks.Visible = False
    BottomAxis.Ticks.Visible = False
    LeftAxis.Axis.Visible = False
    LeftAxis.ExactDateTime = False
    LeftAxis.Increment = 1.000000000000000000
    LeftAxis.MaximumRound = True
    LeftAxis.MinorTicks.Visible = False
    LeftAxis.Ticks.Color = clDefault
    LeftAxis.Ticks.Visible = False
    LeftAxis.Title.Position = tpEnd
    RightAxis.Visible = False
    Shadow.Visible = False
    TopAxis.Visible = False
    View3D = False
    Zoom.Allow = False
    BevelOuter = bvNone
    TabOrder = 2
    Anchors = [akLeft, akRight, akBottom]
    OnResize = Chart1Resize
    DefaultCanvas = 'TGDIPlusCanvas'
    PrintMargins = (
      15
      40
      15
      40)
    ColorPaletteIndex = 0
    object Series1: TLineSeries
      HoverElement = [heCurrent]
      ColorEachPoint = True
      Marks.Transparent = True
      Marks.Style = smsValue
      SeriesColor = clBlack
      Shadow.Visible = False
      Brush.BackColor = clDefault
      ClickTolerance = 1
      DrawStyle = dsAll
      Pointer.HorizSize = 2
      Pointer.InflateMargins = True
      Pointer.Style = psCircle
      Pointer.VertSize = 2
      Pointer.Visible = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object ComboBox1: TComboBox
    Left = 192
    Top = 9
    Width = 236
    Height = 23
    TabOrder = 3
  end
  object ComboBox2: TComboBox
    Left = 456
    Top = 9
    Width = 236
    Height = 23
    TabOrder = 4
  end
  object Edit1: TEdit
    Left = 71
    Top = 40
    Width = 57
    Height = 23
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 5
    Text = '500'
  end
  object UpDown1: TUpDown
    Left = 128
    Top = 40
    Width = 16
    Height = 23
    Associate = Edit1
    Max = 6000
    Position = 500
    TabOrder = 6
  end
  object Edit2: TEdit
    Left = 208
    Top = 40
    Width = 57
    Height = 23
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 7
    Text = '1,000'
  end
  object UpDown2: TUpDown
    Left = 265
    Top = 40
    Width = 16
    Height = 23
    Associate = Edit2
    Max = 60000
    Position = 1000
    TabOrder = 8
  end
  object Button2: TButton
    Left = 69
    Top = 8
    Width = 60
    Height = 25
    Caption = 'Select'
    TabOrder = 9
    OnClick = Button2Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 422
    Width = 697
    Height = 19
    ParentCustomHint = False
    Panels = <
      item
        Width = 90
      end
      item
        Width = 80
      end
      item
        Width = 80
      end
      item
        Width = 80
      end
      item
        Width = 110
      end
      item
        Width = 50
      end>
    ParentShowHint = False
    ShowHint = True
    OnMouseLeave = StatusBar1MouseLeave
    OnMouseMove = StatusBar1MouseMove
  end
  object Edit3: TEdit
    Left = 556
    Top = 40
    Width = 33
    Height = 23
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 11
    Text = '120'
    OnExit = Edit3Exit
    OnKeyDown = Edit3KeyDown
  end
  object UpDown3: TUpDown
    Left = 589
    Top = 40
    Width = 16
    Height = 23
    Associate = Edit3
    Min = 60
    Max = 9999
    Position = 120
    TabOrder = 12
    OnChangingEx = UpDown3ChangingEx
  end
  object Edit4: TEdit
    Left = 384
    Top = 41
    Width = 38
    Height = 23
    Alignment = taRightJustify
    NumbersOnly = True
    TabOrder = 13
    Text = '32'
  end
  object UpDown4: TUpDown
    Left = 422
    Top = 41
    Width = 16
    Height = 23
    Associate = Edit4
    Max = 1500
    Position = 32
    TabOrder = 14
  end
  object IdThreadComponent1: TIdThreadComponent
    Active = False
    Loop = False
    Priority = tpNormal
    StopMode = smTerminate
    OnAfterExecute = IdThreadComponent1AfterExecute
    OnBeforeExecute = IdThreadComponent1BeforeExecute
    OnException = IdThreadComponent1Exception
    OnRun = IdThreadComponent1Run
    Left = 32
    Top = 80
  end
  object BalloonHint1: TBalloonHint
    Style = bhsStandard
    Delay = 250
    Left = 136
    Top = 80
  end
end
