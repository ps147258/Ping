// Type: Ping tool - Application main form.
// Author: 2022 Wei-Lun Huang
// Description: Application Ping main form.
//
// Features:
//   The specified target address is pinged from the specified local address,
//   and the response status is counted.
//
// Tested in Delphi 10 Seattle.
//
// Last modified date: May 10, 2022.

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.CommCtrl,
  Winapi.WinSock, Winapi.MMSystem, Winapi.IpExport, Winapi.Winsock2,
  System.SysUtils, System.Variants, System.Classes, System.Math, System.AnsiStrings,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart,
  IdBaseComponent, IdThreadComponent,
  IpHelper.Addresses, IpHelper.Adapters, IpHelper.IcmpPing;

type
  TForm1 = class(TForm)
    IdThreadComponent1: TIdThreadComponent;

    Panel1: TPanel;
      Label5: TLabel;
      Button2: TButton;
      Label1: TLabel;
      ComboBox1: TComboBox;
      Label2: TLabel;
      ComboBox2: TComboBox;
      Label3: TLabel;
      Edit1: TEdit;
      UpDown1: TUpDown;
      Label4: TLabel;
      Edit2: TEdit;
      UpDown2: TUpDown;
      Label7: TLabel;
      Edit4: TEdit;
      UpDown4: TUpDown;
      Label6: TLabel;
      Edit3: TEdit;
      UpDown3: TUpDown;

    Splitter1: TSplitter;

    PageControl1: TPageControl;
      TabSheet1: TTabSheet;
        Memo1: TMemo;

    Splitter2: TSplitter;

    Chart1: TChart;
      Series1: TLineSeries;

    StatusBar1: TStatusBar;
    Button1: TButton;
    BalloonHint1: TBalloonHint;
    TabSheet2: TTabSheet;
    ListView1: TListView;
    ScrollBar1: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure StatusBar1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure StatusBar1MouseLeave(Sender: TObject);
    procedure PageControl1Resize(Sender: TObject);
    procedure Chart1Resize(Sender: TObject);
    procedure Series1ClickPointer(Sender: TCustomSeries; ValueIndex, X, Y: Integer);
    procedure UpDown3ChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    procedure Edit3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit3Exit(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ScrollBar1Change(Sender: TObject);
    procedure IdThreadComponent1Exception(Sender: TIdThreadComponent; AException: Exception);
    procedure IdThreadComponent1BeforeExecute(Sender: TIdThreadComponent);
    procedure IdThreadComponent1Run(Sender: TIdThreadComponent);
    procedure IdThreadComponent1AfterExecute(Sender: TIdThreadComponent);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private type
    TStatusBarHint = (_SBH_Pings, _SBH_Fails, _SBH_Loss, _SBH_Rate, _SBH_Times, _SBH_Reply);
    TStatusBarHintCllass = (_SBHC_Title, _SBHC_Description);

    // ListView1 字串緩衝
    PListViewString = ^TListViewString;
    TListViewString = record
      Title: string;
      Text: string;
    end;
    TListViewStrings = TList<TListViewString>; // ListView1 字串緩衝
  private const
    cStatusBarHints: array[TStatusBarHint, TStatusBarHintCllass] of string = (
      ('Number of pings'   , 'Number of executions of Windows API ICMP Ping.'), // _SBH_Pings - 總數
      ('Number of failures', 'API failure and package loss.'),                  // _SBH_Fails - 失敗數 (API 錯誤 與 回應失敗)
      ('Number of loss'    , 'The status of the response was unsuccessful.'),   // _SBH_Loss  - 回應失敗數
      ('Rate of Failures'  , 'Rate of the API failure and package loss.'),      // _SBH_Rate  - 回應失敗率
      ('Times of timeout'  , 'The response status is timeout or RTT timeout.'), // _SBH_Times - 超時數
      ('Response time'     , 'Minimum, maximum and average by RTT.'));          // _SBH_Reply - 回應時間狀態
  private
    { Private declarations }
    LastPingError: Cardinal;
    LastStr1: string;      // 最後第一項的訊息
    LastStr2: string;      // 最後第二項的訊息
    LogsBuff: TStringList; // 訊息緩衝，用於同步執行續
    FirstStatusIndex: Integer; // 近期歷史訊息的起位置
    ListViewTopIndex: Integer;         // ListView1 字串快取所指向的起始索引
    ListViewStrings: TListViewStrings; // ListView1 顯示區的字串快取

    procedure AddToComboBox(ComboBox: TComboBox; const s: string); inline;
    procedure AddToComboBox1(const s: string); inline;
    procedure AddToComboBox2(const s: string); inline;
    procedure AddAdapterAddressesToComboBox;
    function ListViewScrollTo(ItemIndex: Integer; Select: Boolean = False): Boolean;
    function IsMemoScrollBottom: Boolean;      // 檢查 Memo1 檢視區是否處於最後一行
    function IsListViewScrollBottom: Boolean;  // 檢查 ListView1 檢視區是否處於最後一行
    function StatusPanel(ID: TStatusBarHint): TStatusPanel; inline;
    function GetStatusIndexBy(X: Integer): Integer; inline; // 座標所指向的狀態欄位索引
    procedure RefreshScrollBarPageSize;             // 更新歷史圖表位置拉條
    procedure SetHistoryLength(NewLength: Integer); // 設定圖表表達長度
    procedure ResetHistory;                         // 圖表保留長度但數值清除
    procedure SstControlEnabled(State: Boolean);    // 部分介面停用或啟用
    procedure RefreshSeriesPointer;                 // 以圖表節點密集度切換節點顯示
    procedure RefreshSeries;                        // 更新圖表

    function InsertTimeStamp(const s: string): string; inline;
    procedure FlushLogsBuff;  // 將訊息緩衝排至 Memo1
    procedure ClearReplyInfo; // 清除歷史訊息
    function AddReplyInfo(const ReplyInfo: TReplyInfo): Integer; // 加入歷史訊息

    procedure FormatInfo(var ValueOut: TListViewString; const ValueIn: TReplyInfo);
    procedure SyncListView; // 同步 ListView1 顯示區字串快取區

    procedure SyncOnBefore; // 執行續起始時
    procedure SyncOnAfter;  // 執行續結束後
    procedure SyncOnBegin;  // 執行續循環階段開始前
    procedure SyncOnStatus; // 執行續循環階段結束後
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Unit2, Debug;

resourcestring
  errStatusHintOver = 'Value [%d] is outside the recognized range of type TStatusBarHint.';

type
  TReplyInfoList = TList<TReplyInfo>;

var
  Ping: TPing = nil;                    // Windows API IcmpSendEcho 的包裝物件
  FormIP, ToIP: TSockAddr;              // sockaddr 資訊
  FormAddrStr, ToAddrStr: string;       // 位置字串
  TimeoutMS: DWORD = _DefaultTimeoutMS; // API 超時時間(毫秒)
  IntervalMS: DWORD = 1000;             // 執行間格(毫秒)
  RequestSize: DWORD = 32;              // 要發送的測試訊息長度
  HistoryLength: Integer = 120;         // 圖表長度
  Replies: DWORD;                       // Ping API 的回應數
  LastTime: Cardinal;                   // 下次更新的時間
  ReplyInfo: TReplyInfo;                // 回應狀態資訊
  ReplyInfoList: TReplyInfoList = nil;  // 回應狀態清單


function CheckScrollBottom(AWinControl: TWinControl): Boolean; //inline;
var
  si: TScrollInfo;
begin
  si.cbSize := SizeOf(si);
  si.fMask := SIF_ALL;
  if GetScrollInfo(AWinControl.Handle, SB_VERT, si) then
    Result := si.nPos + Integer(si.nPage) = si.nMax + 1
  else
    Result := False;
end;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AMajor, AMinor, ABuild: Cardinal;
begin
  Splitter1.MinSize := Panel1.Height;
  Splitter2.MinSize := Chart1.Height;

  LogsBuff := TStringList.Create;
  Ping := TPing.Create;
  ComboBox1.Clear;

  TimeoutMS := UpDown1.Position;
  IntervalMS := UpDown2.Position;

  ReplyInfoList := TReplyInfoList.Create;

  ScrollBar1.Position := ScrollBar1.Min;
  ScrollBar1.Max := ScrollBar1.Min;
  ScrollBar1.Enabled := False;
  Series1.Clear;
  SetHistoryLength(UpDown3.Position);

  ListViewTopIndex := 0;
  ListViewStrings := TListViewStrings.Create;

  if GetProductVersion(ParamStr(0), AMajor, AMinor, ABuild) then
    Caption := Format('%s v%u.%u.%u '+{$IFDEF WIN64}'Win64'{$ELSE}'Win32'{$ENDIF}, [Caption, AMajor, AMinor, ABuild]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if not IdThreadComponent1.Stopped then
    IdThreadComponent1.TerminateAndWaitFor;
  Ping.Free;
  LogsBuff.Free;
  ListViewStrings.Free;
  ReplyInfoList.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  Unicast, Gateway: TSockAddr;
begin
  Form2.RefreshAdapter;
  AddAdapterAddressesToComboBox;
  if Form2.GetFirstAddress(Unicast, Gateway) then
  begin
    ComboBox1.Text := AddressToString(Unicast);
    ComboBox2.Text := AddressToString(Gateway);
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Form2.Showing then
    Form2.ModalResult := mrCancel;
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
  BalloonHint1.HideHint;
end;

procedure TForm1.FormMouseLeave(Sender: TObject);
begin
  BalloonHint1.HideHint;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Panel1.Top := 0;
end;

procedure TForm1.Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  if NewSize > TSplitter(Sender).MinSize then
    NewSize := TSplitter(Sender).MinSize
  else if NewSize < TSplitter(Sender).MinSize then
    NewSize := 0;
end;

procedure TForm1.StatusBar1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  {$J+}
  LastX: Integer = -1;
  LastIndex: Integer = -1;
  {$J-}
var
  I: Integer;
  Point: TPoint;
begin
  if LastX = X then
    Exit;
  LastX := X;

  I := GetStatusIndexBy(X);

  if LastIndex = I then
    if BalloonHint1.ShowingHint then
      Exit;
  LastIndex := I;

  if I < 0 then
    Exit;

  if BalloonHint1.ShowingHint then
    BalloonHint1.HideHint;

  Point := StatusBar1.ClientToScreen(TPoint.Create(X, Y));

  BalloonHint1.Title := cStatusBarHints[TStatusBarHint(I), _SBHC_Title];
  BalloonHint1.Description := cStatusBarHints[TStatusBarHint(I), _SBHC_Description];
  BalloonHint1.ShowHint(Point);
end;

procedure TForm1.StatusBar1MouseLeave(Sender: TObject);
begin
  BalloonHint1.HideHint;
end;

procedure TForm1.PageControl1Resize(Sender: TObject);
begin
  if Splitter1.Top < Splitter1.MinSize then
    Button1.Top := Splitter1.Top
  else
    Button1.Top := Panel1.Height - 8 - Button1.Height;
end;

procedure TForm1.Chart1Resize(Sender: TObject);
begin
  RefreshSeriesPointer;
end;

procedure TForm1.Series1ClickPointer(Sender: TCustomSeries; ValueIndex, X, Y: Integer);
var
  I, J: Integer;
  Count: Integer;
  PageSize: Integer;
begin
  I := ScrollBar1.Position;
  Count := ReplyInfoList.Count;
  PageSize := Series1.Count;
  J := Count - PageSize;
  if I > J then
    I := J;
  I := PageSize - 1 - ValueIndex + I;
  ListViewScrollTo(I, True);
end;

procedure TForm1.UpDown3ChangingEx(Sender: TObject; var AllowChange: Boolean;
  NewValue: Integer; Direction: TUpDownDirection);
begin
  SetHistoryLength(NewValue);
end;

procedure TForm1.Edit3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Edit: TEdit absolute Sender;
begin
  case Key of
    VK_RETURN:
    begin
      SetHistoryLength(UpDown3.Position);
      if string(Edit.Text).ToInteger <> UpDown3.Position then
        Edit.Text := UpDown3.Position.ToString;
    end;
  end;
end;

procedure TForm1.Edit3Exit(Sender: TObject);
var
  Edit: TEdit absolute Sender;
begin
  SetHistoryLength(UpDown3.Position);
  if string(Edit.Text).ToInteger <> UpDown3.Position then
    Edit.Text := UpDown3.Position.ToString;
end;

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
var
  StrItem: PListViewString;
  iItem, I: Integer;
begin
  SyncListView;

  iItem := Item.Index;
  I := iItem - ListViewTopIndex;

  if I < 0 then
    Exit;
  if I >= ListViewStrings.Count then
    Exit;

  StrItem := @ListViewStrings.List[I];
  Item.Caption := PChar(StrItem.Title);
  if Item.SubItems.Count > 0 then
    Item.SubItems[0] := StrItem.Text
  else
    Item.SubItems.Add(StrItem.Text);
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  I: Integer;
begin
  case ScrollCode of
    scTrack:
    begin
      I := TScrollBar(Sender).Max - TScrollBar(Sender).PageSize;
      if ScrollPos > I then
        ScrollPos := I;
    end;
  end;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
var
  ScrollBar: TScrollBar absolute Sender;
  iStrat, iEnd, iMax: Integer;
begin
  RefreshSeries;
  iStrat := ScrollBar.Position;
  iEnd := iStrat + HistoryLength;
  iMax := ReplyInfoList.Count - 1;
  if iEnd > iMax then
    iEnd := iMax;

  ScrollBar.Hint := DateTimeToStr(ReplyInfoList.Items[iStrat].Timestamp) +
                    ' ~ ' +
                    DateTimeToStr(ReplyInfoList.Items[iEnd].Timestamp);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Item: TListItem;
begin
  // 設定視窗為至於主視窗的中央
  Form2.Left := Self.Left + (Self.Width - Form2.Width) div 2;
  Form2.Top := Self.Top + (Self.Height - Form2.Height) div 2;

  try
    // 呼叫顯示 通訊位址選擇視窗 與暫停此介面的處理，直至所呼叫的視窗關閉
    if Form2.ShowModal <> mrOk then
      Exit;

    // 取得選擇的清單項目，不存在則退出處理
    Item := Form2.ListView1.Selected;
    if not Assigned(Item) then
      Exit;

    // 如清單缺少子項目則發出例外
    if Item.SubItems.Count < 1 then
      raise Exception.Create('Adapter list error.');
  finally
    AddAdapterAddressesToComboBox;
  end;

  // 設定 來源 與 目地 位址
  ComboBox1.Text := Item.Caption;
  ComboBox2.Text := Item.SubItems[0];
end;

procedure TForm1.AddToComboBox(ComboBox: TComboBox; const s: string);
begin
  if ComboBox.Items.IndexOf(s) < 0 then
    ComboBox.Items.Add(s);
end;

procedure TForm1.AddToComboBox1(const s: string);
begin
  AddToComboBox(ComboBox1, s);
end;

procedure TForm1.AddToComboBox2(const s: string);
begin
  AddToComboBox(ComboBox2, s);
end;

procedure TForm1.AddAdapterAddressesToComboBox;
var
  ListItems: TListItems;
  ListItem: TListItem;
  I: Integer;
begin
  ComboBox1.Items.BeginUpdate;
  ComboBox2.Items.BeginUpdate;
  try
    ListItems := Form2.ListView1.Items;
    for I := 0 to ListItems.Count - 1 do
    begin
      ListItem := ListItems[I];
      AddToComboBox1(ListItem.Caption);
      AddToComboBox2(ListItem.SubItems[0]);
    end;
  finally
    ComboBox2.Items.EndUpdate;
    ComboBox1.Items.EndUpdate;
  end;
end;

function TForm1.ListViewScrollTo(ItemIndex: Integer; Select: Boolean): Boolean;
var
  si: TScrollInfo;
  TopR, R: TRect;
  ItemHeight: Integer;
  Item: TListItem;

  function GetItemDisplayRect(Index: Integer): TRect;
  begin
    Result.left := LVIR_BOUNDS;
    ListView1.Perform(LVM_GETITEMRECT, Index, LPARAM(@Result));
  end;
begin
  Result := False;

  si.cbSize := SizeOf(si);
  si.fMask := SIF_ALL;
  if not GetScrollInfo(ListView1.Handle, SB_VERT, si) then
    Exit;

  try
    TopR := GetItemDisplayRect(si.nPos);
    R := GetItemDisplayRect(ItemIndex);

    if R.Top < TopR.Top then
    begin
      ListView1.Scroll(R.Left, R.Top - TopR.Top);
      Result := True;
    end
    else
    begin
      ItemHeight := ListView1.ClientHeight;
      if R.Bottom > ItemHeight then
      begin
        ListView1.Scroll(R.Left, R.Top - (ItemHeight - TopR.Top));
        Result := True;
      end;
    end;
  finally
    if Select then
    begin
      Item := ListView1.Items[ItemIndex];
      Item.Selected := True;
      ListView1.SetFocus;
    end;
    ListView1.Invalidate;
  end;
end;

function TForm1.IsMemoScrollBottom: Boolean;
begin
  Result := CheckScrollBottom(Memo1);
end;

function TForm1.IsListViewScrollBottom: Boolean;
begin
  Result := CheckScrollBottom(ListView1);
end;

function TForm1.StatusPanel(ID: TStatusBarHint): TStatusPanel;
begin
  Result := StatusBar1.Panels[Integer(ID)];
end;

function TForm1.GetStatusIndexBy(X: Integer): Integer;
var
  I, W: Integer;
  Panels: TStatusPanels;
begin
  if (X < 0) or (X > StatusBar1.ClientWidth) then
    Exit(-1);

  I := 0;
  W := 0;
  Panels := StatusBar1.Panels;
  while I < Panels.Count do
  begin
    Inc(W, Panels[I].Width);
    if W > X then
      Exit(I);
    Inc(I);
  end;
  Result := Panels.Count - 1;
end;

procedure TForm1.SetHistoryLength(NewLength: Integer);
var
  I, Count: Integer;
begin
  HistoryLength := NewLength;
  Series1.BeginUpdate;
  try
    Count := Series1.Count;
    if Count > NewLength then
      Series1.Delete(NewLength, Count - NewLength)
    else if Count < NewLength then
      for I := Count to NewLength do
        Series1.AddNull(I);
    RefreshSeriesPointer;
  finally
    Series1.EndUpdate;
  end;
  Chart1.Refresh;
  RefreshScrollBarPageSize;
end;

procedure TForm1.ResetHistory;
var
  I: Integer;
begin
  Series1.BeginUpdate;
  try
    for I := 0 to Series1.Count - 1 do
      Series1.SetNull(I);
  finally
    Series1.EndUpdate;
  end;
//  ScrollBar1.Min := 0;
  ScrollBar1.PageSize := 0;
  ScrollBar1.Position := ScrollBar1.Min;
  ScrollBar1.Max := ScrollBar1.Min;
end;

procedure TForm1.RefreshScrollBarPageSize;
var
  PageSize: Integer;
begin
  PageSize := HistoryLength + 1;
  ScrollBar1.Enabled := ReplyInfoList.Count > PageSize;
  if ScrollBar1.Enabled then
  begin
    ScrollBar1.Max := ReplyInfoList.Count - 1;
    ScrollBar1.PageSize := PageSize;
  end;
end;

procedure TForm1.SstControlEnabled(State: Boolean);
var
  P: Cardinal;
begin
  Button2.Enabled   := State;
  ComboBox1.Enabled := State;
  ComboBox2.Enabled := State;
  Edit1.Enabled     := State;
  UpDown1.Enabled   := State;
  Edit2.Enabled     := State;
  UpDown2.Enabled   := State;
  Edit4.Enabled     := State;
  UpDown4.Enabled   := State;

  if State then
  begin // 展開面板
    P := (Splitter1.Top shl 16) or (Splitter1.Width shr 1);
    Splitter1.Perform(WM_LBUTTONDOWN, MK_LBUTTON, P);
    Splitter1.Perform(WM_LBUTTONUP, MK_LBUTTON, P);
    Splitter1.Perform(WM_LBUTTONDOWN, MK_LBUTTON, P);
    Splitter1.Perform(WM_LBUTTONUP, MK_LBUTTON, P);
    if Splitter1.Top < Splitter1.MinSize then
    begin // 如果未達預期結果則直接設定控制項大小位置
      Panel1.SetBounds(0, 0, Panel1.Width, Splitter1.MinSize);
      Splitter1.Top := Splitter1.MinSize;
    end;
  end
  else
  begin // 收起面板
    P := (Splitter1.MinSize shl (16 - 1)) or (Splitter1.Width shr 1);
    Splitter1.Perform(WM_LBUTTONDOWN, MK_LBUTTON, (Splitter1.Top shl 16) or (Splitter1.Width shr 1));
    Splitter1.Perform(WM_MOUSEMOVE, MK_LBUTTON, P);
    Splitter1.Perform(WM_LBUTTONUP, MK_LBUTTON, P);
    if Splitter1.Top >= Splitter1.MinSize then
    begin // 如果未達預期結果則直接設定控制項大小位置
      Panel1.SetBounds(0, 0, Panel1.Width, 0);
      Splitter1.Top := 0;
    end;
  end;
end;

procedure TForm1.RefreshSeriesPointer;
begin
  Series1.Pointer.Visible := (Chart1.ClientWidth div HistoryLength) > (Series1.Pointer.Size * 2 + 2);
end;

procedure TForm1.RefreshSeries;
var
  I, J, iEnd: Integer;
  pReply: PReplyInfo;
begin
  //
  // 更新回應時間圖表
  //
  iEnd := ScrollBar1.Position + HistoryLength;
  if iEnd >= ReplyInfoList.Count then
    iEnd := ReplyInfoList.Count - 1;

  Series1.BeginUpdate;
  try
    for I := 0 to HistoryLength do
    begin
      if I < ReplyInfoList.Count then
      begin
        J := iEnd - I;
        pReply := @ReplyInfoList.List[J];
        if pReply.IsValid then
        begin
          Series1.YValue[I] := pReply.RoundTripTime;
          Series1.ValueColor[I] := clGreen;
        end
        else
        begin
          Series1.SetNull(I);
        end;
      end
      else if I < Series1.Count then
        Series1.SetNull(I)
      else
        Series1.AddNull();
    end;

  finally
    Series1.EndUpdate;
  end;
end;

function TForm1.InsertTimeStamp(const s: string): string;
const
  cFormat = 'mm/dd hh:nn:ss.zzz ';
begin
  Result := FormatDateTime(cFormat, Now) + s;
end;

procedure TForm1.FlushLogsBuff;
begin
  if LogsBuff.Count = 0 then
    Exit;

  Memo1.Lines.AddStrings(LogsBuff);
  LogsBuff.Clear;
end;

procedure TForm1.ClearReplyInfo;
begin
  ScrollBar1.Position := ScrollBar1.Min;
  ScrollBar1.Max := ScrollBar1.Min;
  ScrollBar1.Enabled := False;
  ListView1.Clear;
  ReplyInfoList.Clear;
end;

function TForm1.AddReplyInfo(const ReplyInfo: TReplyInfo): Integer;
var
  IsScrollBottom: Boolean;
begin
  ReplyInfoList.Add(ReplyInfo);

  IsScrollBottom := ScrollBar1.Position >= ScrollBar1.Max - ScrollBar1.PageSize;
  RefreshScrollBarPageSize;
  if IsScrollBottom then
    ScrollBar1.Position := ScrollBar1.Max;

  ListView1.Items.BeginUpdate;
  IsScrollBottom := IsListViewScrollBottom;
  try
    Result := ListView1.Items.Count;
    // 設定 ListView1.Items.Count 並只重繪受影響的顯示區且不做焦點捲動
    // 若直接設定 ListView1.Items.Count，由於未指定 LVSICF_NOSCROLL 因此會導致捲動至焦點 Item
    ListView1.Perform(LVM_SETITEMCOUNT, Result + 1, LVSICF_NOINVALIDATEALL or LVSICF_NOSCROLL);
  finally
    ListView1.Items.EndUpdate;
  end;
  if IsScrollBottom then
    ListViewScrollTo(Result);
end;

procedure TForm1.FormatInfo(var ValueOut: TListViewString; const ValueIn: TReplyInfo);
begin
  ValueOut.Title := FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', ValueIn.Timestamp);
  ValueOut.Text := ValueIn.GetErrorMessage;
  if ValueOut.Text.IsEmpty then
    ValueOut.Text := Format('Replies: %u, RoundTripTime: %ums.', [ValueIn.Replies, ValueIn.RoundTripTime]);
end;

procedure TForm1.SyncListView;
var
  si: TScrollInfo;
  Info: TReplyInfo;
  StrItem: TListViewString;
  I, J: Integer;
  procedure Delete(Index: Integer);
  var
    Temp: TListViewString;
  begin
    Temp := ListViewStrings.Items[Index];
    Temp.Title := '';
    Temp.Text := '';
    ListViewStrings.Delete(Index);
  end;
begin
  si.cbSize := SizeOf(si);
  si.fMask := SIF_ALL;
  if not GetScrollInfo(ListView1.Handle, SB_VERT, si) then
    Exit;

  if si.nPos < ListViewTopIndex then
  begin
    for I := 0 to Min(ListViewTopIndex - si.nPos, ListViewStrings.Count) - 1 do
      Delete(ListViewStrings.Count - 1);
    for I := ListViewStrings.Count to si.nPage - 1 do
    begin
      J := si.nPos + I;
      Info := ReplyInfoList[J];
      FormatInfo(StrItem, Info);
      ListViewStrings.Insert(0, StrItem);
    end;
  end
  else
  begin
    for I := 0 to Min(si.nPos - ListViewTopIndex, ListViewStrings.Count) - 1 do
      Delete(0);
    for I := ListViewStrings.Count to si.nPage - 1 do
    begin
      J := si.nPos + I;
      Info := ReplyInfoList[J];
      FormatInfo(StrItem, Info);
      ListViewStrings.Add(StrItem);
    end;
  end;
  ListViewTopIndex := si.nPos;
end;

procedure TForm1.SyncOnBefore;
begin
  SstControlEnabled(False);
  Button1.Caption := 'Stop';
  Memo1.Clear;
  LastStr1 := '';
  LastStr2 := '';
  FirstStatusIndex := -1;

  FormAddrStr := ComboBox1.Text;
  ToAddrStr := ComboBox2.Text;

  TimeoutMS := UpDown1.Position;
  IntervalMS := UpDown2.Position;
  RequestSize := UpDown4.Position;

  Replies := 0;
  ResetHistory;

  ClearReplyInfo;
end;

procedure TForm1.SyncOnAfter;
begin
  Memo1.Lines.Add(InsertTimeStamp('Terminated.'));
  FormAddrStr := '';
  ToAddrStr := '';
  Button1.Caption := 'Start';
  SstControlEnabled(True);
end;

procedure TForm1.SyncOnBegin;
begin
  Memo1.Lines.BeginUpdate;
  try
    FlushLogsBuff; // 將所有日誌緩衝區的訊息全部輸入至 Memo1 中，然後清除日誌緩衝區
    FirstStatusIndex := Memo1.Lines.Count;
  finally
    Memo1.Lines.EndUpdate;
  end;

  if not IdThreadComponent1.Terminated then
    AddToComboBox2(ComboBox2.Text);
end;

procedure TForm1.SyncOnStatus;
var
  Scrolling: Boolean;
  I: Integer;
  IcmpEcho: PIcmpEchoReplyEx;
  s: string;
  function GetRoundTripTimeStatus(const P: TPing): string; inline;
  var
    Min, Max, Average: string;
    function FormatRTT(Value: Word): string; overload; inline;
    begin
      if Value = 0 then
        Result := '<1' // 只是表示最小無限接近 0 因為時間不可能等於 0
      else
        Result := Value.ToString;
    end;
  begin
    if P.RttMin > P.RttMax then
      Exit('Min: ?ms, Max: ?ms, Average: ?ms');

    Min := FormatRTT(P.RttMin);
    Max := FormatRTT(P.RttMax);
    Average := P.RttAverage.ToString(ffGeneral, 1, 3);
    Result := Format('Min: %sms, Max: %sms, Average: %sms', [Min, Max, Average]);
  end;
begin
  AddReplyInfo(ReplyInfo);
  s := ReplyInfo.GetErrorMessage;   // 取得錯誤訊息
  IcmpEcho := Ping.EchoReply;

  RefreshSeries;

  if s.IsEmpty then
  begin
    // 回應結果的訊息
    case ReplyInfo.Family of
    AF_INET:
      s := Format(
        'Reply from %s, RoundTripTime: %ums, Size: %u, TTL: %u, TOS: %u, Flags: %u.', [
        AddressToString(ReplyInfo.Address.v4), ReplyInfo.RoundTripTime, Ping.RequestSize,
        IcmpEcho.v4.Options.Ttl, IcmpEcho.v4.Options.Tos, IcmpEcho.v4.Options.Flags]);
    AF_INET6:
      s := Format(
        'Reply from %s, RoundTripTime: %ums.', [
        AddressToString(ReplyInfo.Address.v6), ReplyInfo.RoundTripTime]);
    else s := '';
    end;
  end;

  // 檢查目前 Memo1 檢視區域是否顯示最後一行
  Scrolling := IsMemoScrollBottom;

  // 輸出訊息，只保留最近的訊息
  Memo1.Lines.BeginUpdate;
  try
    for I := Memo1.Lines.Count - HistoryLength downto FirstStatusIndex do
      Memo1.Lines.Delete(I);
    Memo1.Lines.Add(InsertTimeStamp(s));
  finally
    Memo1.Lines.EndUpdate;
  end;
  LastStr2 := LastStr1;
  LastStr1 := s;
  LastPingError := Ping.Error;

  // 如果原本檢視區域位置是處於最後一行則，重新捲動至最後一行
  if Scrolling then
    Memo1.Perform(EM_SCROLL, SB_BOTTOM, 0);

  // 更新狀態顯示
  StatusBar1.Panels.BeginUpdate;
  try
    StatusPanel(_SBH_Pings).Text := 'Pings: ' + Ping.Times.ToString;
    StatusPanel(_SBH_Fails).Text := 'Fails: ' + Ping.Fails.ToString;
    StatusPanel(_SBH_Loss).Text  := 'Lost: ' + Ping.Lost.ToString;
    StatusPanel(_SBH_Times).Text := 'Timeouts: ' + Ping.Timeouts.ToString;
    StatusPanel(_SBH_Rate).Text  := 'Loss: ' + Ping.LossRate.ToString(ffGeneral, 3, 0) + '%';
    StatusPanel(_SBH_Reply).Text := GetRoundTripTimeStatus(Ping);
  finally
    StatusBar1.Panels.EndUpdate;
  end;
end;

procedure TForm1.IdThreadComponent1Exception(Sender: TIdThreadComponent; AException: Exception);
begin
  Memo1.Lines.Add('Exception: ' + AException.Message);
end;

procedure TForm1.IdThreadComponent1BeforeExecute(Sender: TIdThreadComponent);
var
  Hints: TAddrInfoW;
  ToAddresses: TAddrInfoList;
  pInfo: PAddrInfo;
  Option: TIpOptionInformation;
  I: Integer;
  b: Boolean;
begin
  // 與主執行續同步，通知主執行續做初始化前的作業
  Sender.Synchronize(SyncOnBefore);

  try
    // 如果有沒有輸入 來源 位址則 標記此執行續停止 並 退出處理
    if ToAddrStr.IsEmpty then
    begin
      LogsBuff.Add('Missing input source or destination address.');
      Sender.Terminate;
      Exit;
    end;

    // 如果 來源位址 轉換失敗則 標記此執行續停止 並 退出處理
    FillChar(FormIP, SizeOf(FormIP), 0);
    if not StringToAddress(FormAddrStr, FormIP) then
    begin
      LogsBuff.Add('Unable to get address data corresponding to source address string.');
      Sender.Terminate;
      Exit;
    end;

    // 如果目地 網域 或 位址 轉換至 sockaddr 結果為 0 則 標記此執行續停止 並 退出處理
    FillChar(ToIP, SizeOf(ToIP), 0);
    if not StringToAddress(ToAddrStr, ToIP) then
    begin
      FillChar(Hints, SizeOf(Hints), 0);
      Hints.ai_family := FormIP.family;
      Hints.ai_socktype := SOCK_STREAM;
      Hints.ai_protocol := IPPROTO_TCP;

      ToAddresses := nil;
      try
        ToAddresses := GetHostAddress(Hints, ToAddrStr);
        b := Assigned(ToAddresses);
        if b then
          b := ToAddresses.Count > 0;

        if not b then // 沒有任何符合的位址資訊
        begin
          Sender.Terminate;
          LogsBuff.Add(Format(
            'Domain %s IP: <No IP found with the same source family %s>.', [
            ToAddrStr, GetFamilyStr(Hints.ai_family, True)]));
          Exit;
        end;

        case ToAddresses.Count of
          1:   // 只有單筆位址資訊
          begin
            ToIP := ToAddresses.List[0].addr;
            LogsBuff.Add(Format('Domain %s IP[%d]: %s', [ToAddrStr,
              ToAddresses.Count, AddressToString(ToIP)]));
          end;
          else // 多筆位址資訊
          begin
            LogsBuff.Add(Format('Domain %s IP[%d]: ', [ToAddrStr, ToAddresses.Count]));
            b := True;
            for I := 0 to ToAddresses.Count - 1 do
            begin
              pInfo := @ToAddresses.List[I];
              LogsBuff.Add(Format('%s', [AddressToString(pInfo.addr)]));
              if b then
              begin
                b := False;
                ToIP := pInfo.addr;
              end;
            end;
          end;
        end;
      finally
        if Assigned(ToAddresses) then
          FreeAndNil(ToAddresses);
      end;
    end;

    //
    // 設定 Ping 參數
    //
    Ping.TimeoutMS := TimeoutMS;
    Ping.IcmpCreate(FormIP, ToIP);
    Ping.CreatRequest(RequestSize);

    LogsBuff.Add(Format(
      'Ping form %s to %s, RequestSize: %ubytes, Timeout %ums, Interval %ums.', [
      AddressToString(FormIP), AddressToString(ToIP), RequestSize, TimeoutMS, IntervalMS]));
    if Ping.GetIpOptions(Option) then // 如 Ping 的 IpOptions 有被設定時
    begin
      LogsBuff.Add(Format(
        'Options: Ttl %d, Tos %d, Flags 0x%0.2X.', [
        Option.Ttl, Option.Tos, Option.Flags]));
    end;
  finally
    // 與主執行續同步，通知主執行續初始化作業結束
    Sender.Synchronize(SyncOnBegin);
  end;

  LastTime := timeGetTime - IntervalMS;
end;

procedure TForm1.IdThreadComponent1Run(Sender: TIdThreadComponent);
var
  Curr, N: Cardinal;
begin
  // 取得時間並且計算時間長度
  Curr := timeGetTime;
  if Curr >= LastTime then
    N := Curr - LastTime
  else
    N := Cardinal.MaxValue - LastTime + Curr;

  if N > IntervalMS then
  begin
    Inc(LastTime, IntervalMS);

    // 執行 Ping，結束後回傳 回應數
    Replies := Ping.SendEcho;
    Ping.GetReplyInfo(ReplyInfo);

    // 與主執行續同步，更新介面狀態
    IdThreadComponent1.Synchronize(SyncOnStatus);
  end;

  // 暫停執行
  if IntervalMS < 200 then
    Sleep(IntervalMS div 4)
  else
    Sleep(50);
end;

procedure TForm1.IdThreadComponent1AfterExecute(Sender: TIdThreadComponent);
begin
  // 關閉 Ping 的 Icmp 通訊
  Ping.IcmpClose;
  // 與主執行續同步，通知主執行續子執行續作業已完成
  Sender.Synchronize(SyncOnAfter);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not IdThreadComponent1.Stopped then
  begin
    IdThreadComponent1.TerminateAndWaitFor;
    Exit;
  end;
  IdThreadComponent1.Start;
end;

end.
