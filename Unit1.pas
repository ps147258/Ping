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
// Last modified date: May 7, 2022.

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.WinSock, Winapi.MMSystem,
  Winapi.IpExport, Winapi.Winsock2,
  System.SysUtils, System.Variants, System.Classes, System.Math, System.AnsiStrings,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart,
  IdBaseComponent, IdThreadComponent,
  IpHelper.Addresses, IpHelper.Adapters, IpHelper.IcmpPing;

type
  TForm1 = class(TForm)
    IdThreadComponent1: TIdThreadComponent;
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

    Button1: TButton;
    Memo1: TMemo;
    Series1: TLineSeries;
    Chart1: TChart;
    StatusBar1: TStatusBar;
    Label6: TLabel;
    Edit3: TEdit;
    UpDown3: TUpDown;
    Label7: TLabel;
    Edit4: TEdit;
    UpDown4: TUpDown;
    BalloonHint1: TBalloonHint;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure StatusBar1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure StatusBar1MouseLeave(Sender: TObject);
    procedure Chart1Resize(Sender: TObject);
    procedure UpDown3ChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
    procedure Edit3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit3Exit(Sender: TObject);
    procedure IdThreadComponent1Exception(Sender: TIdThreadComponent; AException: Exception);
    procedure IdThreadComponent1BeforeExecute(Sender: TIdThreadComponent);
    procedure IdThreadComponent1Run(Sender: TIdThreadComponent);
    procedure IdThreadComponent1AfterExecute(Sender: TIdThreadComponent);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private type
    TStatusBarHint = (_SBH_Pings, _SBH_Fails, _SBH_Loss, _SBH_Rate, _SBH_Times, _SBH_Reply);
  private const
    cStatusBarHints: array[TStatusBarHint] of string = (
      'Number of pings.',    // _SBH_Pings
      'Number of failures.', // _SBH_Fails
      'Number of loss.',     // _SBH_Loss
      'Packet Loss Rate.',   // _SBH_Rate
      'Timeout times.',      // _SBH_Times
      'Response time.');     // _SBH_Reply
  private
    { Private declarations }
    LastPingError: Cardinal;
    LastStr1: string;
    LastStr2: string;
    LogsBuff: TStringList;

    function IsMemoScrollBottom: Boolean;
    function StatusPanel(ID: TStatusBarHint): TStatusPanel; inline;
    function GetStatusIndexBy(X: Integer): Integer; inline;
    procedure SetHistoryLength(NewLength: Integer);
    procedure ResetHistory;
    procedure SstControlEnabled(State: Boolean);
    procedure RefreshSeriesPointer; inline;

    procedure FlushLogsBuff;

    procedure SyncOnBefore;
    procedure SyncOnAfter;
    procedure SyncOnBegin;
    procedure SyncOnStatus;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Unit2;//, HighAccuracyGauge;

resourcestring
  errStatusHintOver = 'Value [%d] is outside the recognized range of type TStatusBarHint.';

var
  Ping: TPing = nil;
  AppThread: THandle;
  FormIP, ToIP: TSockAddr;
  FormAddrStr, ToAddrStr: string;
  TimeoutMS: DWORD = _DefaultTimeoutMS;
  IntervalMS: DWORD = 1000;
  RequestSize: DWORD = 32;
  HistoryLength: Integer = 120;
  Replies: DWORD;
  LastTime: Cardinal;
//  Gauge: TPerformanceGauge;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AMajor, AMinor, ABuild: Cardinal;
begin
  if GetProductVersion(ParamStr(0), AMajor, AMinor, ABuild) then
    Caption := Format('%s v%u.%u.%u '+{$IFDEF WIN64}'Win64'{$ELSE}'Win32'{$ENDIF}, [Application.Title, AMajor, AMinor, ABuild]);
  LogsBuff := TStringList.Create;
  Ping := TPing.Create;
  AppThread := GetCurrentThread;
  ComboBox1.Clear;

  TimeoutMS := UpDown1.Position;
  IntervalMS := UpDown2.Position;

  Series1.Clear;
  SetHistoryLength(UpDown3.Position);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if not IdThreadComponent1.Stopped then
    IdThreadComponent1.TerminateAndWaitFor;
  Ping.Free;
  LogsBuff.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  Unicast, Gateway: TSockAddr;
begin
  Form2.RefreshAdapter;
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

  BalloonHint1.Title := cStatusBarHints[TStatusBarHint(I)];
  BalloonHint1.Description := '';
  BalloonHint1.ShowHint(Point);
end;

procedure TForm1.StatusBar1MouseLeave(Sender: TObject);
begin
  BalloonHint1.HideHint;
end;

procedure TForm1.Chart1Resize(Sender: TObject);
begin
  RefreshSeriesPointer;
end;

procedure TForm1.UpDown3ChangingEx(Sender: TObject; var AllowChange: Boolean;
  NewValue: Integer; Direction: TUpDownDirection);
begin
  SetHistoryLength(NewValue);
end;

procedure TForm1.Edit3KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: SetHistoryLength(UpDown3.Position);
  end;
end;

procedure TForm1.Edit3Exit(Sender: TObject);
begin
  SetHistoryLength(UpDown3.Position);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Item: TListItem;
begin
  Form2.Left := Self.Left + (Self.Width - Form2.Width) div 2;
  Form2.Top := Self.Top + (Self.Height - Form2.Height) div 2;

  if Form2.ShowModal <> mrOk then
    Exit;

  Item := Form2.ListView1.Selected;
  if not Assigned(Item) then
    Exit;

  if Item.SubItems.Count < 1 then
    raise Exception.Create('Adapter list error.');

  ComboBox1.Text := Item.Caption;
  ComboBox2.Text := Item.SubItems[0];
end;

function TForm1.IsMemoScrollBottom: Boolean;
var
  si: TScrollInfo;
begin
  si.cbSize := SizeOf(si);
  si.fMask := SIF_ALL;
  if GetScrollInfo(Memo1.Handle, SB_VERT, si) then
    Result := si.nPos + Integer(si.nPage) = si.nMax + 1
  else
    Result := False;
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
    begin
      Result := I;
      Exit;
    end;
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
        Series1.AddNull(I);//(I, -1, '', clRed);
  finally
    Series1.EndUpdate;
  end;
  Chart1.Refresh;
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
end;

procedure TForm1.SstControlEnabled(State: Boolean);
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
end;

procedure TForm1.RefreshSeriesPointer;
begin
  Series1.Pointer.Visible := (Chart1.ClientWidth div UpDown3.Position) > (Series1.Pointer.Size * 2 + 2);
end;

procedure TForm1.FlushLogsBuff;
begin
  if LogsBuff.Count = 0 then
    Exit;

  Memo1.Lines.AddStrings(LogsBuff);
  LogsBuff.Clear;
end;

procedure TForm1.SyncOnBefore;
begin
  SstControlEnabled(False);
  Button1.Caption := 'Stop';
  Memo1.Clear;
  LastStr1 := '';
  LastStr2 := '';

  FormAddrStr := ComboBox1.Text;
  ToAddrStr := ComboBox2.Text;

  TimeoutMS := UpDown1.Position;
  IntervalMS := UpDown2.Position;
  RequestSize := UpDown4.Position;

  Replies := 0;
  ResetHistory;
end;

procedure TForm1.SyncOnAfter;
begin
  FormAddrStr := '';
  ToAddrStr := '';
  Button1.Caption := 'Start';
  SstControlEnabled(True);
end;

procedure TForm1.SyncOnBegin;
begin
  Memo1.Lines.BeginUpdate;
  try
    FlushLogsBuff;
    Memo1.Lines.Add(Format(
      'Ping form %s to %s, Timeout %ums, Interval %ums.', [
      AddressToString(FormIP), AddressToString(ToIP), TimeoutMS, IntervalMS]));
    Memo1.Lines.Add(Format(
      'Request size: %ubytes, Options: Ttl %d, Tos %d, Flags 0x%0.2X.', [
      Ping.RequestSize, Ping.Options.Ttl, Ping.Options.Tos, Ping.Options.Flags]));
  finally
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TForm1.SyncOnStatus;
var
  Scrolling: Boolean;
  I, J: Integer;
  pReply: PIcmpEchoReplyEx;
  s: string;
  function InsertTimeStamp(const s: string): string; inline;
  const
    cFormat = 'mm/dd hh:nn:ss.zzz ';
  begin
    Result := FormatDateTime(cFormat, Now) + s;
  end;
  function GetRoundTripTimeStatus(const P: TPing): string; inline;
    function FormatRTT(Value: Word): string; overload; inline;
    begin
      if Value = 0 then
        Result := '<1'
      else
        Result := Value.ToString;
    end;
  begin
    Result := 'Min: '+FormatRTT(P.RttMin)+'ms, Max: '+FormatRTT(P.RttMax)+'ms, Average: '+
      P.RttAverage.ToString(ffGeneral, 1, 3)+'ms';
  end;
begin
  s := Ping.ErrorMessage;
  pReply := Ping.EchoReply;
  Series1.BeginUpdate;
  try
    for I := Series1.Count - 2 downto 0 do
    begin
      Series1.YValue[I + 1] := Series1.YValue[I];
      Series1.ValueColor[I + 1] := Series1.ValueColor[I];
    end;
    I := -1;
    if s.IsEmpty then
    begin
      case Ping.Family of
      AF_INET : if pReply.v4.Status = IP_SUCCESS then I := pReply.v4.RoundTripTime;
      AF_INET6: if pReply.v6.Status = IP_SUCCESS then I := pReply.v6.RoundTripTime;
      end;
    end;

    if I < 0 then
      Series1.SetNull(0)
    else
    begin
      Series1.YValue[0] := I;
      Series1.ValueColor[0] := clGreen;
    end;
  finally
    Series1.EndUpdate;
  end;

  if s.IsEmpty then
  begin
    case Ping.Family of
    AF_INET:
      s := Format(
        'Reply from %s, RoundTripTime: %ums, Size: %u, TTL: %u, TOS: %u, Flags: %u.', [
        AddressToString(pReply.v4.Address), pReply.v4.RoundTripTime, pReply.v4.DataSize,
        pReply.v4.Options.Ttl, pReply.v4.Options.Tos, pReply.v4.Options.Flags]);
    AF_INET6:
      s := Format(
        'Reply from %s, RoundTripTime: %ums.', [
        AddressToString(pReply.v6.Address.sin6_addr), pReply.v6.RoundTripTime]);
    else s := '';
    end;
  end;

  Scrolling := IsMemoScrollBottom;

  Memo1.Lines.BeginUpdate;
  try
    J := Memo1.Lines.Count - 1;
    if LastStr1.IsEmpty or (LastStr1 <> LastStr2) then
      Memo1.Lines.Add(InsertTimeStamp(s))
    else
      Memo1.Lines[J] := InsertTimeStamp(s);
  finally
    Memo1.Lines.EndUpdate;
  end;

  LastStr2 := LastStr1;
  LastStr1 := s;
  LastPingError := Ping.Error;

  if Scrolling then
    Memo1.Perform(EM_SCROLL, SB_BOTTOM, 0);

  StatusBar1.Panels.BeginUpdate;
  try
    StatusPanel(_SBH_Pings).Text := 'Pings: ' + Ping.Times.ToString;
    StatusPanel(_SBH_Fails).Text := 'Fails: ' + Ping.Fails.ToString;
    StatusPanel(_SBH_Loss).Text  := 'Lost: ' + Ping.Fails.ToString;
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
  I: Integer;
  b: Boolean;
begin
  Sender.Synchronize(SyncOnBefore);

  ToAddresses := nil;
  try
    FillChar(FormIP, SizeOf(FormIP), 0);
    if ToAddrStr.IsEmpty or not StringToAddress(FormAddrStr, FormIP) then
    begin
      LogsBuff.Add('');
      Sender.Terminate;
      Exit;
    end;

    FillChar(ToIP, SizeOf(ToIP), 0);
    if not StringToAddress(ToAddrStr, ToIP) then
    begin
      FillChar(Hints, SizeOf(Hints), 0);
      Hints.ai_family := FormIP.family;
      Hints.ai_socktype := SOCK_STREAM;
      Hints.ai_protocol := IPPROTO_TCP;

      ToAddresses := GetHostAddress(Hints, ToAddrStr);
      if not Assigned(ToAddresses) then
      begin
        Sender.Terminate;
        Exit;
      end;

      case ToAddresses.Count of
        0:
        begin
          ToIP := ToAddresses.List[0].addr;
          LogsBuff.Add(Format(
            'Domain %s IP[%d]: <No IP found with the same source family %s>.', [
            ToAddrStr, ToAddresses.Count, GetFamilyStr(Hints.ai_family, True)]));
          Sender.Terminate;
          Exit;
        end;
        1:
        begin
          ToIP := ToAddresses.List[0].addr;
          LogsBuff.Add(Format('Domain %s IP[%d]: %s', [ToAddrStr,
            ToAddresses.Count, AddressToString(ToIP)]));
        end;
        else
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
    end;
  finally
    if Assigned(ToAddresses) then
      FreeAndNil(ToAddresses);
  end;

  Ping.TimeoutMS := TimeoutMS;
  Ping.IcmpCreate(FormIP, ToIP);
  Ping.CreatRequest(RequestSize);

  Sender.Synchronize(SyncOnBegin);

  LastTime := timeGetTime - IntervalMS;
end;

procedure TForm1.IdThreadComponent1Run(Sender: TIdThreadComponent);
var
  Curr, N: Cardinal;
begin
  Curr := timeGetTime;

  if Curr >= LastTime then
    N := Curr - LastTime
  else
    N := Cardinal.MaxValue - LastTime + Curr;

  if N > IntervalMS then
  begin
    Inc(LastTime, IntervalMS);

    Replies := Ping.SendEcho;

    IdThreadComponent1.Synchronize(SyncOnStatus);
  end;

  if IntervalMS < 200 then
    Sleep(IntervalMS div 4)
  else
    Sleep(50);
end;

procedure TForm1.IdThreadComponent1AfterExecute(Sender: TIdThreadComponent);
begin
  Ping.IcmpClose;
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
