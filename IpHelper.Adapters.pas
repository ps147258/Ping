// Type: Windows network API Adapters.
// Author: 2022 Wei-Lun Huang
// Description: Retrieves the addresses associated with the adapters on the local computer.
//
// Features:
//   1. TAdapterScanThread
//      Execute GetAdaptersAddresses with a thread to get local Adapters information (IP_ADAPTER_ADDRESSES).
//   2. TAdaptersInfo
//      Obtain IP_ADAPTER_ADDRESSES through TAdapterScanThread, then analyze and list it.
//
// Tested in Delphi 10 Seattle.
//
// Last modified date: May 7, 2022.

unit IpHelper.Adapters;

interface

uses
  Winapi.Windows, Winapi.IpTypes, Winapi.IpHlpApi, Winapi.Winsock2, Winapi.MMSystem,
  System.SysUtils, System.Classes, System.Generics.Collections, System.Math,
  System.AnsiStrings,
  Vcl.ExtCtrls,
  IpHelper.Addresses;


const
  GAA_FLAG_INCLUDE_WINS_INFO = $0040;
  // Return addresses of Windows Internet Name Service (WINS) servers.
  // This flag is supported on Windows Vista and later.

  GAA_FLAG_INCLUDE_GATEWAYS = $0080;
  // Return the addresses of default gateways.
  // This flag is supported on Windows Vista and later.

  GAA_FLAG_INCLUDE_ALL_INTERFACES = $0100;
  // Return addresses for all NDIS interfaces.
  // This flag is supported on Windows Vista and later.

  GAA_FLAG_INCLUDE_ALL_COMPARTMENTS = $0200;
  // Return addresses in all routing compartments.
  // This flag is not currently supported and reserved for future use.

  GAA_FLAG_INCLUDE_TUNNEL_BINDINGORDER = $0400;
  // Return the adapter addresses sorted in tunnel binding order.
  // This flag is supported on Windows Vista and later.

  _Default_GAA_FLAG = GAA_FLAG_INCLUDE_GATEWAYS;
//    GAA_FLAG_SKIP_UNICAST or GAA_FLAG_SKIP_ANYCAST or GAA_FLAG_SKIP_MULTICAST or
//    GAA_FLAG_SKIP_DNS_SERVER or GAA_FLAG_SKIP_FRIENDLY_NAME;

type
  TAdaptersInfoState = (_AIM_Removed, _AIM_Added, _AIM_Changed, _AIM_Same);

  TAdapterAddresses = IP_ADAPTER_ADDRESSES;
  PAdapterAddresses = PIP_ADAPTER_ADDRESSES;

  TAdapterChangedEvent = procedure(ASender: TObject; State: TAdaptersInfoState;
    const AOld, ANew: TAdapterAddresses) of object;

  // 網路配接卡資訊，僅用於同步不同執行續間的資料存取
  TAdapterAddrSync = record
    State: TAdaptersInfoState;
    DataOld: PAdapterAddresses;
    DataNew: PAdapterAddresses;
    procedure SetValue(State: TAdaptersInfoState;
      DataOld, DataNew: PAdapterAddresses); overload; inline;
  end;
  PAdapterAddrSync = ^TAdapterAddrSync;

  TAdaptersInfoTable = TList<TAdapterAddrSync>;
  TAdapterBuffer = TArray<Byte>;

  TAdaptersAddressesProc = reference to procedure (Sender: TObject; var Buffer: TArray<Byte>; Size: ULONG; ErroCode: DWORD);

  // 執行 Windows API GetAdaptersAddresses 的執行續，因為所需時間較長，
  // 而該函數通常需要於背景運作，因此以 TThread 來方便使用。
  TAdapterScanThread = class(TThread)
  private
    FFinishProc: TAdaptersAddressesProc;
    FFamily: ULONG;
    FFlags: DWORD;
    FBuffer: TAdapterBuffer;
    FSize: ULONG;
    FErroCode: DWORD;
    function GetFirst: PIpAdapterInfo; inline;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFinishProc: TAdaptersAddressesProc; Family: ULONG; Flags: DWORD; DefaultSize: ULONG);

    property Family: ULONG read FFamily;
    property Flags: DWORD read FFlags;
    property ErroCode: DWORD read FErroCode;

    property Size: ULONG read FSize;
    property Buffer: TAdapterBuffer read FBuffer;
    property First: PIpAdapterInfo read GetFirst;
  end;

  TAdaptersInfoPointerList = TList<PAdapterAddresses>;

  // 網路配接卡資訊
  TAdaptersInfo = class(TComponent)
  private const
    _DefaultIntervalMS = 3000; // 預設的掃描間隔(毫秒)
    _MaxScanTimerMS = 50;      // 檢查執行續狀態的計時器間隔(毫秒)
  private
    FFamily: ULONG;                    // GetAdaptersAddresses 的目標 IP 類型
    FFlags: DWORD;                     // GetAdaptersAddresses 的查詢方式
    FBuffer: TAdapterBuffer;           // GetAdaptersAddresses 回傳多筆 IP_ADAPTER_ADDRESSES 的緩衝區
    FList: TAdaptersInfoPointerList;   // 配接卡資訊指標清單指向 FBuffer 中的每一筆 IP_ADAPTER_ADDRESSES
    FSyncBuffer: TAdapterAddrSync;     // 執行續資料同步用的緩衝區
    FOnDetected: TAdapterChangedEvent; // 發現每一筆 網路配接卡資訊 的動作
    FOnChanged: TNotifyEvent;          // 如果發現有變更的動作
    FScanIntervalMS: DWORD;            // 掃描間格
    FTimeoutStamp: DWORD;              // 內部使用的執行超時時間點
    FTimer: TTimer;                    // 用於檢查內部執行續運作狀態
    FActive: Boolean;                  // 掃描是否處於運作中
    FManualRefresh: Boolean;           // 標記是否處於手動更新 (執行續會等待子執行續執行結束)
    FScanner: TAdapterScanThread;      // 運作 GetAdaptersAddresses 的執行續

    procedure OnTimer(Sender: TObject);

    procedure SetFamily(Value: ULONG);
    procedure SetFlags(Value: DWORD);
    function GetCheckIntervalMS: DWORD;
    procedure SetCheckIntervalMS(Millisecond: DWORD);
    procedure SetScanIntervalMS(Millisecond: DWORD);

    procedure ResetTimeoutStamp; inline;
    function GetIsScanning: Boolean;
    procedure SetActive(Value: Boolean);

    function GetCount: Integer;
    function GetFirstPtr: PIpAdapterInfo;
    function GetFirst: TIpAdapterInfo;
    function GetItem(Index: Integer): TAdapterAddresses;

    procedure DoProcessAdapterList(Sender: TObject; var Buffer: TArray<Byte>; Size: ULONG; ErroCode: DWORD);
    procedure DoScanAdapter;

    procedure DoDetected;
    procedure DoChanged;
  public
    constructor Create(AOwner: TComponent; Active: Boolean = True); reintroduce;
    destructor Destroy; override;

    procedure Refresh; // 取得最新資訊

    property Family: ULONG read FFamily write SetFamily;
    property Flags: DWORD read FFlags write SetFlags;
    property IsScanning: Boolean read GetIsScanning;
    property CheckIntervalMS: DWORD read GetCheckIntervalMS write SetCheckIntervalMS;
    property ScanIntervalMS: DWORD read FScanIntervalMS write SetScanIntervalMS;
    property Active: Boolean read FActive write SetActive;
    property Count: Integer read GetCount;
    property First: TIpAdapterInfo read GetFirst;
    property Items[Index: Integer]: TAdapterAddresses read GetItem;
    property OnDetected: TAdapterChangedEvent read FOnDetected write FOnDetected;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

resourcestring
  _ErrSettingsAreLocked = 'Attribute %s cannot be changed when adapter change scanning.';
  _ErrParameterNegative = 'Parameter %s cannot be negative.';

{ TAdapterAddrSync }

procedure TAdapterAddrSync.SetValue(
  State: TAdaptersInfoState; DataOld, DataNew: PAdapterAddresses);
begin
  Self.State := State;
  Self.DataOld := DataOld;
  Self.DataNew := DataNew;
end;

{ TAdaptersScanThread }

constructor TAdapterScanThread.Create(const AFinishProc: TAdaptersAddressesProc; Family: ULONG; Flags: DWORD; DefaultSize: ULONG);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FFlags := Flags;
  FFamily := Family;
  FSize := DefaultSize;
  FFinishProc := AFinishProc;
end;

function TAdapterScanThread.GetFirst: PIpAdapterInfo;
begin
  Result := PIpAdapterInfo(PByte(FBuffer));
end;

procedure TAdapterScanThread.Execute;
var
  pBuf: PIP_ADAPTER_ADDRESSES;
begin
  // 預設暫存區大小
  FSize := Max(Length(FBuffer), SizeOf(IP_ADAPTER_ADDRESSES));
  // 建立暫存區並取得資訊
  repeat
    SetLength(FBuffer, FSize);
    pBuf := PIP_ADAPTER_ADDRESSES(PByte(FBuffer));
    FillChar(pBuf^, FSize, 0);
    FErroCode := GetAdaptersAddresses(FFamily, FFlags, nil, pBuf, @FSize);
  until (FErroCode <> ERROR_BUFFER_OVERFLOW);

  if Assigned(FFinishProc) then
    TThread.Synchronize(Self, procedure
    begin
      FFinishProc(Self, FBuffer, FSize, FErroCode);
    end);
end;

{ TAdaptersInfo }

constructor TAdaptersInfo.Create(AOwner: TComponent; Active: Boolean);
begin
  inherited Create(AOwner);

  FFamily := AF_UNSPEC;
  FFlags := _Default_GAA_FLAG;
  FList := TAdaptersInfoPointerList.Create;
  FScanIntervalMS := _DefaultIntervalMS;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := _MaxScanTimerMS;
  FTimer.OnTimer := OnTimer;

  SetActive(Active);
end;

destructor TAdaptersInfo.Destroy;
begin
  SetActive(False);
  FTimer.Enabled := False;
  if Assigned(FScanner) then
  begin
    FScanner.WaitFor;
    FScanner.Free;
  end;
  FTimer.Free;
  FList.Free;
  inherited;
end;

procedure TAdaptersInfo.SetFamily(Value: ULONG);
begin
  if FActive then
    raise Exception.CreateFmt(_ErrSettingsAreLocked, ['AdapterFamily']);
  FFamily := Value;
end;

procedure TAdaptersInfo.SetFlags(Value: DWORD);
begin
  if FActive then
    raise Exception.CreateFmt(_ErrSettingsAreLocked, ['AdapterFlags']);
  FFlags := Value;
end;

function TAdaptersInfo.GetCheckIntervalMS: DWORD;
begin
  Result := FTimer.Interval;
end;

procedure TAdaptersInfo.SetCheckIntervalMS(Millisecond: DWORD);
begin
  if Millisecond > _MaxScanTimerMS then
    FTimer.Interval := _MaxScanTimerMS
  else
    FTimer.Interval := Millisecond;
end;

procedure TAdaptersInfo.SetScanIntervalMS(Millisecond: DWORD);
begin
  if Millisecond <> FScanIntervalMS then
  begin
    if IsScanning then
      if Millisecond < FScanIntervalMS then
        FTimeoutStamp := FTimeoutStamp - FScanIntervalMS + Millisecond;

    FScanIntervalMS := Millisecond;
    SetCheckIntervalMS(Millisecond);
  end;
end;

procedure TAdaptersInfo.OnTimer(Sender: TObject);
var
  TimeStamp: DWORD;
begin
  if FScanIntervalMS > 0 then
  begin
    TimeStamp := timeGetTime;
    if FTimeoutStamp > TimeStamp then
      Exit
    else if FTimeoutStamp > (Cardinal.MaxValue - TimeStamp) then
      Exit;
    FTimeoutStamp := TimeStamp + FScanIntervalMS;
  end;

  if Assigned(FScanner) then
  begin
    if not FScanner.Finished then
      Exit;
    FreeAndNil(FScanner);
  end;

  if FActive then
    DoScanAdapter
  else
    TTimer(Sender).Enabled := False;
end;

procedure TAdaptersInfo.ResetTimeoutStamp;
begin
  FTimeoutStamp := timeGetTime + FScanIntervalMS;
end;

function TAdaptersInfo.GetIsScanning: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TAdaptersInfo.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if Value then
      DoScanAdapter;
  end;
end;

function TAdaptersInfo.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TAdaptersInfo.GetFirstPtr: PIpAdapterInfo;
begin
  Result := PIpAdapterInfo(FList.List);
end;

function TAdaptersInfo.GetFirst: TIpAdapterInfo;
begin
  Result := GetFirstPtr^;
end;

function TAdaptersInfo.GetItem(Index: Integer): TAdapterAddresses;
begin
  Result := FList.Items[Index]^;
end;

procedure TAdaptersInfo.DoProcessAdapterList(Sender: TObject; var Buffer: TArray<Byte>; Size: ULONG; ErroCode: DWORD);
var
  IsNew: Boolean;
  IsChange: Boolean;
  OldCount, MaxCount: Integer;
  I, J: Integer;
  pNew, pCurrNew, pCurrOld: PAdapterAddresses;
  Table: TAdaptersInfoTable;
  TableList: PAdapterAddrSync;
  ItemState: PAdapterAddrSync;
  DiffFlags: TAdapterAddrFlags;
begin
  case ErroCode of
    ERROR_SUCCESS: // 成功
    begin
      pNew := PAdapterAddresses(PByte(Buffer));
      OldCount := FList.Count;
      if OldCount > 0 then
      begin // 與已存在的清單做比對
        IsChange := False;
        Table := TAdaptersInfoTable.Create;
        try
          MaxCount := Max(OldCount, Ceil(Size / SizeOf(IP_ADAPTER_INFO)));
          Table.Count := MaxCount;
          TableList := PAdapterAddrSync(Table.List);

          //
          // 初始標記清除旗標
          //
          for I := 0 to OldCount - 1 do
          begin
            {$POINTERMATH ON}
            ItemState := @TableList[I];
            {$POINTERMATH OFF}
            ItemState.State := _AIM_Removed;
            ItemState.DataOld := FList.Items[I];
            ItemState.DataNew := nil;
          end;

          //
          // 如果有已有相同配接卡名稱存在，則比對與設定 相同 或是 變更 的旗標
          //
          OldCount := FList.Count;
          J := OldCount;
          pCurrNew := pNew;
          while Assigned(pCurrNew) do
          begin
            IsNew := True;
            for I := 0 to OldCount - 1 do
            begin
              {$POINTERMATH ON}
              ItemState := @TableList[I];
              {$POINTERMATH OFF}
              pCurrOld := ItemState.DataOld;
              if ItemState.State <> _AIM_Removed then
                Continue;
              // 比對 AdaptersAddresses，如果項目不相同則會設定該項目的旗標。
              // Simplify = True 遇到相異則立即退出並回傳旗標。
              DiffFlags := CompareAdaptersAddressesFirst(pCurrOld^, pCurrNew^, True);
              if DiffFlags = [] then // 全部相同
              begin
                IsNew := False;
                ItemState.State := _AIM_Same;
                ItemState.DataNew := pCurrNew;
                Break;
              end;
              if not (_AAF_AdapterName in DiffFlags) then // 配接卡名稱相同
              begin
                IsChange := True;
                IsNew := False;
                ItemState.State := _AIM_Changed;
                ItemState.DataNew := pCurrNew;
                Break;
              end;
            end;
            if IsNew then
            begin
              IsChange := True;
              {$POINTERMATH ON}
              ItemState := @TableList[J];
              {$POINTERMATH OFF}
              ItemState.SetValue(_AIM_Added, nil, pCurrNew);
              Inc(J);
            end;
            pCurrNew := pCurrNew.Next;
          end;

          //
          // 如有異動則更新清單資訊
          //
          if IsChange then
          begin
            if MaxCount > J then
              MaxCount := J;

            J := 0;
            for I := 0 to MaxCount - 1 do
            begin
              {$POINTERMATH ON}
              FSyncBuffer := TableList[I];
              {$POINTERMATH OFF}

              case FSyncBuffer.State of
                _AIM_Removed: FList.Delete(J); // 刪除項目，但維持清單索引位置
                _AIM_Added:
                begin // 增加項目，前往下一個清單索引
                  FList.Add(FSyncBuffer.DataNew);
                  Inc(J);
                end;
                _AIM_Changed, _AIM_Same:
                begin // 覆蓋項目，前往下一個清單索引
                  FList.Items[J] := FSyncBuffer.DataNew;
                  Inc(J);
                end;
              end;

              if FSyncBuffer.State <> _AIM_Same then
                DoDetected;
            end;
            FBuffer := Buffer;
          end;

        finally
          Table.Free;
          if IsChange then
            DoChanged;
        end;
      end
      else
      begin // 建立清單並將資訊填入至清單
        try
          FBuffer := Buffer;
          pCurrNew := pNew;
          while Assigned(pCurrNew) do
          begin
            FList.Add(pCurrNew);
            FSyncBuffer.SetValue(_AIM_Added, nil, pCurrNew);
            DoDetected;
            pCurrNew := pCurrNew.Next;
          end;
        finally
          DoChanged;
        end;
      end;
    end;

    ERROR_NO_DATA: // 無任何介面卡資訊
    begin
      OldCount := FList.Count;
      if OldCount > 0 then
      begin
        for I := 0 to OldCount - 1 do
        begin
          FSyncBuffer.SetValue(_AIM_Removed, nil, FList.Items[I]);
          DoDetected;
        end;
        FList.Clear;
        SetLength(FBuffer, 0);
        DoChanged;
      end;
    end;

    else RaiseLastOSError; // 若失敗則產生例外
  end;
end;

procedure TAdaptersInfo.DoScanAdapter;
begin
  if Assigned(FScanner) then
    Exit;

  FScanner := TAdapterScanThread.Create(DoProcessAdapterList, FFamily, FFlags, Length(FBuffer));
  FScanner.Start;

  ResetTimeoutStamp;
  FTimer.Enabled := True;
end;

procedure TAdaptersInfo.DoDetected;
begin
  if Assigned(FOnDetected) then
    FOnDetected(Self, FSyncBuffer.State, FSyncBuffer.DataOld^, FSyncBuffer.DataNew^);
end;

procedure TAdaptersInfo.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TAdaptersInfo.Refresh;
begin
  if FActive then
  begin // 當循環掃描啟動時等待正在處理的掃描結束
    if IsScanning then
    begin
      if Assigned(FScanner) then
        FScanner.WaitFor;
      Exit;
    end;
  end
  else
  begin // 當不在循環掃時使用建立新的掃描執行續，並且等待完成
    FManualRefresh := True;
    try
      if Assigned(FScanner) then
        Exit;
      FScanner := TAdapterScanThread.Create(DoProcessAdapterList, FFamily, FFlags, Length(FBuffer));
      FScanner.Start;
      FScanner.WaitFor;
      FreeAndNil(FScanner);
    finally
      FManualRefresh := False;
    end;
  end;
end;

end.
