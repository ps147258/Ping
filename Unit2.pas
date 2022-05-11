// Type: Adapter Selection form.
// Author: 2022 Wei-Lun Huang
// Description: Adapter Selection.
//
// Features:
//   List adapters addresses, and select addresses by user.
//
// Tested in Delphi 10 Seattle.
//
// Last modified date: May 7, 2022.

unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.IpTypes, Winapi.Winsock2,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  IpHelper.Addresses, IpHelper.Adapters;

type
  TForm2 = class(TForm)
    ListView1: TListView;
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    // AdapterAddresses 分類配對
    procedure OnAdapterDetected(ASender: TObject;
      State: TAdaptersInfoState; const AOld, ANew: TAdapterAddresses);

    procedure OnAdapterChanged(Sender: TObject); // 整理介面清單
    procedure DoSelected;                        // 若已選擇則返回
    procedure RefreshAdapter;                    // 重新整理 Adapter 列表
    // 取得清單中的第一筆 (如有 IPv4 則優先)
    function GetFirstAddress(var Unicast, Gateway: TSockAddr): Boolean;
    // 取得清單中指定索引的項目
    function GetAddress(Index: Integer; var Unicast, Gateway: TSockAddr): Boolean;
    // 取得清單中選擇的項目
    function GetSelect(var Unicast, Gateway: TSockAddr): Boolean;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

var
  AdapterAddressList: TAdapterAddressList = nil;
  AdaptersInfo: TAdaptersInfo = nil;

procedure TForm2.FormCreate(Sender: TObject);
begin
  AdapterAddressList := TAdapterAddressList.Create;
  AdaptersInfo := TAdaptersInfo.Create(Self, False);
  AdaptersInfo.Flags := GAA_FLAG_INCLUDE_GATEWAYS;
  AdaptersInfo.OnDetected := OnAdapterDetected;
  AdaptersInfo.OnChanged := OnAdapterChanged;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  AdapterAddressList.Free;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  AdaptersInfo.Active := True;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AdaptersInfo.Active := False;
end;

procedure TForm2.OnAdapterDetected(ASender: TObject; State: TAdaptersInfoState;
  const AOld, ANew: TAdapterAddresses);
var
  AdapterName, FriendlyName: string;
  p: PAdapterAddresses;
  Unicast: PIP_ADAPTER_UNICAST_ADDRESS;
  Gateway: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
  UnicastList: TSockAddrList;
  GatewayList: TSockAddrList;
  pUnicast: PSockAddr;
  pGateway: PSockAddr;
  b: Boolean;
  I, J: Integer;
begin

  UnicastList := TSockAddrList.Create; // 本機位址臨時清單 (方便後面的Family配對)
  GatewayList := TSockAddrList.Create; // 閘道位址臨時清單 (方便後面的Family配對)
  try
    case State of
      _AIM_Removed: p := @AOld;
      _AIM_Added  : p := @ANew;
      _AIM_Changed: p := @ANew;
      else Exit;
    end;
    AdapterName := string(p.AdapterName);
    FriendlyName := WideCharToString(p.FriendlyName);

    // 將Unicast位址清單化(臨時清單)
    Unicast := p.FirstUnicastAddress;
    while Assigned(Unicast) do
    begin
      if (Unicast.Union.Flags < IP_ADAPTER_ADDRESS_TRANSIENT) and
        (Unicast.DadState = IpDadStatePreferred) then
      begin
        case Unicast.Address.lpSockaddr.sa_family of
          AF_INET, AF_INET6: UnicastList.Add(Unicast.Address);
        end;
      end;
      Unicast := Unicast.Next;
    end;

    // 將Gateway位址清單化(臨時清單)
    Gateway := p.FirstGatewayAddress;
    while Assigned(Gateway) do
    begin
      case Gateway.Address.lpSockaddr.sa_family of
        AF_INET, AF_INET6: GatewayList.Add(Gateway.Address);
      end;
      Gateway := Gateway.Next;
    end;

    // 當 State 為已移除或變更時，刪除 AdapterName 對應的 AdapterAddress 項目
    case State of
      _AIM_Removed, _AIM_Changed:
      begin
        J := 0;
        repeat
          J := AdapterAddressList.Find(AdapterName, J);
          if J >= 0 then
            AdapterAddressList.Delete(J);
        until (J < 0);
      end;
    end;

    // 當 State 為新增或變更時，將位址與Family相對應的方式加入清單
    // 通常一個 Adapter 會有一個 IPv4 或 IPv6 或 兩者皆有
    // 也不排除會有更多，但目前只針對只有一組 [IPv4] 或 [IPv6] 或 [IPv4 與 IPv6]
    case State of
      _AIM_Added, _AIM_Changed:
      begin
        I := 0;
        while I < UnicastList.Count do
        begin
          b := True;
          pUnicast := @UnicastList.List[I];
          J := 0;
          while J < GatewayList.Count do
          begin
            pGateway := @GatewayList.List[J];
            // 當 Unicast 與 Gateway 的 Family 不相同時繼續處理下一筆 Gateway
            if pUnicast.base.sin_family <> pGateway.base.sin_family then
            begin
              Inc(J);
              Continue;
            end;
            // 當 Unicast 與 Gateway 有相同的 Family 時視為一組將其加入清單
            // 然後刪除以加入 AdapterAddressList 的 Unicast 與 Gateway 臨時清單
            AdapterAddressList.Add(AdapterName, FriendlyName, pUnicast^, pGateway^);
            UnicastList.Delete(I);
            GatewayList.Delete(J);
            b := False;
            Break;
          end;
          if b then
            Inc(I);
        end;
      end;
    end;

  finally
    GatewayList.Free;
    UnicastList.Free;
  end;
end;

procedure TForm2.OnAdapterChanged(Sender: TObject);
var
  p: PAdapterAddress;
  GroupCount, ItemCount: Integer;
  I, J, iGroup, iItem: Integer;
  GroupId: Integer;
  b: Boolean;
  bGroup: array of Boolean;
  bItem: array of Boolean;
  Group: TListGroup;
  Item: TListItem;
  s, sUnicast, sGateway: string;
begin
  iGroup := 0;
  iItem := 0;
  GroupCount := ListView1.Groups.Count;
  SetLength(bGroup, GroupCount);
  FillChar(PBoolean(bGroup)^, GroupCount * SizeOf(Boolean), 0);
  ItemCount := ListView1.Items.Count;
  SetLength(bItem, ItemCount);
  FillChar(PBoolean(bItem)^, ItemCount * SizeOf(Boolean), 0);
  for I := 0 to AdapterAddressList.Count - 1 do
  begin
    p := @AdapterAddressList.List[I];
    s := p.FriendlyName;
    sUnicast := AddressToString(p.Unicast);
    sGateway := AddressToString(p.Gateway);

    //
    // 下面搜尋 Group 與 Item 各使用兩次迴圈原意是打算以程式碼長度換取
    // 某些狀況下時的效率增進，
    // 以上一次的索引作為起始搜尋一次，如沒找到則再由0搜到停止前一個的位置。
    // 但一台電腦中通常不會有太多的 網路配接卡(Adapter) 與 本機網路的來源通訊位置，
    // 因此未來也可能會做修改。
    //

    //
    // Adapter 群組
    //

    GroupId := -1;
    J := iGroup;
    while J < ListView1.Groups.Count do
    begin
      if ListView1.Groups.Items[J].Header = s then
      begin
        if J < GroupCount then
          bGroup[J] := True;
        iGroup := J;
        GroupId := ListView1.Groups.Items[J].GroupID;
        Break;
      end;
      Inc(J);
    end;
    if GroupId < 0 then
    begin
      J := 0;
      while J < iGroup do
      begin
        if ListView1.Groups.Items[J].Header = s then
        begin
          if J < GroupCount then
            bGroup[J] := True;
          iGroup := J;
          GroupId := ListView1.Groups.Items[J].GroupID;
          Break;
        end;
        Inc(J);
      end;
    end;

    if GroupId < 0 then
    begin
      Group := ListView1.Groups.Add;
      Group.Header := s;
      iGroup := Group.Index;
      GroupId := Group.GroupID;
    end;

    //
    // Address 列表
    //

    b := True;
    J := iItem;
    while J < ListView1.Items.Count do
    begin
      if ListView1.Items[J].Caption = sUnicast then
      begin
        if ListView1.Items[J].SubItems[0] = sGateway then
        begin
          if J < ItemCount then
            bItem[J] := True;
          iItem := J;
          b := False;
          Break;
        end;
      end;
      Inc(J);
    end;
    if b then
    begin
      J := 0;
      while J < iItem do
      begin
        if ListView1.Items[J].Caption = sUnicast then
        begin
          if ListView1.Items[J].SubItems[0] = sGateway then
          begin
            if iItem < ItemCount then
              bItem[J] := True;
            iItem := J;
            b := False;
            Break;
          end;
        end;
        Inc(J);
      end;
    end;

    if b then
    begin
      Item := ListView1.Items.Add;
      Item.Data := Pointer(p.AdapterName);
      Item.Caption := sUnicast;
      Item.SubItems.Add(sGateway);
      Item.GroupID := GroupId;
      p.Data := Item;
      iItem := Item.Index;
    end;

  end;

  //
  // 清除已不存在的f群組與項目
  //

  for I := GroupCount - 1 downto 0 do
    if not bGroup[I] then
      ListView1.Groups.Delete(I);

  for I := ItemCount - 1 downto 0 do
    if not bItem[I] then
      ListView1.Items.Delete(I);
end;

procedure TForm2.DoSelected;
begin
  if not Assigned(ListView1.Selected) then
    Exit;

  ModalResult := mrOk;
end;

procedure TForm2.ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  case Change of
    ctState: Button1.Enabled := Assigned(TListView(Sender).Selected);
  end;
end;

procedure TForm2.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    DoSelected;
end;

procedure TForm2.ListView1DblClick(Sender: TObject);
begin
  DoSelected;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  DoSelected;
end;

procedure TForm2.RefreshAdapter;
begin
  AdaptersInfo.Refresh;
end;

function TForm2.GetFirstAddress(var Unicast, Gateway: TSockAddr): Boolean;
var
  I: Integer;
  Family: Smallint;
  p1, p2: PAdapterAddress;
begin
  p2 := nil;
  for I := 0 to AdapterAddressList.Count - 1 do
  begin
    p1 := @AdapterAddressList.List[I];
    Family := p1.Unicast.family;
    if p1.Gateway.family = Family then
    begin
      if Assigned(p2) then
      begin
        if Family = AF_INET then
        begin
          p2 := p1;
          Break;
        end;
      end
      else
      begin
        p2 := p1;
        if Family = AF_INET then
          Break;
      end;
    end;
  end;
  Result := Assigned(p2);
  if Result then
  begin
    Unicast := p2.Unicast;
    Gateway := p2.Gateway;
  end;
end;

function TForm2.GetAddress(Index: Integer; var Unicast, Gateway: TSockAddr): Boolean;
var
  I: Integer;
  p: PAdapterAddress;
  Item: TListItem;
  s: string;
begin
  Result := False;
  if (Index < 0) or (Index >= ListView1.Items.Count) then
    Exit;

  Item := ListView1.Items[Index];

  if not Assigned(Item.Data) then
    Exit;

  s := string(Item.Data);

  for I := 0 to AdapterAddressList.Count - 1 do
  begin
    p := @AdapterAddressList.List[I];

    if p.AdapterName <> s then
      Continue;
    if Item.Caption <> AddressToString(p.Unicast) then
      Continue;
    if Item.SubItems[0] <> AddressToString(p.Gateway) then
      Continue;

    Unicast := p.Unicast;
    Gateway := p.Gateway;
    Exit(True);
  end;
end;

function TForm2.GetSelect(var Unicast, Gateway: TSockAddr): Boolean;
begin
  Result := GetAddress(ListView1.ItemIndex, Unicast, Gateway);
end;

end.
