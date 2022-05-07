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
    procedure OnAdapterDetected(ASender: TObject;
      State: TAdaptersInfoState; const AOld, ANew: TAdapterAddresses);
    procedure OnAdapterChanged(Sender: TObject);
    procedure DoSelected;
    procedure RefreshAdapter;
    function GetFirstAddress(var Unicast, Gateway: TSockAddr): Boolean;
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

  UnicastList := TSockAddrList.Create;
  GatewayList := TSockAddrList.Create;
  try
    case State of
      _AIM_Removed: p := @AOld;
      _AIM_Added  : p := @ANew;
      _AIM_Changed: p := @ANew;
      else Exit;
    end;
    AdapterName := string(p.AdapterName);
    FriendlyName := WideCharToString(p.FriendlyName);

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

    Gateway := p.FirstGatewayAddress;
    while Assigned(Gateway) do
    begin
      case Gateway.Address.lpSockaddr.sa_family of
        AF_INET, AF_INET6: GatewayList.Add(Gateway.Address);
      end;
      Gateway := Gateway.Next;
    end;

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
            if pUnicast.base.sin_family = pGateway.base.sin_family then
            begin
              AdapterAddressList.Add(AdapterName, FriendlyName, pUnicast^, pGateway^);
              UnicastList.Delete(I);
              GatewayList.Delete(J);
              b := False;
              Break;
            end
            else
            begin
              Inc(J);
            end;
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

function TForm2.GetSelect(var Unicast, Gateway: TSockAddr): Boolean;
var
  I: Integer;
  p: PAdapterAddress;
  Item: TListItem;
  s: string;
begin
  Result := False;
  Item := ListView1.Selected;

  if not Assigned(Item) then
    Exit;
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

end.
