// Type: Ping tool - Application.
// Author: 2022 Wei-Lun Huang
// Description: Application Ping.
//
// Features:
//   Ping response between specified local address and remote address,
//   and graphical for RoundTripTime in recent.
//
// Tested in Delphi 10 Seattle.
//
// Last modified date: May 7, 2022.

program Ping;

{$R *.dres}

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  IpHelper.IcmpPing in 'IpHelper.IcmpPing.pas',
  IpHelper.Adapters in 'IpHelper.Adapters.pas',
  IpHelper.Addresses in 'IpHelper.Addresses.pas',
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
