unit Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, GAdsLib, GAdsLib.Utils.Types, GAdsLib.Utils.SharedResources;
type
  TForm1 = class(TForm)
    Memo1: TMemo;
    edtRefreshToken: TLabeledEdit;
    edtAccessToken: TLabeledEdit;
    edtDevToken: TLabeledEdit;
    edtClientID: TLabeledEdit;
    edtClientSecret: TLabeledEdit;
    edtCustomerID: TLabeledEdit;
    edtCustomerLoggedIn: TLabeledEdit;
    btnCreateGAds: TButton;
    btnSerialize: TButton;
    edtQuery: TLabeledEdit;
    Button1: TButton;
    procedure btnCreateGAdsClick(Sender: TObject);
    procedure btnSerializeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  GAds: TGoogleAds;

implementation

{$R *.dfm}

procedure TForm1.btnCreateGAdsClick(Sender: TObject);
begin
  GAds := TGoogleAds.Create(edtClientID.Text, edtClientSecret.Text, edtRefreshToken.Text, edtCustomerID.Text, edtCustomerLoggedIn.Text, edtDevToken.Text);
  Memo1.Text := GAds.GetLatestResponse;
end;

procedure TForm1.btnSerializeClick(Sender: TObject);
var
  gsr: TGAdsSharedResources;
begin
  gsr.ClientID := GAds.ClientID;
  gsr.ClientSecret := GAds.ClientSecret;
  gsr.RefreshToken := GAds.RefreshToken;
  gsr.CustomerID := GAds.CustomerID;
  gsr.LoginCustomerID := GAds.LoginCustomerID;
  gsr.DeveloperToken := GAds.DeveloperToken;
  gsr.AccessToken := GAds.AccessToken;
  gsr.AccessTokenExpiryAt := GAds.AccessTokenExpiryAt;
  gsr.RefreshTokenExpiryAt := GAds.RefreshTokenExpiryAt;

  Memo1.Lines.Add(gsr.Serialize());
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Resp: TGAdsCampaigns;
  Camp: TGAdsCampaignEntry;
  Idx: Integer;
begin
  Resp := GAds.RunCampaignsQuery();
  Idx := 0;
  for Camp in Resp do
  begin
    Memo1.Lines.Add(Format('%d - %s%s', [Idx, Camp.GetInsertQuery(), sLineBreak]));
  end;

  GAds.SaveCampaigns(Resp);
end;

end.
