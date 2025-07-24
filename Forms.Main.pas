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
    Button2: TButton;
    Button3: TButton;
    procedure btnCreateGAdsClick(Sender: TObject);
    procedure btnSerializeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  GAds := TGoogleAds.Create('./cfg.ini');
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
  {Camp: TGAdsCampaignEntry;
  Idx: Integer;  }
begin
  Resp := GAds.RunCampaignsQuery();
{  Idx := 0;
  for Camp in Resp do
    begin
        Memo1.Lines.Add(Format('%d - %s%s', [Idx, Camp.GetInsertQuery(), sLineBreak]));
          end;}

  GAds.SaveCampaigns(Resp);
  Memo1.Lines.Add('Sucess C Qry');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Add(GAds.DatabaseResources);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Resp: TGAdsProducts;
  Prod: TGAdsProductEntry;
  Idx: Integer;
begin
  Resp := GAds.RunShoppingViewQuery();
  Idx := 0;
  for Prod in Resp do
  begin
    Memo1.Lines.Add('Rec No. ' + IntToStr(Idx));
    Memo1.Lines.Add(Prod.ToString());
    Memo1.Lines.Add('');
    Inc(Idx);
  end;

  GAds.SaveShoppingView(Resp);
  Memo1.Lines.Add('Success P Qry');

end;

end.
