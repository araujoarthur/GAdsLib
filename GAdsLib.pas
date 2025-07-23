unit GAdsLib;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils, System.JSON, System.IniFiles, System.Net.HttpClient, System.Net.URLClient,
  GAdsLib.Utils.SharedResources, GAdsLib.Utils.Types;

type
  ///  <summary>
  ///  Classe de abstração da interação com a API do Google Ads.
  ///  </summary>
  TGoogleAds = class
  private
    // User Provided Fields
    FCLIENT_ID:               String;
    FCLIENT_SECRET:           String;
    FREFRESH_TOKEN:           String;
    FCUSTOMER_ID:             String;
    FLOGIN_CUSTOMER_ID:       String;
    FDEVELOPER_TOKEN:         String;

    // API Provided Fields
    FACCESS_TOKEN:            String;
    FREFRESH_TOKEN_EXPIRY_AT: Int64;
    FACCESS_TOKEN_EXPIRY_AT:  Int64;  // Timestamp da expiração do AT

    FEXPIRATION_THRESHOLD:    Int64; // Default to MINUTE;

    // Internal Fields
      // Database Connection, HTTP Client
    FHTTPClient: THTTPClient;
    FDATABASE_RESOURCES: TDatabaseSharedResources;

    {$IFDEF DEBUG}
    FLatestResponse: String;
    FLatestHeaders: TArray<TNameValuePair>;
    {$ENDIF}

    // Will check if the token requires a refresh.
    // It should also check if the token has not been changed in the database before making the request and/or refreshing the token.
    function RequiresRefresh(): Boolean;
    function FetchRemoteResources(): TGAdsSharedResources;
    procedure SaveRemoteResources(ARes: TGAdsSharedResources);

    procedure SetDeveloperToken(const Value: String);
    procedure SetLoginCustomerID(const Value: String);

    procedure EnforceConformity();

    procedure DoRefreshToken();
    procedure LoadAccessFromFile(AFilePath: String; AStrict: Boolean = False);
    function GetDatabaseResources: String;
  public
    {$IFDEF DEBUG}
    function GetLatestResponse(): String;
    function GetLatestHeaders(): TArray<TNameValuePair>;
    {$ENDIF}


    property ClientID:              String  read FCLIENT_ID             write FCLIENT_ID;
    property ClientSecret:          String  read FCLIENT_SECRET         write FCLIENT_SECRET;
    property RefreshToken:          String  read FREFRESH_TOKEN         write FREFRESH_TOKEN;
    property CustomerID:            String  read FCUSTOMER_ID           write FCUSTOMER_ID;
    property LoginCustomerID:       String  read FLOGIN_CUSTOMER_ID     write SetLoginCustomerID;
    property DeveloperToken:        String  read FDEVELOPER_TOKEN       write SetDeveloperToken;
    property AccessToken:           String  read FACCESS_TOKEN;
    property RefreshTokenExpiryAt:  Int64   read FREFRESH_TOKEN_EXPIRY_AT;
    property AccessTokenExpiryAt:   Int64   read FACCESS_TOKEN_EXPIRY_AT;
    property ExpirationThreshold:   Int64   read FEXPIRATION_THRESHOLD  write FEXPIRATION_THRESHOLD;
    property DatabaseResources:     String  read GetDatabaseResources;

    constructor Create(AClientID, AClientSecret, ARefreshToken, ACustomerID, ALoginCustomerID, ADeveloperToken: String); overload;
    constructor Create(APath: String); overload;
    // Deve ser utilizado apenas para primeira inicialização e testes.

    procedure LoadAccessFromDB();
    function RunSearchStreamQuery(AQuery: String): TJSONValue;
    function RunCampaignsQuery(): TGAdsCampaigns;
    procedure SaveCampaigns(ACampaigns: TGAdsCampaigns);
  end;

const
  // Seconds in different units
  MINUTE = 60;
  HOUR   = 3600;
  DAY    = 86400;

  // Endpoints (Google had as of now at least 20 versions of these)
  REFRESH_TOKEN_URL = 'https://www.googleapis.com/oauth2/v3/token';

implementation

uses
  System.Generics.Collections, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet;

{ TGoogleAds }

constructor TGoogleAds.Create(AClientID, AClientSecret, ARefreshToken,
  ACustomerID, ALoginCustomerID, ADeveloperToken: String);
begin
  FCLIENT_ID := AClientID;
  FCLIENT_SECRET := AClientSecret;
  FREFRESH_TOKEN := ARefreshToken;
  FCUSTOMER_ID := ACustomerID;
  FLOGIN_CUSTOMER_ID := ALoginCustomerID;
  FDEVELOPER_TOKEN := ADeveloperToken;

  FREFRESH_TOKEN_EXPIRY_AT := 0;
  FACCESS_TOKEN_EXPIRY_AT := 0;

  FEXPIRATION_THRESHOLD := 1*Minute;

  FDATABASE_RESOURCES := TDatabaseSharedResources.FromFile('cfg.ini');
  Self.FHTTPClient := THTTPClient.Create();

  Self.FHTTPClient.CustomHeaders['developer-token'] := FDEVELOPER_TOKEN;
  Self.FHTTPClient.CustomHeaders['login-customer-id'] := FLOGIN_CUSTOMER_ID;
end;

constructor TGoogleAds.Create(APath: String);
begin
  FREFRESH_TOKEN_EXPIRY_AT := 0;
  FACCESS_TOKEN_EXPIRY_AT := 0;

  FEXPIRATION_THRESHOLD := 1*Minute;
  Self.LoadAccessFromFile(APath);
  FDATABASE_RESOURCES := TDatabaseSharedResources.FromFile(APath);
  Self.FHTTPClient := THTTPClient.Create();

  Self.FHTTPClient.CustomHeaders['developer-token'] := FDEVELOPER_TOKEN;
  Self.FHTTPClient.CustomHeaders['login-customer-id'] := FLOGIN_CUSTOMER_ID;
end;

procedure TGoogleAds.DoRefreshToken;
var
  RefreshHTTPClient: THTTPClient;
  RefreshBody: String;
  RefreshResponse: IHTTPResponse;
  ResponseJSON: TJSONObject;
  ExpiresIn: Int64;
  ExpiresRefreshIn: Int64;
  FreezeDelta: Int64;
  SharedRes: TGAdsSharedResources;
begin

  RefreshHTTPClient := THTTPClient.Create;
  try
    RefreshBody := Format('{"grant_type":"refresh_token", "client_id":"%s", "client_secret":"%s", "refresh_token":"%s",}', [FCLIENT_ID, FCLIENT_SECRET, FREFRESH_TOKEN]);
    RefreshResponse := RefreshHTTPClient.Post(REFRESH_TOKEN_URL, TStringStream.Create(RefreshBody, TEncoding.UTF8));
    FLatestResponse := RefreshResponse.ContentAsString();

    if
      (RefreshResponse.StatusCode >= 200)
      and (RefreshResponse.StatusCode < 300)
      and RefreshResponse.MimeType.Contains('application/json')
    then
    begin
      FreezeDelta := DateTimeToUnix(Now());
      ResponseJSON := TJSONObject(TJSONObject.ParseJSONValue(RefreshResponse.ContentAsString));
      if not ResponseJSON.TryGetValue<String>('access_token', FACCESS_TOKEN) then
        raise Exception.Create('Não foi possível obter novo Token.');

      if not ResponseJSON.TryGetValue<Int64>('expires_in', ExpiresIn) then
        raise Exception.Create('Não foi possível obter a expiração do Token');

      FACCESS_TOKEN_EXPIRY_AT := FreezeDelta + ExpiresIn - 5;

      if not ResponseJSON.TryGetValue<Int64>('refresh_token_expires_in', ExpiresRefreshIn) then
        raise Exception.Create('Não foi possível obter a expiração do Refresh Token');

      FREFRESH_TOKEN_EXPIRY_AT := FreezeDelta + ExpiresRefreshIn - 5;

      SharedRes.ClientID := FCLIENT_ID;
      SharedRes.ClientSecret := FCLIENT_SECRET;
      SharedRes.RefreshToken := FREFRESH_TOKEN;
      SharedRes.CustomerID := FCUSTOMER_ID;
      SharedRes.LoginCustomerID := FLOGIN_CUSTOMER_ID;
      SharedRes.DeveloperToken := FDEVELOPER_TOKEN;
      SharedRes.AccessToken := FACCESS_TOKEN;
      SharedRes.AccessTokenExpiryAt := FACCESS_TOKEN_EXPIRY_AT;
      SharedRes.RefreshTokenExpiryAt := FREFRESH_TOKEN_EXPIRY_AT;

      Self.SaveRemoteResources(SharedRes);
    end;

  finally
    RefreshHTTPClient.Free;
  end;

end;

procedure TGoogleAds.EnforceConformity;
var
  SharedRes: TGAdsSharedResources;
begin
  // Obtem o token do banco de dados.
  SharedRes := FetchRemoteResources();

  // O token no banco é mais novo do que o local
  if SharedRes.AccessTokenExpiryAt > Self.FACCESS_TOKEN_EXPIRY_AT then
  begin
    Self.ClientID := SharedRes.ClientID;
    Self.ClientSecret := SharedRes.ClientSecret;
    Self.CustomerID := SharedRes.CustomerID;
    Self.LoginCustomerID := SharedRes.LoginCustomerID;
    Self.DeveloperToken := SharedRes.DeveloperToken;
    Self.RefreshToken := SharedRes.RefreshToken;
    Self.FACCESS_TOKEN_EXPIRY_AT := SharedRes.AccessTokenExpiryAt;
    Self.FREFRESH_TOKEN_EXPIRY_AT := SharedRes.RefreshTokenExpiryAt;
    Self.FACCESS_TOKEN := SharedRes.AccessToken;
  end;

  // Checa se o token atual requer refresh
  if RequiresRefresh() then
    DoRefreshToken();

  Self.FHTTPClient.CustomHeaders['Authorization'] := Format('Bearer %s', [FACCESS_TOKEN]);
end;

function TGoogleAds.FetchRemoteResources: TGAdsSharedResources;
var
  Conn: TFDConnection;
  Query: TFDQuery;
  Mem: TFDMemTable;
  ResourcesObject: TJSONObject;
  ResourcesStr: String;
begin
  Conn := TFDConnection.Create(nil);
  Query := TFDQuery.Create(nil);
  Mem := TFDMemTable.Create(nil);
  try
    Result.AccessTokenExpiryAt := 0;
    Result.RefreshTokenExpiryAt := 0;
    Conn.Params.DriverID := 'FB';
    Conn.Params.Database := FDATABASE_RESOURCES.Path;
    Conn.Params.Username := FDATABASE_RESOURCES.Username;
    Conn.Params.Password := FDATABASE_RESOURCES.Password;
    Conn.Params.Add(Format('Server=%s', [FDATABASE_RESOURCES.Address]));

    Query.Connection := Conn;
    Conn.Connected := True;

    Query.SQL.Text := 'SELECT resources FROM shared_res WHERE id = ''googleads''';
    Query.Open();
    Mem.Data := Query.Data;
    Query.Close;

    if Mem.RecordCount < 1 then
      Exit;

    ResourcesStr := Mem.FieldByName('resources').AsString;
    if ResourcesStr = '' then
      Exit;

    ResourcesObject := TJSONObject(TJSONObject.ParseJSONValue(ResourcesStr));
    Result := TGAdsSharedResources.FromJSONObject(ResourcesObject);
    ResourcesObject.Free;
  finally
    Mem.Free;
    Query.Close;
    Query.Free;
    Conn.Connected := False;
    Conn.Free;
  end;
end;

{$IFDEF DEBUG}
function TGoogleAds.GetDatabaseResources: String;
begin
  Result := FDATABASE_RESOURCES.ToString();
end;

function TGoogleAds.GetLatestHeaders: TArray<TNameValuePair>;
begin
  Result := FLatestHeaders;
end;

function TGoogleAds.GetLatestResponse: String;
begin
  Result := FLatestResponse;
end;
{$ENDIF}

procedure TGoogleAds.LoadAccessFromDB;
var
  SharedRes: TGAdsSharedResources;
begin
  // Obtem o token do banco de dados.
  SharedRes := FetchRemoteResources();

  // O token no banco é mais novo do que o local
  Self.ClientID := SharedRes.ClientID;
  Self.ClientSecret := SharedRes.ClientSecret;
  Self.CustomerID := SharedRes.CustomerID;
  Self.LoginCustomerID := SharedRes.LoginCustomerID;
  Self.DeveloperToken := SharedRes.DeveloperToken;
  Self.RefreshToken := SharedRes.RefreshToken;
  Self.FACCESS_TOKEN_EXPIRY_AT := SharedRes.AccessTokenExpiryAt;
  Self.FREFRESH_TOKEN_EXPIRY_AT := SharedRes.RefreshTokenExpiryAt;

end;

procedure TGoogleAds.LoadAccessFromFile(AFilePath: String; AStrict:Boolean = False);
var
  IniFile: TIniFile;
begin
  if not FileExists(AFilePath) then
    raise Exception.Create('Arquivo inexistente: "'+AFilePath+'".');

  IniFile := TIniFile.Create(AFilePath);
  FACCESS_TOKEN := IniFile.ReadString('GAdsLib', 'accessToken', '');

  if (FACCESS_TOKEN = '') and AStrict then
    raise Exception.Create('Access Token não encontrado no arquivo "'+AFilePath+'".');

  // Tenta recuperar um refresh token novo, se houver.
  FREFRESH_TOKEN := IniFile.ReadString('GAdsLib', 'refreshToken', FREFRESH_TOKEN);


  FREFRESH_TOKEN_EXPIRY_AT := IniFile.ReadInteger('GAdsLib', 'refreshTokenExpiryAt', -1);
  FACCESS_TOKEN_EXPIRY_AT := IniFile.ReadInteger('GAdsLib', 'accessTokenExpiryAt', -1);

  if ((FREFRESH_TOKEN_EXPIRY_AT = -1) or (FACCESS_TOKEN_EXPIRY_AT = -1)) and AStrict then
    raise Exception.Create('Expiração dos tokens não encontrada');
end;

function TGoogleAds.RequiresRefresh: Boolean;
var
  ExpirationThreshold: Int64;
begin
  ExpirationThreshold := DateTimeToUnix(Now()) + FEXPIRATION_THRESHOLD;
  Result :=  ExpirationThreshold >= FACCESS_TOKEN_EXPIRY_AT; // Retorna verdadeiro se o limite de expiração (1 minuto a partir da chamada) for maior que o timestamp de expiração do token.
end;

function TGoogleAds.RunCampaignsQuery: TGAdsCampaigns;
var
  Response: TJSONValue;
  ResponseObject: TJSONObject;
  ResponseArray, ResultsArray: TJSONArray;

const
  GQLQuery = 'SELECT campaign.id, campaign.name, campaign.status, campaign_budget.amount_micros, campaign_budget.status, campaign.advertising_channel_type FROM campaign WHERE campaign.advertising_channel_type IN (''SHOPPING'', ''PERFORMANCE_MAX'')';

begin
  Response := Self.RunSearchStreamQuery(GQLQuery);
  try
    if (Response is TJSONObject) then
    begin
      // Deal with error
      Exit(nil);
    end;


    if not (Response is TJSONArray) then
      raise Exception.Create('Erro Desconhecido ao Executar Query: ' + Response.ToString);

    ResponseArray := TJSONArray(Response);
    ResponseObject := TJSONObject(ResponseArray.Items[0]);

    if not ResponseObject.TryGetValue<TJSONArray>('results', ResultsArray) then
      raise Exception.Create('Formato JSON inesperado');


    Result := TGAdsCampaigns.CopyFromJSON(ResultsArray);
  finally
    Response.Free;
  end;
end;

function TGoogleAds.RunSearchStreamQuery(AQuery: String): TJSONValue;
var
  Response: IHTTPResponse;
  RequestURL: String;
  RequestBody: String;
begin
  EnforceConformity();
  RequestBody := Format('{"query":"%s",}', [AQuery]);
  RequestURL := Format('https://googleads.googleapis.com/v20/customers/%s/googleAds:searchStream', [FCUSTOMER_ID]);

  {$IFDEF DEBUG}
  FLatestHeaders := FHTTPClient.GetRequest('post', RequestURL).Headers;
  {$ENDIF}

  Response := FHTTPClient.Post(RequestURL, TStringStream.Create(RequestBody, TEncoding.UTF8));

  {$IFDEF DEBUG}
  FLatestResponse := Response.ContentAsString();
  {$ENDIF}

  if (Response.StatusCode >= 200) and (Response.StatusCode < 300) then
  begin
    Exit(TJSONObject.ParseJSONValue(Response.ContentAsString()));
  end;

  Exit(nil);
end;

procedure TGoogleAds.SaveCampaigns(ACampaigns: TGAdsCampaigns);
var
  Conn: TFDConnection;
  Query: TFDQuery;
  Idx: Integer;
const
  IUQuery = 'UPDATE OR INSERT INTO gads_campanhas (id, resource_name, nome, status, budget_resource_name, orcamento, orcamento_status, data_atualizado) VALUES (:id, :resource_name, :nome, :status, :budget_resource_name, :orcamento, :orcamento_status, :data_atualizado) MATCHING (id)';
begin
  Conn := TFDConnection.Create(nil);
  Query := TFDQuery.Create(nil);
  try
    Conn.Params.DriverID := 'FB';
    Conn.Params.Database := FDATABASE_RESOURCES.Path;
    Conn.Params.Username := FDATABASE_RESOURCES.Username;
    Conn.Params.Password := FDATABASE_RESOURCES.Password;
    Conn.Params.Add(Format('Server=%s', [FDATABASE_RESOURCES.Address]));

    Query.Connection := Conn;
    Conn.StartTransaction();

    try
      Query.SQL.Text := IUQuery;

      Query.Params.ArraySize := ACampaigns.Count;
      
      for Idx := 0 to (ACampaigns.Count - 1) do
      begin
        Query.Params[0].AsLargeInts[Idx] := ACampaigns.Campaigns[Idx].ID;                   // ID
        Query.Params[1].AsStrings[Idx] := ACampaigns.Campaigns[Idx].RESOURCE_NAME;          // Resource Name
        Query.Params[2].AsStrings[Idx] := ACampaigns.Campaigns[Idx].NOME;                   // Nome
        Query.Params[3].AsStrings[Idx] := ACampaigns.Campaigns[Idx].STATUS;                 // Status
        Query.Params[4].AsStrings[Idx] := ACampaigns.Campaigns[Idx].BUDGET_RESOURCE_NAME;   // Budget Resource Name
        Query.Params[5].AsLargeInts[Idx] := ACampaigns.Campaigns[Idx].ORCAMENTO;            // Orcamento
        Query.Params[6].AsStrings[Idx] := ACampaigns.Campaigns[Idx].ORCAMENTO_STATUS;       // Orcamento Status
        Query.Params[7].AsDateTimes[Idx] := Now();                                          // Data Atualizado
      end;
        
      Query.Execute(ACampaigns.Count, 0);
      
      Conn.Commit;
    except on E: Exception do
      begin
        Conn.Rollback;
        raise;
      end;
    end;
    
  finally
    Query.Close;
    Query.Free;
    Conn.Connected := False;
    Conn.Free;
  end;
end;

procedure TGoogleAds.SaveRemoteResources(ARes: TGAdsSharedResources);
var
  Conn: TFDConnection;
  Query: TFDQuery;
begin

  Conn := TFDConnection.Create(nil);
  Query := TFDQuery.Create(nil);
  try
    Conn.Params.DriverID := 'FB';
    Conn.Params.Database := FDATABASE_RESOURCES.Path;
    Conn.Params.Username := FDATABASE_RESOURCES.Username;
    Conn.Params.Password := FDATABASE_RESOURCES.Password;
    Conn.Params.Add(Format('Server=%s', [FDATABASE_RESOURCES.Address]));

    Query.Connection := Conn;
    Conn.Connected := True;
    Query.SQL.Text := Format('UPDATE OR INSERT INTO shared_res(id, resources) VALUES (''googleads'', ''%s'') MATCHING(id)', [ARes.Serialize()]);
    Query.ExecSQL;
  finally
    Query.Close;
    Query.Free;
    Conn.Connected := False;
    Conn.Close;
  end;
end;

procedure TGoogleAds.SetDeveloperToken(const Value: String);
begin
  FDEVELOPER_TOKEN := Value;
  Self.FHTTPClient.CustomHeaders['developer-token'] := Value;
end;

procedure TGoogleAds.SetLoginCustomerID(const Value: String);
begin
  FLOGIN_CUSTOMER_ID := Value;
   Self.FHTTPClient.CustomHeaders['login-customer-id'] := Value;
end;

end.
