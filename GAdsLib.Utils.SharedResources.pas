unit GAdsLib.Utils.SharedResources;

interface

uses System.JSON;

type
  TGAdsSharedResources = record
    ClientID: String;
    ClientSecret: String;
    RefreshToken: String;
    CustomerID: String;
    LoginCustomerID: String;
    DeveloperToken: String;
    AccessToken: String;
    AccessTokenExpiryAt: Int64;
    RefreshTokenExpiryAt: Int64;

    function Serialize(): String;
    class function FromJSONObject(AObject: TJSONObject): TGAdsSharedResources; static;
  end;

  TDatabaseSharedResources = record
    Username: String;
    Password: String;
    Address: String;
    Path: String;

    class function FromFile(APath: String): TDatabaseSharedResources; static;
  end;

  const
    Serializable = '{"clientId":"%s", "clientSecret":"%s", "refreshToken":"%s", "customerId":"%s", "loginCustomerId":"%s", "developerToken":"%s", "accessToken":"%s", "accessTokenExpiryAt":"%d", "refreshTokenExpiryAt":"%d"}';

implementation

uses
  System.SysUtils, System.IniFiles;

{ TGAdsSharedResources }

class function TGAdsSharedResources.FromJSONObject(
  AObject: TJSONObject): TGAdsSharedResources;

var
  accessTokenExpiryAtTemp: String;
  refreshTOkenExpiryAtTemp: String;
begin
  if not AObject.TryGetValue<String>('clientId', Result.ClientID) then
    Result.ClientID := '';

  if not AObject.TryGetValue<String>('clientSecret', Result.ClientSecret) then
    Result.ClientSecret := '';

  if not AObject.TryGetValue<String>('refreshToken', Result.RefreshToken) then
    Result.RefreshToken := '';

  if not AObject.TryGetValue<String>('customerId', Result.CustomerID) then
    Result.CustomerID := '';

  if not AObject.TryGetValue<String>('loginCustomerId', Result.LoginCustomerID) then
    Result.LoginCustomerID := '';

  if not AObject.TryGetValue<String>('developerToken', Result.DeveloperToken) then
    Result.DeveloperToken := '';

  if not AObject.TryGetValue<String>('accessToken', Result.AccessToken) then
    Result.AccessToken := '';

  if not AObject.TryGetValue<String>('accessTokenExpiryAt', accessTokenExpiryAtTemp) then
    Result.AccessTokenExpiryAt := 0
  else
    Result.AccessTokenExpiryAt := StrToInt64(accessTokenExpiryAtTemp);

  if not AObject.TryGetValue<String>('refreshTokenExpiryAt', refreshTokenExpiryAtTemp) then
    Result.RefreshTokenExpiryAt := 0
  else
    Result.RefreshTokenExpiryAt := StrToInt64(refreshTokenExpiryAtTemp);
end;

function TGAdsSharedResources.Serialize: String;
begin

  Result := Format(Serializable, [ClientID, ClientSecret, RefreshToken, CustomerID, LoginCustomerID, DeveloperToken, AccessToken, AccessTokenExpiryAt, RefreshTokenExpiryAt])
end;

{ TDatabaseSharedResources }

class function TDatabaseSharedResources.FromFile(
  APath: String): TDatabaseSharedResources;
var
  IniFile: TIniFile;
begin
  if not FileExists(APath) then
    raise Exception.Create('Arquivo Inexistente');

  IniFile := TIniFile.Create(APath);

  Result.Username := IniFile.ReadString('database', 'username', '');
  Result.Password := IniFile.ReadString('database', 'password', '');
  Result.Address := IniFile.ReadString('database', 'address', '');
  Result.Path := IniFile.ReadString('database', 'path', '');
end;

end.
