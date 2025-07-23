unit GAdsLib.Utils.Types;

interface

uses FireDAC.Comp.Client, System.JSON, System.SysUtils, System.Classes;

type

  ///  <summary>
  ///  Interface base para as coleções da biblioteca GAdsLib.
  ///  </summary>
  IGAdsCollection<T> = interface
    ['{B12B4BD6-4530-4400-9B64-9CB885E70564}']
    function GetItem(Index: Integer): T;
    function GetCount(): Integer;
    property Count: Integer read GetCount;
  end;

  ///  <summary>
  ///  Classe enumeradora das coleções da biblioteca GAdsLib.
  ///  </summary>
  TGAdsCollectionEnumerator<U> = class
  private
    FCollection: IGAdsCollection<U>;
    FIndex: Integer;
  public
    constructor Create(const ACollection: IGAdsCollection<U>);
    function GetCurrent(): U;
    function MoveNext(): Boolean;
    property Current: U read GetCurrent;
  end;

  ///  <summary>
  ///  Representa uma única campanha retornada pela API ou armazenada no banco de dados (WIP).
  ///  </summary>
  TGAdsCampaignEntry = record
    ID: Int64;
    RESOURCE_NAME: String;
    NOME: String;
    STATUS: String;
    TIPO_CANAL: String;
    BUDGET_RESOURCE_NAME: String;
    ORCAMENTO: Int64;
    ORCAMENTO_STATUS: String;
    DATA_ATUALIZADO: TDateTime;
    DATA_INSERIDO: TDateTime;

    ///  <summary>
    ///  Cria uma nova estrutura com as informações recebidas
    ///  </summary>
    class function New(AID: Int64; AResourceName, ANome, AStatus, ATipoCanal, ABudgetResourceName: String;
                        AOrcamento: Int64; AOrcamentoStatus: String; ADataAtualizado, ADataInserido: TDateTime): TGAdsCampaignEntry; static;

    ///  <summary>
    ///  Cria uma nova estrutura vazia.
    ///  </summary>
    class function EmptyNew(): TGAdsCampaignEntry; static;

    ///  <summary>
    ///   Cria uma nova estrutura a partir de um objeto JSON retornado pela API.
    ///  </summary>
    ///  <remarks>
    ///  O objeto em questão deve ser especifícamente uma única entrada da array results fornecida pela API.
    ///  </remarks>
    class function NewFromJSON(const AJSON: TJSONObject): TGAdsCampaignEntry; static;

  end;

  ///  <summary>
  ///  Coleção de campanhas retornada pela API do Google Ads ou armazenada no banco de dados (WIP).
  ///  Esta classe implementa a interface <see cref="IGAdsCollection<T>"/> para o tipo concreto <see cref="TGAdsCampaignEntry"/>.
  ///  <summary>
  TGAdsCampaigns = class(TObject, IGAdsCollection<TGAdsCampaignEntry>)
  private
    FCampaigns: array of TGAdsCampaignEntry;

    constructor Create;
    function GetCount: Integer;
    function GetCampaign(Index: Integer): TGAdsCampaignEntry;
    function GetItem(Index: Integer): TGAdsCampaignEntry;

    // Declare some COM methods to disable reference counting for this interfaced object.
    // This is needed here because I needed a way to bypass the limitations of generics in Delphi.
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef(): Integer; stdcall;
    function _Release(): Integer; stdcall;
  public
    ///  <summary>
    ///  Quantidade de elementos na coleção.
    ///  </summary>
    property Count: Integer read GetCount;
    ///  <summary>
    ///  Acesso indexado aos elementos da coleção.
    ///  </summary>
    property Campaigns[Index: Integer]: TGAdsCampaignEntry read GetCampaign;

    destructor Destroy; override;

    ///  <summary>
    ///  Copia uma coleção de campanhas do resultado de uma query SQL (WIP).
    ///  </summary>
    ///  <param name="AQuery">
    ///  Instância da query que contém os resultados a serem copiados. A query <b>deve</b> estar aberta.
    ///  </param>
    ///  <returns>
    ///  Instância de <see cref="TGAdsCampaigns"/> contendo todas as campanhas de <i>AQuery</i>.
    ///  </returns>
    class function CopyFromQuery(AQuery: TFDQuery): TGAdsCampaigns; static;

    ///  <summary>
    ///  Copia uma coleção de campanhas de uma array JSON.
    ///  </summary>
    ///  <param name="AQuery">
    ///  Instância de <see cref="TJSONArray"/> que contem as infomrmações a serem copiadas.
    ///  </param>
    ///  <remarks>
    ///  Atualmente, a API responde uma array com um único objeto que contém, entre outros,
    ///   o campo <i>results</i>. Essa função espera receber a array armazenada no campo <i>results</i>, e não a
    ///   array respondida pela API diretamente.
    ///  </remarks>
    ///  <returns>
    ///  Instância de <see cref="TGAdsCampaigns"/> contendo todas as campanhas de <i>AQuery</i>.
    ///  </returns>
    class function CopyFromJSON(const AJSON: TJSONArray): TGAdsCampaigns; static;

    ///  <summary>
    ///  Habilita a coleção a ser utilizada em <i>loops</i> do tipo <c>for ... in ...</c>
    ///  </summary>
    function GetEnumerator(): TGAdsCollectionEnumerator<TGAdsCampaignEntry>;

  end;

  ///  <summary>
  ///  Representa um único produto retornado pela API do Google Ads ou armazenado no banco de dados (WIP).
  ///  </summary>
  TGAdsProductEntry = record
    SKU: String;
    ID_CAMPANHA: Int64;
    TITULO: String;
    LABEL_PRODUTO: String;
    METRICAS_CLIQUES: Integer;
    METRICAS_VALOR_CONVERSOES: Double;
    METRICAS_CONVERSOES: Double;
    METRICAS_CUSTO: Int64;
    METRICAS_TODAS_CONVERSOES: Double;
    METRICAS_IMPRESSOES: Integer;
    DATA_ATUALIZADO: TDateTime;
    DATA_INSERIDO: TDateTime;

    ///  <summary>
    ///  Cria uma nova estrutura com as informações recebidas
    ///  </summary>
    class function New(ASKU: String; AID_Campanha: Int64; ATitulo: String;
      ALabel: String; AMetricasCliques: Integer; AMetricasValorConversoes,
      AMetricasConversoes: Double; AMetricasCusto: Int64;
      AMetricasTodasConversoes: Double; AMetricasImpressoes: Integer;
      ADataAtualizado, ADataInserido: TDateTime): TGAdsProductEntry; static;

    /// <summary>
    /// Cria uma nova estrutura vazia.
    ///  </summary>
    class function EmptyNew(): TGAdsProductEntry; static;

    ///  <summary>
    ///   Cria uma nova estrutura a partir de um objeto JSON retornado pela API.
    ///  </summary>
    ///  <remarks>
    ///  O objeto em questão deve ser especifícamente uma única entrada da array results fornecida pela API.
    ///  </remarks>
    class function NewFromJSON(const AJSON: TJSONObject): TGAdsProductEntry; static;

  end;

  ///  <summary>
  ///  Coleção de Produtos Retornados pela API ou armazenados no banco de dados (WIP).
  ///  </summary>
  TGAdsProducts = class(TObject, IGAdsCollection<TGAdsProductEntry>)
  private
    FProducts: array of TGAdsProductEntry;

    function GetCount(): Integer;
    function GetItem(Index: Integer): TGAdsProductEntry;

    // IInterface methods declaration to override reference counting.
    function _AddRef(): Integer; stdcall;
    function _Release(): Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function GetProduct(Index: Integer): TGAdsProductEntry;
  public
    property Count: Integer read GetCount;
    property Products[Index: Integer]: TGAdsProductEntry read GetProduct;
  end;

  const
    GADS_UT_EMPTY_INTEGER = -1;
    GADS_UT_EMPTY_STRING = '';
    GADS_UT_EMPTY_DOUBLE = -1.0;
    GADS_UT_EMPTY_DATETIME = 0.0;

implementation

{ TGAdsCampaigns }

// must pass the results array here.
class function TGAdsCampaigns.CopyFromJSON(const AJSON: TJSONArray): TGAdsCampaigns;
var
  Val: TJSONValue;
  Obj: TJSONObject;
  Campaign: TGAdsCampaignEntry;
  Len: Integer;
begin
  Result := TGAdsCampaigns.Create();

  for Val in AJSON do
  begin
    if not (Val is TJSONObject)  then
      raise Exception.Create('Pelo menos um elemento não era um objeto JSON.');

    Obj := TJSONObject(Val);
    Campaign := TGAdsCampaignEntry.NewFromJSON(Obj);

    Len := Length(Result.FCAMPAIGNS);
    SetLength(Result.FCAMPAIGNS, Len+1);
    Result.FCAMPAIGNS[Len] := Campaign;
  end;
end;

class function TGAdsCampaigns.CopyFromQuery(AQuery: TFDQuery): TGAdsCampaigns;
var
  Len: Integer;
  CampaignEntry: TGAdsCampaignEntry;
begin
  Result := TGAdsCampaigns.Create();

  AQuery.First();
  while not AQuery.Eof do
  begin
    CampaignEntry := TGAdsCampaignEntry.EmptyNew();
    CampaignEntry.ID := AQuery.FieldByName('id').AsLargeInt;
    CampaignEntry.RESOURCE_NAME := AQuery.FieldByName('resource_name').AsString;
    CampaignEntry.NOME := AQuery.FieldByName('nome').AsString;
    CampaignEntry.STATUS := AQuery.FieldByName('status').AsString;
    CampaignEntry.BUDGET_RESOURCE_NAME := AQuery.FieldByName('budget_resource_name').AsString;
    CampaignEntry.ORCAMENTO := AQuery.FieldByName('orcamento').AsLargeInt;
    CampaignEntry.ORCAMENTO_STATUS := AQuery.FieldByName('orcamento_status').AsString;
    CampaignEntry.DATA_ATUALIZADO := AQuery.FieldByName('data_atualizado').AsDateTime;
    CampaignEntry.DATA_INSERIDO := AQuery.FieldByName('data_inserido').AsDateTime;

    Len := Length(Result.FCAMPAIGNS);
    SetLength(Result.FCAMPAIGNS, Len + 1);
    Result.FCAMPAIGNS[Len] := CampaignEntry;
  end;
end;

constructor TGAdsCampaigns.Create;
begin
  SetLength(FCAMPAIGNS, 0);
end;

destructor TGAdsCampaigns.Destroy;
begin

  inherited;
end;

function TGAdsCampaigns.GetCampaign(Index: Integer): TGAdsCampaignEntry;
begin

  if (Index < 0) or (Index >= Length(FCampaigns)) then
  begin
    raise Exception.Create('Indice excede os limites');
  end;

  Result := FCampaigns[Index];

end;

function TGAdsCampaigns.GetCount: Integer;
begin
  Result := Length(Self.FCAMPAIGNS);
end;

function TGAdsCampaigns.GetEnumerator: TGAdsCollectionEnumerator<TGAdsCampaignEntry>;
begin
  Result := TGAdsCollectionEnumerator<TGAdsCampaignEntry>.Create(Self);
end;

function TGAdsCampaigns.GetItem(Index: Integer): TGAdsCampaignEntry;
begin
  Result := Self.GetCampaign(Index);
end;

function TGAdsCampaigns.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TGAdsCampaigns._AddRef: Integer;
begin
  Result := -1;
end;

function TGAdsCampaigns._Release: Integer;
begin
  Result := -1;
end;

{ TGAdsCampaignEntry }

class function TGAdsCampaignEntry.EmptyNew: TGAdsCampaignEntry;
begin
  Result := TGAdsCampaignEntry.New(GADS_UT_EMPTY_INTEGER,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_INTEGER,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_DATETIME,
  GADS_UT_EMPTY_DATETIME);
end;

class function TGAdsCampaignEntry.New(AID: Int64; AResourceName, ANome, AStatus,
  ATipoCanal, ABudgetResourceName: String; AOrcamento: Int64; AOrcamentoStatus: String;
  ADataAtualizado, ADataInserido: TDateTime): TGAdsCampaignEntry;
begin
  Result.ID := AID;
  Result.RESOURCE_NAME := AResourceName;
  Result.NOME := ANome;
  Result.BUDGET_RESOURCE_NAME := ABudgetResourceName;
  Result.ORCAMENTO := AOrcamento;
  Result.STATUS := AStatus;
  Result.TIPO_CANAL := ATipoCanal;
  Result.ORCAMENTO_STATUS := AOrcamentoStatus;
  Result.DATA_ATUALIZADO := ADataAtualizado;
  Result.DATA_INSERIDO := ADataInserido;
end;

class function TGAdsCampaignEntry.NewFromJSON(
  const AJSON: TJSONObject): TGAdsCampaignEntry;
var
  CampaignObject: TJSONObject;
  BudgetObject: TJSONObject;
  Success: Boolean;
begin
  if (not AJSON.TryGetValue<TJSONObject>('campaign', CampaignObject)) or (not AJSON.TryGetValue<TJSONObject>('campaignBudget', BudgetObject)) then
    raise Exception.Create('O objeto não é uma campanha válida.');

  Success := True;

  // Extracting Campaign Data
  Success := Success and CampaignObject.TryGetValue<Int64>('id', Result.ID);
  Success := Success and CampaignObject.TryGetValue<String>('resourceName', Result.RESOURCE_NAME);
  Success := Success and CampaignObject.TryGetValue<String>('status', Result.STATUS);
  Success := Success and CampaignObject.TryGetValue<String>('name', Result.NOME);
  Success := Success and CampaignObject.TryGetValue<String>('advertisingChannelType', Result.TIPO_CANAL);

  // Extracting Budget Data
  Success := Success and BudgetObject.TryGetValue<String>('resourceName', Result.BUDGET_RESOURCE_NAME);
  Success := Success and BudgetObject.TryGetValue<String>('status', Result.ORCAMENTO_STATUS);
  Success := Success and BudgetObject.TryGetValue<Int64>('amountMicros', Result.ORCAMENTO);

  if not Success then
    raise Exception.Create('O objeto não é uma campanha válida.');


end;

{ TGAdsCampaigns.TGAdsCampaignsEnumerator }



{ TGAdsCampaignsEnumerator<T, U> }

constructor TGAdsCollectionEnumerator<U>.Create(const ACollection: IGAdsCollection<U>);
begin
  FIndex := 0;
  FCollection := ACollection;
end;

function TGAdsCollectionEnumerator<U>.GetCurrent: U;
begin
  Result := FCollection.GetItem(FIndex);
end;

function TGAdsCollectionEnumerator<U>.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FCollection.Count;
end;

{ TGAdsProducts }

function TGAdsProducts.GetCount: Integer;
begin
  Result := Length(FProducts);
end;

function TGAdsProducts.GetItem(Index: Integer): TGAdsProductEntry;
begin
  Result := Self.GetProduct(Index);
end;

function TGAdsProducts.GetProduct(Index: Integer): TGAdsProductEntry;
begin
  if (Index < 0) or (Index >= Length(FProducts)) then
  begin
    raise Exception.Create('Indice excede os limites');
  end;

  Result := FProducts[Index];
end;

function TGAdsProducts.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TGAdsProducts._AddRef: Integer;
begin
  Result := -1;
end;

function TGAdsProducts._Release: Integer;
begin
  Result := -1;
end;

{ TGAdsProductEntry }

class function TGAdsProductEntry.EmptyNew: TGAdsProductEntry;
begin
  Result := TGAdsProductEntry.New(GADS_UT_EMPTY_STRING,    //SKU
  GADS_UT_EMPTY_INTEGER,  // Id campanha
  GADS_UT_EMPTY_STRING,   // Titulo
  GADS_UT_EMPTY_STRING,   // Label
  GADS_UT_EMPTY_INTEGER,  // Cliques
  GADS_UT_EMPTY_DOUBLE,   // ValorConversoes
  GADS_UT_EMPTY_DOUBLE,   // Conversoes
  GADS_UT_EMPTY_INTEGER,  // Custo
  GADS_UT_EMPTY_DOUBLE,   // TodasConversoes
  GADS_UT_EMPTY_INTEGER,  // Impressoes
  GADS_UT_EMPTY_DATETIME,
  GADS_UT_EMPTY_DATETIME);
end;

class function TGAdsProductEntry.New(ASKU: String; AID_Campanha: Int64; ATitulo,
  ALabel: String; AMetricasCliques: Integer; AMetricasValorConversoes,
  AMetricasConversoes: Double; AMetricasCusto: Int64;
  AMetricasTodasConversoes: Double; AMetricasImpressoes: Integer;
  ADataAtualizado, ADataInserido: TDateTime): TGAdsProductEntry;
begin
  Result.SKU := ASKU;
  Result.ID_CAMPANHA := AID_Campanha;
  Result.TITULO := ATitulo;
  Result.LABEL_PRODUTO := ALabel;
  Result.METRICAS_CLIQUES := AMetricasCliques;
  Result.METRICAS_VALOR_CONVERSOES := AMetricasValorConversoes;
  Result.METRICAS_CONVERSOES := AMetricasConversoes;
  Result.METRICAS_CUSTO := AMetricasCusto;
  Result.METRICAS_TODAS_CONVERSOES := AMetricasTodasConversoes;
  Result.METRICAS_IMPRESSOES := AMetricasImpressoes;
  Result.DATA_ATUALIZADO := ADataAtualizado;
  Result.DATA_INSERIDO := ADataInserido;
end;

class function TGAdsProductEntry.NewFromJSON(const
  AJSON: TJSONObject): TGAdsProductEntry;
var
  CampaignObject, MetricsObject, SegmentsObject: TJSONObject;
  Success: Boolean;
begin
  if not (
    AJSON.TryGetValue<TJSONObject>('campaign', CampaignObject) and
    AJSON.TryGetValue<TJSONObject>('metrics', MetricsObject) and
    AJSON.TryGetValue<TJSONObject>('segments', SegmentsObject)
  ) then
    raise Exception.Create('O objeto não é uma entrada de produto valida');

  Success := True;

  // Extracting Segments Data
  Success := Success and SegmentsObject.TryGetValue<String>('productItemId', Result.SKU);
  Success := Success and SegmentsObject.TryGetValue<String>('productTitle', Result.TITULO);
  Result.LABEL_PRODUTO := SegmentsObject.GetValue<String>('productFeedLabel', '');

  // Extracting Campaign Data
  Success := Success and CampaignObject.TryGetValue<Int64>('id', Result.ID_CAMPANHA);

  // Extracting Metrics Data
  Success := Success and MetricsObject.TryGetValue<Integer>('clicks', Result.METRICAS_CLIQUES);
  Success := Success and MetricsObject.TryGetValue<Double>('conversionsValue', Result.METRICAS_VALOR_CONVERSOES);
  Success := Success and MetricsObject.TryGetValue<Double>('conversions', Result.METRICAS_CONVERSOES);
  Success := Success and MetricsObject.TryGetValue<Int64>('costMicros', Result.METRICAS_CUSTO);
  Success := Success and MetricsObject.TryGetValue<Double>('allConversions', Result.METRICAS_TODAS_CONVERSOES);
  Success := Success and MetricsObject.TryGetValue<Integer>('impressions', Result.METRICAS_IMPRESSOES);

  if not Success then
    raise Exception.Create('O objeto não é uma entrada de produto válida');

end;

end.
