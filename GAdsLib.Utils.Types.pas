unit GAdsLib.Utils.Types;

interface

uses FireDAC.Comp.Client, System.JSON, System.SysUtils, System.Classes;

type

  ///  <summary>
  ///  Representa uma única campanha retornada pela API ou armazenada no banco de dados (WIP).
  ///  </summary>
  TGAdsCampaignEntry = record
    ID: Int64;
    RESOURCE_NAME: String;
    NOME: String;
    STATUS: String;
    BUDGET_RESOURCE_NAME: String;
    ORCAMENTO: Int64;
    ORCAMENTO_STATUS: String;
    DATA_ATUALIZADO: TDateTime;
    DATA_INSERIDO: TDateTime;

    ///  <summary>
    ///  Cria uma nova estrutura com as informações recebidas
    ///  </summary>
    class function New(AID: Int64; AResourceName, ANome, AStatus, ABudgetResourceName: String;
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
    class function NewFromJSON(AJSON: TJSONObject): TGAdsCampaignEntry; static;

    ///  <summary>
    ///  Retorna uma string SQL formatada para inserção ou atualização da entrada em questão no banco de dados.
    ///  </summary>
    function GetInsertQuery(): String;

  end;

  ///  <summary>
  ///  Representa uma coleção de campanhas retornada pela API do Google Ads ou armazenada no banco de dados (WIP)
  ///  <summary>
  TGAdsCampaigns = class
  private
    FCAMPAIGNS: array of TGAdsCampaignEntry;

    constructor Create;
    function GetCampaignsCount: Integer;
    function GetCampaign(Index: Integer): TGAdsCampaignEntry;

  ///  <summary>
  ///   Classe enumeradora da coleção <see cref="TGAdsCampaigns"/>.
  ///  </summary>
  public type TGAdsCampaignsEnumerator = class
    private
      FParent: TGAdsCampaigns;
      FIndex: Integer;
    public
      constructor Create(AParent: TGAdsCampaigns);
      function GetCurrent(): TGAdsCampaignEntry;
      function MoveNext(): Boolean;
      property Current: TGAdsCampaignEntry read GetCurrent;
  end;

  public
    ///  <summary>
    ///  Quantidade de elementos na coleção.
    ///  </summary>
    property Count: Integer read GetCampaignsCount;
    ///  <summary>
    ///  Acesso indexado aos elementos da coleção.
    ///  </summary>
    property Campaigns[Index: Integer]: TGAdsCampaignEntry read GetCampaign;

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
    class function CopyFromJSON(AJSON: TJSONArray): TGAdsCampaigns; static;

    ///  <summary>
    ///  Habilita a coleção a ser utilizada em <i>loops</i> do tipo <c>for ... in ...</c>
    ///  </summary>
    function GetEnumerator(): TGAdsCampaignsEnumerator;

  end;

  const
    GADS_UT_EMPTY_INTEGER = -1;
    GADS_UT_EMPTY_STRING = '';
    GADS_UT_EMPTY_DATETIME = 0.0;

implementation

{ TGAdsCampaigns }

// must pass the results array here.
class function TGAdsCampaigns.CopyFromJSON(AJSON: TJSONArray): TGAdsCampaigns;
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

function TGAdsCampaigns.GetCampaign(Index: Integer): TGAdsCampaignEntry;
begin

  if (Index < 0) or (Index >= Length(FCAMPAIGNS)) then
  begin
    raise EListError.Create('Indice excede os limites');
  end;

  Result := FCAMPAIGNS[Index];
  
end;

function TGAdsCampaigns.GetCampaignsCount: Integer;
begin
  Result := Length(Self.FCAMPAIGNS);
end;

function TGAdsCampaigns.GetEnumerator: TGAdsCampaignsEnumerator;
begin
  Result := TGAdsCampaignsEnumerator.Create(Self);
end;

{ TGAdsCampaignEntry }

class function TGAdsCampaignEntry.EmptyNew: TGAdsCampaignEntry;
begin
  Result := TGAdsCampaignEntry.New(GADS_UT_EMPTY_INTEGER,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_INTEGER,
  GADS_UT_EMPTY_STRING,
  GADS_UT_EMPTY_DATETIME,
  GADS_UT_EMPTY_DATETIME);
end;

function TGAdsCampaignEntry.GetInsertQuery: String;
var
  InsertValues: String;
const
  InsertQuery = 'UPDATE OR INSERT INTO gads_campanhas (%s) VALUES (%s) MATCHING (%s)';
  InsertFields = 'id, resource_name, nome, status, budget_resource_name, orcamento, orcamento_status, data_atualizado';
begin
  InsertValues := Format('%d, ''%s'', ''%s'', ''%s'', ''%s'', %s, ''%s'', ''%s''', [ID, 
  RESOURCE_NAME,
  NOME,
  STATUS,
  BUDGET_RESOURCE_NAME,
  IntToStr(ORCAMENTO),
  ORCAMENTO_STATUS,
  FormatDateTime('yyyy-mm-dd hh:nn:ss', Now())]);
  
  Result := Format(InsertQuery, [InsertFields, InsertValues,'id']);
end;

class function TGAdsCampaignEntry.New(AID: Int64; AResourceName, ANome, AStatus,
  ABudgetResourceName: String; AOrcamento: Int64; AOrcamentoStatus: String;
  ADataAtualizado, ADataInserido: TDateTime): TGAdsCampaignEntry;
begin
  Result.ID := AID;
  Result.RESOURCE_NAME := AResourceName;
  Result.NOME := ANome;
  Result.BUDGET_RESOURCE_NAME := ABudgetResourceName;
  Result.ORCAMENTO := AOrcamento;
  Result.ORCAMENTO_STATUS := AOrcamentoStatus;
  Result.DATA_ATUALIZADO := ADataAtualizado;
  Result.DATA_INSERIDO := ADataInserido;
end;

class function TGAdsCampaignEntry.NewFromJSON(
  AJSON: TJSONObject): TGAdsCampaignEntry;
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

  // Extracting Budget Data
  Success := Success and BudgetObject.TryGetValue<String>('resourceName', Result.BUDGET_RESOURCE_NAME);
  Success := Success and BudgetObject.TryGetValue<String>('status', Result.ORCAMENTO_STATUS);
  Success := Success and BudgetObject.TryGetValue<Int64>('amountMicros', Result.ORCAMENTO);

  if not Success then
    raise Exception.Create('O objeto não é uma campanha válida.');

  
end;

{ TGAdsCampaigns.TGAdsCampaignsEnumerator }

constructor TGAdsCampaigns.TGAdsCampaignsEnumerator.Create(
  AParent: TGAdsCampaigns);
begin
  FIndex := 0;
  FParent := AParent;
end;

function TGAdsCampaigns.TGAdsCampaignsEnumerator.GetCurrent: TGAdsCampaignEntry;
begin
  Result := FParent.FCAMPAIGNS[FIndex];
end;

function TGAdsCampaigns.TGAdsCampaignsEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FParent.Count;
end;

end.
