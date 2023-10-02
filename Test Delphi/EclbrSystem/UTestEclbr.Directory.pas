unit UTestEclbr.Directory;

interface

uses
  DUnitX.TestFramework,
  Rtti,
  SysUtils,
  Generics.Collections,
  eclbr.directory,
  eclbr.vector;

type
  TDirExTest = class
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestMap;
    [Test]
    procedure TestFilter;
  end;

implementation

{ TArrayDataTest }

procedure TDirExTest.Setup;
begin

end;

procedure TDirExTest.TearDown;
begin

end;

procedure TDirExTest.TestFilter;
var
  LDir: TDirEx;
  LFiles: TVector<string>;
  LFilteredFiles: TVector<string>;
  LFile: string;
begin
  // Cria uma inst�ncia de TDirEx
  LDir := TDirEx.New('caminho_do_diretorio');

  // Obt�m a lista de arquivos no diret�rio
  LFiles := LDir.GetFiles('');

  // Usa o m�todo Filter para encontrar todos os arquivos com extens�o .txt
  LFilteredFiles := LFiles.Filter(
    function(Value: string): Boolean
    begin
      Result := TPath.GetExtension(Value).ToLower = '.txt';
    end);

  // Agora voc� pode verificar ou fazer algo com a lista de arquivos filtrados
  // Por exemplo, verificar se todos os arquivos t�m extens�o .txt
  for LFile in LFilteredFiles do
    Assert.AreEqual('.txt', TPath.GetExtension(LFile).ToLower);
end;

procedure TDirExTest.TestMap;
var
  LDir: TDirEx;
  LFiles: TVector<string>;
  LPrefixedFiles: TVector<string>;
begin
  // Cria uma inst�ncia de TDirEx
  LDir := TDirEx.New('caminho_do_diretorio');

  // Obt�m a lista de arquivos no diret�rio
  LFiles := LDir.GetFiles('');

  // Usa o m�todo Map para adicionar um prefixo aos nomes de arquivo
  LPrefixedFiles := LFiles.Map<string>(function(Value: string): string
                                       begin
                                         Result := 'Prefixo_' + Value;
                                       end);

  // Agora voc� pode verificar ou fazer algo com a lista de arquivos com prefixo
  // Por exemplo, verificar se os arquivos est�o corretamente prefixados
  Assert.AreEqual('Prefixo_arquivo1.txt', LPrefixedFiles[0]);
  Assert.AreEqual('Prefixo_arquivo2.txt', LPrefixedFiles[1]);
end;


initialization
  TDUnitX.RegisterTestFixture(TDirExTest);

end.

