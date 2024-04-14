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

uses IOUtils;

{ TArrayDataTest }

procedure TDirExTest.Setup;
begin

end;

procedure TDirExTest.TearDown;
begin

end;

procedure TDirExTest.TestFilter;
var
  LFiles: TVector<String>;
  LFilteredFiles: TVector<String>;
  LFile: String;
begin
  // Obtém a lista de arquivos no diretório
  LFiles := TDirEx.New('caminho_do_diretorio').GetFiles('');

  // Usa o método Filter para encontrar todos os arquivos com extensão .txt
  LFilteredFiles := LFiles.Filter(
    function(Value: String): Boolean
    begin
      Result := TPath.GetExtension(Value).ToLower = '.txt';
    end);

  // Agora você pode verificar ou fazer algo com a lista de arquivos filtrados
  // Por exemplo, verificar se todos os arquivos têm extensão .txt
  for LFile in LFilteredFiles do
    Assert.AreEqual('.txt', TPath.GetExtension(LFile).ToLower);
end;

procedure TDirExTest.TestMap;
var
  LFiles: TVector<String>;
  LPrefixedFiles: TVector<String>;
begin
  // Obtém a lista de arquivos no diretório
  LFiles := TDirEx.New('caminho_do_diretorio').GetFiles('');

  // Usa o método Map para adicionar um prefixo aos nomes de arquivo
  LPrefixedFiles := LFiles.Map<String>(function(Value: String): String
                                       begin
                                         Result := 'Prefixo_' + Value;
                                       end);

  // Agora você pode verificar ou fazer algo com a lista de arquivos com prefixo
  // Por exemplo, verificar se os arquivos estão corretamente prefixados
  Assert.AreEqual('Prefixo_arquivo1.txt', LPrefixedFiles[0]);
  Assert.AreEqual('Prefixo_arquivo2.txt', LPrefixedFiles[1]);
end;


initialization
  TDUnitX.RegisterTestFixture(TDirExTest);

end.


