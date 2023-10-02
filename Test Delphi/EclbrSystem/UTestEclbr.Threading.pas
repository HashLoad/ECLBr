unit UTestEclbr.Threading;

interface

uses
  DUnitX.TestFramework,
  Rtti,
  SysUtils,
  DateUtils,
  Classes,
  Generics.Collections,
  eclbr.threading;

type
  [TestFixture]
  TTestUtils = class
  private
    function FetchData: TValue;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestExec;
    [Test]
    procedure TestAwaitProc;
    [Test]
    procedure TestRunFuture;
    [Test]
    procedure TestAwaitFunc;
    [Test]
    procedure TestAwaitFuture;
    [Test]
    procedure TestFetchAsyncAwait;
  end;

implementation

procedure TTestUtils.Setup;
begin

end;

procedure TTestUtils.TearDown;
begin

end;

procedure TTestUtils.TestAwaitProc;
var
  LFuture: TFuture;
  LExecuted: Boolean;
begin
  LFuture := Async(procedure
                   begin
                     LExecuted := True;
                   end)
            .Await;

  Assert.IsTrue(LFuture.IsOk);
  Assert.IsTrue(LFuture.Ok<boolean>);
end;

procedure TTestUtils.TestFetchAsyncAwait;
var
  LFuture: TFuture;
begin
  LFuture := Async(FetchData).Await;

  Assert.IsTrue(LFuture.IsOk);
  Assert.AreEqual(LFuture.Ok<string>, 'sucesso!');
end;

function TTestUtils.FetchData: TValue;
begin
  Sleep(50);
  Result := 'sucesso!';
end;

procedure TTestUtils.TestAwaitFunc;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     Sleep(300);
                     Result := 'Delphi Await';
                   end)
            .Await;

  Assert.IsTrue(LFuture.IsOk);
  Assert.AreEqual(LFuture.Ok<string>, 'Delphi Await');
end;

procedure TTestUtils.TestAwaitFuture;
var
  LFuture: TFuture;
  LContinue: boolean;
begin
  LContinue := false;
  LFuture := Async(function: TValue
                   begin
                     Result := 'Await and Continue';
                   end)
            .Await(procedure
                   begin
                     LContinue := true;
                   end);

  Assert.IsTrue(LContinue);
  Assert.IsTrue(LFuture.IsOk);
  Assert.AreEqual(LFuture.Ok<string>, 'Await and Continue');
end;

procedure TTestUtils.TestExec;
var
  LFuture: TFuture;
  LExecuted: Boolean;
begin
  LFuture := Async(procedure
                   begin
                     LExecuted := True;
                   end)
            .Run;

  Assert.IsTrue(LFuture.Ok<boolean>);
end;

procedure TTestUtils.TestRunFuture;
var
  LFuture: TFuture;
  LErr: string;
begin
  // Neste caso no ECLBr o "Run" não usa Function somente Procedure, se quiser
  // usar Function use o "Await", pois ele espera o resultado para devolver, o
  // "Run" não fica esperando, somente aloca a thread e executa
  LFuture := Async(function: TValue
                   begin
                     Result := False;
                   end)
            .Run;

  Assert.IsTrue(LFuture.IsErr);
  LErr := 'O método "Exec" não deve ser chamado como função. Use o método "Await" para aguardar a conclusão da tarefa e acessar o resultado, ou chame-o como procedure.';
  Assert.AreEqual(LErr, LFuture.Err);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestUtils);

end.
