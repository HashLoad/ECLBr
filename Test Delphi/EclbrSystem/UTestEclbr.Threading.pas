unit UTestEclbr.Threading;

interface

uses
  DUnitX.TestFramework,
  Rtti,
  SysUtils,
  DateUtils,
  Classes,
  Generics.Collections,
  eclbr.threading,
  eclbr.std;

type
  [TestFixture]
  TTesTStd = class
  private
    function FetchData: TValue;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestRunProc;
    [Test]
    procedure TestNoAwaitProc;
    [Test]
    procedure TestAwaitProc;
    [Test]
    procedure TestRunFutureProc;
    [Test]
    procedure TestAwaitFunc;
    [Test]
    procedure TestAwaitFuture;
    [Test]
    procedure TestFetchAsyncAwait;
  end;

implementation

procedure TTesTStd.Setup;
begin

end;

procedure TTesTStd.TearDown;
begin

end;

procedure TTesTStd.TestAwaitProc;
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
  Assert.IsTrue(LFuture.Ok<Boolean>);
end;

procedure TTesTStd.TestFetchAsyncAwait;
var
  LFuture: TFuture;
begin
  LFuture := Async(FetchData).Await;

  Assert.IsTrue(LFuture.IsOk);
  Assert.AreEqual(LFuture.Ok<String>, 'sucesso!');
end;

procedure TTesTStd.TestNoAwaitProc;
var
  LFuture: TFuture;
  LExecuted: Boolean;
begin
  LFuture := Async(procedure
                   begin
                     LExecuted := True;
                   end)
            .NoAwait;

  Assert.IsTrue(LFuture.Ok<Boolean>);
end;

function TTesTStd.FetchData: TValue;
begin
  Sleep(50);
  Result := 'sucesso!';
end;

procedure TTesTStd.TestAwaitFunc;
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
  Assert.AreEqual(LFuture.Ok<String>, 'Delphi Await');
end;

procedure TTesTStd.TestAwaitFuture;
var
  LFuture: TFuture;
  LContinue: Boolean;
begin
  LContinue := False;
  LFuture := Async(function: TValue
                   begin
                     Result := 'Await and Continue';
                   end)
            .Await(procedure
                   begin
                     LContinue := True;
                   end);

  Assert.IsTrue(LContinue);
  Assert.IsTrue(LFuture.IsOk);
  Assert.AreEqual(LFuture.Ok<String>, 'Await and Continue');
end;

procedure TTesTStd.TestRunProc;
var
  LFuture: TFuture;
  LExecuted: Boolean;
begin
  LFuture := Async(procedure
                   begin
                     LExecuted := True;
                   end)
            .Run;

  Assert.IsTrue(LFuture.Ok<Boolean>);
end;

procedure TTesTStd.TestRunFutureProc;
var
  LFuture: TFuture;
  LErr: String;
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
  LErr := 'The "Run" method should not be invoked as a function. Utilize the "Await" method to wait for task completion and access the result, or invoke it as a procedure.';
  Assert.AreEqual(LErr, LFuture.Err);
end;

initialization
  TDUnitX.RegisterTestFixture(TTesTStd);

end.
