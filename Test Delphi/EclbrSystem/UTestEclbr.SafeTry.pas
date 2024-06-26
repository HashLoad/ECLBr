unit UTestEclbr.SafeTry;

interface

uses
  DUnitX.TestFramework, eclbr.safetry, SysUtils;

type
  [TestFixture]
  TTestSafeTry = class
  public
    [Test]
    procedure TestTryExceptFinally_Ok;
    [Test]
    procedure TestTryExceptFinally_Exception;
    [Test]
    procedure TestTryOnly_Ok;
    [Test]
    procedure TestTryOnly_Exception;
    [Test]
    procedure TestTryFinally_Ok;
    [Test]
    procedure TestTryFinally_Exception;
    [Test]
    procedure TestFinallyOnly;
    [Test]
    procedure TestNoTry;
    [Test]
    procedure TestEndWithoutException;
    [Test]
    procedure TestEndWithException;
  end;

implementation

{ TTestSafeTry }

procedure TTestSafeTry.TestTryExceptFinally_Ok;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try(procedure
                           begin
                             WriteLn('Executando try');
                           end)
                     .&Except(procedure(E: Exception)
                              begin
                                WriteLn('Executando except');
                              end)
                     .&Finally(procedure
                               begin
                                 WriteLn('Executando finally');
                               end)
                     .&End;

  Assert.IsTrue(LResult.IsOk);
  Assert.IsFalse(LResult.IsErr);
end;

procedure TTestSafeTry.TestTryExceptFinally_Exception;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try(procedure
                           begin
                             raise Exception.Create('Erro no bloco try');
                           end)
                     .&Except(procedure(E: Exception)
                              begin
                                WriteLn('Executando except');
                              end)
                     .&Finally(procedure
                               begin
                                 WriteLn('Executando finally');
                               end)
                     .&End;

  Assert.IsTrue(LResult.IsErr);
  Assert.AreEqual('Erro no bloco try', LResult.GetExceptionMessage);
end;

procedure TTestSafeTry.TestTryOnly_Ok;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try(
    procedure
    begin
      WriteLn('Executando try');
    end)
  .&End;

  Assert.IsTrue(LResult.IsOk);
  Assert.IsFalse(LResult.IsErr);
end;

procedure TTestSafeTry.TestTryOnly_Exception;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try(
    procedure
    begin
      raise Exception.Create('Erro no bloco try');
    end)
  .&End;

  Assert.IsTrue(LResult.IsErr);
  Assert.AreEqual('Erro no bloco try', LResult.GetExceptionMessage);
end;

procedure TTestSafeTry.TestTryFinally_Ok;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try(
    procedure
    begin
      WriteLn('Executando try');
    end)
  .&Finally(
    procedure
    begin
      WriteLn('Executando finally');
    end)
  .&End;

  Assert.IsTrue(LResult.IsOk);
  Assert.IsFalse(LResult.IsErr);
end;

procedure TTestSafeTry.TestTryFinally_Exception;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try(
    procedure
    begin
      raise Exception.Create('Erro no bloco try');
    end)
  .&Finally(
    procedure
    begin
      WriteLn('Executando finally');
    end)
  .&End;

  Assert.IsTrue(LResult.IsErr);
  Assert.AreEqual('Erro no bloco try', LResult.GetExceptionMessage);
end;

procedure TTestSafeTry.TestFinallyOnly;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try.&Finally(
    procedure
    begin
      WriteLn('Executando finally');
    end)
  .&End;

  Assert.IsTrue(LResult.IsOk);
  Assert.IsFalse(LResult.IsErr);
end;

procedure TTestSafeTry.TestNoTry;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try.&End;

  Assert.IsTrue(LResult.IsOk);
  Assert.IsFalse(LResult.IsErr);
end;

procedure TTestSafeTry.TestEndWithoutException;
var
  Resultado: TSafeResult<Boolean>;
begin
  Resultado := TSafeTry.&Try(
    procedure
    begin
      WriteLn('Executando try');
    end)
  .&End;

  Assert.IsTrue(Resultado.IsOk);
  Assert.IsFalse(Resultado.IsErr);
end;

procedure TTestSafeTry.TestEndWithException;
var
  LResult: TSafeResult<Boolean>;
begin
  LResult := TSafeTry.&Try(
    procedure
    begin
      raise Exception.Create('Erro no bloco try');
    end)
  .&End;

  Assert.IsTrue(LResult.IsErr);
  Assert.AreEqual('Erro no bloco try', LResult.GetExceptionMessage);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSafeTry);

end.
