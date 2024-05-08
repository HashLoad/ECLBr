unit UTestEclbr.ResultPair;

interface

uses
  DUnitX.TestFramework, System.SysUtils, eclbr.result.pair, Rtti;

type
  TTestTResultPair = class
  private
    FDividend: Integer;
    FDivisor: Integer;
    FSuccessValue: Integer;
    FFailureValue: String;
    function _ResultTryExcept: TResultPair<Integer, String>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestMap;
    [Test]
    procedure TestMapTryException;
    [Test]
    procedure TestSuccess;
    [Test]
    procedure TestFailure;
    [Test]
    procedure TestTryException;
    [Test]
    procedure TestFlatMap;
    [Test]
    procedure TestFlatMapFailure;
    [Test]
    procedure TestPure;
    [Test]
    procedure TestPureFailure;
    [Test]
    procedure TestSwap;
    [Test]
    procedure TestRecover;
    [Test]
    procedure TestReduceSuccess;
    [Test]
    procedure TestReduceFailure;
    [Test]
    procedure TestGetSuccessOrElse;
    [Test]
    procedure TestGetSuccessOrException;
    [Test]
    procedure TestGetSuccessOrDefaultNoDefault;
    [Test]
    procedure TestGetSuccessOrDefaultWithDefault;
    [Test]
    procedure TestGetFailureOrElse;
    [Test]
    procedure TestGetFailureOrException;
    [Test]
    procedure TestGetFailureOrDefaultNoDefault;
    [Test]
    procedure TestGetFailureOrDefaultWithDefault;
    [Test]
    procedure TestCalculateTotalPrice;
    [Test]
    procedure TestCheckoutWithOutOfStockItem;
  end;

implementation

{ TestTResultPair }

function TTestTResultPair._ResultTryExcept: TResultPair<Integer, String>;
begin
  try
    Result.Success(42);
  except
    Result.Failure('Falilure');
  end;
end;

procedure TTestTResultPair.Setup;
begin
  FDividend := 10;
  FDivisor := 2;
  FSuccessValue := 42;
  FFailureValue := 'Error';
end;

procedure TTestTResultPair.TearDown;
begin

end;

procedure TTestTResultPair.TestCalculateTotalPrice;
var
  LTotalPrice: Double;
  LResultPair: TResultPair<Double, String>;
begin
  // Comece com Success() porque o carrinho está vazio no início
  try
    LResultPair := TResultPair<Double, String>.New
      .Success(0.0)
      .ThenOf(
        function (const ASubtotal: Double): TValue
        begin
          // Adicione um item ao carrinho
          Result := ASubtotal + 29.99;
        end
      )
      .ThenOf(
        function (const ASubtotal: Double): TValue
        begin
          // Adicione outro item ao carrinho
          Result := ASubtotal + 19.99;
        end
      ).Return;

    // Verifique se o resultado é sucesso e obtenha o preço total
    if LResultPair.IsSuccess then
      LTotalPrice := LResultPair.SuccessOrDefault(0.0)
    else
      LTotalPrice := 0.0;

    Assert.AreEqual(49.98, LTotalPrice); // Verifica se o preço total está correto
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestCheckoutWithOutOfStockItem;
var
  LResultPair: TResultPair<Boolean, String>;
begin
  try
    // Comece com Success() porque o carrinho não está vazio no início
    LResultPair := TResultPair<Boolean, String>.New
      .Success(True)
      .ThenOf(
        function (const CartNotEmpty: Boolean): TValue
        begin
          // Verifique se o item do carrinho está em estoque (simulação de falha)
          if not CartNotEmpty then
            Result := 'Carrinho vazio!'
          else
            Result := True;
        end)
      .ThenOf(
        function (const ItemInStock: Boolean): TValue
        begin
          // Tente prosseguir com o pagamento se o item estiver em estoque
          if ItemInStock then
            Result := True
          else
            Result := 'Item fora de estoque!';
        end
      ).Return;
    // Verifique se o resultado é sucesso ou falha
    if LResultPair.IsSuccess then
      Assert.IsTrue(LResultPair.SuccessOrDefault(False))
    else
      Assert.AreEqual('Item fora de estoque!', LResultPair.FailureOrDefault('Erro desconhecido'));
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestFailure;
var
  LResultPair: TResultPair<Integer, String>;
begin
  LResultPair := TResultPair<Integer, String>.New.Failure('Error');
  try
    Assert.IsFalse(LResultPair.isSuccess);
    Assert.IsTrue(LResultPair.isFailure);
    Assert.AreEqual('Error', LResultPair.ValueFailure);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestFlatMap;
var
  LResultPair: TResultPair<Integer, String>;
begin
  try
    LResultPair := TResultPair<Integer, String>.New.Success(FSuccessValue)
      .FlatMap<Integer>(
        function(Value: Integer): Integer
        var
          LResult: TResultPair<Integer, String>;
        begin
          try
            LResult := TResultPair<Integer, String>.New.Success(Value * 2);
            Result := LResult.ValueSuccess;
          finally
            LResult.Dispose;
          end;
        end);

    Assert.IsTrue(LResultPair.isSuccess);
    Assert.AreEqual(FSuccessValue * 2, LResultPair.ValueSuccess);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestFlatMapFailure;
var
  LResultPair: TResultPair<Integer, String>;
begin
  try
    LResultPair := TResultPair<Integer, String>.New.Failure(FFailureValue)
               .FlatMap<String>(
                  function(Error: String): String
                  var
                    LResult: TResultPair<Integer, String>;
                  begin
                    try
                      LResult := TResultPair<Integer, String>.New.Failure(Error + 'Handled');
                      Result := LResult.ValueFailure;
                    finally
                      LResult.Dispose;
                    end;
                  end);

    Assert.IsTrue(LResultPair.isFailure);
    Assert.AreEqual(FFailureValue + 'Handled', LResultPair.ValueFailure);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestMap;
var
  LResultPair: TResultPair<Double, String>;
  LResult: Double;
begin
  try
    LResultPair := LResultPair
               .Success(FDividend div FDivisor)
               .Map<Double>(function(Value: Double): Double
                            begin
                              Result := Value * 2.5;
                            end);

    LResult := (FDividend div FDivisor) * 2.5;
    Assert.AreEqual(LResultPair.ValueSuccess, LResult, '');
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestMapTryException;
var
  LResultPair: TResultPair<Double, String>;
  LResult: Double;
begin
  try
    LResultPair := TResultPair<Double, String>.New
                   .Success(42)
                   .Map<Double>(function(Value: Double): Double
                                begin
                                  Result := Value * 2.5;
                                end);

    LResult := 42 * 2.5;
    Assert.AreEqual(LResultPair.ValueSuccess, LResult);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestPure;
var
  LResultPair: TResultPair<Integer, String>;
begin
  try
    LResultPair := TResultPair<Integer, String>.New.Pure(FSuccessValue);

    Assert.IsTrue(LResultPair.isSuccess);
    Assert.AreEqual(FSuccessValue, LResultPair.ValueSuccess);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestPureFailure;
var
  LResultPair: TResultPair<Integer, String>;
begin
  try
    LResultPair := TResultPair<Integer, String>.New.Pure(FFailureValue);

    Assert.IsTrue(LResultPair.isFailure);
    Assert.AreEqual(FFailureValue, LResultPair.ValueFailure);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestRecover;
var
  LResultPair: TResultPair<Integer, String>;
  LRecoveredPair: TResultPair<Double, Integer>;
  LLength: Double;
begin
  try
    LResultPair := TResultPair<Integer, String>.New.Failure(FFailureValue);
    LRecoveredPair := LResultPair.Recover<Double>(
      function(Error: String): Double
      begin
        if Error = 'Error' then
          Result := Length(Error)
        else
          Result := 0;
      end);

    LLength := Length(FFailureValue);
    Assert.IsTrue(LRecoveredPair.isSuccess);
    Assert.AreEqual(LLength, LRecoveredPair.ValueSuccess);
  finally
    LResultPair.Dispose;
    LRecoveredPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestSuccess;
var
  LResultPair: TResultPair<Integer, String>;
begin
  LResultPair := TResultPair<Integer, String>.New.Success(42);
  try
    Assert.IsTrue(LResultPair.isSuccess);
    Assert.IsFalse(LResultPair.isFailure);
    Assert.AreEqual(42, LResultPair.ValueSuccess);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestSwap;
var
  LResultPair: TResultPair<Integer, String>;
  LSwappedPair: TResultPair<String, Integer>;
begin
  try
    LResultPair := TResultPair<Integer, String>.New.Failure(FFailureValue);
    LSwappedPair := LResultPair.Swap;

    Assert.IsTrue(LSwappedPair.isSuccess);
    Assert.AreEqual(FFailureValue, LSwappedPair.ValueSuccess);
  finally
    LResultPair.Dispose;
    LSwappedPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestTryException;
var
  LResultPair: TResultPair<Integer, String>;
  LSuccessCalled: Boolean;
  LFailureCalled: Boolean;
begin
  LSuccessCalled := True;
  LFailureCalled := False;

  LResultPair := _ResultTryExcept;

  LResultPair.TryException(
    procedure (Value: Integer)
    begin
      LSuccessCalled := True;
    end,
    procedure (Value: String)
    begin
      LFailureCalled := False;
    end
  );

  try
    Assert.IsTrue(LSuccessCalled);
    Assert.IsFalse(LFailureCalled);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestReduceSuccess;
var
  LResultPair: TResultPair<Integer, String>;
  LSum: Integer;
begin
  LResultPair := LResultPair.Success(FSuccessValue);
  try
    LSum := LResultPair.Reduce<Integer>(
      function(Value: Integer; Error: String): Integer
      begin
        Result := Value + 5;
      end);

    Assert.AreEqual(47, LSum);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrDefaultNoDefault;
var
  LResultPair: TResultPair<String, Integer>;
begin
  LResultPair := LResultPair.Failure(42);
  try
    Assert.AreEqual(LResultPair.FailureOrDefault, 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrDefaultWithDefault;
var
  LResultPair: TResultPair<String, Integer>;
begin
  LResultPair := TResultPair<String, Integer>.New.Failure(42);
  try
    Assert.AreEqual(LResultPair.FailureOrDefault(100), 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrElse;
var
  LResultPair: TResultPair<String, Integer>;
begin
  LResultPair := LResultPair.Failure(42);
  try
    Assert.AreEqual(LResultPair.FailureOrElse(
      function(Value: Integer): Integer
      begin
        Result := Value * 2;
      end
    ), 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrException;
var
  LResultPair: TResultPair<Integer, String>;
begin
  LResultPair := TResultPair<Integer, String>.New.Success(42);
  try
    Assert.WillRaise(
      procedure
      begin
        LResultPair.FailureOrException;
      end
    );
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrDefaultNoDefault;
var
  LResultPair: TResultPair<Integer, String>;
begin
  LResultPair := TResultPair<Integer, String>.New.Success(42);
  try
    Assert.AreEqual(LResultPair.SuccessOrDefault, 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrDefaultWithDefault;
var
  LResultPair: TResultPair<Integer, String>;
begin
  LResultPair := TResultPair<Integer, String>.New.Success(42);
  try
    Assert.AreEqual(LResultPair.SuccessOrDefault(100), 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrElse;
var
  LResultPair: TResultPair<Integer, String>;
begin
  LResultPair := TResultPair<Integer, String>.New.Success(42);
  try
    Assert.AreEqual(LResultPair.SuccessOrElse(
      function(Value: Integer): Integer
      begin
        Result := Value * 2;
      end
    ), 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrException;
var
  LResultPair: TResultPair<Integer, String>;
begin
  LResultPair := TResultPair<Integer, String>.New.Failure('42');
  try
    Assert.WillRaise(
      procedure
      begin
        LResultPair.SuccessOrException;
      end
    );
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestReduceFailure;
var
  LResultPair: TResultPair<Integer, String>;
  LDefaultValue: Integer;
begin
  LResultPair := TResultPair<Integer, String>.New.Failure(FFailureValue);
  try
    LDefaultValue := LResultPair.Reduce<Integer>(
      function(Value: Integer; Error: String): Integer
      begin
        Result := 0;
      end);

    Assert.AreEqual(0, LDefaultValue);
  finally
    LResultPair.Dispose;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTResultPair);
end.
