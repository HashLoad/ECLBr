unit UTestResultPair;

interface

uses
  DUnitX.TestFramework, System.SysUtils, eclbr.result.pair;

type
  TTestTResultPair = class
  private
    FDividend: Integer;
    FDivisor: Integer;
    FSuccessValue: Integer;
    FFailureValue: string;
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
    procedure TestFoldSuccess;
    [Test]
    procedure TestFoldFailure;
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
  LResultPair: TResultPair<Double, string>;
begin
  // Comece com Success() porque o carrinho está vazio no início
  try
    LResultPair := TResultPair<Double, string>.New
      .Success(0.0)
      .ThenOf(
        function (const ASubtotal: Double): TResultPair<Double, string>
        begin
          // Adicione um item ao carrinho
          Result := TResultPair<Double, string>.New.Success(ASubtotal + 29.99);
        end
      )
      .ThenOf(
        function (const ASubtotal: Double): TResultPair<Double, string>
        begin
          // Adicione outro item ao carrinho
          Result := TResultPair<Double, string>.New.Success(ASubtotal + 19.99);
        end
      ).Return;

    // Verifique se o resultado é sucesso e obtenha o preço total
    if LResultPair.IsSuccess then
      LTotalPrice := LResultPair.GetSuccessOrDefault(0.0)
    else
      LTotalPrice := 0.0;

    Assert.AreEqual(49.98, LTotalPrice, 0.001); // Verifica se o preço total está correto
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestCheckoutWithOutOfStockItem;
var
  LResultPair: TResultPair<Boolean, string>;
begin
  try
    // Comece com Success() porque o carrinho não está vazio no início
    LResultPair := TResultPair<Boolean, string>.New
      .Success(True)
      .ThenOf(
        function (const CartNotEmpty: Boolean): TResultPair<Boolean, string>
        begin
          // Verifique se o item do carrinho está em estoque (simulação de falha)
          if not CartNotEmpty then
            Result := TResultPair<Boolean, string>.New.Failure('Carrinho vazio!')
          else
            Result := TResultPair<Boolean, string>.New.Success(True);
        end)
      .ThenOf(
        function (const ItemInStock: Boolean): TResultPair<Boolean, string>
        begin
          // Tente prosseguir com o pagamento se o item estiver em estoque
          if ItemInStock then
            Result := TResultPair<Boolean, string>.New.Success(True)
          else
            Result := TResultPair<Boolean, string>.New.Failure('Item fora de estoque!');
        end
      ).Return;
    // Verifique se o resultado é sucesso ou falha
    if LResultPair.IsSuccess then
      Assert.IsTrue(LResultPair.GetSuccessOrDefault(False))
    else
      Assert.AreEqual('Item fora de estoque!', LResultPair.GetFailureOrDefault('Erro desconhecido'));
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestFailure;
var
  LResultPair: TResultPair<Integer, string>;
begin
  LResultPair := TResultPair<Integer, string>.New.Failure('Error');
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
  LResultPair: TResultPair<Integer, string>;
begin
  try
    LResultPair := TResultPair<Integer, string>.New.Success(FSuccessValue)
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
  LResultPair: TResultPair<Integer, string>;
begin
  try
    LResultPair := TResultPair<Integer, string>.New.Failure(FFailureValue)
               .FlatMapFailure<String>(
                  function(Error: string): string
                  var
                    LResult: TResultPair<Integer, string>;
                  begin
                    try
                      LResult := TResultPair<Integer, string>.New.Failure(Error + 'Handled');
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
  LResultPair: TResultPair<Double, string>;
  LResult: Double;
begin
  try
    LResultPair := TResultPair<Double, string>
               .New
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
  LResultPair: TResultPair<Double, string>;
  LResult: Double;
begin
  try
    LResultPair := TResultPair<Double, string>.New.Success(42)
               .Map<Double>(
                  function(Value: Double): Double
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
  LResultPair: TResultPair<Integer, string>;
begin
  try
    LResultPair := TResultPair<Integer, string>.New.Pure(FSuccessValue);

    Assert.IsTrue(LResultPair.isSuccess);
    Assert.AreEqual(FSuccessValue, LResultPair.ValueSuccess);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestPureFailure;
var
  LResultPair: TResultPair<Integer, string>;
begin
  try
    LResultPair := TResultPair<Integer, string>.New.PureFailure(FFailureValue);

    Assert.IsTrue(LResultPair.isFailure);
    Assert.AreEqual(FFailureValue, LResultPair.ValueFailure);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestRecover;
var
  LResultPair: TResultPair<Integer, string>;
  LRecoveredPair: TResultPair<Double, Integer>;
  LLength: Double;
begin
  try
    LResultPair := TResultPair<Integer, string>.New.Failure(FFailureValue);
    LRecoveredPair := LResultPair.Recover<Double>(
      function(Error: string): Double
      begin
        Result := Length(Error);
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
  LResultPair: TResultPair<Integer, string>;
begin
  LResultPair := TResultPair<Integer, string>.New.Success(42);
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
  LResultPair: TResultPair<Integer, string>;
  LSwappedPair: TResultPair<string, Integer>;
begin
  try
    LResultPair := TResultPair<Integer, string>.New.Failure(FFailureValue);
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
  LResultPair: TResultPair<Integer, string>;
  LSuccessCalled: Boolean;
  LFailureCalled: Boolean;
begin
  LSuccessCalled := False;
  LFailureCalled := False;
  try
    LResultPair := TResultPair<Integer, string>.New
                                               .Success(42)
                                               .TryException(
      procedure (Value: Integer)
      begin
        LSuccessCalled := True;
      end,
      procedure (Value: String)
      begin
        LFailureCalled := True;
      end
    );

    Assert.IsTrue(LSuccessCalled);
    Assert.IsFalse(LFailureCalled);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestFoldSuccess;
var
  LResultPair: TResultPair<Integer, string>;
  LSum: Integer;
begin
  LResultPair := TResultPair<Integer, string>.New.Success(FSuccessValue);
  try
    LSum := LResultPair.Fold<Integer>(
      function(Value: Integer; Error: string): Integer
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
  LResultPair: TResultPair<string, Integer>;
begin
  LResultPair := TResultPair<string, Integer>.New.Failure(42);
  try
    Assert.AreEqual(LResultPair.GetFailureOrDefault, 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrDefaultWithDefault;
var
  LResultPair: TResultPair<string, Integer>;
begin
  LResultPair := TResultPair<string, Integer>.New.Failure(42);
  try
    Assert.AreEqual(LResultPair.GetFailureOrDefault(100), 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrElse;
var
  ResultPair: TResultPair<string, Integer>;
begin
  ResultPair := TResultPair<string, Integer>.New.Failure(42);
  try
    Assert.AreEqual(ResultPair.GetFailureOrElse(
      function(Value: Integer): Integer
      begin
        Result := Value * 2;
      end
    ), 42);
  finally
    ResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrException;
var
  LResultPair: TResultPair<Integer, string>;
begin
  LResultPair := TResultPair<Integer, string>.New.Success(42);
  try
    Assert.WillRaise(
      procedure
      begin
        LResultPair.GetFailureOrException;
      end
    );
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrDefaultNoDefault;
var
  LResultPair: TResultPair<Integer, string>;
begin
  LResultPair := TResultPair<Integer, string>.New.Success(42);
  try
    Assert.AreEqual(LResultPair.GetSuccessOrDefault, 42);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrDefaultWithDefault;
var
  LResultPair: TResultPair<Integer, string>;
begin
  LResultPair := TResultPair<Integer, string>.New.Success(42);
  try
    Assert.AreEqual(LResultPair.GetSuccessOrDefault(100), 42);
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
    Assert.AreEqual(LResultPair.GetSuccessOrElse(
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
  LResultPair: TResultPair<Integer, string>;
begin
  LResultPair := TResultPair<Integer, string>.New.Failure('42');
  try
    Assert.WillRaise(
      procedure
      begin
        LResultPair.GetSuccessOrException;
      end
    );
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestFoldFailure;
var
  LResultPair: TResultPair<Integer, string>;
  LDefaultValue: Integer;
begin
  LResultPair := TResultPair<Integer, string>.New.Failure(FFailureValue);
  try
    LDefaultValue := LResultPair.Fold<Integer>(
      function(Value: Integer; Error: string): Integer
      begin
        Result := 0;
      end);

    Assert.AreEqual(0, LDefaultValue);
  finally
    LResultPair.Dispose;
  end;
end;

{ TMyClass }

class function TMyClass.New: TMyClass;
begin
  Result := TMyClass.Create;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTResultPair);
end.
