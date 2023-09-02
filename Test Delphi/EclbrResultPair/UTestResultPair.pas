unit UTestResultPair;

interface

uses
  DUnitX.TestFramework, System.SysUtils, eclbr.result.pair, eclbr.result.pair.container;

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

procedure TTestTResultPair.TestFailure;
var
  LResultPair: TResultPair<Integer, string>;
begin
  LResultPair.Failure('Error');
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
//  LResultPair: ICrp<integer, string>;
begin
//  LResultPair := TCrp<integer, string>.New;
  try
    LResultPair.Success(FSuccessValue)
//    LResultPair.Grp.Success(FSuccessValue)
      .FlatMap<Integer>(
        function(Value: Integer): Integer
        var
          LResult: TResultPair<Integer, String>;
        begin
          try
            Result := LResult.Success(Value * 2).ValueSuccess;
          finally
            LResult.Dispose;
          end;
        end);

    Assert.IsTrue(LResultPair.isSuccess);
    Assert.AreEqual(FSuccessValue * 2, LResultPair.ValueSuccess);
//    Assert.IsTrue(LResultPair.Grp.isSuccess);
//    Assert.AreEqual(FSuccessValue * 2, LResultPair.Grp.ValueSuccess);
  finally
    LResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestFlatMapFailure;
var
  LResultPair: TResultPair<Integer, string>;
begin
  try
    LResultPair.Failure(FFailureValue)
               .FlatMapFailure<String>(
                  function(Error: string): string
                  var
                    LResult: TResultPair<Integer, string>;
                  begin
                    try
                      Result := LResult.Failure(Error + 'Handled').ValueFailure;
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
    LResultPair.Success(FDividend div FDivisor)
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

procedure TTestTResultPair.testMapTryException;
var
  LResultPair: TResultPair<Double, string>;
  LResult: Double;
begin
  try
    LResultPair.Success(42)
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
    LResultPair.Pure(FSuccessValue);

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
    LResultPair.PureFailure(FFailureValue);

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
    LResultPair.Failure(FFailureValue);
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
  LResultPair.Success(42);
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
    LResultPair.Failure(FFailureValue);
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
    LResultPair.Success(42).TryException(
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
  LResultPair.Success(FSuccessValue);
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
  ResultPair: TResultPair<string, Integer>;
begin
  ResultPair.Failure(42);
  try
    Assert.AreEqual(ResultPair.GetFailureOrDefault, 42);
  finally
    ResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrDefaultWithDefault;
var
  ResultPair: TResultPair<string, Integer>;
begin
  ResultPair.Failure(42);
  try
    Assert.AreEqual(ResultPair.GetFailureOrDefault(100), 42);
  finally
    ResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetFailureOrElse;
var
  ResultPair: TResultPair<string, Integer>;
begin
  ResultPair.Failure(42);
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
  ResultPair: TResultPair<Integer, string>;
begin
  ResultPair.Success(42);
  try
    Assert.WillRaise(
      procedure
      begin
        ResultPair.GetFailureOrException;
      end
    );
  finally
    ResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrDefaultNoDefault;
var
  ResultPair: TResultPair<Integer, string>;
begin
  ResultPair.Success(42);
  try
    Assert.AreEqual(ResultPair.GetSuccessOrDefault, 42);
  finally
    ResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrDefaultWithDefault;
var
  ResultPair: TResultPair<Integer, string>;
begin
  ResultPair.Success(42);
  try
    Assert.AreEqual(ResultPair.GetSuccessOrDefault(100), 42);
  finally
    ResultPair.Dispose;
  end;
end;

procedure TTestTResultPair.TestGetSuccessOrElse;
var
  LResultPair: TResultPair<Integer, String>;
begin
  LResultPair.Success(42);
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
//  LResultPair: ICrp<Integer, String>;
begin
//  LResultPair := TCrp<Integer, String>.New;
//  LResultPair.Grp.Failure('42');
  LResultPair.Failure('42');
  try
    Assert.WillRaise(
      procedure
      begin
//        LResultPair.Grp.GetSuccessOrException;
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
  LResultPair.Failure(FFailureValue);
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

initialization
  TDUnitX.RegisterTestFixture(TTestTResultPair);
end.
