unit UTestEclbr.List;

interface

uses
  DUnitX.TestFramework,
  Generics.Collections,
  eclbr.include,
  eclbr.vector;

type
  TProduct = class
  private
    FName: String;
    FPrice: Double;
    FDescount: Double;
  public
    constructor Create(AName: String; APrice: Double; ADescount: Double);
    function Price: Double;
    function Descount: Double;
  end;

  TListTest = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestListMap;
    [Test]
    procedure TestListMapMap;
    [Test]
    procedure TestListFilter;
    [Test]
    procedure TestListFilterMap;
    [Test]
    procedure TestListReducer;
    [Test]
    procedure TestListReducerAvg;
  end;

implementation

uses
  eclbr.list;

{ TArrayDataTest }

procedure TListTest.Setup;
begin

end;

procedure TListTest.TearDown;
begin

end;

procedure TListTest.TestListFilter;
var
  LList: TListEx<Double>;
  LResult: TVector<Double>;
  LValorEsperado: Double;
begin
  LList := TListEx<Double>.Create([7.1, 8.3, 6.3, 7.7, 9.1, 4.3]);
  try
    LResult := LList.Filter(function(Value: Double): Boolean
                            begin
                              Result := Value >= 7.0;
                            end);
    Assert.AreEqual(4, LResult.Count);

    LValorEsperado := 9.1;
    Assert.AreEqual(LValorEsperado, LResult.Items[3]);
  finally
    LList.Free;
  end;
end;

procedure TListTest.TestListFilterMap;
var
  LList: TListEx<TProduct>;
  LItem: TProduct;
  LResult: TVector<Integer>;
  LValorEsperado: TArray<Integer>;
begin
  LList := TListEx<TProduct>.Create([TProduct.Create('...', 23.67, 0.2),
                                     TProduct.Create('...', 120.99, 0.3),
                                     TProduct.Create('...', 3567.67, 0.5),
                                     TProduct.Create('...', 10.80, 0.1),
                                     TProduct.Create('...', 7.43, 0.05),
                                     TProduct.Create('...', 12355.33, 0.15)]);
  try
    LResult := LList.Filter(function(AProduct: TProduct): boolean
                            begin
                              Result := AProduct.Price >= 1000;
                            end)
                    .Map<Double>(function (AProduct: TProduct): Double
                                 begin
                                   Result := AProduct.Price * (1 - AProduct.Descount);
                                 end)
                    .Map<Integer>(function (AValue: Double): Integer
                                 begin
                                   Result := Round(AValue * 0.3);
                                 end);

    LValorEsperado := [535, 3151];
    Assert.AreEqual(LValorEsperado[0], LResult[0]);
    Assert.AreEqual(LValorEsperado[1], LResult[1]);
  finally
    for LItem in LList do
      LItem.Free;
    LList.Free;
  end;
end;

procedure TListTest.TestListMap;
var
  LList: TListEx<TProduct>;
  LItem: TProduct;
  LResult: TVector<Double>;
  LValorEsperado: Double;
begin
  LList := TListEx<TProduct>.Create([TProduct.Create('...', 23.67, 0.2),
                                     TProduct.Create('...', 120.99, 0.3),
                                     TProduct.Create('...', 3567.67, 0.5),
                                     TProduct.Create('...', 10.80, 0.1),
                                     TProduct.Create('...', 7.43, 0.05),
                                     TProduct.Create('...', 12355.33, 0.15)]);
  try
    LResult := LList.Map<Double>(function (AProduct: TProduct): Double
                                 begin
                                   Result := AProduct.Price * (1 - AProduct.Descount);
                                 end);
    Assert.AreEqual(6, LResult.Count);

    LValorEsperado := 9.72;
    Assert.AreEqual(LValorEsperado, LResult.Items[3]);
  finally
    for LItem in LList do
      LItem.Free;
    LList.Free;
  end;
end;

procedure TListTest.TestListMapMap;
var
  LList: TListEx<TProduct>;
  LItem: TProduct;
  LResult: TVector<Integer>;
  LValorEsperado: TArray<Integer>;
begin
  LList := TListEx<TProduct>.Create([TProduct.Create('...', 23.67, 0.2),
                                     TProduct.Create('...', 120.99, 0.3),
                                     TProduct.Create('...', 3567.67, 0.5),
                                     TProduct.Create('...', 10.80, 0.1),
                                     TProduct.Create('...', 7.43, 0.05),
                                     TProduct.Create('...', 12355.33, 0.15)]);
  try
    LResult := LList.Map<Double>(function (AProduct: TProduct): Double
                                 begin
                                   Result := AProduct.Price * (1 - AProduct.Descount);
                                 end)
                    .Map<Integer>(function (AValue: Double): Integer
                                 begin
                                   Result := Round(AValue * 0.3);
                                 end);

    LValorEsperado := [6, 25, 535, 3, 2, 3151];

    Assert.AreEqual(LValorEsperado[0], LResult[0]);
    Assert.AreEqual(LValorEsperado[1], LResult[1]);
    Assert.AreEqual(LValorEsperado[2], LResult[2]);
    Assert.AreEqual(LValorEsperado[3], LResult[3]);
    Assert.AreEqual(LValorEsperado[4], LResult[4]);
    Assert.AreEqual(LValorEsperado[5], LResult[5]);
  finally
    for LItem in LList do
      LItem.Free;
    LList.Free;
  end;
end;

procedure TListTest.TestListReducer;
var
  LList: TListEx<integer>;
  LResult: integer;
  LValorEsperado: integer;
begin
  LList := TListEx<integer>.Create([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  try
    LResult := LList.Reduce(function(Arg1, Arg2: integer): integer
                            begin
                              Result := Arg1 + Arg2;
                            end);
    LValorEsperado := 55;
    Assert.AreEqual(LValorEsperado, LResult);
  finally
    LList.Free;
  end;
end;

procedure TListTest.TestListReducerAvg;
var
  LList: TListEx<integer>;
  LValorEsperado: Double;
  LResult: Tuple;
begin
  LList := TListEx<integer>.Create([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  try
    LResult := LList.Reduce(function(Arg1: integer; Arg2: Tuple): Tuple
                            var
                              LSum, LCount: integer;
                              LAvg: Double;
                            begin
                              LSum   := Arg2[0].AsType<integer> + Arg1;
                              LCount := Arg2[1].AsType<integer> + 1;
                              LAvg   := LSum / LCount;

                              Result := [LSum, LCount, LAvg];
                            end,
                            [0, 0, 0]);
    // Sum
    Assert.AreEqual(55, LResult[0].AsType<integer>);
    // Count
    Assert.AreEqual(10, LResult[1].AsType<integer>);
    // Avg
    LValorEsperado := 5.5;
    Assert.AreEqual(LValorEsperado, LResult[2].AsType<Double>);
  finally
    LList.Free;
  end;
end;

{ TProduct }

constructor TProduct.Create(AName: String; APrice, ADescount: Double);
begin
  FName := AName;
  FPrice := APrice;
  FDescount := ADescount;
end;

function TProduct.Descount: Double;
begin
  Result := FDescount;
end;

function TProduct.Price: Double;
begin
  Result := FPrice;
end;

initialization
  TDUnitX.RegisterTestFixture(TListTest);

end.

