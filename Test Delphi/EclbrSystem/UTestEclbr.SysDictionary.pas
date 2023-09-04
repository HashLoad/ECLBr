unit UTestEclbr.SysDictionary;

interface

uses
  DUnitX.TestFramework,
  Generics.Collections,
  eclbr.sysvector,
  eclbr.sysdictionary;

type
  TDictionaryHelperTest = class
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestAddRange;
    [Test]
    procedure TestForEach;
    [Test]
    procedure TestForEachIndexed;
    [Test]
    procedure TestRotate;
    [Test]
    procedure TestUnique;
    [Test]
    procedure TestSortedKeys;
    [Test]
    procedure TestShuffleKeys;
    [Test]
    procedure TestMap;
    [Test]
    procedure TestFilter;
    [Test]
    procedure TestReduce;
//    [Test]
    procedure TestGroupBy;
    [Test]
    procedure TestMapFilterMap;
  end;

implementation

uses
  System.SysUtils;

{ TArrayDataTest }

procedure TDictionaryHelperTest.Setup;
begin

end;

procedure TDictionaryHelperTest.TearDown;
begin

end;


procedure TDictionaryHelperTest.TestAddRange;
var
  LSourceDict, LTargetDict: TDictionaryHelper<Integer, string>;
begin
  // Arrange
  LSourceDict := TDictionaryHelper<Integer, string>.Create;
  LTargetDict := TDictionaryHelper<Integer, string>.Create;

  try
    LSourceDict.Add(1, 'One');
    LSourceDict.Add(2, 'Two');
    LSourceDict.Add(3, 'Three');

    // Act
    LTargetDict.AddRange(LSourceDict);

    // Assert
    Assert.AreEqual('One', LTargetDict[1]);
    Assert.AreEqual('Two', LTargetDict[2]);
    Assert.AreEqual('Three', LTargetDict[3]);
  finally
    LSourceDict.Free;
    LTargetDict.Free;
  end;
end;

procedure TDictionaryHelperTest.TestForEach;
var
  LDictionaryHelper: TDictionaryHelper<Integer, string>;
  LCollectedValues: TList<string>;
begin
  // Arrange
  LDictionaryHelper := TDictionaryHelper<Integer, string>.Create;
  LCollectedValues := TList<string>.Create;
  try
    LDictionaryHelper.Add(1, 'One');
    LDictionaryHelper.Add(2, 'Two');
    LDictionaryHelper.Add(3, 'Three');

    // Act
    LDictionaryHelper.ForEach(
      procedure(Key: Integer; Value: string)
      begin
        LCollectedValues.Add(Value);
      end
    );

    // Assert
    Assert.AreEqual(3, LCollectedValues.Count);
    Assert.IsTrue(LCollectedValues.Contains('One'));
    Assert.IsTrue(LCollectedValues.Contains('Two'));
    Assert.IsTrue(LCollectedValues.Contains('Three'));
  finally
    LDictionaryHelper.Free;
    LCollectedValues.Free;
  end;
end;

procedure TDictionaryHelperTest.TestForEachIndexed;
var
  LDictionaryHelper: TDictionaryHelper<Integer, string>;
  LIndexList: TList<Integer>;
  LKeyList: TList<Integer>;
  LValueList: TList<string>;
begin
  // Arrange
  LDictionaryHelper := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionaryHelper.Add(1, 'One');
    LDictionaryHelper.Add(2, 'Two');
    LDictionaryHelper.Add(3, 'Three');

    LIndexList := TList<Integer>.Create;
    LKeyList := TList<Integer>.Create;
    LValueList := TList<string>.Create;

    // Act
    LDictionaryHelper.ForEachIndexed(
      procedure(Index: Integer; Key: Integer; Value: string)
      begin
        LIndexList.Add(Index);
        LKeyList.Add(Key);
        LValueList.Add(Value);
      end
    );

    // Assert
    Assert.AreEqual(3, LIndexList.Count); // Ensure three items were iterated
    Assert.AreEqual(0, LIndexList[0]);
    Assert.AreEqual(1, LIndexList[1]);
    Assert.AreEqual(2, LIndexList[2]);

    Assert.AreEqual(3, LKeyList.Count);
    Assert.AreEqual(1, LKeyList[0]);
    Assert.AreEqual(2, LKeyList[1]);
    Assert.AreEqual(3, LKeyList[2]);

    Assert.AreEqual(3, LValueList.Count);
    Assert.AreEqual('One', LValueList[0]);
    Assert.AreEqual('Two', LValueList[1]);
    Assert.AreEqual('Three', LValueList[2]);
  finally
    LDictionaryHelper.Free;
    LIndexList.Free;
    LKeyList.Free;
    LValueList.Free;
  end;
end;

procedure TDictionaryHelperTest.TestRotate;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LRotatedPairs: TArray<TPair<Integer, string>>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LRotatedPairs := LDictionary.Rotate(1);

    // Assert
    Assert.AreEqual(3, Length(LRotatedPairs));

    // Verifique se os pares chave-valor estão na ordem correta após a rotação
    Assert.AreEqual(3, LRotatedPairs[0].Key);
    Assert.AreEqual('Three', LRotatedPairs[0].Value);

    Assert.AreEqual(1, LRotatedPairs[1].Key);
    Assert.AreEqual('One', LRotatedPairs[1].Value);

    Assert.AreEqual(2, LRotatedPairs[2].Key);
    Assert.AreEqual('Two', LRotatedPairs[2].Value);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestUnique;
var
  LDictionary: TDictionaryHelper<Integer, string>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'One');
    LDictionary.Add(4, 'Two');
    LDictionary.Add(5, 'Three');

    // Act
    LDictionary.Unique;

    // Assert
    Assert.AreEqual(3, LDictionary.Count);
    Assert.AreEqual('One', LDictionary[1]);
    Assert.AreEqual('Two', LDictionary[2]);
    Assert.AreEqual('Three', LDictionary[5]);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestSortedKeys;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LSortedKeys: TArray<Integer>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(3, 'Three');
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');

    // Act
    LSortedKeys := LDictionary.SortedKeys;

    // Assert
    Assert.AreEqual(3, Length(LSortedKeys));
    Assert.AreEqual(1, LSortedKeys[0]);
    Assert.AreEqual(2, LSortedKeys[1]);
    Assert.AreEqual(3, LSortedKeys[2]);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestShuffleKeys;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LShuffledKeys: TArray<Integer>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LShuffledKeys := LDictionary.ShuffleKeys;

    // Assert
    Assert.AreEqual(3, Length(LShuffledKeys));

    // Verifique se todas as chaves originais estão presentes na matriz retornada
    // não tem teste para vê como ficou embaralhada
    Assert.Contains<Integer>(LShuffledKeys, 1);
    Assert.Contains<Integer>(LShuffledKeys, 2);
    Assert.Contains<Integer>(LShuffledKeys, 3);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestMap;
var
  LDictionary: TDictionaryHelper<Integer, string>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LDictionary.Map(
      function(Key: Integer; Value: string): string
      begin
        Result := IntToStr(Length(Value));
      end
    );

    // Assert
    Assert.AreEqual(3, LDictionary.Count);
    Assert.AreEqual('3', LDictionary[1]); // 'One' tem comprimento 3
    Assert.AreEqual('3', LDictionary[2]); // 'Two' tem comprimento 3
    Assert.AreEqual('5', LDictionary[3]); // 'Three' tem comprimento 5
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestFilter;
var
  LDictionary: TDictionaryHelper<Integer, string>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');

    // Act
    LDictionary.Filter(
      function(Key: Integer; Value: string): Boolean
      begin
        Result := Length(Value) = 3;
      end
    );

    // Assert
    Assert.AreEqual(2, LDictionary.Count);
    Assert.IsTrue(LDictionary.ContainsKey(1));
    Assert.IsTrue(LDictionary.ContainsKey(2));
    Assert.IsFalse(LDictionary.ContainsKey(3));
    Assert.IsFalse(LDictionary.ContainsKey(4));
  finally
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestReduce;
var
  LDictionary: TDictionaryHelper<string, Integer>;
  LResultValue: Integer;
begin
  // Arrange
  LDictionary := TDictionaryHelper<string, Integer>.Create;
  try
    LDictionary.Add('One', 1);
    LDictionary.Add('Two', 2);
    LDictionary.Add('Three', 3);
    LDictionary.Add('Four', 4);

    // Act
    LResultValue := LDictionary.Reduce(
      function(accumulator, currentValue: Integer): Integer
      begin
        // Soma os valores acumulados com os valores atuais
        Result := accumulator + currentValue;
      end
    );

    // Assert
    Assert.AreEqual(10, LResultValue); // 1 + 2 + 3 + 4 = 10
  finally
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestGroupBy;
var
  LDictionary: TDictionaryHelper<string, Integer>;
  LGroupedDictionary: TDictionary<string, TList<Integer>>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<string, Integer>.Create;
  try
    LDictionary.Add('One', 1);
    LDictionary.Add('Two', 2);
    LDictionary.Add('Three', 3);
    LDictionary.Add('Four', 4);
    LDictionary.Add('Five', 5);

    // Act
    LGroupedDictionary := LDictionary.GroupBy<String>(
      function(Value: Integer): string
      begin
        // Agrupa os valores por sua paridade
        if Value mod 2 = 0 then
          Result := 'Even'
        else
          Result := 'Odd';
      end
    );

    // Assert
    Assert.AreEqual(2, LGroupedDictionary.Count); // Deve haver 2 grupos: Par e Ímpar

    // Verifique se os valores foram agrupados corretamente
    Assert.AreEqual(2, LGroupedDictionary['Even'].Count);
    Assert.AreEqual(3, LGroupedDictionary['Odd'].Count);
  finally
    // Clean up
    LDictionary.Free;
    LGroupedDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestMapFilterMap;
var
  LMap: TDictionaryHelper<integer, string>;
  LIlteredMap: TDictionaryHelper<integer, integer>;
begin
  // Arrange
  LMap := TDictionaryHelper<Integer, string>.Create;
  try
    LMap.Add(3, 'Pling');
    LMap.Add(5, 'Plang');
    LMap.Add(7, 'Plong');

    LIlteredMap := LMap.Filter(
                           function(Key: Integer; Value: String): boolean
                           begin
                             Result := 28 mod Key = 0;
                           end)
                       .Map(function(Key: Integer; Value: string): String
                            begin
                              Result := IntToStr(Key);
                            end)
                       .Collect<Integer>(function(Value: String): integer
                            begin
                              Result := StrToInt(Value);
                            end);

    Assert.IsTrue(LIlteredMap.Count = 1);
    Assert.AreEqual(LIlteredMap.ToString, '7: 7');
  finally
    LMap.Free;
    LIlteredMap.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDictionaryHelperTest);

end.

