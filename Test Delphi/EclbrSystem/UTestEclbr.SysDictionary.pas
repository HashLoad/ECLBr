unit UTestEclbr.SysDictionary;

interface

uses
  DUnitX.TestFramework,
  Generics.Collections,
  eclbr.sysvector,
  eclbr.sysmap,
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
    [Test]
    procedure TestGroupBy;
    [Test]
    procedure TestMapFilterMap;
    [Test]
    procedure TestJoin;
    [Test]
    procedure TestPartition;
    [Test]
    procedure TestTake;
    [Test]
    procedure TestSkip;
    [Test]
    procedure TestSlice;
    [Test]
    procedure TestZip;
//    [Test]
    procedure TestFlatMap;
  end;

implementation

uses
  SysUtils;

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
  LGroupedDictionary: TDictionary<string, TVector<Integer>>;
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
    Assert.AreEqual(2, LGroupedDictionary['Even'].Length);
    Assert.AreEqual(3, LGroupedDictionary['Odd'].Length);
  finally
    // Clean up
    LDictionary.Free;
    LGroupedDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestJoin;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LSeparator: string;
  LResultStr: string;
begin
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    // Arrange
    LSeparator := ', ';
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LResultStr := LDictionary.Join(LSeparator);

    // Assert
    Assert.AreEqual('1: One, 2: Two, 3: Three', LResultStr);
  finally
    LDictionary.Free;
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
                       { TODO -oDictionary -cFEATURE :
                       "I tried to convert everything internally, but I had a problem. I will reanalyze it in the future and implement an overload of the Collect<T> method without passing the anonymous method." }
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

procedure TDictionaryHelperTest.TestPartition;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LPartitions: TPair<TMap<Integer, string>, TMap<Integer, string>>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');

    // Act
    LPartitions := LDictionary.Partition(
      function(Value: string): Boolean
      begin
        // Predicate: Keep strings with even length
        Result := Length(Value) mod 2 = 0;
      end
    );

    // Assert
    Assert.AreEqual(2, LPartitions.Key.Length); // Two items with even-length values
    Assert.AreEqual(3, LPartitions.Value.Length); // Three items with odd-length values

    Assert.IsTrue(LPartitions.Key.Contains(4));
    Assert.IsTrue(LPartitions.Key.Contains(5));

    Assert.IsTrue(LPartitions.Value.Contains(1));
    Assert.IsTrue(LPartitions.Value.Contains(3));

    Assert.AreEqual('Four', LPartitions.Key[4]);
    Assert.AreEqual('Five', LPartitions.Key[5]);

    Assert.AreEqual('One', LPartitions.Value[1]);
    Assert.AreEqual('Two', LPartitions.Value[2]);
    Assert.AreEqual('Three', LPartitions.Value[3]);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestTake;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LTakenDictionary: TDictionaryHelper<Integer, string>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');

    // Act
    LTakenDictionary := LDictionary.Take(3);

    // Assert
    Assert.AreEqual(3, LTakenDictionary.Count);

    Assert.IsTrue(LTakenDictionary.ContainsKey(1));
    Assert.IsTrue(LTakenDictionary.ContainsKey(2));
    Assert.IsTrue(LTakenDictionary.ContainsKey(3));

    Assert.IsFalse(LTakenDictionary.ContainsKey(4));
    Assert.IsFalse(LTakenDictionary.ContainsKey(5));

    Assert.AreEqual('One', LTakenDictionary[1]);
    Assert.AreEqual('Two', LTakenDictionary[2]);
    Assert.AreEqual('Three', LTakenDictionary[3]);
  finally
    // Clean up
    LDictionary.Free;
    LTakenDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestSkip;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LSkippedDictionary: TDictionaryHelper<Integer, string>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');

    // Act
    LSkippedDictionary := LDictionary.Skip(2);

    // Assert
    Assert.AreEqual(3, LSkippedDictionary.Count);

    Assert.IsTrue(LSkippedDictionary.ContainsKey(3));
    Assert.IsTrue(LSkippedDictionary.ContainsKey(4));
    Assert.IsTrue(LSkippedDictionary.ContainsKey(5));

    Assert.IsFalse(LSkippedDictionary.ContainsKey(1));
    Assert.IsFalse(LSkippedDictionary.ContainsKey(2));

    Assert.AreEqual('Three', LSkippedDictionary[3]);
    Assert.AreEqual('Four', LSkippedDictionary[4]);
    Assert.AreEqual('Five', LSkippedDictionary[5]);
  finally
    // Clean up
    LDictionary.Free;
    LSkippedDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestSlice;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LSlicedDictionary: TDictionaryHelper<Integer, string>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');

    // Act
    LSlicedDictionary := LDictionary.Slice(1, 3); // Slice from index 1 to 3

    // Assert
    Assert.AreEqual(3, LSlicedDictionary.Count);

    Assert.IsTrue(LSlicedDictionary.ContainsKey(2));
    Assert.IsTrue(LSlicedDictionary.ContainsKey(3));
    Assert.IsTrue(LSlicedDictionary.ContainsKey(4));

    Assert.IsFalse(LSlicedDictionary.ContainsKey(1));
    Assert.IsFalse(LSlicedDictionary.ContainsKey(5));

    Assert.AreEqual('Two', LSlicedDictionary[2]);
    Assert.AreEqual('Three', LSlicedDictionary[3]);
    Assert.AreEqual('Four', LSlicedDictionary[4]);
  finally
    // Clean up
    LDictionary.Free;
    LSlicedDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestZip;
var
  LDictionary1, LDictionary2: TDictionaryHelper<Integer, string>;
  LZippedDictionary: TDictionaryHelper<Integer, string>;
begin
  // Arrange
  LDictionary1 := TDictionaryHelper<Integer, string>.Create;
  LDictionary2 := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary1.Add(1, 'One');
    LDictionary1.Add(2, 'Two');
    LDictionary1.Add(3, 'Three');

    LDictionary2.Add(1, 'Uno');
    LDictionary2.Add(2, 'Dos');
    LDictionary2.Add(3, 'Tres');

    // Act
    LZippedDictionary := LDictionary1.Zip<string, string>(LDictionary2,
      function(Value1: string; Value2: string): string
      begin
        Result := Value1 + ' | ' + Value2;
      end
    );

    // Assert
    Assert.AreEqual(3, LZippedDictionary.Count);

    Assert.IsTrue(LZippedDictionary.ContainsKey(1));
    Assert.IsTrue(LZippedDictionary.ContainsKey(2));
    Assert.IsTrue(LZippedDictionary.ContainsKey(3));

    Assert.AreEqual('One | Uno', LZippedDictionary[1]);
    Assert.AreEqual('Two | Dos', LZippedDictionary[2]);
    Assert.AreEqual('Three | Tres', LZippedDictionary[3]);
  finally
    // Clean up
    LDictionary1.Free;
    LDictionary2.Free;
    LZippedDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestFlatMap;
var
  LDictionary: TDictionaryHelper<Integer, string>;
  LFlatMappedDictionary: TDictionaryHelper<Integer, Integer>;
  LFlatMappedValues: TList<Integer>;
begin
  // Arrange
  LDictionary := TDictionaryHelper<Integer, string>.Create;
  try
    LDictionary.Add(1, '1,2,3');
    LDictionary.Add(2, '4,5');
    LDictionary.Add(3, '6');

    // Act
    LFlatMappedDictionary := LDictionary.FlatMap<Integer>(
      function(Value: System.Rtti.TValue): TArray<Integer>
      var
        LValues: TArray<string>;
        LItem: string;
      begin
        LValues := Value.ToString.Split([',']);
        LFlatMappedValues := TList<Integer>.Create;
        try
          for LItem in LValues do
          begin
            LFlatMappedValues.Add(StrToInt(LItem));
          end;
          Result := LFlatMappedValues.ToArray;
        finally
        end;
      end
    );

    // Assert
    Assert.AreEqual(6, LFlatMappedDictionary.Count);

    Assert.IsTrue(LFlatMappedDictionary.ContainsKey(1));
    Assert.IsTrue(LFlatMappedDictionary.ContainsKey(2));
    Assert.IsTrue(LFlatMappedDictionary.ContainsKey(3));

    Assert.AreEqual(1, LFlatMappedDictionary[1]);
    Assert.AreEqual(2, LFlatMappedDictionary[2]);
    Assert.AreEqual(3, LFlatMappedDictionary[3]);
    Assert.AreEqual(4, LFlatMappedDictionary[4]);
    Assert.AreEqual(5, LFlatMappedDictionary[5]);
    Assert.AreEqual(6, LFlatMappedDictionary[6]);
  finally
    // Clean up
    LDictionary.Free;
    LFlatMappedDictionary.Free;
    LFlatMappedValues.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDictionaryHelperTest);

end.

