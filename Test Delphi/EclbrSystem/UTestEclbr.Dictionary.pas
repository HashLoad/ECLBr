unit UTestEclbr.Dictionary;

interface

uses
  DUnitX.TestFramework,
  Rtti,
  SysUtils,
  Generics.Collections,
  eclbr.vector,
  eclbr.map,
  eclbr.dictionary;

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
    [Test]
    procedure TestFlatMap;
    [Test]
    procedure TestSkipMemoryLeak;
    [Test]
    procedure TestIntersect;
    [Test]
    procedure TestExcept;
    [Test]
    procedure TestMaxKey;
    [Test]
    procedure TestMinKey;
    [Test]
    procedure TestMaxValue;
    [Test]
    procedure TestMinValue;
    [Test]
    procedure TestDistinctBy;
    [Test]
    procedure TestFindAll;
    [Test]
    procedure TestTakeWhile;
    [Test]
    procedure TestSkipWhile;
    [Test]
    procedure TestPartitionBy;
  end;

implementation

{ TArrayDataTest }

procedure TDictionaryHelperTest.Setup;
begin

end;

procedure TDictionaryHelperTest.TearDown;
begin

end;


procedure TDictionaryHelperTest.TestAddRange;
var
  LSourceDict, LTargetDict: TDictEx<Integer, string>;
begin
  // Arrange
  LSourceDict := TDictEx<Integer, string>.Create;
  LTargetDict := TDictEx<Integer, string>.Create;

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

procedure TDictionaryHelperTest.TestDistinctBy;
var
  LDictionary: TDictEx<Integer, string>;
  LDistinctDict: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');
    LDictionary.Add(6, 'Six');

    // Act
    LDistinctDict := LDictionary.DistinctBy<Integer>(
      function(Key: Integer): Integer
      begin
        // Use uma chave seletora que retorna o resto da divisão por 2 (par ou ímpar)
        Result := Key mod 2;   // 0 ou 1
      end
    );

    // Assert
    Assert.AreEqual(2, LDistinctDict.Count);
    Assert.IsTrue(LDistinctDict.Contains(0)); // Chaves pares (0, 2, 4, 6)
    Assert.IsTrue(LDistinctDict.Contains(1)); // Chaves ímpares (1, 3, 5)
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestExcept;
var
  LDictionary1, LDictionary2: TDictEx<Integer, string>;
  LExceptedDict: TMap<Integer, string>;
begin
  // Arrange
  LDictionary1 := TDictEx<Integer, string>.Create;
  LDictionary2 := TDictEx<Integer, string>.Create;
  LExceptedDict := [];

  try
    LDictionary1.Add(1, 'One');
    LDictionary1.Add(2, 'Two');
    LDictionary1.Add(3, 'Three');

    LDictionary2.Add(2, 'Two');
    LDictionary2.Add(3, 'Three');
    LDictionary2.Add(4, 'Four');

    // Act
    LExceptedDict := LDictionary1.&Except(LDictionary2);

    // Assert
    Assert.AreEqual(1, LExceptedDict.Count);

    Assert.IsTrue(LExceptedDict.Contains(1));

    Assert.IsFalse(LExceptedDict.Contains(2));
    Assert.IsFalse(LExceptedDict.Contains(3));
    Assert.IsFalse(LExceptedDict.Contains(4));
  finally
    // Clean up
    LDictionary1.Free;
    LDictionary2.Free;
  end;
end;

procedure TDictionaryHelperTest.TestForEach;
var
  LDictionaryHelper: TDictEx<Integer, string>;
  LCollectedValues: TList<string>;
begin
  // Arrange
  LDictionaryHelper := TDictEx<Integer, string>.Create;
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
  LDictionaryHelper: TDictEx<Integer, string>;
  LIndexList: TList<Integer>;
  LKeyList: TList<Integer>;
  LValueList: TList<string>;
begin
  // Arrange
  LDictionaryHelper := TDictEx<Integer, string>.Create;
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
  LDictionary: TDictEx<Integer, string>;
  LRotatedPairs: TArray<TPair<Integer, string>>;
  LResult: Integer;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LRotatedPairs := LDictionary.Rotate(1);

    LResult := Length(LRotatedPairs);
    // Assert
    Assert.AreEqual(3, LResult);

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
  LDictionary: TDictEx<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
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
  LDictionary: TDictEx<Integer, string>;
  LSortedKeys: TArray<Integer>;
  LResult: Integer;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
  try
    LDictionary.Add(3, 'Three');
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');

    // Act
    LSortedKeys := LDictionary.SortedKeys;

    LResult := Length(LSortedKeys);
    // Assert
    Assert.AreEqual(3, LResult);
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
  LDictionary: TDictEx<Integer, string>;
  LShuffledKeys: TArray<Integer>;
  LResult: Integer;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LShuffledKeys := LDictionary.ShuffleKeys;

    LResult := Length(LShuffledKeys);
    // Assert
    Assert.AreEqual(3, LResult);

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
  LDictionary: TDictEx<Integer, string>;
  LResultList: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LResultList := LDictionary.Map(
      function(Key: Integer; Value: string): string
      begin
        Result := IntToStr(Length(Value));
      end
    );

    // Assert
    Assert.AreEqual(3, LResultList.Count);
    Assert.AreEqual('3', LResultList[1]); // 'One' tem comprimento 3
    Assert.AreEqual('3', LResultList[2]); // 'Two' tem comprimento 3
    Assert.AreEqual('5', LResultList[3]); // 'Three' tem comprimento 5
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestFilter;
var
  LDictionary: TDictEx<Integer, string>;
  LResultList: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');

    // Act
    LResultList := LDictionary.Filter(
      function(Key: Integer; Value: string): Boolean
      begin
        Result := Length(Value) = 3;
      end
    );

    // Assert
    Assert.AreEqual(2, LResultList.Count);
    Assert.IsTrue(LResultList.Contains(1));
    Assert.IsTrue(LResultList.Contains(2));
    Assert.IsFalse(LResultList.Contains(3));
    Assert.IsFalse(LResultList.Contains(4));
  finally
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestFindAll;
var
  LDictionary: TDictEx<Integer, string>;
  LFilteredDict: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');
    LDictionary.Add(6, 'Six');

    // Act
    LFilteredDict := LDictionary.FindAll(
      function(Value: string): Boolean
      begin
        // Filtra elementos cujo valor contenha a letra 'e'
        Result := Pos('e', Value) > 0;
      end
    );

    // Assert
    Assert.AreEqual(3, LFilteredDict.Count);

    Assert.IsFalse(LFilteredDict.Contains(2)); // 'Two' não contém 'e'
    Assert.IsFalse(LFilteredDict.Contains(4)); // 'Four' não contém 'e'
    Assert.IsFalse(LFilteredDict.Contains(6)); // 'Six' não contém 'e'

    Assert.IsTrue(LFilteredDict.Contains(1)); // 'One' contém 'e'
    Assert.IsTrue(LFilteredDict.Contains(3)); // 'Three' contém 'e'
    Assert.IsTrue(LFilteredDict.Contains(5)); // 'Five' contém 'e'
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestReduce;
var
  LDictionary: TDictEx<string, Integer>;
  LResultValue: Integer;
begin
  // Arrange
  LDictionary := TDictEx<string, Integer>.Create;
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
  LDictionary: TDictEx<string, Integer>;
  LGroupedDictionary: TMap<string, TVector<Integer>>;
begin
  // Arrange
  LDictionary := TDictEx<string, Integer>.Create;
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
  end;
end;

procedure TDictionaryHelperTest.TestIntersect;
var
  LDictionary1, LDictionary2: TDictEx<Integer, string>;
  LIntersectedDict: TMap<Integer, string>;
begin
  // Arrange
  LDictionary1 := TDictEx<Integer, string>.Create;
  LDictionary2 := TDictEx<Integer, string>.Create;
  LIntersectedDict := [];

  try
    LDictionary1.Add(1, 'One');
    LDictionary1.Add(2, 'Two');
    LDictionary1.Add(3, 'Three');

    LDictionary2.Add(2, 'Two');
    LDictionary2.Add(3, 'Three');
    LDictionary2.Add(4, 'Four');

    // Act
    LIntersectedDict := LDictionary1.Intersect(LDictionary2);

    // Assert
    Assert.AreEqual(2, LIntersectedDict.Count);

    Assert.IsTrue(LIntersectedDict.Contains(2));
    Assert.IsTrue(LIntersectedDict.Contains(3));

    Assert.IsFalse(LIntersectedDict.Contains(1));
    Assert.IsFalse(LIntersectedDict.Contains(4));
  finally
    // Clean up
    LDictionary1.Free;
    LDictionary2.Free;
  end;end;

procedure TDictionaryHelperTest.TestJoin;
var
  LDictionary: TDictEx<Integer, string>;
  LResultStr: string;
begin
  LDictionary := TDictEx<Integer, string>.Create;
  try
    // Arrange
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LResultStr := LDictionary.Join(', ');

    // Assert
    Assert.AreEqual('1: One, 2: Two, 3: Three', LResultStr);
  finally
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestMapFilterMap;
var
  LMap: TDictEx<integer, string>;
  LIlteredMap: TMap<integer, integer>;
begin
  // Arrange
  LMap := TDictEx<Integer, string>.Create;
  try
    LMap.Add(3, '105');
    LMap.Add(5, '120');
    LMap.Add(7, '250');

    LIlteredMap := LMap.Filter(
                           function(Key: Integer; Value: string): Boolean
                           begin
                             Result := 28 mod Key = 0;
                           end)
                       .Map<Integer>(function(Key: Integer; Value: string): Integer
                           begin
                             Result := StrToInt(Value);
                           end);

    Assert.IsTrue(LIlteredMap.Count = 1); // <<<<===== FILTRO
    Assert.AreEqual(LIlteredMap[7], 250); // <<<<==== CONVERT
    Assert.AreEqual(LIlteredMap.ToString, '7=250');
  finally
    LMap.Free;
  end;
end;

procedure TDictionaryHelperTest.TestMaxKey;
var
  LDictionary: TDictEx<Integer, string>;
  LMaxKey: Integer;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LMaxKey := LDictionary.MaxKey;

    // Assert
    Assert.AreEqual(3, LMaxKey);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestMaxValue;
var
  LDictionary: TDictEx<Integer, string>;
  LMaxValue: string;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LMaxValue := LDictionary.MaxValue;

    // Assert
    Assert.AreEqual('Two', LMaxValue);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestMinKey;
var
  LDictionary: TDictEx<Integer, string>;
  LMinKey: Integer;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LMinKey := LDictionary.MinKey;

    // Assert
    Assert.AreEqual(1, LMinKey);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestMinValue;
var
  LDictionary: TDictEx<Integer, string>;
  LMinValue: string;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');

    // Act
    LMinValue := LDictionary.MinValue;

    // Assert
    Assert.AreEqual('One', LMinValue);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestPartition;
var
  LDictionary: TDictEx<Integer, string>;
  LPartitions: TPair<TMap<Integer, string>, TMap<Integer, string>>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
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
    Assert.AreEqual(2, LPartitions.Key.Count); // Two items with even-length values
    Assert.AreEqual(3, LPartitions.Value.Count); // Three items with odd-length values

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
  LDictionary: TDictEx<Integer, string>;
  LTakenDictionary: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
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

    Assert.IsTrue(LTakenDictionary.Contains(1));
    Assert.IsTrue(LTakenDictionary.Contains(2));
    Assert.IsTrue(LTakenDictionary.Contains(3));

    Assert.IsFalse(LTakenDictionary.Contains(4));
    Assert.IsFalse(LTakenDictionary.Contains(5));

    Assert.AreEqual('One', LTakenDictionary[1]);
    Assert.AreEqual('Two', LTakenDictionary[2]);
    Assert.AreEqual('Three', LTakenDictionary[3]);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestTakeWhile;
var
  LDictionary: TDictEx<Integer, string>;
  LTakenDict: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');
    LDictionary.Add(6, 'Six');

    // Act
    LTakenDict := LDictionary.TakeWhile(
      function(Key: Integer): Boolean
      begin
        // Continue até que a chave seja menor ou igual a 3
        Result := Key <= 3;
      end
    );

    // Assert
    Assert.AreEqual(3, LTakenDict.Count);

    Assert.IsTrue(LTakenDict.Contains(1));
    Assert.IsTrue(LTakenDict.Contains(2));
    Assert.IsTrue(LTakenDict.Contains(3));

    Assert.IsFalse(LTakenDict.Contains(4)); // Não deve conter a chave 4
    Assert.IsFalse(LTakenDict.Contains(5)); // Não deve conter a chave 5
    Assert.IsFalse(LTakenDict.Contains(6)); // Não deve conter a chave 6
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestSkip;
var
  LDictionary: TDictEx<Integer, string>;
  LSkippedDictionary: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
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

    Assert.IsTrue(LSkippedDictionary.Contains(3));
    Assert.IsTrue(LSkippedDictionary.Contains(4));
    Assert.IsTrue(LSkippedDictionary.Contains(5));

    Assert.IsFalse(LSkippedDictionary.Contains(1));
    Assert.IsFalse(LSkippedDictionary.Contains(2));

    Assert.AreEqual('Three', LSkippedDictionary[3]);
    Assert.AreEqual('Four', LSkippedDictionary[4]);
    Assert.AreEqual('Five', LSkippedDictionary[5]);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestSkipMemoryLeak;
var
  LFor: Integer;
  LDictionary: TDictEx<Integer, String>;
  LResult: TMap<Integer, string>;
begin
  LDictionary := TDictEx<Integer, String>.Create;
  try
    for LFor := 0 to 10 do
    begin
      LDictionary.Add(LFor, 'Valor ' + LFor.ToString);
      LResult := LDictionary.Skip(1);
    end;

    // Agora, verificamos se não há vazamento de memória
    Assert.Pass('Teste bem-sucedido: Nenhum vazamento de memória detectado.');
  finally
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestSkipWhile;
var
  LDictionary: TDictEx<Integer, string>;
  LSkipDict: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');
    LDictionary.Add(6, 'Six');

    // Act
    LSkipDict := LDictionary.SkipWhile(
      function(Key: Integer): Boolean
      begin
        // Pule até que a chave seja maior que 3
        Result := Key <= 3;
      end
    );

    // Assert
    Assert.AreEqual(3, LSkipDict.Count);

    Assert.IsFalse(LSkipDict.Contains(1)); // Não deve conter a chave 1
    Assert.IsFalse(LSkipDict.Contains(2)); // Não deve conter a chave 2
    Assert.IsFalse(LSkipDict.Contains(3)); // Não deve conter a chave 3

    Assert.IsTrue(LSkipDict.Contains(4));
    Assert.IsTrue(LSkipDict.Contains(5));
    Assert.IsTrue(LSkipDict.Contains(6));
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestSlice;
var
  LDictionary: TDictEx<Integer, string>;
  LSlicedDictionary: TMap<Integer, string>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
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

    Assert.IsTrue(LSlicedDictionary.Contains(2));
    Assert.IsTrue(LSlicedDictionary.Contains(3));
    Assert.IsTrue(LSlicedDictionary.Contains(4));

    Assert.IsFalse(LSlicedDictionary.Contains(1));
    Assert.IsFalse(LSlicedDictionary.Contains(5));

    Assert.AreEqual('Two', LSlicedDictionary[2]);
    Assert.AreEqual('Three', LSlicedDictionary[3]);
    Assert.AreEqual('Four', LSlicedDictionary[4]);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestZip;
var
  LDictionary1, LDictionary2: TDictEx<Integer, string>;
  LZippedDictionary: TMap<Integer, string>;
begin
  // Arrange
  LDictionary1 := TDictEx<Integer, string>.Create;
  LDictionary2 := TDictEx<Integer, string>.Create;
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

    Assert.IsTrue(LZippedDictionary.Contains(1));
    Assert.IsTrue(LZippedDictionary.Contains(2));
    Assert.IsTrue(LZippedDictionary.Contains(3));

    Assert.AreEqual('One | Uno', LZippedDictionary[1]);
    Assert.AreEqual('Two | Dos', LZippedDictionary[2]);
    Assert.AreEqual('Three | Tres', LZippedDictionary[3]);
  finally
    // Clean up
    LDictionary1.Free;
    LDictionary2.Free;
  end;
end;

procedure TDictionaryHelperTest.TestFlatMap;
var
  LDictionary: TDictEx<Integer, string>;
  LFlatMappedDictionary: TMap<Integer, Integer>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;
  try
    LDictionary.Add(1, '1,2,3');
    LDictionary.Add(2, '4,5');
    LDictionary.Add(3, '6');

    // Act
    LFlatMappedDictionary := LDictionary.FlatMap<Integer>(
      function(Value: TValue): TArray<Integer>
      var
        LValues: TArray<string>;
        LItem: string;
        LFlatMappedValues: TVector<Integer>;
      begin
        LValues := Value.ToString.Split([',']);
        LFlatMappedValues := [];
        for LItem in LValues do
        begin
          LFlatMappedValues.Add(StrToInt(LItem));
        end;
        Result := LFlatMappedValues;
      end
    );

    // Assert
    Assert.AreEqual(6, LFlatMappedDictionary.Count);

    Assert.IsTrue(LFlatMappedDictionary.Contains(1));
    Assert.IsTrue(LFlatMappedDictionary.Contains(2));
    Assert.IsTrue(LFlatMappedDictionary.Contains(3));

    Assert.AreEqual(1, LFlatMappedDictionary[1]);
    Assert.AreEqual(2, LFlatMappedDictionary[2]);
    Assert.AreEqual(3, LFlatMappedDictionary[3]);
    Assert.AreEqual(4, LFlatMappedDictionary[4]);
    Assert.AreEqual(5, LFlatMappedDictionary[5]);
    Assert.AreEqual(6, LFlatMappedDictionary[6]);
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

procedure TDictionaryHelperTest.TestPartitionBy;
var
  LDictionary: TDictEx<Integer, string>;
  LPartitionedDict: TMap<Boolean, TVector<string>>;
begin
  // Arrange
  LDictionary := TDictEx<Integer, string>.Create;

  try
    LDictionary.Add(1, 'One');
    LDictionary.Add(2, 'Two');
    LDictionary.Add(3, 'Three');
    LDictionary.Add(4, 'Four');
    LDictionary.Add(5, 'Five');
    LDictionary.Add(6, 'Six');

    // Act
    LPartitionedDict := LDictionary.PartitionBy(
      function(Value: string): Boolean
      begin
        // Particiona com base na primeira letra sendo vogal ou consoante
        Result := CharInSet(Value[1], ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U', 'T']);
      end
    );

    // Assert
    Assert.AreEqual(2, LPartitionedDict.Count);

    // Verifica a partição das vogais
    Assert.AreEqual(3, LPartitionedDict[True].Count);

    Assert.IsTrue(LPartitionedDict.Contains(True));

    Assert.IsTrue(LPartitionedDict[True].Contains('One'));
    Assert.IsTrue(LPartitionedDict[True].Contains('Two'));
    Assert.IsTrue(LPartitionedDict[True].Contains('Three'));

    // Verifica a partição das consoantes
    Assert.AreEqual(3, LPartitionedDict[False].Count);

    Assert.IsTrue(LPartitionedDict.Contains(False));

    Assert.IsTrue(LPartitionedDict[False].Contains('Four'));
    Assert.IsTrue(LPartitionedDict[False].Contains('Five'));
    Assert.IsTrue(LPartitionedDict[False].Contains('Six'));
  finally
    // Clean up
    LDictionary.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDictionaryHelperTest);

end.

