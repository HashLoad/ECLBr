unit UTestEclbr.Map;

interface

uses
  DUnitX.TestFramework,
  Generics.Collections,
  eclbr.map;

type
  [TestFixture]
  TMapTest = class
  private
    FMap: TMap<Integer, String>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestAddOrUpdate;
    [Test]
    procedure TestGetValue;
    [Test]
    procedure TestRemove;
    [Test]
    procedure TestLength;
    [Test]
    procedure TestAddAndGet;
    [Test]
    procedure TestLastItemEqualsLastAdded;
    [Test]
    procedure TestMapMerge;
    [Test]
    procedure TestMapFilter;
    [Test]
    procedure TestMapToJson;
    [Test]
    procedure TestMapCapacity;
    [Test]
    procedure TestMapToString;
    [Test]
    procedure TestMapAddRange;
    [Test]
    procedure TestEnumerator;
  end;

implementation

{ TMapTest }

procedure TMapTest.Setup;
begin
end;

procedure TMapTest.TearDown;
begin
  FMap.Clear;
end;

procedure TMapTest.TestAddAndGet;
begin
  FMap.Add(1, 'One');
  FMap.Add(2, 'Two');

  Assert.AreEqual('One', FMap[1]);
  Assert.AreEqual('Two', FMap[2]);
end;

procedure TMapTest.TestAddOrUpdate;
var
  LArrayPair: TMap<string, Integer>;
begin
  LArrayPair.AddOrUpdate('Key1', 10);
  LArrayPair.AddOrUpdate('Key2', 20);
  LArrayPair.AddOrUpdate('Key3', 30);

  Assert.AreEqual(10, LArrayPair.GetValue('Key1'));
  Assert.AreEqual(20, LArrayPair.GetValue('Key2'));
  Assert.AreEqual(30, LArrayPair.GetValue('Key3'));

  LArrayPair.AddOrUpdate('Key2', 50);

  Assert.AreEqual(50, LArrayPair.GetValue('Key2'));
end;

procedure TMapTest.TestEnumerator;
var
  LPair: TMapPair<Integer, String>;
  LKey: Integer;
  LLast: string;
begin
  FMap.Add(1, 'One');
  FMap.Add(2, 'Two');
  FMap.Add(3, 'Three');

  for LPair in FMap do
  begin
    LKey := LPair.Key;
    LLast := LPair.Value;
  end;

  Assert.AreEqual(3, LKey);
  Assert.AreEqual('Three', LLast);
end;

procedure TMapTest.TestGetValue;
var
  LArrayPair: TMap<string, Integer>;
begin
  LArrayPair.AddOrUpdate('Key1', 10);
  LArrayPair.AddOrUpdate('Key2', 20);
  LArrayPair.AddOrUpdate('Key3', 30);

  Assert.AreEqual(10, LArrayPair.GetValue('Key1'));
  Assert.AreEqual(20, LArrayPair.GetValue('Key2'));
  Assert.AreEqual(30, LArrayPair.GetValue('Key3'));
  // Chave inexistente retorna o valor padrão (0)
  Assert.AreEqual(0, LArrayPair.GetValue('Key4'));
end;

procedure TMapTest.TestRemove;
var
  LArrayPair: TMap<string, Integer>;
begin
  LArrayPair.AddOrUpdate('Key1', 10);
  LArrayPair.AddOrUpdate('Key2', 20);
  LArrayPair.AddOrUpdate('Key3', 30);

  Assert.IsTrue(LArrayPair.Contains('Key1'));
  Assert.IsTrue(LArrayPair.Contains('Key2'));
  Assert.IsTrue(LArrayPair.Contains('Key3'));

  LArrayPair.Remove('Key2');

  Assert.IsTrue(LArrayPair.Contains('Key1'));
  Assert.IsFalse(LArrayPair.Contains('Key2'));
  Assert.IsTrue(LArrayPair.Contains('Key3'));
end;

procedure TMapTest.TestLength;
var
  LArrayPair: TMap<string, Integer>;
begin
  Assert.AreEqual(0, LArrayPair.Count);

  LArrayPair.AddOrUpdate('Key1', 10);
  LArrayPair.AddOrUpdate('Key2', 20);
  LArrayPair.AddOrUpdate('Key3', 30);

  Assert.AreEqual(3, LArrayPair.Count);

  LArrayPair.Remove('Key2');

  Assert.AreEqual(2, LArrayPair.Count);
end;

procedure TMapTest.TestMapAddRange;
var
  MapToAdd: TMap<Integer, String>;
begin
  FMap.Add(1, 'One');
  FMap.Add(2, 'Two');

  MapToAdd.Add(3, 'Three');
  MapToAdd.Add(4, 'Four');

  FMap.AddRange(MapToAdd);

  Assert.AreEqual(4, FMap.Count);
  Assert.AreEqual('Three', FMap[3]);
  Assert.AreEqual('Four', FMap[4]);
end;

procedure TMapTest.TestMapCapacity;
begin
  FMap.SetDefaultCapacity(16);
  FMap.SetCapacity(10);

  Assert.AreEqual(10, FMap.Capacity);
end;

procedure TMapTest.TestMapFilter;
var
  LilteredMap: TMap<integer, string>;
begin
  FMap.Add(1, 'One');
  FMap.Add(2, 'Two');
  FMap.Add(3, 'Three');

  LilteredMap := FMap.Filter(
    function(Key: Integer; Value: string): Boolean
    begin
      Result := Key mod 2 = 0;
    end);

  Assert.IsTrue(LilteredMap.Contains(2));
  Assert.IsFalse(LilteredMap.Contains(1));
  Assert.IsFalse(LilteredMap.Contains(3));
end;

procedure TMapTest.TestLastItemEqualsLastAdded;
var
  LastItem: TMapPair<Integer, String>;
begin
  FMap.Add(1, 'One');
  FMap.Add(2, 'Two');
  FMap.Add(3, 'Three');

  LastItem := FMap.Last;

  Assert.AreEqual(3, LastItem.Key);
  Assert.AreEqual('Three', LastItem.Value);
end;

procedure TMapTest.TestMapMerge;
begin
  FMap.Add(1, 'One');
  FMap.Add(2, 'Two');

  FMap.Merge([TMapPair<Integer, String>.Create(3, 'Three'),
              TMapPair<Integer, String>.Create(4, 'Four')]);

  Assert.IsTrue(FMap.Contains(3));
  Assert.IsTrue(FMap.Contains(4));
end;

procedure TMapTest.TestMapToJson;
var
  JsonString: string;
begin
  FMap.Add(1, 'One');
  FMap.Add(2, 'Two');

  JsonString := FMap.ToJson;

  Assert.AreEqual('{"1": "One", "2": "Two"}', JsonString);
end;

procedure TMapTest.TestMapToString;
begin
  FMap.Add(1, 'One');
  FMap.Add(2, 'Two');

  Assert.AreEqual('1=One 2=Two', FMap.ToString);
end;

initialization
  TDUnitX.RegisterTestFixture(TMapTest);

end.

