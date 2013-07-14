unit tmemoryleak;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { MemoryLeakTest }

  MemoryLeakTest= class(TTestCase)
  published
    procedure TestHeap;
  end; 

var
  HeapAllocated : LongInt;

implementation


{ MemoryLeakTest }

procedure MemoryLeakTest.TestHeap;
var
  HeapStatus: THeapStatus;
  UsedHeap: Integer;
begin
  HeapStatus := GetHeapStatus;
  if HeapStatus.HeapErrorCode <> 0 then
    Fail('HeapErrorCode <> 0');
  if HeapStatus.Overhead <> 0 then
    Fail('Overhead <> 0');
  UsedHeap := HeapStatus.TotalAllocated-HeapStatus.TotalFree;
//  if Usedheap <> 314720 then
//    Fail('Memory leak');
end;

initialization

  RegisterTest(MemoryLeakTest); 
end.

