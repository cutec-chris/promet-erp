{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_besen;

interface

uses
  BESENArrayUtils, BESENASTNodes, BESENCharset, BESENCodeContext, 
  BESENCodeGeneratorContext, BESENCodeJITx64, BESENCompiler, BESENConstants, 
  BESENContext, BESENDeclarativeEnvironmentRecord, BESENDecompiler, 
  BESENDoubleList, BESENEnvironmentRecord, BESENErrors, BESENEvalCache, 
  BESENEvalCacheItem, BESENGarbageCollector, BESENHashMap, BESENHashUtils, 
  BESENIntegerList, BESENKeyIDManager, BESENLexer, BESENLexicalEnvironment, 
  BESENNativeCodeMemoryManager, BESENObject, BESENObjectArgGetterFunction, 
  BESENObjectArgSetterFunction, BESENObjectArray, BESENObjectArrayPrototype, 
  BESENObjectBoolean, BESENObjectConsole, BESENObjectDate, 
  BESENObjectDeclaredFunction, BESENObjectEnvironmentRecord, BESENObjectError, 
  BESENObjectErrorPrototype, BESENObjectFunctionPrototype, BESENObjectGlobal, 
  BESENObjectMath, BESENObjectNativeFunction, BESENObjectNumberConstructor, 
  BESENObjectNumberPrototype, BESENObjectRegExp, BESENObjectRegExpConstructor, 
  BESENObjectRegExpPrototype, BESENObjectStringConstructor, 
  BESENObjectThrowTypeErrorFunction, BESENOpcodes, BESENParser, 
  BESENPointerList, BESENPointerSelfBalancedTree, BESENRandomGenerator, 
  BESENRegExp, BESENRegExpCache, BESENSelfBalancedTree, BESENStringList, 
  BESENStringTree, BESENStringUtils, BESENTypes, BESENUnicodeTables, 
  BESENValue, BESENValueContainer, BESENVersionConstants, BESEN, 
  BESENBaseObject, BESENCode, BESENCodeJIT, BESENCodeJITx86, 
  BESENCodeSnapshot, BESENCollector, BESENCollectorObject, BESENDateUtils, 
  BESENGlobals, BESENInt64SelfBalancedTree, BESENLocale, BESENNativeObject, 
  BESENNumberUtils, BESENObjectArrayConstructor, BESENObjectBindingFunction, 
  BESENObjectBooleanConstructor, BESENObjectBooleanPrototype, 
  BESENObjectConstructor, BESENObjectDateConstructor, 
  BESENObjectDatePrototype, BESENObjectErrorConstructor, BESENObjectFunction, 
  BESENObjectFunctionArguments, BESENObjectFunctionConstructor, 
  BESENObjectJSON, BESENObjectNumber, BESENObjectPropertyDescriptor, 
  BESENObjectPrototype, BESENObjectString, BESENObjectStringPrototype, 
  BESENUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('laz_besen', @Register);
end.
