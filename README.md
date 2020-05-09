# Opis

## Struktura plików

kod źródłowy znajduje się w katalogu `src`

- `Interpreter.hs` - kod uruchamiający
    1. parser
    2. analizator statyczny
    3. interpreter
- `CommonDeclarations.hs` - deklaracje zmiennych i funkcji
   używanych w innych plikach
- `Expressions.hs` - kod odpowiedzialny za interpretację wyrażeń
- `Statements.hs` - kod odpowiedzialny za interpretację instrukcji
- `TypeChecker.hs` - kod odpowiedzialny za kontrolę
  statyczną

## Wykonanie instrukcji

Ze względu na instrukcje wymagające skoku w kodzie
(return, break, continue), funkcja wykonująca instrukcje
jest podzielona na dwie warstwy: funkcję `exec` która sprawdza,
czy kolejna instrukcja powinna zostać pominięta
oraz funkcję `internalExec`, wykonująca faktyczną interpretację.

## Zmienne, funkcje, środowisko

W celu implementacji statycznej widoczności
oraz przysłaniania identyfikatorów **środowisko interpretacji**
podzielone jest na dwie części: `Environment` oraz `Store`.
Dodatkowo, oprócz zmiennych i funkcji w środowisku przechowywana
jest informacja o wynikach funkcji (`ReturnParameter`)
oraz flagi umożliwiające wywołanie instrukcji `break` i `continue`
(odpowiednio `BreakParameter` oraz `ContinueParameter`).

**Środowisko** używane do **kontroli typów** jest skonstruowane analogicznie
do środowiska interpretacji. Różnica polega na tym, że przechowywane
są tylo typy. Dodatkowo, nie jest konieczne przechowywanie informacji
o instrukcjach `break` i `continue`, ponieważ 