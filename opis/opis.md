# Opis języka

Statycznie typowany język imperatywny
ze składnią wzorowana na C, C++ i Javie.
Podstawowe konstrukcje (pętle, instrukcje warunkowe,
deklaracja zmiennych itp.)
mają semantykę tradycyjną, jak w powyżej wymienionych językach.

# Typy proste

- int
- bool
- string
- void (dla funkcji nie dających wyniku)

# Typy złożone

## Tablice

- stałej długości tablice typów int, bool, string (nie można robić tablic tablic
  ani tablic krotek)
- składnia podobna do Javy
- dodatkowa funkcja len() pobierająca długość tablicy
- inicjalizacja literałem lub podanie długości i inicjalizacja wartościami domyślnymi
- wartości można pobierać z tablicy za pomocą nawiasów kwadratowych
- wartości można aktualizować za pomocą nawiasów kwadratowych

```
int[] a = {1,2,3};
int[] b = new int[10];

int c = a[0];
b[0] = 1;
int e = len(a);
```

## Zagnieżdżone krotki

- zagnieżdżone krotki typów int, bool, string (elementem krotki nie może być tablica)
- składnia przypisania z krotki, tie, inspirowana std::tie z C++,
  przy czym tutaj obsługujemy zagnieżdżone przypisanie
- wartości można pobierać z krotki za pomocą nawiasów kwadratowych
- ze względu na kontrolę typów indeks użyty do pobrania
  wartości w krotce musi być liczbą całkowitą znaną przed wykonaniem programu

```
<int, bool, string> x = new tuple(1, true, "string");

int i; bool a; string s;
tie (i, b, s) = y;

bool d = y[1];
```

# Funkcje

dowolnie zagnieżdżone definicje funkcji z zachowaniem poprawności
statycznego wiązania identyfikatorów (jak w Pascalu)

# Błędy wykonania

obsługa błędów wykonania wraz z podaniem miejsca wystąpienia błędu
