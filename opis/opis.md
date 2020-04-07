# Opis języka
```java
int[] a = {1,2,3};
int[] b = new int[10];

int c = a[0];
b[0] = 1;
int e = len(a);
```

Statycznie typowany język imperatywny
ze składnią wzorowana na C, C++ i Javie.

# Typy proste

- int
- bool
- string
- void

# Typy złożone

## Tablice

- stałej długości tablice typów int, bool, string (nie można robić tablic tablic ani tablic krotek)
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

- potencjalnie zagnieżdżone krotki typów int, bool, string (elementem krotki nie może być tablica)
- składnia tie inspirowana std::tie z C++, przy tutaj obsługujemy zagnieżdżone przypisanie
- wartości można pobierać z krotki za pomocą nawiasów kwadratowych
  (ze względu na kontrolę typów indeks w krotce musi być literałem liczby całkowitej)
- wartości można aktualizować za pomocą nawiasów kwadratowych

```
<int, bool, string> x = new tuple(1, true, "string");

int i; bool a; string s;
tie (i, b, s) = y;

y[0] = 2;
bool d = y[2];
```
