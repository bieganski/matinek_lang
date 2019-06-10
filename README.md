Interpreter funkcyjnego języka składnią zbliżonego do Haskella.
Ewaluacja wyrażeń jest zachłanna.
Wynik programu jest ewaluacją wyrażenia o nazwie "main"
Interpreter czyta input z wejścia, zatem aby uruchumić na zawartości pliku "test.txt",
należy wpisać "cat test.txt | ./interpreter".

Kolejne przykłady (goodN.in) obrazują kolejne funkcjonalności interpretera:

N =
1.  Prosty przykład działania
2.  Importowanie modułów
3.  Sposoby deklaracji funkcji
4.  Rekurencja
5.  Typy algebraiczne
6.  Wbudowane typy (List, Maybe)
7.  Lukier składniowy dla list (input i output)
8.  Pattern matching



Uwagi:
Gramatyka AbsGrammar za pomocą Simplify.hs jest konwertowana na prostszą ProgGrammar.hs.
Do zrobienia zostało mi podpięcie typechecku, póki co jest typowanie dynamiczne.

W paczce znajdują się pliki wynikowe programu bnfc, można natomiast
wygenerować je ręcznie.


Druga iteracja - poprawki:

- dodane przykłady bad, dodatkowe good
- działająca rekonstrukcja typów (statyczna)
- ładna obsługa błędów (nie używam nigdzie 'error')
- definicje typów dopuszczalne na początku pliku
- rekurencyjne definicje globalne

