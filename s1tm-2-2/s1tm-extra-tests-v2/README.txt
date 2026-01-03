COMO USAR (SEM DRAMA)

1) Copia a pasta "s1tm-extra-tests-v2" para dentro do teu projeto (a mesma pasta onde tens o ficheiro 'dune').

   Exemplo:
     s1tm-2/
       dune
       s1tm.ml
       test/
       s1tm-extra-tests-v2/

2) Corre os testes a partir da pasta do projeto:
     bash s1tm-extra-tests-v2/run_extra_tests.sh

O script compara o output do teu programa com os ficheiros .exp.

NOTA:
- Estes testes assumem que o teu output e 'YES'/'NO' (como no enunciado).
- Se estiveres a imprimir 'TRUE'/'FALSE', estes testes vao falhar (e isso e mesmo o objetivo: apanha o erro cedo).
