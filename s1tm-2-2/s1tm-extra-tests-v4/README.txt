Testes extra v4 (mais agressivos)

Inclui:
- Limite 200 passos (aceita em 199; DON'T KNOW em 201)
- Máquina com marcação (X/Z) e múltiplas passagens na fita
- Incremento binário (carry / overflow)
- Um teste "armadilha" de YAML: símbolo Y pode ser interpretado como boolean em alguns parsers (espera INVALID)

Como correr:
  bash s1tm-extra-tests-v4/run_extra_tests_v4.sh
