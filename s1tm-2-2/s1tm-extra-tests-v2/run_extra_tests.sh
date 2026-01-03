#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$ROOT_DIR/.." && pwd)"
TEST_DIR="$ROOT_DIR/extra_tests"

if ! command -v dune >/dev/null 2>&1; then
  echo "ERRO: 'dune' nao esta instalado ou nao esta no PATH." >&2
  echo "Instala OCaml + dune (via opam) e volta a correr." >&2
  exit 2
fi

cd "$PROJECT_DIR"

# Build uma vez (para apanhar erros de compilacao antes dos testes)
echo "[build] dune build"
dune build >/dev/null

echo "[run] a correr testes em: $TEST_DIR"

pass=0
fail=0

tmp_out="$(mktemp)"
trap 'rm -f "$tmp_out"' EXIT

for in_file in "$TEST_DIR"/*.in; do
  base="${in_file%.in}"
  exp_file="$base.exp"
  name="$(basename "$base")"

  if [[ ! -f "$exp_file" ]]; then
    echo "[SKIP] $name (nao existe .exp)"
    continue
  fi

  # Normaliza CRLF para LF no output, para nao falhar em ambientes diferentes
  set +e
  dune exec -- ./s1tm.exe < "$in_file" | tr -d '\r' > "$tmp_out"
  status=$?
  set -e

  if [[ $status -ne 0 ]]; then
    echo "[FAIL] $name (programa terminou com erro: exit $status)"
    fail=$((fail+1))
    continue
  fi

  if diff -u "$exp_file" "$tmp_out" >/dev/null; then
    echo "[PASS] $name"
    pass=$((pass+1))
  else
    echo "[FAIL] $name"
    diff -u "$exp_file" "$tmp_out" || true
    fail=$((fail+1))
  fi

done

echo
if [[ $fail -eq 0 ]]; then
  echo "OK: $pass testes passaram."
else
  echo "ERRO: $fail testes falharam, $pass passaram."
  exit 1
fi
