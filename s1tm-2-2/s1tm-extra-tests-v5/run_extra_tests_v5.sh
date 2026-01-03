#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTDIR="$ROOT/extra_tests_v5"

echo "[build] dune build"
dune build >/dev/null

fail=0
pass=0

echo "[run] a correr testes em: $TESTDIR"
for in_file in "$TESTDIR"/*.in; do
  name="$(basename "$in_file" .in)"
  exp_file="$TESTDIR/$name.exp"
  out_file="$(mktemp)"

  dune exec -- ./s1tm.exe < "$in_file" > "$out_file" || true

  if diff -u "$exp_file" "$out_file" >/dev/null; then
    echo "[PASS] $name"
    pass=$((pass+1))
  else
    echo "[FAIL] $name"
    diff -u "$exp_file" "$out_file" || true
    fail=$((fail+1))
  fi

  rm -f "$out_file"
done

echo
if [ "$fail" -eq 0 ]; then
  echo "OK: $pass testes passaram."
  exit 0
else
  echo "ERRO: $fail testes falharam; $pass passaram."
  exit 1
fi
