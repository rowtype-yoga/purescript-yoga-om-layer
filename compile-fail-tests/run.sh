#!/bin/bash
cd "$(dirname "$0")/.."

# Build the library first
bunx spago build > /dev/null 2>&1

PASS=0
FAIL=0
TOTAL=0

for f in compile-fail-tests/*.purs; do
  EXPECT=$(head -1 "$f" | sed 's/-- EXPECT: //')
  NAME=$(basename "$f" .purs)
  TOTAL=$((TOTAL + 1))

  cp "$f" "src/_CompileFailTest.purs"
  OUTPUT=$(bunx spago build 2>&1) || true
  rm -f "src/_CompileFailTest.purs"

  if echo "$OUTPUT" | grep -q "Build succeeded"; then
    echo "FAIL $NAME — compiled successfully (should have failed)"
    FAIL=$((FAIL + 1))
  elif echo "$OUTPUT" | grep -q "$EXPECT"; then
    echo "PASS $NAME — failed with expected: $EXPECT"
    PASS=$((PASS + 1))
  else
    echo "FAIL $NAME — failed but without expected '$EXPECT'"
    echo "     Got: $(echo "$OUTPUT" | grep "ERROR\|error" | head -3)"
    FAIL=$((FAIL + 1))
  fi
done

echo ""
echo "$PASS passed, $FAIL failed out of $TOTAL compile-fail tests"

if [ "$FAIL" -ne 0 ]; then
  exit 1
fi
