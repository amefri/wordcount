#!/bin/bash

# Skript für stack build, stack test, Dateiüberprüfung und abschließendes Ausführen von stack exec

# Mit set -e stoppen wir das Skript, falls ein Befehl fehlschlägt
set -e

# Name der Datei, die eingelesen werden soll
FILE="warandpeace.txt"

# Überprüfen, ob die Datei existiert
if [ ! -f "$FILE" ]; then
  echo "Die Datei '$FILE' wurde nicht gefunden."
  echo "Bitte sicherstellen, dass '$FILE' im aktuellen Verzeichnis vorhanden ist."
  exit 1
else
  echo "Die Datei '$FILE' wurde gefunden. Fahre mit dem Build fort."
fi

# Ausgabe des Startzeitpunkts
echo "Start: $(date)"

# Stack build ausführen
echo "Running stack build..."
stack build
if [ $? -eq 0 ]; then
  echo "stack build completed successfully!"
else
  echo "stack build failed!"
  exit 1
fi

# Stack test ausführen
echo "Running stack test..."
stack test
if [ $? -eq 0 ]; then
  echo "stack test completed successfully!"
else
  echo "stack test failed!"
  exit 1
fi

# Abschließendes Ausführen von stack exec
EXECUTABLE="stack exec project-exe"

echo "Running final executable: $EXECUTABLE"
$EXECUTABLE
if [ $? -eq 0 ]; then
  echo "Execution of $EXECUTABLE completed successfully!"
else
  echo "Execution of $EXECUTABLE failed!"
  exit 1
fi

# Ausgabe des Endzeitpunkts
echo "End: $(date)"
echo "All tasks completed successfully!"
exit 0
