# Red-Black Tree

Dieses Projekt implementiert eine vollständige Datenverarbeitungspipeline, die Textdateien liest, bereinigt, in Wörter zerlegt, in einen Red-Black-Baum einfügt, die Wörter sortiert und die sortierten Wörter in eine Ausgabedatei schreibt. Es enthält außerdem umfangreiche Tests, um die verschiedenen Komponenten und Randfälle zu überprüfen.

----------------

## Anleitung zum automatischen Ausführen

### 1. Voraussetzungen

- Installiertes [Haskell Stack](https://docs.haskellstack.org/en/stable/) oder passende Erweiterung in VSCode.
- Eine Eingabedatei namens `warandpeace.txt` im aktuellen Verzeichnis (Testbeispiel).

### 2. Kompilieren und automatisch Ausführen

Folgende Befehle in die Konsole eingeben:

```bash
chmod +x run.sh  # Skript ausführbar machen
./run.sh        # Skript starten
```

### 3. Eingabe- und Ausgabedateien

- **Eingabe**: `warandpeace.txt` (oder eine andere Datei mit Textinhalt).
- **Ausgabe**: `output.txt` mit sortierten Wörtern.

Das Skript erledigt folgende Schritte automatisch:
- Überprüft, ob die Eingabedatei existiert.
- Führt `stack build` aus, um das Projekt zu kompilieren.
- Führt `stack test` aus, um die Tests auszuführen.
- Startet die Anwendung mit `stack exec project-exe`.

-------------------------

## Anleitung zum manuellen Ausführen:

1. Projekt aufbauen:
   ```bash
   stack build
   ```
2. Tests ausführen:
   ```bash
   stack test
   ```
3. Programm starten:
   ```bash
   stack exec project-exe
   ```
4. Bei Programmstart:
   - Pfad zur Eingangsdatei eingeben (z.B. `warandpeace.txt`).

5. Mit ````nano output.txt```` die Ausgabedatei ansehen

## Autorinnen
Amelie Frimberger & Patricia Tetcu


