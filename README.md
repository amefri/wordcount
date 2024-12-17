# Wordcount

**Ein Haskell-Programm, das eine Textdatei einliest, die Wörter alphabetisch sortiert und Duplikate entfernt.**

## Voraussetzungen

Um dieses Projekt zu verwenden, benötigst du Folgendes:

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) (zum Bauen und Ausführen von Haskell-Projekten)
- Eine Unix-basierte Shell (z.B. Bash) zum Ausführen der `run.sh`-Datei

## Installation

1. Klone das Repository:

   ```bash
   git clone https://github.com/amefri/wordcount/
   cd wordcount
   ```

2. Stelle sicher, dass Stack installiert ist:

   ```bash
   stack --version
   ```

3. Kompiliere und führe das Projekt aus, indem du das Shell-Skript `run.sh` ausführst:

```bash
   chmod +x run.sh
  ./run.sh
  ```
Gib danach den Pfad der Datei ein die du sortieren möchtest.
  

## Aufbau des Projekts

Die wichtigsten Dateien und Verzeichnisse:

```
wordcount/
├── app/                   # Hauptanwendungscode
│   └── Main.hs            # Einstiegspunkt der Anwendung
├── src/                   # Quellcode des Projekts
│   └── Lib.hs             # Bibliotheksfunktionen
├── test/                  # Tests für das Projekt
│   └── Spec.hs            # Testfälle für die Anwendung
├── run.sh                 # Skript zum Kompilieren und Ausführen
├── package.yaml           # Projektkonfiguration für Stack
├── stack.yaml             # Stack-spezifische Konfigurationsdatei
├── stack.yaml.lock        # Stack-Lockdatei
├── project.cabal          # Cabal-Konfigurationsdatei
├── output.txt             # Beispielausgabedatei
├── Setup.hs               # Setup-Skript für Cabal
├── LICENSE                # Lizenzdatei
├── CHANGES.md             # Änderungsprotokoll
└── README.md              # Diese Datei
```

## Verwendung

1. Erstelle eine Eingabedatei (z.B. `input.txt`) mit einigen Wörtern, getrennt durch Leerzeichen oder Zeilenumbrüche:

   ```
   banana apple orange apple kiwi banana
   ```

2. Führe das Skript aus:

   ```bash
   ./run.sh input.txt
   ```

3. Die Ausgabe zeigt die Wörter alphabetisch sortiert und ohne Duplikate:

   ```
   apple banana kiwi orange
   ```

Falls du das Projekt manuell mit Stack ausführen möchtest, benutze folgende Befehle:

```bash
stack build        # Kompiliert das Projekt
stack exec wordcount < input.txt   # Führt das Projekt aus
```

## Beispielausgabe

Beispiel-Eingabedatei (`input.txt`):

```
apple banana kiwi apple banana orange
```

Ausgabe im Terminal:

```
apple banana kiwi orange
```




**Patricia Tetcu, Amelie Frimberger**  
**[GitHub Repository](https://github.com/amefri/wordcount/)**
