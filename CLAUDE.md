# Yomikun — iOS Companion App

The `ios/` directory implements an iPad reader for Japanese texts annotated by the Emacs yomikun package. The Emacs side remains unchanged; iOS is read-only for word status (with writes going back to the shared SQLite databases). Python preprocessing scripts live in `ios/ios-python/`.

## Architecture overview

```
Japanese HTML file
      │
      ▼
ios/ios-python/process.py      ← MeCab tokenization on Mac
      │  wraps tokens in <span class="yk" data-root data-wtype …>
      │  embeds CSS + JS in the HTML (self-contained file)
      ▼
Annotated HTML file
      │  copied via Files app / iCloud to iPad simulator
      ▼
ios/ Yomikun.app               ← SwiftUI + WKWebView
      │  injects word statuses from jp-status.db at load time
      │  tap a word → definition sheet + status buttons
      │  long-press + select → native Copy or "Comment"
      ▼
jp-status.db / dictionary.db  ← shared SQLite databases
      │  cleaned copy (emacsql quotes stripped) via prepare-dbs.py
      │  stored in app sandbox Documents folder
```

## Database notes

The Emacs side stores **all SQLite string values wrapped in double-quotes** (emacsql serialisation, e.g. `を` → `"を"`). The iOS app uses a clean copy produced by `ios/ios-python/prepare-dbs.py`, which strips these quotes. The original Emacs databases are never touched.

### Status DB schema (`jp-status.db`)
```sql
words   (morph TEXT, mtype TEXT, surface TEXT, status TEXT, date TEXT,
         PRIMARY KEY (morph, mtype, surface))
comments (id INTEGER PRIMARY KEY AUTOINCREMENT, file TEXT, xpath TEXT,
          offset INTEGER, comment TEXT, created TEXT)
```
`comments` is created by the iOS app on first launch.

### Dictionary DB schema (`dictionary.db`)
```sql
dict      (root TEXT, pronun TEXT, pos TEXT, def TEXT, rank INT)
dict2     (root TEXT, pronun TEXT, pos TEXT, def TEXT, rank INT)
entries   (root TEXT, reading TEXT, pos TEXT, wtype TEXT, gloss TEXT)
compounds (compound TEXT)
```

## Word status values
`unknown` | `learning` | `known` | `ignore`

Words absent from the DB are treated as `unknown`.

## Key workflows

**Prepare databases (run once, or after DB schema changes):**
```bash
cd ios/ios-python
python prepare-dbs.py          # auto-detects iPad Air 11-inch simulator sandbox
```

**Process a book chapter:**
```bash
cd ios/ios-python
SANDBOX=$(xcrun simctl get_app_container 34B57F6D-0E5B-45C4-A939-9379E18BB43B \
          org.turingmachine.yomikun data)/Documents
python process.py ~/Sync/large/jp/books/kino-no-tabi-7/html/kino-7-001.html \
       --out-dir "$SANDBOX"
```
Always re-run process.py after editing it (CSS/JS changes require regenerating HTML files).

**Build and install the app:**
```bash
cd ios
xcodebuild -project Yomikun.xcodeproj -scheme Yomikun \
  -destination 'platform=iOS Simulator,id=34B57F6D-0E5B-45C4-A939-9379E18BB43B' \
  -configuration Debug build
xcrun simctl install 34B57F6D-0E5B-45C4-A939-9379E18BB43B \
  ~/Library/Developer/Xcode/DerivedData/Yomikun-*/Build/Products/Debug-iphonesimulator/Yomikun.app
xcrun simctl launch 34B57F6D-0E5B-45C4-A939-9379E18BB43B org.turingmachine.yomikun
```

## Simulator

- Device: iPad Air 11-inch (M3), ID `34B57F6D-0E5B-45C4-A939-9379E18BB43B`
- Bundle ID: `org.turingmachine.yomikun`
- iCloud is **not** active (personal developer account); databases and HTML files live in the app sandbox Documents folder
- Files are visible in the simulator's Files app under "On My iPad → Yomikun"
