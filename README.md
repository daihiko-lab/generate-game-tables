# generate-game-tables

> **開発中** — このリポジトリは現在開発中です。使い方や出力形式は予告なく変わる可能性があります。

2人非同時ゲームの利得行列を `flextable` で組み、`webshot2` でPNG画像として出力するRスクリプトです。Nash均衡のハイライト付きPNGも出力できます。

## 出力例

既定設定 (`GAME SETTINGS`) で生成したサンプルです。

**通常版**

![利得表 (通常版)](docs/samples/3strategies.png)

**Nash均衡ハイライト付き**

![利得表 (Nash均衡ハイライト付き)](docs/samples/3strategies_with_nash.png)

## Nash均衡のハイライトについて

主目的は利得表の画像生成です。Nash均衡は**純粋戦略**に限り、該当セルを表上でハイライトする補助機能です。混合戦略均衡の計算や、複数均衡がある場合の理論的な整理・説明は行いません。

## セル注釈

特定の利得セルに `※` マークを付け、表の直下に解説を右寄せで並べられます。`GAME_CELL_ANNOTATIONS_ENABLED <- FALSE` にすると注釈は描画されません (設定内容は残しておいても構いません)。

`GAME_CELL_ANNOTATIONS` に、注釈1件につきリスト1要素を書きます。

- `text` … 表下に出す説明文 (必須)
- `row`, `col` … 対象セル (1始まり。`row` = 左のプレイヤー、`col` = 上のプレイヤー)
- `cells` … 同じ解説を複数セルに付けるときは `list(c(row, col), …)` で指定
- `mark` … 記号の番号 (省略時は自動。解説が1種類だけのときは `※` のみ、2種類以上で `※1`, `※2`, …)

```r
GAME_CELL_ANNOTATIONS_ENABLED <- TRUE
GAME_CELL_ANNOTATIONS <- list(
  list(cells = list(c(2, 2)), text = "無駄なく、平等に分割"),
  list(cells = list(c(3, 1), c(1, 3)), text = "無駄はないが、不平等な分割")
)
```

## 必要なもの

- R (4.x 系を推奨)
- [webshot2](https://cran.r-project.org/package=webshot2) が利用できる **Chrome / Chromium** (初回実行時や `install_chromium()` の案内に従ってください)

## 依存パッケージ

`flextable`, `webshot2`, `magrittr`, `officer`, `grid`, `gridExtra`, `png`

初回実行時はプロンプトに従ってCRANからインストールできます。

## 使い方

リポジトリのルートで:

```sh
Rscript generate_game_tables.R
```

PNGは既定で `output/` に保存されます (`GAME_OUTPUT_DIR` で変更できます)。`output/` 内の PNG は `.gitignore` 対象です。README 用のサンプル画像は `docs/samples/` に置きます (再生成したあと、必要ならここへコピーしてください)。

## カスタマイズ

`generate_game_tables.R` 先頭の **GAME SETTINGS** で、戦略集合・利得行列・プレイヤー表示ラベル・セル注釈・出力先などを編集してください。処理を実行したくない場合は `GAME_ENABLED <- FALSE` にしてください。

## ライセンス

MIT License を [`LICENSE`](LICENSE) に記載しています。
