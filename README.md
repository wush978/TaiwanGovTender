# 政府標案的資料爬梳

## Page Crawler

`parse_page.R`是抓取查詢招標結果表格的程式。

## 使用方法

### R 的套件設定

請直接執行`Rscript bootstrap.R`來安裝需要的套件

### 爬梳決標查詢結果

```sh
R --vanilla --args --month 2015-11 < parse_page.R | tee parse_page-2015-11.log
```

以上指令會建立一個名稱為`2015-11`的資料夾，並把每一頁的結果，以csv的格式透過.gz格式壓縮後儲存至該資料夾中。


