# NVD0540I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0540I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　Ｄ３６５連携　　　　　　　　　　　　　*
*    モジュール名　　：　倉庫間在庫移動実績照会　　　　　　　　*
*    作成日／作成者　：　2020/04/27   ASS.II                   *
*    処理内容　　　　：　倉庫間の在庫移動指示・出庫・入庫作業の*
*    　　　　　　　　　　実績を照会する。　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0540I.
 AUTHOR.                ASS.II.
 DATE-WRITTEN.          20/04/27.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 新入出庫ファイル >>-*
     SELECT   DNSFILF4  ASSIGN        TO  DA-01-VI-DNSFILL4
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  DYNAMIC
                        RECORD KEY    IS  DNS4-F05 DNS4-F12
                                          DNS4-F01 DNS4-F02
                        FILE STATUS   IS  DNS4-ST1.
     SELECT   DNSFILF5  ASSIGN        TO  DA-01-VI-DNSFILL5
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  DYNAMIC
                        RECORD KEY    IS  DNS5-F08 DNS5-F18
                                          DNS5-F01 DNS5-F02
                        FILE STATUS   IS  DNS5-ST1.
     SELECT   DNSFILF7  ASSIGN        TO  DA-01-VI-DNSFILL7
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  DYNAMIC
                        RECORD KEY    IS  DNS7-F05 DNS7-F14
                                          DNS7-F01 DNS7-F02
                        FILE STATUS   IS  DNS7-ST1.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN        TO  DA-01-VI-ZSOKMS1
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  RANDOM
                        RECORD KEY    IS  SOK-F01
                        FILE STATUS   IS  SOK-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN        TO  DA-01-VI-MEIMS1
                        ORGANIZATION  IS  INDEXED
                        ACCESS MODE   IS  DYNAMIC
                        RECORD KEY    IS  MEI-F011   MEI-F0121
                                          MEI-F0122  MEI-F0123
                        FILE STATUS   IS  MEI-ST1.
*----<< 画面ファイル >>-*
     SELECT   DSPFILE   ASSIGN            TO  GS-DSPF
                        SYMBOLIC DESTINATION  "DSP"
                        FORMAT            IS  DSP-FMT
                        GROUP             IS  DSP-GRP
                        PROCESSING MODE   IS  DSP-PRO
                        SELECTED FUNCTION IS  DSP-FNC
                        FILE STATUS       IS  DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 新入出庫ファイル >>-*
 FD  DNSFILF4           LABEL     RECORD     IS  STANDARD.
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS4  AS PREFIX.
 FD  DNSFILF5           LABEL     RECORD     IS  STANDARD.
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS5  AS PREFIX.
 FD  DNSFILF7           LABEL     RECORD     IS  STANDARD.
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS7  AS PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS1   OF   XFDLIB    JOINING   SOK  AS PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     FVD05401  OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "NVD0540I".
 01  DATE-FLG                PIC  9(01)     VALUE   ZERO.
*\\
 01  ZAI-SIME.
     03  ZAI-SIME1           PIC  9(06)     VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES      ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(04).
         05  ZAI-SIME1R2     PIC  9(02).
     03  ZAI-SIME2           PIC  9(02)     VALUE ZERO.
*
 01  ZAI-SIMER               REDEFINES ZAI-SIME
                             PIC  9(08).
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT             PIC  X(08).
     03  DSP-GRP             PIC  X(08).
     03  DSP-PRO             PIC  X(02).
     03  DSP-FNC             PIC  X(04).
     03  DSP-ST1             PIC  X(02).
     03  DSP-ST2             PIC  X(04).
     03  DSP-CON             PIC  X(06).
     03  WK-GRP.
         05  WK-BODY         PIC  X(04).
         05  WK-LINE         PIC  9(02).
         05  FILLER          PIC  X(02).
 01  STATUS-AREA.
     03  IN-DATA             PIC  X(01).
     03  DNS4-STATUS.
         05  DNS4-ST1        PIC  X(02).
         05  DNS4-ST2        PIC  X(04).
     03  DNS5-STATUS.
         05  DNS5-ST1        PIC  X(02).
         05  DNS5-ST2        PIC  X(04).
     03  DNS7-STATUS.
         05  DNS7-ST1        PIC  X(02).
         05  DNS7-ST2        PIC  X(04).
     03  MEI-STATUS.
         05  MEI-ST1         PIC  X(02).
         05  MEI-ST2         PIC  X(04).
     03  SOK-STATUS.
         05  SOK-ST1         PIC  X(02).
         05  SOK-ST2         PIC  X(04).
*画面退避用
     COPY   FVD05401  OF XMDLIB  JOINING   SAV  AS   PREFIX.
*新入出庫ファイル読込エリア
     COPY     DNSFILF   OF   XFDLIB    JOINING   DNS  AS PREFIX.
 01  TABLE-AREA.
     03  TABLE-G.
       05  TABLE1      OCCURS 10.
           07  TABLE2  OCCURS  4.
               09  TBL-DPNO         PIC   9(07).
               09  TBL-GYONO        PIC   9(01).
               09  TBL-RENKE        PIC   X(01).
               09  TBL-SYOCD        PIC   X(08).
               09  TBL-HIN1         PIC   X(05).
               09  TBL-HIN2         PIC   X(02).
               09  TBL-HIN3         PIC   X(01).
               09  TBL-SIJISU       PIC   9(07).99.
               09  TBL-STK1         PIC   X(06).
               09  TBL-STK2         PIC   X(06).
               09  TBL-STK3         PIC   X(06).
               09  TBL-STK4         PIC   X(06).
               09  TBL-STK5         PIC   X(06).
               09  TBL-SSOKCD       PIC   X(02).
               09  TBL-STAN1        PIC   X(01).
               09  TBL-STAN2        PIC   X(03).
               09  TBL-STAN3        PIC   X(02).
               09  TBL-SYKBI        PIC   9(08).
               09  TBL-SYKSU        PIC   9(07).99.
               09  TBL-NSOKCD       PIC   X(02).
               09  TBL-NTAN1        PIC   X(01).
               09  TBL-NTAN2        PIC   X(03).
               09  TBL-NTAN3        PIC   X(02).
               09  TBL-NYKBI        PIC   9(08).
               09  TBL-NYKSU        PIC   9(07).99.
     03  TABLE-G-R  REDEFINES TABLE-G.
         05  TABLE2-R  OCCURS  40.
               09  TBL-DPNO-R       PIC   9(07).
               09  TBL-GYONO-R      PIC   9(01).
               09  TBL-RENKE-R      PIC   X(01).
               09  TBL-SYOCD-R      PIC   X(08).
               09  TBL-HINCD-R      PIC   X(08).
               09  TBL-SIJISU-R     PIC   9(07).99.
               09  TBL-STK1-R       PIC   X(06).
               09  TBL-STK2-R       PIC   X(06).
               09  TBL-STK3-R       PIC   X(06).
               09  TBL-STK4-R       PIC   X(06).
               09  TBL-STK5-R       PIC   X(06).
               09  TBL-SSOKCD-R     PIC   X(02).
               09  TBL-STANA-R      PIC   X(06).
               09  TBL-SYKBI-R      PIC   9(08).
               09  TBL-SYKSU-R      PIC   9(07).99.
               09  TBL-NSOKCD-R     PIC   X(02).
               09  TBL-NTANA-R      PIC   X(06).
               09  TBL-NYKBI-R      PIC   9(08).
               09  TBL-NYKSU-R      PIC   9(07).99.
*----<< ﾜｰｸ ｴﾘｱ >>----
 01  WK-AREA.
     03  WK-BSYNO1           PIC  X(02)  VALUE  SPACE.
     03  WK-BSYNO2           PIC  X(02)  VALUE  SPACE.
     03  WK-SIME             PIC  9(08)  VALUE  ZERO.
 01  WK-DPNO-AREA.
     03  WK-DPNO             PIC  9(07)  VALUE  ZERO.
 01  WK-HINMEI.
     03  WK-HINMEI1          PIC  N(15).
     03  WK-HINMEI2          PIC  N(15).
 01  WK-GYONO.
     03  WK-NUM1             PIC  N(01)  VALUE  NC"１".
     03  WK-NUM2             PIC  N(01)  VALUE  NC"２".
     03  WK-NUM3             PIC  N(01)  VALUE  NC"３".
     03  WK-NUM4             PIC  N(01)  VALUE  NC"４".
     03  WK-NUM5             PIC  N(01)  VALUE  NC"５".
     03  WK-NUM6             PIC  N(01)  VALUE  NC"６".
 01  FILLER                  REDEFINES   WK-GYONO.
     03  GYO-TBL             PIC  N(01)  OCCURS  6.
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り".
     03  G003                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り_前頁_次頁".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
                    NC"表示中のＰＦキー以外は使用できません".
     03  MSG02               PIC  N(20)  VALUE
                    NC"正しい日付区分を入力して下さい".
     03  MSG03               PIC  N(20)  VALUE
                    NC"倉庫マスタに存在しません".
     03  MSG04               PIC  N(20)  VALUE
                    NC"正しい日付を入力して下さい".
     03  MSG05               PIC  N(20)  VALUE
                    NC"次頁がありません".
     03  MSG06               PIC  N(20)  VALUE
                    NC"前頁がありません".
     03  MSG07               PIC  N(20)  VALUE
                    NC"対象データが表示件数を超えました".
     03  MSG08               PIC  N(20)  VALUE
                   NC"対象データがありません".
     03  MSG09               PIC  N(20)  VALUE
                    NC"入力された場所は存在しません．".
     03  MSG10               PIC  N(20)  VALUE
                    NC"入力された伝票_は存在しません．".
     03  MSG11               PIC  N(20)  VALUE
                    NC"削除区分に誤りがあります．".
     03  MSG12               PIC  N(20)  VALUE
                    NC"入力された商品は未登録です．".
     03  MSG13               PIC  N(20)  VALUE
                    NC"次レコードは存在しません．".
     03  MSG14               PIC  N(20)  VALUE
                    NC"前レコードは存在しません．".
     03  MSG15               PIC  N(20)  VALUE
                    NC"処理区分に誤りがあります．".
     03  MSG16               PIC  N(20)  VALUE
                    NC"＊＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG17               PIC  N(20)  VALUE
                    NC"明細を入力して下さい．".
     03  MSG18               PIC  N(20)  VALUE
                    NC"商品在庫マスタに未登録です．".
     03  MSG19               PIC  N(20)  VALUE
                    NC"担当者に誤りがあります．".
     03  MSG20               PIC  N(20)  VALUE
                    NC"未出庫フラグに誤りがあります".
     03  MSG21               PIC  N(20)  VALUE
                    NC"移動元はＮＡＶＳ倉庫のみ指定可能！".
     03  MSG22               PIC  N(20)  VALUE
                    NC"移動元、先ともＳＬＩＭＳ倉庫は指定不可".
     03  MSG23               PIC  N(20)  VALUE
                    NC"倉庫コードが条件マスタに未登録".
     03  MSG24               PIC  N(20)  VALUE
                    NC"＊＊＊＊＊＊＊＊＊＊＊＊＊".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       24.
*----<< ｼｽﾃﾑ ﾋﾂﾞｹ･ｼﾞｶﾝ ｴﾘｱ >>----
 01  WK-SYS-DATE             PIC  9(08).
*
 01  WK-SYS-YMD              PIC  9(06).
 01  WK-SYS-DATER    REDEFINES    WK-SYS-YMD.
     03  WK-SYS-YYR          PIC  9(02).
     03  WK-SYS-MMR          PIC  9(04).
     03  WK-SYS-DDR          PIC  9(04).
 01  WK-NYSYMD.
     03  WK-NYSYMD1          PIC  9(02).
     03  WK-NYSYMD2          PIC  9(02).
     03  WK-NYSYMD3          PIC  9(02).
 01  WK-NYS-DATE             PIC  9(08).
 01  SYS-DATE                PIC  9(06).
 01  FILLER                  REDEFINES  SYS-DATE.
     03  SYS-YY              PIC  9(02).
     03  SYS-MM              PIC  9(02).
     03  SYS-DD              PIC  9(02).
 01  SYS-DATE2               PIC  9(08).
 01  FILLER                  REDEFINES  SYS-DATE2.
     03  SYS-YYYY.
         05  SYS-YY2-1       PIC  9(02).
         05  SYS-YY2-2       PIC  9(02).
     03  SYS-MM2             PIC  9(02).
     03  SYS-DD2             PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*----<< ｲﾝﾃﾞｯｸｽ >>----
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  L                   PIC  9(02).
     03  IDX1                PIC  9(02).
     03  IXB                 PIC  9(02).
     03  IXC                 PIC  9(02).
     03  IXD                 PIC  9(02).
     03  IXE                 PIC  9(02).
*----<< ﾌﾗｸﾞ ｴﾘｱ >>----
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  DNS-FLG             PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  SYR-FLG             PIC  9(02)  VALUE  ZERO.
*----<< ｶｳﾝﾄ ｴﾘｱ >>----
 01  COUNTERS.
     03  P-CNT               PIC  9(07)  VALUE  ZERO.
     03  C-CNT               PIC  9(07)  VALUE  ZERO.
     03  S-CNT               PIC  9(07)  VALUE  ZERO.
     03  MAX-CNT             PIC  9(07)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE  ZERO.
*----<< ﾋﾝﾀﾝ ﾜｰｸ ｴﾘｱ >>----
 01  WK-HINT-AREA.
     03  WK-HINTI-X.
         05  WK-HINTI        PIC  X(01)  OCCURS  5.
     03  WK-HINTO-X.
         05  WK-HINTO        PIC  X(01)  OCCURS  5.
     03  WK-HINT             PIC  X(05).
*----<< ｷｰ ﾜｰｸ ｴﾘｱ >>----
 01  WK-AREA.
     03  WK-FDATE              PIC  9(08).
     03  WK-TDATE              PIC  X(08).
*
*****日付入力許容範囲（年月日）
 01  WK-HANI-ARE.
     03  WK-HANI1            PIC  9(08)  VALUE  ZERO.
     03  WK-HANI2            PIC  9(08)  VALUE  ZERO.
*****日付入力許容範囲（年月日）
 01  WK-HENKAN.
     03  WK-HENKAN-1         PIC  9(04)  VALUE  ZERO.
     03  WK-HENKAN-2         PIC  9(02)  VALUE  ZERO.
     03  WK-HENKAN-3         PIC  9(02)  VALUE  ZERO.
*****年チェック
 01  WK-CHKS-DATE.
     03  WK-CHKS-YYYY        PIC  9(04)  VALUE  ZERO.
     03  WK-CHKS-MM          PIC  9(02)  VALUE  ZERO.
     03  WK-CHKS-DD          PIC  9(02)  VALUE  ZERO.
*****年チェック
 01  WK-CHKN-DATE.
     03  WK-CHKN-YYYY        PIC  9(04)  VALUE  ZERO.
     03  WK-CHKN-MM          PIC  9(02)  VALUE  ZERO.
     03  WK-CHKN-DD          PIC  9(02)  VALUE  ZERO.
*****在庫締年月
 01  WK-ZAIKO-SIME           PIC  9(09).
 01  FILLER                  REDEFINES   WK-ZAIKO-SIME.
     03  WK-ZAIKO-SIME-0     PIC  9(01).
     03  WK-ZAIKO-SIME-1     PIC  9(04).
     03  WK-ZAIKO-SIME-2     PIC  9(02).
     03  WK-ZAIKO-SIME-3     PIC  9(02).
*****日付入力許容範囲（月）
 01  WK-H-TUKI.
     03  WK-H-TUKI-1         PIC  9(02)  VALUE  ZERO.
     03  WK-H-TUKI-2         PIC  9(02)  VALUE  ZERO.
*
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
******************************************************************
 PROCEDURE           DIVISION.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  新入出庫ファイル  --------------------------------*
 DNS4-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DNSFILF4.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"新入出庫ファイル異常！"
              "ST1=" DNS4-ST1                " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 DNS5-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DNSFILF5.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"新入ファイル異常！"
              "ST1=" DNS5-ST1                " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 DNS7-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DNSFILF7.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"新入出庫ファイル異常！"
              "ST1=" DNS7-ST1                " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   倉庫マスタ　-----------------------------------*
 AOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSOKMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"倉庫マスタ異常！"
              "ST1=" SOK-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG  =  "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-RTN           SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE2.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " START "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
*画面日付・時刻編集
     MOVE      SYS-DATE2(1:4)     TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)     TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)     TO   HEN-DATE-DD.
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*フラグ初期化
     INITIALIZE         FLAGS.
*ファイル　オープン
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     I-O       DSPFILE.
*
     MOVE     0                   TO   SYR-FLG.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-RTN           SECTION.
*
     EVALUATE    SYR-FLG
         WHEN    0      PERFORM   DSP-INIT-RTN
         WHEN    1      PERFORM   DSP-GRP01-RTN
         WHEN    2      PERFORM   DSP-GRP02-RTN
         WHEN    3      PERFORM   DSP-GRP03-RTN
         WHEN    4      PERFORM   DSP-KAKNIN-RTN
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-RTN            SECTION.
* 各ファイルをクローズする
     CLOSE              HMEIMS
                        ZSOKMS
                        DSPFILE.
*
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 DSP-INIT-RTN           SECTION.
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     MOVE     SPACE          TO   FVD05401.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FVD05401"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
* 処理区分の入力を行う
     MOVE     1              TO   SYR-FLG.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      日付区分　入力　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GRP01-RTN          SECTION.
     MOVE     G001           TO   GUIDE.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
* アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"   MOVE      0         TO   SYR-FLG
     WHEN     "F005"   MOVE     "END"      TO   END-FLG
     WHEN     "E000"   PERFORM   CHK-DTKBN-RTN
     WHEN     OTHER    MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      日付区分　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-DTKBN-RTN          SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     EVALUATE    DKBN
         WHEN    SPACE
*****            MOVE  NC"出庫倉庫"  TO  SOKSYU
                 MOVE  2             TO  SYR-FLG
         WHEN    "1"
*****            MOVE  NC"出庫倉庫"  TO  SOKSYU
                 MOVE  2             TO  SYR-FLG
         WHEN    "2"
*****            MOVE  NC"入庫倉庫"  TO  SOKSYU
                 MOVE  2             TO  SYR-FLG
         WHEN    OTHER
                 MOVE  2             TO  ERR-FLG
                 MOVE  "R"           TO  EDIT-OPTION OF DKBN
     END-EVALUATE.
 CHK-DTKBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００２　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GRP02-RTN       SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              PERFORM  CLR-HEAD-RTN
              MOVE      1         TO   SYR-FLG
     WHEN     "E000"
              PERFORM  CLR-HEAD-RTN
              PERFORM  CHK-GRP02-RTN
              IF     ERR-FLG   =  ZERO
                     MOVE   3     TO   SYR-FLG
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3
*--------------------------------------------------------------*
 CHK-GRP02-RTN       SECTION.
*
*倉庫ＣＤ
     IF  SOKCD       =   SPACE
         MOVE   NC"全倉庫"      TO  SOKNM
     ELSE
         MOVE   SOKCD           TO  SOK-F01
         READ   ZSOKMS
             INVALID  KEY
                 IF  ERR-FLG  =  ZERO
                     MOVE   3        TO  ERR-FLG
                 END-IF
                 MOVE  "R"      TO  EDIT-OPTION  OF  SOKCD
                 MOVE  "C"      TO  EDIT-CURSOR  OF  SOKCD
                 MOVE  SPACE    TO  SOKNM
             NOT  INVALID  KEY
                 MOVE  SOK-F02  TO  SOKNM
         END-READ
     END-IF.
     EVALUATE  DKBN
         WHEN  SPACE
               MOVE  NC"出庫倉庫"    TO  SOKSYU
         WHEN  "1"
               MOVE  NC"出庫倉庫"    TO  SOKSYU
         WHEN  "2"
               MOVE  NC"入庫倉庫"    TO  SOKSYU
          WHEN OTHER
               MOVE  SPACE           TO  SOKSYU
     END-EVALUATE.
*
 CHK-GRP02-EXT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-GRP03-RTN       SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP003"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              MOVE      2         TO   SYR-FLG
              PERFORM  CLR-HEAD-RTN
     WHEN     "E000"
              PERFORM  CLR-HEAD-RTN
              MOVE     SPACE      TO   BODY1
              PERFORM  CHK-GRP03-RTN
              IF  ERR-FLG   =   ZERO
                  PERFORM DNS-OPEN-RTN
                  PERFORM DNS-START-RTN
                  IF  DNS-FLG   =  ZERO
                      PERFORM DNS-WORK-RTN
                      MOVE   1        TO  P-CNT
                      PERFORM DNS-DSP-RTN
                      MOVE   4        TO  SYR-FLG
                  ELSE
                      MOVE   8        TO   ERR-FLG
                  END-IF
                  PERFORM DNS-CLOSE-RTN
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-GRP03-RTN      SECTION.
*
*開始日付
     IF  FDATE       NOT NUMERIC
         MOVE     ZERO                TO   FDATE
     END-IF.
     IF  FDATE       NOT =  ZERO
         MOVE     "2"                 TO   LINK-IN-KBN
         MOVE     FDATE               TO   LINK-IN-YMD8
         CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
         IF  LINK-OUT-RET      =  ZERO
             MOVE     FDATE           TO   WK-FDATE
         ELSE
             MOVE     ZERO            TO   WK-FDATE
             IF  ERR-FLG  =  ZERO
                 MOVE   4        TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION  OF  FDATE
             MOVE  "C"           TO  EDIT-CURSOR  OF  FDATE
         END-IF
     ELSE
         MOVE     ZERO           TO   WK-FDATE
     END-IF.
*終了日付
     IF  TDATE       NOT NUMERIC
         MOVE     ZERO                TO   TDATE
     END-IF.
     IF  TDATE       NOT =  ZERO      AND
         TDATE       NOT =  99999999
         MOVE     "2"                 TO   LINK-IN-KBN
         MOVE     TDATE               TO   LINK-IN-YMD8
         CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
         IF  LINK-OUT-RET   =  ZERO
             MOVE     TDATE           TO   WK-TDATE
         ELSE
             MOVE     ZERO            TO   WK-TDATE
             IF  ERR-FLG  =  ZERO
                 MOVE   4        TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION  OF  TDATE
             MOVE  "C"           TO  EDIT-CURSOR  OF  TDATE
         END-IF
     ELSE
         MOVE     99999999       TO   WK-TDATE
         MOVE     99999999       TO   TDATE
     END-IF.
*開始・終了の関連チェック
     IF  WK-FDATE  NOT =  ZERO  AND
         WK-TDATE  NOT =  ZERO  AND
         WK-FDATE     >   WK-TDATE
         IF  ERR-FLG  =  ZERO
             MOVE   4        TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  FDATE
                                 EDIT-OPTION  OF  TDATE
         MOVE  "C"           TO  EDIT-CURSOR  OF  FDATE
     END-IF.
*
 CHK-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KAKNIN-RTN    SECTION.
     MOVE      G003            TO   GUIDE.
     MOVE     "KAK001"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
* アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              PERFORM CLR-TAIL-RTN
              MOVE      3         TO   SYR-FLG
     WHEN     "F011"
              COMPUTE  C-CNT = P-CNT - 1
              IF  C-CNT = ZERO
                  MOVE  6             TO  ERR-FLG
              ELSE
                  COMPUTE  P-CNT = P-CNT - 1
                  PERFORM  DNS-DSP-RTN
                  MOVE      4         TO   SYR-FLG
              END-IF
     WHEN     "F012"
              COMPUTE  C-CNT = P-CNT + 1
              IF  C-CNT  >   MAX-CNT
                  MOVE  5             TO  ERR-FLG
              ELSE
                  COMPUTE  P-CNT = P-CNT + 1
                  PERFORM  DNS-DSP-RTN
                  MOVE      4         TO   SYR-FLG

              END-IF
     WHEN     "E000"
              MOVE      4         TO   SYR-FLG
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      存在チェック処理（伝票■）                  *
*--------------------------------------------------------------*
 CHK-DPNO-RTN           SECTION.
* 該当データを画面にセットする
     MOVE    HEAD1           TO   SAV-HEAD1.
     MOVE    SPACE           TO   FVD05401.
     MOVE    SAV-HEAD1       TO   HEAD1.
     PERFORM     DNS-DSP-RTN.
*
 CHK-DPNO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3         ＷＯＲＫへ読込 　　　　　　　　　　　　
*--------------------------------------------------------------*
 DNS-WORK-RTN           SECTION.
*表示データ読込
     MOVE     SPACE           TO   BODY1.
     MOVE     ZERO            TO   DNS-FLG.
     MOVE     ZERO            TO   OUT-CNT.
     INITIALIZE                       TABLE-G.
     PERFORM   VARYING  I  FROM  1  BY  1
               UNTIL    I  >  40  OR  DNS-FLG  =  1
            ADD   1           TO   OUT-CNT
            MOVE  DNS-F01     TO   TBL-DPNO-R   (I)
            MOVE  DNS-F02     TO   TBL-GYONO-R  (I)
            MOVE  DNS-F22     TO   TBL-RENKE-R  (I)
            MOVE  DNS-F03     TO   TBL-SYOCD-R  (I)
            MOVE  DNS-F04     TO   TBL-HINCD-R  (I)
            MOVE  DNS-F13     TO   TBL-SIJISU-R (I)
            MOVE  DNS-F25(1)  TO   TBL-STK1-R   (I)
            MOVE  DNS-F25(2)  TO   TBL-STK2-R   (I)
            MOVE  DNS-F25(3)  TO   TBL-STK3-R   (I)
            MOVE  DNS-F25(4)  TO   TBL-STK4-R   (I)
            MOVE  DNS-F25(5)  TO   TBL-STK5-R   (I)
            MOVE  DNS-F05     TO   TBL-SSOKCD-R (I)
            MOVE  DNS-F07     TO   TBL-STANA-R  (I)
            MOVE  DNS-F14     TO   TBL-SYKBI-R  (I)
            MOVE  DNS-F15     TO   TBL-SYKSU-R  (I)
            MOVE  DNS-F08     TO   TBL-NSOKCD-R (I)
            MOVE  DNS-F10     TO   TBL-NTANA-R  (I)
            MOVE  DNS-F18     TO   TBL-NYKBI-R  (I)
            MOVE  DNS-F19     TO   TBL-NYKSU-R  (I)
            PERFORM   DNS-READ-RTN
     END-PERFORM.
     COMPUTE  MAX-CNT   =  OUT-CNT  /  4  +  0.9.
 DNS-WORK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3         画面へ表示　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DNS-DSP-RTN            SECTION.
*データ表示
     MOVE     SPACE              TO   MEISAI(1)
                                      MEISAI(2)
                                      MEISAI(3)
                                      MEISAI(4)
     PERFORM   VARYING  I  FROM  1  BY  1
               UNTIL    I  >  4
        IF    TBL-DPNO(P-CNT I) NOT = ZERO
            MOVE  TBL-DPNO    (P-CNT I)  TO  DPNO     (I)
            MOVE  TBL-GYONO   (P-CNT I)  TO  GYONO    (I)
            IF    TBL-RENKE   (P-CNT I)  =   "1"
                  MOVE  NC"済"           TO  D101     (I)
            ELSE
                  MOVE  NC"未"           TO  D101     (I)
            END-IF
            MOVE  TBL-SYOCD   (P-CNT I)  TO  SYOCD    (I)
            MOVE  TBL-HIN1    (P-CNT I)  TO  HIN101   (I)
            MOVE  TBL-HIN2    (P-CNT I)  TO  HIN201   (I)
            MOVE  TBL-HIN3    (P-CNT I)  TO  HIN301   (I)
*           *商品名称取得
            MOVE   TBL-SYOCD  (P-CNT I)  TO  MEI-F011
            MOVE   TBL-HIN1   (P-CNT I)  TO  MEI-F0121
            MOVE   TBL-HIN2   (P-CNT I)  TO  MEI-F0122
            MOVE   TBL-HIN3   (P-CNT I)  TO  MEI-F0123
            PERFORM   MEI-READ-RTN
            MOVE  MEI-F021           TO  WK-HINMEI1
            MOVE  MEI-F022           TO  WK-HINMEI2
            MOVE  WK-HINMEI          TO  HINMEI(I)
            MOVE  TBL-SIJISU  (P-CNT I)  TO  SURYOU   (I)
            MOVE  TBL-STK1    (P-CNT I)  TO  STK101   (I)
            MOVE  TBL-STK2    (P-CNT I)  TO  STK201   (I)
            MOVE  TBL-STK3    (P-CNT I)  TO  STK301   (I)
            MOVE  TBL-STK4    (P-CNT I)  TO  STK401   (I)
            MOVE  TBL-STK5    (P-CNT I)  TO  STK501   (I)
            MOVE  TBL-SSOKCD  (P-CNT I)  TO  BA101    (I)
            MOVE  TBL-STAN1   (P-CNT I)  TO  TAN101   (I)
            MOVE  TBL-STAN2   (P-CNT I)  TO  TAN201   (I)
            MOVE  TBL-STAN3   (P-CNT I)  TO  TAN301   (I)
            MOVE  TBL-SYKBI   (P-CNT I)  TO  SYKYMD   (I)
            MOVE  TBL-SYKSU   (P-CNT I)  TO  SYKSU1   (I)
            MOVE  TBL-NSOKCD  (P-CNT I)  TO  B201     (I)
            MOVE  TBL-NTAN1   (P-CNT I)  TO  AIT101   (I)
            MOVE  TBL-NTAN2   (P-CNT I)  TO  AIT201   (I)
            MOVE  TBL-NTAN3   (P-CNT I)  TO  AIT301   (I)
            MOVE  TBL-NYKBI   (P-CNT I)  TO  NYKYMD   (I)
            MOVE  TBL-NYKSU   (P-CNT I)  TO  NYKSU    (I)
         END-IF
     END-PERFORM.
 DNS-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　　　　チェック　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-KAKNIN-RTN     SECTION.
     MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
 CHK-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      入出庫ファイルオープン　　　　　　　　　　　*
*--------------------------------------------------------------*
 DNS-OPEN-RTN        SECTION.
*ファイル ＯＰＥＮ
     EVALUATE  DKBN
         WHEN  SPACE
*指示日
               OPEN     INPUT       DNSFILF4
         WHEN  "1"
*出庫日
               OPEN     INPUT       DNSFILF7
         WHEN  "2"
*入庫日
               OPEN     INPUT       DNSFILF5
     END-EVALUATE.
     MOVE      ZERO     TO   DNS-FLG.
 DNS-OPEN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      入出庫表示開始処理　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DNS-START-RTN        SECTION.
*ファイル ＯＰＥＮ
     EVALUATE  DKBN
         WHEN  SPACE
*指示日
               PERFORM   DNS4-START-SEC
               IF        DNS-FLG            =   ZERO
                  PERFORM     DNS4-READ-SEC
                  MOVE   DNS4-REC           TO  DNS-REC
               END-IF
         WHEN  "1"
*出庫日
               PERFORM   DNS7-START-SEC
               IF        DNS-FLG            =   ZERO
                  PERFORM     DNS7-READ-SEC
                  MOVE   DNS7-REC           TO  DNS-REC
               END-IF
         WHEN  "2"
*入庫日
               PERFORM   DNS5-START-SEC
               IF        DNS-FLG            =   ZERO
                  PERFORM     DNS5-READ-SEC
                  MOVE   DNS5-REC           TO  DNS-REC
               END-IF
     END-EVALUATE.
 DNS-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      入出庫ファイル読込処理　　　　　　　　　　　*
*--------------------------------------------------------------*
 DNS-READ-RTN        SECTION.
*ファイル ＯＰＥＮ
     EVALUATE  DKBN
         WHEN  SPACE
*指示日
                PERFORM     DNS4-READ-SEC
                MOVE   DNS4-REC           TO  DNS-REC
         WHEN   "1"
*出庫日
                PERFORM     DNS7-READ-SEC
                MOVE   DNS7-REC           TO  DNS-REC
         WHEN   "2"
*入庫日
                PERFORM     DNS5-READ-SEC
                MOVE   DNS5-REC           TO  DNS-REC
     END-EVALUATE.
 DNS-READ-EXIT.
     EXIT.
*=============================================================
*    指示日      新入出庫Ｆ　ＳＴＡＲＴ処理
*=============================================================
 DNS4-START-SEC      SECTION.
     INITIALIZE                  DNS4-REC.
     IF    SOKCD      NOT =   SPACE
           MOVE  SOKCD    TO  DNS4-F05
     END-IF.
*
     START DNSFILF4  KEY >=      DNS4-F05  DNS4-F12
                                 DNS4-F01  DNS4-F02
         INVALID
           MOVE   1        TO  DNS-FLG
     END-START.
 DNS4-START-EXIT.
     EXIT.
*=============================================================
*    指示日      新入出庫Ｆ　ＲＥＡＤ処理
*=============================================================
 DNS4-READ-SEC    SECTION.
*リード
 DNS4-010.
     READ  DNSFILF4     NEXT
         AT   END
           MOVE   1        TO  DNS-FLG
           GO              TO  DNS4-READ-EXIT
     END-READ.
*カウント
*****ADD   1               TO         READ-CNT.
*倉庫ＣＤ判定
     IF   (SOKCD       NOT =   SPACE   )  AND
          (SOKCD       NOT =   DNS4-F05)
           MOVE   1        TO  DNS-FLG
           GO              TO  DNS4-READ-EXIT
     END-IF.
*指定日判定
     IF  (WK-FDATE         >   DNS4-F12  OR
          WK-TDATE         <   DNS4-F12)
           GO              TO  DNS4-010
     END-IF.
*
 DNS4-READ-EXIT.
     EXIT.
*=============================================================
*    入庫日      新入出庫Ｆ　ＳＴＡＲＴ処理
*=============================================================
 DNS5-START-SEC      SECTION.
     INITIALIZE                  DNS5-REC.
     IF    SOKCD         NOT =   SPACE
           MOVE       SOKCD  TO  DNS5-F08
     END-IF.
*
     START DNSFILF5  KEY >=      DNS5-F08  DNS5-F18
                                 DNS5-F01  DNS5-F02
         INVALID
           MOVE   1        TO  DNS-FLG
     END-START.
 DNS5-START-EXIT.
     EXIT.
*=============================================================
*    入庫日      新入出庫Ｆ　ＲＥＡＤ処理
*=============================================================
 DNS5-READ-SEC    SECTION.
*リード
 DNS5-010.
     READ  DNSFILF5     NEXT
         AT   END
           MOVE   1        TO  DNS-FLG
           GO              TO  DNS5-READ-EXIT
     END-READ.
*カウント
*****ADD   1               TO         READ-CNT.
*倉庫ＣＤ判定
     IF   (SOKCD       NOT =   SPACE   )  AND
          (SOKCD       NOT =   DNS5-F08)
           MOVE   1        TO  DNS-FLG
           GO              TO  DNS5-READ-EXIT
     END-IF.
*入庫日判定
     IF  (WK-FDATE         >   DNS5-F18  OR
          WK-TDATE         <   DNS5-F18)
           GO              TO  DNS5-010
     END-IF.
*
 DNS5-READ-EXIT.
     EXIT.
*=============================================================
*    出庫日      新入出庫Ｆ　ＳＴＡＲＴ処理
*=============================================================
 DNS7-START-SEC      SECTION.
     INITIALIZE                  DNS7-REC.
     IF    SOKCD         NOT =   SPACE
           MOVE       SOKCD  TO  DNS7-F05
     END-IF.
*
     START DNSFILF7  KEY >=      DNS7-F05  DNS7-F14
                                 DNS7-F01  DNS7-F02
         INVALID
           MOVE   1        TO  DNS-FLG
     END-START.
 DNS7-START-EXIT.
     EXIT.
*=============================================================
*    出庫日      新入出庫Ｆ　ＲＥＡＤ処理
*=============================================================
 DNS7-READ-SEC    SECTION.
*リード
 DNS7-010.
     READ  DNSFILF7     NEXT
         AT   END
           MOVE   1        TO  DNS-FLG
           GO              TO  DNS7-READ-EXIT
     END-READ.
*カウント
*****ADD   1               TO         READ-CNT.
*倉庫ＣＤ判定
     IF   (SOKCD       NOT =   SPACE   )  AND
          (SOKCD       NOT =   DNS7-F05)
           MOVE   1        TO  DNS-FLG
           GO              TO  DNS7-READ-EXIT
     END-IF.
*出庫日判定
     IF  (WK-FDATE         >   DNS7-F14  OR
          WK-TDATE         <   DNS7-F14)
           GO              TO  DNS7-010
     END-IF.
*
 DNS7-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      入出庫ファイルクロース　　　　　　　　　　　*
*--------------------------------------------------------------*
 DNS-CLOSE-RTN        SECTION.
*ファイル ＯＰＥＮ
     EVALUATE  DKBN
         WHEN  SPACE
*指示日
               CLOSE   DNSFILF4
         WHEN  "1"
*出庫日
               CLOSE   DNSFILF7
         WHEN  "2"
*入庫日
               CLOSE   DNSFILF5
     END-EVALUATE.
 DNS-CLOSE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 DSP-RD-RTN           SECTION.
*
     IF       ERR-FLG  =  0
              MOVE      SPACE               TO   MESAGE
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MESAGE
     END-IF.
     MOVE     "ALLF"         TO   DSP-GRP.
     PERFORM  DSP-WRITE-RTN.
*
     IF       ERR-FLG  NOT  =  0
              MOVE      "AL"           TO   DSP-PRO
              MOVE      0              TO   ERR-FLG
     ELSE
              MOVE      "NE"           TO   DSP-PRO
     END-IF.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 DSP-RD-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 DSP-WRITE-RTN          SECTION.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
*
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FVD05401.
 DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品名称マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 MEI-READ-RTN       SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HMEIMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   倉庫マスタ　　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SOK-READ-RTN         SECTION.
     MOVE     0         TO   INV-FLG.
     READ     ZSOKMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF DKBN
                                  EDIT-CURSOR OF SOKCD
                                  EDIT-CURSOR OF FDATE
                                  EDIT-CURSOR OF TDATE.
     MOVE     "M"            TO   EDIT-OPTION OF DKBN
                                  EDIT-OPTION OF SOKCD
                                  EDIT-OPTION OF FDATE
                                  EDIT-OPTION OF TDATE.
*
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KAKNIN.
     MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
