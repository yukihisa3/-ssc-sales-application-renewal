# NKE0900L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0900L.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　検品       　　　　　　　
*    サブシステム　　　　：　検品システム連携
*    モジュール名　　　　：　データ連携件数リスト（共通）
*    作成日／作成者　　　：　2018/12/20  INOUE
*    処理概要　　　　　　：　受け取ったパラメタに応じて　　
*    　　　　　　　　　　：  件数リストを出力する
*    更新履歴　　　　　　：  2023/01/18  INOUE
*    　　　　　　　　　　：  受取パラメタ追加(8)
*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               NKE0900L.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2018/12/20.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     YA          IS        NIHONGO
     YA-22       IS        YA-22
     YB          IS        YB
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 担当者マスタ >>--*
     SELECT     TANMS1     ASSIGN         DA-01-VI-TANMS1
                           ORGANIZATION   INDEXED
                           ACCESS    MODE RANDOM
                           RECORD    KEY  TAN-F01 TAN-F02
                           STATUS         TAN-ST.
* プリンター *
     SELECT     PRTF       ASSIGN    TO   LP-04.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*----<< 担当者マスタ >>--*
 FD    TANMS1    LABEL RECORD   IS   STANDARD.
       COPY      TANMS1         OF   XFDLIB
                 JOINING        TAN  PREFIX.
*----<< プリンター >>--*
 FD    PRTF      LINAGE         IS   66.
 01    P-REC                    PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  TAN-ST            PIC X(02).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
*年度
 01  YYYY.
     03  YY1               PIC  9(02).
     03  YY2               PIC  9(02).
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-NYUKIN         PIC 9(02) VALUE     ZERO.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
*帳票表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*帳票表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*退避エリア
 01  WK-BMNCD-BK           PIC  X(04).
 01  WK-TANCD-BK           PIC  X(02).
 01  WK-TANNM-BK           PIC  N(10).
 01  WK-TORCD-BK           PIC  X(08).
 01  WK-TORNM-BK           PIC  N(15).
*変換エリア
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"０１２３４５６７８９　".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.

 01  WK-KENSU-X.
     03  WK-KENSU          PIC  9(07) VALUE  ZERO.
 01  WK-HENKAN.
     03  WK-HENKAN-N       PIC  N(07).
     03  WK-HENKAN-NR  REDEFINES  WK-HENKAN-N
                           PIC  N(01)  OCCURS 7.
 01  WK-SHUKA-X.
     03  WK-SHUKA-G        PIC  9(08) VALUE  ZERO.
 01  WK-NYUKA-X.
     03  WK-NYUKA-G        PIC  9(08) VALUE  ZERO.
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 01  IX                    PIC  9(04) VALUE 0.
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  IN-CNT                PIC  9(07) VALUE 0.
 01  FG-ZSOKMS-INV         PIC  X(03) VALUE SPACE.
 01  FG-HTANMS-INV         PIC  X(03) VALUE SPACE.
 01  SET-FLG               PIC  X(03) VALUE SPACE.
 01  HENKAN-FLG            PIC  X(03) VALUE SPACE.
*帳票出力定義エリア
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "NKE0900L".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(15)
*        CHARACTER  TYPE  IS  YB-22      VALUE
         CHARACTER  TYPE  IS  YA-22      VALUE
         NC"＜【ホスト】連携データ件数表＞".
     02  FILLER              PIC  X(24)  VALUE  SPACE.
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"年".
     02  SYSMM               PIC  Z9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"月".
     02  SYSDD               PIC  Z9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"日".
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  LPAGE               PIC  ZZ9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"頁".
****  見出し行２***
 01  MIDASI-2.
*    02  FILLER              PIC  X(39)  VALUE  SPACE.
*    02  LIST-KUBUN          PIC  N(10)
*        CHARACTER  TYPE  IS  YB-22      VALUE  SPACE.
*    02  FILLER              PIC  X(32)  VALUE  SPACE.
     02  FILLER              PIC  X(116) VALUE  SPACE.
     02  TIMEHH              PIC  99.
     02  FILLER              PIC  N(01)  VALUE  NC"："
                             CHARACTER  TYPE  IS  NIHONGO.
     02  TIMEMM              PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"："
                             CHARACTER  TYPE  IS  NIHONGO.
     02  TIMESS              PIC  Z9.
 01  MIDASI-3.
     02  FILLER              PIC  X(38)   VALUE  SPACE.
     02  FILLER              PIC  N(06)
         CHARACTER  TYPE     IS   NIHONGO
         VALUE NC"連携担当者：".
     02  LIST-TANCD          PIC  X(07)   VALUE  SPACE.
     02  FILLER              PIC  X(01)   VALUE  SPACE.
     02  LIST-TANNM          PIC  N(10)
         CHARACTER  TYPE     IS   NIHONGO VALUE  SPACE.
****  線１           ****
 01  HASEN-1.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
****  線２           ****
 01  HASEN-2.
     02  FILLER              PIC  X(27) VALUE  SPACE.
     02  FILLER              PIC  X(34) VALUE  ALL "-".

****  明細行0               ****
 01  MEISAI-0.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(04)
*        CHARACTER  TYPE  IS YB-22      VALUE  NC"処理名：".
         CHARACTER  TYPE  IS YA-22      VALUE  NC"処理名：".
     02  MEISAI-0-SYORINM    PIC  N(10)
*        CHARACTER  TYPE  IS YB-22      VALUE  SPACE.
         CHARACTER  TYPE  IS YA-22      VALUE  SPACE.
 01  MEISAI-00.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(04)
*        CHARACTER  TYPE  IS YB-22      VALUE  NC"モード：".
         CHARACTER  TYPE  IS YA-22      VALUE  NC"モード：".
     02  MEISAI-00-MODENM    PIC  N(10)
*        CHARACTER  TYPE  IS YB-22      VALUE  SPACE.
         CHARACTER  TYPE  IS YA-22      VALUE  SPACE.
 01  MEISAI-000.
     02  FILLER              PIC  X(31) VALUE  SPACE.
     02  FILLER              PIC  N(06)
*        CHARACTER  TYPE  IS YB-22      VALUE  NC"＜連携件数＞".
         CHARACTER  TYPE  IS YA-22      VALUE  NC"＜連携件数＞".
****  明細行1               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(07) VALUE
*    NC"出荷指示件数："      CHARACTER TYPE IS YB-22.
     NC"出荷指示件数："      CHARACTER TYPE IS YA-22.
*    02  MEI1                PIC  N(07) CHARACTER TYPE IS YB-22.
     02  MEI1                PIC  N(07) CHARACTER TYPE IS YA-22.
     02  FILLER              PIC  N(01) VALUE
*        NC"件"           CHARACTER TYPE IS YB-22.
         NC"件"           CHARACTER TYPE IS YA-22.
****  明細行2               ****
 01  MEISAI-2.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(07) VALUE
*    NC"出荷確定件数："      CHARACTER TYPE IS YB-22.
     NC"出荷確定件数："      CHARACTER TYPE IS YA-22.
*    02  MEI2                PIC  N(07) CHARACTER TYPE IS YB-22.
     02  MEI2                PIC  N(07) CHARACTER TYPE IS YA-22.
     02  FILLER              PIC  N(01) VALUE
*        NC"件"           CHARACTER TYPE IS YB-22.
         NC"件"           CHARACTER TYPE IS YA-22.
****  明細行3               ****
 01  MEISAI-3.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(07) VALUE
*    NC"入荷指示件数："      CHARACTER TYPE IS YB-22.
     NC"入荷指示件数："      CHARACTER TYPE IS YA-22.
*    02  MEI3                PIC  N(07) CHARACTER TYPE IS YB-22.
     02  MEI3                PIC  N(07) CHARACTER TYPE IS YA-22.
     02  FILLER              PIC  N(01) VALUE
*        NC"件"           CHARACTER TYPE IS YB-22.
         NC"件"           CHARACTER TYPE IS YA-22.
****  明細行4               ****
 01  MEISAI-4.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(07) VALUE
*    NC"入荷確定件数："      CHARACTER TYPE IS YB-22.
     NC"入荷確定件数："      CHARACTER TYPE IS YA-22.
*    02  MEI4                PIC  N(07) CHARACTER TYPE IS YB-22.
     02  MEI4                PIC  N(07) CHARACTER TYPE IS YA-22.
     02  FILLER              PIC  N(01) VALUE
*        NC"件"           CHARACTER TYPE IS YB-22.
         NC"件"           CHARACTER TYPE IS YA-22.
****  明細行5               ****
 01  MEISAI-5.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(07) VALUE
*    NC"返品確定件数："      CHARACTER TYPE IS YB-22.
     NC"返品確定件数："      CHARACTER TYPE IS YA-22.
*    02  MEI5                PIC  N(07) CHARACTER TYPE IS YB-22.
     02  MEI5                PIC  N(07) CHARACTER TYPE IS YA-22.
     02  FILLER              PIC  N(01) VALUE
*        NC"件"           CHARACTER TYPE IS YB-22.
         NC"件"           CHARACTER TYPE IS YA-22.
****  明細行6               ****
 01  MEISAI-6.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(07) VALUE
*    NC"棚卸指示件数："      CHARACTER TYPE IS YB-22.
     NC"棚卸指示件数："      CHARACTER TYPE IS YA-22.
*    02  MEI6                PIC  N(07) CHARACTER TYPE IS YB-22.
     02  MEI6                PIC  N(07) CHARACTER TYPE IS YA-22.
     02  FILLER              PIC  N(01) VALUE
*        NC"件"           CHARACTER TYPE IS YB-22.
         NC"件"           CHARACTER TYPE IS YA-22.
****  明細行7               ****
 01  MEISAI-7.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(07) VALUE
*    NC"棚卸確定件数："      CHARACTER TYPE IS YB-22.
     NC"棚卸確定件数："      CHARACTER TYPE IS YA-22.
*    02  MEI7                PIC  N(07) CHARACTER TYPE IS YB-22.
     02  MEI7                PIC  N(07) CHARACTER TYPE IS YA-22.
     02  FILLER              PIC  N(01) VALUE
*        NC"件"           CHARACTER TYPE IS YB-22.
         NC"件"           CHARACTER TYPE IS YA-22.
*
****  明細行8               ****
 01  MEISAI-8.
     02  FILLER              PIC  X(35) VALUE  SPACE.
     02  FILLER              PIC  N(07) VALUE
     NC"受注件数　　："      CHARACTER TYPE IS YA-22.
     02  MEI8                PIC  N(07) CHARACTER TYPE IS YA-22.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YA-22.
*
 LINKAGE                   SECTION.
 01  PARA-IN-BUMON         PIC X(04).
 01  PARA-IN-TANTOU        PIC X(02).
 01  PARA-IN-SYORI-KUBUN   PIC X(01).
 01  PARA-IN-KENSU         PIC 9(07).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PARA-IN-BUMON
                                          PARA-IN-TANTOU
                                          PARA-IN-SYORI-KUBUN
                                          PARA-IN-KENSU.
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC.
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
     MOVE        61        TO        LINE-CNT.
*
     DISPLAY "PARA-IN-BUMON       = " PARA-IN-BUMON  UPON CONS.
     DISPLAY "PARA-IN-TANTOU      = " PARA-IN-TANTOU UPON CONS.
     DISPLAY "PARA-IN-SYORI-KUBUN = " PARA-IN-SYORI-KUBUN
             UPON CONS.
     DISPLAY "PARA-IN-KENSU       = " PARA-IN-KENSU  UPON CONS.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     OPEN      INPUT     TANMS1.
     OPEN      OUTPUT    PRTF.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
     PERFORM     HEAD-SET-SEC.
     PERFORM     MEIEDT-SEC.
     PERFORM     MIDA-SEC.
     PERFORM     MEIWRT-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE     TANMS1  PRTF.
 END-EXIT.
     EXIT.
**********************************************************
*                 ヘッダデータ編集                      *
**********************************************************
 HEAD-SET-SEC                  SECTION.
*
     MOVE        WK-YS     TO        YY1.
     MOVE        WK-Y      TO        YY2.
     MOVE        YYYY      TO        SYSYY.
     MOVE        WK-M      TO        SYSMM.
     MOVE        WK-D      TO        SYSDD.
     MOVE        WK-TIME(1:2)  TO    TIMEHH.
     MOVE        WK-TIME(3:2)  TO    TIMEMM.
     MOVE        WK-TIME(5:2)  TO    TIMESS.
*
*連携担当者
     MOVE        PARA-IN-BUMON       TO    LIST-TANCD(1:4)
                                           TAN-F01.
     MOVE        "-"                 TO    LIST-TANCD(5:1)
     MOVE        PARA-IN-TANTOU      TO    LIST-TANCD(6:2)
                                           TAN-F02.
     READ        TANMS1
         INVALID
                 MOVE  ALL NC"？"    TO    LIST-TANNM
         NOT INVALID
                 MOVE  TAN-F03       TO    LIST-TANNM
     END-READ.
*
 HEAD-SET-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
*
     IF  PAGE-CNT  >  ZERO
         MOVE       SPACE   TO      P-REC
         WRITE      P-REC   AFTER   PAGE
     END-IF.
*
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       SPACE   TO  P-REC.
     WRITE      P-REC   AFTER  2.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-3   AFTER  1.
     WRITE      P-REC   FROM    HASEN-1    AFTER  2.
*
*    ADD         10        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*件数
     MOVE    PARA-IN-KENSU TO  WK-KENSU.
     PERFORM ZENKAKU-SEC.
*処理名・モード
     EVALUATE    PARA-IN-SYORI-KUBUN
      WHEN "1"
            MOVE  NC"出荷指示データ　　　"  TO  MEISAI-0-SYORINM
            MOVE  NC"基幹→検品システム　"  TO  MEISAI-00-MODENM
            MOVE  WK-HENKAN-N               TO  MEI1
      WHEN "2"
            MOVE  NC"出荷確定データ　　　"  TO  MEISAI-0-SYORINM
            MOVE  NC"検品システム→基幹　"  TO  MEISAI-00-MODENM
            MOVE  WK-HENKAN-N               TO  MEI2
      WHEN "3"
            MOVE  NC"入荷予定データ　　　"  TO  MEISAI-0-SYORINM
            MOVE  NC"基幹→検品システム　"  TO  MEISAI-00-MODENM
            MOVE  WK-HENKAN-N               TO  MEI3
      WHEN "4"
            MOVE  NC"入荷確定データ　　　"  TO  MEISAI-0-SYORINM
            MOVE  NC"検品システム→基幹　"  TO  MEISAI-00-MODENM
            MOVE  WK-HENKAN-N               TO  MEI4
      WHEN "5"
            MOVE  NC"返品確定データ　　　"  TO  MEISAI-0-SYORINM
            MOVE  NC"検品システム→基幹　"  TO  MEISAI-00-MODENM
            MOVE  WK-HENKAN-N               TO  MEI5
      WHEN "6"
            MOVE  NC"棚卸予定データ　　　"  TO  MEISAI-0-SYORINM
            MOVE  NC"基幹→検品システム　"  TO  MEISAI-00-MODENM
            MOVE  WK-HENKAN-N               TO  MEI6
      WHEN "7"
            MOVE  NC"棚卸確定データ　　　"  TO  MEISAI-0-SYORINM
            MOVE  NC"検品システム→基幹　"  TO  MEISAI-00-MODENM
            MOVE  WK-HENKAN-N               TO  MEI7
      WHEN "8"
            MOVE  NC"受注データ　　　　　"  TO  MEISAI-0-SYORINM
            MOVE  NC"基幹→検品システム　"  TO  MEISAI-00-MODENM
            MOVE  WK-HENKAN-N               TO  MEI8
      WHEN OTHER
            MOVE  NC"（？処理区分不明？）"  TO  MEISAI-0-SYORINM
            MOVE  NC"　　　　　　　　　　"  TO  MEISAI-00-MODENM
     END-EVALUATE.
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
*
*    ADD        1          TO        IN-CNT.
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    MEISAI-0   AFTER  3.
     WRITE      P-REC   FROM    MEISAI-00  AFTER  3.
     WRITE      P-REC   FROM    MEISAI-000 AFTER  3.
     WRITE      P-REC   FROM    MEISAI-8   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-1   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-2   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-3   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-4   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-5   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-6   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-7   AFTER  2.
*    ADD         4         TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
****************************************************************
*    全角変換　　　                                          *
****************************************************************
 ZENKAKU-SEC          SECTION.
* 日本語変換する。
     MOVE   SPACE  TO  HENKAN-FLG.
     PERFORM  VARYING IX  FROM 1 BY 1 UNTIL   IX > 7
     IF ( WK-KENSU-X(IX:1) NOT = 0 ) AND
        ( WK-KENSU-X(IX:1) NOT = SPACE )
         MOVE "SET"   TO  HENKAN-FLG
     END-IF
         EVALUATE    WK-KENSU-X(IX:1)
             WHEN    0
                 IF HENKAN-FLG = "SET"
                     MOVE NC"０"  TO   WK-HENKAN-NR(IX)
                 ELSE
                     MOVE NC"　"  TO   WK-HENKAN-NR(IX)
                 END-IF
             WHEN    1
                 MOVE NC"１"  TO   WK-HENKAN-NR(IX)
             WHEN    2
                 MOVE NC"２"  TO   WK-HENKAN-NR(IX)
             WHEN    3
                 MOVE NC"３"  TO   WK-HENKAN-NR(IX)
             WHEN    4
                 MOVE NC"４"  TO   WK-HENKAN-NR(IX)
             WHEN    5
                 MOVE NC"５"  TO   WK-HENKAN-NR(IX)
             WHEN    6
                 MOVE NC"６"  TO   WK-HENKAN-NR(IX)
             WHEN    7
                 MOVE NC"７"  TO   WK-HENKAN-NR(IX)
             WHEN    8
                 MOVE NC"８"  TO   WK-HENKAN-NR(IX)
             WHEN    9
                 MOVE NC"９"  TO   WK-HENKAN-NR(IX)
         END-EVALUATE
     END-PERFORM.
 ZENKAKU-EXIT.
     EXIT.

```
