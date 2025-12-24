# SBI0060L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBI0060L.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　BIツール連携
*    業務名　　　　　　　：　           　　　　　　　
*    モジュール名　　　　：　BI連携用抽出データ件数表
*    作成日／作成者　　　：　2018/12/11  INOUE
*    処理概要　　　　　　：　受け取ったパラメタに応じて　　
*    　　　　　　　　　　：  件数表を出力する
*    更新履歴　　　　　　：
*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SBI0060L.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2018/12/11.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     YA          IS        NIHONGO
     YB          IS        YB
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
* プリンター *
     SELECT     PRTF       ASSIGN    TO        LP-04.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*プリンター
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
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
     02  FILLER              PIC  X(08)  VALUE  "SBI0060L".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(15)
         CHARACTER  TYPE  IS  YB-22      VALUE
         NC"＜ＢＩ連携用　　データ件数表＞".
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
     02  FILLER              PIC  X(39)  VALUE  SPACE.
     02  LIST-KUBUN          PIC  N(10)
         CHARACTER  TYPE  IS  YB-22      VALUE  SPACE.
     02  FILLER              PIC  X(32)  VALUE  SPACE.
     02  TIMEHH              PIC  99.
     02  FILLER              PIC  N(01)  VALUE  NC"："
                             CHARACTER  TYPE  IS  NIHONGO.
     02  TIMEMM              PIC  Z9.
     02  FILLER              PIC  N(01)  VALUE  NC"："
                             CHARACTER  TYPE  IS  NIHONGO.
     02  TIMESS              PIC  Z9.
****  線１           ****
 01  HASEN-1.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
****  線２           ****
 01  HASEN-2.
     02  FILLER              PIC  X(27) VALUE  SPACE.
     02  FILLER              PIC  X(34) VALUE  ALL "-".

****  明細行0               ****
 01  MEISAI-0.
     02  FILLER              PIC  X(37)  VALUE  SPACE.
     02  MEISAI-0-MEISYO1    PIC  N(02)
         CHARACTER  TYPE  IS YB          VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS YB          VALUE  NC"データ名".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  MEISAI-0-MEISYO2    PIC  N(02)
         CHARACTER  TYPE  IS YB          VALUE  SPACE.
     02  FILLER              PIC  N(05)
         CHARACTER  TYPE  IS YB          VALUE  NC"データ件数".
****  明細行1               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(37) VALUE  SPACE.
     02  FILLER              PIC  N(12) VALUE
     NC"１．日次計上分データ　　"       CHARACTER TYPE IS YB.
     02  FILLER              PIC  X(10) VALUE  SPACE.
     02  MEI1                PIC  N(07) CHARACTER TYPE IS YB.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB.
****  明細行2               ****
 01  MEISAI-2.
     02  FILLER              PIC  X(37) VALUE  SPACE.
     02  FILLER              PIC  N(12) VALUE
     NC"２．日次未計上分データ　"       CHARACTER TYPE IS YB.
     02  FILLER              PIC  X(10) VALUE  SPACE.
     02  MEI2                PIC  N(07) CHARACTER TYPE IS YB.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB.
****  明細行3               ****
 01  MEISAI-3.
     02  FILLER              PIC  X(37) VALUE  SPACE.
     02  FILLER              PIC  N(12) VALUE
     NC"３．日次振替データ　　　"       CHARACTER TYPE IS YB.
     02  FILLER              PIC  X(10) VALUE  SPACE.
     02  MEI3                PIC  N(07) CHARACTER TYPE IS YB.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB.
****  明細行4               ****
 01  MEISAI-4.
     02  FILLER              PIC  X(37) VALUE  SPACE.
     02  FILLER              PIC  N(12) VALUE
     NC"４．商品変換ＴＢＬデータ"       CHARACTER TYPE IS YB.
     02  FILLER              PIC  X(10) VALUE  SPACE.
     02  MEI4                PIC  N(07) CHARACTER TYPE IS YB.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB.
****  明細行5               ****
 01  MEISAI-5.
     02  FILLER              PIC  X(37) VALUE  SPACE.
     02  FILLER              PIC  N(12) VALUE
     NC"５．商品名称マスタデータ"       CHARACTER TYPE IS YB.
     02  FILLER              PIC  X(10) VALUE  SPACE.
     02  MEI5                PIC  N(07) CHARACTER TYPE IS YB.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB.
****  明細行6               ****
 01  MEISAI-6.
     02  FILLER              PIC  X(37) VALUE  SPACE.
     02  FILLER              PIC  N(12) VALUE
     NC"６．在庫データ　　　　　"       CHARACTER TYPE IS YB.
     02  FILLER              PIC  X(10) VALUE  SPACE.
     02  MEI6                PIC  N(07) CHARACTER TYPE IS YB.
     02  FILLER              PIC  N(01) VALUE
         NC"件"           CHARACTER TYPE IS YB.
*
 LINKAGE                   SECTION.
 01  PAR-IN-LIST-KUBUN    PIC X(01).
 01  PAR-IN-KENSU1        PIC 9(06).
 01  PAR-IN-KENSU2        PIC 9(06).
 01  PAR-IN-KENSU3        PIC 9(06).
 01  PAR-IN-KENSU4        PIC 9(06).
 01  PAR-IN-KENSU5        PIC 9(06).
 01  PAR-IN-KENSU6        PIC 9(06).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PAR-IN-LIST-KUBUN
                                          PAR-IN-KENSU1
                                          PAR-IN-KENSU2
                                          PAR-IN-KENSU3
                                          PAR-IN-KENSU4
                                          PAR-IN-KENSU5
                                          PAR-IN-KENSU6.
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
     CLOSE     PRTF.
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
     EVALUATE    PAR-IN-LIST-KUBUN
          WHEN "1"
                 MOVE  NC"（　データ抽出時　）"  TO  LIST-KUBUN
          WHEN "2"
                 MOVE  NC"（　ＢＩ連携時　　）"  TO  LIST-KUBUN
          WHEN OTHER
                 MOVE  NC"（？帳票区分不明？）"  TO  LIST-KUBUN
     END-EVALUATE.
*
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
     WRITE      P-REC   FROM    MIDASI-2   AFTER  2.
     WRITE      P-REC   FROM    HASEN-1    AFTER  1.
*
*    ADD         10        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*処理モード
     EVALUATE    PAR-IN-LIST-KUBUN
          WHEN "1"
                 MOVE   NC"抽出"           TO   MEISAI-0-MEISYO1
                                                MEISAI-0-MEISYO2
          WHEN "2"
                 MOVE   NC"連携"           TO   MEISAI-0-MEISYO1
                                                MEISAI-0-MEISYO2
          WHEN OTHER
                 MOVE   NC"？？"           TO   MEISAI-0-MEISYO1
                                                MEISAI-0-MEISYO2
     END-EVALUATE.
*件数1行目
     MOVE    PAR-IN-KENSU1 TO  WK-KENSU.
     PERFORM ZENKAKU-SEC.
     MOVE    WK-HENKAN-N   TO  MEI1.
*件数2行目
     MOVE    PAR-IN-KENSU2 TO  WK-KENSU.
     PERFORM ZENKAKU-SEC.
     MOVE    WK-HENKAN-N   TO  MEI2.
*件数3行目
     MOVE    PAR-IN-KENSU3 TO  WK-KENSU.
     PERFORM ZENKAKU-SEC.
     MOVE    WK-HENKAN-N   TO  MEI3.
*件数4行目
     MOVE    PAR-IN-KENSU4 TO  WK-KENSU.
     PERFORM ZENKAKU-SEC.
     MOVE    WK-HENKAN-N   TO  MEI4.
*件数5行目
     MOVE    PAR-IN-KENSU5 TO  WK-KENSU.
     PERFORM ZENKAKU-SEC.
     MOVE    WK-HENKAN-N   TO  MEI5.
*件数１行目
     MOVE    PAR-IN-KENSU6 TO  WK-KENSU.
     PERFORM ZENKAKU-SEC.
     MOVE    WK-HENKAN-N   TO  MEI6.
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
     WRITE      P-REC   FROM    MEISAI-1   AFTER  3.
     WRITE      P-REC   FROM    MEISAI-2   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-3   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-4   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-5   AFTER  2.
     WRITE      P-REC   FROM    MEISAI-6   AFTER  2.
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
