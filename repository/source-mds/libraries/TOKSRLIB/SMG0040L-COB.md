# SMG0040L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SMG0040L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　プログラム実行結果                *
*    モジュール名　　　　：　プログラム実行結果リスト          *
*    作成日／作成者　　　：　2001/04/17 T.TAKAHASHI            *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SMG0040L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/04/17.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*プリント定義ファイル
     SELECT     PRTFILE    ASSIGN    TO        GS-PRTF
                PROCESSING MODE      IS        PRT-PROC
                GROUP                IS        PRT-GROUP
                FORMAT               IS        PRT-FORMAT
                CONTROL              IS        PRT-CNTL
                FILE STATUS          IS        PRT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
     COPY        FMG00401  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  PG-ID
 01  WK-PGID                 PIC X(08)  VALUE  "SMG0040L".
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  PRT-ST              PIC X(02).
*帳票制御用領域
 01  PRT-CONTROL.
     03  PRT-PROC            PIC  X(02).
     03  PRT-GROUP           PIC  X(08).
     03  PRT-FORMAT          PIC  X(08).
     03  PRT-CNTL            PIC  X(06).
*
 01  FILE-ERR.
     03  PRT-ERR             PIC N(15) VALUE
                        NC"プリンターエラー".
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
 01  LINK-IN-KBN                  PIC X(01).
 01  LINK-IN-YMD6                 PIC 9(06).
 01  LINK-IN-YMD8                 PIC 9(08).
 01  LINK-OUT-RET                 PIC X(01).
 01  LINK-OUT-YMD                 PIC 9(08).
*メッセージ編集
 01  WK-HENKAN.
     03  HEN-KEKA                 PIC  N(04).
     03  HEN-PGNM                 PIC  N(20).
     03  HEN-KEKA1                PIC  N(20).
     03  HEN-KEKA2                PIC  N(20).
     03  HEN-KEKA3                PIC  N(20).
     03  HEN-KEKA4                PIC  N(20).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-KEKA                    PIC  X(01).
 01  LINK-PGNM                    PIC  N(20).
 01  LINK-KEKA1                   PIC  N(20).
 01  LINK-KEKA2                   PIC  N(20).
 01  LINK-KEKA3                   PIC  N(20).
 01  LINK-KEKA4                   PIC  N(20).
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LINK-KEKA
                                  LINK-PGNM
                                  LINK-KEKA1
                                  LINK-KEKA2
                                  LINK-KEKA3
                                  LINK-KEKA4.
 DECLARATIVES.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 MAIN-SEC                  SECTION.
*ファイルＯＰＥＮ
     OPEN        OUTPUT      PRTFILE.
     MOVE        SPACE       TO   PRT-FMG00401.
*日本語ワーク初期化
     MOVE        SPACE       TO   WK-HENKAN.
*ＰＧＩＤセット
     MOVE     WK-PGID             TO   PRT-PGID.
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
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
     MOVE      HEN-DATE           TO   PRT-SDATE.
*
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
     MOVE      HEN-TIME           TO   PRT-STIME.
***  処理名称／処理結果セット
     EVALUATE  LINK-KEKA
         WHEN  "1"   MOVE NC"正常終了"  TO   HEN-KEKA
         WHEN  "2"   MOVE NC"異常終了"  TO   HEN-KEKA
         WHEN  "3"   MOVE NC"ＥＲＲ有"  TO   HEN-KEKA
         WHEN  "4"   MOVE NC"ＥＲＲ無"  TO   HEN-KEKA
         WHEN  OTHER MOVE NC"＊＊＊＊"  TO   HEN-KEKA
     END-EVALUATE.
     MOVE      LINK-PGNM          TO   HEN-PGNM.
     MOVE      LINK-KEKA1         TO   HEN-KEKA1.
     MOVE      LINK-KEKA2         TO   HEN-KEKA2.
     MOVE      LINK-KEKA3         TO   HEN-KEKA3.
     MOVE      LINK-KEKA4         TO   HEN-KEKA4.
     MOVE      HEN-KEKA           TO   PRT-KEKA.
     MOVE      HEN-PGNM           TO   PRT-PGNM.
     MOVE      HEN-KEKA1          TO   PRT-KEKA1.
     MOVE      HEN-KEKA2          TO   PRT-KEKA2.
     MOVE      HEN-KEKA3          TO   PRT-KEKA3.
     MOVE      HEN-KEKA4          TO   PRT-KEKA4.
*****DISPLAY "LINK-PGNM = " LINK-PGNM  UPON  CONS.
*プリント出力
     MOVE        SPACE       TO   PRT-CONTROL.
     MOVE        "FMG00401"  TO   PRT-FORMAT.
     MOVE        "SCREEN"    TO   PRT-GROUP.
     WRITE       PRT-FMG00401.
*終了処理
     CLOSE       PRTFILE.
*
 MAIN-SEC-EXIT.
     EXIT.

```
