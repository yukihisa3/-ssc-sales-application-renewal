# DCM0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/DCM0030B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭ発注業務                    *
*    サブシステム　　　　：　ＤＣＭ　　　　　　　　　　　　　　*
*    モジュール名　　　　：　実行履歴用日付時刻取得            *
*    作成日／更新日　　　：　2022/05/18                        *
*    作成者／更新者　　　：　NAV INOUE                         *
*    処理概要　　　　　　：　システム日付・時刻をパラメタ出力　*
*                                                              *
****************************************************************
**履歴**********************************************************
*    2022/05/18  井上　　新規作成（DCM0030B.TOKSRLIB)を流用
*    0000/00/00  　　　　　　　　　　　　　　　　　　　　　　　
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           DCM0030B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2022/05/18.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*区分、ＦＬＧエリア
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  GO-FLG                   PIC  X(02)  VALUE  SPACE.
*システム日付格納
 01  SYS-DATE                     PIC  9(06)  VALUE  ZERO.
*システム時間格納
 01  WK-TIME.
     03  WK-TIME-1                PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-2                PIC  9(04)  VALUE  ZERO.
 01  WK-TIME-R   REDEFINES   WK-TIME.
     03  WK-TIME-R1               PIC  9(06).
     03  WK-TIME-R2               PIC  9(02).
*時間編集領域
 01  WK-DATE.
     03  WK-YYYY                  PIC  9(04)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
     03  WK-DD                    PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 LINKAGE               SECTION.
 01  LINK-HIDUKE           PIC 9(08).
 01  LINK-JIKAN6           PIC 9(06).
 01  LINK-JIKAN4           PIC 9(04).
**************************************************************
**************************************************************
 PROCEDURE             DIVISION
                       USING  LINK-HIDUKE
                              LINK-JIKAN6
                              LINK-JIKAN4.
**************************************************************
 DECLARATIVES.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
*システム日付取得／時刻取得
     ACCEPT    SYS-DATE    FROM    DATE.
     ACCEPT    WK-TIME     FROM    TIME.
*システム日付６桁→８桁変換（サブ日付チェック／変換）
 INIT010.
     MOVE      "3"         TO      LINK-IN-KBN.
     MOVE      SYS-DATE    TO      LINK-IN-YMD6.
     MOVE      ZERO        TO      LINK-IN-YMD8.
     MOVE      ZERO        TO      LINK-OUT-RET.
     MOVE      ZERO        TO      LINK-OUT-YMD.
     CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD TO     WK-DATE.
     MOVE      WK-DATE      TO     LINK-HIDUKE.
     MOVE      WK-TIME-R1   TO     LINK-JIKAN6.
     MOVE      WK-TIME-1    TO     LINK-JIKAN4.
     DISPLAY   NC"＃受信日付" "(LINK-HIDUKE) = " LINK-HIDUKE
                                                    UPON CONS.
     DISPLAY   NC"＃受信時刻" "(LINK-JIKAN6) = " LINK-JIKAN6
                                                    UPON CONS.
     DISPLAY   NC"＃受信時刻" "(LINK-JIKAN4) = " LINK-JIKAN4
                                                    UPON CONS.
*    DISPLAY "LINK-HIDUKE = " LINK-HIDUKE  UPON CONS.
*    DISPLAY "LINK-JIKAN4 = " LINK-JIKAN4  UPON CONS.
*    DISPLAY "LINK-JIKAN6 = " LINK-JIKAN6  UPON CONS.
     STOP  RUN.
 PROCESS-END.
     EXIT.

```
