***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム
*    業務名　　　　　：
*    モジュール名　　：起動情報取得
*    作成日／更新日　：2012/03/15
*    作成者／更新者　：NAV
*    処理概要　　　　：
*      PRIMERGY6000の営業日欄の曜日を取得しパラメタに渡す。
*      当情報はNASへのバックアップ等、曜日別制御（営業日を
*      考慮した処理制御）に使用する。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           ONCHECK.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2012/03/15.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE    IS  CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*
*起動情報更新ファイル
     SELECT  HCONTF
       ASSIGN    TO    HCONTL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         CON-F01 *> KEY
       FILE STATUS     CON-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 起動情報更新ファイル                               *
****************************************************************
 FD  HCONTF
     LABEL     RECORD    IS   STANDARD.
     COPY      HCONTF    OF   XFDLIB
     JOINING   CON       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  CON-ST             PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03) VALUE SPACE.
     03  FG-HCONTF-INV      PIC  9(01).
*
*ワーク領域
 01  WRK-AREA.
     03  CT-IN0             PIC  9(08).
     03  CT-IN              PIC  9(08).
     03  CT-IN2             PIC  9(08).
     03  CT-UP              PIC  9(08).
     03  CT-DL              PIC  9(08).
     03  CT-PG              PIC  9(08).
     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).
*
     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).
*
     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).
*
     03  IX                 PIC  9(04).
     03  IX2                PIC  9(04).
*
     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO REDEFINES WK-REN-NO-X
                            PIC  9(09).
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"０１２３４５６７８９　".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.
*
*  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).
*
*　システム日付／時刻／曜日
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
     03  SYS-TIME           PIC  9(06)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.
 01  YOUBI-AREA.
     03  SYS-YOUBI          PIC  9(01)  VALUE  ZERO.
     03  WK-YOUBI           PIC S9(01)  VALUE  ZERO.
*
*　日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC  9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC  9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC  9(02)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  CON-ERR           PIC  N(20)  VALUE
         NC"起動情報更新ファイルエラー".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
*
* 出力パラメータ
 01  PARA-YOUBI             PIC  9(01).
*
**************************************************************
 PROCEDURE             DIVISION      USING     PARA-YOUBI.
**************************************************************
 DECLARATIVES.
 CON-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HCONTF.
     DISPLAY     CON-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CON-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
*
     DISPLAY   "**  START ONCHECK    **"  UPON CONS.
*
*ファイルのＯＰＥＮ
     OPEN  INPUT  HCONTF.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.
*
     MOVE   001             TO  CON-F01.
     PERFORM   HCONTF-RD-SEC.
*
 INIT-EXIT.
     EXIT.
***************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.
*
 MAIN-01.
*
*    ｼｽﾃﾑ日付/時刻/曜日GET
     PERFORM  SDATE-GET-SEC.
*
*    ﾚｺｰﾄﾞ非存在時は、前曜日を渡す
     IF       FG-HCONTF-INV = 1
              DISPLAY NC"更新レコードなし" UPON CONS
              COMPUTE WK-YOUBI  =  SYS-YOUBI - 1
              IF      WK-YOUBI <=  0
                      MOVE  7      TO  WK-YOUBI
              END-IF
              MOVE    WK-YOUBI     TO  PARA-YOUBI
     ELSE
              MOVE    CON-F023     TO  PARA-YOUBI
     END-IF.
*
     DISPLAY NC"曜日＝"   PARA-YOUBI   UPON CONS.
*
 MAIN-99.
     MOVE     "END"  TO  END-FLG.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  起動情報更新ファイル検索                                    *
****************************************************************
 HCONTF-RD-SEC        SECTION.
     MOVE  "HCONTF-RD-SEC" TO  S-NAME.
*
     READ  HCONTF
       INVALID
         MOVE  1            TO  FG-HCONTF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HCONTF-INV
     END-READ.
*
 HCONTF-RD-EXIT.
     EXIT.
****************************************************************
*    システム日付取得                                          *
****************************************************************
 SDATE-GET-SEC              SECTION.
     MOVE  "SDATE-GET-SEC"  TO  S-NAME.
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB" USING LINK-IN-KBN
                            LINK-IN-YMD6
                            LINK-IN-YMD8
                            LINK-OUT-RET
                            LINK-OUT-YMD.
     MOVE    LINK-OUT-YMD   TO   SYS-DATE.
*    時刻GET
     ACCEPT  WK-TIME        FROM TIME.
     MOVE    WK-TIME(1:6)   TO   SYS-TIME.
*    曜日GET
     ACCEPT  SYS-YOUBI FROM DAY-OF-WEEK.
*
     DISPLAY "SYS-DATE=" SYS-DATE UPON CONS.
     DISPLAY "SYS-TIME=" SYS-TIME UPON CONS.
     DISPLAY "SYS-YOUBI=" SYS-YOUBI UPON CONS.
*
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.
*
*ファイル ＣＬＯＳＥ
     CLOSE  HCONTF.
     DISPLAY   "**  END   ONCHECK    **"  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  ONCHECK     END PROGRAM  >>******************
