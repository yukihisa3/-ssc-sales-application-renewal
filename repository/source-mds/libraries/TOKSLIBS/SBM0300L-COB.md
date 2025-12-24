# SBM0300L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM0300L.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　
*    業務名　　　　　：　流通ＢＭＳオンライン
*    モジュール名　　：　納品センターマスタリスト
*    作成日／更新日　：　2012/11/05
*    作成者／更新者　：　NAV
*    処理概要　　　　：　　　　　　　　　　　　　　　　　　　
*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBM0300L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/11/05.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   PITCH-20       *> 2.0ピッチ
         YA-21     IS   PITCH-20-YKBAI *> 2.0ピッチ、横倍
         YB        IS   PITCH-15       *> 1.5ピッチ
         YB-21     IS   PITCH-15-YKBAI *> 1.5ピッチ、横倍
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 納品センターマスタ >>--*
**** SELECT  SOKCETF   ASSIGN TO     DA-01-VI-SOKCETL1
     SELECT  SOKCETF   ASSIGN TO     SOKCETL1
                       ORGANIZATION  INDEXED
                       ACCESS MODE   SEQUENTIAL
                       RECORD KEY    SCT-F01  SCT-F02
                                     SCT-F03
                       FILE STATUS   SCT-ST.
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO    DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS   ASSIGN        DA-01-VI-TOKMS2
                       ORGANIZATION  INDEXED
                       ACCESS MODE   RANDOM
                       RECORD KEY    TOK-F01
                       STATUS        TOK-ST.
*----<< プリンタ >>-*
     SELECT   PRTF     ASSIGN        LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 納品センターマスタ >>--*
 FD  SOKCETF  LABEL RECORD  IS  STANDARD.
     COPY  SOKCETF OF XFDLIB
           JOINING  SCT  AS PREFIX.

*----<< 取引先マスタ >>--*
 FD  HTOKMS  LABEL RECORD   IS   STANDARD.
     COPY  HTOKMS OF XFDLIB
           JOINING  TOK  AS PREFIX.

*----<< プリンタ >>-*
 FD  PRTF  LABEL RECORD  IS OMITTED.
 01  PRT-REC            PIC  X(200).

*倉庫マスタ
 FD  ZSOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*店舗マスタ
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
     03  FG-HTOKMS-INV  PIC  9(01).
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)  VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)  VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SCT-ST            PIC  X(02).
 01  TOK-ST            PIC  X(02).
 01  SOK-ST            PIC  X(02).
 01  TEN-ST            PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).

 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).

 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).

*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH          PIC   9(02)  VALUE  ZERO.
     03  WK-MM          PIC   9(02)  VALUE  ZERO.


*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER  PIC  X(01)  VALUE  SPACE.
     03  FILLER  PIC  X(08)  VALUE  "SBM0300L".
     03  FILLER  PIC  X(32)  VALUE  SPACE.
     03  FILLER  PIC  N(18)  CHARACTER TYPE  PITCH-20  VALUE
       NC"　納品センター　マスタリスト".

 01  HEAD02  CHARACTER TYPE  PITCH-15.
     03  FILLER  PIC  X(117)  VALUE  SPACE.
     03  HD01-Y  PIC  9999.
     03  FILLER  PIC  X(01)  VALUE  "/".
     03  HD01-M  PIC  99.
     03  FILLER  PIC  X(01)  VALUE  "/".
     03  HD01-D  PIC  99.
     03  FILLER  PIC  X(01)  VALUE  SPACE.
     03  HD01-PG PIC  ZZ9.
     03  FILLER  PIC  N(01)  VALUE  NC"頁".

 01  KUGIRI-SEN.
     03  FILLER  PIC  X(01)  VALUE  SPACE.
     03  FILLER  PIC  X(132) VALUE  ALL "-".

 01  HEAD03  CHARACTER TYPE  PITCH-15.
     03  FILLER  PIC  X(02)  VALUE  SPACE.
     03  FILLER  PIC  N(02)  VALUE  NC"倉庫".
     03  FILLER  PIC  X(16)  VALUE  SPACE.
     03  FILLER  PIC  N(03)  VALUE  NC"取引先".
     03  FILLER  PIC  X(21)  VALUE  SPACE.
     03  FILLER  PIC  N(02)  VALUE  NC"店舗".
     03  FILLER  PIC  X(19)  VALUE  SPACE.
     03  FILLER  PIC  N(06)  VALUE  NC"納品センター".
     03  FILLER  PIC  X(26)  VALUE  SPACE.
     03  FILLER  PIC  N(04)  VALUE  NC"登録者　".
     03  FILLER  PIC  N(04)  VALUE  NC"作成日".
***  03  FILLER  PIC  X(04)  VALUE  SPACE.
***  03  FILLER  PIC  N(04)  VALUE  NC"作成時間".
     03  FILLER  PIC  X(04)  VALUE  SPACE.
     03  FILLER  PIC  N(04)  VALUE  NC"更新者　".
     03  FILLER  PIC  N(04)  VALUE  NC"更新日".
***  03  FILLER  PIC  X(02)  VALUE  SPACE.
***  03  FILLER  PIC  N(04)  VALUE  NC"更新時間".

 01  MEIS01  CHARACTER TYPE PITCH-15  VALUE SPACE.
     03  FILLER             PIC  X(02).
     03  MS01-SOKCD         PIC  X(02).
     03  FILLER             PIC  X(01).
     03  MS01-SOK-NM        PIC  N(10).
     03  FILLER             PIC  X(01).
*
     03  MS01-TOKCD         PIC  X(08).
     03  FILLER             PIC  X(01).
     03  MS01-TOK-NM        PIC  N(10).
*
     03  FILLER             PIC  X(01).
     03  MS01-TENCD         PIC  X(05).
     03  FILLER             PIC  X(01).
     03  MS01-TEN-NM        PIC  N(10).
*
     03  FILLER             PIC  X(01).
     03  MS01-NCENCD        PIC  X(13).
*
     03  FILLER             PIC  X(22).
     03  MS01-TOROKSHA      PIC  X(02).
     03  FILLER             PIC  X(02).
     03  MS01-SAKSEIBI      PIC  X(10).
     03  FILLER  REDEFINES  MS01-SAKSEIBI.
       05  MS01-SAKSEIBI-Y  PIC  9(04).
       05  MS01-SAKSEIBI-E1 PIC  X(01).
       05  MS01-SAKSEIBI-M  PIC  9(02).
       05  MS01-SAKSEIBI-E2 PIC  X(01).
       05  MS01-SAKSEIBI-D  PIC  9(02).
     03  FILLER             PIC  X(02).
*
***  03  MS01-SAKSEITM      PIC  X(05).
***  03  FILLER  REDEFINES  MS01-SAKSEITM.
***    05  MS01-SAKSEITM-H  PIC  9(02).
***    05  MS01-SAKSEITM-E1 PIC  X(01).
***    05  MS01-SAKSEITM-M  PIC  9(02).
***  03  FILLER             PIC  X(02).
     03  MS01-KOSNSHA       PIC  X(02).
     03  FILLER             PIC  X(02).
*
     03  MS01-KOSNBI        PIC  X(10).
     03  FILLER  REDEFINES  MS01-KOSNBI.
       05  MS01-KOSNBI-Y    PIC  9(04).
       05  MS01-KOSNBI-E1   PIC  X(01).
       05  MS01-KOSNBI-M    PIC  9(02).
       05  MS01-KOSNBI-E2   PIC  X(01).
       05  MS01-KOSNBI-D    PIC  9(02).
     03  FILLER             PIC  X(01).
*
***  03  MS01-KOSNTM        PIC  X(05).
***  03  FILLER  REDEFINES  MS01-KOSNTM.
***    05  MS01-KOSNTM-H    PIC  9(02).
***    05  MS01-KOSNTM-E1   PIC  X(01).
***    05  MS01-KOSNTM-M    PIC  9(02).
*
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE                SECTION.
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 納品センターマスタ >>--*
 SCT-ERR                   SECTION.
     USE AFTER EXCEPTION PROCEDURE  SOKCETF.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SBM0300L SOKCETF ERROR STS=" SCT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     STOP RUN.
*----<< 取引先マスタ >>--*
 TOK-ERR                SECTION.
     USE AFTER EXCEPTION PROCEDURE  HTOKMS.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SBM0300L HTOKMS ERROR STS=" TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     STOP RUN.
*----<< 倉庫マスタ >>--*
 SOK-ERR                SECTION.
     USE AFTER EXCEPTION PROCEDURE  ZSOKMS.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SBM0300L ZSOKMS ERROR STS=" SOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     STOP RUN.
*----<< 店舗マスタ >>--*
 TEN-ERR                SECTION.
     USE AFTER EXCEPTION PROCEDURE  HTENMS.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SBM0300L HTENMS ERROR STS=" TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     STOP RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    全体処理                                                  *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     MOVE    "PROG-CNTL"      TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    初期処理                                                  *
*--------------------------------------------------------------*
 INIT-SEC           SECTION.
     MOVE  "INIT-SEC"            TO  S-NAME.
     ACCEPT  SYS-DATE  FROM  DATE.
     MOVE "3"                    TO  LINK-IN-KBN.
     MOVE SYS-DATE               TO  LINK-IN-YMD6.
     CALL "SKYDTCKB"  USING  LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     IF  LINK-OUT-RET = ZERO
         MOVE LINK-OUT-YMD       TO  SYS-DATEW
     ELSE
         MOVE ZERO               TO  SYS-DATEW
     END-IF.

     ACCEPT  SYS-TIME  FROM  TIME.

     DISPLAY  "*** SBM0300L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
              UPON CONS.
*
     OPEN  INPUT  SOKCETF.
     OPEN  INPUT  ZSOKMS  HTOKMS  HTENMS.
     OPEN  OUTPUT PRTF.

     MOVE  99                    TO  LINE-CNT.

     PERFORM  INF-RD-SEC.
     IF END-FLG = "END"
        GO TO  INIT-EXIT
     END-IF.

 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    納品センターマスタ読込み                                  *
*--------------------------------------------------------------*
 INF-RD-SEC           SECTION.
     MOVE  "INF-RD-SEC"          TO  S-NAME.
*
     READ  SOKCETF
       AT END
         MOVE  "END"             TO  END-FLG
         GO TO  INF-RD-EXIT
     END-READ.
*
 INF-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    主処理                                                    *
*--------------------------------------------------------------*
 MAIN-SEC           SECTION.
     MOVE  "MAIN-SEC"            TO  S-NAME.
     IF  LINE-CNT >= 58
         PERFORM  HD-PRT-SEC
     END-IF.

     PERFORM  MS-PRT-SEC.
     PERFORM  INF-RD-SEC.

 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印刷処理                                            *
*--------------------------------------------------------------*
 HD-PRT-SEC            SECTION.
     MOVE  "HD-PRT-SEC"          TO  S-NAME.
     MOVE  SYS-YYW               TO  HD01-Y.
     MOVE  SYS-MMW               TO  HD01-M.
     MOVE  SYS-DDW               TO  HD01-D.
     IF PAGE-CNT NOT = ZERO
        MOVE SPACE               TO PRT-REC
        WRITE  PRT-REC  AFTER PAGE
     END-IF.
     ADD 1  TO PAGE-CNT.
     MOVE  PAGE-CNT              TO  HD01-PG.
     WRITE  PRT-REC  FROM HEAD01      AFTER 2.
     WRITE  PRT-REC  FROM HEAD02      AFTER 1.
     WRITE  PRT-REC  FROM KUGIRI-SEN  AFTER 1.
     WRITE  PRT-REC  FROM HEAD03      AFTER 1.
     WRITE  PRT-REC  FROM KUGIRI-SEN  AFTER 1.
     MOVE  ZERO                  TO LINE-CNT.
 HD-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印刷処理                                              *
*--------------------------------------------------------------*
 MS-PRT-SEC            SECTION.
     MOVE  "MS-PRT-SEC"          TO  S-NAME.
*
     MOVE  SPACE                 TO  MEIS01.
     MOVE  SCT-F01               TO  MS01-SOKCD.
     MOVE  SCT-F01               TO  SOK-F01.
** 倉庫マスタ
     READ  ZSOKMS
       INVALID
         MOVE  ALL  NC"＊"       TO  MS01-SOK-NM
       NOT INVALID
         MOVE  SOK-F02           TO  MS01-SOK-NM
     END-READ.
*
     MOVE  SCT-F02               TO  MS01-TOKCD.
     MOVE  SCT-F02               TO  TOK-F01.
** 取引先マスタ
     READ  HTOKMS
       INVALID
         MOVE  ALL  NC"＊"       TO  MS01-TOK-NM
       NOT INVALID
         MOVE  TOK-F03           TO  MS01-TOK-NM
     END-READ.

*
     MOVE  SCT-F03               TO  MS01-TENCD.
     MOVE  SCT-F03               TO  TEN-F011.
     MOVE  SCT-F02               TO  TEN-F52.
** 店舗マスタ
     READ  HTENMS
       INVALID
         MOVE  ALL  NC"＊"       TO  MS01-TEN-NM
       NOT INVALID
         MOVE  TEN-F03           TO  MS01-TEN-NM
     END-READ.
*
     MOVE  SCT-F04               TO  MS01-NCENCD.
*
     MOVE  SCT-F94               TO  MS01-TOROKSHA.
     MOVE  SCT-F95(1:4)          TO  MS01-SAKSEIBI-Y.
     MOVE  "/"                   TO  MS01-SAKSEIBI-E1.
     MOVE  SCT-F95(5:2)          TO  MS01-SAKSEIBI-M.
     MOVE  "/"                   TO  MS01-SAKSEIBI-E2.
     MOVE  SCT-F95(7:2)          TO  MS01-SAKSEIBI-D.
**** MOVE  SCT-F96(1:2)          TO  MS01-SAKSEITM-H.
**** MOVE  ":"                   TO  MS01-SAKSEITM-E1.
**** MOVE  SCT-F96(3:2)          TO  MS01-SAKSEITM-M.
     MOVE  SCT-F97               TO  MS01-KOSNSHA.
*
     IF SCT-F98 NOT = ZERO
        MOVE  SCT-F98(1:4)       TO  MS01-KOSNBI-Y
        MOVE  "/"                TO  MS01-KOSNBI-E1
        MOVE  SCT-F98(5:2)       TO  MS01-KOSNBI-M
        MOVE  "/"                TO  MS01-KOSNBI-E2
        MOVE  SCT-F98(7:2)       TO  MS01-KOSNBI-D
****    MOVE  SCT-F99(1:2)       TO  MS01-KOSNTM-H
****    MOVE  ":"                TO  MS01-KOSNTM-E1
****    MOVE  SCT-F99(3:2)       TO  MS01-KOSNTM-M
     ELSE
        MOVE  SPACE              TO  MS01-KOSNBI
****    MOVE  SPACE              TO  MS01-KOSNTM
     END-IF.
*
     WRITE  PRT-REC  FROM MEIS01      AFTER 1.

**** WRITE  PRT-REC  FROM KUGIRI-SEN  AFTER 1.

     ADD  1  TO LINE-CNT.
 MS-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    終了処理                                                  *
*--------------------------------------------------------------*
 END-SEC            SECTION.
     MOVE  "END-SEC"             TO  S-NAME.
     CLOSE  SOKCETF.
     CLOSE  HTOKMS  ZSOKMS  HTENMS.
     CLOSE  PRTF.
*
     ACCEPT  SYS-DATE  FROM  DATE.
     ACCEPT  SYS-TIME  FROM  TIME.
     DISPLAY
       "*** SBM0300L ﾏｽﾀ ﾘｽﾄ PAGE = "
       PAGE-CNT
       UPON CONS.
     DISPLAY  "*** SBM0300L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
              UPON CONS.
 END-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
