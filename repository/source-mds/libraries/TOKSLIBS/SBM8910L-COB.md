# SBM8910L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM8910L.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　：　（株）サカタのタネ殿　　　　　　　　
*    業務名　　　　　：　流通ＢＭＳオンライン
*    モジュール名　　：　取引先振分マスタリスト
*    作成日／更新日　：　2012/11/09
*    作成者／更新者　：　NAV
*    処理概要　　　　：　　　　　　　　　　　　　　　　　　　
*    作成日／更新日　：　2012/12/25
*    作成者／更新者　：　NAV
*    処理概要　　　　：　別バッチ区分追加　　　　　　　　　　
*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBM8910L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/11/09.
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
*----<< 取引先振分マスタ >>--*
     SELECT  IONFURF   ASSIGN TO     IONFURL2
                       ORGANIZATION  INDEXED
                       ACCESS MODE   SEQUENTIAL
                       RECORD KEY    IOF-F03
                                     IOF-F02
                       FILE STATUS   IOF-ST.
*条件ファイル
     SELECT     HJYOKEN    ASSIGN    TO    DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       JYO-F01  JYO-F02
                           FILE      STATUS    JYO-ST.
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
*----<< 取引先振分マスタ >>--*
 FD  IONFURF  LABEL RECORD  IS  STANDARD.
     COPY  IONFURF OF XFDLIB
           JOINING  IOF  AS PREFIX.

*----<< 取引先マスタ >>--*
 FD  HTOKMS  LABEL RECORD   IS   STANDARD.
     COPY  HTOKMS OF XFDLIB
           JOINING  TOK  AS PREFIX.

*----<< プリンタ >>-*
 FD  PRTF  LABEL RECORD  IS OMITTED.
 01  PRT-REC            PIC  X(200).

*条件ファイル
 FD  HJYOKEN.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
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
     03  JTBL-IX        PIC  9(02).
     03  IX             PIC  9(02).
*
* 条件ファイル用テーブル領域
 01  JYOTBL-AREA.
     03  JTBL-WK       OCCURS  10.
       05  JTBL-JKNO              PIC  X(02).
       05  JTBL-JKCD              PIC  X(13).
       05  JTBL-JKNM              PIC  N(20).
*
 01  JTBL-SW           PIC  X(01).
 01  WK-BR-TOKCD       PIC  X(08).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  IOF-ST            PIC  X(02).
 01  TOK-ST            PIC  X(02).
 01  JYO-ST            PIC  X(02).
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
     03  FILLER  PIC  X(08)  VALUE  "SBM8910L".
     03  FILLER  PIC  X(32)  VALUE  SPACE.
     03  FILLER  PIC  N(18)  CHARACTER TYPE  PITCH-20  VALUE
       NC"イオン取引先　振分マスタリスト".

 01  HEAD02  CHARACTER TYPE  PITCH-15.
     03  FILLER  PIC  X(10)  VALUE  SPACE.
     03  FILLER  PIC  N(04)  VALUE
       NC"取引先：".
     03  HD01-TOKCD   PIC    X(08).
     03  FILLER  PIC  X(02)  VALUE  SPACE.
     03  HD01-TOKNM   PIC    N(20).
     03  FILLER  PIC  X(061)  VALUE  SPACE.
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
     03  FILLER  PIC  N(02)  VALUE  NC"店舗".
     03  FILLER  PIC  X(24)  VALUE  SPACE.
     03  FILLER  PIC  N(02)  VALUE  NC"会社".
     03  FILLER  PIC  X(38)  VALUE  SPACE.
***  03  FILLER  PIC  X(21)  VALUE  SPACE.
     03  FILLER  PIC  N(06)  VALUE  NC"別バッチ区分".
     03  FILLER  PIC  X(22)  VALUE  SPACE.
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
     03  MS01-TENCD         PIC  X(05).
     03  FILLER             PIC  X(01).
     03  MS01-TEN-NM        PIC  N(10).
     03  FILLER             PIC  X(01).
*
     03  MS01-KCODE         PIC  X(13).
     03  FILLER             PIC  X(01).
     03  MS01-KCD-NM        PIC  N(20).
*
*****03  FILLER             PIC  X(26).
     03  FILLER             PIC  X(02).
     03  MS01-BTKBN         PIC  X(01).
     03  FILLER             PIC  X(23).
*
     03  FILLER             PIC  X(07).
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
*----<< 取引先振分マスタ >>--*
 IOF-ERR                   SECTION.
     USE AFTER EXCEPTION PROCEDURE  IONFURF.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SBM8910L IONFURF ERROR STS=" IOF-ST " "
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
     DISPLAY  "### SBM8910L HTOKMS ERROR STS=" TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     STOP RUN.
*----<< 条件ファイル>--*
 JYO-ERR                SECTION.
     USE AFTER EXCEPTION PROCEDURE  HJYOKEN.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SBM8910L HJYOKEN  ERROR STS=" JYO-ST " "
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
     OPEN  INPUT  IONFURF.
     OPEN  INPUT  HTOKMS  HTENMS.
     OPEN  OUTPUT PRTF.
     OPEN  INPUT  HJYOKEN.
     MOVE  99                    TO  LINE-CNT.
* 条件ファイルテーブル設定
     PERFORM  JYOKEN-TBL-SEC.
     CLOSE   HJYOKEN.
*
     PERFORM  INF-RD-SEC.
     IF END-FLG = "END"
        GO TO  INIT-EXIT
     END-IF.
*
     MOVE  IOF-F03            TO  WK-BR-TOKCD.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    取引先振分　マスタ読込み                                  *
*--------------------------------------------------------------*
 INF-RD-SEC           SECTION.
     MOVE  "INF-RD-SEC"          TO  S-NAME.
*
     READ  IONFURF
       AT END
         MOVE  "END"             TO  END-FLG
         GO TO  INF-RD-EXIT
     END-READ.
*
 INF-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      条件ファイルテーブルセット　　　　　　　　　*
*--------------------------------------------------------------*
 JYOKEN-TBL-SEC         SECTION.
*
     MOVE     69             TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
*
     MOVE    ZERO            TO   JTBL-IX.
     PERFORM VARYING   IX  FROM 1 BY 1 UNTIL IX > 10
         MOVE   SPACE   TO   JTBL-JKNM (IX)
         MOVE   SPACE   TO   JTBL-JKCD (IX)
         MOVE   SPACE   TO   JTBL-JKNO (IX)
     END-PERFORM.
*
*
     START    HJYOKEN   KEY  >=   JYO-F01  JYO-F02
           INVALID
              GO  TO    JYOKEN-TBL-200
     END-START.
 JYOKEN-TBL-010.
*
     READ    HJYOKEN   NEXT
           AT  END
           GO  TO    JYOKEN-TBL-200
     END-READ.
*
     IF   JYO-F01  =  69
          ADD   1   TO   JTBL-IX
        IF  JTBL-IX  <=  10
          MOVE    JYO-F03   TO   JTBL-JKNM (JTBL-IX)
          MOVE    JYO-F14(1:13)   TO  JTBL-JKCD (JTBL-IX)
          MOVE    JYO-F02(1:2)    TO  JTBL-JKNO (JTBL-IX)
          GO  TO  JYOKEN-TBL-010
        END-IF
     END-IF.
*
 JYOKEN-TBL-200.
*
 JYOKEN-TBL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    主処理                                                    *
*--------------------------------------------------------------*
 MAIN-SEC           SECTION.
     MOVE  "MAIN-SEC"            TO  S-NAME.
     IF  LINE-CNT >= 58
         OR  IOF-F03  NOT  =  WK-BR-TOKCD
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
        MOVE  IOF-F03            TO  WK-BR-TOKCD
     END-IF.
     ADD 1  TO PAGE-CNT.
     MOVE  PAGE-CNT              TO  HD01-PG.
     MOVE  WK-BR-TOKCD           TO  HD01-TOKCD.
     MOVE  WK-BR-TOKCD           TO  TOK-F01.
** 取引先マスタ
     READ  HTOKMS
       INVALID
         MOVE  ALL  NC"＊"       TO  HD01-TOKNM
       NOT INVALID
         MOVE  TOK-F02           TO  HD01-TOKNM
     END-READ.
*
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
*
     MOVE  IOF-F02               TO  MS01-TENCD.
     MOVE  IOF-F02               TO  TEN-F011.
     MOVE  IOF-F03               TO  TEN-F52.
** 店舗マスタ
     READ  HTENMS
       INVALID
         MOVE  ALL  NC"＊"       TO  MS01-TEN-NM
       NOT INVALID
         MOVE  TEN-F03           TO  MS01-TEN-NM
     END-READ.
*
     MOVE  IOF-F01               TO  MS01-KCODE.
*
     MOVE  ALL  NC"＊"           TO  MS01-KCD-NM.
     MOVE  SPACE                 TO  JTBL-SW.
* 会社名検索
     PERFORM VARYING   IX  FROM 1 BY 1
            UNTIL  IX  >  JTBL-IX
            OR  JTBL-SW  =  "Y"
        IF  IOF-F01  =  JTBL-JKCD (IX)
            MOVE   JTBL-JKNM (IX)     TO    MS01-KCD-NM
            MOVE   "Y"     TO   JTBL-SW
        END-IF
     END-PERFORM.
*別バッチ区分
     MOVE  IOF-F04               TO  MS01-BTKBN.
*
     MOVE  IOF-F94               TO  MS01-TOROKSHA.
     MOVE  IOF-F95(1:4)          TO  MS01-SAKSEIBI-Y.
     MOVE  "/"                   TO  MS01-SAKSEIBI-E1.
     MOVE  IOF-F95(5:2)          TO  MS01-SAKSEIBI-M.
     MOVE  "/"                   TO  MS01-SAKSEIBI-E2.
     MOVE  IOF-F95(7:2)          TO  MS01-SAKSEIBI-D.
**** MOVE  IOF-F96(1:2)          TO  MS01-SAKSEITM-H.
**** MOVE  ":"                   TO  MS01-SAKSEITM-E1.
**** MOVE  IOF-F96(3:2)          TO  MS01-SAKSEITM-M.
     MOVE  IOF-F97               TO  MS01-KOSNSHA.
*
     IF IOF-F98 NOT = ZERO
        MOVE  IOF-F98(1:4)       TO  MS01-KOSNBI-Y
        MOVE  "/"                TO  MS01-KOSNBI-E1
        MOVE  IOF-F98(5:2)       TO  MS01-KOSNBI-M
        MOVE  "/"                TO  MS01-KOSNBI-E2
        MOVE  IOF-F98(7:2)       TO  MS01-KOSNBI-D
****    MOVE  IOF-F99(1:2)       TO  MS01-KOSNTM-H
****    MOVE  ":"                TO  MS01-KOSNTM-E1
****    MOVE  IOF-F99(3:2)       TO  MS01-KOSNTM-M
     ELSE
        MOVE  SPACE              TO  MS01-KOSNBI
****    MOVE  SPACE              TO  MS01-KOSNTM
     END-IF.
*
     WRITE  PRT-REC  FROM MEIS01      AFTER 1.
*
**** WRITE  PRT-REC  FROM KUGIRI-SEN  AFTER 1.

     ADD  1  TO LINE-CNT.
 MS-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    終了処理                                                  *
*--------------------------------------------------------------*
 END-SEC            SECTION.
     MOVE  "END-SEC"             TO  S-NAME.
     CLOSE  IONFURF.
     CLOSE  HTOKMS  HTENMS.
     CLOSE  PRTF.
*
     ACCEPT  SYS-DATE  FROM  DATE.
     ACCEPT  SYS-TIME  FROM  TIME.
     DISPLAY
       "*** SBM8910L ﾏｽﾀ ﾘｽﾄ PAGE = "
       PAGE-CNT
       UPON CONS.
     DISPLAY  "*** SBM8910L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
              UPON CONS.
 END-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
