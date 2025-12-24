# SKY2501B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY2501B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　振替データ振分更新　　　　　　　　*
*    作成日／更新日　　　：　2000/06/07                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　振替データを部門毎に振分けを行う。*
*                        ：　                                  *
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY2501B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/05/09.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<<計上 データ >>--*
     SELECT   TOKFRR   ASSIGN         DA-01-S-TOKFRR
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK1-ST.
*----<<本社 データ >>--*
     SELECT   TOKFHO   ASSIGN         DA-01-S-TOKFHO
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOH1-ST.
*----<<福岡 データ >>--*
     SELECT   TOKFFU   ASSIGN         DA-01-S-TOKFFU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOF1-ST.
*----<<大阪 データ >>--*
     SELECT   TOKFOS   ASSIGN         DA-01-S-TOKFOS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOO1-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 売上データ >>--*
 FD  TOKFRR            LABEL RECORD   IS   STANDARD.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TOK1      PREFIX.
*----<< 売上データ >>--*
 FD  TOKFHO            LABEL RECORD   IS   STANDARD.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TOH1      PREFIX.
*----<< 売上データ >>--*
 FD  TOKFFU            LABEL RECORD   IS   STANDARD.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TOF1      PREFIX.
*----<< 売上データ >>--*
 FD  TOKFOS            LABEL RECORD   IS   STANDARD.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TOO1      PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01)   VALUE ZERO.
 01  WK-CNT.
     03  IN-CNT         PIC  9(07).
     03  HOF-CNT        PIC  9(07).
     03  FKF-CNT        PIC  9(07).
     03  OSF-CNT        PIC  9(07).
     03  ERR-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TOK1-ST           PIC  X(02).
 01  TOH1-ST           PIC  X(02).
 01  TOF1-ST           PIC  X(02).
 01  TOO1-ST           PIC  X(02).
*
 01  WK-BUMON          PIC  9(04)      VALUE  ZERO.
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークＦ >>--*
 TOKFRR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKFRR.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2501B TOKFRR ERROR " TOK1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ワークＦ >>--*
 TOKFHO                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKFHO.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2501B TOKFHO ERROR " TOH1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ワークＦ >>--*
 TOKFFU                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKFFU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2501B TOKFFU ERROR " TOF1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ワークＦ >>--*
 TOKFOS                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKFOS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY2501B TOKFOS ERROR " TOO1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    9.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY2501B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     TOKFRR.
     OPEN     OUTPUT    TOKFHO.
     OPEN     OUTPUT    TOKFFU.
     OPEN     OUTPUT    TOKFOS.
*クリア
     INITIALIZE         FLAGS  WK-CNT.
*売上Ｆリード
     PERFORM       TOKFRR-RD-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*振分先判定
     MOVE      TOK1-F01          TO    WK-BUMON.
     EVALUATE  WK-BUMON
         WHEN  2900
               MOVE     SPACE    TO    TOH1-REC
               INITIALIZE              TOH1-REC
               MOVE     TOK1-REC TO    TOH1-REC
               WRITE    TOH1-REC
               ADD      1        TO    HOF-CNT
         WHEN  2910
               MOVE     SPACE    TO    TOF1-REC
               INITIALIZE              TOF1-REC
               MOVE     TOK1-REC TO    TOF1-REC
               WRITE    TOF1-REC
               ADD      1        TO    FKF-CNT
         WHEN  2990
               MOVE     SPACE    TO    TOO1-REC
               INITIALIZE              TOO1-REC
               MOVE     TOK1-REC TO    TOO1-REC
               WRITE    TOO1-REC
               ADD      1        TO    OSF-CNT
         WHEN  OTHER
               ADD      1        TO    ERR-CNT
     END-EVALUATE.
*売上Ｆリード
     PERFORM       TOKFRR-RD-SEC.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    TOKFRR TOKFHO TOKFFU TOKFOS.
*
     DISPLAY  NC"振替Ｄ　　　件数＝" IN-CNT   UPON CONS.
     DISPLAY  NC"本社Ｄ振分　件数＝" HOF-CNT  UPON CONS.
     DISPLAY  NC"福岡Ｄ振分　件数＝" FKF-CNT  UPON CONS.
     DISPLAY  NC"大阪Ｄ振分　件数＝" FKF-CNT  UPON CONS.
     DISPLAY  NC"未振分　　　件数＝" ERR-CNT  UPON CONS.

     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY2501B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      売上Ｆリード                                *
*--------------------------------------------------------------*
 TOKFRR-RD-SEC         SECTION.
     READ   TOKFRR  AT  END
            MOVE     9   TO     END-FLG
            NOT  AT  END
            ADD      1   TO     IN-CNT
     END-READ.
 TOKFRR-RD-EXIT.
     EXIT.

```
