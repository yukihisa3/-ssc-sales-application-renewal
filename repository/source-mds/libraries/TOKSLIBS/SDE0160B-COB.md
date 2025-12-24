# SDE0160B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SDE0160B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　伝票ＥＸＣＥＬ取込                *
*    モジュール名　　　　：　実行状況チェック　　　　　　　    *
*    作成日／作成者　　　：　2016/09/13  INOUE                 *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　伝票EXCELデータ処理状況の把握
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SDE0160B.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2016/09/13.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*実行状況ファイル
     SELECT     DENACTL1   ASSIGN    TO        DENACTL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       ACT-F01
                           FILE      STATUS    ACT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
*  FILE=実行状況ファイル　                                    *
****************************************************************
 FD  DENACTL1
     LABEL       RECORD    IS        STANDARD.
     COPY        DENACTL1  OF        XFDLIB
     JOINING     ACT       AS        PREFIX.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  ACT-ST              PIC X(02).
***
 01  WK-AREA.
     03  WK-MEMONO           PIC 9(11) VALUE ZERO.
***
 01  FILE-ERR.
     03  ACT-ERR             PIC N(15) VALUE
                        NC"実行状況Ｆエラー".
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-IN-WKSTN              PIC  X(08).
 01  LINK-OUT-PHASE             PIC  X(01).
 01  LINK-OUT-DATE-TORIKOMI     PIC  9(08).
 01  LINK-OUT-TIME-TORIKOMI     PIC  9(06).
 01  LINK-OUT-DATE-HENKAN       PIC  9(08).
 01  LINK-OUT-TIME-HENKAN       PIC  9(06).
 01  LINK-OUT-DATE-KEIJOU       PIC  9(08).
 01  LINK-OUT-TIME-KEIJOU       PIC  9(06).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LINK-IN-WKSTN
                                  LINK-OUT-PHASE
                                  LINK-OUT-DATE-TORIKOMI
                                  LINK-OUT-TIME-TORIKOMI
                                  LINK-OUT-DATE-HENKAN
                                  LINK-OUT-TIME-HENKAN
                                  LINK-OUT-DATE-KEIJOU
                                  LINK-OUT-TIME-KEIJOU.
****************************************************************
 DECLARATIVES.
 ACT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DENACTL1.
     DISPLAY     ACT-ERR   UPON      CONS.
     DISPLAY     ACT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E                  *
****************************************************************
 MAIN-SEC                  SECTION.
*
*ファイルＯＰＥＮ
 MAIN-01.
     OPEN        I-O                   DENACTL1.
     MOVE        SPACE            TO   ACT-REC.
     INITIALIZE                        ACT-REC.
     MOVE        SPACE            TO   LINK-OUT-PHASE.
     MOVE        ZERO             TO   LINK-OUT-DATE-TORIKOMI
                                       LINK-OUT-TIME-TORIKOMI
                                       LINK-OUT-DATE-HENKAN
                                       LINK-OUT-TIME-HENKAN
                                       LINK-OUT-DATE-KEIJOU
                                       LINK-OUT-TIME-KEIJOU.
*
 MAIN-02.
*    DISPLAY    "LINK-IN-WKSTN = " LINK-IN-WKSTN UPON CONS.
     MOVE        LINK-IN-WKSTN    TO   ACT-F01
     READ        DENACTL1
       INVALID
         MOVE    "0"              TO   LINK-OUT-PHASE
         MOVE    LINK-IN-WKSTN    TO   ACT-F01
         WRITE   ACT-REC
         GO                       TO   MAIN-03
     END-READ.
*
     IF     ( ACT-F041 NOT = ZERO )
              MOVE    "3"         TO   LINK-OUT-PHASE
              MOVE    ACT-F021    TO   LINK-OUT-DATE-TORIKOMI
              MOVE    ACT-F022    TO   LINK-OUT-TIME-TORIKOMI
              MOVE    ACT-F031    TO   LINK-OUT-DATE-HENKAN
              MOVE    ACT-F032    TO   LINK-OUT-TIME-HENKAN
              MOVE    ACT-F041    TO   LINK-OUT-DATE-KEIJOU
              MOVE    ACT-F042    TO   LINK-OUT-TIME-KEIJOU
              GO                  TO   MAIN-03
     END-IF.
*
     IF     ( ACT-F031 NOT = ZERO )  AND
            ( ACT-F041     = ZERO )
              MOVE    "2"         TO   LINK-OUT-PHASE
              MOVE    ACT-F021    TO   LINK-OUT-DATE-TORIKOMI
              MOVE    ACT-F022    TO   LINK-OUT-TIME-TORIKOMI
              MOVE    ACT-F031    TO   LINK-OUT-DATE-HENKAN
              MOVE    ACT-F032    TO   LINK-OUT-TIME-HENKAN
              GO                  TO   MAIN-03
     END-IF.
*
     IF     ( ACT-F021 NOT = ZERO )  AND
            ( ACT-F031     = ZERO )
              MOVE    "1"         TO   LINK-OUT-PHASE
              MOVE    ACT-F021    TO   LINK-OUT-DATE-TORIKOMI
              MOVE    ACT-F022    TO   LINK-OUT-TIME-TORIKOMI
              GO                  TO   MAIN-03
     END-IF.
*
 MAIN-03.
*    DISPLAY "PHASE   = " LINK-OUT-PHASE           UPON CONS.
*    DISPLAY "DATE-1  = " LINK-OUT-DATE-TORIKOMI   UPON CONS.
*    DISPLAY "TIME-1  = " LINK-OUT-TIME-TORIKOMI   UPON CONS.
*    DISPLAY "DATE-2  = " LINK-OUT-DATE-HENKAN     UPON CONS.
*    DISPLAY "TIME-2  = " LINK-OUT-TIME-HENKAN     UPON CONS.
*    DISPLAY "DATE-3  = " LINK-OUT-DATE-KEIJOU     UPON CONS.
*    DISPLAY "TIME-3  = " LINK-OUT-TIME-KEIJOU     UPON CONS.
     CLOSE    DENACTL1.
     STOP     RUN.
*
 MAIN-EXIT.
     EXIT.

```
