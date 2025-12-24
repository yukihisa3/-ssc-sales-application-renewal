****************************************************************
*    顧客名　　　　　：　_サカタのタネ　特販部　殿向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　伝票番号自動採番                     *
*    作成日／更新日　：　2020/02/26                            *
*    作成者／更新者　：　NAV-ASSIST                            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKYNSCK2.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/02/26.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*条件マスタ
     SELECT   HJYOKEN   ASSIGN    TO   DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS  MODE   IS RANDOM
                        RECORD  KEY    IS JYO-F01 JYO-F02
                        FILE    STATUS IS JYO-ST.
***************************************************************
*INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*--------------------------------------------------------------*
 FD    HJYOKEN     LABEL      RECORD    IS    STANDARD.
       COPY        HJYOKEN    OF    XFDLIB
                   JOINING    JYO   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-DENNO                PIC  9(07).
 01  JYO-ST                  PIC  X(02).
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  ST-PG           PIC  X(08) VALUE "SKYSNCKB".
         05  FILLER          PIC  X(11)
                                  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SKYSNCKB".
         05  FILLER          PIC  X(11)
                                  VALUE "  END  *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SKYSNCKB".
         05  FILLER          PIC  X(11)
                                  VALUE " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06) VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05) VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  FILLER          PIC  X(07) VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  IN-F01                  PIC  9(02).
 01  IN-F02                  PIC  X(08).
 01  OUT-DENNO               PIC  9(07).
*
******************************************************************
 PROCEDURE    DIVISION       USING     IN-F01
                                       IN-F02
                                       OUT-DENNO.
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     HJYOKEN.
     MOVE     "HJYOKEN"    TO     AB-FILE.
     MOVE     JYO-ST       TO     AB-STS.
     DISPLAY  MSG-ABEND           UPON CONS.
     DISPLAY  SEC-NAME            UPON CONS.
     DISPLAY  ABEND-FILE          UPON CONS.
     MOVE     4000         TO     PROGRAM-STATUS.
     STOP     RUN.
 END      DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE     "PROG-CNTL" TO   SEC-NAME.
*伝票番号(OUT)初期化
     OPEN     I-O       HJYOKEN.
     MOVE     ZERO      TO   OUT-DENNO.
*条件マスタ読込
     PERFORM  JYO-READ.
     IF       OUT-DENNO =    JYO-F06
              MOVE      JYO-F05   TO   JYO-F04
     ELSE
              ADD       1         TO   WK-DENNO
              MOVE      WK-DENNO  TO   JYO-F04
     END-IF.
     REWRITE  JYO-REC.
     CLOSE              HJYOKEN.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL   1.1   条件マスタ読込　　　                      *
*--------------------------------------------------------------*
 JYO-READ         SECTION.
     MOVE     "JYO-READ"     TO     SEC-NAME.
     MOVE     SPACE          TO     JYO-REC.
     INITIALIZE                     JYO-REC.
     MOVE     IN-F01         TO     JYO-F01.
     MOVE     IN-F02         TO     JYO-F02.
     READ     HJYOKEN
         INVALID
              MOVE   SPACE   TO     JYO-REC
              INITIALIZE            JYO-REC
         NOT INVALID
              MOVE   JYO-F04 TO     OUT-DENNO  WK-DENNO
     END-READ.
  JYO-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
