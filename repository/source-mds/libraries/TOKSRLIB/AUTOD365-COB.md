# AUTOD365

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/AUTOD365.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　　                  *
*    モジュール名　　　　：　Ｄ３６５自動送受信制御            *
*    作成日／作成者　　　：　2021/05/14 INOUE                  *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　実行_を採番し、下位ＣＬを実行する*
*                            　　　　　　　　　　　　　　　    *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            AUTOD365.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/05/14.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*条件ファイル
     SELECT     HJYOKEN    ASSIGN    TO        JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                           FILE      STATUS    JYO-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
*  FILE=条件ファイル　　                                       *
****************************************************************
 FD  HJYOKEN
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  JYO-ST              PIC X(02).
***
 01  WK-AREA.
     03  PARA-SET-DATE       PIC 9(08) VALUE ZERO.
     03  PARA-SET-JTIME      PIC 9(04) VALUE ZERO.
     03  PARA-SET-JGROUP     PIC 9(02) VALUE ZERO.
     03  PARA-SET-RUNNO      PIC 9(07) VALUE ZERO.
     03  PARA-SET-TANCD      PIC X(02) VALUE SPACE.
     03  PARA-SET-ATSD       PIC X(01) VALUE "0".
     03  WK-DATE             PIC 9(06) VALUE  ZERO.
     03  SYS-DATE            PIC 9(08) VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD8           PIC 9(08).
*
***
 01  FILE-ERR.
     03  JYO-ERR             PIC N(15) VALUE
                        NC"条件Ｆエラー".
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-IN-JTIME            PIC  9(04).
 01  LINK-IN-JGROUP           PIC  9(02).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LINK-IN-JTIME  LINK-IN-JGROUP.
 DECLARATIVES.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E                  *
****************************************************************
 MAIN-SEC                  SECTION.
*
     DISPLAY  "AUTOD365 START"    UPON CONS.
*
*ファイルＯＰＥＮ
     OPEN        I-O         HJYOKEN.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   SYS-DATE.
     MOVE        99                TO   JYO-F01
     MOVE        "D365"            TO   JYO-F02
     READ        HJYOKEN
       INVALID
         DISPLAY NC"条件ファイルなし！" UPON CONS
         DISPLAY   "F01=99 F02=D365"    UPON CONS
         MOVE    4000              TO   PROGRAM-STATUS
         STOP    RUN
       NOT INVALID
         ADD   1         TO  JYO-F04
         IF    JYO-F04   >   JYO-F06
               MOVE JYO-F05 TO  JYO-F04
         END-IF
         REWRITE  JYO-REC
         MOVE  SYS-DATE       TO  PARA-SET-DATE
         MOVE  LINK-IN-JTIME  TO  PARA-SET-JTIME
         MOVE  LINK-IN-JGROUP TO  PARA-SET-JGROUP
         MOVE  JYO-F04        TO  PARA-SET-RUNNO
         MOVE  "99"           TO  PARA-SET-TANCD
         MOVE  "0"            TO  PARA-SET-ATSD
*
         DISPLAY "D365SECK  CALL"          UPON CONS
         DISPLAY " DATE =" PARA-SET-DATE   UPON CONS
         DISPLAY " TIME =" PARA-SET-JTIME  UPON CONS
         DISPLAY " GROUP=" PARA-SET-JGROUP UPON CONS
         DISPLAY " RUNNO=" PARA-SET-RUNNO  UPON CONS
         DISPLAY " TANCD=" PARA-SET-TANCD  UPON CONS
         DISPLAY " ATSD =" PARA-SET-ATSD   UPON CONS
*
         CALL "D365SECK"      USING
                                  PARA-SET-DATE
                                  PARA-SET-JTIME
                                  PARA-SET-JGROUP
                                  PARA-SET-RUNNO
                                  PARA-SET-TANCD
                                  PARA-SET-ATSD
     END-READ.
*
*ファイルＣＬＯＳＥ
     CLOSE       HJYOKEN.
*
     DISPLAY    "AUTOD365 END"    UPON CONS.
*
     STOP        RUN.
*
 MAIN-EXIT.
     EXIT.

```
