****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム                  *
*    モジュール名　　　　：　ＬＩＮＫＳ再計算Ｆ端末番号取得    *
*    作成日／作成者　　　：　14/08/01  T.TAKAHASHI             *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　ワークステーション名を受け取り　　*
*                            再計算Ｆの番号を取得する。        *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SBT0620B.
 AUTHOR.                TAKAHASHI.
 DATE-WRITTEN.          14/08/01.
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
     03  WK-MEMONO           PIC 9(11) VALUE ZERO.
***
 01  FILE-ERR.
     03  JYO-ERR             PIC N(15) VALUE
                        NC"条件Ｆエラー".
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LKIN-WKSNAME            PIC  X(08).
 01  LKOUT-TANNO             PIC  9(03).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LKIN-WKSNAME  LKOUT-TANNO.
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
*ファイルＯＰＥＮ
     OPEN        INPUT       HJYOKEN.
*
     DISPLAY "LKIN-WKSNAME = " LKIN-WKSNAME UPON CONS.
     MOVE        65               TO   JYO-F01
     MOVE        LKIN-WKSNAME     TO   JYO-F02
     READ        HJYOKEN
       INVALID
         MOVE    1                TO   LKOUT-TANNO
       NOT INVALID
         MOVE    JYO-F12C         TO   LKOUT-TANNO
     END-READ.
     DISPLAY "TANMATU = " LKOUT-TANNO   UPON CONS.
*ファイルＣＬＯＳＥ
     CLOSE       HJYOKEN.
     STOP        RUN.
*
 MAIN-EXIT.
     EXIT.
