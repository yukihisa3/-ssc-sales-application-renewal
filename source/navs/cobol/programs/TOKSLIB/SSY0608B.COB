****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム                  *
*    モジュール名　　　　：　メモ_取得　　                    *
*    作成日／作成者　　　：　99/10/08  /HAGIWARA               *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　ワークステーション名を受け取り　　*
*                            条件ファイルよりメモ_を抽出      *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY0608B.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/10/08.
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
 01  LKOUT-MEMONO            PIC  9(05).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LKIN-WKSNAME  LKOUT-MEMONO.
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
     MOVE        51               TO   JYO-F01
*----------------- テスト用 ------------------------*
*    MOVE        "SAKATAPC"       TO   JYO-F02
*---------------------------------------------------*
     MOVE        LKIN-WKSNAME     TO   JYO-F02
     READ        HJYOKEN
       INVALID
         MOVE    ZERO             TO   LKOUT-MEMONO
       NOT INVALID
*********MOVE    JYO-F04          TO   WK-MEMONO
         MOVE    JYO-F05          TO   WK-MEMONO
         MOVE    WK-MEMONO(5:5)   TO   LKOUT-MEMONO
     END-READ.
     DISPLAY "MEMO = " LKOUT-MEMONO UPON CONS.
*ファイルＣＬＯＳＥ
     CLOSE       HJYOKEN.
     STOP        RUN.
*
 MAIN-EXIT.
     EXIT.
