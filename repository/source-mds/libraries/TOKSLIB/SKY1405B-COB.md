# SKY1405B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1405B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム                  *
*    モジュール名　　　　：　倉庫コード取得                    *
*    作成日／作成者　　　：　99/10/19                          *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　ワークステーション名を受け取り    *
*                            条件ファイルより倉庫コードを受け  *
*                            す。                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY1405B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/10/19.
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
 01  LINK-IN-WS              PIC  X(08).
 01  LINK-OUT-SOKCD          PIC  9(02).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LINK-IN-WS  LINK-OUT-SOKCD.
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
     MOVE        65               TO   JYO-F01
     MOVE        LINK-IN-WS       TO   JYO-F02
     READ        HJYOKEN
       INVALID
         MOVE    1                TO   LINK-OUT-SOKCD
       NOT INVALID
         MOVE    JYO-F04          TO   LINK-OUT-SOKCD
     END-READ.
*ファイルＣＬＯＳＥ
     CLOSE       HJYOKEN.
     STOP        RUN.
*
 MAIN-EXIT.
     EXIT.

```
