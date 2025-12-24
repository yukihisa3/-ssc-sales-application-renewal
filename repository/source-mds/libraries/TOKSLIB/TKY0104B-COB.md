# TKY0104B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/TKY0104B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　自動発注システム                  *
*    モジュール名　　　　：　在庫マスタ自動発注数スライド      *
*    作成日／作成者　　　：　03/06/10  T.TAKAHASHI             *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　条件Ｆをチェックし、在庫マスタの　*
*                            自動発注数のスライドの判断を行う。*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            TKY0104B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/06/10.
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
 01  LINK-KBN                PIC  X(01).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LINK-KBN.
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
     MOVE        99               TO   JYO-F01
     MOVE        "ZAIJIDO"        TO   JYO-F02
     READ        HJYOKEN
       INVALID
         MOVE   "1"               TO   LINK-KBN
       NOT INVALID
         MOVE    JYO-F14(1:1)     TO   LINK-KBN
     END-READ.
*ファイルＣＬＯＳＥ
     CLOSE       HJYOKEN.
     STOP        RUN.
*
 MAIN-EXIT.
     EXIT.

```
