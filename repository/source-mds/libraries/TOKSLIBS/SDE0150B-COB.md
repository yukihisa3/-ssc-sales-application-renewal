# SDE0150B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SDE0150B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　伝票ＥＸＣＥＬ取込                *
*    モジュール名　　　　：　実行状況更新　　　　　　　　　    *
*    作成日／作成者　　　：　2016/09/15  INOUE                 *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　伝票EXCELデータ処理状況の更新
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SDE0150B.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2016/09/15.
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
 01  LINK-IN-WKSTN                PIC  X(08).
 01  LINK-IN-PHASE                PIC  X(01).
 01  LINK-IN-DATE                 PIC  9(08).
 01  LINK-IN-TIME                 PIC  9(06).
*
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LINK-IN-WKSTN
                                  LINK-IN-PHASE
                                  LINK-IN-DATE
                                  LINK-IN-TIME.
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
*更新内容チェック
 MAIN-00.
*   実行状況
     IF          LINK-IN-PHASE   =   "1" OR "2" OR "3"
                 CONTINUE
     ELSE
                 DISPLAY NC"実行状況が１．２．３以外！？　"
                                       LINK-IN-PHASE  UPON CONS
                 MOVE   4000      TO   PROGRAM-STATUS
                 GO               TO   MAIN-04
     END-IF.
*   日付
     IF        ( LINK-IN-DATE     IS   NUMERIC ) AND
               ( LINK-IN-DATE     NOT  = ZERO  )
                 CONTINUE
     ELSE
                 DISPLAY NC"日付が不正！？　"
                                       LINK-IN-DATE   UPON CONS
                 MOVE   4000      TO   PROGRAM-STATUS
                 GO               TO   MAIN-04
     END-IF.
*   時刻
     IF        ( LINK-IN-TIME     IS   NUMERIC ) AND
               ( LINK-IN-TIME     NOT  = ZERO  )
                 CONTINUE
     ELSE
                 DISPLAY NC"時刻が不正！？　"
                                       LINK-IN-TIME   UPON CONS
                 MOVE   4000      TO   PROGRAM-STATUS
                 GO               TO   MAIN-04
     END-IF.
*
*ファイルＯＰＥＮ
 MAIN-01.
     OPEN        I-O                   DENACTL1.
*
*存在チェック
 MAIN-02.
     DISPLAY    "LINK-IN-PHASE = " LINK-IN-PHASE UPON CONS.
     DISPLAY    "LINK-IN-DATE  = " LINK-IN-DATE  UPON CONS.
     DISPLAY    "LINK-IN-TIME  = " LINK-IN-TIME  UPON CONS.
     MOVE        LINK-IN-WKSTN    TO   ACT-F01.
     READ        DENACTL1
       INVALID
                 GO               TO   MAIN-021
       NOT INVALID
                 GO               TO   MAIN-022
     END-READ.
*
*レコードＷＲＩＴＥ
 MAIN-021.
     MOVE    SPACE            TO     ACT-REC.
     INITIALIZE                      ACT-REC.
     MOVE    LINK-IN-WKSTN    TO     ACT-F01.
     IF      LINK-IN-PHASE   =      "1"
             MOVE     LINK-IN-DATE   TO   ACT-F021
             MOVE     LINK-IN-TIME   TO   ACT-F022
     END-IF.
     IF      LINK-IN-PHASE   =      "2"
             MOVE     LINK-IN-DATE   TO   ACT-F021
                                          ACT-F031
             MOVE     LINK-IN-TIME   TO   ACT-F022
                                          ACT-F032
     END-IF.
     IF      LINK-IN-PHASE   =      "3"
             MOVE     LINK-IN-DATE   TO   ACT-F021
                                          ACT-F031
                                          ACT-F041
             MOVE     LINK-IN-TIME   TO   ACT-F022
                                          ACT-F032
                                          ACT-F042
     END-IF.
     WRITE   ACT-REC.
*
*レコードＲＥＷＲＩＴＥ
 MAIN-022.
     IF      LINK-IN-PHASE   =      "1"
             MOVE     LINK-IN-DATE   TO   ACT-F021
             MOVE     LINK-IN-TIME   TO   ACT-F022
             MOVE     ZERO           TO   ACT-F031
                                          ACT-F032
                                          ACT-F041
                                          ACT-F042
     END-IF.
     IF      LINK-IN-PHASE   =      "2"
             MOVE     LINK-IN-DATE   TO   ACT-F031
             MOVE     LINK-IN-TIME   TO   ACT-F032
             MOVE     ZERO           TO   ACT-F041
                                          ACT-F042
     END-IF.
     IF      LINK-IN-PHASE   =      "3"
             MOVE     LINK-IN-DATE   TO   ACT-F041
             MOVE     LINK-IN-TIME   TO   ACT-F042
     END-IF.
     REWRITE ACT-REC.
*
*ファイルＣＬＯＳＥ
 MAIN-03.
     CLOSE    DENACTL1.
*
 MAIN-04.
     STOP     RUN.
*
 MAIN-EXIT.
     EXIT.

```
