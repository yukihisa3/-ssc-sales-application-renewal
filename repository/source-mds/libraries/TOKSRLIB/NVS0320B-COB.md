# NVS0320B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0320B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　：　（株）サカタのタネ殿　　　　　　　　　　*
*    業務名　　　　：　基幹　　　　　　　　　　　　　　        *
*    サブシステム　：　Ｄ３６５連携　　　　　　　　　　        *
*    モジュール名　：　商品変換ＴＢＬコンバート　　　　　　　  *
*    作成日／作成者：　2021/08/18 INOUE                        *
*    処理概要　　　：　ＳＵＢ商品変換ＴＢＬのサカタ商品ＣＤで  *
*                      ＳＵＢ商品名称Ｍを索引し、Ｄ３６５商品  *
*                      ＣＤ、ＪＡＮＣＤが空白の場合、取得しセ  *
*                      ットする。　　　　　　　　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者：　                                        *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVS0320B.
*                  流用:D365CNV3
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/02.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 商品変換ＴＢＬ　 >>----*
     SELECT   SUBTBLL1  ASSIGN              DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE      STATUS    TBL-STATUS.
*----<< SUB商品名称マスタ　 >>----*
     SELECT   SUBMEIL1  ASSIGN              DA-01-VI-SUBMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    商品変換ＴＢＬ　
******************************************************************
 FD  SUBTBLL1   LABEL RECORD   IS   STANDARD.
     COPY      SUBTBLL1        OF   XFDLIB
     JOINING   TBL                 PREFIX.
******************************************************************
*    SUB商品名称マスタ
******************************************************************
 FD  SUBMEIL1  LABEL RECORD   IS   STANDARD.
     COPY      SUBMEIL1         OF   XFDLIB
     JOINING   MEI                 PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  UPDT-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
     03  TBL-F17-CNT         PIC  9(08)     VALUE  ZERO.
     03  TBL-F18-CNT         PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  CRTTBLF-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  IMPSHOD1-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SUBTBLL1-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SUBMEIL1-INV-FLG      PIC  X(03)     VALUE  SPACE.
     03  CHK-FLG             PIC  X(01)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*
 01  WK-ST.
     03  MEI-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  CRT-STATUS        PIC  X(02).
     03  CRS-STATUS        PIC  X(02).
     03  NON-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVS0320B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0320B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVS0320B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-IN-JDATE             PIC   9(08).
*01  PARA-IN-JTIME             PIC   9(04).
*01  PARA-IN-TORICD            PIC   9(08).
*01  PARA-IN-SOKO              PIC   X(02).
*01  PARA-IN-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBTBLL1.
     MOVE      "SUBTBLL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBMEIL1.
     MOVE      "SUBMEIL1"   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       SUBTBLL1.
     OPEN     INPUT     SUBMEIL1.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*    商品変換ＴＢＬ読込み
     PERFORM SUBTBLL1-READ-SEC.
     DISPLAY "-------------------------------------" UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SUBTBLL1-READ-SEC    SECTION.
*
     MOVE    "SUBTBLL1-READ-SEC"     TO  S-NAME.
*
     READ     SUBTBLL1
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO SUBTBLL1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     IF  READ-CNT(6:3) = "000" OR "500"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
*****IF  TBL-F17  =  SPACE
*    OR  TBL-F18  =  SPACE
*        CONTINUE
*    ELSE
*        ADD         1              TO  SKIP2-CNT
*        GO                         TO  SUBTBLL1-READ-SEC
*****END-IF.
*
 SUBTBLL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
 MAIN001.
     MOVE     SPACE          TO  CHK-FLG.
*    SUB商品名称マスタ検索
     MOVE     TBL-F031       TO  MEI-F011.
     MOVE     TBL-F0321      TO  MEI-F0121.
     MOVE     TBL-F0322      TO  MEI-F0122.
     MOVE     TBL-F0323      TO  MEI-F0123.
     READ     SUBMEIL1
         INVALID
              ADD    1       TO  SKIP-CNT
              GO             TO  MAIN020
     END-READ.
*
     IF   TBL-F17  NOT =  MEI-D01
          MOVE       MEI-D01 TO  TBL-F17
          MOVE       "1"     TO  CHK-FLG
          ADD        1       TO  TBL-F17-CNT
     END-IF.
*
     IF   TBL-F18  NOT =  MEI-D02
          MOVE       MEI-D02 TO  TBL-F18
          MOVE       "1"     TO  CHK-FLG
          ADD        1       TO  TBL-F18-CNT
     END-IF.
*****IF   TBL-F17  =  SPACE
*         MOVE   MEI-D01     TO  TBL-F17
*****END-IF.
*
*****IF   TBL-F18  =  SPACE
*         MOVE   MEI-D02     TO  TBL-F18
*****END-IF.
*
     IF  CHK-FLG =  "1"
         REWRITE  TBL-REC
         ADD      1              TO  UPDT-CNT
     END-IF.
*
 MAIN020.
*    移行商品データ　読込み
     PERFORM SUBTBLL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "-------------------------------------" UPON CONS.
     DISPLAY NC"商品変換ＴＢＬ" ":SUBTBLL1  "         UPON CONS.
     DISPLAY NC"　　　　　　　" " IN  = " READ-CNT   UPON CONS.
     DISPLAY NC"更新なし　　　" ":SUBTBLL1  "        UPON CONS.
     DISPLAY NC"　　　　　　　" " SKIP= " SKIP2-CNT  UPON CONS.
     DISPLAY NC"名称Ｍ無　　　" ":SUBMEIL1  "        UPON CONS.
     DISPLAY NC"　　　　　　　" " SKIP= " SKIP-CNT   UPON CONS.
     DISPLAY NC"更新あり　　　" ":SUBTBLL1 "         UPON CONS.
     DISPLAY NC"　　　　　　　" " OUT = " UPDT-CNT   UPON CONS.
     DISPLAY NC"ＪＡＮＣＤ違い"                      UPON CONS.
     DISPLAY NC"　　　　　　　" " CHK = " TBL-F17-CNT UPON CONS.
     DISPLAY NC"商品ＣＤ違い　"                       UPON CONS.
     DISPLAY NC"　　　　　　　" " CHK = " TBL-F18-CNT UPON CONS.
     DISPLAY "-------------------------------------" UPON CONS.
*
     CLOSE     SUBTBLL1  SUBMEIL1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
