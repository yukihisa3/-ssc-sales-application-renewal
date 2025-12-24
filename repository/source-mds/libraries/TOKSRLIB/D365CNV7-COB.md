# D365CNV7

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/D365CNV7.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　：　（株）サカタのタネ殿　　　　　　　　　　*
*    サブシステム　：　Ｄ３６５連携　　　　　　　　　　        *
*    モジュール名　：　商品変換テーブルＯＲＤセット区分セット  *
*    作成日／作成者：　2021/06/04 INOUE                        *
*    処理概要　　　：　商品変換ＴＢＬのＯＲＤセット区分をセット*
*                      　　　　　　　　　　　　　　　　　      *
*    更新履歴                                                  *
*    更新日／更新者：　                                        *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            D365CNV7.
*                  流用:FLMCNV01
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/06/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ＯＲＤ区分セットＤＴ >>----*
     SELECT   IMPORDD1  ASSIGN              DA-01-VI-IMPORDD1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IN-F01
                                            IN-F02
                        FILE      STATUS    IN-STATUS.
*----<< 商品変換テーブル >>----*
     SELECT   SUBTBLL1   ASSIGN              DA-01-VI-SUBTBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE      STATUS    TBL-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    雑コード移行データ
******************************************************************
 FD  IMPORDD1  LABEL RECORD   IS   STANDARD.
     COPY      IMPORDD1       OF   XFDLIB
     JOINING   IN                  PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SUBTBLL1   LABEL RECORD   IS   STANDARD.
     COPY      SUBTBLL1        OF   XFDLIB
     JOINING   TBL                 PREFIX.
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
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  WKSHOTBL-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  IMPORDD1-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHOBL1-INV-FLG      PIC  X(03)     VALUE  SPACE.
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
     03  DEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  OUT-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "D365CNV7".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "D365CNV7".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "D365CNV7".
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
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   IMPORDD1.
     MOVE      "IMPORDD1 "   TO   AB-FILE.
     MOVE      IN-STATUS     TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
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
     OPEN     INPUT     IMPORDD1.
     OPEN     I-O       SUBTBLL1.
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
*
*    雑コード移行データＳＴＡＲＴ
*    MOVE     SPACE          TO   IN-REC.
*    INITIALIZE                   IN-REC.
*    MOVE     137607         TO   IN-F01.
*    START    IMPORDD1  KEY  >=   IN-F01
*        INVALID   KEY
*             MOVE    "END"  TO   END-FLG
*             GO             TO   INIT-EXIT
*    END-START.
*
*    雑コード移行データ読込み
     PERFORM IMPORDD1-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IMPORDD1-READ-SEC    SECTION.
*
     MOVE    "IMPORDD1-READ-SEC"    TO  S-NAME.
*
     READ     IMPORDD1
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  IMPORDD1-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 IMPORDD1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
 MAIN000.
*    商品変換ＴＢＬ検索
     MOVE     IN-F01         TO  TBL-F01.
     MOVE     IN-F02         TO  TBL-F02.
     READ     SUBTBLL1
         INVALID
              DISPLAY NC"変換ＴＢＬなし！　商品＝"
                                           IN-F01 UPON CONS
              ADD   1        TO  SKIP-CNT
              GO             TO  MAIN020
     END-READ.
*
 MAIN010.
*
*  ＯＲＤセット区分
     MOVE     IN-F03         TO  TBL-F19.
*
     REWRITE  TBL-REC.
     ADD      1              TO  WRT-CNT.
*
 MAIN020.
*    雑コード移行データ　読込み
     PERFORM IMPORDD1-READ-SEC.
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
     DISPLAY  NC"雑　移行データ　" "IN  = " READ-CNT  UPON CONS.
     DISPLAY  NC"抽出対象外　　　" "SKIP= " SKIP-CNT  UPON CONS.
     DISPLAY  NC"変換ＴＢＬ更新　" "OUT = " WRT-CNT   UPON CONS.
*
     CLOSE     IMPORDD1  SUBTBLL1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
