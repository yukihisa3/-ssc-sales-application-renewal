# SSE7104B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSE7104B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｊ本田請求処理　　　　　　　　　　*
*    モジュール名　　　　：　請求データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　05/08/16                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより　　　　　　　*
*　　　　　　　　　　　　：　送信用ファイルを作成する          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE7104B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/08/16.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル>>-*
     SELECT   JDJSEKF   ASSIGN         DA-01-VI-JDJSEKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F02
                                       SEI-F03
                        STATUS         SEI-ST.
*----<< 配信データ >>--*
     SELECT   CVCSF     ASSIGN         DA-01-S-CVCSF
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCS-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル>>-*
 FD  JDJSEKF            LABEL RECORD   IS   STANDARD.
     COPY     JDJSEKF   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 配信データ >>--*
 FD  CVCSF              LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL0-REC                 PIC  X(256).
     03  ONLR1-REC  REDEFINES     ONL0-REC.
       05  ONL1-REC.
         07  ONL1-A1              PIC  9(02).
         07  ONL1-A2              PIC  9(06).
         07  ONL1-A3              PIC  X(04).
         07  ONL1-A4              PIC  X(40).
         07  ONL1-A5              PIC  X(204).
     03  ONLR2-REC  REDEFINES     ONL0-REC.
       05  ONL2-REC.
         07  ONL2-A1              PIC  9(02).
         07  ONL2-A2              PIC  X(02).
         07  ONL2-A3              PIC  X(02).
         07  ONL2-A4              PIC  X(02).
         07  ONL2-A5              PIC  9(02).
         07  ONL2-A6              PIC  9(01).
         07  ONL2-A7              PIC  9(06).
         07  ONL2-A8              PIC  9(06).
         07  ONL2-A9              OCCURS  7.
             09  ONL2-A91         PIC  9(06).
             09  ONL2-A92         PIC  9(06)V9(02).
             09  ONL2-A93         PIC  9(08).
         07  ONL2-A10             PIC  9(08).
         07  ONL2-A11             PIC  X(71).
     03  ONLR3-REC  REDEFINES     ONL0-REC.
       05  ONL3-REC.
         07  ONL3-A1              PIC  9(02).
         07  ONL3-A2              PIC  9(06).
         07  ONL3-A3              PIC S9(09).
         07  ONL3-A4              PIC  X(239).
*
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01)    VALUE  ZERO.
     03  IDX            PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST             PIC  X(02).
 01  CVCS-ST            PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN        PIC  9(05)     VALUE  ZERO.
     03  KEI-KIN        PIC S9(09)     VALUE  ZERO.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-AREA.
     03  WK-TORICD      PIC  9(06).
 01  IND                PIC  9(02)     VALUE 1.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(47)  VALUE  SPACE.
         05  FILLER     PIC  N(12)  VALUE
                        NC"【　配信データリスト　】".
         05  FILLER     PIC  X(27)  VALUE  SPACE.
         05  FILLER     PIC  X(05)  VALUE  "DATE:".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-013     PIC  Z9.
 01  HEAD02.
         05  FILLER     PIC  X(110) VALUE  SPACE.
         05  FILLER     PIC  X(05)  VALUE  "PAGE:".
         05  HD-02      PIC  ZZZ9.
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"配信先".
         05  FILLER     PIC  X(35)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"伝票枚数".
         05  FILLER     PIC  X(08)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"請求金額".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  ME-03      PIC  X(08)  VALUE  SPACE.
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  ME-04      PIC  N(16)  VALUE NC"ジョイフル本田".
         05  FILLER     PIC  X(09)  VALUE  SPACE.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  ME-05      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  ME-06      PIC  --,---,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 SETGKFA-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JDJSEKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE7104B JDJSEKF ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
*----<< 配信データ >>--*
 CVCSKHAI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE7104B CVCSF    ERROR " CVCS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     IF       END-FLG   =    0
              PERFORM       MAIN-RTN
                            UNTIL END-FLG = 1
     END-IF.
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7104B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     JDJSEKF.
     OPEN     OUTPUT    CVCSF.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         GOKEI-AREA.
     INITIALIZE         WK-AREA.
*
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE     SPACE          TO   ONL-REC.
     INITIALIZE                   ONL-REC.
*
*    データ読込
     PERFORM  SEI-READ.
*
*    ﾍｯﾀﾞﾚｺｰﾄﾞ書込
     MOVE     SPACE          TO   ONL-REC.
     INITIALIZE                   ONL-REC.
     MOVE     "01"           TO   ONL1-A1.
     MOVE     SEI-F01        TO   ONL1-A2.
     MOVE     "2243"         TO   ONL1-A3.
     MOVE     "ｶﾌﾞｼｷｶﾞｲｼｬ ｻｶﾀﾉﾀﾈ"     TO ONL1-A4.
     WRITE                        ONL-REC.
*
     INITIALIZE         COUNTERS.
     MOVE     ZERO           TO   END-FLG.
     MOVE     99             TO   LINE-CNT.
     CLOSE              JDJSEKF.
     OPEN     INPUT     JDJSEKF.
*
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE     SPACE          TO   ONL-REC.
     INITIALIZE                   ONL-REC.
*
     PERFORM  SEI-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     MOVE     SPACE      TO   ONL-REC.
     INITIALIZE               ONL-REC.
*    請求明細レコード出力
     MOVE     "02"       TO   ONL2-A1.
     MOVE     SEI-F02(2:2) TO   ONL2-A2.
     MOVE     SEI-F05    TO   ONL2-A3.
     MOVE     SEI-F06    TO   ONL2-A4.
     MOVE     SEI-F07    TO   ONL2-A5.
     MOVE     SEI-F08    TO   ONL2-A6.
     MOVE     SEI-F04    TO   ONL2-A7.
     MOVE     SEI-F03    TO   ONL2-A8.
 LOOP-START.
     MOVE     SEI-F091(IND)   TO   ONL2-A91(IND).
     MOVE     SEI-F092(IND)   TO   ONL2-A92(IND).
     MOVE     SEI-F093(IND)   TO   ONL2-A93(IND).
     ADD      1          TO   IND.
     IF       IND > 7
              MOVE  1    TO   IND
              GO    TO   LOOP-END
     ELSE
              GO    TO   LOOP-START
     END-IF.
 LOOP-END.
     MOVE     SEI-F10    TO   ONL2-A10.
*
*    伝区判定
     IF       ONL2-A5   =     "21"    OR  "11"
              MULTIPLY        -1      BY  ONL2-A10
     END-IF.
*
*    合計計算
     COMPUTE  KEI-KIN    =    KEI-KIN  +  ONL2-A10.
     ADD      1          TO   OUT-CNT.
     ADD      1          TO   KEI-KEN.
     WRITE    ONL-REC.
*
     PERFORM  SEI-READ.
     IF       END-FLG    =    1
              MOVE       SPACE         TO ONL-REC
              INITIALIZE                  ONL-REC
              MOVE       "09"          TO ONL3-A1
              MOVE       KEI-KEN       TO ONL3-A2
              MOVE       KEI-KIN       TO ONL3-A3
              WRITE                       ONL-REC
     END-IF.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
*---<< ｺﾞｳｹｲ ｲﾝｻﾂ >>---*
     PERFORM  PRINT-RTN.
*
     CLOSE    JDJSEKF.
     CLOSE    CVCSF.
     CLOSE    PRTF.
*
     DISPLAY "+++ ｺﾞｳｹｲﾃﾞｰﾀ INPUT =" IN-CNT  " +++" UPON CONS.
     DISPLAY "+++ ｾｲｷｭｳﾃﾞｰﾀ OUTPUT=" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7104B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　印刷処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PRINT-RTN              SECTION.
     IF       LINE-CNT  >    63
              PERFORM   HEAD-PRINT
     END-IF.
     MOVE     NC"ジョイフル本田"  TO   ME-04.
     MOVE     KEI-KEN        TO   ME-05.
     MOVE     KEI-KIN        TO   ME-06.
*
     WRITE    PRT-REC   FROM MEIS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
 PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　タイトルプリント　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HEAD-PRINT             SECTION.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1              TO   PAGE-CNT.
     MOVE     SYS-YY         TO   HD-011.
     MOVE     SYS-MM         TO   HD-012.
     MOVE     SYS-DD         TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     2.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     7              TO   LINE-CNT.
 HEAD-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
     READ     JDJSEKF   AT   END
              MOVE      1         TO   END-FLG
              GO   TO   SEI-READ-EXIT
     END-READ.
*
     ADD      1              TO   IN-CNT.
*
 SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
