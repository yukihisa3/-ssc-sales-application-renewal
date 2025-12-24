# SSE0231B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSE0231B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ホーマーグリーンオンライン　　　　*
*    モジュール名　　　　：　請求データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　00/08/09                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより　　　　　　　*
*　　　　　　　　　　　　：　送信用ファイルを作成する          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE0231B.
 AUTHOR.                Y.YOSHIDA.
 DATE-WRITTEN.          00/08/09.
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
     SELECT   SETGKFA   ASSIGN         DA-01-VI-SETGKFA2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F03
                                       SEI-F04
                                       SEI-F05
                        STATUS         SEI-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
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
 FD  SETGKFA            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 配信データ >>--*
 FD  CVCSF              LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL0-REC                 PIC  X(240).
     03  ONLR1-REC  REDEFINES     ONL0-REC.
       05  ONL1-REC               OCCURS    6.
         07  ONL1-A1              PIC  9(06).
         07  ONL1-A2              PIC  X(15).
         07  ONL1-A3              PIC  9(07).
         07  ONL1-A4              PIC S9(09).
         07  ONL1-A5              PIC  X(02).
         07  ONL1-A6              PIC  X(01).
     03  ONLR2-REC  REDEFINES     ONL0-REC.
       05  ONL2-REC               OCCURS    6.
         07  ONL2-A1              PIC  9(06).
         07  ONL2-A2              PIC  9(03).
         07  ONL2-A3              PIC  9(02).
         07  ONL2-A4              PIC  9(04).
         07  ONL2-A5              PIC  9(06).
         07  ONL2-A6              PIC  9(07).
         07  ONL2-A7              PIC S9(09).
         07  ONL2-A8              PIC  X(02).
         07  ONL2-A9              PIC  X(01).
     03  ONLR3-REC  REDEFINES     ONL0-REC.
       05  ONL3-REC               OCCURS    6.
         07  ONL3-A1              PIC  9(06).
         07  ONL3-A2              PIC  9(03).
         07  ONL3-A3              PIC  9(02).
         07  ONL3-A4              PIC  X(17).
         07  ONL3-A5              PIC S9(09).
         07  ONL3-A6              PIC  X(02).
         07  ONL3-A7              PIC  X(01).
     03  FILLER                   PIC  X(16).
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
 01  TOK-ST             PIC  X(02).
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
         05  FILLER     PIC  X(09).
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  N(16).
         05  FILLER     PIC  X(02).
         05  ME-05      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(02).
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
     USE AFTER     EXCEPTION PROCEDURE      SETGKFA.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0231B SETGKFA ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0231B HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 配信データ >>--*
 CVCSKHAI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0231B CVCSF    ERROR " CVCS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
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
     DISPLAY  "*** SSE0231B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SETGKFA.
     OPEN     INPUT     HTOKMS.
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
     MOVE     883            TO   SEI-F01.
     START    SETGKFA   KEY  >=   SEI-F01
                                  SEI-F03
                                  SEI-F04
                                  SEI-F05
         INVALID   KEY
              MOVE      1         TO   END-FLG
              GO   TO   INIT-RTN-EXIT
     END-START.
*
 INIT-RTN-010.
*
     PERFORM  SEI-READ.
     IF       END-FLG   =    1
         IF   KEI-KEN   NOT  =    ZERO
*             請求合計データ出力
              MOVE      SPACE          TO   ONL-REC
              INITIALIZE                    ONL-REC
              MOVE      1              TO   IDX
              MOVE      SPACE          TO   ONL1-REC(IDX)
              INITIALIZE                    ONL1-REC(IDX)
              MOVE      880            TO   ONL1-A1 (IDX)
              MOVE      SPACE          TO   ONL1-A2 (IDX)
              MOVE      KEI-KEN        TO   ONL1-A3 (IDX)
              MOVE      KEI-KIN        TO   ONL1-A4 (IDX)
              MOVE      SPACE          TO   ONL1-A5 (IDX)
              MOVE     "1"             TO   ONL1-A6 (IDX)
              GO   TO        INIT-RTN-020
         ELSE
              GO   TO        INIT-RTN-EXIT
         END-IF
     ELSE
*        合計計算
         ADD  SEI-F06   TO   KEI-KIN
         ADD  1         TO   KEI-KEN
         GO   TO        INIT-RTN-010
     END-IF.
*
 INIT-RTN-020.
*
     INITIALIZE         COUNTERS.
     MOVE     ZERO      TO   END-FLG.
     MOVE     99        TO   LINE-CNT.
     CLOSE              SETGKFA.
     OPEN     INPUT     SETGKFA.
*
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE     883            TO   SEI-F01.
     START    SETGKFA   KEY  >=   SEI-F01
                                  SEI-F03
                                  SEI-F04
                                  SEI-F05
         INVALID   KEY
              MOVE      1    TO   END-FLG
              GO   TO   INIT-RTN-EXIT
     END-START.
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
     IF       IDX       =    6
              WRITE     ONL-REC
              ADD       1         TO   OUT-CNT
              MOVE      ZERO      TO   IDX
              MOVE      SPACE     TO   ONL-REC
              INITIALIZE               ONL-REC
     END-IF.
     ADD      1              TO   IDX.
*    請求明細レコード出力
     MOVE     SPACE          TO   ONL2-REC(IDX)
     INITIALIZE                   ONL2-REC(IDX)
     MOVE     880            TO   ONL2-A1 (IDX).
     MOVE     ZERO           TO   ONL2-A2 (IDX).
     MOVE     ZERO           TO   ONL2-A3 (IDX).
     MOVE     SEI-F03        TO   ONL2-A4 (IDX).
     MOVE     SEI-F10        TO   ONL2-A5 (IDX).
     MOVE     SEI-F05(3:7)   TO   ONL2-A6 (IDX).
     MOVE     SEI-F06        TO   ONL2-A7 (IDX).
     MOVE     SPACE          TO   ONL2-A8 (IDX).
     MOVE    "2"             TO   ONL2-A9 (IDX).
*
     PERFORM  SEI-READ.
*
     IF       END-FLG   =    1
         IF   IDX       >    ZERO
              WRITE     ONL-REC
              ADD       1    TO   OUT-CNT
         END-IF
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
     CLOSE    SETGKFA.
     CLOSE    HTOKMS.
     CLOSE    CVCSF.
     CLOSE    PRTF.
*
     DISPLAY "+++ ｺﾞｳｹｲﾃﾞｰﾀ INPUT =" IN-CNT  " +++" UPON CONS.
     DISPLAY "+++ ｾｲｷｭｳﾃﾞｰﾀ OUTPUT=" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0231B END *** "
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
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     883            TO   ME-03.
     MOVE     883            TO   TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
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
*　　　　取引先マスタＲＥＡＤ　　　　　　　　　　　　　　　　　*
****************************************************************
 TOK-READ               SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
     READ     SETGKFA   AT   END
              MOVE      1         TO   END-FLG
              GO   TO   SEI-READ-EXIT
     END-READ.
*
     IF       SEI-F01   NOT =     883
              MOVE      1         TO   END-FLG
              GO   TO   SEI-READ-EXIT
     END-IF.
*
     ADD      1              TO   IN-CNT.
     IF       IN-CNT    =    1
              MOVE      880            TO   WK-TORICD
     END-IF.
*
 SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
