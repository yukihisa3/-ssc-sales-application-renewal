****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｊ本田請求処理　　　　　　　　　　*
*    モジュール名　　　　：　請求データ作成（オンライン用）　　*
*    作成日／更新日　　　：　05/10/07                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより　　　　　　　*
*　　　　　　　　　　　　：　送信用ファイルを作成する          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE7107B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/10/07.
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
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52  TEN-F011
                        STATUS         TEN-ST.
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
*----<< 店舗マスタ>>-*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY      HTENMS   OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 配信データ >>--*
 FD  CVCSF              LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL0-REC                 PIC  X(128).
     03  ONLR1-REC  REDEFINES     ONL0-REC.
*----  ファイルヘッダ  ----
       05  ONL1-REC.
         07  ONL1-A1              PIC  X(02).
         07  ONL1-A2              PIC  X(02).
         07  ONL1-A3              PIC  9(05).
         07  ONL1-A4              PIC  9(08).
         07  ONL1-A5              PIC  9(06).
         07  ONL1-A6              PIC  9(08).
         07  ONL1-A7              PIC  9(06).
         07  ONL1-A8              PIC  9(05).
         07  ONL1-A9              PIC  X(01).
         07  ONL1-A10             PIC  X(05).
         07  ONL1-A11             PIC  X(01).
         07  ONL1-A12             PIC  X(79).
     03  ONLR2-REC  REDEFINES     ONL0-REC.
*----  請求ヘッダ  ----
       05  ONL2-REC.
         07  ONL2-B1              PIC  X(02).
         07  ONL2-B2              PIC  X(02).
         07  ONL2-B3              PIC  9(05).
         07  ONL2-B4              PIC  9(10).
         07  ONL2-B5              PIC  X(05).
         07  ONL2-B6              PIC  X(01).
         07  ONL2-B7              PIC  X(04).
         07  ONL2-B8              PIC  X(20).
         07  ONL2-B9              PIC  X(10).
         07  ONL2-B10             PIC  9(08).
         07  ONL2-B11             PIC  9(08).
         07  ONL2-B12             PIC  9(08).
         07  ONL2-B13             PIC  X(45).
     03  ONLR3-REC  REDEFINES     ONL0-REC.
*----  請求伝票合計----
       05  ONL3-REC.
         07  ONL3-C1              PIC  X(02).
         07  ONL3-C2              PIC  X(02).
         07  ONL3-C3              PIC  9(05).
         07  ONL3-C4              PIC  X(02).
         07  ONL3-C5              PIC  X(01).
         07  ONL3-C6              PIC  X(04).
         07  ONL3-C7              PIC  X(04).
         07  ONL3-C8              PIC  9(08).
         07  ONL3-C9              PIC  9(08).
         07  ONL3-C10             PIC  9(07).
         07  ONL3-C11             PIC S9(10).
         07  ONL3-C12             PIC S9(09).
         07  ONL3-C13             PIC  X(02).
         07  ONL3-C14             PIC  X(02).
         07  ONL3-C15             PIC S9(10).
         07  ONL3-C16             PIC S9(09).
         07  ONL3-C17             PIC  X(02).
         07  ONL3-C18             PIC  X(02).
         07  ONL3-C19             PIC  X(02).
         07  ONL3-C20             PIC  X(37).
     03  ONLR4-REC  REDEFINES     ONL0-REC.
*----  請求合計----
       05  ONL4-REC.
         07  ONL4-D1              PIC  X(02).
         07  ONL4-D2              PIC  X(02).
         07  ONL4-D3              PIC  9(05).
         07  ONL4-D4              PIC  9(06).
         07  ONL4-D5              PIC S9(10).
         07  ONL4-D6              PIC S9(09).
         07  ONL4-D7              PIC  9(06).
         07  ONL4-D8              PIC  9(06).
         07  ONL4-D9              PIC  9(06).
         07  ONL4-D10             PIC S9(10).
         07  ONL4-D11             PIC S9(09).
         07  ONL4-D12             PIC S9(10).
         07  ONL4-D13             PIC  X(47).
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
     03  READ-CNT       PIC  9(06)    VALUE  ZERO.
     03  WRITE-CNT      PIC  9(06)    VALUE  ZERO.
     03  OUT-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  TEN-CNT        PIC  9(05)    VALUE  ZERO.
     03  MEI-CNT        PIC  9(05)    VALUE  ZERO.
     03  GYO-CNT        PIC  9(05)    VALUE  ZERO.
     03  ZEN-CNT        PIC  9(05)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
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
 01  WK-TENCD           PIC  9(04)     VALUE ZERO.
 01  WK-SEI-F02         PIC  9(04)     VALUE ZERO.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATEW          PIC  9(08).
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
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  WK-HEN-DATE.
     03  WK-HEN-YYYY    PIC  9(04)     VALUE  ZERO.
     03  WK-HEN-MM      PIC  9(02)     VALUE  ZERO.
     03  WK-HEN-DD      PIC  9(02)     VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 JDJSEKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JDJSEKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE7107B JDJSEKF ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"              TO      PROGRAM-STATUS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE7107B HTENMS ERROR " TEN-ST  " "
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
     DISPLAY  "### SSE7107B CVCSF    ERROR " CVCS-ST " "
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
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7107B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     JDJSEKF   HTENMS.
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
*    データ全件読込
     PERFORM  SEI-READ.
     IF       END-FLG  =  1
              DISPLAY NC"＃対象データ無し＃" UPON CONS
              STOP  RUN
     END-IF.
     PERFORM  SEI-HENSYU     UNTIL  END-FLG = 1.
     COMPUTE  ZEN-CNT  =  MEI-CNT +  TEN-CNT +  2.
*    ﾍｯﾀﾞﾚｺｰﾄﾞ書込
     MOVE     SPACE          TO   ONL-REC.
     INITIALIZE                   ONL-REC.
     ADD      1              TO   GYO-CNT.
     MOVE     "04"           TO   ONL1-A1.
     MOVE     "F1"           TO   ONL1-A2.
     MOVE     GYO-CNT        TO   ONL1-A3.
     MOVE     SYS-DATEW      TO   ONL1-A4   ONL1-A6.
     MOVE     SYS-TIME(1:6)  TO   ONL1-A5   ONL1-A7.
     MOVE     ZEN-CNT        TO   ONL1-A8.
*TEST時は、"T"をｾｯﾄ
     MOVE     " "            TO   ONL1-A9.
     MOVE     "02243"        TO   ONL1-A10.
     MOVE     SPACE          TO   ONL1-A11  ONL1-A12.
     WRITE                        ONL-REC.
*
     ADD      1              TO   WRITE-CNT.
     MOVE     ZERO           TO   END-FLG.
     MOVE     99             TO   LINE-CNT.
     CLOSE              JDJSEKF.
     OPEN     INPUT     JDJSEKF.
*
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE     SPACE          TO   ONL-REC.
     INITIALIZE                   ONL-REC.
     MOVE     ZERO           TO   WK-SEI-F02   READ-CNT.
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
     IF    SEI-F02  NOT =  WK-SEI-F02
*          請求ヘッダ出力
           MOVE     SPACE          TO   ONL-REC
           INITIALIZE                   ONL-REC
           ADD      1              TO   GYO-CNT
           MOVE     "04"       TO   ONL2-B1
           MOVE     "H1"       TO   ONL2-B2
           MOVE     GYO-CNT    TO   ONL2-B3
           MOVE     ZERO       TO   ONL2-B4
           MOVE     "02243"    TO   ONL2-B5
           MOVE     SPACE      TO   ONL2-B6
           MOVE     SEI-F02    TO   WK-TENCD
           MOVE     WK-TENCD   TO   ONL2-B7
           MOVE     "ｶﾌﾞ)ｻｶﾀﾉﾀﾈ" TO ONL2-B8
*----------店舗マスタ読込--
           MOVE     2243       TO   TEN-F52
           MOVE     SEI-F02    TO   TEN-F011
           READ     HTENMS     INVALID
                    MOVE  ALL  "*"  TO  ONL2-B9
                    NOT  INVALID
                    MOVE  TEN-F04   TO  ONL2-B9
           END-READ
           MOVE     SEI-F01    TO   ONL2-B10 ONL2-B12 WK-HEN-DATE
           COMPUTE  WK-HEN-MM  =  WK-HEN-MM  -   1
           IF       WK-HEN-MM  <  1
                    ADD    -1  TO   WK-HEN-YYYY
                    MOVE   12  TO   WK-HEN-MM
           END-IF
           MOVE     21         TO   WK-HEN-DD
           MOVE     WK-HEN-DATE TO  ONL2-B11
*----------対象期間・自　計算(TO DO)
           WRITE    ONL-REC
           ADD      1          TO   WRITE-CNT
           MOVE     SEI-F02    TO   WK-SEI-F02
     END-IF.
*----請求伝票合計出力
     MOVE     SPACE          TO   ONL-REC.
     INITIALIZE                   ONL-REC.
     ADD      1              TO   GYO-CNT.
     MOVE     "04"       TO   ONL3-C1.
     MOVE     "D1"       TO   ONL3-C2.
     MOVE     GYO-CNT    TO   ONL3-C3.
     MOVE     SEI-F07    TO   ONL3-C4.
     MOVE     "1"        TO   ONL3-C5.
     MOVE     "00"       TO   ONL3-C6(1:2).
     MOVE     SEI-F05    TO   ONL3-C6(3:2).
     MOVE     "00"       TO   ONL3-C7(1:2).
     MOVE     SEI-F06    TO   ONL3-C7(3:2).
     MOVE     ZERO       TO   ONL3-C8.
     MOVE     SEI-F04    TO   ONL3-C9.
     MOVE     SEI-F03    TO   ONL3-C10.
     MOVE     SEI-F10    TO   ONL3-C11.
     MOVE     ZERO       TO   ONL3-C12.
     MOVE     SPACE      TO   ONL3-C13.
     MOVE     "00"       TO   ONL3-C14.
     MOVE     ZERO       TO   ONL3-C15.
     MOVE     ZERO       TO   ONL3-C16.
*
*    伝区判定
     IF       SEI-F07   =     "21"    OR  "11"
              MULTIPLY        -1      BY  ONL3-C11
     END-IF.
*
*    合計計算
     COMPUTE  KEI-KIN    =    KEI-KIN  +  ONL3-C11.
     ADD      1          TO   WRITE-CNT.
     WRITE    ONL-REC.
*
     PERFORM  SEI-READ.
     IF       END-FLG    =    1
*          請求合計出力
           MOVE     SPACE          TO   ONL-REC
           INITIALIZE                   ONL-REC
           ADD      1              TO   GYO-CNT
           MOVE     "04"       TO   ONL4-D1
           MOVE     "T1"       TO   ONL4-D2
           MOVE     GYO-CNT    TO   ONL4-D3
           MOVE     READ-CNT   TO   ONL4-D4
           MOVE     KEI-KIN    TO   ONL4-D5
           MOVE     ZERO       TO   ONL4-D6
           MOVE     ZERO       TO   ONL4-D7
           MOVE     ZERO       TO   ONL4-D8
           MOVE     ZERO       TO   ONL4-D9
           MOVE     ZERO       TO   ONL4-D10
           MOVE     ZERO       TO   ONL4-D11
           MOVE     ZERO       TO   ONL4-D12
*----------対象期間・自　計算(TO DO)
           WRITE    ONL-REC
           ADD      1          TO   WRITE-CNT
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
     DISPLAY "+++ ｺﾞｳｹｲﾃﾞｰﾀ INPUT =" READ-CNT  " +++" UPON CONS.
     DISPLAY "+++ ｾｲｷｭｳﾃﾞｰﾀ OUTPUT=" WRITE-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7107B END *** "
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
     MOVE     READ-CNT       TO   ME-05.
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
     ADD      1              TO   READ-CNT  MEI-CNT.
*
 SEI-READ-EXIT.
     EXIT.
****************************************************************
*　　　　請求データ件数カウント　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-HENSYU             SECTION.
*
     IF    SEI-F02  NOT =  WK-SEI-F02
           ADD      1        TO   TEN-CNT
           MOVE  SEI-F02     TO   WK-SEI-F02
     END-IF.
*
     PERFORM  SEI-READ.
*
 SEI-HENSYU-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
